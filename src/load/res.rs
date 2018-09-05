//! Result data file loader.

use common::{
    res::{RunRes, ToolRes},
    *,
};

use super::{
    run::{new_codes, LCode},
    serde_error,
};

/// Loads a toml res data file.
pub fn toml<P>(gconf: &GConf, run_res: &mut RunRes, file: P) -> Res<ToolRes>
where
    P: AsRef<Path>,
{
    let file = file.as_ref();

    let mut txt = String::new();
    File::open(file)?.read_to_string(&mut txt)?;

    let res = match ::toml::from_str::<LRes>(&txt) {
        Ok(res) => res,
        Err(e) => bail!(serde_error(gconf, &e, &txt)),
    };

    res.finalize(gconf, run_res, file.to_string_lossy().into_owned())
}

/// Result of a tool running on a benchmark.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NewBenchRes {
    /// Success with a time and an optional validation code.
    Success(Duration, Validation),
    /// Timeout.
    Timeout,
    /// Error.
    Error(Duration),
}
impl NewBenchRes {
    /// True if the result is an error.
    pub fn is_err(self) -> bool {
        if let NewBenchRes::Error(_) = self {
            true
        } else {
            false
        }
    }

    /// True if the result is a timeout.
    pub fn is_tmo(self) -> bool {
        self == NewBenchRes::Timeout
    }

    /// Success code accessor.
    pub fn code(self) -> Option<Validation> {
        if let NewBenchRes::Success(_, code) = self {
            Some(code)
        } else {
            None
        }
    }

    /// Map over the different types of data.
    pub fn map<
        T,
        FSucc: FnOnce(Duration, Validation) -> T,
        FTmo: FnOnce() -> T,
        FErr: FnOnce(Duration) -> T,
    >(
        &self,
        f_succ: FSucc,
        f_tmo: FTmo,
        f_err: FErr,
    ) -> T {
        match *self {
            NewBenchRes::Success(time, vald) => f_succ(time, vald),
            NewBenchRes::Timeout => f_tmo(),
            NewBenchRes::Error(time) => f_err(time),
        }
    }
}

/// A run result right after loading.
#[derive(Debug, Serialize, Deserialize)]
struct LRes {
    timeout: String,
    tool: NewToolConf,
    codes: StrMap<LCode>,
    data: StrMap<LNewBenchRes>,
}
impl LRes {
    /// Finalizes a run result.
    fn finalize(self, gconf: &GConf, run_res: &mut RunRes, file: String) -> Res<ToolRes> {
        let LRes {
            timeout,
            tool,
            codes,
            data,
        } = self;

        let (mut suc_count, mut err_count, mut tmo_count) = (0, 0, 0);

        let timeout = Duration::from_str(&timeout).chain_err(|| {
            format!(
                "while recovering timeout in result file of tool {}",
                gconf.bad(tool.ident())
            )
        })?;

        let codes = new_codes(gconf, codes).chain_err(|| {
            format!(
                "while recovering the exit codes for tool {}",
                gconf.bad(tool.ident())
            )
        })?;

        let mut benchs = BenchHMap::with_capacity(data.len());

        for (key, res) in data {
            use std::str::FromStr;
            let index: BenchIdx = usize::from_str(&key)
                .map_err::<Error, _>(|e| {
                    format!(
                        "illegal benchmark index {}: not an integer ({})",
                        gconf.bad(&key),
                        e
                    ).into()
                }).chain_err(|| {
                    format!(
                        "while handling data file for tool {}",
                        gconf.bad(tool.ident())
                    )
                }).map(|index| index.into())?;

            let (bench, res) = res.into_bench_res().chain_err(|| {
                format!(
                    "while handling bench #{} for tool {}",
                    index,
                    gconf.sad(tool.ident())
                )
            })?;

            match &res {
                NewBenchRes::Success(_, _) => suc_count += 1,
                NewBenchRes::Error(_) => err_count += 1,
                NewBenchRes::Timeout => tmo_count += 1,
            }

            let prev = benchs.insert(index, res);

            assert! { prev.is_none() }

            let prev = run_res.benchs.entry(index).or_insert_with(|| bench.clone());

            if prev != &bench {
                let mut blah = String::new();
                let mut other_tools = run_res.tools.iter();

                if let Some(other_tool) = other_tools.next() {
                    blah += other_tool.tool.ident();
                    for other in other_tools {
                        blah += ", ";
                        blah += other.tool.ident()
                    }
                }

                bail!(
                    "disagreement on bench #{}: tool {} calls it {}\n\
                     but tool(s) {} call it {}",
                    gconf.bad(&format!("{}", index)),
                    gconf.bad(tool.ident()),
                    gconf.bad(&bench),
                    blah,
                    gconf.sad(prev)
                )
            }
        }

        Ok(ToolRes {
            tool,
            timeout,
            file,
            benchs,
            codes,
            suc_count,
            err_count,
            tmo_count,
        })
    }
}

/// A benchmark run result.
#[derive(Debug, Serialize, Deserialize)]
struct LNewBenchRes {
    bench: String,
    time: Option<String>,
    code: Option<i32>,
}
impl LNewBenchRes {
    /// Turns itself into a `NewBenchRes`.
    fn into_bench_res(self) -> Res<(String, NewBenchRes)> {
        let LNewBenchRes { bench, time, code } = self;
        let res = if let Some(time) = time {
            let time = Duration::from_str(&time)
                .chain_err(|| format!("while recovering runtime for benchmarks `{}`", bench))?;

            if let Some(code) = code {
                NewBenchRes::Success(time, code)
            } else {
                NewBenchRes::Error(time)
            }
        } else {
            // Timeout.
            NewBenchRes::Timeout
        };

        Ok((bench, res))
    }
}
