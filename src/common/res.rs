//! Types representing results.

use rayon::slice::ParallelSliceMut;

use common::*;

/// Result of a tool running on a benchmark.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BenchRes {
    /// Success with a time and an optional validation code.
    Success(Duration, Option<Validation>),
    /// Timeout.
    Timeout,
    /// Error.
    Error,
}
impl BenchRes {
    /// Sets the validation code.
    pub fn set_code(&mut self, code: Validation) {
        if let BenchRes::Success(_, ref mut opt) = *self {
            *opt = Some(code)
        }
    }

    /// Returns the code of a result.
    pub fn code(&self) -> Option<Validation> {
        self.map(|_, res| res, || None, || None)
    }

    /// Map over the different types of data.
    pub fn map<
        T,
        FSucc: FnOnce(Duration, Option<Validation>) -> T,
        FTmo: FnOnce() -> T,
        FErr: FnOnce() -> T,
    >(
        &self,
        f_succ: FSucc,
        f_tmo: FTmo,
        f_err: FErr,
    ) -> T {
        match *self {
            BenchRes::Success(time, vald) => f_succ(time, vald),
            BenchRes::Timeout => f_tmo(),
            BenchRes::Error => f_err(),
        }
    }

    /// True if `self` is an error.
    pub fn is_err(&self) -> bool {
        *self == BenchRes::Error
    }
    /// True if `self` is a timeout.
    pub fn is_tmo(&self) -> bool {
        match *self {
            BenchRes::Timeout => true,
            _ => false,
        }
    }
}

/// Stores the results for a tool on some benchmarks.
pub struct ToolRes {
    /// Tool configuration.
    pub tool: NewToolConf,
    /// Timeout.
    pub timeout: Duration,
    /// Data file.
    pub file: String,
    /// Bench results.
    pub benchs: BenchHMap<NewBenchRes>,
    /// Exit codes.
    pub codes: NewCodes,
    /// Number of successes.
    pub suc_count: usize,
    /// Number of errors.
    pub err_count: usize,
    /// Number of timeouts.
    pub tmo_count: usize,
}
impl CodesExt for ToolRes {
    fn codes(&self) -> &NewCodes {
        &self.codes
    }
}
impl ToolRes {
    /// Constructor.
    fn new(
        tool: NewToolConf,
        timeout: Duration,
        file: String,
        codes: NewCodes,
        benchs: BenchHMap<NewBenchRes>,
    ) -> Self {
        let (mut suc_count, mut err_count, mut tmo_count) = (0, 0, 0);
        for res in benchs.values() {
            match res {
                NewBenchRes::Success(_, _) => suc_count += 1,
                NewBenchRes::Error(_) => err_count += 1,
                NewBenchRes::Timeout => tmo_count += 1,
            }
        }
        ToolRes {
            tool,
            timeout,
            file,
            codes,
            benchs,
            suc_count,
            err_count,
            tmo_count,
        }
    }

    /// Loads a tool result from a file.
    fn of(file: &str, run_res: &mut RunRes) -> Res<Self> {
        let mut reader = ::std::fs::OpenOptions::new()
            .read(true)
            .open(&file)
            .chain_err(|| format!("while opening data file `{}`", file))?;
        let mut txt = String::new();
        reader
            .read_to_string(&mut txt)
            .chain_err(|| format!("error reading file `{}`", file))?;
        ::load::res(&GConf::mk(Verb::Normal, true, false), run_res, &file)
    }

    /// Returns the lowest and highest runtime for successful benchmarks.
    pub fn time_interval(&self) -> Option<(Duration, Duration)> {
        let mut interval = None;
        for res in self.benchs.values() {
            if let NewBenchRes::Success(time, _) = res {
                let time = *time;
                let (lo, hi) = interval.unwrap_or((time, time));
                interval = Some((::std::cmp::min(time, lo), ::std::cmp::max(time, hi)))
            }
        }
        interval
    }

    /// Cumul data.
    ///
    /// Sorts (`<=`) all the success results.
    fn as_cumul_data(&self) -> Vec<Duration> {
        let mut times =
            self.benchs
                .values()
                .fold(Vec::with_capacity(self.benchs.len()), |mut vec, res| {
                    if let NewBenchRes::Success(time, _) = res {
                        vec.push(*time)
                    }
                    vec
                });
        times.shrink_to_fit();
        times.par_sort_unstable();
        times
    }

    /// Writes the cumulative data to `<plot_basename>_<tool_short>.data`.
    ///
    /// Returns the number of benchmarks written. If zero, then the data file
    /// was **not** created.
    pub fn write_cumul(&self, conf: &PlotConf) -> Res<usize> {
        if self.benchs.is_empty() {
            return Ok(0);
        }

        let sorted = self.as_cumul_data();
        let count = sorted.len();

        let path = conf.data_file_path_of(&self.tool)?;
        let mut tool_file = conf.open_file_writer(path.as_path()).chain_err(|| {
            format!(
                "while opening file `{}`, plot data file for `{}`",
                conf.emph(path.to_string_lossy()),
                conf.emph(self.tool.ident())
            )
        })?;

        let mut acc = Duration::zero();

        for (count, time) in (0..).zip(sorted.into_iter()) {
            acc += time;
            writeln!(tool_file, "{} {}", count, acc.as_sec_str()).chain_err(|| {
                format!(
                    "while writing cumul plot data for `{}` to `{}`",
                    conf.emph(self.tool.ident()),
                    conf.sad(path.to_string_lossy())
                )
            })?
        }

        Ok(count)
    }
}

/// Stores the result of a benchi run.
pub struct RunRes {
    /// Tools results.
    pub tools: Vec<ToolRes>,
    /// Benchs.
    pub benchs: BenchHMap<String>,
}
impl RunRes {
    /// From some data files.
    pub fn of_files(files: Vec<String>) -> Res<Self> {
        let mut res = RunRes {
            tools: Vec::with_capacity(files.len()),
            benchs: BenchHMap::with_capacity(211),
        };
        for file in files {
            let tool_res = ToolRes::of(&file, &mut res)?;
            res.tools.push(tool_res)
        }
        res.benchs.shrink_to_fit();
        Ok(res)
    }

    /// Removes all results for the benchmarks for which at least one of the
    /// tools errored.
    ///
    /// Returns the number of benchmarks dropped.
    pub fn rm_errs(&mut self) -> usize {
        let mut tool_buf = Vec::with_capacity(self.tools.len());
        let mut to_rm = vec![];
        let mut removed = 0;
        while let Some(mut current) = self.tools.pop() {
            for (bench, res) in &current.benchs {
                if res.is_err() {
                    let prev = self.benchs.remove(bench);
                    assert!(prev.is_some());
                    to_rm.push(*bench)
                }
            }
            removed += to_rm.len();
            for bench in to_rm.drain(0..) {
                let _ = current.benchs.remove(&bench);
                for tool in tool_buf.iter_mut().chain(self.tools.iter_mut()) {
                    let _ = tool.benchs.remove(&bench);
                }
            }
            tool_buf.push(current)
        }
        while let Some(tool) = tool_buf.pop() {
            self.tools.push(ToolRes::new(
                tool.tool,
                tool.timeout,
                tool.file,
                tool.codes,
                tool.benchs,
            ))
        }
        removed
    }

    /// Turns all errors into timeouts.
    ///
    /// Returns the number of errors changed **over all tools**.
    pub fn errs_as_tmos(&mut self) -> usize {
        let mut changed = 0;
        for tool in &mut self.tools {
            for res in tool.benchs.values_mut() {
                if res.is_err() {
                    changed += 1;
                    *res = NewBenchRes::Timeout;
                    tool.err_count -= 1;
                    tool.tmo_count += 1;
                }
            }
        }
        changed
    }

    /// Checks that a bench index corresponds to the right string. If there's no
    /// name associated to the index, adds it.
    pub fn check_bench_index(&mut self, bench: BenchIdx, name: String) -> Res<()> {
        if let Some(bench_name) = self.benchs.get(&bench) {
            if name == *bench_name {
                return Ok(());
            } else {
                let e: Res<()> = Err(format!("but was previously seen as `{}`", bench_name).into());
                return e.chain_err(|| format!("benchmark with index {} is `{}`", bench, name));
            }
        }
        self.benchs.insert(bench, name);
        Ok(())
    }
}

/// Handles the plot data files for different validators.
pub enum DataFileHandler<'a> {
    /// Merged, uses only one file.
    Merged {
        /// The only data file used.
        file: File,
        /// The path to the only data file used.
        path: PathBuf,
    },
    /// Split, as many files as validators *used*.
    Split {
        /// The `Codes` common to all tools.
        codes: NewCodes,
        /// Map from error codes to data files and their path.
        map: HashMap<Validation, (File, PathBuf)>,
        /// File and path for unknowns (in case of double timeout / error).
        unknown: Option<(File, PathBuf)>,
        /// Conf.
        conf: &'a PlotConf,
    },
}
impl<'a> DataFileHandler<'a> {
    /// Creates a handler from a RunRes and a conf.
    ///
    /// Error if not in `merged` mode and validation configuration don't match.
    pub fn new(conf: &'a PlotConf, run_res: &RunRes) -> Res<Self> {
        if conf.merge {
            // Merged, only one file needed.
            let (file, path) = Self::data_file_of(conf, None)?;
            Ok(DataFileHandler::Merged { file, path })
        } else {
            // Not merging, check the validators make sense.
            let codes = {
                let mut iter = run_res.tools.iter();
                if let Some(tool_res) = iter.next() {
                    let codes = tool_res.codes.clone();
                    for other_tool_res in iter {
                        if other_tool_res.codes != codes {
                            return (Err(format!(
                                "If you still want to plot the data, consider using \
                                 option `{}`: `benchi plot {} ...",
                                conf.happy("--merge"),
                                conf.emph("--merge on")
                            ).into()) as Res<Self>)
                                .chain_err(|| {
                                    format!(
                                        "the data files most likely come from {}.",
                                        conf.sad("different benchi runs")
                                    )
                                }).chain_err(|| {
                                    format!(
                  "data for tools {} and {} do not agree on their validators,",
                  conf.bad(& tool_res.tool.ident()),
                  conf.bad(& other_tool_res.tool.ident())
                )
                                });
                        }
                    }
                    codes
                } else {
                    bail!("no data file provided")
                }
            };

            if codes.is_empty() {
                // No validator provided, doing a merge.
                let (file, path) = Self::data_file_of(conf, None)?;
                Ok(DataFileHandler::Merged { file, path })
            } else {
                let map = HashMap::with_capacity(codes.len());
                Ok(DataFileHandler::Split {
                    codes,
                    map,
                    unknown: None,
                    conf,
                })
            }
        }
    }

    /// Returns the data file corresponding to a validator.
    pub fn file_of(&mut self, code: Option<Validation>) -> Res<(&mut File, &PathBuf)> {
        match *self {
            DataFileHandler::Merged {
                ref mut file,
                ref path,
            } => Ok((file, path)),

            DataFileHandler::Split { ref mut map, .. }
                if code.map(|code| map.contains_key(&code)).unwrap_or(false) =>
            {
                let file_n_path = map.get_mut(&code.unwrap()).unwrap();
                Ok((&mut file_n_path.0, &file_n_path.1))
            }

            DataFileHandler::Split {
                ref mut map,
                ref codes,
                ref mut unknown,
                ref conf,
            } => if let Some(code) = code {
                // Not initialized yet, let's doodis.
                if let Some(vald) = codes.get(code) {
                    let file_n_path = Self::data_file_of(conf, Some(&vald.name))?;
                    let _ = map.insert(code, file_n_path);
                    let file_n_path = map.get_mut(&code).unwrap();
                    Ok((&mut file_n_path.0, &file_n_path.1))
                } else {
                    bail!(format!("unknown validation code {}", code))
                }
            } else if let Some((ref mut file, ref path)) = *unknown {
                Ok((file, path))
            } else {
                *unknown = Some(Self::data_file_of(conf, Some("err_or_tmo"))?);
                let file_n_path = unknown.as_mut().unwrap();
                Ok((&mut file_n_path.0, &file_n_path.1))
            },
        }
    }

    /// Data file from a string.
    fn data_file_of(conf: &PlotConf, s: Option<&str>) -> Res<(File, PathBuf)> {
        let mut path = PathBuf::from(&conf.file);
        if let Some(s) = s {
            if let Some(stem) = path
                .file_stem()
                .map(|stem| stem.to_string_lossy().to_string())
            {
                path.set_file_name(&format!("{}_{}", stem, s))
            } else {
                bail!(format!("illegal plot file name `{}`", conf.bad(&conf.file)))
            }
        }
        let success = path.set_extension("data");
        if !success {
            bail!(format!("illegal plot file name `{}`", conf.bad(&conf.file)))
        }
        let mut file = conf.open_file_writer(&path).chain_err(|| {
            format!(
                "while opening data file `{}`",
                conf.sad(path.to_string_lossy())
            )
        })?;
        writeln!(
            file,
            "# Generated by {} v{}\n\n",
            crate_name!(),
            crate_version!()
        ).chain_err(|| {
            format!(
                "while writing to comparative data file `{}`",
                conf.sad(path.to_string_lossy())
            )
        })?;
        Ok((file, path))
    }

    /// Fold over data files.
    ///
    /// Parameters of the fold function:
    /// - accumulator,
    /// - validator code,
    /// - validator counter,
    /// - name of the validator,
    /// - path to the file.
    pub fn fold_data_paths<
        Acc,
        Fold: Fn(Acc, Option<i32>, usize, Option<&str>, &str) -> Res<Acc>,
    >(
        &self,
        mut init: Acc,
        fold: Fold,
    ) -> Res<Acc> {
        match *self {
            DataFileHandler::Merged { ref path, .. } => {
                fold(init, None, 1, None, &path.to_string_lossy().to_string())
            }
            DataFileHandler::Split {
                ref codes,
                ref map,
                ref unknown,
                ..
            } => {
                let mut count = 0;
                if let Some((_, ref path)) = *unknown {
                    init = fold(
                        init,
                        None,
                        0,
                        Some("??"),
                        &path.to_string_lossy().to_string(),
                    )?
                }
                // Incrementing here on purpose, this ensures that only unknown data
                // gets index 0.
                count += 1;
                for (code, &(_, ref path)) in map {
                    let vald = codes
                        .get(*code)
                        .ok_or_else(|| format!("unknown validation code {}", code))?;
                    init = fold(
                        init,
                        Some(*code),
                        count,
                        Some(&vald.graph),
                        &path.to_string_lossy().to_string(),
                    )?;
                    count += 1
                }
                Ok(init)
            }
        }
    }
}
