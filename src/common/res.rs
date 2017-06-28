//! Types representing results.

use common::* ;

/// Duration from a success regex match.
pub fn duration_of_time_re<'a>(caps: ::regex::Captures<'a>) -> Res<Duration> {
  u64::from_str( & caps["secs"] ).and_then(
    |secs| u32::from_str( & caps["nanos"] ).map(
      |nanos| Duration::new(secs, nanos)
    )
  ).chain_err(
    || format!("while parsing time string")
  )
}



/// Validation code.
pub type Validation = i32 ;

/// Result of a tool running on a benchmark.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BenchRes {
  /// Success with a time and an optional validation code.
  Success( Duration, Option<Validation> ),
  /// Timeout.
  Timeout,
  /// Error.
  Error,
}
impl BenchRes {

  /// Sets the validation code.
  pub fn set_code(& mut self, code: Validation) {
    match * self {
      BenchRes::Success(_, ref mut opt) => * opt = Some(code),
      _ => (),
    }
  }

  /// Map over the different types of data.
  pub fn map<
    T,
    FSucc: FnOnce(Duration, Option<Validation>) -> T,
    FTmo: FnOnce() -> T, FErr: FnOnce() -> T,
  >(& self, f_succ: FSucc, f_tmo: FTmo, f_err: FErr) -> T {
    match * self {
      BenchRes::Success(time, vald) => f_succ(time, vald),
      BenchRes::Timeout => f_tmo(),
      BenchRes::Error => f_err(),
    }
  }

  /// True if `self` is an error.
  pub fn is_err(& self) -> bool {
    * self == BenchRes::Error
  }
  /// True if `self` is a timeout.
  pub fn is_tmo(& self) -> bool {
    match * self {
      BenchRes::Timeout => true,
      _ => false
    }
  }
}


/// Stores the results for a tool on some benchmarks.
pub struct ToolRes {
  /// Tool configuration.
  pub tool: ToolConf,
  /// Timeout.
  pub timeout: Duration,
  /// Data file.
  pub file: String,
  /// Bench results.
  pub res: HashMap<BenchIndex, BenchRes>,
  /// Validation configuration.
  pub vald_conf: ValdConf,
  /// Number of successes.
  pub suc_count: usize,
  /// Number of errors.
  pub err_count: usize,
  /// Number of timeouts.
  pub tmo_count: usize,
}
impl ToolRes {
  /// Creates a tool result.
  pub fn mk(
    tool: ToolConf, timeout: Duration, file: String,
    res: HashMap<BenchIndex, BenchRes>, vald_conf: ValdConf
  ) -> Self {
    let (mut suc_count, mut err_count, mut tmo_count) = (0, 0, 0) ;
    for (_, res) in & res {
      match * res {
        BenchRes::Success(_, _) => suc_count += 1,
        BenchRes::Error => err_count += 1,
        BenchRes::Timeout => tmo_count += 1,
      }
    }
    ToolRes {
      tool, timeout, file, res, vald_conf, suc_count, err_count, tmo_count
    }
  }
  /// Loads a tool result from a file.
  fn of(file: String, run_res: & mut RunRes) -> Res<Self> {
    let read = ::std::fs::OpenOptions::new().read(true).open(& file).chain_err(
      || format!("while opening data file `{}`", file)
    ) ? ;
    let mut br = BufReader::new(read) ;
    let mut buffer = Vec::with_capacity(1207) ;
    br.read_to_end(& mut buffer).chain_err(
      || format!("error reading file `{}`", file)
    ) ? ;
    ::parse::dump(& buffer, file, run_res)
  }

  /// Returns the lowest and highest runtime.
  pub fn time_interval(& self) -> Option<(Duration, Duration)> {
    let mut interval = None ;
    for (_, res) in & self.res {
      if let BenchRes::Success(time, _) = * res {
        let (lo, hi) = interval.unwrap_or( (time, time) ) ;
        interval = Some((
          ::std::cmp::min(time, lo), ::std::cmp::max(time, hi)
        ))
      }
    }
    interval
  }

  /// Cumul data.
  ///
  /// Sorts (`<=`) all the success results.
  fn as_cumul_data(& self) -> Vec<Duration> {
    let mut times = self.res.iter().fold(
      Vec::with_capacity( self.res.len() ),
      |mut vec, (_, res)| {
        if let BenchRes::Success(time, _) = * res {
          vec.push(time)
        }
        vec
      }
    ) ;
    times.shrink_to_fit() ;
    times.sort() ;
    times
  }

  /// Writes the cumulative data to `<plot_basename>_<tool_short>.data`.
  ///
  /// Returns the number of benchmarks written. If zero, then the data file
  /// was **not** created.
  pub fn write_cumul(& self, conf: & PlotConf) -> Res<usize> {
    if self.res.is_empty() { return Ok(0) }

    let sorted = self.as_cumul_data() ;
    let count = sorted.len() ;

    let path = conf.data_file_path_of(& self.tool) ? ;
    let mut tool_file = conf.open_file_writer(
      path.as_path()
    ).chain_err(
      || format!(
        "while opening file `{}`, data file for `{}`",
        conf.sad(& self.file), conf.emph( & self.tool.name )
      )
    ) ? ;

    let mut acc = Duration::zero() ;

    for (count, time) in (0..).zip( sorted.into_iter() ) {
      acc += time ;
      writeln!(tool_file, "{} {}", count, acc.as_sec_str()).chain_err(
        || format!(
          "while writing cumul plot data for `{}` to `{}`",
          conf.emph(& self.tool.name),
          conf.sad( path.to_string_lossy() )
        )
      ) ?
    }

    Ok(count)
  }
}



/// Stores the result of a benchi run.
pub struct RunRes {
  /// Tools results.
  pub tools: Vec<ToolRes>,
  /// Benchs.
  pub benchs: HashMap<BenchIndex, String>,
}
impl RunRes {
  /// From some data files.
  pub fn of_files(files: Vec<String>) -> Res<Self> {
    let mut res = RunRes {
      tools: Vec::with_capacity( files.len() ),
      benchs: HashMap::with_capacity( 211 ),
    } ;
    for file in files {
      let tool_res = ToolRes::of(file, & mut res) ? ;
      res.tools.push(tool_res)
    }
    res.benchs.shrink_to_fit() ;
    Ok(res)
  }

  /// Removes all results for the benchmarks for which at least one of the
  /// tools error'd.
  ///
  /// Returns the number of benchmarks dropped.
  pub fn rm_errs(& mut self) -> usize {
    let mut tool_buf = Vec::with_capacity( self.tools.len() ) ;
    let mut to_rm = vec![] ;
    let mut removed = 0 ;
    while let Some(mut current) = self.tools.pop() {
      for (bench, res) in & current.res {
        if res.is_err() {
          to_rm.push(* bench)
        }
      }
      removed += to_rm.len() ;
      for bench in to_rm.drain(0..) {
        let _ = current.res.remove(& bench) ;
        for tool in tool_buf.iter_mut().chain( self.tools.iter_mut() ) {
          let _ = tool.res.remove(& bench) ;
        }
      }
    }
    removed
  }


  /// Turns all errors into timeouts.
  ///
  /// Returns the number of errors changed **over all tools**.
  pub fn errs_as_tmos(& mut self) -> usize {
    let mut changed = 0 ;
    for tool in self.tools.iter_mut() {
      for (_, res) in tool.res.iter_mut() {
        if res.is_err() {
          changed += 1 ;
          * res = BenchRes::Timeout
        }
      }
    }
    changed
  }


  /// Checks that a bench index corresponds to the right string. If there's no
  /// name associated to the index, adds it.
  pub fn check_bench_index(
    & mut self, bench: BenchIndex, name: String
  ) -> Res<()> {
    match self.benchs.get(& bench) {
      Some(bench_name) => if name == * bench_name {
        return Ok(())
      } else {
        let e: Res<()> = Err(
          format!(
            "but was previously seen as `{}`", bench_name
          ).into()
        ) ;
        return e.chain_err(
          || format!("benchmark with index {} is `{}`", bench, name)
        )
      },
      None => ()
    }
    self.benchs.insert(bench, name) ;
    Ok(())
  }
}