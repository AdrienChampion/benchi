//! Types representing results.

use rayon::slice::ParallelSliceMut ;

use common::* ;


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

  /// Returns the code of a result.
  pub fn code(& self) -> Option<Validation> {
    self.map( |_, res| res, || None, || None )
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
impl ValdConfExt for ToolRes {
  fn vald_conf(& self) -> & ValdConf { & self.vald_conf }
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
    times.par_sort_unstable() ;
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
        "while opening file `{}`, plot data file for `{}`",
        conf.emph( path.to_string_lossy() ), conf.emph( & self.tool.name )
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
    /// The `ValdConf` common to all tools.
    vald_conf: ValdConf,
    /// Map from error codes to data files and their path.
    map: HashMap<Validation, (File, PathBuf)>,
    /// File and path for unknowns (in case of double timeout / error).
    unknown: Option<(File, PathBuf)>,
    /// Conf.
    conf: & 'a PlotConf,
  },
}
impl<'a> DataFileHandler<'a> {
  /// Creates a handler from a RunRes and a conf.
  ///
  /// Error if not in `merged` mode and validation configuration don't match.
  pub fn mk(conf: & 'a PlotConf, run_res: & RunRes) -> Res<Self> {

    if conf.merge { // Merged, only one file needed.
      let (file, path) = Self::data_file_of(conf, None) ? ;
      Ok( DataFileHandler::Merged { file, path } )

    } else { // Not merging, check the validators make sense.
      let vald_conf = {
        let mut iter = run_res.tools.iter() ;
        if let Some(tool_res) = iter.next() {
          let vald_conf = tool_res.vald_conf.clone() ;
          for other_tool_res in iter {
            if other_tool_res.vald_conf != vald_conf {
              return (
                Err(
                  format!(
                    "If you still want to plot the data, consider using \
                    option `{}`: `benchi plot ... compare {} ...",
                    conf.happy("--merged"),
                    conf.emph("--merged on")
                  ).into()
                ) as Res<Self>
              ).chain_err(
                || format!(
                  "the data files most likely come from {}.",
                  conf.sad("different benchi runs")
                )
              ).chain_err(
                || format!(
                  "data for tools {} and {} do not agree on their validators,",
                  conf.bad(& tool_res.tool.name), conf.bad(& tool_res.tool.name)
                )
              )
            }
          }
          vald_conf
        } else {
          bail!("no data file provided")
        }
      } ;

      if vald_conf.is_empty() {
        // No validator provided, doing a merged.
        let (file, path) = Self::data_file_of(conf, None) ? ;
        Ok( DataFileHandler::Merged { file, path } )
      } else {
        let map = HashMap::with_capacity( vald_conf.len() ) ;
        Ok(
          DataFileHandler::Split { vald_conf, map, unknown: None, conf }
        )
      }
    }
  }

  /// Returns the data file corresponding to a validator.
  pub fn file_of(
    & mut self, code: Option<Validation>
  ) -> Res<(& mut File, & PathBuf)> {
    match * self {
      DataFileHandler::Merged { ref mut file, ref path } => Ok((file, path)),
      
      DataFileHandler::Split { ref mut map, .. }
      if code.map(|code| map.contains_key(& code)).unwrap_or(false) => {
        let file_n_path = map.get_mut(& code.unwrap()).unwrap() ;
        Ok( (& mut file_n_path.0, & file_n_path.1) )
      },
      
      DataFileHandler::Split {
        ref mut map, ref vald_conf, ref mut unknown, ref conf,
      } => if let Some(code) = code {
        // Not initialized yet, let's doodis.
        if let Some(vald) = vald_conf.get(code) {
          let file_n_path = Self::data_file_of(conf, Some(& vald.alias)) ? ;
          let _ = map.insert(code, file_n_path) ;
          let file_n_path = map.get_mut(& code).unwrap() ;
          Ok( (& mut file_n_path.0, & file_n_path.1) )
        } else {
          bail!( format!("unknown validation code {}", code) )
        }
      } else if let & mut Some((ref mut file, ref path)) = unknown {
        Ok((file, path))
      } else {
        * unknown = Some( Self::data_file_of(conf, Some("err_or_tmo")) ? ) ;
        let file_n_path = unknown.as_mut().unwrap() ;
        Ok( (& mut file_n_path.0, & file_n_path.1) )
      },
    }
  }

  /// Data file from a string.
  fn data_file_of(
    conf: & PlotConf, s: Option<& str>
  ) -> Res<(File, PathBuf)> {
    let mut path = PathBuf::from(& conf.file) ;
    if let Some(s) = s {
      if let Some(stem) = path.file_stem().map(
        |stem| stem.to_string_lossy().to_string()
      ) {
        path.set_file_name( & format!("{}_{}", stem, s) )
      } else {
        bail!(
          format!("illegal plot file name `{}`", conf.bad(& conf.file))
        )
      }
    }
    let success = path.set_extension("data") ;
    if ! success {
      bail!(
        format!("illegal plot file name `{}`", conf.bad(& conf.file))
      )
    }
    let mut file = conf.open_file_writer(& path).chain_err(
      || format!(
        "while opening data file `{}`",
        conf.sad( path.to_string_lossy() )
      )
    ) ? ;
    writeln!(
      file, "# Generated by {} v{}\n\n", crate_name!(), crate_version!()
    ).chain_err(
      || format!(
        "while writing to comparative data file `{}`",
        conf.sad( path.to_string_lossy() )
      )
    ) ? ;
    Ok( (file, path) )
  }

  /// Fold over data files.
  ///
  /// Parameters of the fold function:
  /// - accumulator,
  /// - validator counter,
  /// - name of the validator,
  /// - path to the file.
  pub fn fold_data_paths<
    Acc, Fold: Fn(Acc, usize, Option<& str>, & str) -> Res<Acc>
  >(& self, mut init: Acc, fold: Fold) -> Res<Acc> {
    match * self {
      DataFileHandler::Merged { ref path, .. } => fold(
        init, 1, None, & path.to_string_lossy().to_string()
      ),
      DataFileHandler::Split {
        ref vald_conf, ref map, ref unknown, ..
      } => {
        let mut count = 0 ;
        if let Some((_, ref path)) = * unknown {
          init = fold(
            init, 0, Some("??"), & path.to_string_lossy().to_string()
          ) ?
        }
        // Incrementing here on purpose, this ensures that only unknown data
        // gets index 0.
        count += 1 ;
        for ( code, & (_, ref path) ) in map {
          let vald = vald_conf.get(* code).ok_or_else(
            || format!("unknown validation code {}", code)
          ) ? ;
          init = fold(
            init, count, Some(& vald.desc),
            & path.to_string_lossy().to_string()
          ) ? ;
          count += 1
        }
        Ok(init)
      },
    }
  }


}