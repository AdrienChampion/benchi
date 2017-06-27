//! Types to load and process bench results.

use common::* ;
use common::res::* ;
use common::plot::* ;
use errors::* ;
use consts::data::* ;

/// The results for a tool.
pub struct ToolData<T> {
  /// Tool configuration.
  pub tool: ToolConf,
  /// Timeout in micros read from the dump.
  pub timeout: Duration,
  /// File the data is dumped to.
  pub file: String,
  /// Sorted anonymous successes containing only the runtime in micros.
  pub res: T,
  /// Validator configuration.
  pub vald_conf: ValdConf,
}
impl<T> ToolData<T> {
  /// Creates a tool data from a file.
  #[inline]
  fn polymorphic_of_file<
    P: AsRef<Path>,
    F: Fn(
      Vec<(BenchIndex, BenchRes)>, Duration
    ) -> Res<T>
  >(conf: & PlotConf, path: P, treatment: F) -> Res<Self> {
    let file = ::std::fs::OpenOptions::new().read(true).open(& path).chain_err(
      || format!(
        "while opening data file `{}`", conf.sad(
          path.as_ref().to_str().expect(
            "weird path cannot be converted to string"
          )
        )
      )
    ) ? ;
    let reader = BufReader::new(file) ;
    let mut lines = LinesIter::mk( reader ) ;
    let (tool, mut line_cnt) = ToolConf::from_dump(& mut lines, 0).chain_err(
      || format!(
        "while loading data file `{}`", conf.sad(
          path.as_ref().to_str().expect(
            "weird path cannot be converted to string"
          )
        )
      )
    ) ? ;

    // Parse timeout.
    let timeout = lines.next().ok_or_else::<Error,_>(
      || "expected timeout info after tool info, found nothing".into()
    ).and_then(
      |line| line.chain_err::<_,Error>(
        || "while retrieving the line for timeout info".into()
      )
    ).and_then(
      |line| if let Some(cap) = ::consts::dump::timeout_re.captures(& line) {
        duration_of_time_re(cap)
      } else {
        Err(
          format!(
            "expected timeout info after tool info, found `{}`", line
          ).into()
        )
      }
    ) ? ;
    line_cnt += 1 ;

    // Parse validators, if any.
    let vald_conf = ValdConf::empty() ;

    let mut vec: Vec<(BenchIndex, _)> = Vec::with_capacity(100) ;
    'lines: for line in lines {
      line_cnt += 1 ;
      let line = line.chain_err(
        || format!("while reading data file for `{}`", conf.sad(& tool.name))
      ) ? ;
      if let Some(caps) = result_re.captures(& line) {
        let uid = usize::from_str(& caps["uid"]).chain_err(
          || format!("while parsing benchmark uid")
        ) ? ;
        let _bench = & caps["bench"] ;
        let data = & caps["res"] ;
        let vald = match & caps["vald"] {
          "?" => None,
          vald => Some(
            Validation::from_str(vald).chain_err(
              || format!("while parsing validation code")
            ) ?
          ),
        } ;
        if let Some(data) = BenchRes::of_str( data, vald ) ? {
          vec.push( (uid.into(), data) )
        } else {
          bail!(
            format!(
              "data on line {} is ill-formed: `{}`",
              conf.emph(& format!("{}", line_cnt)), data
            )
          )
        }
      }
    }

    let data_file = {
      let mut path = PathBuf::from(& conf.file) ;
      let data_file = path.file_stem().and_then(
        |file| file.to_str()
      ).map(
        |file| format!("{}_{}.data", file, tool.short)
      ).unwrap_or_else(
        || format!("{}.data", tool.short)
      ) ;
      path.pop() ;
      path.push(& data_file) ;
      if let Some(file) = path.to_str() { file.to_string() } else {
        bail!(
          format!("illegal data file path `{}`", conf.file)
        )
      }
    } ;

    Ok(
      ToolData {
        tool, timeout, file: data_file,
        res: treatment(vec, timeout)?, vald_conf
      }
    )
  }
}

impl ToolData<
  ( Duration, Duration, HashMap<BenchIndex, BenchRes> )
> {
  /// Creates a tool data for a comparative plot from a file. Returns the max
  /// bench index plus one, zero if there was no bench.
  #[inline]
  pub fn compare_of_file<P: AsRef<Path>>(
    conf: & PlotConf, path: P
  ) -> Res< (Self, usize) > {
    ToolData::polymorphic_of_file(
      conf, path, |vec, timeout| {
        let mut map = HashMap::with_capacity( vec.len() ) ;
        let mut max_time = Duration::zero() ;
        let mut min_time = None ;
        let mut tmo = None ;
        let mut max_index = None ;
        for (bench, data) in vec {
          max_index = Some(
            ::std::cmp::max(
              max_index.unwrap_or(* bench), * bench
            )
          ) ;
          match & data {
            & BenchRes::Success(time, _) => {
              max_time = ::std::cmp::max(max_time, time) ;
              min_time = Some(
                ::std::cmp::min(
                  min_time.unwrap_or(time), time
                )
              )
            }
            & BenchRes::Timeout => tmo = Some(timeout),
            _ => (),
          }
          let was_there = map.insert(bench, data) ;
          if let Some(_) = was_there {
            bail!(
              format!(
                "benchmark number {} appears twice",
                conf.bad( & format!("{}", bench) )
              )
            )
          }
        }
        Ok((
          min_time.unwrap_or( Duration::new(0, 1_000) ),
          tmo.unwrap_or(max_time),
          ( map, max_index.map(|max| max + 1).unwrap_or(0) )
        ))
      },
    ).map(
      |data| {
        let (time_1, time_2, (map, max)) = data.res ;
        let data = ToolData {
          tool: data.tool,
          timeout: data.timeout,
          file: data.file,
          res: (time_1, time_2, map),
          vald_conf: data.vald_conf,
        } ;
        (data, max)
      }
    )
  }

  /// Checks if the map is empty.
  #[inline]
  pub fn is_empty(& self) -> bool {
    self.res.2.is_empty()
  }

  /// Length of the map.
  #[inline]
  pub fn len(& self) -> usize {
    self.res.2.len()
  }

  /// Max time found in the map.
  #[inline]
  pub fn max_time(& self) -> Duration {
    self.res.1
  }

  /// Min time found in the map.
  #[inline]
  pub fn min_time(& self) -> Duration {
    self.res.0
  }

  /// Gets something from the map.
  #[inline]
  pub fn get(& self, index: BenchIndex) -> Option<& BenchRes> {
    self.res.2.get(& index)
  }
}

impl ToolData< Vec<Duration> > {
  /// Creates a tool data for a cumulative plot from a file.
  #[inline]
  pub fn cumul_of_file<P: AsRef<Path>>(
    conf: & PlotConf, path: P
  ) -> Res<Self> {
    ToolData::polymorphic_of_file(
      conf, path, |vec, _| {
        let mut res = Vec::with_capacity( vec.len() ) ;
        for (_, data) in vec {
          if let BenchRes::Success(time, _) = data {
            res.push(time)
          }
        }
        res.shrink_to_fit() ;
        res.sort() ;
        Ok(res)
      },
    )
  }

  /// Writes the cumulative data to `<conf.file>.<tool short name>.data`.
  ///
  /// Returns the total time and the time of the first element of res.
  pub fn cumul_write(& self, conf: & PlotConf) -> Res<(Duration, Duration)> {
    let path = PathBuf::from(& self.file) ;
    let mut tool_file = conf.open_file_writer(
      path.as_path()
    ).chain_err(
      || format!(
        "while opening file `{}`, data file for `{}`",
        conf.sad(& self.file), conf.emph( & self.tool.name )
      )
    ) ? ;
    self.tool.dump_info(& self.timeout, & mut tool_file).chain_err(
      || format!(
        "while dumping info for tool `{}` in data file `{}`",
        conf.emph( & self.tool.name ),
        conf.emph( & self.file )
      )
    ) ? ;
    let mut total_time = Duration::zero() ;
    let min_time = if self.res.is_empty() {
      Duration::zero()
    } else { self.res[0] } ;
    let mut cnt = 0 ;
    for time in self.acc_iter() {
      cnt += 1 ;
      total_time = time ;
      writeln!(tool_file, "{} {}", cnt, time.as_sec_str()).chain_err(
        || format!(
          "while writing data file `{}` for tool `{}`",
          conf.sad(& self.file), conf.emph( & self.tool.name )
        )
      ) ?
    }
    Ok((total_time, min_time))
  }

  /// Iterator on the **accumulation** of the runtimes in millis.
  #[inline]
  pub fn acc_iter<'a>(& 'a self) -> AccIter<'a> {
    AccIter { data: & self, curr: 0, acc: Duration::zero() }
  }
}


/// Iterator on some tool data.
pub struct AccIter<'a> {
  /// The data.
  data: & 'a ToolData< Vec<Duration> >,
  /// Current index.
  curr: usize,
  /// Accumulator.
  acc: Duration,
}
impl<'a> Iterator for AccIter<'a> {
  type Item = Duration ;
  fn next(& mut self) -> Option<Duration> {
    if self.curr >= self.data.res.len() { None } else {
      self.acc += self.data.res[self.curr] ;
      self.curr += 1 ;
      Some(self.acc)
    }
  }
}