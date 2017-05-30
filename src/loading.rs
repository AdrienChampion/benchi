//! Types to load and process bench results.

use common::* ;
use errors::* ;
use consts::re::data::* ;

/// Seconds and microseconds from a regex match.
fn micros_of_time_re<'a>(cap: ::regex::Captures<'a>) -> Res<u64> {
  use std::str::FromStr ;
  let s = cap.get(1).and_then(
    |secs| {
      let micros = cap.get(2) ;
      micros.map(|micros| (secs, micros))
    }
  ).map(
    |(secs, micros)| u64::from_str(
      & format!("{}{}", secs.as_str(), micros.as_str())
    )
  ) ;
  match s {
    None | Some( Err(_) ) => bail!(
      "sorry, internal problem with regex `data_success_re`, \
      please notify the developer"
    ),
    Some( Ok(val) ) => Ok(val),
  }
}

/// Result of a tool running on a benchmark.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Data {
  /// Success with a time in microseconds.
  Success(u64),
  /// Timeout.
  Timeout(u64),
  /// Error.
  Error,
}
impl Data {
  /// Parses a string. Error on regex match but conversion fail. None if no
  /// regex match.
  pub fn of_str(s: & str) -> Res< Option<Data> > {
    use std::str::FromStr ;
    if let Some(cap) = data_success_re.captures(s) {
      return micros_of_time_re(cap).map(
        |val| Some( Data::Success(val) )
      )
    }
    
    if let Some(cap) = data_timeout_re.captures(s) {
      let s = cap.get(1).and_then(
        |secs| {
          let micros = cap.get(2) ;
          micros.map(|micros| (secs, micros))
        }
      ).map(
        |(secs, micros)| u64::from_str(
          & format!("{}{}", secs.as_str(), micros.as_str())
        )
      ) ;
      match s {
        None | Some( Err(_) ) => bail!(
          "sorry, internal problem with regex `data_timeout_re`, \
          please notify the developer"
        ),
        Some( Ok(val) ) => return Ok(
          Some( Data::Timeout(val) )
        ),
      }
    }

    if s == "error" {
      return Ok( Some(Data::Error) )
    }

    Ok(None)
  }

  /// Ternary map (?).
  pub fn map<
    T, FSucc: FnOnce(u64) -> T, FTmo: FnOnce(u64) -> T, FErr: FnOnce() -> T,
  >(& self, f_succ: FSucc, f_tmo: FTmo, f_err: FErr) -> T {
    match * self {
      Data::Success(time) => f_succ(time),
      Data::Timeout(time) => f_tmo(time),
      Data::Error => f_err(),
    }
  }

  /// True if `self` is an error.
  pub fn is_err(& self) -> bool {
    * self == Data::Error
  }
  /// True if `self` is a timeout.
  pub fn is_tmo(& self) -> bool {
    match * self {
      Data::Timeout(_) => true,
      _ => false
    }
  }
}

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
}
impl<T> ToolData<T> {
  /// Creates a tool data from a file.
  #[inline]
  fn polymorphic_of_file<
    P: AsRef<Path>, F: Fn( Vec<(BenchIndex, Data)> ) -> T
  >(
    conf: & PlotConf, path: P, treatment: F
  ) -> Res<Self> {
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
    let mut lines = reader.lines() ;
    let tool = ToolConf::from_dump(& mut lines).chain_err(
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
      |line| if let Some(cap) = ::consts::re::dump::timeout.captures(& line) {
        micros_of_time_re(cap).map(
          |micros| {
            let secs = micros / 1_000_000 ;
            Duration::new(secs, (micros - secs * 1_000_000) as u32)
          }
        )
      } else {
        Err(
          format!(
            "expected timeout info after tool info, found `{}`", line
          ).into()
        )
      }
    ) ? ;

    let mut vec: Vec<(BenchIndex, _)> = Vec::with_capacity(100) ;
    let mut line_cnt = 0 ;
    'lines: for line in lines {
      use std::str::FromStr ;
      line_cnt += 1 ;
      let line = line.chain_err(
        || format!("while reading data file for `{}`", conf.sad(& tool.name))
      ) ? ;
      if let Some(cap) = runtime_re.captures(& line) {
        let uid = cap.get(1).ok_or_else::<Error,_>(
          ||
            "sorry, internal problem with regex `runtime_re`, \
            please notify the developer".into()
        ) ? ;
        let uid = usize::from_str( uid.as_str() ).chain_err(
          || "sorry, internal problem with regex `runtime_re`, \
            please notify the developer"
        ) ? ;
        let data = cap.get(3).ok_or_else::<Error, _>(
          ||
            "sorry, internal problem with regex `runtime_re`, \
            please notify the developer".into()
        ) ? ;
        if let Some(data) = Data::of_str( data.as_str() ) ? {
          vec.push( (uid.into(), data) )
        } else {
          bail!(
            format!(
              "data on line {} is ill-formed",
              conf.emph(& format!("{}", line_cnt))
            )
          )
        }
      }
    }

    let data_file = format!(
      "{}.{}.data", conf.file, tool.short
    ) ;

    Ok(
      ToolData {
        tool, timeout, file: data_file, res: treatment(vec)
      }
    )
  }
}

impl ToolData< (u64, u64, HashMap<BenchIndex, Data>) > {
  /// Creates a tool data for a comparative plot from a file.
  #[inline]
  pub fn compare_of_file<P: AsRef<Path>>(
    conf: & PlotConf, path: P
  ) -> Res<Self> {
    ToolData::polymorphic_of_file(
      conf, path, |vec| {
        let mut map = HashMap::with_capacity( vec.len() ) ;
        let mut timeout = None ;
        let mut max_time = 0 ;
        let mut min_time = None ;
        for (bench, data) in vec {
          match & data {
            & Data::Success(time) => {
              max_time = ::std::cmp::max(max_time, time) ;
              min_time = Some(
                ::std::cmp::min(
                  min_time.unwrap_or(time), time
                )
              )
            }
            & Data::Timeout(to) => timeout = Some(to),
            _ => (),
          }
          let was_there = map.insert(bench, data) ;
          assert!( was_there.is_none() )
        }
        ( min_time.unwrap_or(1), timeout.unwrap_or(max_time), map )
      },
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
  pub fn max_time(& self) -> u64 {
    self.res.1
  }

  /// Min time found in the map.
  #[inline]
  pub fn min_time(& self) -> u64 {
    self.res.0
  }

  /// Gets something from the map.
  #[inline]
  pub fn get(& self, index: BenchIndex) -> Option<& Data> {
    self.res.2.get(& index)
  }
}

impl ToolData< Vec<u64> > {
  /// Creates a tool data for a cumulative plot from a file.
  #[inline]
  pub fn cumul_of_file<P: AsRef<Path>>(
    conf: & PlotConf, path: P
  ) -> Res<Self> {
    ToolData::polymorphic_of_file(
      conf, path, |vec| {
        let mut res = Vec::with_capacity( vec.len() ) ;
        for (_, data) in vec {
          if let Data::Success(time) = data {
            res.push(time)
          }
        }
        res.shrink_to_fit() ;
        res.sort() ;
        res
      },
    )
  }

  /// Writes the cumulative data to `<conf.file>.<tool short name>.data`.
  ///
  /// Returns the total time and the time of the first element of res.
  pub fn cumul_write(& self, conf: & PlotConf) -> Res<(u64, u64)> {
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
    let mut total_time = 0 ;
    let min_time = if self.res.is_empty() { 0 } else { self.res[0] } ;
    let mut cnt = 0 ;
    for time in self.acc_iter() {
      cnt += 1 ;
      total_time = time ;
      writeln!(tool_file, "{} {}", cnt, time).chain_err(
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
    AccIter { data: & self, curr: 0, acc: 0u64 }
  }
}


/// Iterator on some tool data.
pub struct AccIter<'a> {
  /// The data.
  data: & 'a ToolData< Vec<u64> >,
  /// Current index.
  curr: usize,
  /// Accumulator.
  acc: u64,
}
impl<'a> Iterator for AccIter<'a> {
  type Item = u64 ;
  fn next(& mut self) -> Option<u64> {
    if self.curr >= self.data.res.len() { None } else {
      self.acc += self.data.res[self.curr] ;
      self.curr += 1 ;
      Some(self.acc)
    }
  }
}