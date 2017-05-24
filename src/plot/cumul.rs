//! Cumulative plot generation.

use std::iter::Iterator ;

use regex::Regex ;

use common::* ;
use errors::* ;


lazy_static!{
  #[doc = "Regex extracting the runtime of a benchmark line."]
  static ref runtime_re: Regex = Regex::new(
    r#"^\s*[0-9]*\s*"[^"]*"\s*([0-9]*)\.([0-9]*).*$"#
  ).unwrap() ;
}


/// The results for a tool.
pub struct ToolData {
  /// Tool configuration.
  pub tool: ToolConf,
  /// Sorted anonymous successes containing only the runtime in micros.
  res: Vec<u64>,
}
impl ToolData {
  /// Creates a tool data from a file.
  #[inline]
  pub fn of_file<P: AsRef<Path>>(conf: & GConf, path: P) -> Res<Self> {
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

    let mut vec = Vec::with_capacity(100) ;
    'lines: for line in lines {
      let line = line.chain_err(
        || format!("while reading data file for `{}`", conf.sad(& tool.name))
      ) ? ;
      if let Some(cap) = runtime_re.captures(& line) {
        use std::str::FromStr ;
        let secs = if let Some(secs) = cap.get(1) { secs } else {
          continue 'lines
        } ;
        let micros = if let Some(micros) = cap.get(2) { micros } else {
          continue 'lines
        } ;
        if let Ok(micros) = u64::from_str(
          & format!("{}{:0>3}", secs.as_str(), micros.as_str())
        ) {
          vec.push( micros )
        }
      }
    }

    Ok( Self::mk(tool, vec) )
  }
  /// Creates a tool data from some anonymous successes.
  #[inline]
  pub fn mk(tool: ToolConf, mut res: Vec<u64>) -> Self {
    res.sort() ;
    ToolData { tool, res }
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
  data: & 'a ToolData,
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


/// Generates the plots.
pub fn work(conf: & GConf, files: Vec<String>) -> Res<()> {
  log!{
    conf => "Generating cumulative plot for {} tools...", files.len()
  }

  let mut tool_data = Vec::with_capacity( files.len() ) ;
  let mut empty_data = vec![] ;
  for file in & files {
    let data = ToolData::of_file(conf, & file).chain_err(
      || format!("while preparing for cumulative plot generation")
    ) ? ;
    if ! data.res.is_empty() {
      tool_data.push(data)
    } else {
      warn!(
        conf =>
          "ignoring data for `{}`: everything is timeout or error",
          conf.bad(& data.tool.name)
      ) ;
      empty_data.push(data)
    }
  }

  println!("data:") ;
  for data in & tool_data {
    println!("  {} ({}):", data.tool.name, data.tool.short) ;
    print!(  "    ") ;
    for time in & data.res {
      print!(" {}", time)
    }
    println!("") ;
    print!(  "    ") ;
    for time in data.acc_iter() {
      print!(" {}", time)
    }
    println!(" (cumulative)") ;
    println!("")
  }

  Ok(())
}