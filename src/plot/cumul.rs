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
  /// File the data is dumped to.
  file: String,
  /// Sorted anonymous successes containing only the runtime in micros.
  res: Vec<u64>,
}
impl ToolData {
  /// Creates a tool data from a file.
  #[inline]
  pub fn of_file<P: AsRef<Path>>(conf: & PlotConf, path: P) -> Res<Self> {
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

    let data_file = format!("{}.{}.data", conf.file, tool.short) ;

    Ok( Self::mk(tool, data_file, vec) )
  }
  /// Creates a tool data from some anonymous successes.
  #[inline]
  pub fn mk(tool: ToolConf, file: String, mut res: Vec<u64>) -> Self {
    res.sort() ;
    ToolData { tool, file, res }
  }

  /// Writes the cumulative data to `<conf.file>.<tool short name>.data`.
  ///
  /// Returns the total time and the time of the first element of res.
  pub fn write(& self, conf: & PlotConf) -> Res<(u64, u64)> {
    let path = PathBuf::from(& self.file) ;
    let mut tool_file = conf.open_file_writer(
      path.as_path()
    ).chain_err(
      || format!(
        "while opening file `{}`, data file for `{}`",
        conf.sad(& self.file), conf.emph( & self.tool.name )
      )
    ) ? ;
    self.tool.dump_info(& mut tool_file).chain_err(
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
pub fn work(conf: & PlotConf, files: Vec<String>) -> Res<()> {
  log!{
    conf => "Generating cumulative plot for {} tools...", files.len()
  }

  let mut tool_data = Vec::with_capacity( files.len() ) ;
  let mut empty_data = vec![] ;
  let mut bench_count = 0 ;
  for file in & files {
    let data = ToolData::of_file(conf, & file).chain_err(
      || format!("while preparing for cumulative plot generation")
    ) ? ;
    bench_count = ::std::cmp::max( bench_count, data.res.len() ) ;
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

  if tool_data.is_empty() {
    warn!(
      conf => "no data to plot, nothing to do"
    ) ;
    return Ok(())
  }

  if tool_data.len() > 5 {
    bail!(
      format!("not enough colors to create plot, I need to add more colors")
    )
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

  let mut max_total_time = 0 ;
  // Cannot fail if the checks above are not changed.
  let mut min_time = tool_data[0].res[0] ;
  for data in & tool_data {
    let (total_time, min) = data.write(conf) ? ;
    max_total_time = ::std::cmp::max(max_total_time, total_time) ;
    min_time = ::std::cmp::min(min_time, min)
  }

  log!(conf => "y in [{}, {}]", min_time, max_total_time) ;

  let mut file = conf.open_file_writer(& conf.file).chain_err(
    || format!(
      "while opening plot file `{}`", conf.emph(& conf.file)
    )
  ) ? ;

  file.write_all(
    format!(
      "# Generated by {} v{}", crate_name!(), crate_version!()
    ).as_bytes()
  ).and_then(
    |()| file.write_all( plot_prefix.as_bytes() )
  ).and_then(
    |()| file.write_all(
      format!(
        "set output \"{}.pdf\"\n\
        set xrange [1:{}]\n\
        set yrange [0.001:*]\n\nplot \\\n",
        conf.file,
        bench_count,
        // min_time - min_time / 10,
        // max_total_time + max_total_time / 10,
      ).as_bytes()
    )
  ).chain_err(
    || format!(
      "while writing to plot file `{}`", conf.emph(& conf.file)
    )
  ) ? ;

  let mut data_iter = tool_data.iter() ;

  if let Some(data) = data_iter.next() {
    file.write_all(
      format!(
        "  \"{}\" using 1:($2/1000000) smooth csplines t '{} ({})' w l",
        data.file, data.tool.graph, data.res.len()
      ).as_bytes()
    ).chain_err(
      || format!(
        "while writing to plot file `{}`", conf.emph(& conf.file)
      )
    ) ? ;
    // "output/cumul.plot.find_3.data" using 1:2 smooth cumulative t 'find 3 (4)'
    for data in data_iter {
      file.write_all(
        format!(
          ", \\\n  \"{}\" using 1:($2/1000000) smooth csplines t '{} ({})' w l",
          data.file, data.tool.graph, data.res.len()
        ).as_bytes()
      ).chain_err(
        || format!(
          "while writing to plot file `{}`", conf.emph(& conf.file)
        )
      ) ?
    }

    // Run gnuplot.
    Command::new("gnuplot").arg(& conf.file).output().chain_err(
      || format!(
        "while running gnuplot command on `{}`", conf.emph(& conf.file)
      )
    ) ? ;

    ()
  } else {
    // Should be unreachable.
    ()
  }

  Ok(())
}



static plot_prefix: & str = r#"

set border linecolor rgbcolor "0x000000"
set key textcolor rgbcolor "0x000000"
set term pdf enhanced dashed \
    font "Helvetica,19" \
    background rgb "0xFFFFFF"

set style line 1 lt 1 dt 1 lw 3 pt 3 linecolor rgb "0x000000"
set style line 2 lt 2 dt 2 lw 3 pt 3 linecolor rgb "0xCC0000"
set style line 3 lt 3 dt 4 lw 3 pt 3 linecolor rgb "0x008800"
set style line 4 lt 4 dt 3 lw 3 pt 3 linecolor rgb "0x0000CC"
set style line 5 lt 5 dt 5 lw 3 pt 3 linecolor rgb "0xFF8000"


set xlabel "Number of benchmarks solved" textcolor rgbcolor "0x000000"
set ylabel "Time in seconds" textcolor rgbcolor "0x000000" offset 3
set key bottom right
set xtics 1
set logscale y
"# ;


// set xrange [1:890]
// set yrange [1:*]

// plot \
//   "temp_0.plot" u 1:($2/1000.) t ' Kind 2 (866 solved)' w l ls 1, \
//   "temp_1.plot" u 1:($2/1000.) t ' jKind (863 solved)' w l ls 2, \
//   "temp_2.plot" u 1:($2/1000.) t ' NuXmv (842 solved)' w l ls 3, \
//   "temp_3.plot" u 1:($2/1000.) t ' PKind (780 solved)' w l ls 4, \
//   "temp_4.plot" u 1:($2/1000.) t ' Zustre (845 solved)' w l ls 5
// "# ;