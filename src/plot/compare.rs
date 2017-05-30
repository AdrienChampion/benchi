//! Comparison plot.

use common::* ;
use errors::* ;
use loading::{ Data, ToolData } ;

/// Generates the comparison scatterplot between two tools.
pub fn work(conf: & PlotConf, file_1: String, file_2: String) -> Res<()> {
  log!{
    conf => "Generating comparison scatterplot..." ; {
      log!{ conf, verb => "  loading data files..." }
    }
  }

  let data_1 = ToolData::compare_of_file(
    conf, & file_1
  ).chain_err(
    || format!("while preparing for comparison scatterplot generation")
  ) ? ;
  if data_1.is_empty() {
    warn!(
      conf =>
        "data for `{}`: everything is timeout or error,",
        conf.bad(& data_1.tool.name) ;
        "a cumulative plot would be more informative..."
    )
  }

  let data_2 = ToolData::compare_of_file(
    conf, & file_2
  ).chain_err(
    || format!("while preparing for comparison scatterplot generation")
  ) ? ;
  if data_2.is_empty() {
    warn!(
      conf =>
        "data for `{}`: everything is timeout or error",
        conf.bad(& data_2.tool.name) ;
        "a cumulative plot would be more informative..."
    )
  }

  let bench_count = ::std::cmp::max( data_1.len(), data_2.len() ) ;
  if data_1.len() != data_2.len() {
    warn!(
      conf =>
        "File `{}` has {} benchmarks, but",
        conf.emph(& file_1), conf.sad(& format!("{}", data_1.len())) ;
        "file `{}` has {} of them.",
        conf.emph(& file_2), conf.sad(& format!("{}", data_2.len())) ;
        "Data files from the same run \
        should have the same number of benchmarks" ;
        "unless there was an error or it was cancelled." ;
        "Missing benchmarks from one or the other will be treated as errors."
    )
  }
  let max_time = ::std::cmp::max( data_1.max_time(), data_2.max_time() ) ;
  // let tmo_time_1 = ::std::cmp::max(
  //   data_1.max_time() + data_1.max_time() / 10, data_1.max_time() + 1_000_000
  // ) ;
  // let error_time_1 = ::std::cmp::max(
  //   data_1.max_time() + data_1.max_time() / 5, data_1.max_time() + 2_000_000
  // ) ;
  // let tmo_time_2 = ::std::cmp::max(
  //   data_2.max_time() + data_2.max_time() / 10, data_2.max_time() + 1_000_000
  // ) ;
  // let error_time_2 = ::std::cmp::max(
  //   data_2.max_time() + data_2.max_time() / 5, data_2.max_time() + 2_000_000
  // ) ;
  let tmo_time_1 = ::std::cmp::max(
    max_time + max_time / 4, max_time + 1_000_000
  ) ;
  let tmo_time_2 = tmo_time_1 ;
  let error_time_1 = ::std::cmp::max(
    max_time + max_time / 2, max_time + 2_000_000
  ) ;
  let error_time_2 = error_time_1 ;


  let data_file = format!("{}.data", conf.file) ;
  log!{
    conf, verb => "  writing data file `{}`...", conf.emph(& data_file)
  }
  let mut file = conf.open_file_writer(& data_file).chain_err(
    || format!(
      "while opening comparison data file `{}` (write)", conf.emph(& data_file)
    )
  ) ? ;
  file.write_all(
    format!(
      "# Generated by {} v{}\n\n", crate_name!(), crate_version!()
    ).as_bytes()
  ).and_then(
    |()| file.write_all(
      format!(
        "# `{}` VERSUS `{}`\n", data_1.tool.name, data_2.tool.name
      ).as_bytes()
    )
  ).chain_err(
    || format!(
      "while writing to comparison data file `{}`", conf.emph(& data_file)
    )
  ) ? ;
  // Count double timeouts, errors and double errors.
  let (
    mut dble_timeouts, mut timeouts_1, mut timeouts_2,
    mut dble_errors, mut errors_1, mut errors_2
  ) = (0, 0, 0, 0, 0, 0) ;
  'iter_benchs: for index in BenchIndex::iter(bench_count) {
    match ( data_1.get(index).cloned(), data_2.get(index).cloned() ) {
      // Double error. Maybe? Dunno. Count but ignore I guess.
      (None, None) => {
        warn!{
          conf =>
          "benchmark with uid {} appears in neither of the data files",
          conf.emph( & format!("{}", * index) )
        }
        dble_errors += 1 ;
        errors_1 += 1 ;
        errors_2 += 1
      },
      (d_1, d_2) => {
        let (d_1, d_2) = (
          d_1.unwrap_or( Data::Error ), d_2.unwrap_or( Data::Error )
        ) ;
        if d_1.is_err() && d_2.is_err() {
          dble_errors += 1 ;
          errors_1 += 1 ;
          errors_2 += 1 ;
          continue 'iter_benchs
        }
        if d_1.is_tmo() && d_2.is_tmo() {
          dble_timeouts += 1 ;
          timeouts_1 += 1 ;
          timeouts_2 += 1 ;
          continue 'iter_benchs
        }

        let time_1 = d_1.map(
          |time| time,
          |_| {
            timeouts_1 += 1 ;
            tmo_time_1
          }, || {
            errors_1 += 1 ;
            error_time_1
          }
        ) ;
        let time_2 = d_2.map(
          |time| time,
          |_| {
            timeouts_2 += 1 ;
            tmo_time_2
          }, || {
            errors_2 += 1 ;
            error_time_2
          }
        ) ;

        file.write_all(
          format!("{} {}\n", time_1, time_2).as_bytes()
        ).chain_err(
          || format!(
            "while writing to comparison data file `{}`",
            conf.emph(& data_file)
          )
        ) ?
      },
    }
  }


  let min_time = ::std::cmp::min(
    data_1.min_time(), data_2.min_time()
  ) ;
  let min_time = min_time - min_time / 10 ;


  let pdf_file = {
    let mut path = PathBuf::from(& conf.file) ;
    let worked = path.set_extension("pdf") ;
    if ! worked {
      bail!(
        format!("illegal plot file `{}`", conf.file)
      )
    }
    if let Some(s) = path.to_str() {
      s.to_string()
    } else {
      bail!(
        format!("illegal plot file `{}`", conf.file)
      )
    }
  } ;

  log!{
    conf, verb =>
    "  writing plot file `{}`...", conf.emph(& conf.file) ;
    "  (output pdf file is `{}`)", conf.emph(& pdf_file) ;
  }

  let mut file = conf.open_file_writer(& conf.file).chain_err(
    || format!(
      "while opening comparison plot file `{}` (write)", conf.emph(& conf.file)
    )
  ) ? ;

  let _title = if timeouts_1 + errors_1 + timeouts_2 + errors_2 > 0 {
    let mut title = format!(
      "\
        {}: {} timeouts and {} errors\\n\
        {}: {} timeouts and {} errors\
      ",
      data_1.tool.name, timeouts_1, errors_1,
      data_2.tool.name, timeouts_2, errors_2,
    ) ;
    if dble_timeouts > 0 {
      title = format!(
        "{}\\n{} double timeouts (both timeout-ed)", title, dble_timeouts
      )
    }
    if dble_errors > 0 {
      title = format!(
        "{}\\n{} double errors (both failed)", title, dble_timeouts
      )
    }
    format!("set title \"{}\"", title)
  } else { "".into() } ;

  let err_time = error_time_1 as f64 / 1_000_000.0 ;
  let tmo_time = tmo_time_1 as f64 / 1_000_000.0 ;

  // println!("err time: {}, tmo time: {}", err_time, tmo_time) ;

  file.write_all(
    format!(
      "# Generated by {} v{}\n", crate_name!(), crate_version!()
    ).as_bytes()
  ).and_then(
    |()| file.write_all( plot_prefix.as_bytes() )
  ).and_then(
    |()| file.write_all(
      format!(
        "
set output \"{}\"

set xlabel \"{} (seconds, logscale)\" textcolor rgbcolor \"0x000000\"
set ylabel \"{} (seconds, logscale)\" textcolor rgbcolor \"0x000000\"

set xrange [{}:{}]
set yrange [{}:{}]

# Timeout vertical line.
set arrow from {}, graph 0 to {}, graph 1 nohead ls 3
# Error vertical line.
set arrow from {}, graph 0 to {}, graph 1 nohead ls 4

plot \\
  {} t 'Timeout' with lines ls 3, \\
  {} t 'Error' with lines ls 4, \\
  x notitle with lines ls 2, \\
  '{}' using ($1/1000000):($2/1000000) notitle with points ls 1
\
        ", pdf_file,
        data_1.tool.name, data_2.tool.name,
        min_time as f64 / 1_000_000.0, err_time + err_time / 10.0,
        min_time as f64 / 1_000_000.0, err_time + err_time / 10.0,
        tmo_time, tmo_time,
        err_time, err_time,
        tmo_time, err_time,
        data_file
      ).as_bytes()
    )
  ).chain_err(
    || format!(
      "while writing to comparison plot file `{}`", conf.emph(& conf.file)
    )
  ) ? ;

  if conf.pdf {
    log!{ conf, verb => "  running gnuplot..." }
    // Run gnuplot.
    let status = Command::new("gnuplot").arg(& conf.file).status().chain_err(
      || format!(
        "while running gnuplot command on `{}`", conf.emph(& conf.file)
      )
    ) ? ;

    if ! status.success() {
      bail!(
        format!("gnuplot failed on plot file `{}`", conf.emph(& conf.file))
      )
    }

    if let Some(ref cmd) = conf.then {
      log!{ conf, verb => "  running user-provided command" }
      let status = Command::new(cmd).arg(& pdf_file).status().chain_err(
        || format!(
          "while running `{} {}` (user-provided command)",
          conf.emph(cmd), conf.emph(& pdf_file)
        )
      ) ? ;
      if ! status.success() {
        bail!(
          format!(
            "failure on user-provided command `{} {}`",
            conf.emph(cmd), conf.emph(& pdf_file)
          )
        )
      }
    }
  }

  log!{ conf => "Done" }

  Ok(())
}



static plot_prefix: & str = r#"

set border 3 linecolor rgbcolor "0x000000"
set xtics nomirror
set ytics nomirror
set grid
set key textcolor rgbcolor "0x000000"
set term pdf enhanced \
    font "Helvetica,15" \
    background rgb "0xFFFFFF"

set size ratio 1

# For points.
set style line 1 lt 1 dt 1 lw 2 pt 1 linecolor rgb "0x3333FF"
# For y = x.
set style line 2 lt 1 dt 1 lw 1 pt 3 linecolor rgb "0xA0A0A0"
# For timeouts.
set style line 3 lt 1 dt 1 lw 2 pt 3 linecolor rgb "0xFF9933"
# For errors.
set style line 4 lt 1 dt 1 lw 2 pt 3 linecolor rgb "0xFF0000"

unset key
set logscale x
set logscale y

set key above maxrows 1 samplen 2 font ",11"
"# ;