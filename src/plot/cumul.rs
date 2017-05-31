//! Cumulative plot generation.

use std::iter::Iterator ;

use common::* ;
use errors::* ;
use loading::ToolData ;


/// Generates the cumulative plot between several tools.
pub fn work(conf: & PlotConf, files: Vec<String>) -> Res<()> {
  log!{
    conf =>
      "Generating cumulative plot for {} tools...", files.len() ; {
        log!{
          conf, verb => "  loading {} data files...", files.len()
        }
      }
  }

  let mut tool_data = Vec::with_capacity( files.len() ) ;
  let mut empty_data = vec![] ;
  let mut bench_count = 0 ;
  for file in & files {
    let data = ToolData::cumul_of_file(conf, & file).chain_err(
      || format!(
        "while preparing for cumulative plot generation for `{}`", file
      )
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

  if tool_data.len() > 16 {
    warn!(
      conf => "only 16 colors defined, plot might fail"
    )
  }

  let mut _max_total_time = Duration::zero() ;
  // Cannot fail if the checks above are not changed.
  let mut _min_time = tool_data[0].res[0] ;
  for data in & tool_data {
    let (total_time, min) = data.cumul_write(conf) ? ;
    _max_total_time = ::std::cmp::max(_max_total_time, total_time) ;
    _min_time = ::std::cmp::min(_min_time, min)
  }

  log!{ conf, verb => "  writing plot file `{}`...", conf.emph(& conf.file) }

  let mut file = conf.open_file_writer(& conf.file).chain_err(
    || format!(
      "while opening cumul plot file `{}` (write)", conf.emph(& conf.file)
    )
  ) ? ;

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
  log!{ conf, verb => "  (output pdf file is `{}`)", conf.emph(& pdf_file) }

  file.write_all(
    format!(
      "# Generated by {} v{}\n", crate_name!(), crate_version!()
    ).as_bytes()
  ).and_then(
    |()| file.write_all( plot_prefix.as_bytes() )
  ).and_then(
    |()| if bench_count <= 10 {
      file.write_all( "set xtics 1\n".as_bytes() )
    } else {
      Ok(())
    }
  ).and_then(
    |()| file.write_all(
      format!(
        "set output \"{}\"\n\nplot \\\n",
        pdf_file,
      ).as_bytes()
    )
  ).chain_err(
    || format!(
      "while writing to cumulative plot file `{}`", conf.emph(& conf.file)
    )
  ) ? ;

  let mut data_iter = (1..).zip( tool_data.iter() ) ;

  if let Some( (index, data) ) = data_iter.next() {
    file.write_all(
      format!(
        "  \"{}\" using 2:1 w lp ls {} t '{} ({})'",
        data.file, index, data.tool.graph, data.res.len()
      ).as_bytes()
    ).chain_err(
      || format!(
        "while writing to plot file `{}`", conf.emph(& conf.file)
      )
    ) ? ;

    for (index, data) in data_iter {
      file.write_all(
        format!(
          ", \\\n  \"{}\" using 2:1 w lp ls {} t '{} ({})'",
          data.file, index, data.tool.graph, data.res.len()
        ).as_bytes()
      ).chain_err(
        || format!(
          "while writing to plot file `{}`", conf.emph(& conf.file)
        )
      ) ?
    }

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

    ()
  } else {
    // Should be unreachable.
    ()
  }

  log!{ conf => "Done" }

  Ok(())
}



static plot_prefix: & str = r#"

set border 3 linecolor rgbcolor "0x000000"
set key textcolor rgbcolor "0x000000"
set term pdf enhanced \
    font "Helvetica,15" \
    background rgb "0xFFFFFF"

set style line 1  dt 1 lw 1 ps 0.5 pi 15 lc rgb "0xCC0000"
set style line 2  dt 1 lw 1 ps 0.5 pi 15 lc rgb "0x66CC00"
set style line 3  dt 1 lw 1 ps 0.5 pi 15 lc rgb "0x00CCCC"
set style line 4  dt 1 lw 1 ps 0.5 pi 15 lc rgb "0xCC6600"
set style line 5  dt 1 lw 1 ps 0.5 pi 15 lc rgb "0x0000CC"
set style line 6  dt 1 lw 1 ps 0.5 pi 15 lc rgb "0xCC00CC"
set style line 7  dt 1 lw 1 ps 0.5 pi 15 lc rgb "0x999900"
set style line 8  dt 1 lw 1 ps 0.5 pi 15 lc rgb "0x606060"
set style line 9  dt 1 lw 1 ps 0.5 pi 15 lc rgb "0xCC0000"
set style line 10 dt 1 lw 1 ps 0.5 pi 15 lc rgb "0x66CC00"
set style line 11 dt 1 lw 1 ps 0.5 pi 15 lc rgb "0x00CCCC"
set style line 12 dt 1 lw 1 ps 0.5 pi 15 lc rgb "0xCC6600"
set style line 13 dt 1 lw 1 ps 0.5 pi 15 lc rgb "0x0000CC"
set style line 14 dt 1 lw 1 ps 0.5 pi 15 lc rgb "0xCC00CC"
set style line 15 dt 1 lw 1 ps 0.5 pi 15 lc rgb "0x999900"
set style line 16 dt 1 lw 1 ps 0.5 pi 15 lc rgb "0x606060"

set xtics nomirror
set ytics nomirror
set grid

set xlabel "Time in seconds (logscale)" textcolor rgbcolor "0x000000"
set ylabel "Benchmarks passed" textcolor rgbcolor "0x000000"

set key above samplen 2 font ",11"

set logscale x
set autoscale
"# ;
