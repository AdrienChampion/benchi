//! Cumulative plot generation.

prelude!(plot::*, common::res::RunRes);

/// Generates the cumulative plot between several tools.
pub fn work(conf: &PlotConf, files: Vec<String>) -> Res<Option<String>> {
    log! { conf => "  loading tool data..." }
    let mut run_res = RunRes::of_files(files.clone())?;

    if conf.no_errors {
        let dropped = run_res.rm_errs();
        log! {
          conf =>
            "  dropped {} benchmark{} for which one tool \
            or more failed (--no_errs on).",
            dropped, if dropped == 1 {""} else {"s"}
        }
    } else if conf.errs_as_tmos {
        let changed = run_res.errs_as_tmos();
        log! {
          conf =>
            "  changed {} error result{} to timeout{} (--errs_as_tmos on).",
            changed,
            if changed == 1 {""} else {"s"}, if changed == 1 {""} else {"s"}
        }
    }

    let mut tool_data = Vec::with_capacity(run_res.tools.len());
    let mut empty_data = vec![];
    let mut bench_count = 0;

    for tool_res in &run_res.tools {
        let count = tool_res.write_cumul(conf)?;
        if count == 0 {
            warn!(
              conf =>
                "ignoring data for `{}`: everything is timeout or error",
                conf.sad(& tool_res.tool.name)
            );
            empty_data.push(tool_res)
        } else {
            bench_count = ::std::cmp::max(bench_count, count);
            tool_data.push(tool_res)
        }
    }

    if tool_data.is_empty() {
        warn!(
          conf => "no data to plot, nothing to do"
        );
        return Ok(None);
    }

    if tool_data.len() > 16 {
        warn!(
          conf => "only 16 colors defined, plot might fail"
        )
    }

    log! { conf, verb => "  writing plot file `{}`...", conf.emph(& conf.file) }

    let mut file = conf.open_file_writer(&conf.file).chain_err(|| {
        format!(
            "while opening cumul plot file `{}` (write)",
            conf.emph(&conf.file)
        )
    })?;

    let output_file = {
        let mut path = PathBuf::from(&conf.file);
        let worked = path.set_extension(conf.fmt.ext());
        if !worked {
            bail!(format!("illegal plot file `{}`", conf.file))
        }
        if let Some(s) = path.to_str() {
            s.to_string()
        } else {
            bail!(format!("illegal plot file `{}`", conf.file))
        }
    };
    log! {
      conf, verb => "  > output pdf file is `{}`", conf.emph(& output_file)
    }

    file.write_all(
        format!(
            "# Generated by {} v{}\n\n{}\n",
            clap_lib::crate_name!(),
            clap_lib::crate_version!(),
            conf.fmt.term()
        )
        .as_bytes(),
    )
    .and_then(|()| file.write_all(plot_prefix.as_bytes()))
    .and_then(|()| {
        file.write_all(
            format!(
                r#"
set xlabel "Benchmarks passed (of {})" textcolor rgbcolor "0x000000"
set ylabel "Time in seconds (logscale)" textcolor rgbcolor "0x000000"


"#,
                run_res.benchs.len()
            )
            .as_bytes(),
        )
    })
    .and_then(|()| dump_linestyles(&mut file, bench_count))
    .and_then(|()| {
        if bench_count <= 10 {
            file.write_all("set xtics 1\n".as_bytes())
        } else {
            Ok(())
        }
    })
    .and_then(|()| {
        file.write_all(format!("set output \"{}\"\n\nplot \\\n", output_file,).as_bytes())
    })
    .chain_err(|| {
        format!(
            "while writing to cumulative plot file `{}`",
            conf.emph(&conf.file)
        )
    })?;

    let mut data_iter = (1..).zip(tool_data.iter());

    if let Some((index, tool_res)) = data_iter.next() {
        let data_file = conf.data_file_path_of(&tool_res.tool)?;
        file.write_all(
            format!(
                "  \"{}\" using 1:2 w lp ls {} t '{} ({})'",
                data_file.to_string_lossy(),
                index,
                tool_res.tool.graph,
                tool_res.suc_count
            )
            .as_bytes(),
        )
        .chain_err(|| format!("while writing to plot file `{}`", conf.emph(&conf.file)))?;

        for (index, tool_res) in data_iter {
            let data_file = conf.data_file_path_of(&tool_res.tool)?;
            file.write_all(
                format!(
                    ", \\\n  \"{}\" using 1:2 w lp ls {} t '{} ({})'",
                    data_file.to_string_lossy(),
                    index,
                    tool_res.tool.graph,
                    tool_res.suc_count
                )
                .as_bytes(),
            )
            .chain_err(|| format!("while writing to plot file `{}`", conf.emph(&conf.file)))?
        }

        ()
    } else {
        // Should be unreachable.
        ()
    }

    Ok(Some(output_file))
}

static plot_prefix: &str = r#"

set border 3 linecolor rgbcolor "0x000000"
set key textcolor rgbcolor "0x000000"

set xtics nomirror
set ytics nomirror
set grid

set key above samplen 2 font ",15"

set logscale y
set autoscale
set format y "10^{%L}"
"#;

/// Dumps the linestyles for the plot.
fn dump_linestyles<W: Write>(w: &mut W, bench_count: usize) -> ::std::io::Result<()> {
    write!(
        w,
        "\
set style line 1  dt 1 lw 1 ps 0.5 pi {0} lc rgb \"0xCC0000\"
set style line 2  dt 1 lw 1 ps 0.5 pi {0} lc rgb \"0x4C9900\"
set style line 3  dt 1 lw 1 ps 0.5 pi {0} lc rgb \"0x00CCCC\"
set style line 4  dt 1 lw 1 ps 0.5 pi {0} lc rgb \"0xCC6600\"
set style line 5  dt 1 lw 1 ps 0.5 pi {0} lc rgb \"0x0000CC\"
set style line 6  dt 1 lw 1 ps 0.5 pi {0} lc rgb \"0xCC00CC\"
set style line 7  dt 1 lw 1 ps 0.5 pi {0} lc rgb \"0x999900\"
set style line 8  dt 1 lw 1 ps 0.5 pi {0} lc rgb \"0x606060\"
set style line 9  dt 1 lw 1 ps 0.5 pi {0} lc rgb \"0xCC0000\"
set style line 10 dt 1 lw 1 ps 0.5 pi {0} lc rgb \"0x66CC00\"
set style line 11 dt 1 lw 1 ps 0.5 pi {0} lc rgb \"0x00CCCC\"
set style line 12 dt 1 lw 1 ps 0.5 pi {0} lc rgb \"0xCC6600\"
set style line 13 dt 1 lw 1 ps 0.5 pi {0} lc rgb \"0x0000CC\"
set style line 14 dt 1 lw 1 ps 0.5 pi {0} lc rgb \"0xCC00CC\"
set style line 15 dt 1 lw 1 ps 0.5 pi {0} lc rgb \"0x999900\"
set style line 16 dt 1 lw 1 ps 0.5 pi {0} lc rgb \"0x606060\"
\
    ",
        bench_count / 20
    )
}
