//! Comparative plot.

use common::{
    plot::*,
    res::{DataFileHandler, RunRes, ToolRes},
    *,
};

/// Generates the comparative scatterplot between two tools.
pub fn work(conf: &PlotConf, file_1: &str, file_2: &str) -> Res<Option<String>> {
    let mut data = CmpPlotData::new(conf, file_1, file_2)?;

    let has_errs = data.res_1.err_count + data.res_2.err_count > 0;

    let tmo_time = data.get_tmo_time();
    let err_time = data.get_err_time();
    let min_time = data.get_min_time();

    let mut not_in_res_2 = vec![];
    let (mut dble_tmos, mut dble_errs) = (0, 0);

    let (res_1_name, res_2_name) = (data.res_1.tool.ident(), data.res_2.tool.ident());

    let mut code_to_count = HashMap::new();

    for (bench, bench_res_1) in data.res_1.benchs.drain() {
        if let Some(bench_res_2) = data.res_2.benchs.remove(&bench) {
            if bench_res_1.is_tmo() && bench_res_2.is_tmo() {
                dble_tmos += 1
            } else if bench_res_1.is_err() && bench_res_2.is_err() {
                dble_errs += 1
            }
            let code = if conf.merge {
                None
            } else {
                match (bench_res_1.code(), bench_res_2.code()) {
                    (Some(code_1), Some(code_2)) => if code_1 != code_2 {
                        warn!( conf =>
                            "the tools disagree on benchmark {}", conf.sad(
                                data.run_res.benchs.get(& bench).ok_or_else(
                                    || format!("[bug] unknown benchmark {}", bench)
                                )?
                            ) ;
                            "{} finished with {}, but {} finished with {}",
                            conf.emph(& res_1_name),
                            conf.sad(
                                & data.res_1.codes.get(code_1).ok_or_else(
                                    || format!(
                                        "[bug] unknown validation code {} for {}",
                                        code_1, res_1_name
                                    )
                                )?.graph
                            ),
                            conf.emph(& res_2_name),
                            conf.sad(
                                & data.res_2.codes.get(code_2).ok_or_else(
                                    || format!(
                                        "[bug] unknown validation code {} for {}",
                                        code_2, res_2_name
                                    )
                                )?.graph
                            ) ;
                            "skipping this benchmark"
                        );
                        continue;
                    } else {
                        Some(code_1)
                    },

                    (opt_1, opt_2) => if opt_1.is_some() {
                        opt_1
                    } else {
                        opt_2
                    },
                }
            };
            let (data_file, data_file_path) = data.files.file_of(code)?;
            writeln!(
                data_file,
                "{} {} {}",
                bench_res_1
                    .map(|time, _| time, || tmo_time, |_| err_time)
                    .as_sec_str(),
                bench_res_2
                    .map(|time, _| time, || tmo_time, |_| err_time)
                    .as_sec_str(),
                data.run_res
                    .benchs
                    .get(&bench)
                    .ok_or_else(|| format!("[bug] unknown benchmark {}", bench))?
            ).chain_err(|| {
                format!(
                    "while writing to comparative data file `{}`",
                    conf.sad(data_file_path.to_string_lossy())
                )
            })?;
            *code_to_count.entry(code).or_insert(0) += 1
        } else {
            not_in_res_2.push(bench)
        }
    }

    if !not_in_res_2.is_empty() || !data.res_2.benchs.is_empty() {
        warn!( conf =>
            {
                if ! not_in_res_2.is_empty() {
                    warn!(conf, line =>
                        "found {} benchmarks in `{}`'s data that are not in `{}`'s",
                        conf.sad(& format!("{}", not_in_res_2.len())),
                        conf.emph( data.res_1.tool.ident() ),
                        conf.emph( data.res_2.tool.ident() )
                    )
                }
                if ! data.res_2.benchs.is_empty() {
                    warn!(conf, line =>
                        "found {} benchmarks in `{}`'s data that are not in `{}`'s",
                        conf.sad(& format!("{}", data.res_2.benchs.len())),
                        conf.emph( data.res_2.tool.ident() ),
                        conf.emph( data.res_1.tool.ident() )
                    )
                }
            }
        )
    }

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

    log!{
      conf, verb =>
      "  writing plot file `{}`...", conf.emph(& conf.file) ;
      "  (output pdf file is `{}`)", conf.emph(& output_file) ;
    }

    let mut file = conf.open_file_writer(&conf.file).chain_err(|| {
        format!(
            "while opening comparative plot file `{}` (write)",
            conf.emph(&conf.file)
        )
    })?;

    let title = if dble_tmos + dble_errs > 0 && conf.title {
        format!(
            "set title '{} double timeout{}, {} double error{}' font ',13'\n\n",
            dble_tmos,
            if dble_tmos == 1 { "" } else { "s" },
            dble_errs,
            if dble_errs == 1 { "" } else { "s" },
        )
    } else {
        "".into()
    };

    let (vert_err_line, horz_err_line, max_range) = if !has_errs {
        (
            "".to_string(),
            "".to_string(),
            (tmo_time + tmo_time / 10).as_sec_str(),
        )
    } else {
        (
            format!(
                "\
# Error vertical line.
set arrow from {}, graph 0 to {}, graph 1 nohead ls 3\n\
      ",
                err_time.as_sec_str(),
                err_time.as_sec_str()
            ),
            format!(
                "
  {} t 'Error' with lines ls 3, \\\
      ",
                err_time.as_sec_str()
            ),
            (err_time + err_time / 10).as_sec_str(),
        )
    };

    file.write_all(
        format!(
            "# Generated by {} v{}\n\n{}\n",
            crate_name!(),
            crate_version!(),
            conf.fmt.compare_term()
        ).as_bytes(),
    ).and_then(|()| file.write_all(plot_prefix.as_bytes()))
    .and_then(|()| {
        file.write_all(
            format!(
                "
set output \"{}\"

{}set xlabel \"{}, {}/{} passed (seconds)\" textcolor rgbcolor \"0x000000\"
set ylabel \"{}, {}/{} passed (seconds)\" textcolor rgbcolor \"0x000000\"

set xrange [{}:{}]
set yrange [{}:{}]

# Timeout vertical line.
set arrow from {}, graph 0 to {}, graph 1 nohead ls 2
{}

set lmargin at screen 0.14 ;
set rmargin at screen 0.95 ;
set bmargin at screen 0.12 ;
set tmargin at screen {} ;

plot \\
  {} t 'Timeout' with lines ls 2, \\{}
  x notitle with lines ls 1\
        ",
                output_file,
                title,
                data.res_1.tool.graph_name(),
                data.res_1.suc_count,
                data.run_res.benchs.len(),
                data.res_2.tool.graph_name(),
                data.res_2.suc_count,
                data.run_res.benchs.len(),
                min_time.as_sec_str(),
                max_range,
                min_time.as_sec_str(),
                max_range,
                tmo_time.as_sec_str(),
                tmo_time.as_sec_str(),
                vert_err_line,
                if title.is_empty() { "0.95" } else { "0.90" },
                tmo_time.as_sec_str(),
                horz_err_line,
                // data_file_path.to_string_lossy()
            ).as_bytes(),
        )
    }).chain_err(|| {
        format!(
            "while writing to comparative plot file `{}`",
            conf.emph(&conf.file)
        )
    })?;

    data.files
        .fold_data_paths(&mut file, |file, code, index, desc, path| {
            write!(
                file,
                ", \\\n  '{}' using 1:2 {} with points ls {}",
                path,
                desc.map(|desc| format!(
                    "title '{} ({})'",
                    desc,
                    code_to_count.get(&code).cloned().unwrap_or(0)
                )).unwrap_or_else(|| "notitle".into()),
                index + 4
            ).chain_err(|| {
                format!(
                    "while writing to comparative plot file `{}`",
                    conf.emph(&conf.file)
                )
            }).map(|()| file)
        })?;

    Ok(Some(output_file))
}

/// Comparative plot handler.
struct CmpPlotData<'a> {
    /// Run information.
    run_res: RunRes,
    /// First tool result.
    res_1: ToolRes,
    /// Second tool result.
    res_2: ToolRes,
    /// Data file handler.
    files: DataFileHandler<'a>,
    /// Lowest runtime between both tools.
    lo_time: Duration,
    /// Highest runtime between both tools.
    hi_time: Duration,
}

impl<'a> CmpPlotData<'a> {
    /// Constructor.
    fn new(conf: &'a PlotConf, file_1: &str, file_2: &str) -> Res<Self> {
        log!{ conf => "  loading tool data..." }
        let mut run_res = RunRes::of_files(vec![file_1.into(), file_2.into()])?;

        if conf.no_errors {
            let dropped = run_res.rm_errs();
            log!{
              conf =>
                "  dropped {} benchmark{} for which one tool \
                or more failed (--no_errs on).",
                dropped, if dropped == 1 {""} else {"s"}
            }
        } else if conf.errs_as_tmos {
            let changed = run_res.errs_as_tmos();
            log!{
              conf =>
                "  changed {} error result{} to timeout{} (--errs_as_tmos on).",
                changed,
                if changed == 1 {""} else {"s"}, if changed == 1 {""} else {"s"}
            }
        }

        let files = DataFileHandler::new(conf, &run_res)?;

        let res_2 = if let Some(res) = run_res.tools.pop() {
            res
        } else {
            bail!("[bug] run result has no tool data")
        };
        let res_1 = if let Some(res) = run_res.tools.pop() {
            res
        } else {
            bail!("[bug] run result has only one tool run")
        };
        if !run_res.tools.is_empty() {
            bail!("[bug] run result has more than two tool runs")
        }

        if res_1.suc_count == 0 {
            warn!( conf =>
                "data for `{}`: everything is timeout or error",
                conf.bad( res_1.tool.ident() )
            )
        }
        if res_2.suc_count == 0 {
            warn!( conf =>
                "data for `{}`: everything is timeout or error",
                conf.bad( res_2.tool.ident() )
            )
        }

        let (lo_time, hi_time) = match (res_1.time_interval(), res_2.time_interval()) {
            (None, None) => (
                Duration::zero(),
                ::std::cmp::max(res_1.timeout, res_2.timeout),
            ),
            (Some((lo, hi)), None) => (lo, hi),
            (None, Some((lo, hi))) => (lo, hi),
            (Some((lo1, hi1)), Some((lo2, hi2))) => {
                (::std::cmp::min(lo1, lo2), ::std::cmp::max(hi1, hi2))
            }
        };

        Ok(CmpPlotData {
            run_res,
            res_1,
            res_2,
            files,
            lo_time,
            hi_time,
        })
    }

    /// Timeout time to use in the plot.
    fn get_tmo_time(&self) -> Duration {
        self.hi_time + self.hi_time / 10
    }
    /// Error time to use in the plot.
    fn get_err_time(&self) -> Duration {
        self.hi_time + self.hi_time / 5
    }
    /// Minimal time to use in the plot.
    fn get_min_time(&self) -> Duration {
        self.lo_time / 10
    }
}

static plot_prefix: &str = r#"

set border 3 linecolor rgbcolor "0x000000"
set xtics nomirror
set ytics nomirror
set grid
set key textcolor rgbcolor "0x000000"

set size ratio 1

# For y = x.
set style line 1 lt 1 dt 1 lw 1 pt 3 linecolor rgb "0xA0A0A0"
# For timeouts.
set style line 2 lt 1 dt 1 lw 2 pt 3 linecolor rgb "0xFF9933"
# For errors.
set style line 3 lt 1 dt 1 lw 2 pt 3 linecolor rgb "0xFF0000"

# For points.
set style line 4  dt 1 lw 1 pt 3  lc rgb "0xFF0000"
set style line 5  dt 1 lw 1 pt 1  lc rgb "0x0000CC"
set style line 6  dt 1 lw 1 pt 2  lc rgb "0x009900"
set style line 7  dt 1 lw 1 pt 8  lc rgb "0xFF8000"
set style line 8  dt 1 lw 1 pt 10 lc rgb "0x4C9900"
set style line 9  dt 1 lw 1 pt 12 lc rgb "0xCC00CC"
set style line 10 dt 1 lw 1 pt 14 lc rgb "0x999900"
set style line 11 dt 1 lw 1 pt 66 lc rgb "0x606060"
set style line 12 dt 1 lw 1 pt 67 lc rgb "0xCC0000"
set style line 13 dt 1 lw 1 pt 68 lc rgb "0x66CC00"
set style line 14 dt 1 lw 1 pt 69 lc rgb "0x00CCCC"
set style line 15 dt 1 lw 1 pt 72 lc rgb "0xCC6600"
set style line 16 dt 1 lw 1 pt 73 lc rgb "0x0000CC"
set style line 17 dt 1 lw 1 pt 74 lc rgb "0xCC00CC"
set style line 18 dt 1 lw 1 pt 75 lc rgb "0x999900"
set style line 19 dt 1 lw 1 pt 16 lc rgb "0x606060"

unset key
set logscale x
set format x "10^{%L}"
set logscale y
set format y "10^{%L}"

set key above samplen 2 font ",13"
"#;
