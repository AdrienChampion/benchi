//! Plot basic types and helpers.

prelude!();

/// Plot configuration.
pub struct PlotConf {
    /// Output file.
    pub file: String,
    /// Run gnuplot?
    pub gnuplot: bool,
    /// Command to run.
    pub then: Option<String>,
    /// Gnuplot format (terminal).
    pub fmt: PlotFmt,
    /// Generate title?
    pub title: bool,
    /// Ignore errors?
    pub no_errors: bool,
    /// Consider errors as timeout?
    pub errs_as_tmos: bool,
    /// Merge all validation results?
    pub merge: bool,
    /// Global conf.
    gconf: GConf,
}
impl GConfExt for PlotConf {
    fn gconf(&self) -> &GConf {
        &self.gconf
    }
}
impl PlotConf {
    /// Creates a plot conf.
    #[inline]
    pub fn mk(
        file: String,
        gnuplot: bool,
        then: Option<String>,
        fmt: PlotFmt,
        title: bool,
        no_errors: bool,
        errs_as_tmos: bool,
        merge: bool,
        gconf: GConf,
    ) -> Res<Self> {
        use std::process::Stdio;
        let gnuplot = if gnuplot {
            let status = Command::new("gnuplot")
                .arg("-V")
                .stdin(Stdio::null())
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .status();
            if !status.map(|s| s.success()).unwrap_or(false) {
                warn! {
                  gconf =>
                    "gnuplot does not seem to be installed"
                }
                false
            } else {
                true
            }
        } else {
            false
        };
        let file = file.path_subst();
        let (fmt, file) = {
            if let Some(s) = Path::new(&file.clone()).extension() {
                match &s.to_string_lossy().into_owned() as &str {
                    "plot" => (fmt, file),
                    ref s => {
                        let mut path = PathBuf::from(file);
                        path.set_extension("plot");
                        let fmt = if let Some(fmt) = PlotFmt::of_str(s) {
                            fmt
                        } else {
                            bail!(format!("unrecognized plot extension `{}`", s))
                        };
                        (fmt, path.to_string_lossy().to_string())
                    }
                }
            } else {
                (fmt, format!("{}.plot", file))
            }
        };
        Ok(PlotConf {
            file,
            gnuplot,
            then,
            fmt,
            title,
            no_errors,
            errs_as_tmos,
            merge,
            gconf,
        })
    }

    /// Plot data file specific to a tool.
    ///
    /// Corresponds to `<plot_path>/<plot_file_basename>_<tool_short>.data`.
    pub fn data_file_path_of(&self, tool: &ToolConf) -> Res<PathBuf> {
        let mut res = PathBuf::new();
        let path = Path::new(&self.file);
        res.push(if let Some(path) = path.parent() {
            path
        } else {
            bail!(format!(
                "illegal plot output file `{}`",
                self.sad(&self.file)
            ))
        });
        let file_name = if let Some(name) = path.file_stem() {
            name.to_string_lossy()
        } else {
            bail!(format!(
                "illegal plot output file `{}`",
                self.sad(&self.file)
            ))
        };
        res.push(&format!("{}_{}.data", file_name, tool.short));
        Ok(res)
    }
}

/// Plot formats.
#[derive(Clone, Copy, Debug)]
pub enum PlotFmt {
    /// PDF.
    Pdf,
    /// SVG.
    Svg,
    /// PNG.
    Png,
    /// LaTeX.
    Tex,
}
impl PlotFmt {
    /// Extension of a format.
    pub fn ext(&self) -> &'static str {
        match *self {
            PlotFmt::Pdf => "pdf",
            PlotFmt::Svg => "svg",
            PlotFmt::Png => "png",
            PlotFmt::Tex => "tex",
        }
    }
    /// Gnuplot terminal of a format.
    pub fn term(&self) -> &'static str {
        match *self {
            PlotFmt::Pdf => {
                "\
        set term pdf enhanced dashed \
        font \"Helvetica,15\" background rgb \"0xFFFFFF\"\
      "
            }
            PlotFmt::Svg => {
                "\
        set term svg enhanced \
        font \"Helvetica,15\" background rgb \"0xFFFFFF\"\
      "
            }
            PlotFmt::Png => {
                "\
        set term pngcairo enhanced \
        font \"Helvetica,15\" background rgb \"0xFFFFFF\"\
      "
            }
            PlotFmt::Tex => "set term latex",
        }
    }
    /// Gnuplot terminal of a format, `compare` version.
    pub fn compare_term(&self) -> &'static str {
        match *self {
            PlotFmt::Pdf => {
                "\
        set term pdf enhanced dashed \
        font \"Helvetica,15\" background rgb \"0xFFFFFF\" \
        size 3.1,3 ;\n\n\
        set lmargin at screen 0.14 ;\n\
        set rmargin at screen 0.95 ;\n\
        set bmargin at screen 0.12 ;\n\
        set tmargin at screen 0.95 ;\
      "
            }
            PlotFmt::Svg => {
                "\
        set term svg enhanced \
        font \"Helvetica,15\" background rgb \"0xFFFFFF\"\
      "
            }
            PlotFmt::Png => {
                "\
        set term pngcairo enhanced \
        font \"Helvetica,15\" background rgb \"0xFFFFFF\"\
      "
            }
            PlotFmt::Tex => "set term latex",
        }
    }
    /// Describes the legal values of the flag, should match the body of
    /// `Self::of_str`.
    #[inline]
    pub fn values() -> &'static str {
        "pdf|svg|png|tex"
    }
    /// Plot format of a string. Update `Self::values` if you change this.
    pub fn of_str(s: &str) -> Option<Self> {
        match s {
            "pdf" | "PDF" => Some(PlotFmt::Pdf),
            "svg" | "SVG" => Some(PlotFmt::Svg),
            "png" | "PNG" => Some(PlotFmt::Png),
            "tex" | "TEX" => Some(PlotFmt::Tex),
            _ => None,
        }
    }
    /// Plot format string validator.
    pub fn validator(s: &str) -> Result<(), String> {
        if let None = PlotFmt::of_str(&s) {
            Err(format!("expected `{}`, got `{}`", Self::values(), s))
        } else {
            Ok(())
        }
    }
}

/// Plot kinds.
pub enum PlotKind {
    /// Cumulative plot.
    Cumul {
        /// Data files.
        files: Vec<String>,
    },
    /// Comparative scatterplot.
    Compare {
        /// Data file 1.
        file_1: String,
        /// Data file 2.
        file_2: String,
    },
}

/// The plot subcommand.
pub fn plot_subcommand<'a>() -> ::clap_lib::Command<'a> {
    use clap::utils::*;

    clap::Command::new("plot")
        .about("Generates a plot.")
        .subcommand_required(true)
        .arg(
            clap::Arg::new("then")
                .long("--then")
                .help(
                    "\
Specifies a command to run on the file generated (ignored if `--run_gp off`)\
      ",
                )
                .value_name("command")
                .takes_value(true), //.number_of_values(1)
        )
        .arg(
            clap::Arg::new("run_gp")
                .long("--run_gp")
                .help(
                    "\
Runs `gnuplot` (or not) to generate the final plot\
      ",
                )
                .default_value("on")
                .takes_value(
                    true, // ).number_of_values(
                         // 1
                )
                .validator(bool_validator)
                .value_name(bool_format),
        )
        .arg(
            clap::Arg::new("title")
                .long("--title")
                .help("(de)activates the title of the plot")
                .default_value("on")
                .takes_value(
                    true, // ).number_of_values(
                         // 1
                )
                .validator(bool_validator)
                .value_name(bool_format),
        )
        .arg(
            clap::Arg::new("no_errors")
                .long("--no_errs")
                .help(
                    "\
Completely ignore benchmarks for which at least one tool returned an error\
      ",
                )
                .default_value("off")
                .takes_value(
                    true, // ).number_of_values(
                         // 1
                )
                .validator(bool_validator)
                .value_name(bool_format),
        )
        .arg(
            clap::Arg::new("merge")
                .long("--merge")
                .help(
                    "\
Ignore validators, plot everything together\
      ",
                )
                .default_value("off")
                .takes_value(
                    true, // ).number_of_values(
                         // 1
                )
                .validator(bool_validator)
                .value_name(bool_format),
        )
        .arg(
            clap::Arg::new("errs_as_tmos")
                .long("--errs_as_tmos")
                .help(
                    "\
Consider errors as timeouts\
      ",
                )
                .default_value("off")
                .takes_value(
                    true, // ).number_of_values(
                         // 1
                )
                .validator(bool_validator)
                .value_name(bool_format),
        )
        .arg(
            clap::Arg::new("gp_fmt")
                .long("--to")
                .help(
                    "\
Specifies the format the plot should generate (latex format is a bit
experimental for now).
Alternatively the extension can be set on the <PLOT_FILE>:
  // Sets `--to` to pdf:
  benchi plot my_plot.pdf ...
  // Sets `--to` to svg:
  benchi plot my_plot.svg ...\
      ",
                )
                .default_value("pdf")
                .takes_value(
                    true, // ).number_of_values(
                         // 1
                )
                .validator(PlotFmt::validator)
                .value_name(PlotFmt::values()),
        )
        .arg(
            clap::Arg::new("PLOT_FILE")
                .help(
                    "\
Output plot file\
      ",
                )
                .value_name("plot file")
                .required(true)
                .index(1),
        )
        .subcommand(cumul_subcommand())
        .subcommand(compare_subcommand())
}

/// `PlotConf` from some matches. `None` if `plot` subcommand is not present.
pub fn plot_clap(matches: &clap::Matches) -> Option<Res<Clap>> {
    use clap::utils::*;

    if let Some(plot_matches) = matches.subcommand_matches("plot") {
        // Global configuration.
        let conf = clap::gconf_of_matches(&matches);

        let file = plot_matches
            .value_of("PLOT_FILE")
            .expect("unreachable(plot:cumul:FILE): required")
            .to_string();

        let run_gp = bool_of_str(
            plot_matches
                .value_of("run_gp")
                .expect("unreachable(plot:run_gp): default provided"),
        )
        .expect("unreachable(plot:run_gp): input validated in clap");

        let title = bool_of_str(
            plot_matches
                .value_of("title")
                .expect("unreachable(plot:title): default provided"),
        )
        .expect("unreachable(plot:title): input validated in clap");

        let no_errors = bool_of_str(
            plot_matches
                .value_of("no_errors")
                .expect("unreachable(plot:no_errors): default provided"),
        )
        .expect("unreachable(plot:no_errors): input validated in clap");

        let errs_as_tmos = bool_of_str(
            plot_matches
                .value_of("errs_as_tmos")
                .expect("unreachable(plot:errs_as_tmos): default provided"),
        )
        .expect("unreachable(plot:errs_as_tmos): input validated in clap");

        let merge = bool_of_str(
            plot_matches
                .value_of("merge")
                .expect("unreachable(plot:merge): default provided"),
        )
        .expect("unreachable(plot:merge): input validated in clap");

        let fmt = PlotFmt::of_str(
            plot_matches
                .value_of("gp_fmt")
                .expect("unreachable(plot:gp_fmt): default provided"),
        )
        .expect("unreachable(plot:gp_fmt): input validated in clap");

        let cmd = if run_gp {
            plot_matches.value_of("then").map(|s| s.to_string())
        } else {
            None
        };

        let plot_conf = match PlotConf::mk(
            file,
            run_gp,
            cmd,
            fmt,
            title,
            no_errors,
            errs_as_tmos,
            merge,
            conf,
        ) {
            Ok(conf) => conf,
            Err(e) => return Some(Err(e)),
        };

        if let Some(cumul_matches) = plot_matches.subcommand_matches("cumul") {
            Some(Ok(cumul_clap(plot_conf, &cumul_matches)))
        } else if let Some(compare_matches) = plot_matches.subcommand_matches("compare") {
            Some(Ok(compare_clap(plot_conf, &compare_matches)))
        } else {
            Some(Err("unreachable(plot:<unknown>): \
          subcommand required can only be `cumul` or `compare`"
                .into()))
        }
    } else {
        None
    }
}

/// The cumul subcommand.
pub fn cumul_subcommand<'a>() -> clap::Command<'a> {
    clap::Command::new("cumul")
        .about("Generates a cumulative plot")
        .arg(
            clap::Arg::new("DATA")
                .help(
                    "\
Data files to use for plot generation\
      ",
                )
                .value_name("data file")
                .multiple_occurrences(true)
                .required(true),
        )
}

/// Cumul plot conf from some **sub**-matches.
pub fn cumul_clap(conf: PlotConf, matches: &crate::clap::Matches) -> Clap {
    let mut data_files = vec![];
    for data_file in matches
        .primary_values_of("DATA")
        .expect("unreachable(plot:cumul:DATA): required")
    {
        data_files.push(data_file.to_string())
    }

    Clap::Plot(conf, PlotKind::Cumul { files: data_files })
}

/// The compare subcommand.
pub fn compare_subcommand<'a>() -> clap::Command<'a> {
    use clap::*;

    Command::new("compare")
        .about("Generates a comparative scatterplot between two tools")
        .arg(
            Arg::new("FILE_1")
                .help(
                    "\
First data file (x axis)\
      ",
                )
                .value_name("data file")
                .required(true),
        )
        .arg(
            Arg::new("FILE_2")
                .help(
                    "\
Second data file (y axis)\
      ",
                )
                .value_name("data file")
                .required(true),
        )
}

/// Compare plot conf from some **sub**-matches.
pub fn compare_clap(conf: PlotConf, matches: &crate::clap::Matches) -> Clap {
    let file_1 = matches
        .value_of("FILE_1")
        .expect("unreachable(plot:compare:FILE_1): required")
        .to_string();
    let file_2 = matches
        .value_of("FILE_2")
        .expect("unreachable(plot:compare:FILE_2): required")
        .to_string();

    Clap::Plot(conf, PlotKind::Compare { file_1, file_2 })
}
