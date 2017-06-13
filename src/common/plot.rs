//! Plot basic types and helpers.

use common::* ;


/// Plot configuration.
pub struct PlotConf {
  /// Output file.
  pub file: String,
  /// Generate pdf?
  pub pdf: bool,
  /// Command to run.
  pub then: Option<String>,
  /// Gnuplot format (terminal).
  pub fmt: PlotFmt,
  /// Ignore errors?
  pub no_errors: bool,
  /// Consider errors as timeout?
  pub errs_as_tmo: bool,
  /// Global conf.
  gconf: GConf,
}
impl GConfExt for PlotConf {
  fn gconf(& self) -> & GConf { & self.gconf }
}
impl PlotConf {
  /// Creates a plot conf.
  #[inline]
  pub fn mk(
    file: String, pdf: bool, then: Option<String>,
    fmt: PlotFmt, no_errors: bool, errs_as_tmo: bool,
    gconf: GConf
  ) -> Self {
    let file = file.path_subst() ;
    PlotConf { file, pdf, then, fmt, no_errors, errs_as_tmo, gconf }
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
  pub fn ext(& self) -> & 'static str {
    match * self {
      PlotFmt::Pdf => "pdf",
      PlotFmt::Svg => "svg",
      PlotFmt::Png => "png",
      PlotFmt::Tex => "tex",
    }
  }
  /// Gnuplot terminal of a format.
  pub fn term(& self) -> & 'static str {
    match * self {
      PlotFmt::Pdf => "\
        set term pdf enhanced \
        font \"Helvetica,15\" background rgb \"0xFFFFFF\"\
      ",
      PlotFmt::Svg => "\
        set term svg enhanced\
        font \"Helvetica,15\" background rgb \"0xFFFFFF\"\
      ",
      PlotFmt::Png => "\
        set term pngcairo enhanced\
        font \"Helvetica,15\" background rgb \"0xFFFFFF\"\
      ",
      PlotFmt::Tex => "set term latex",
    }
  }
  /// Describes the legal values of the flag, should match the body of
  /// `Self::of_str`.
  #[inline]
  pub fn values() -> & 'static str {
    "pdf|svg|png|tex"
  }
  /// Plot format of a string. Update `Self::values` if you change this.
  pub fn of_str(s: & str) -> Option<Self> {
    match s {
      "pdf" => Some(PlotFmt::Pdf),
      "svg" => Some(PlotFmt::Svg),
      "png" => Some(PlotFmt::Png),
      "tex" => Some(PlotFmt::Tex),
      _ => None,
    }
  }
  /// Plot format string validator.
  pub fn validator(s: String) -> Result<(), String> {
    if let None = PlotFmt::of_str(& s) {
      Err( format!("expected `{}`, got `{}`", Self::values(), s) )
    } else {
      Ok(())
    }
  }
}





/// A plot command.
pub enum Plot {
  /// Compares two tools benchmark per benchmark (scatterplot).
  Comparative {
    /// The file to write the plot to.
    file: Option<String>,
    /// The first short tool name.
    tool_1: String,
    /// The second short tool name.
    tool_2: String,
  },

  /// Cumulative plot for some tools.
  Cumulative {
    /// The file to write the plot to.
    file: Option<String>,
    /// Some tools to compare.
    tools: Vec<String>,
  },
}
impl Plot {
  /// Creates a comparative plot command.
  #[inline]
  pub fn comparative(
    file: Option<String>, tool_1: String, tool_2: String
  ) -> Self {
    Plot::Comparative { file, tool_1, tool_2 }
  }
  /// Creates a cumulative plot command.
  #[inline]
  pub fn cumulative(
    file: Option<String>, tools: Vec<String>
  ) -> Self {
    Plot::Cumulative { file, tools }
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
  }
}



/// The plot subcommand.
pub fn plot_subcommand<'a, 'b>() -> ::clap_lib::App<'a, 'b> {
  use clap_lib::* ;
  use consts::clap::* ;
  use clap::utils::* ;

  SubCommand::with_name("plot").about(
    "Generates a plot."
  ).setting( AppSettings::SubcommandRequired ).arg(
    Arg::with_name("then").long("--then").help(
      "\
Specifies a command to run on the pdf generated (ignored if `--pdf off`)\
      "
    ).value_name("command").takes_value(true)
  ).arg(
    Arg::with_name("run_gp").long("--run_gp").help(
      "\
Runs `gnuplot` (or not) to generate the final plot\
      "
    ).default_value("on").takes_value(true).validator(
      bool_validator
    ).value_name(bool_format)
  ).arg(
    Arg::with_name("no_errors").long("--no_errs").help(
      "\
Completely ignore benchmarks for which at least one tool returned an error\
      "
    ).default_value("off").takes_value(true).validator(
      bool_validator
    ).value_name(bool_format)
  ).arg(
    Arg::with_name("errs_as_tmo").long("--errs_as_tmo").help(
      "\
Consider errors as timeouts\
      "
    ).default_value("off").takes_value(true).validator(
      bool_validator
    ).value_name(bool_format)
  ).arg(
    Arg::with_name("gp_fmt").long("--to").help(
      "\
Specifies the format the plot should generate (latex format is a bit
experimental for now)\
      "
    ).default_value("pdf").takes_value(true).validator(
      PlotFmt::validator
    ).value_name( PlotFmt::values() )
  ).arg(
    Arg::with_name("PLOT_FILE").help(
      "\
Output plot file\
      "
    ).value_name("plot file").required(true).index(1)
  ).subcommand(
    cumul_subcommand()
  ).subcommand(
    compare_subcommand()
  )
}

/// `PlotConf` from some matches. `None` if `plot` subcommand is not present.
pub fn plot_clap<'a>(
  matches: & ::clap::Matches<'a>
) -> Option< Res<Clap> > {
  use clap::utils::* ;

  if let Some(plot_matches) = matches.subcommand_matches("plot") {
    // Global configuration.
    let conf = ::clap::gconf_of_matches(& matches) ;

    let file = plot_matches.value_of("PLOT_FILE").expect(
      "unreachable(plot:cumul:FILE): required"
    ).to_string() ;

    let run_gp = bool_of_str(
      plot_matches.value_of("run_gp").expect(
        "unreachable(plot:run_gp): default provided"
      )
    ).expect(
      "unreachable(plot:run_gp): input validated in clap"
    ) ;

    let no_errors = bool_of_str(
      plot_matches.value_of("no_errors").expect(
        "unreachable(plot:no_errors): default provided"
      )
    ).expect(
      "unreachable(plot:no_errors): input validated in clap"
    ) ;

    let errs_as_tmo = bool_of_str(
      plot_matches.value_of("errs_as_tmo").expect(
        "unreachable(plot:errs_as_tmo): default provided"
      )
    ).expect(
      "unreachable(plot:errs_as_tmo): input validated in clap"
    ) ;

    let fmt = PlotFmt::of_str(
      plot_matches.value_of("gp_fmt").expect(
        "unreachable(plot:gp_fmt): default provided"
      )
    ).expect(
      "unreachable(plot:gp_fmt): input validated in clap"
    ) ;

    let cmd = if run_gp {
      plot_matches.value_of("then").map(|s| s.to_string())
    } else { None } ;

    let plot_conf = PlotConf::mk(
      file, run_gp, cmd, fmt, no_errors, errs_as_tmo, conf
    ) ;

    if let Some(cumul_matches) = plot_matches.subcommand_matches("cumul") {
      Some(
        Ok( cumul_clap(plot_conf, & cumul_matches) )
      )
    } else if let Some(
      compare_matches
    ) = plot_matches.subcommand_matches("compare") {
      Some(
        Ok( compare_clap(plot_conf, & compare_matches) )
      )
    } else {
      Some(
        Err(
          "unreachable(plot:<unknown>): \
          subcommand required can only be `cumul` or `compare`".into()
        )
      )
    }
  } else {
    None
  }
}



/// The cumul subcommand.
pub fn cumul_subcommand<'a, 'b>() -> ::clap_lib::App<'a, 'b> {
  use clap_lib::* ;

  SubCommand::with_name("cumul").about(
    "Generates a cumulative plot"
  ).arg(
    Arg::with_name("DATA").help(
      "\
Data files to use for plot generation\
      "
    ).value_name("data file").multiple(true).required(true)
  )
}

/// Cumul plot conf from some **sub**-matches.
pub fn cumul_clap<'a>(
  conf: PlotConf, matches: & ::clap::Matches<'a>
) -> Clap {
  let mut data_files = vec![] ;
  for data_file in matches.primary_values_of("DATA").expect(
    "unreachable(plot:cumul:DATA): required"
  ) {
    data_files.push( data_file.to_string() )
  }

  Clap::Plot(
    conf, PlotKind::Cumul { files: data_files }
  )
}

/// The compare subcommand.
pub fn compare_subcommand<'a, 'b>() -> ::clap_lib::App<'a, 'b> {
  use clap_lib::* ;

  SubCommand::with_name("compare").about(
    "Generates a comparative scatterplot between two tools"
  ).arg(
    Arg::with_name("FILE_1").help(
      "\
First data file (x axis)\
      "
    ).value_name("data file").required(true)
  ).arg(
    Arg::with_name("FILE_2").help(
      "\
Second data file (y axis)\
      "
    ).value_name("data file").required(true)
  )
}

/// Compare plot conf from some **sub**-matches.
pub fn compare_clap<'a>(
  conf: PlotConf, matches: & ::clap::Matches<'a>
) -> Clap {
  let file_1 = matches.value_of("FILE_1").expect(
    "unreachable(plot:compare:FILE_1): required"
  ).to_string() ;
  let file_2 = matches.value_of("FILE_2").expect(
    "unreachable(plot:compare:FILE_2): required"
  ).to_string() ;

  Clap::Plot(
    conf, PlotKind::Compare { file_1, file_2 }
  )
}


