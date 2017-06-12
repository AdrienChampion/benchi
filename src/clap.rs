//! Command-Line Argument Parsing (clap).

use std::str::FromStr ;

use clap_lib::{ App, Arg, ArgMatches } ;

use regex::Regex ;

use errors::* ;
use common::* ;


fn tmo_err(got: & str) -> Error {
  clap_err(
    "argument timeout", format!("expected `{}`, got {}", tmo_format, got)
  )
}
static tmo_format: & str = "[int]s|[int]min" ;
lazy_static!{
  static ref tmo_regex: Regex = Regex::new(
    r"^(?P<value>\d*)(?P<unit>min|s)$"
  ).unwrap() ;
}

/// Timeout from a string.
fn tmo_of_str(s: & str) -> Res<Duration> {
  if let Some(caps) = tmo_regex.captures(s) {
    debug_assert_eq!{ caps.len(), 3 }

    let value = if let Ok(value) = u64::from_str(
      & caps["value"]
    ) { value } else {
      bail!(
        clap_err(
          "timeout argument",
          format!("expected integer, got `{}`", & caps["value"])
        )
      )
    } ;

    let coef = match & caps["unit"] {
      "s" => 1,
      "min" => 60,
      "" => bail!(
        clap_err(
          "timeout argument",
          format!("missing time unit `s` or `min`")
        )
      ),
      s => bail!(
        clap_err(
          "timeout argument",
          format!("expected time unit `s` or `min`, got `{}`", s)
        )
      ),
    } ;

    Ok( Duration::new(coef * value, 0) )
  } else {
    bail!(
      tmo_err("timeout argument", )
    )
  }
}

/// Timeout validator.
fn tmo_validator(s: String) -> Result<(), String> {
  if let Ok(_) = tmo_of_str(& s) {
    Ok(())
  } else {
    Err(
      format!("expected <{}>, got `{}`", tmo_format, s)
    )
  }
}

/// Bool format.
static bool_format: & str = "on|true|off|false" ;

/// Boolean of a string.
fn bool_of_str(s: & str) -> Option<bool> {
  match & s as & str {
    "on" | "true" => Some(true),
    "off" | "false" => Some(false),
    _ => None,
  }
}

/// Validates boolean input.
fn bool_validator(s: String) -> Result<(), String> {
  if let Some(_) = bool_of_str(& s) {
    Ok(())
  } else {
    Err(
      format!("expected `on/true` or `off/false`, got `{}`", s)
    )
  }
}

/// Validates integer input.
fn int_validator(s: String) -> Result<(), String> {
  match usize::from_str(& s) {
    Ok(_) => Ok(()),
    Err(_) => Err(
      format!("expected an integer, got `{}`", s)
    ),
  }
}

macro_rules! while_opening {
  ($conf:expr, $file:expr) => (
    || format!(
      "while opening file {}",
      $conf.emph( $file.as_ref().to_str().expect("ill-formated file name...") )
    )
  ) ;
}

/// Loads the tool file.
fn load_conf<F: AsRef<Path>>(conf: & GConf, tool_file: F) -> Res<
  ( ValdConf, Vec<ToolConf>, ArgMatches<'static> )
> {
  use std::io::Read ;

  let mut file = try!(
    File::open(& tool_file).chain_err( while_opening!(conf, tool_file) )
  ) ;
  let mut buff = Vec::with_capacity(217) ;
  let _ = try!(
    file.read_to_end(& mut buff).chain_err( while_opening!(conf, tool_file) )
  ) ;

  let (options, vald_conf, tool_confs) = try!(
    ::parse::work(& GConf::default(), & buff)
  ) ;

  // Make sure names are unique.
  {
    let mut tool_iter = tool_confs.iter() ;
    while let Some(tool_a) = tool_iter.next() {
      let other_tools = tool_iter.clone() ;
      for tool_b in other_tools {
        if tool_a.name == tool_b.name {
          bail!(
            "two of the tools have the same name `{}`",
            conf.bad(& tool_a.name),
          )
        }
        if tool_a.short == tool_b.short {
          bail!(
            "tools `{}` and `{}` have the same short name `{}`",
            conf.emph(& tool_a.name),
            conf.emph(& tool_b.name),
            conf.bad(& tool_a.short),
          )
        }
        if tool_a.graph == tool_b.graph {
          bail!(
            "tools `{}` and `{}` have the same graph name `{}`",
            conf.emph(& tool_a.name),
            conf.emph(& tool_b.name),
            conf.bad(& tool_a.graph),
          )
        }
      }
    }
  }

  let mut actual_options = vec![ "benchi.conf_file".to_string() ] ;
  #[allow(unused_assignments)]
  for line in options {
    let mut buf = String::new() ;
    for c in line.chars() {
      match c {
        ' ' | '\t' => if ! buf.is_empty() {
          actual_options.push(buf) ;
          buf = String::new()
        },
        _ => buf.push(c),
      }
    }
    if ! buf.is_empty() {
      actual_options.push(buf) ;
      buf = String::new()
    }
  }

  let option_clap = App::main_opts().run_opts(1, None) ;
  let matches = match option_clap.get_matches_from_safe(& actual_options) {
    Ok(matches) => matches,
    Err(e) => {
      println!(
        "{} while parsing options of configuration file {}:",
        conf.bad("Error"),
        conf.emph(
          tool_file.as_ref().to_str().expect("ill-formated file name...")
        )
      ) ;
      e.exit()
    },
  } ;

  Ok( (vald_conf, tool_confs, matches) )
}



/// Clap.
pub fn work() -> Res< Clap > {
  use clap_lib::* ;

  let matches = App::main_opts().run_opts(
    2, Some(
      Arg::with_name("CONF").help(
        "The configuration file (see `benchi conf -h` for details)"
      ).required(true).index(1).value_name("conf file")
    )
  ).plot_opts().inspect_opts().conf_opts().get_matches() ;

  let conf = {
    Matches { primary: & matches, secondary: None }.clap_main()
  } ;

  if let Some(run_matches) = matches.subcommand_matches("run") {
    let tool_file = run_matches.value_of("CONF").expect(
      "unreachable(CONF): required"
    ).to_string() ;
    let (vald_conf, tools, file_matches) = try!(
      load_conf(& conf, tool_file)
    ) ;
    let matches = Matches {
      primary: & matches, secondary: Some(& file_matches)
    } ;
    let conf = matches.clap_main() ;
    return Ok( Clap::Run(matches.clap_run(conf, vald_conf)?, tools) )
  }

  if let Some(plot_matches) = matches.subcommand_matches("plot") {
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

    if let Some(cumul_matches) = plot_matches.subcommand_matches("cumul") {
      let mut data_files = vec![] ;
      for data_file in cumul_matches.values_of("DATA").expect(
        "unreachable(plot:cumul:DATA): required"
      ) {
        data_files.push( data_file.to_string() )
      }

      return Ok(
        Clap::Plot(
          PlotConf::mk(file, run_gp, cmd, fmt, no_errors, errs_as_tmo, conf),
          PlotKind::Cumul { files: data_files }
        )
      )
    }
    
    if let Some(compare_matches) = plot_matches.subcommand_matches("compare") {
      let file_1 = compare_matches.value_of("FILE_1").expect(
        "unreachable(plot:compare:FILE_1): required"
      ) ;
      let file_2 = compare_matches.value_of("FILE_2").expect(
        "unreachable(plot:compare:FILE_2): required"
      ) ;

      return Ok(
        Clap::Plot(
          PlotConf::mk(file, run_gp, cmd, fmt, no_errors, errs_as_tmo, conf),
          PlotKind::Compare {
            file_1: file_1.into(),
            file_2: file_2.into()
          }
        )
      )
    }
  }

  if let Some(conf_matches) = matches.subcommand_matches("conf") {
    let conf_file = conf_matches.value_of("CONF").expect(
      "unreachable(CONF): required"
    ).to_string() ;
    return Ok( Clap::Conf(conf, conf_file) )
  }

  bail!("called with unimplemented command")

}



/// Extends `clap`'s `App`.
trait AppExt {
  type Argument ;
  /// Adds the main options.
  fn main_opts() -> Self ;
  /// Adds the `run` options, except `conf`.
  ///
  /// `bench_index` is the index to give to the bench option.
  fn run_opts(
    self, bench_index: u64, add_arg: Option<Self::Argument>,
  ) -> Self ;
  /// Adds the `plot` options.
  fn plot_opts(self) -> Self ;
  /// Adds the `inspect` options.
  fn inspect_opts(self) -> Self ;
  /// Adds the `conf` options, the subcommand explaining configuration files.
  fn conf_opts(self) -> Self ;
}
impl<'a, 'b> AppExt for App<'a, 'b> {
  type Argument = Arg<'a, 'b> ;
  fn main_opts() -> Self {
    use clap_lib::* ;
    App::new(
      crate_name!()
    ).version(
      crate_version!()
    ).author(
      crate_authors!()
    ).about(
      "`benchi` is an easy to use benchmarking tool."
    ).arg(
      Arg::with_name("force").short("-f").long("--force").help(
        "When writing a file, overwrite if present"
      )
    ).arg(
      Arg::with_name("quiet").short("-q").long("--quiet").help(
        "No output, except errors"
      ).conflicts_with("verbose")
    ).arg(
      Arg::with_name("verbose").short("-v").long("--verb").help(
        "Verbose output"
      ).conflicts_with("quiet")
    ).arg(
      Arg::with_name("colored").short("-c").long("--color").help(
        "Colored output"
      ).default_value("on").takes_value(true).validator(
        bool_validator
      ).value_name(bool_format)
    ).after_help(
      "\
# Path substitutions

Output paths such as `run`'s output directory or `plot`'s output file can use
benchi's substitution mechanism. That is, occurrences of `<today>` are replaced
with `<year>_<month>_<day>` and occurrences of `<now>` are replaced with
`<hour>_<minute>`.
      "
    ).setting( AppSettings::SubcommandRequired )
  }

  fn run_opts(
    self, bench_index: u64, add_arg: Option<Arg<'a, 'b>>,
  ) -> Self {
    use clap_lib::* ;

    let app = SubCommand::with_name("run").about(
      "Runs benchmarks according to some configuration file."
    ).after_help(
      "\
The different tools run on each benchmark: the value of `--tools` decides how
many tools can run at the same time. The value of `--benchs` specifies the
number of benchmarks handled in parallel.

So, with `--benchs 2` and `--tools 3`, 6 (2*3) threads will handle up to 2
benchmarks simultaneously, with up to 3 tools running in parallel on each of
them. There's actually more threads than that do organize everything, but only
6 of them really run tools.
                     ___________master___________
                    |                            |
               ___bench___                  ___bench___
              |     |     |                |     |     |
             tool  tool  tool             tool  tool  tool

# Examples

`run --benchs 6 --tools 1 ...` runs the tools sequentially on 6 benchmarks at
the same time
`run --benchs 1 --tools 2 ...` runs up to 2 tools in parallel on each benchmark
sequentially\
      "
    ).arg(
      Arg::with_name("out_dir").short("-o").long("--out_dir").help(
        "Sets the output directory."
      ).value_name("dir").default_value("<today>_at_<now>").takes_value(true)
    ).arg(
      Arg::with_name("timeout").short("-t").long("--timeout").help(
        "Sets the timeout for each run"
      ).value_name(tmo_format).validator(
        tmo_validator
      ).default_value("1min").takes_value(true)
    ).arg(
      Arg::with_name("para_benchs").long("--benchs").help(
        "Number of benchmarks to run in parallel"
      ).value_name("int").default_value("1").takes_value(true).validator(
        int_validator
      )
    ).arg(
      Arg::with_name("para_tools").long("--tools").help(
        "Number of tools to run in parallel on each benchmark"
      ).value_name("int").default_value("1").takes_value(true).validator(
        int_validator
      )
    ).arg(
      Arg::with_name("try").long("--try").help(
        "Only runs on `n` benchmarks (to try the set up)"
      ).value_name("int").takes_value(true).validator(
        int_validator
      )
    ).arg(
      Arg::with_name("log_stdout").long("--log").help(
        "\
(De)activates stdout logging of the tools.\
        "
      ).default_value("on").takes_value(true).validator(
        bool_validator
      ).value_name(bool_format)
    ).arg(
      Arg::with_name("BENCHS").help(
        "\
The file containing the inputs to give to the tools. Optional, can be
specified in the configuration file.\
        "
      ).value_name("bench file").index(bench_index)
    ) ;

    let app = if let Some(arg) = add_arg {
      app.arg(arg)
    } else {
      app
    } ;
    self.subcommand(app)
  }

  fn plot_opts(self) -> Self {
    use clap_lib::* ;

    let app = SubCommand::with_name("plot").about(
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
      SubCommand::with_name("cumul").about(
        "Generates a cumulative plot"
      ).arg(
        Arg::with_name("DATA").help(
          "\
Data files to use for plot generation\
          "
        ).value_name("data file").multiple(true).required(true)
      )
    ).subcommand(
      SubCommand::with_name("compare").about(
        "Generates a comparison scatterplot between two tools"
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
    ) ;

    // let app = if let Some(arg) = add_arg {
    //   app.arg(arg)
    // } else {
    //   app
    // } ;
    self.subcommand(app)
  }

  /// Adds inspection options.
  fn inspect_opts(self) -> Self {
    use clap_lib::* ;

    let app = SubCommand::with_name("inspect").about(
      "Interactive data inspection. (UNIMPLEMENTED)"
    ).about("UNIMPLEMENTED") ;

    self.subcommand(app)
  }

  /// Adds configuration explanation options.
  fn conf_opts(self) -> Self {
    use clap_lib::* ;

    let app = SubCommand::with_name("conf").about(
      "Explanation of the configuration file format."
    ).about("\
To run benchmarks you must provide a configuration file that describes the
tools to run. It can also include options normally passed as command-line
arguments (CLA), so that you can simply run `benchi run my_conf_file` (more on
that below).

Single line comments use Rust/C/C++ syntax `//` and can appear anywhere. There
is currently no multi-line comments.

# Tool description

A tool descriptions is
- a 'name' used for user interaction,
- a 'short' name used to create tool-specific files and directories,
- a 'command' to run the tool, and
- a 'graph' name used for keys and axes when drawing graphs.

The graph name is optional, if none is provided benchi will use 'name' as the
graph name.

The actual syntax is
               <desc> ::= <name> { <short> [<graph>] <command> }
              <graph> ::= graph: <name>
              <short> ::= short: [a-zA-Z0-9_-.]+
            <command> ::= cmd: \"[^\\\"]+\"
               <name> ::= [^\\n{{}}\\\"/]+

## Example:

> find in root {
>   short: find_in_root
>   cmd: \"find / -name\"
>   graph: Find in Root
> }

# Options

The configuration sets benchi's options directly if contains an `options` field
of the form `options: \"[^\\\"]+\"`. Benchi will interpret the string as CLAs.

The only exception difference is that CLAs have a mandatory configuration file
argument while configuration file options do not. (It would not make any
sense.)

NB: actual CLAs override the options in the configuration file. See example
below.

## Example:

> options: \"
>   -v run -o output_<today>_at_<now> --tools 3 --benchs 1 -t 3s my_benchs
> \"

With these options in `my_conf`, then
                   `benchi -q run --benchs 10 my_conf my_other_benchs`

benchmarks 3 tools in parallel with a timeout of 3 seconds and dumps the output
to `output_<today>_at_<now>` (see Path Substitution in the top-level help for
details on `<today>` and `<now>`). BUT the rest of the configuration file's
options (`-v` and `--tools`) are overriden by the CLAs, so benchi will run in
quiet mode over 10 benchmarks in parallel, using `my_other_benchs` as the
benchmark list file.\
    ").arg(
      Arg::with_name("CONF").help(
        "\
Dumps an example configuration file to <file.conf>, and a benchmark list file
to <file.benchs>.\
        "
      ).required(true).index(1).value_name("file.conf")
    ) ;

    self.subcommand(app)
  }
}


/// Wraps two matches, a primary and a secondary one. Whenever a request is
/// made, it goes to the primary matches first and then, if **no occurence is
/// found** the secondary one.
struct Matches<'a> {
  primary: & 'a ArgMatches<'static>,
  secondary: Option< & 'a ArgMatches<'static> >,
}
impl<'a> Matches<'a> {
  fn is_present(& self, name: & str) -> bool {
    self.primary.is_present(name) ||
    self.secondary.map(|m| m.is_present(name)).unwrap_or(false)
  }
  fn is_present_in_primary(& self, name: & str) -> bool {
    self.primary.is_present(name)
  }
  fn value_of(& self, name: & str) -> Option<&str> {
    if self.primary.occurrences_of(name) > 0 || self.secondary.is_none() {
      self.primary.value_of(name)
    } else {
      self.secondary.and_then(|m| m.value_of(name))
    }
  }
  #[allow(unused)]
  fn sub_is_present(& self, sub: & str, name: & str) -> bool {
    if let Some(matches) = self.primary.subcommand_matches(sub) {
      if matches.is_present(name) {
        return true
      }
    }
    self.secondary.map(
      |m| m.subcommand_matches(sub).map(
        |matches| matches.is_present(name)
      ).unwrap_or(false)
    ).unwrap_or(false)
  }
  fn sub_value_of(
    & self, sub: & str, name: & str
  ) -> Option<String> {
    match (
      self.primary.subcommand_matches(sub),
      self.secondary.and_then(|m| m.subcommand_matches(sub))
    ) {
      (None, None) => None,
      (Some(p), Some(s)) => Matches {
        primary: p, secondary: Some(s)
      }.value_of(name).map(|s| s.to_string()),
      (Some(p), None) => p.value_of(name).map(|s| s.to_string()),
      (None, Some(s)) => s.value_of(name).map(|s| s.to_string()),
    }
  }

  /// Main options.
  fn clap_main(& self) -> GConf {
    // File overwrite.
    let ow_files = self.is_present("force") ;

    // Quiet / verbose.
    let verb = if self.is_present_in_primary("quiet") {
      Verb::Quiet
    } else if self.is_present_in_primary("verbose") {
      Verb::Verbose
    } else {
      if self.is_present("quiet") {
        Verb::Quiet
      } else if self.is_present("verbose") {
        Verb::Verbose
      } else {
        Verb::Normal
      }
    } ;

    // Colored.
    let colored = self.value_of("colored").and_then(
      |s| {
        bool_of_str(& s)
      }
    ).expect(
      "unreachable(colored): default is provided and input validated in clap"
    ) ;

    GConf::mk(verb, colored, ow_files)
  }

  // Run options, except the tool file.
  fn clap_run(& self, conf: GConf, vald_conf: ValdConf) -> Res<RunConf> {

    // Output directory.
    let out_dir = self.sub_value_of("run", "out_dir").expect(
      "unreachable(out_dir): default is provided"
    ).to_string() ;

    // Bench and tool parallel settings.
    let bench_par = self.sub_value_of("run", "para_benchs").map(
      |s| usize::from_str(& s)
    ).expect(
      "unreachable(bench_par): default is provided"
    ).expect(
      "unreachable(bench_par): input validated in clap"
    ) ;
    let tool_par = self.sub_value_of("run", "para_tools").map(
      |s| usize::from_str(& s)
    ).expect(
      "unreachable(tool_par): default is provided"
    ).expect(
      "unreachable(tool_par): input validated in clap"
    ) ;

    let try = self.sub_value_of("run", "try").map(
      |s| usize::from_str(& s).expect(
        "unreachable(tool_par): input validated in clap"
      )
    ) ;

    // Timeout.
    let timeout = self.sub_value_of("run", "timeout").map(
      |s| tmo_of_str(& s)
    ).expect(
      "unreachable(timeout): default is provided"
    ).expect(
      "unreachable(timeout): input validated in clap"
    ) ;
    
    let log_stdout = self.sub_value_of("run", "log_stdout").and_then(
      |s| {
        bool_of_str(& s)
      }
    ).expect(
      "unreachable(log_stdout): \
      default is provided and input validated in clap"
    ) ;
    
    let tool_file = self.sub_value_of("run", "CONF").expect(
      "unreachable(CONF): required"
    ).to_string() ;

    // Bench file.
    let bench_file = if let Some(f) = self.sub_value_of("run", "BENCHS") {
      f
    } else {
      bail!(
        "no benchmark file specified in command line or configuration file"
      )
    } ;

    Ok(
      RunConf::mk(
        bench_par, tool_par, timeout, try, log_stdout,
        out_dir, tool_file, bench_file,
        conf, vald_conf
      )
    )
  }
}






#[test]
fn clap_tmo() {
  fn test(secs: u64, string: & str) {
    let exp = Duration::new(secs, 0) ;
    println!("`{}` should be parsed as {}", string, exp.as_sec_str()) ;
    assert!( tmo_validator(string.into()).is_ok() ) ;
    assert_eq!( exp, tmo_of_str(string).unwrap() )
  }

  test(10, "10s") ;
  test(42, "42s") ;
  test(42 * 60, "42min") ;

  assert!( tmo_of_str("").is_err() ) ;
  assert!( tmo_validator( "".into() ).is_err() ) ;
  assert!( tmo_of_str("7").is_err() ) ;
  assert!( tmo_validator( "7".into() ).is_err() ) ;
  assert!( tmo_of_str("s").is_err() ) ;
  assert!( tmo_validator( "s".into() ).is_err() ) ;
  assert!( tmo_of_str("min").is_err() ) ;
  assert!( tmo_validator( "min".into() ).is_err() ) ;
  assert!( tmo_of_str("b42s").is_err() ) ;
  assert!( tmo_validator( "b42s".into() ).is_err() ) ;
  assert!( tmo_of_str("42 min").is_err() ) ;
  assert!( tmo_validator( "42 min".into() ).is_err() ) ;
}