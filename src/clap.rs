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
    r"^(\d*)(min|s)$"
  ).unwrap() ;
}

/// Timeout from a string.
fn tmo_of_str(s: & str) -> Res<Duration> {
  if let Some(caps) = tmo_regex.captures(s) {
    debug_assert_eq!{ caps.len(), 3 }

    caps.get(1).ok_or(
      tmo_err(& s)
    ).and_then(
      |to| u64::from_str(
        to.as_str()
      ).map_err(
        |e| clap_err(
          "timeout argument",
          format!("expected integer, got `{}` ({})", to.as_str(), e)
        )
      )
    ).and_then(
      |num| {
        caps.get(2).ok_or(
          tmo_err(& s)
        ).and_then(
          |unit| match unit.as_str() {
            "min" => Ok(
              Duration::new(60 * num, 0)
            ),
            "s" => Ok(
              Duration::new(num, 0)
            ),
            s => Err(
              clap_err(
                "timeout argument",
                format!("expected `min` or `s`, got `{}`", s)
              )
            ),
          }
        )
      }
    )
  } else {
    bail!(
      tmo_err("timeout argument", )
    )
  }
}

// Timeout validator.
fn tmo_validator(s: String) -> Result<(), String> {
  if let Ok(_) = tmo_of_str(& s) {
    Ok(())
  } else {
    Err(
      format!("expected <{}>, got `{}`", tmo_format, s)
    )
  }
}


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
  ( Vec<ToolConf>, ArgMatches<'static> )
> {
  use std::io::Read ;

  let mut file = try!(
    File::open(& tool_file).chain_err( while_opening!(conf, tool_file) )
  ) ;
  let mut buff = Vec::with_capacity(217) ;
  let _ = try!(
    file.read_to_end(& mut buff).chain_err( while_opening!(conf, tool_file) )
  ) ;

  let (options, tool_confs) = try!(
    ::parse::work(& GConf::default(), & buff)
  ) ;

  // Make sure names are unique.
  {
    let mut tool_iter = tool_confs.iter() ;
    while let Some(tool_a) = tool_iter.next() {
      let other_tools = tool_iter.clone() ;
      for tool_b in other_tools {
        if tool_a.name.get() == tool_b.name.get() {
          bail!(
            "two of the tools have the same name `{}`",
            conf.bad(& tool_a.name),
          )
        }
        if tool_a.short.get() == tool_b.short.get() {
          bail!(
            "tools `{}` and `{}` have the same short name `{}`",
            conf.emph(& tool_a.name),
            conf.emph(& tool_b.name),
            conf.bad(& tool_a.short),
          )
        }
        if tool_a.graph.get() == tool_b.graph.get() {
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

  let tool_confs: Vec<_> = tool_confs.into_iter().map(
    |tool| tool.to_tool_conf()
  ).collect() ;

  Ok( (tool_confs, matches) )
}



/// Clap.
pub fn work() -> Res< Clap > {
  use clap_lib::* ;

  let matches = App::main_opts().run_opts(
    2, Some(
      Arg::with_name("CONF").help(
        "The configuration file (see `benchi conf -h` for details)"
      ).required(true).index(1)
    )
  ).plot_opts().get_matches() ;

  let conf = {
    Matches { primary: & matches, secondary: None }.clap_main()
  } ;

  if let Some(run_matches) = matches.subcommand_matches("run") {
    let tool_file = run_matches.value_of("CONF").expect(
      "unreachable(CONF): required"
    ).to_string() ;
    let (tools, file_matches) = try!(
      load_conf(& conf, tool_file)
    ) ;
    let matches = Matches {
      primary: & matches, secondary: Some(& file_matches)
    } ;
    return Ok( Clap::Run(matches.clap_run(conf)?, tools) )
  } 

  if let Some(plot_matches) = matches.subcommand_matches("plot") {
    
    if let Some(cumul_matches) = plot_matches.subcommand_matches("cumul") {
      let mut files = vec![] ;
      for file in cumul_matches.values_of("DATA").expect(
        "unreachable(plot:cumul:DATA): default is provided"
      ) {
        files.push( file.to_string() )
      }
      return Ok( Clap::CumulPlot(conf, files) )
    }
  }

  let msg = format!(
    "anything else that the {} or {} subcommands",
    conf.emph("run"), conf.emph("plot")
  ) ;
  bail!(
    ::errors::ErrorKind::Unimpl( conf, msg )
  )

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
      "`benchi` is a customizable benchmarking tool."
    ).arg(
      Arg::with_name("quiet").short("-q").help(
        "No output, except errors"
      ).conflicts_with("verbose")
    ).arg(
      Arg::with_name("verbose").short("-v").help(
        "Verbose output"
      ).conflicts_with("quiet")
    ).arg(
      Arg::with_name("colored").short("-c").long("--color").help(
        "Colored output"
      ).default_value("on").takes_value(true).validator(bool_validator)
    )
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
        "\
Sets the output directory. If the path ends with `today` (`now`), then `today`
(`now`) will be replaced with `<y>_<m>_<d>` (`<y>_<m>_<d>_at_<h><min>`)
representing the current date (and time).\
        "
      ).value_name("dir").default_value("./").takes_value(true)
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
      Arg::with_name("BENCHS").help(
        "\
The file containing the inputs to give to the tools. Optional, can be
specified in the configuration file.\
        "
      ).index(bench_index)
    ) ;

    let app = if let Some(arg) = add_arg {
      app.arg(arg)
    } else {
      app
    } ;
    self.subcommand(app)
  }

  fn plot_opts(
    self
  ) -> Self {
    use clap_lib::* ;

    let app = SubCommand::with_name("plot").about(
      "Generates a plot."
    ).subcommand(
      SubCommand::with_name("cumul").about(
        "Generates a cumulative plot"
      ).arg(
        Arg::with_name("DATA").help(
          "\
The data files to use for plot generation.\
          "
        ).multiple(true).required(true)
      )
    ) ;

    // let app = if let Some(arg) = add_arg {
    //   app.arg(arg)
    // } else {
    //   app
    // } ;
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

    GConf::mk(verb, colored)
  }

  // Run options, except the tool file.
  fn clap_run(& self, conf: GConf) -> Res<RunConf> {

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
        bench_par, tool_par, timeout, try,
        out_dir, tool_file, bench_file,
        conf
      )
    )
  }
}