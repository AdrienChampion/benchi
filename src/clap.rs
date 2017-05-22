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
fn load_conf<F: AsRef<Path>>(conf: & Conf, tool_file: F) -> Res<
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
    ::parse::work(& conf, & buff)
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

  let option_clap = App::main_opts(true).run_opts(1, None, true) ;
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
pub fn work() -> Res< (Conf, Vec<ToolConf>) > {
  use clap_lib::* ;

  let matches: ArgMatches = App::main_opts(true).run_opts(
    2, Some(
      Arg::with_name("CONF").help(
        "The configuration file (see `benchi conf -h` for details)"
      ).required(true).index(1)
    ), true
  ).get_matches() ;

  let mut conf = Conf::default() ;

  let colored = matches.value_of("colored").and_then(
    |s| bool_of_str(& s)
  ).expect(
    "unreachable(colored): default is provided and input validated in clap"
  ) ;
  conf.coloring(colored) ;

  if let Some(run_matches) = matches.subcommand_matches("run") {
    let tool_file = run_matches.value_of("CONF").expect(
      "unreachable(CONF): default is provided"
    ).to_string() ;
    let (tools, file_matches) = try!(
      load_conf(& conf, tool_file)
    ) ;
    let matches = Matches { primary: & matches, secondary: & file_matches } ;
    matches.clap_main(& mut conf) ;
    try!( matches.clap_run(& mut conf) ) ;
    Ok( (conf, tools) )
  } else {
    let msg = format!(
      "anything else that the {} subcommand", conf.emph("run")
    ) ;
    bail!(
      ::errors::ErrorKind::Unimpl( Arc::new(conf), msg )
    )
  }

  // clap_main(& matches, & mut conf) ;

  // // Run mode.
  // if let Some(matches) = matches.subcommand_matches("run") {
  //   clap_run(& matches, & mut conf) ;
  //   // Conf file.
  //   conf.tool_file = matches.value_of("CONF").expect(
  //     "unreachable(CONF): default is provided"
  //   ).to_string() ;
  //   Ok(conf)
  // } else {
  //   let msg = format!(
  //     "anything else that the {} subcommand", conf.emph("run")
  //   ) ;
  //   bail!(
  //     ::errors::ErrorKind::Unimpl( Arc::new(conf), msg )
  //   )
  // }

}



/// Extends `clap`'s `App`.
trait AppExt {
  type Argument ;
  /// Adds the main options.
  fn main_opts(defaults: bool) -> Self ;
  /// Adds the `run` options, except `conf`.
  ///
  /// `bench_index` is the index to give to the bench option.
  fn run_opts(
    self, bench_index: u64, add_arg: Option<Self::Argument>,
    defaults: bool
  ) -> Self ;
}
impl<'a, 'b> AppExt for App<'a, 'b> {
  type Argument = Arg<'a, 'b> ;
  fn main_opts(defaults: bool) -> Self {
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
      Arg::with_name("out_dir").short("-o").long("--out_dir").help(
        "\
Sets the output directory. If the path ends with `today` (`now`), then `today`
(`now`) will be replaced with `<y>_<m>_<d>` (`<y>_<m>_<d>_at_<h><min>`)
representing the current date (and time).\
        "
      ).value_name("dir").default_if("./", defaults).takes_value(true)
    ).arg(
      Arg::with_name("quiet").short("-q").help(
        "No output, except errors"
      ).conflicts_with("verbose")
    ).arg(
      Arg::with_name("verbose").short("-v").help(
        "More verbose output"
      ).conflicts_with("quiet")
    ).arg(
      Arg::with_name("colored").short("-c").long("--color").help(
        "Colored output"
      ).default_if("on", defaults).takes_value(true).validator(bool_validator)
    )
  }

  fn run_opts(
    self, bench_index: u64, add_arg: Option<Arg<'a, 'b>>,
    defaults: bool
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
them.
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
      Arg::with_name("timeout").short("-t").long("--timeout").help(
        "Sets the timeout for each run"
      ).value_name(tmo_format).validator(
        tmo_validator
      ).default_if("1min", defaults).takes_value(true)
    ).arg(
      Arg::with_name("para_benchs").long("--benchs").help(
        "Number of benchmarks to run in parallel"
      ).value_name("int").default_if(
        "1", defaults
      ).takes_value(true).validator(
        int_validator
      )
    ).arg(
      Arg::with_name("para_tools").long("--tools").help(
        "Number of tools to run in parallel on each benchmark"
      ).value_name("int").default_if(
        "1", defaults
      ).takes_value(true).validator(
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
}


/// Extends `clap`'s `Arg`.
trait ArgExt {
  /// Adds a default value if the flag is true.
  fn default_if(self, default: & 'static str, do_it: bool) -> Self ;
}
impl ArgExt for Arg<'static, 'static> {
  fn default_if(self, default: & 'static str, do_it: bool) -> Self {
    if do_it { self.default_value(default) } else { self }
  }
}


/// Wraps two matches, a primary and a secondary one. Whenever a request is
/// made, it goes to the primary matches first and then, if **no occurence is
/// found** the secondary one.
struct Matches<'a> {
  primary: & 'a ArgMatches<'static>,
  secondary: & 'a ArgMatches<'static>,
}
impl<'a> Matches<'a> {
  fn is_present(& self, name: & str) -> bool {
    self.primary.is_present(name) || self.secondary.is_present(name)
  }
  fn value_of(& self, name: & str) -> Option<&str> {
    if self.primary.occurrences_of(name) > 0 {
      self.primary.value_of(name)
    } else {
      self.secondary.value_of(name)
    }
  }
  #[allow(unused)]
  fn sub_is_present(& self, sub: & str, name: & str) -> bool {
    if let Some(matches) = self.primary.subcommand_matches(sub) {
      if matches.is_present(name) {
        return true
      }
    }
    self.secondary.subcommand_matches(sub).map(
      |matches| matches.is_present(name)
    ).unwrap_or(false)
  }
  fn sub_value_of(
    & self, sub: & str, name: & str
  ) -> Option<String> {
    match (
      self.primary.subcommand_matches(sub),
      self.secondary.subcommand_matches(sub)
    ) {
      (None, None) => None,
      (Some(p), Some(s)) => Matches {
        primary: p, secondary: s
      }.value_of(name).map(|s| s.to_string()),
      (Some(p), None) => p.value_of(name).map(|s| s.to_string()),
      (None, Some(s)) => s.value_of(name).map(|s| s.to_string()),
    }
  }
  /// Main options.
  fn clap_main(& self, conf: & mut Conf) {
    // Quiet / verbose.
    conf.quiet = self.is_present("quiet") ;
    conf.verb = self.is_present("verbose") ;

    // Colored.
    let colored = self.value_of("colored").and_then(
      |s| bool_of_str(& s)
    ).expect(
      "unreachable(colored): default is provided and input validated in clap"
    ) ;
    conf.coloring(colored) ;

    // Output directory.
    conf.out_dir = self.value_of("out_dir").expect(
      "unreachable(out_dir): default is provided"
    ).to_string() ;
  }

  // Run options, except the tool file.
  fn clap_run(& self, conf: & mut Conf) -> Res<()> {

    // Bench and tool parallel settings.
    conf.bench_par = self.sub_value_of("run", "para_benchs").map(
      |s| usize::from_str(& s)
    ).expect(
      "unreachable(bench_par): default is provided"
    ).expect(
      "unreachable(bench_par): input validated in clap"
    ) ;
    conf.tool_par = self.sub_value_of("run", "para_tools").map(
      |s| usize::from_str(& s)
    ).expect(
      "unreachable(tool_par): default is provided"
    ).expect(
      "unreachable(tool_par): input validated in clap"
    ) ;

    conf.try = self.sub_value_of("run", "try").map(
      |s| usize::from_str(& s).expect(
        "unreachable(tool_par): input validated in clap"
      )
    ) ;

    // Timeout.
    conf.timeout = self.sub_value_of("run", "timeout").map(
      |s| tmo_of_str(& s)
    ).expect(
      "unreachable(timeout): default is provided"
    ).expect(
      "unreachable(timeout): input validated in clap"
    ) ;

    // Bench file.
    conf.bench_file = if let Some(f) = self.sub_value_of("run", "BENCHS") {
      f
    } else {
      bail!(
        "no benchmark file specified in command line or configuration file"
      )
    } ;

    Ok(())
  }
}