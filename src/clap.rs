//! Command-Line Argument Parsing (clap).

use clap_lib::{
  App, Arg, SubCommand, ArgMatches, AppSettings
} ;

use errors::* ;
use common::* ;
use common::run::* ;
use common::plot::* ;
use common::inspect::* ;

use consts::clap::* ;

use clap::utils::* ;

macro_rules! while_opening {
  ($conf:expr, $file:expr) => (
    || format!(
      "while opening file {}",
      $conf.emph( $file.as_ref().to_str().expect("ill-formated file name...") )
    )
  ) ;
}

/// Useful functions for clap.
pub mod utils {
  use errors::* ;
  use common::* ;
  use consts::clap::* ;

  /// Returns a clap error for the timeout argument.
  pub fn tmo_err(got: & str) -> Error {
    clap_err(
      "argument timeout",
      format!("expected `{}`, got {}", ::consts::clap::tmo_format, got)
    )
  }

  /// Timeout from a string.
  pub fn tmo_of_str(s: & str) -> Res<Duration> {
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
  pub fn tmo_validator(s: String) -> Result<(), String> {
    if let Ok(_) = tmo_of_str(& s) {
      Ok(())
    } else {
      Err(
        format!("expected <{}>, got `{}`", ::consts::clap::tmo_format, s)
      )
    }
  }

  /// Boolean of a string.
  pub fn bool_of_str(s: & str) -> Option<bool> {
    match & s as & str {
      "on" | "true" => Some(true),
      "off" | "false" => Some(false),
      _ => None,
    }
  }

  /// Validates boolean input.
  pub fn bool_validator(s: String) -> Result<(), String> {
    if let Some(_) = bool_of_str(& s) {
      Ok(())
    } else {
      Err(
        format!("expected `on/true` or `off/false`, got `{}`", s)
      )
    }
  }

  /// Validates integer input.
  pub fn int_validator(s: String) -> Result<(), String> {
    match usize::from_str(& s) {
      Ok(_) => Ok(()),
      Err(_) => Err(
        format!("expected an integer, got `{}`", s)
      ),
    }
  }



  /// Loads the tool file.
  pub fn load_conf<F: AsRef<Path>>(conf: & GConf, tool_file: F) -> Res<
    ( ValdConf, Vec<ToolConf>, ::clap_lib::ArgMatches<'static> )
  > {
    use std::io::Read ;
    use clap::* ;

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

    let option_clap = main_app().subcommand(
      conf_run_subcommand()
    ) ;
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
}









/// Wraps several matches, a primary and some secondary ones. Whenever a
/// request is made, it goes to the primary matches first and then, if **no
/// occurrence is found** the secondary ones.
#[derive(Clone, Debug)]
pub struct Matches<'a> {
  primary: ArgMatches<'a>,
  secondary: Vec<ArgMatches<'a> >,
}
impl<'a> Matches<'a> {
  /// Creates a `Matches` from an `ArgMatches`.
  pub fn mk(primary: ArgMatches<'a>) -> Self {
    Matches { primary, secondary: vec![] }
  }

  /// Adds a secondary `ArgMatches`.
  pub fn push(& mut self, secondary: ArgMatches<'a>) {
    self.secondary.push(secondary)
  }

  /// Checks if an argument is present in any of the matches.
  pub fn occurs(& self, name: & str) -> bool {
    if self.primary_occurs(name) {
      true
    } else {
      for secondary in & self.secondary {
        if secondary.occurrences_of(name) > 0 {
          return true
        }
      }
      false
    }
  }

  /// Checks if an argument is present in the primary match.
  pub fn primary_occurs(& self, name: & str) -> bool {
    self.primary.occurrences_of(name) > 0
  }

  /// Calls `values_of` **on the primary matches only**.
  pub fn primary_values_of(
    & 'a self, name: & str
  ) -> Option< ::clap_lib::Values<'a> > {
    self.primary.values_of(name)
  }

  /// Retrieves the value of an argument.
  pub fn value_of(& self, name: & str) -> Option<& str> {
    if self.primary_occurs(name) || self.secondary.is_empty() {
      self.primary.value_of(name)
    } else {
      for secondary in & self.secondary {
        if secondary.occurrences_of(name) > 0 {
          return secondary.value_of(name)
        }
      }
      // Occurs nowhere, returning the value from primary (default or nothing).
      self.primary.value_of(name)
    }
  }

  /// Subcommand matches.
  pub fn subcommand_matches(
    & self, name: & str
  ) -> Option< Self > {
    let mut primary = self.primary.subcommand_matches(name).cloned() ;
    let mut secondary = vec![] ;
    for sec in & self.secondary {
      let sec_sub = sec.subcommand_matches(name).cloned() ;
      if primary.is_none() {
        primary = sec_sub
      } else if let Some(m4tch) = sec_sub {
        secondary.push(m4tch)
      }
    }
    primary.map(
      |primary| Matches { primary, secondary }
    )
  }
}


/// The main application.
pub fn main_app<'a, 'b>() -> App<'a, 'b> {
  App::new(
    crate_name!()
  ).version(
    crate_version!()
  ).author(
    crate_authors!()
  ).about(
    "`benchi` is a benchmarking tool."
  ).arg(
    Arg::with_name("force").short("-f").long("--force").help(
      "When writing a file, overwrite if present"
    ).default_value("off").takes_value(true).number_of_values(
      1
    ).validator(
      bool_validator
    ).value_name(bool_format)
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
    ).default_value("on").takes_value(true).number_of_values(
      1
    ).validator(
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

/// Builds the `GConf` from some matches.
pub fn gconf_of_matches<'a>(matches: & Matches<'a>) -> GConf {
  // File overwrite.
  let ow_files = matches.value_of("force").and_then(
    |s| {
      bool_of_str(& s)
    }
  ).expect(
    "unreachable(force): default is provided and input validated in clap"
  ) ;

  // Quiet / verbose.
  let verb = if matches.primary_occurs("quiet") {
    Verb::Quiet
  } else if matches.primary_occurs("verbose") {
    Verb::Verbose
  } else {
    if matches.occurs("quiet") {
      Verb::Quiet
    } else if matches.occurs("verbose") {
      Verb::Verbose
    } else {
      Verb::Normal
    }
  } ;

  // Colored.
  let colored = matches.value_of("colored").and_then(
    |s| {
      bool_of_str(& s)
    }
  ).expect(
    "unreachable(colored): default is provided and input validated in clap"
  ) ;

  GConf::mk(verb, colored, ow_files)
}



/// All the subcommands.
pub fn subcommands<'a, 'b>() -> Vec< App<'a, 'b> > {
  vec![
    clap_run_subcommand(),
    plot_subcommand(),
    inspect_subcommand(),
    conf_subcommand(),
  ]
}

/// Complete clap.
pub fn mk_clap<'a, 'b>() -> App<'a, 'b> {
  main_app().subcommands( subcommands() )
}

/// Clap.
pub fn work() -> Res< Clap > {
  let original_matches = mk_clap().get_matches() ;

  let matches = & Matches::mk(original_matches) ;

  if let Some(res) = run_clap(matches) {
    res
  } else if let Some(res) = plot_clap(matches) {
    res
  } else if let Some(res) = conf_clap(matches) {
    res
  } else {
    bail!("called with unimplemented command")
  }
}






/// The configuration subcommand (explanation).
fn conf_subcommand<'a, 'b>() -> App<'a, 'b> {

  SubCommand::with_name("conf").about(
    "Explanation of the configuration file format."
  ).before_help("\
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
  )
}


/// `Conf` conf from some matches. `None` if `conf` subcommant is not present.
pub fn conf_clap<'a>(
  matches: & ::clap::Matches<'a>
) -> Option< Res<Clap> > {
  if let Some(conf_matches) = matches.subcommand_matches("conf") {
    let conf = ::clap::gconf_of_matches(& matches) ;
    let conf_file = conf_matches.value_of("CONF").expect(
      "unreachable(CONF): required"
    ).to_string() ;
    Some(
      Ok( Clap::Conf(conf, conf_file) )
    )
  } else {
    None
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


#[test]
fn clap_fails() {
  let clap = mk_clap() ;

  let args: Vec<& 'static str> = vec!["benchi", ] ;
  assert!( clap.clone().get_matches_from_safe(args).is_err() ) ;
  let args: Vec<& 'static str> = vec!["benchi", "run"] ;
  assert!( clap.clone().get_matches_from_safe(args).is_err() ) ;
  let args: Vec<& 'static str> = vec!["benchi", "conf"] ;
  assert!( clap.clone().get_matches_from_safe(args).is_err() ) ;
  let args: Vec<& 'static str> = vec!["benchi", "plot"] ;
  assert!( clap.clone().get_matches_from_safe(args).is_err() ) ;
  let args: Vec<& 'static str> = vec!["benchi", "plot", "file.plot"] ;
  assert!( clap.clone().get_matches_from_safe(args).is_err() ) ;
  let args: Vec<& 'static str> = vec!["benchi", "plot", "file.plot", "cumul"] ;
  assert!( clap.clone().get_matches_from_safe(args).is_err() ) ;
  let args: Vec<& 'static str> = vec![
    "benchi", "plot", "file.plot", "compare"
  ] ;
  assert!( clap.clone().get_matches_from_safe(args).is_err() ) ;
  let args: Vec<& 'static str> = vec![
    "benchi", "plot", "file.plot", "compare", "data"
  ] ;
  assert!( clap.clone().get_matches_from_safe(args).is_err() ) ;
  let args: Vec<& 'static str> = vec![
    "benchi", "plot", "file.plot", "compare", "data", "data'", "data''"
  ] ;
  assert!( clap.clone().get_matches_from_safe(args).is_err() ) ;
}


#[test]
fn hierarchical_matches_and_gconf() {
  let clap = mk_clap() ;

  let args_1 = vec!["benchi", "run", "-o", "output", "run.conf", "benchs"] ;
  let m_1 = clap.clone().get_matches_from_safe(args_1).expect(
    "should not fail"
  ) ;
  let mut m = Matches::mk(m_1) ;
  let args_2 = vec!["benchi", "run", "-o", "blah", "blah.conf", "not benchs"] ;
  let m_2 = clap.clone().get_matches_from_safe(args_2).expect(
    "should not fail"
  ) ;
  m.push(m_2) ;
  let conf = ::clap::gconf_of_matches(& m) ;
  assert_eq!(
    conf, GConf::mk(Verb::Normal, true, false)
  ) ;
  assert!(
    m.subcommand_matches("run").unwrap().primary_occurs("out_dir")
  ) ;
  assert_eq!(
    m.subcommand_matches("run").unwrap().value_of("out_dir"), Some("output")
  ) ;
  assert_eq!(
    m.subcommand_matches("run").unwrap().value_of("CONF"), Some("run.conf")
  ) ;
  assert_eq!(
    m.subcommand_matches("run").unwrap().value_of("BENCHS"), Some("benchs")
  ) ;
  assert_eq!(
    m.value_of("force"), Some("off")
  ) ;

  let args_1 = vec!["benchi", "-f", "on", "run", "run.conf", "benchs"] ;
  let m_1 = clap.clone().get_matches_from_safe(args_1).expect(
    "should not fail"
  ) ;
  let mut m = Matches::mk(m_1) ;
  let args_2 = vec!["benchi", "run", "-o", "output", "blah.conf"] ;
  let m_2 = clap.clone().get_matches_from_safe(args_2).expect(
    "should not fail"
  ) ;
  m.push(m_2) ;
  let conf = ::clap::gconf_of_matches(& m) ;
  assert_eq!(
    conf, GConf::mk(Verb::Normal, true, true)
  ) ;
  assert_eq!(
    m.value_of("force"), Some("on")
  ) ;
  assert!(
    ! m.subcommand_matches("run").unwrap().primary_occurs("out_dir")
  ) ;
  assert!(
    m.subcommand_matches("run").unwrap().occurs("out_dir")
  ) ;
  assert_eq!(
    m.subcommand_matches("run").unwrap().value_of("out_dir"), Some("output")
  ) ;
  assert_eq!(
    m.subcommand_matches("run").unwrap().value_of("CONF"), Some("run.conf")
  ) ;
  assert_eq!(
    m.subcommand_matches("run").unwrap().value_of("BENCHS"), Some("benchs")
  ) ;

  let args_1 = vec!["benchi", "-q", "run", "run.conf"] ;
  let m_1 = clap.clone().get_matches_from_safe(args_1).expect(
    "should not fail"
  ) ;
  let mut m = Matches::mk(m_1) ;
  let args_2 = vec![
    "benchi", "-c", "off", "-f", "on", "run", "blah.conf", "benchs"
  ] ;
  let m_2 = clap.clone().get_matches_from_safe(args_2).expect(
    "should not fail"
  ) ;
  m.push(m_2) ;
  let conf = ::clap::gconf_of_matches(& m) ;
  assert_eq!(
    conf, GConf::mk(Verb::Quiet, false, true)
  ) ;
  assert_eq!(
    m.value_of("force"), Some("on")
  ) ;
  assert!(
    ! m.subcommand_matches("run").unwrap().primary_occurs("out_dir")
  ) ;
  assert!(
    ! m.subcommand_matches("run").unwrap().occurs("out_dir")
  ) ;
  assert_eq!(
    m.subcommand_matches("run").unwrap().value_of("CONF"), Some("run.conf")
  ) ;
  assert_eq!(
    m.value_of("colored"), Some("off")
  ) ;
  assert_eq!(
    m.value_of("force"), Some("on")
  ) ;
  assert_eq!(
    m.subcommand_matches("run").unwrap().value_of("BENCHS"), Some("benchs")
  ) ;
}