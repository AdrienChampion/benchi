//! Command-Line Argument Parsing (clap).

use std::str::FromStr ;

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



/// Clap.
pub fn work() -> Res<Conf> {
  use new_clap::* ;

  let matches = App::new(
    crate_name!()
  ).version(
    crate_version!()
  ).author(
    crate_authors!()
  ).about(
    "`benchi` is a customizable benchmarking tool."
  ).arg(
    Arg::with_name("out_dir").short("-o").long("--out_dir").help(
      "Sets the output directory"
    ).value_name("dir").default_value("./").takes_value(true)
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
    ).default_value("on").takes_value(true).validator(bool_validator)
  ).subcommand(


    //
    // |===| Run subcommand.
    //
    SubCommand::with_name("run").about(
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
      ).value_name("int").takes_value(true)
    ).arg(
      Arg::with_name("log_out").long("--log_output").help(
        "Log the output of the runs"
      )
    ).arg(
      Arg::with_name("CONF").help(
        "The configuration file (see `benchi conf -h` for details)"
      ).required(true).index(1)
    ).arg(
      Arg::with_name("BENCHS").help(
        "\
The file containing the inputs to give to the tools. Optional, can be
specified in the configuration file.\
        "
      ).index(2)
    )


  ).get_matches() ;


  // Quiet / verbose.
  let quiet = matches.is_present("quiet") ;
  let verb = matches.is_present("verbose") ;

  // Colored.
  let colored = matches.value_of("colored").and_then(
    |s| bool_of_str(& s)
  ).expect(
    "unreachable(colored): default is provided and input validated in clap"
  ) ;

  // Output directory.
  let out_dir = matches.value_of("out_dir").expect(
    "unreachable(out_dir): default is provided"
  ).to_string() ;

  // Run mode.
  if let Some(matches) = matches.subcommand_matches("run") {

    let log_output = matches.is_present("log_out") ;

    // Bench and tool parallel settings.
    let bench_par = matches.value_of("para_benchs").map(
      usize::from_str
    ).expect(
      "unreachable(bench_par): default is provided"
    ).expect(
      "unreachable(bench_par): input validated in clap"
    ) ;
    let tool_par = matches.value_of("para_tools").map(
      usize::from_str
    ).expect(
      "unreachable(tool_par): default is provided"
    ).expect(
      "unreachable(tool_par): input validated in clap"
    ) ;

    // Timeout.
    let timeout = matches.value_of("timeout").map(
      tmo_of_str
    ).expect(
      "unreachable(timeout): default is provided"
    ).expect(
      "unreachable(timeout): input validated in clap"
    ) ;

    // Conf file.
    let tool_file = matches.value_of("CONF").expect(
      "unreachable(CONF): default is provided"
    ).to_string() ;
    // Bench file.
    let bench_file = matches.value_of("BENCHS").map(|s| s.to_string()) ;

    Ok(
      Conf::mk(
        bench_par, tool_par, timeout,
        out_dir, tool_file, bench_file,
        quiet, verb, log_output, colored
      )
    )

  } else {
    panic!("aaa")
  }

}



