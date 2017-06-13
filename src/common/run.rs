//! Run basic types and helpers.

use common::* ;


/// Run configuration. Constructor is private on purpose, it does path
/// substitution.
#[derive(Debug)]
pub struct RunConf {
  /// Number of parallel bench runs.
  pub bench_par: usize,
  /// Number of parallel tool runs.
  pub tool_par: usize,
  /// Timeout.
  pub timeout: Duration,
  /// Output directory.
  pub out_dir: String,
  /// Tool configuration file.
  pub tool_file: String,
  /// Benchmark file.
  pub bench_file: String,
  /// Trying?
  pub try: Option<usize>,
  /// Logging stdout?
  pub log_stdout: bool,
  /// Global configuration.
  gconf: GConf,
  /// Validator Conf.
  vald_conf: ValdConf,
}
impl GConfExt for RunConf {
  fn gconf(& self) -> & GConf { & self.gconf }
}
impl ValdConfExt for RunConf {
  fn vald_conf(& self) -> & ValdConf { & self.vald_conf }
}
impl RunConf {
  /// Creates a configuration.
  #[inline]
  pub fn mk(
    bench_par: usize, tool_par: usize,
    timeout: Duration, try: Option<usize>, log_stdout: bool,
    out_dir: String, tool_file: String, bench_file: String,
    gconf: GConf, vald_conf: ValdConf
  ) -> Self {
    let out_dir = out_dir.path_subst() ;
    RunConf {
      bench_par, tool_par, timeout, try, log_stdout,
      out_dir, tool_file, bench_file,
      gconf, vald_conf
    }
  }

  /// Name of the validator for some benchmark.
  #[inline]
  pub fn validator_path_of(& self, tool: & ToolConf) -> Option<PathBuf> {
    if tool.validator.is_none() {
      None
    } else {
      let mut path = PathBuf::from(& self.out_dir) ;
      path.push(& tool.short) ;
      path.push("validator") ;
      path.set_extension("sh") ;
      Some(path)
    }
  }
}


/// Clap version of the run commands: has a mandatory configuration file
/// `CONF` in position 1.
pub fn clap_run_subcommand<'a, 'b>() -> ::clap_lib::App<'a, 'b> {
  run_subcommand(
    Some(
      ::clap_lib::Arg::with_name("CONF").help(
        "The configuration file (see `benchi conf -h` for details)"
      ).required(true).index(1).value_name("conf file")
    )
  )
}

/// Configuration file version of the run commands: no configuration file
/// argument.
pub fn conf_run_subcommand<'a, 'b>() -> ::clap_lib::App<'a, 'b> {
  run_subcommand(None)
}


/// The `run` subcommand.
///
/// The optional argument, if present causes the benchmark file argument to
/// have index `2` instead of `1`. This is used to add the configuration file
/// argument before the benchmark file in the clap subcommand.
///
/// In the subcommand for parsing options from the configuration file however,
/// there is no configuration file argument (obviously).
fn run_subcommand<'a, 'b>(
  add_arg: Option< ::clap_lib::Arg<'a, 'b> >
) -> ::clap_lib::App<'a, 'b> {
  use clap_lib::* ;
  use consts::clap::* ;
  use clap::utils::* ;
  let bench_file_index = if add_arg.is_some() { 2 } else { 1 } ;

  let run_app = SubCommand::with_name("run").about(
    "Runs benchmarks according to some configuration file (see `conf` \
    subcommand)."
  ).before_help(
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
      "Sets the output directory"
    ).value_name("dir").default_value("<today>_at_<now>").takes_value(true)
  ).arg(
    Arg::with_name("timeout").short("-t").long("--timeout").help(
      "Sets the timeout for each run"
    ).value_name(::consts::clap::tmo_format).validator(
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
    ).value_name("bench file").index(bench_file_index)
  ) ;

  if let Some(arg) = add_arg {
    run_app.arg(arg)
  } else {
    run_app
  }
}