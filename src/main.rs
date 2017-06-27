//! `benchi` runs benchmarks.

#![forbid(missing_docs)]
#![allow(non_upper_case_globals)]

extern crate chrono ;
#[macro_use]
extern crate clap as clap_lib ;
extern crate pbr ;
extern crate ansi_term as ansi ;
extern crate regex ;
#[macro_use]
extern crate nom ;
#[macro_use]
extern crate error_chain ;
#[macro_use]
extern crate lazy_static ;

pub mod consts ;
#[macro_use]
pub mod common ;
pub mod clap ;
pub mod parse ;
pub mod run ;
pub mod loading ;
pub mod plot ;
pub mod inspect ;


use errors::* ;
use common::* ;
use common::run::* ;


/// Errors.
pub mod errors {
  use std::io::Write ;
  use common::* ;

  error_chain!{
    types {
      Error, ErrorKind, ResExt, Res ;
    }

    foreign_links {
      Io(::std::io::Error)
      #[doc = "IO error."] ;
    }

    errors {
      #[doc = "Clap: argument name, error description."]
      Clap(arg: String, blah: String) {
        description("clap error")
        display(
          "clap error on {}{}", arg, if blah.is_empty() {
            "".to_string()
          } else {
            format!(": {}", blah)
          }
        )
      }
      #[doc = "Tool run error."]
      ToolRun(conf: GConf, tool: ToolConf, bench: String) {
        description("error during tool run")
        display(
          "failure while running '{}' on '{}'",
          conf.sad(& tool.name), conf.emph(& bench)
        )
      }
    }
  }

  /// Clap error.
  pub fn clap_err<
    S1: Into<String>, S2: Into<String>
  >(arg: S1, blah: S2) -> Error {
    Error::from_kind(
      ErrorKind::Clap( arg.into(), blah.into() )
    )
  }

  /// Prints an error and exits.
  fn write_err_exit<C: ColorExt, W: Write>(
    conf: & C, err: & Error, w: & mut W
  ) -> ::std::io::Result<()> {
    let (head, indent) = (
      conf.bad("|===| "), conf.bad("| ")
    ) ;
    try!{
      writeln!( w, "{}{}:", head, conf.bad("Error") )
    }
    for err in err.iter() {
      try!{ writeln!(w, "{}{}", indent, err) }
    }
    try!{ writeln!( w, "{}", head ) }
    Ok(())
  }

  /// Prints an error.
  pub fn print_one_err<C: ColorExt>(conf: & C, err: Error) {
    let stderr = & mut ::std::io::stderr() ;

    if let Err(io_e) = write_err_exit(conf, & err, stderr) {
      println!(
        "An error occured, but writing to stderr {}:", conf.bad("failed")
      ) ;
      println!("> {}", io_e) ;
      println!("") ;

      let stdout = & mut ::std::io::stdout() ;
      if let Err(io_e) = write_err_exit(conf, & err, stdout) {
        println!(
          "Writing to stdout {}:", conf.bad("also failed")
        ) ;
        println!("> {}", io_e) ;
        println!("") ;

        println!("{} Original error:", conf.bad("|===|")) ;
        println!("{} {}", conf.bad("|"), err) ;
        println!("{}", conf.bad("|===|"))
      }
    }
  }


  /// Prints an error and exits if `exit` is true.
  pub fn print_err<C: ColorExt>(conf: & C, err: Error, exit: bool) {
    print_one_err(conf, err) ;
    if exit {
      ::std::process::exit(2)
    }
  }
}



macro_rules! while_opening {
  ($conf:expr, $file:ident) => ({
    let e = ErrorKind::Msg(
      format!("while opening file {}", $conf.emph(& $conf.$file))
    ) ;
    || e
  }) ;
}







/// Entry point.
fn main() {

  match clap::work() {
    Ok( Clap::Run(mut conf, tools) ) => {

      log!{
        conf, verb =>
          "{}:", conf.emph("Configuration") ;
          "           timeout: {}s", conf.timeout.as_secs() ;
          "           out dir: {}", conf.happy(& conf.out_dir) ;
          "      benchs in //: {}", conf.bench_par ;
          "       tools in //: {}", conf.tool_par ;
          "  max threads used: {}",
          conf.emph(& format!("{}", conf.bench_par * conf.tool_par)) ;
          {
            if let Some(max) = conf.try {
              log!{ conf, verb => "               try: {}", max }
            }
          }
          ""
      }

      log!{
        conf, verb => "Loading instance..."
      }

      let instance = match load_instance(& mut conf, tools) {
        Ok(instance) => instance,
        Err(e) => {
          print_err(& conf, e, true) ;
          unreachable!()
        },
      } ;

      log!{
        conf, verb => "done"
      }

      let (conf, instance) = (
        Arc::new(conf), Arc::new(instance)
      ) ;

      if let Err(e) = work( conf.clone(), instance.clone() ) {
        print_err(& * conf, e, true)
      }
    },

    Ok( Clap::Plot(conf, kind) ) => {
      if let Err(e) = plot::work(& conf, kind) {
        print_err(& conf, e, true)
      }
    },

    Ok( Clap::Conf(conf, file) ) => {
      if let Err(e) = common::example_conf_file(& conf, file) {
        print_err(& conf, e, true)
      }
    },

    Err(e) => print_err(& GConf::default(), e, true)
  }

  ::std::process::exit(0)
}


fn load_instance(conf: & mut RunConf, tools: Vec<ToolConf>) -> Res<Instance> {

  // Make sure names are unique.
  {
    let mut tool_iter = tools.iter() ;
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

  let mut benchs = {

    let buff_read = try!(
      File::open(& conf.bench_file).map(
        |file| BufReader::new(file)
      ).chain_err(
        while_opening!(conf, bench_file)
      )
    ) ;
    let mut benchs = Vec::with_capacity( 200 ) ;

    for maybe_line in buff_read.lines() {
      benchs.push( try!(maybe_line) )
    }
    benchs

  } ;

  if let Some(max) = conf.try {
    benchs.truncate(max)
  }

  // let tools = tools.into_iter().map(
  //   |tool| tool.to_tool_conf()
  // ).collect() ;

  let instance = Instance::mk(tools, benchs) ;

  if instance.tool_len() < conf.tool_par {
    conf.tool_par = instance.tool_len()
  }
  if instance.bench_len() < conf.bench_par {
    conf.bench_par = instance.bench_len()
  }

  Ok(instance)
}



fn work(conf: Arc<RunConf>, instance: Arc<Instance>) -> Res<()> {
  if instance.tool_len() == 0 || instance.bench_len() == 0 {
    return Ok(())
  }

  // Check that the timeout command exists.
  match Command::new("timeout").arg("1").arg("echo").arg("test").stdout(
    ::std::process::Stdio::null()
  ).stderr(
    ::std::process::Stdio::null()
  ).stdin(
    ::std::process::Stdio::null()
  ).status() {
    Err(_) => bail!(
      format!(
        "could not find `{}` command, make sure it is installed",
        conf.emph("timeout")
      )
    ),
    _ => (),
  }

  // Create output directory if it doesn't already exist.
  try!(
    mk_dir(& conf.out_dir).chain_err(
      || format!(
        "while creating output directory `{}`", conf.emph(& conf.out_dir)
      )
    )
  ) ;

  let mut master = try!(
    run::Master::mk(conf.clone(), instance.clone())
  ) ;

  log!(
    conf =>
      { log!( conf, verb => "" ) }
      "Running {} tools on {} benchmarks...",
      instance.tool_len(), instance.bench_len()
  ) ;

  let time = try!( master.run() ) ;

  log!(
    conf =>
      let time = format!(
        "{}.{}", time.as_secs(), time.subsec_nanos() / 1_000_000u32
      ) ;
      " " ;
      let pref = if master.errors > 0 || master.timeouts > 0 {
        " with "
      } else {
        ""
      } ;
      let timeouts = if master.timeouts > 0 {
        format!("{} timeouts", master.timeouts)
      } else {
        "".into()
      } ;
      let sep = if master.errors > 0 && master.timeouts > 0 {
        " and "
      } else {
        ""
      } ;
      let errors = if master.errors > 0 {
        format!("{} errors", master.errors)
      } else {
        "".into()
      } ;
      "Done in {}s{}{}{}{}",
      conf.emph(& time),
      pref, conf.sad(& timeouts), sep, conf.bad(& errors) ;
      "" ;
      "Average runtime:" ;
      {
        for tool in instance.tools() {
          let (avg, cnt) = master.avg_runtime[tool] ;
          log!{
            conf =>
              "  {}: {}",
              conf.emph(& instance[tool].name),
              if cnt > 0 { avg.as_sec_str() } else {
                "no benchmark passed".into()
              }
          }
        }
      } {
        if master.inconsistencies > 0 {
          log!{
            conf =>
            "" ; "{}", conf.bad(
              & format!(
                "Found {} inconsistenc{}",
                master.inconsistencies,
                if master.inconsistencies > 1 { "ies" } else { "y" }
              )
            )
          }
        }
      }
  ) ;

  Ok(())
}