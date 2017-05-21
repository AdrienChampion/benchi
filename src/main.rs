//! `benchi` runs benchmarks.

#![forbid(missing_docs)]
#![allow(non_upper_case_globals)]

#[macro_use]
extern crate clap as new_clap ;
extern crate pbr ;
extern crate ansi_term as ansi ;
extern crate regex ;
#[macro_use]
extern crate nom ;
#[macro_use]
extern crate error_chain ;
#[macro_use]
extern crate lazy_static ;

#[macro_use]
pub mod common ;
pub mod clap ;
pub mod parse ;
pub mod run ;


use common::* ;
use errors::* ;


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
      #[doc = "Unimplemented."]
      Unimpl(conf: Arc<Conf>, blah: String) {
        description("unimplemented feature")
        display(
          "feature {} is {} yet", blah, conf.sad("not implemented")
        )
      }
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
      ToolRun(conf: Arc<Conf>, tool: ToolConf, bench: String) {
        description("error during tool run")
        display(
          "failure while running {} on {}", conf.emph(& tool.name), bench
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
  fn write_err_exit<W: Write>(
    conf: & Conf, err: & Error, w: & mut W
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
  pub fn print_one_err(conf: & Conf, err: Error) {
    let stderr = & mut ::std::io::stderr() ;

    if let Err(io_e) = write_err_exit(& conf, & err, stderr) {
      println!(
        "An error occured, but writing to stderr {}:", conf.bad("failed")
      ) ;
      println!("> {}", io_e) ;
      println!("") ;

      let stdout = & mut ::std::io::stdout() ;
      if let Err(io_e) = write_err_exit(& conf, & err, stdout) {
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
  pub fn print_err(conf: & Conf, err: Error, exit: bool) {
    print_one_err(conf, err) ;
    if exit {
      ::std::process::exit(2)
    }
  }
}



macro_rules! while_opening {
  ($conf:expr) => ({
    let e = ErrorKind::Msg(
      format!("while opening tool conf file {}", $conf.emph(& $conf.tool_file))
    ) ;
    || e
  }) ;
}







/// Entry point.
fn main() {

  match clap::work() {
    Ok(mut conf) => {

      let instance = match load_instance(& mut conf) {
        Ok(instance) => instance,
        Err(e) => {
          print_err(& conf, e, true) ;
          unreachable!()
        },
      } ;

      let (conf, instance) = (
        Arc::new(conf), Arc::new(instance)
      ) ;
      
      if let Err(e) = work( conf.clone(), instance.clone() ) {
        print_err(& conf, e, true)
      } else {
        ::std::process::exit(0)
      }
    },
    Err(e) => print_err(& Conf::default(), e, true)
  }
}



fn load_instance(conf: & mut Conf) -> Res<Instance> {
  use std::io::Read ;

  // Create output directory if it doesn't already exist.
  try!(
    mk_dir(& conf.out_dir).chain_err(
      || format!(
        "while creating output directory `{}`", conf.emph(& conf.out_dir)
      )
    )
  ) ;

  let mut file = try!(
    File::open(& conf.tool_file).chain_err( while_opening!(conf) )
  ) ;
  let mut buff = Vec::with_capacity(217) ;
  let _ = try!(
    file.read_to_end(& mut buff).chain_err( while_opening!(conf) )
  ) ;

  let tool_confs = try!(
    ::parse::work(conf, & buff)
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

  let benchs = {

    let buff_read = try!(
      File::open(& conf.bench_file).map(
        |file| BufReader::new(file)
      )
    ) ;
    let mut benchs = Vec::with_capacity( 200 ) ;

    for maybe_line in buff_read.lines() {
      benchs.push( try!(maybe_line) )
    }
    benchs
  } ;

  let instance = Instance::mk(tool_confs, benchs) ;

  if instance.tool_len() < conf.tool_par {
    conf.tool_par = instance.tool_len()
  }
  if instance.bench_len() < conf.bench_par {
    conf.bench_par = instance.bench_len()
  }

  log!(
    conf =>
      "Running {} tools on {} benchmarks...",
      instance.tool_len(), instance.bench_len()
  ) ;

  Ok(instance)
}



fn work(conf: Arc<Conf>, instance: Arc<Instance>) -> Res<()> {
  let master = try!(
    run::Master::mk(conf.clone(), instance)
  ) ;

  let time = try!( master.run() ) ;

  log!(
    conf =>
      let time = format!(
        "{}.{}", time.as_secs(), time.subsec_nanos() / 1_000_000u32
      ) ;
      "Done in {}s", conf.emph(& time)
  ) ;

  Ok(())
}