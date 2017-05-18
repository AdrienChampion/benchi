//! `benchi` runs benchmarks.

#![forbid(missing_docs)]
#![allow(non_upper_case_globals)]

extern crate indicatif ;
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


/// Errors.
pub mod errors {
  use std::io::Write ;
  use common::Conf ;

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
      Unimpl(blah: String) {
        description("unimplemented feature")
        display("{} is unimplemented", blah)
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

  /// Writes the top-most error as a single line with trailing `nl`.
  fn writeln_top<W: Write>(
    conf: & Conf, err: & Error, w: & mut W
  ) -> ::std::io::Result<()> {
    use self::ErrorKind::* ;
    match * err.kind() {
      Msg(ref blah) => writeln!(
        w, "{}", blah
      ),
      Unimpl(ref blah) => writeln!(
        w, "feature {} is {} yet", blah, conf.sad("not implemented")
      ),
      Clap(ref arg, ref blah) => {
        try!( write!(w, "on {}", arg) ) ;
        if ! blah.is_empty() {
          writeln!(w, ": {}", blah)
        } else { Ok(()) }
      },
      Io(ref e) => writeln!(
        w, "on IO: {}", conf.sad( format!("{}", e) )
      ),
    }
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
    try!{ write!(w, "{}", indent) }
    try!{ writeln_top(conf, err, w) }
    try!{ writeln!( w, "{}", head ) }
    Ok(())
  }

  /// Prints an error and exits.
  pub fn print_err_exit(conf: Conf, err: Error) -> ! {
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

        println!("Original error trace:") ;
        for e in err.iter() {
          println!("{} {}", conf.bad(">"), e)
        }
        println!("")
      }
    }

    ::std::process::exit(2)
  }
}


use common::* ;
use errors::* ;

/// Entry point.
fn main() {
  match clap::work() {
    Ok(conf) => {
      println!("conf:") ;
      println!("   file_par: {}", conf.file_par) ;
      println!("   tool_par: {}", conf.tool_par) ;
      println!("    timeout: {}s", conf.timeout.as_secs()) ;
      println!("    out dir: {}", conf.out_dir) ;
      println!("  tool file: {}", conf.tool_file) ;
      println!("") ;
      
      if let Err(e) = work(& conf) {
        print_err_exit(conf, e)
      } else {
        ::std::process::exit(0)
      }
    },
    Err(e) => print_err_exit(Conf::default(), e)
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

fn work(conf: & Conf) -> Res<()> {
  use std::fs::File ;
  use std::io::Read ;

  let mut file = try!(
    File::open(& conf.tool_file).chain_err( while_opening!(conf) )
  ) ;
  let mut buff = Vec::with_capacity(217) ;
  let _ = try!(
    file.read_to_end(& mut buff).chain_err( while_opening!(conf) )
  ) ;

  let tool_confs = try!(
    ::parse::work(& buff)
  ) ;

  println!(
    "{} parsed tool configuration:", conf.happy("Successfully")
  ) ;

  for tool_conf in tool_confs {
    println!("  {} {{", conf.emph(tool_conf.name)) ;
    println!("    short: {}", conf.emph(tool_conf.short)) ;
    println!("    graph: {}", conf.emph(tool_conf.graph)) ;
    print!(  "      cmd: ") ;
    let mut iter = tool_conf.cmd.iter() ;
    if let Some(line) = iter.next() { print!("{}", line) }
    println!("") ;
    for line in iter {
      println!(  "           {}", line)
    }
    println!("  }}") ;
  }

  Ok(())
}