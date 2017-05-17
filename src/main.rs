//! `benchi` runs benchmarks.

#![forbid(missing_docs)]
#![allow(non_upper_case_globals)]

extern crate indicatif ;
extern crate ansi_term as ansi ;
#[macro_use]
extern crate regex ;
#[macro_use]
extern crate nom ;
#[macro_use]
extern crate error_chain ;
#[macro_use]
extern crate lazy_static ;


/// Errors.
pub mod errors {
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
  >(arg: S1, blah: S2) -> ErrorKind {
    ErrorKind::Clap( arg.into(), blah.into() )
  }
}

#[macro_use]
pub mod common ;
pub mod clap ;

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
    },
    Err(e) => println!("Err: {}", e)
  }
}