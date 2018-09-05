//! Benchi's errors and error handling.

use common::*;
use std::io::Write;

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
  }
}

/// Clap error.
pub fn clap_err<S1: Into<String>, S2: Into<String>>(arg: S1, blah: S2) -> Error {
    Error::from_kind(ErrorKind::Clap(arg.into(), blah.into()))
}

/// Prints an error and exits.
fn write_err_exit<C: ColorExt, W: Write>(
    conf: &C,
    err: &Error,
    w: &mut W,
) -> ::std::io::Result<()> {
    let (head, indent) = (conf.bad("|===| "), conf.bad("| "));
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
pub fn print_one_err<C: ColorExt>(conf: &C, err: &Error) {
    let stderr = &mut ::std::io::stderr();

    if let Err(io_e) = write_err_exit(conf, &err, stderr) {
        println!(
            "An error occured, but writing to stderr {}:",
            conf.bad("failed")
        );
        println!("> {}", io_e);
        println!();

        let stdout = &mut ::std::io::stdout();
        if let Err(io_e) = write_err_exit(conf, &err, stdout) {
            println!("Writing to stdout {}:", conf.bad("also failed"));
            println!("> {}", io_e);
            println!();

            println!("{} Original error:", conf.bad("|===|"));
            println!("{} {}", conf.bad("|"), err);
            println!("{}", conf.bad("|===|"))
        }
    }
}

/// Prints an error and exits if `exit` is true.
pub fn print_err<C: ColorExt>(conf: &C, err: &Error, exit: bool) {
    print_one_err(conf, &err);
    if exit {
        ::std::process::exit(2)
    }
}
