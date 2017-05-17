//! Command-Line Argument Parsing (clap).

use std::env::{ Args, args } ;
use std::str::FromStr ;

use regex::Regex ;

use errors::* ;
use common::* ;

/// Wrapper around `std::env::Args` to allow pushing back an argument.
pub struct ArgVec {
  /// Prefix where things can be pushed.
  ///
  /// The length of the prefix should always be 0 or 1.
  pref: Vec<String>,
  /// Tail where things are popped if `pref` is empty.
  tail: Args,
}
impl ArgVec {
  /// Fetches the arguments.
  pub fn mk() -> Self {
    ArgVec {
      pref: Vec::with_capacity(1), tail: args(),
    }
  }
  /// Pops an argument.
  pub fn pop(& mut self) -> Option<String> {
    debug_assert!{ self.pref.len() <= 1 }
    self.pref.pop().or_else( || self.tail.next() )
  }
  /// Pushes an argument
  pub fn push(& mut self, arg: String) {
    debug_assert_eq!{ self.pref.len(), 0 }
    self.pref.push(arg)
  }
}

// Used for error reporting
static tmo_format: & str = "<int>['min'|'s']" ;
lazy_static!{
  static ref tmo_regex: Regex = Regex::new(
    r"^(\d*)(min|s)$"
  ).unwrap() ;
}
static tmo_name: & str = "argument timeout" ;
fn tmo_err(got: & str) -> ErrorKind {
  clap_err(
    tmo_name, format!("expected `{}`, got {}", tmo_format, got)
  )
}
/// Timeout clap.
fn timeout_clap(args: & mut ArgVec, conf: & mut Duration) -> Res<()> {
  if let Some(arg) = args.pop() {
    if let Some(caps) = tmo_regex.captures(& arg) {
      debug_assert_eq!{ caps.len(), 3 }

      let timeout = caps.get(1).ok_or(
        tmo_err(& arg)
      ).and_then(
        |to| u64::from_str(
          to.as_str()
        ).map_err(
          |e| clap_err(
            tmo_name,
            format!("expected integer, got {} ({})", to.as_str(), e)
          )
        )
      ).and_then(
        |num| {
          caps.get(2).ok_or(
            tmo_err(& arg)
          ).and_then(
            |unit| match unit.as_str() {
              "min" => Ok( Duration::new(60 * num, 0) ),
              "s" => Ok( Duration::new(num, 0) ),
              _ => Err( tmo_err(& arg) ),
            }
          )
        }
      ) ;

      * conf = match timeout {
        Ok(to) => to,
        Err(e) => bail!(e),
      } ;

      Ok(())

    } else { bail!( tmo_err(& arg) ) }
  } else { bail!( tmo_err("nothing") ) }
}






static mode_name: & str = "option bench/tool mode" ;
fn mode_err(got: & str) -> ErrorKind {
  clap_err(
    mode_name, format!("expected `bench` or `tool`, got {}", got)
  )
}
/// Mode parser.
fn mode_clap(
  args: & mut ArgVec, file_par: & mut bool, tool_par: & mut bool
) -> Res<()> {
  
  'is_mode: while let Some(arg) = args.pop() {
    if arg == "//" {
      let okay = args.pop().ok_or(
        mode_err("nothing")
      ).and_then(
        |arg| match arg.as_str() {
          "bench" => {
            * file_par = true ;
            Ok(())
          },
          "tool" => {
            * tool_par = true ;
            Ok(())
          },
          _ => Err( mode_err(& arg) ),
        }
      ) ;
      if let Err(e) = okay { bail!(e) }
    } else {
      args.push(arg) ;
      break 'is_mode
    }
  }
  
  Ok(())
}


static tool_file_name: & str = "tool file argument" ;
/// Parses all arguments.
pub fn work() -> Res<Conf> {
  let args = & mut ArgVec::mk() ;
  let mut conf = Conf::default() ;
  // First argument's name of the binary.
  let _ = args.pop() ;

  try!( timeout_clap(args, & mut conf.timeout) ) ;

  try!( mode_clap(args, & mut conf.file_par, & mut conf.tool_par) ) ;

  if let Some(tool_file) = args.pop() {

    {
      let path = Path::new(& tool_file) ;
      if ! path.exists() {
        bail!(
          clap_err(
            tool_file_name, format!("file `{}` does not exist", tool_file)
          )
        )
      }
      if ! path.is_file() {
        bail!(
          clap_err(
            tool_file_name, format!("`{}` is a directory", tool_file)
          )
        )
      }
    }
    conf.tool_file = tool_file ;

  } else {
    bail!(
      clap_err(tool_file_name, "none provided")
    )
  }

  // Extra arguments?
  if let Some(arg) = args.pop() {
    bail!(
      clap_err( format!("unexpected argument `{}`", arg), "")
    )
  }

  Ok(conf)
}