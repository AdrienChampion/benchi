//! Constants.

/// Time regex as a string (for reuse).
pub static time_re: & str = r"(?P<secs>\d\d*).(?P<nanos>\d\d*)" ;

/// Validator constants.
pub mod validator {
  /// Prefix added to validator scripts.
  pub static pref: & str = "\
#!/bin/bash
bench=\"$1\"
code=\"$2\"
out=\"$3\"
err=\"$4\"

\
  " ;
}

/// Clap-related constants.
pub mod clap {
  use regex::Regex ;

  /// Format for the timeout.
  pub static tmo_format: & str = "[int]s|[int]min" ;
  /// Format for booleans.
  pub static bool_format: & str = "on|true|off|false" ;

  #[test]
  fn regexes() {
    let tmo_res = tmo_regex.captures("720s").unwrap() ;
    assert_eq!( & tmo_res["value"], "720" ) ;
    assert_eq!( & tmo_res["unit"], "s" ) ;

    let tmo_res = tmo_regex.captures("3min").unwrap() ;
    assert_eq!( & tmo_res["value"], "3" ) ;
    assert_eq!( & tmo_res["unit"], "min" ) ;

    assert!( ! tmo_regex.is_match("720") ) ;
    assert!( ! tmo_regex.is_match("720 s") ) ;
    assert!( ! tmo_regex.is_match("s720") ) ;
    assert!( ! tmo_regex.is_match("s 720") ) ;
    assert!( ! tmo_regex.is_match("s") ) ;
    assert!( ! tmo_regex.is_match("720 min") ) ;
    assert!( ! tmo_regex.is_match("min720") ) ;
    assert!( ! tmo_regex.is_match("min 720") ) ;
    assert!( ! tmo_regex.is_match("min") ) ;
    assert!( ! tmo_regex.is_match("") ) ;
  }

  lazy_static!{
    #[doc = "
Regex for timeout in clap. Two groups: `value` (int) and `unit` (`min` or `s`).
    "]
    pub static ref tmo_regex: Regex = Regex::new(
      r"^(?P<value>\d\d*)(?P<unit>min|s)$"
    ).unwrap() ;
  }
}

/// Substitutions in user-provided data.
pub mod subst {
  use regex::Regex ;

  /// Today keyword. **Update CLAP help if you change this.**
  pub static today: & str = "<today>" ;
  /// Now keyword. **Update CLAP help if you change this.**
  pub static now: & str = "<now>" ;
  /// Timeout keyword. **Update CLAP help if you change this.**
  pub static timeout: & str = "<timeout>" ;

  #[test]
  fn regexes() {
    let today_res = today_re.replace_all(
      "blah<today> <today> <today>foo", "date"
    ) ;
    assert_eq!( today_res, "blahdate date datefoo" ) ;
    let now_res = now_re.replace_all(
      "blah<now> <now> <now>foo", "time"
    ) ;
    assert_eq!( now_res, "blahtime time timefoo" ) ;
    let timeout_res = timeout_re.replace_all(
      "blah<timeout> <timeout> <timeout>foo", "tmo"
    ) ;
    assert_eq!( timeout_res, "blahtmo tmo tmofoo" )
  }

  lazy_static!{
    #[doc = "Matches the `today` keyword."]
    pub static ref today_re: Regex = Regex::new(today).expect(
      "problem in `today` static regex"
    ) ;
    #[doc = "Matches the `now` keyword."]
    pub static ref now_re: Regex = Regex::new(now).expect(
      "problem in `now` static regex"
    ) ;
    #[doc = "Matches the `timeout` keyword."]
    pub static ref timeout_re: Regex = Regex::new(timeout).expect(
      "problem in `timeout` static regex"
    ) ;
  }
}

/// Bench data dumping.
pub mod dump {
  /// Comment prefix.
  pub static cmt_pref: & str = "#" ;
  #[doc = "Key for short names."]
  pub static short_name_key: & str = "short" ;
  #[doc = "Key for graph names."]
  pub static graph_name_key: & str = "graph" ;
  #[doc = "Key for commands."]
  pub static cmd_key: & str = "cmd" ;
  #[doc = "Key for timeouts."]
  pub static timeout_key: & str = "timeout" ;
  #[doc = "Key for the validator."]
  pub static vald_key: & str = "validator" ;
  #[doc = "Key for validator conf."]
  pub static vald_conf_key: & str = "validators" ;
  #[doc = "Key for validator conf success codes."]
  pub static vald_conf_suc_key: & str = "success" ;
}

/// Data-related regexs
pub mod data {
  use regex::Regex ;

  #[doc = "Indicates a timeout result."]
  pub static timeout_res: & str = "timeout" ;
  #[doc = "Indicates an error result."]
  pub static error_res: & str = "error" ;

  #[test]
  fn regexes() {
    let res = result_re.captures(
      r#"42 "benchmark line from file" success 72"#
    ).unwrap() ;
    assert_eq!(& res["uid"], "42") ;
    assert_eq!(& res["bench"], "benchmark line from file") ;
    assert_eq!(& res["res"], "success") ;
    assert_eq!(& res["vald"], "72") ;

    let res = success_re.captures(r"743044.123456789").unwrap() ;
    assert_eq!(& res["secs"], "743044") ;
    assert_eq!(& res["nanos"], "123456789") ;
    assert!( timeout_re.is_match("timeout") ) ;
    assert!( error_re.is_match("error") )
  }

  lazy_static!{

    #[doc = "
Regex extracting a benchmark line. Four groups:

- `uid`: bench uid,
- `bench`: bench line from the bench file,
- `res`: result,
- `vald`: validation code, if any.
    "]
    pub static ref result_re: Regex = Regex::new(
      r#"(?x)^
        \s*(?P<uid>\d\d*)
        \s*"(?P<bench>[^"]*)"
        \s*(?P<res>[^\s]*)
        \s*(?P<vald>\d*)
        \s*
      $"#
    ).expect(
      "problem in `runtime` static regex"
    ) ;
    #[doc =
      "
Regex matching a successful result.

Two groups:

- `secs`: seconds,
- `nanos`: nanoseconds.
      "
    ]
    pub static ref success_re: Regex = Regex::new(
      & format!("^{}$", ::consts::time_re)
    ).expect(
      "problem in `success` static regex"
    ) ;
    #[doc =
      "
Regex matching a timeout result. No group.
      "
    ]
    pub static ref timeout_re: Regex = Regex::new(
      r#"^\s*timeout\s*$"#
    ).expect(
      "problem in `timeout` static regex"
    ) ;
    #[doc =
      "
Regex matching an error result. No group.
      "
    ]
    pub static ref error_re: Regex = Regex::new(
      r#"^\s*error\s*$"#
    ).expect(
      "problem in `error` static regex"
    ) ;

  }
}

/// Example configuration file.
pub static ex_conf_file: & str = r#"# Example configuration file.

# Run `benchi help conf` for further details.

# Setting `run` options. Each of these options can be overriden with command-
# line arguments.
options: "-v run -o example/<today>_at_<now> --tools 2 --benchs 3 -t 5s"

# So `benchi -q run -t 3min <this file>` overrides the verbosity setting and
# the timeout from the options above.

find at root {
  short: root_find
  graph: Find at Root
  cmd: "find / -iname"
}

Find in Current Directory {
  short: curr_dir_find
  # Graph name is optional, none provided here so benchi will use the 'name'
  # of the tool instead: `Find in Current Directory` here.
  cmd: "find . -iname"
}

Find in Tmp {
  short: tmp_find
  cmd: "find /tmp -iname"
}

Find in Home {
  short: home_find
  cmd: "find /home -iname"
}

"# ;


/// Example benchmark file.
pub static ex_bench_file: & str = r#"[aA]*
[bB]*
[cC]*
[dD]*
[eE]*
[fF]*
[gG]*
[hH]*
[iI]*
[jJ]*
[kK]*
[lL]*
[mM]*
[nN]*
[oO]*
[pP]*
[qQ]*
[rR]*
[sS]*
[tT]*
[uU]*
[vV]*
[wW]*
[xX]*
[yY]*
[zZ]*"# ;
