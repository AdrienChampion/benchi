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
  use regex::Regex ;

  /// Comment prefix.
  pub static cmt_pref: & str = "#" ;

  lazy_static!{
    #[doc = "Dump prefix (double comment prefix)."]
    pub static ref dmp_pref: String = format!("{}{}", cmt_pref, cmt_pref) ;
  }

  #[doc = "Key for short names."]
  pub static new_short_name_key: & str = "short" ;
  #[doc = "Key for graph names."]
  pub static new_graph_name_key: & str = "graph" ;
  #[doc = "Key for commands."]
  pub static new_cmd_key: & str = "cmd" ;
  #[doc = "Key for timeouts."]
  pub static new_timeout_key: & str = "timeout" ;
  #[doc = "Key for validator file."]
  pub static new_vald_file_key: & str = "validator" ;
  #[doc = "Key for validators."]
  pub static new_vald_key: & str = "success" ;

  #[test]
  fn regexes() {
    println!("testing `empty_cmt_line`") ;
    assert!( empty_cmt_line.is_match("#  ") ) ;
    assert!( empty_cmt_line.is_match("#    ") ) ;
    assert!( ! empty_cmt_line.is_match("##  whatevs  ") ) ;

    println!("testing `name_re`") ;
    let tmp = format!("{} tool-Name 71", * dmp_pref) ;
    let res = name_re.captures(& tmp).unwrap() ;
    assert_eq!(& res["name"], "tool-Name 71") ;

    println!("testing `short_name_re`") ;
    let tmp = format!(
      "{} {}: tool-SHORT_Name_71", * dmp_pref, new_short_name_key
    ) ;
    let res = short_name_re.captures(& tmp).unwrap() ;
    assert_eq!(& res["short"], "tool-SHORT_Name_71") ;

    println!("testing `graph_name_re`") ;
    let tmp = format!(
      "{} {}: tool GRAPH Name_71", * dmp_pref, new_graph_name_key
    ) ;
    let res = graph_name_re.captures(& tmp).unwrap() ;
    assert_eq!(& res["graph"], "tool GRAPH Name_71") ;

    println!("testing `cmd_re`") ;
    let tmp = format!(
      r#"{} {}: "command args `eval` | piped 'blah'""#, * dmp_pref, new_cmd_key
    ) ;
    let res = cmd_re.captures(& tmp).unwrap() ;
    assert_eq!(& res["cmd"], "command args `eval` | piped 'blah'") ;

    println!("testing `timeout_re`") ;
    let tmp = format!("{} {}: 743044.123456789", * dmp_pref, new_timeout_key) ;
    let res = timeout_re.captures(& tmp).unwrap() ;
    assert_eq!( & res["secs"], "743044" ) ;
    assert_eq!( & res["nanos"], "123456789" )
  }
  
  lazy_static!{

    #[doc = "Key for short names."]
    pub static ref short_name_key: String = format!(
      "{} short: ", cmt_pref
    ) ;
    #[doc = "Key for graph names."]
    pub static ref graph_name_key: String = format!(
      "{} graph: ", cmt_pref
    ) ;
    #[doc = "Key for commands."]
    pub static ref cmd_key: String = format!(
      "{} cmd: ", cmt_pref
    ) ;
    #[doc = "Key for timeouts."]
    pub static ref timeout_key: String = format!(
      "{} timeout: ", cmt_pref
    ) ;
    #[doc = "Key for validators."]
    pub static ref vald_key: String = format!(
      "{} success: ", cmt_pref
    ) ;

    #[doc = "Matches a comment line that's not part of the dump info."]
    pub static ref empty_cmt_line: Regex = Regex::new(
      & format!(r"^{}[^{}]", cmt_pref, cmt_pref)
    ).expect(
      "problem in `empty_cmt_line` static regex"
    ) ;

    #[doc = "Matches the name of tool from a dump."]
    pub static ref name_re: Regex = Regex::new(
      & format!(
        r"^{}\s*(?P<name>[a-zA-Z][a-zA-Z0-9\s-_]*)\s*$", * dmp_pref
      )
    ).expect(
      "problem in `name_re` static regex"
    ) ;
    #[doc = "Matches the short name of a tool from a dump as `short`."]
    pub static ref short_name_re: Regex = Regex::new(
      & format!(
        r"^{}\s*{}\s*:\s*(?P<short>[a-zA-Z][a-zA-Z0-9-_]*)\s*$",
        * dmp_pref, new_short_name_key
      )
    ).expect(
      "problem in `short_name_re` static regex"
    ) ;
    #[doc = "Matches the graph name of a tool from a dump as `graph`."]
    pub static ref graph_name_re: Regex = Regex::new(
      & format!(
        r"^{}\s*{}\s*:\s*(?P<graph>[a-zA-Z][a-zA-Z0-9\s-_]*)\s*$",
        * dmp_pref, new_graph_name_key
      )
    ).expect(
      "problem in `graph_name_re` static regex"
    ) ;
    #[doc = "Matches the command of a tool from a dump as `cmd`."]
    pub static ref cmd_re: Regex = Regex::new(
      & format!(
        r#"^{}\s*{}\s*:\s*"(?P<cmd>.*)"\s*$"#, * dmp_pref, new_cmd_key
      )
    ).expect(
      "problem in `cmd_re` static regex"
    ) ;
    #[doc = "
Matches the timeout of a dump.

Two groups:

- `secs`: seconds,
- `nanos`: nanoseconds.
    "]
    pub static ref timeout_re: Regex = Regex::new(
      & format!(
        r"^{}\s*{}\s*:\s*{}$",
        * dmp_pref, new_timeout_key, & * ::consts::time_re
      )
    ).expect(
      "problem in `timeout` static regex"
    ) ;

    #[doc = "Matches a validator as `code`, `alias` and `desc`."]
    pub static ref vald_re: Regex = Regex::new(
      & format!(
        r"(?x)^
          {}\s*{}\s*:\s*
          (?P<code>[0-9][0-9]*|\-[0-9][0-9]*)\s*,\s*
          (?P<alias>[a-zA-Z][a-zA-Z0-9_\-.]*)\s*,\s*
          (?P<desc>.*)\s*
        $", * dmp_pref, new_vald_key
      )
    ).expect(
      "problem in `vald_re` static regex"
    ) ;
    #[doc = "Matches a validator file as `path`."]
    pub static ref vald_file_re: Regex = Regex::new(
      & format!(
        r"^{}\s*{}\s*:\s*(?P<path>_|[^\s]*)\s*$",
        * dmp_pref, new_vald_file_key
      )
    ).expect(
      "problem in `vald_file_re` static regex"
    ) ;

  }
}

/// Data-related regexs
pub mod data {
  use regex::Regex ;

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
