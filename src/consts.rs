/*! Constants.
*/

/// Time regex as a string (for reuse).
static time_re: & str = "\
  (?P<secs>[0-9][0-9]*).(?P<nanos>[0-9][0-9]*)\
" ;

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
  fn subst_regexes() {
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
  pub static cmt_pref:  & str = "#" ;

  #[test]
  fn subst_regexes() {
    let mut res = name_re.captures_iter("# tool-Name 71") ;
    let tmp = res.next() ;
    assert_eq!(
      tmp.as_ref().map(|caps| & caps["name"]), Some("tool-Name 71")
    ) ;
    let mut res = short_name_re.captures_iter("# short: tool-SHORT_Name_71") ;
    let tmp = res.next() ;
    assert_eq!(
      tmp.as_ref().map(|caps| & caps["short"]), Some("tool-SHORT_Name_71")
    ) ;
    let mut res = graph_name_re.captures_iter("# graph: tool GRAPH Name_71") ;
    let tmp = res.next() ;
    assert_eq!(
      tmp.as_ref().map(|caps| & caps["graph"]), Some("tool GRAPH Name_71")
    ) ;
    let mut res = cmd_re.captures_iter(
      r"# cmd: command args `eval` | piped 'blah'"
    ) ;
    let tmp = res.next() ;
    assert_eq!(
      tmp.as_ref().map(|caps| & caps["cmd"]),
      Some("command args `eval` | piped 'blah'")
    ) ;
    let mut res = timeout_re.captures_iter(
      r"# timeout: 743044.123456789"
    ) ;
    let tmp = res.next() ;
    assert_eq!( tmp.as_ref().map(|caps| & caps["secs"]), Some("743044") ) ;
    assert_eq!( tmp.as_ref().map(|caps| & caps["nanos"]), Some("123456789") )
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

    #[doc = "Matches the name of tool from a dump."]
    pub static ref name_re: Regex = Regex::new(
      & format!(
        r"^{}\s*(?P<name>[a-zA-Z][a-zA-Z0-9\s-_]*)$", & * cmt_pref
      )
    ).expect(
      "problem in `name` static regex"
    ) ;
    #[doc = "Matches the short name of a tool from a dump as `short`."]
    pub static ref short_name_re: Regex = Regex::new(
      & format!(
        r"^{}\s*(?P<short>[a-zA-Z][a-zA-Z0-9-_]*)$", & * short_name_key
      )
    ).expect(
      "problem in `short_name` static regex"
    ) ;
    #[doc = "Matches the graph name of a tool from a dump as `graph`."]
    pub static ref graph_name_re: Regex = Regex::new(
      & format!(
        r"^{}\s*(?P<graph>[a-zA-Z][a-zA-Z0-9\s-_]*)$", & * graph_name_key
      )
    ).expect(
      "problem in `graph_name` static regex"
    ) ;
    #[doc = "Matches the command of a tool from a dump as `cmd`."]
    pub static ref cmd_re: Regex = Regex::new(
      & format!(r"^{}\s*(?P<cmd>.*)$", & * cmd_key)
    ).expect(
      "problem in `cmd` static regex"
    ) ;
    #[doc = "
Matches the timeout of a dump.

Two groups:

- `secs`: seconds,
- `nanos`: nanoseconds.
    "]
    pub static ref timeout_re: Regex = Regex::new(
      & format!(r"^{}\s*{}$", & * timeout_key, & * ::consts::time_re)
    ).expect(
      "problem in `timeout` static regex"
    ) ;

  }
}

/// Data-related regexs
pub mod data {
  use regex::Regex ;

  #[test]
  fn subst_regexes() {
    let mut res = result_re.captures_iter(
      r#"42 "benchmark line from file" success 72"#
    ) ;
    let tmp = res.next() ;
    assert_eq!(
      tmp.as_ref().map(|caps| & caps["uid"]), Some("42")
    ) ;
    assert_eq!(
      tmp.as_ref().map(|caps| & caps["bench"]),
      Some("benchmark line from file")
    ) ;
    assert_eq!(
      tmp.as_ref().map(|caps| & caps["res"]), Some("success")
    ) ;
    assert_eq!(
      tmp.as_ref().map(|caps| & caps["vald"]), Some("72")
    ) ;
    let mut res = success_re.captures_iter(
      r"743044.123456789"
    ) ;
    let tmp = res.next() ;
    assert_eq!( tmp.as_ref().map(|caps| & caps["secs"]), Some("743044") ) ;
    assert_eq!( tmp.as_ref().map(|caps| & caps["nanos"]), Some("123456789") ) ;
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
        \s*(?P<uid>[0-9][0-9]*)
        \s*"(?P<bench>[^"]*)"
        \s*(?P<res>[^\s]*)
        \s*(?P<vald>[0-9]*)
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
pub static ex_conf_file: & str = r#"// Example configuration file.

// Run `benchi help conf` for further details.

// Setting `run` options. Each of these options can be overriden with command-
// line arguments.
options: "-v run -o example/<today>_at_<now> --tools 2 --benchs 3 -t 5s"

// So `benchi -q run -t 3min <this file>` overrides the verbosity setting and
// the timeout from the options above.

find at root {
  short: root_find
  graph: Find at Root
  cmd: "find / -iname"
}

Find in Current Directory {
  short: curr_dir_find
  // Graph name is optional, none provided here so benchi will use the 'name'
  // of the tool instead: `Find in Current Directory` here.
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
