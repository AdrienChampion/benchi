//! Constants.

/// Time regex as a string (for reuse).
static time_re: & str = "\
  ([0-9]*).([0-9]*)\
" ;

/// Substitutions in user-provided data.
pub mod subst {
  use regex::Regex ;

  /// Today keyword. **Update CLAP help if you change this.**
  pub static today: & str = "<today>" ;
  /// Now keyword. **Update CLAP help if you change this.**
  pub static now: & str = "<now>" ;
  /// Timeout keyword. **Update CLAP help if you change this.**
  pub static timeout: & str = "<timeout>" ;

  lazy_static!{
    #[doc = "Matches the `today` keyword."]
    pub static ref today_re: Regex = Regex::new(today).unwrap() ;
    #[doc = "Matches the `now` keyword."]
    pub static ref now_re: Regex = Regex::new(now).unwrap() ;
    #[doc = "Matches the `timeout` keyword."]
    pub static ref timeout_re: Regex = Regex::new(timeout).unwrap() ;
  }
}

/// Bench data dumping.
pub mod dump {
  use regex::Regex ;
  
  /// Comment prefix.
  pub static cmt_pref:  & str = "#" ;
  
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
      & format!("^{}\\s*(.*)$", & * cmt_pref)
    ).unwrap() ;
    #[doc = "Matches the short name of a tool from a dump."]
    pub static ref short_name_re: Regex = Regex::new(
      & format!("^{}\\s*([^\\s])$", & * short_name_key)
    ).unwrap() ;
    #[doc = "Matches the graph name of a tool from a dump."]
    pub static ref graph_name_re: Regex = Regex::new(
      & format!("^{}\\s*(.*)$", & * graph_name_key)
    ).unwrap() ;
    #[doc = "Matches the command of a tool from a dump."]
    pub static ref cmd_re: Regex = Regex::new(
      & format!("^{}\\s*(.*)$", & * cmd_key)
    ).unwrap() ;
    #[doc = "
Matches the timeout of a dump. Three groups: bench uid, bench name and result
data.
    "]
    pub static ref timeout_re: Regex = Regex::new(
      & format!("^{}\\s*{}$", & * timeout_key, & * ::consts::time_re)
    ).unwrap() ;

  }
}

/// Data-related regexs
pub mod data {
  use regex::Regex ;

  lazy_static!{

    #[doc = "
Regex extracting the runtime of a benchmark line. Three groups: bench uid,
bench name and result data.
    "]
    pub static ref runtime_re: Regex = Regex::new(
      r#"^\s*([0-9]*)\s\s*"([^"]*)"\s\s*([^\s]*)\s*.*$"#
    ).unwrap() ;
    #[doc =
      "Regex matching a successful result. Two groups: seconds and micros."
    ]
    pub static ref data_success_re: Regex = Regex::new(
      & format!("^{}$", ::consts::time_re)
    ).unwrap() ;
    #[doc =
      "
Regex matching a timeout result. Two groups: seconds and micros of timeout.
      "
    ]
    pub static ref data_timeout_re: Regex = Regex::new(
      r#"^\s*timeout\(([0-9]*)\.([0-9]*)\)\s*$"#
    ).unwrap() ;

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