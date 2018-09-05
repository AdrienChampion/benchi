//! Constants.

/// Validator constants.
pub mod validator {
    use std::collections::BTreeSet;

    lazy_static! {
      /// Reserved names in validators.
      pub static ref reserved: BTreeSet<& 'static str> = {
        let mut set = BTreeSet::new() ;
        set.insert("bench") ;
        set.insert("code") ;
        set.insert("out_file") ;
        set.insert("err_file") ;
        set
      } ;
    }

    /// Prefix added to validator scripts.
    pub static pref: &str = "\
#!/bin/bash
bench=\"$1\"
code=\"$2\"
out_file=\"$3\"
err_file=\"$4\"

\
  ";
}

/// Clap-related constants.
pub mod clap {
    use regex::Regex;

    /// Format for the timeout.
    pub static tmo_format: &str = "[int]s|[int]min";
    /// Format for booleans.
    pub static bool_format: &str = "on|true|off|false";

    #[test]
    fn regexes() {
        let tmo_res = tmo_regex.captures("720s").unwrap();
        assert_eq!(&tmo_res["value"], "720");
        assert_eq!(&tmo_res["unit"], "s");

        let tmo_res = tmo_regex.captures("3min").unwrap();
        assert_eq!(&tmo_res["value"], "3");
        assert_eq!(&tmo_res["unit"], "min");

        assert!(!tmo_regex.is_match("720"));
        assert!(!tmo_regex.is_match("720 s"));
        assert!(!tmo_regex.is_match("s720"));
        assert!(!tmo_regex.is_match("s 720"));
        assert!(!tmo_regex.is_match("s"));
        assert!(!tmo_regex.is_match("720 min"));
        assert!(!tmo_regex.is_match("min720"));
        assert!(!tmo_regex.is_match("min 720"));
        assert!(!tmo_regex.is_match("min"));
        assert!(!tmo_regex.is_match(""));
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
    use regex::Regex;

    /// Today keyword. **Update CLAP help if you change this.**
    pub static today: &str = "<today>";
    /// Now keyword. **Update CLAP help if you change this.**
    pub static now: &str = "<now>";
    /// Timeout keyword. **Update CLAP help if you change this.**
    pub static timeout: &str = "<timeout>";

    #[test]
    fn regexes() {
        let today_res = today_re.replace_all("blah<today> <today> <today>foo", "date");
        assert_eq!(today_res, "blahdate date datefoo");
        let now_res = now_re.replace_all("blah<now> <now> <now>foo", "time");
        assert_eq!(now_res, "blahtime time timefoo");
        let timeout_res = timeout_re.replace_all("blah<timeout> <timeout> <timeout>foo", "tmo");
        assert_eq!(timeout_res, "blahtmo tmo tmofoo")
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
    pub static cmt_pref: &str = "#";
    #[doc = "Key for short names."]
    pub static short_name_key: &str = "short";
    #[doc = "Key for graph names."]
    pub static graph_name_key: &str = "graph";
    #[doc = "Key for commands."]
    pub static cmd_key: &str = "cmd";
    #[doc = "Key for timeouts."]
    pub static timeout_key: &str = "timeout";
    #[doc = "Key for the validator."]
    pub static vald_key: &str = "validator";
    #[doc = "Key for validator conf."]
    pub static vald_conf_key: &str = "validators";
    #[doc = "Key for validator conf success codes."]
    pub static vald_conf_suc_key: &str = "success";
}

/// Data-related regexs
pub mod data {
    #[doc = "Indicates a timeout result."]
    pub static timeout_res: &str = "timeout";
    #[doc = "Indicates an error result."]
    pub static error_res: &str = "error";
}

/// Example configuration file.
pub static ex_conf_file: &str = r#"# Example configuration file.

# Run `benchi help conf` for further details.

# Setting `run` options. Each of these options can be overriden with command-
# line arguments.
options: "-v run -o example/<today>_at_<now> --tools 2 --benchs 2 -t 5s"

# So `benchi -q run -t 3min <this file>` overrides the verbosity setting and
# the timeout from the options above.


# This section defines validator codes that each tool definition can use. In
# practice, these are exit codes for validation scripts (see below).
validators {
  # `success` is the only kind of validator right now.
  # It is followed by the exit code itself (`0`), an identifier to use in the
  # validation scripts (`found_some`), and a nice name to use in the plots
  # (`Found Some`).
  success: 0, found_some, Found Some
  success: 10, found_none, Found None
  # Everything else is considered an error.
}
# NB: this example makes little sense, running it will most likely trigger
# warnings because some `tools` will disagree on the validation codes.
#
# This is expected: the point of validation codes is to make sure the tools
# agree on the result. But in this example, we're running `find` in different
# directories which, again, makes little sense.


find at root {
  short: root_find
  graph: Find at Root
  cmd: "find / -iname"

  # In addition to the validation codes defined above, validators can refer to
  # four things:
  #
  # - `$bench` the argument fed to this tool (benchmark being validated)
  # - `$code` the code the tool returned
  # - `$out` a file containing the tool's `stdout`
  # - `$err` a file containing the tool's `stderr`
  #
  # A tool's validator becomes a bash script located at
  # `<out/dir>/<tool_short_name>/validator.sh`
  validator: ```
code="$found_none"
while read a_file ; do
  if [ -f $a_file ] \
  || [ -d $a_file ] ; then
    code="$found_some"
  else
    exit 2
  fi
done < $out
exit $code
  ```
}

Find in Current Directory {
  short: curr_dir_find
  # Graph name is optional, none provided here so benchi will use the 'name'
  # of the tool instead: `Find in Current Directory` here.
  cmd: "find . -iname"

  # Validators cannot be factored out for multiple tools to use yet, create an
  # issue if you're interested in this feature!
  validator: ```
code="$found_none"
while read a_file ; do
  if [ -f $a_file ] \
  || [ -d $a_file ] ; then
    code="$found_some"
  else
    exit 2
  fi
done < $out
exit $code
  ```
}

Find in Tmp {
  short: tmp_find
  cmd: "find /tmp -iname"
  validator: ```
code="$found_none"
while read a_file ; do
  if [ -f $a_file ] \
  || [ -d $a_file ] ; then
    code="$found_some"
  else
    exit 2
  fi
done < $out
exit $code
  ```
}

Find in Home {
  short: home_find
  cmd: "find /home -iname"
  validator: ```
code="$found_none"
while read a_file ; do
  if [ -f $a_file ] \
  || [ -d $a_file ] ; then
    code="$found_some"
  else
    exit 2
  fi
done < $out
exit $code
  ```
}
"#;

/// Example benchmark file.
pub static ex_bench_file: &str = r#"[aA]*
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
[zZ]*"#;
