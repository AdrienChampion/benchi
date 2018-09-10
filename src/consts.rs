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
        /// Regex for timeout in clap. Two groups: `value` (int) and `unit` (`min` or `s`).
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

/// Example configuration file.
pub static ex_conf_file: &str = r#"# Example configuration file.

# Run `benchi help conf` for further details.

# Setting `run` options. Each of these options can be overriden with command-
# line arguments.
options = "-v run -o example/<today>_at_<now> --tools 2 --benchs 2 -t 7s"

# So `benchi -q run -t 3min <this file>` overrides the verbosity setting and
# the timeout from the options above.

# The following specifies which tools are active. The tools are defined below, but only the ones
# mentioned here will run.
run = [ "root_find", "wd_find", "parent_find" ]

# This next section declares tools: their name(s) and how to run them.
[tools]

    # This subsection defines a tool. The name of the section (`root_find` here) will be used for
    # file names related to this tool.
    [tools.root_find]
    # `cmd` is the command used to run the tool.
    cmd = "find / -iname"
    # `graph` is optional and specifies the name of this tool when plotting graphs.
    graph = "find from root"
    # The `validator` runs after the tool and checks its output/return code and other things. See
    # below for the definition of the `check_output` validator.
    validator = "check_output"

    # Another tool, notice that this one does not have a `graph` name. Meaning graphs will use its
    # short name: `wd_find`. (It's not going to be pretty when plotting graphs.)
    [tools.wd_find]
    cmd = "find . -iname"
    validator = "check_output"

    [tools.parent_find]
    cmd = "find .. -iname"
    graph = "Parent find"
    validator = "check_output"

    # This next tool is not active, it was not mentioned in the `run` field above.
    [tools.tmp_find]
    cmd = "find /tmp -iname"
    validator = "check_output"

# This next section defines validator codes that validation scripts can use (see below).
[codes]
found_some = { code = 0, graph = "Found None" }
found_none = { code = 10, graph = "None" }
timeout = { code = 20 }
# All codes are considered *success* results, except for the special (and optional) *timeout* code.
# It allows the validators (again, below) to force benchi to consider a result a timeout.
# Any other code returned by validation scripts is considered an error.

# This section defines validator (bash) scripts that run once a tool has finished working on a
# benchmark and did not result in an error or a timeout.
#
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
[validators]

check_output = '''
code="$found_none"
while read a_file ; do
  if [ -f $a_file ] \
  || [ -d $a_file ] ; then
    code="$found_some"
    break
  else
    exit 2
  fi
done < $out
exit $code
'''

# NB: this example makes little sense, running it will most likely trigger
# warnings because some `tools` will disagree on the validation codes.
#
# This is expected: the point of validation codes is to make sure the tools
# agree on the result. But in this example, we're running `find` in different
# directories which, again, makes little sense.

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
