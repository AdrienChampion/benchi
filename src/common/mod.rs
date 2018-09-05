//! Common types and functions used by `benchi`.

use std::ops::Index;

pub use std::fs::File;
pub use std::io::{BufRead, BufReader, Lines, Read, Write};
pub use std::iter::{FromIterator, Iterator};
pub use std::path::{Path, PathBuf};
pub use std::process::Command;
pub use std::str::FromStr;
pub use std::sync::mpsc::{Receiver, Sender};
pub use std::sync::Arc;
pub use std::time::{Duration, Instant};

pub use std::collections::BTreeMap as Map;

pub use wait_timeout::ExitStatus;

pub use pbr::{MultiBar, ProgressBar};

use ansi::{Colour, Style};

pub use errors::*;

/// Unimplemented.
macro_rules! bail_unimpl {
    ($conf:expr, $stuff:tt) => {
        bail!(format!("{} is not implemented yet", $conf.emph($stuff)))
    };
}

/// Log macro.
#[macro_export]
macro_rules! log {

    ( | internal | $pref:expr => ) => (()) ;

    ( | internal | $pref:expr => ; $($tail:tt)* ) => (
        log!(| internal | $pref => $($tail)*)
    ) ;

    ( | internal | $pref:expr => let $p:pat = $e:expr ; $($tail:tt)* ) => ({
        let $p = $e ;
        log!(| internal | $pref => $($tail)*)
    }) ;

    ( | internal | $pref:expr => { $($head:tt)+ } $($tail:tt)* ) => ({
        { $($head)+ }
        log!(| internal | $pref => $($tail)*)
    }) ;

    ( | internal | $pref:expr => $($head:expr),* ; $($tail:tt)* ) => ({
        print!("{}", $pref) ;
        println!($($head),*) ;
        log!(| internal | $pref => $($tail)*)
    }) ;

    ( $conf:expr => $($stuff:tt)+ ) => ({
        if ! $conf.quiet() {
            log!( |internal| "" => $($stuff)+ ; )
        }
    }) ;

    ( $conf:expr , verb => $($stuff:tt)+ ) => ({
        if $conf.verbose() {
            log!( |internal| "" => $($stuff)+ ; )
        }
    }) ;
}

/// Warning macro.
#[macro_export]
macro_rules! warn {
    ($conf:expr => $($stuff:tt)+) => (
        if ! $conf.quiet() {
            println!("") ;
            println!("{}:", $conf.sad("|===| Warning")) ;
            warn!{ $conf, line => $($stuff)+ ; }
            println!("{}", $conf.sad("|===|")) ;
            println!("")
        }
    ) ;

    ($conf:expr, line => $($stuff:tt)+) => (
        log!{ |internal| $conf.sad("| ") => $($stuff)+ ; }
    ) ;
}

pub mod inspect;
pub mod plot;
pub mod res;
pub mod run;

mod instance;
mod tool;
mod validator;

use self::{plot::*, run::*};

pub use self::{instance::Instance, tool::*, validator::*};

/// Arc to a `RunConf`.
pub type ARunConf = Arc<RunConf>;

/// Creates a directory if not already there.
#[inline]
pub fn mk_dir<P: AsRef<Path>>(path: P) -> Res<()> {
    ::std::fs::DirBuilder::new()
        .recursive(true)
        .create(path)
        .map_err(|e| e.into())
}

/// Checks that a file exists.
#[inline]
pub fn file_exists<P: AsRef<Path>>(path: P) -> bool {
    let path = path.as_ref();
    path.exists() && path.is_file()
}

/// Can convert to a TOML representation.
pub trait ToToml: ::serde::Serialize {
    /// TOML string version.
    fn to_toml_str(&self) -> Res<String> {
        ::toml::to_string_pretty(self).map_err(|e| {
            let e: Error = format!("unable to write exit codes as TOML: {}", e).into();
            e
        })
    }
}

/// Validation code.
pub type Code = i32;
/// Map from validation codes to something.
pub type CodeMap<T> = Map<Code, T>;

/// Clap result.
pub enum Clap {
    /// Run mode.
    Run(RunConf, Box<ToolInfos>),
    /// Plot mode.
    Plot(PlotConf, PlotKind),
    /// Conf mode (explanation). Second parameter is the file to dump the
    /// example configuration to.
    Conf(GConf, String),
}

/// Can color things.
pub trait ColorExt {
    /// The styles in the colorizer: emph, happy, sad, and bad.
    fn styles(&self) -> &Styles;
    /// String emphasis.
    #[inline]
    fn emph<S: AsRef<str>>(&self, s: S) -> String {
        format!("{}", self.styles().emph.paint(s.as_ref()))
    }
    /// Happy string.
    #[inline]
    fn happy<S: AsRef<str>>(&self, s: S) -> String {
        format!("{}", self.styles().hap.paint(s.as_ref()))
    }
    /// Sad string.
    #[inline]
    fn sad<S: AsRef<str>>(&self, s: S) -> String {
        format!("{}", self.styles().sad.paint(s.as_ref()))
    }
    /// Bad string.
    #[inline]
    fn bad<S: AsRef<str>>(&self, s: S) -> String {
        format!("{}", self.styles().bad.paint(s.as_ref()))
    }
}

/// Contains some styles for coloring.
#[derive(Debug, Clone)]
pub struct Styles {
    /// Emphasis style.
    emph: Style,
    /// Happy style.
    hap: Style,
    /// Sad style.
    sad: Style,
    /// Bad style.
    bad: Style,
}
impl Default for Styles {
    fn default() -> Self {
        Styles::new(true)
    }
}
impl ColorExt for Styles {
    fn styles(&self) -> &Styles {
        self
    }
}
impl Styles {
    /// Creates some styles.
    pub fn new(colored: bool) -> Self {
        Styles {
            emph: if colored {
                Style::new().bold()
            } else {
                Style::new()
            },
            hap: if colored {
                Colour::Green.normal().bold()
            } else {
                Style::new()
            },
            sad: if colored {
                Colour::Yellow.normal().bold()
            } else {
                Style::new()
            },
            bad: if colored {
                Colour::Red.normal().bold()
            } else {
                Style::new()
            },
        }
    }
}

/// Has a verbosity setting.
pub trait VerbExt {
    /// Access to the verbosity.
    fn verb(&self) -> &Verb;
    /// True if quiet.
    #[inline]
    fn quiet(&self) -> bool {
        *self.verb() == Verb::Quiet
    }
    /// True if normal.
    #[inline]
    fn normal(&self) -> bool {
        *self.verb() == Verb::Normal
    }
    /// True if verbose.
    #[inline]
    fn verbose(&self) -> bool {
        *self.verb() == Verb::Verbose
    }
}

/// Verbosity.
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Verb {
    /// No output.
    Quiet,
    /// Normal output.
    Normal,
    /// Verbose output.
    Verbose,
}
impl Default for Verb {
    fn default() -> Self {
        Verb::Normal
    }
}
impl VerbExt for Verb {
    fn verb(&self) -> &Verb {
        self
    }
}

/// Global configuration.
#[derive(Debug, Default, Clone)]
pub struct GConf {
    /// Verbosity.
    verb: Verb,
    /// Colored flag (for comparison).
    colored: bool,
    /// Styles.
    styles: Styles,
    /// Overwrite files when present.
    pub ow_files: bool,
}
impl PartialEq for GConf {
    fn eq(&self, other: &Self) -> bool {
        self.verb == other.verb && self.colored == other.colored && self.ow_files == other.ow_files
    }
}
impl GConfExt for GConf {
    fn gconf(&self) -> &GConf {
        self
    }
}
impl GConf {
    /// Creates a configuration.
    #[inline]
    pub fn new(verb: Verb, colored: bool, ow_files: bool) -> Self {
        GConf {
            verb,
            colored,
            styles: Styles::new(colored),
            ow_files,
        }
    }
}

/// Has a global conf.
pub trait GConfExt: ColorExt {
    /// The global conf.
    fn gconf(&self) -> &GConf;
    /// Opens a file in write mode. Creates parent directory if necessary.
    #[inline]
    fn open_file_writer_exe<P: AsRef<Path>>(&self, path: P, executable: bool) -> Res<File> {
        use std::os::unix::fs::OpenOptionsExt;
        // Create parent directory if necessary.
        {
            let mut buf = path.as_ref().to_path_buf();
            if buf.pop() {
                mk_dir(&buf).chain_err(|| "while creating parent directory")?
            }
        }
        let conf = self.gconf();
        let mut options = ::std::fs::OpenOptions::new();
        options.write(true);
        if executable {
            options.mode(0o744);
        }
        if conf.ow_files {
            options.create(true).truncate(true);
        } else {
            options.create_new(true);
        }
        options.open(path.as_ref()).map_err(|e| match e.kind() {
            ::std::io::ErrorKind::AlreadyExists => ErrorKind::Msg(format!(
                "file exists, not overwriting without {}",
                self.emph("-f")
            )).into(),
            _ => e.into(),
        })
    }
    /// Opens a file in write mode. Creates parent directory if necessary.
    #[inline]
    fn open_file_writer<P: AsRef<Path>>(&self, path: P) -> Res<File> {
        self.open_file_writer_exe(path, false)
    }
}
impl<T: GConfExt> ColorExt for T {
    fn styles(&self) -> &Styles {
        &self.gconf().styles
    }
}
impl<T: GConfExt> VerbExt for T {
    fn verb(&self) -> &Verb {
        &self.gconf().verb
    }
}

wrap_usize! {
    /// Tool index.
    ToolIdx
    /// Range over tools.
    range: ToolRng
    /// Total map from tools to something.
    map: ToolMap with iter: ToolMapIter
    /// Map from tools to something.
    hash map: ToolHMap
}

wrap_usize! {
    /// Bench index.
    BenchIdx
    /// Range over benchs.
    range: BenchRng
    /// Total map from benchs to something.
    map: BenchMap with iter: BenchMapIter
    /// Map from benchs to something.
    hash map: BenchHMap
}

/// Extends `Duration`.
pub trait DurationExt: Sized {
    /// Time in seconds with nanosecond precision as string.
    fn as_sec_str(&self) -> String;
    /// Duration from a string.
    fn from_str(&str) -> Res<Self>;
    /// Zero duration.
    #[inline]
    fn zero() -> Duration {
        Duration::new(0, 0)
    }
}
impl DurationExt for Duration {
    fn as_sec_str(&self) -> String {
        format!("{}.{:0>9}", self.as_secs(), self.subsec_nanos())
    }
    fn from_str(s: &str) -> Res<Self> {
        use std::str::FromStr;

        macro_rules! failed {
            () => {
                bail!("illegal time string `{}`", s)
            };
            (opt $e:expr) => {
                if let Some(res) = $e {
                    res
                } else {
                    failed!()
                }
            };
            ($e:expr) => {
                if let Ok(res) = $e {
                    res
                } else {
                    failed!()
                }
            };
        }

        let mut split = s.split('.');

        let secs = failed!( opt split.next() );
        let nanos = failed!( opt split.next() );

        let secs = if secs.is_empty() {
            0u64
        } else {
            failed!(u64::from_str(secs))
        };
        let nanos = failed!(u32::from_str(&format!(
            "{}{}",
            &nanos,
            "0".repeat(nanos.len() - 9)
        )));

        Ok(Duration::new(secs, nanos))
    }
}

/// Extends string types with a substitution function.
pub trait StrExt {
    /// Replaces all non-ovelapping matches of a regex with something.
    fn subst(&self, regex: &::regex::Regex, something: &str) -> String;
    /// Performs path substitutions: `today` and `now`.
    fn path_subst(&self) -> String {
        use chrono::{Datelike, Local, Timelike};
        use consts::subst::{now_re, today_re};
        let today = Local::today();
        let today = &format!("{}_{:0>2}_{:0>2}", today.year(), today.month(), today.day());
        let now = Local::now();
        let now = &format!("{:0>2}_{:0>2}", now.hour(), now.minute());
        let res = self.subst(&*today_re, today);
        res.subst(&*now_re, now)
    }
}
impl StrExt for str {
    fn subst(&self, regex: &::regex::Regex, something: &str) -> String {
        format!("{}", regex.replace_all(self, something))
    }
}
impl StrExt for String {
    fn subst(&self, regex: &::regex::Regex, something: &str) -> String {
        (self as &str).subst(regex, something)
    }
}

/// Extension for `ExitStatus`.
pub trait ExitStatusExt {
    /// Data string version of the code. `?` if none.
    fn as_data_str(&self) -> String;
}
impl ExitStatusExt for ExitStatus {
    fn as_data_str(&self) -> String {
        if let Some(code) = self.code() {
            format!("{}", code)
        } else {
            "?".into()
        }
    }
}

/// Dumps the example configuration file somewhere.
pub fn example_conf_file(conf: &GConf, file: &str) -> Res<()> {
    use std::io::Write;
    log!{ conf => "Opening `{}`...", conf.emph(& file) }
    let mut writer = conf.open_file_writer(&file).chain_err(|| {
        format!(
            "while opening example configuration file `{}` (write)",
            conf.emph(&file)
        )
    })?;
    log!{ conf => "  writing example configuration..." }
    writer
        .write(::consts::ex_conf_file.as_bytes())
        .chain_err(|| {
            format!(
                "while writing example configuration to file `{}`",
                conf.emph(&file)
            )
        })?;
    log!{ conf => "Done" ; "" }

    let mut path = PathBuf::from(&file);
    let was_okay = path.set_extension("benchs");
    let bench_file = path
        .to_str()
        .and_then(|res| if was_okay { Some(res) } else { None })
        .ok_or_else::<Error, _>(|| {
            format!("`{}` is not a valid file name", conf.emph(&file)).into()
        })?;
    log!{ conf => "Opening `{}`...", conf.emph(bench_file) }
    let mut writer = conf.open_file_writer(bench_file).chain_err(|| {
        format!(
            "while opening benchmark list file `{}`",
            conf.emph(bench_file)
        )
    })?;
    log!{ conf => "  writing benchmark list file..." }
    writer
        .write(::consts::ex_bench_file.as_bytes())
        .chain_err(|| {
            format!(
                "while writing example configuration to file `{}`",
                conf.emph(bench_file)
            )
        })?;
    log!{
      conf =>
        "Done" ;
        "" ;
        "{} for using benchi.", conf.happy("Thank you") ;
        "Make sure to read `{}` to understand what's going on.",
        conf.emph("benchi help run") ;
        "" ;

        "Get started by running" ;
        "> {} {} {} {}",
        conf.happy("benchi"), conf.emph("run"), file, bench_file ;
        "This will run up to 4 threads in parallel. If that's too much try" ;
        "> {} {} --tools 1 {} {}",
        conf.happy("benchi"), conf.emph("run"), file, bench_file ;
        "" ;

        "The commands above dump data to `{}`", conf.emph(
          "example/<today>_at_<now>"
        ) ;
        "  where <today> is today's date `YYYY_MM_DD`" ;
        "  and <now> is the current time `HH_MM`." ;
        "" ;

        "Generate a nice cumulative plot of all the results with" ;
        "> {} {} example/graphs/cumul.plot \
        {} example/<today>_at_<now>/*.data",
        conf.happy("benchi"), conf.emph("plot"), conf.emph("cumul") ;
        "If you have gnuplot, benchi will generate `example/graphs/cumul.pdf` \
        automatically." ;
        "" ;

        "Or generate a comparative scatterplot between two of them with, e.g." ;
        "> {} {} example/graphs/cmp.plot \\",
        conf.happy("benchi"), conf.emph("plot") ;
        "         {} example/<today>_at_<now>/home_find.data",
        conf.emph("compare") ;
        "                 example/<today>_at_<now>/tmp_find.data" ;
        "If you have gnuplot, benchi will generate `example/graphs/cmp.pdf` \
        automatically." ;
        "" ;
        "Make sure to read `{}`.", conf.emph("benchi help plot")
    }
    Ok(())
}

/// Type returned by a `channel` function: a sender and a receiver.
pub type Channel<T> = (Sender<T>, Receiver<T>);
