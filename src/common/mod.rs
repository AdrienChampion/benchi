//! Common types and functions used by `benchi`.

use std::ops::Index;

pub use std::collections::HashMap;
pub use std::fs::File;
pub use std::io::{BufRead, BufReader, Lines, Read, Write};
pub use std::iter::{FromIterator, Iterator};
pub use std::path::{Path, PathBuf};
pub use std::process::Command;
pub use std::str::FromStr;
pub use std::sync::mpsc::{Receiver, Sender};
pub use std::sync::Arc;
pub use std::time::{Duration, Instant};

pub use std::collections::{BTreeMap as Map, BTreeSet as Set};

pub use wait_timeout::ExitStatus;

pub use pbr::{MultiBar, ProgressBar};

use ansi::{Colour, Style};

pub use errors::*;

pub use load::{NewBenchRes, NewCode, NewCodes, NewToolConf, NewToolConfs};

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

pub mod res;
pub mod run;
use common::run::*;
pub mod plot;
use common::plot::*;
pub mod inspect;
// use common::inspect::* ;

/// Grants access to validation codes.
pub trait CodesExt {
    /// Validation codes.
    fn codes(&self) -> &NewCodes;
}

/// Map from strings to something.
pub type StrMap<T> = Map<String, T>;

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
pub type Validation = i32;

/// Clap result.
pub enum Clap {
    /// Run mode.
    Run(RunConf, Box<NewToolConfs>),
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
        Styles::mk(true)
    }
}
impl ColorExt for Styles {
    fn styles(&self) -> &Styles {
        self
    }
}
impl Styles {
    /// Creates some styles.
    pub fn mk(colored: bool) -> Self {
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
    pub fn mk(verb: Verb, colored: bool, ow_files: bool) -> Self {
        GConf {
            verb,
            colored,
            styles: Styles::mk(colored),
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

/// Store the tools and the path to the benchmarks. Should **always** be
/// immutable.
pub struct Instance {
    /// The tools.
    tools: NewToolConfs,
    /// The benchmarks.
    benchs: BenchMap<String>,
}
unsafe impl Sync for Instance {}
impl Instance {
    /// Creates an instance.
    #[inline]
    pub fn new(tools: NewToolConfs, benchs: BenchMap<String>) -> Self {
        Instance { tools, benchs }
    }
    /// Iterator over the tool indices of the instance.
    #[inline]
    pub fn tools(&self) -> ToolRng {
        ToolRng::zero_to(self.tools.len())
    }
    /// Iterator over the bench indices of the instance.
    #[inline]
    pub fn benchs(&self) -> BenchRng {
        BenchRng::zero_to(self.benchs.len())
    }
    /// Number of tools.
    #[inline]
    pub fn tool_len(&self) -> usize {
        self.tools.len()
    }
    /// Number of benchs.
    #[inline]
    pub fn bench_len(&self) -> usize {
        self.benchs.len()
    }
    /// String of a bench.
    #[inline]
    pub fn str_of_bench(&self, index: BenchIdx) -> &str {
        &self.benchs[index]
    }

    /// Safe name for a bench, unique and can be used as file id.
    ///
    /// ```rust
    /// # use benchi::common::{ Instance, NewToolConfs, BenchMap } ;
    /// let benchs: BenchMap<_> = vec![ "unused".to_string() ; 150 ].into() ;
    /// let tools = NewToolConfs::new() ;
    /// let instance = Instance::new(tools, benchs) ;
    /// assert_eq! { & instance.safe_name_for_bench(23.into()), "023" }
    ///
    /// let benchs: BenchMap<_> = vec![ "unused".to_string() ; 1500 ].into() ;
    /// let tools = NewToolConfs::new() ;
    /// let instance = Instance::new(tools, benchs) ;
    /// assert_eq! { & instance.safe_name_for_bench(23.into()), "0023" }
    /// ```
    #[inline]
    pub fn safe_name_for_bench(&self, index: BenchIdx) -> String {
        format!("{:0>1$}", *index, format!("{}", self.benchs.len()).len())
    }

    /// Path to the directory of a tool.
    #[inline]
    pub fn path_of_tool(&self, conf: &Arc<RunConf>, tool: ToolIdx) -> PathBuf {
        let mut path = PathBuf::from(&conf.out_dir);
        path.push(&self[tool].ident());
        path
    }

    /// Stderr directory path of a tool.
    #[inline]
    pub fn err_path_of_tool(&self, conf: &Arc<RunConf>, tool: ToolIdx) -> PathBuf {
        let mut path = self.path_of_tool(conf, tool);
        path.push("err");
        path
    }
    /// Stdout directory path of a tool.
    #[inline]
    pub fn out_path_of_tool(&self, conf: &Arc<RunConf>, tool: ToolIdx) -> PathBuf {
        let mut path = self.path_of_tool(conf, tool);
        path.push("out");
        path
    }

    /// Path to the stderr of a tool on a bench.
    #[inline]
    pub fn err_path_of(&self, conf: &Arc<RunConf>, tool: ToolIdx, bench: BenchIdx) -> PathBuf {
        let mut path = self.err_path_of_tool(conf, tool);
        path.push(self.safe_name_for_bench(bench));
        path
    }

    /// Path to the stdout of a tool on a bench.
    #[inline]
    pub fn out_path_of(&self, conf: &Arc<RunConf>, tool: ToolIdx, bench: BenchIdx) -> PathBuf {
        let mut path = self.out_path_of_tool(conf, tool);
        path.push(self.safe_name_for_bench(bench));
        path
    }

    /// Creates the error path of a tool.
    #[inline]
    pub fn mk_err_dir(&self, conf: &Arc<RunConf>, tool: ToolIdx) -> Res<()> {
        mk_dir(self.err_path_of_tool(conf, tool)).chain_err(|| {
            format!(
                "while creating err directory for {}",
                conf.emph(&self[tool].ident())
            )
        })
    }
    /// Creates the output path of a tool.
    #[inline]
    pub fn mk_out_dir(&self, conf: &Arc<RunConf>, tool: ToolIdx) -> Res<()> {
        mk_dir(self.out_path_of_tool(conf, tool)).chain_err(|| {
            format!(
                "while creating output directory for {}",
                conf.emph(&self[tool].ident())
            )
        })
    }
    /// Initializes the data file and the validator for some tool.
    #[inline]
    pub fn init_data_file_and_validator(&self, conf: &Arc<RunConf>, tool: ToolIdx) -> Res<File> {
        self.init_validator(conf, tool)?;
        let mut path = PathBuf::new();
        path.push(&conf.out_dir);
        path.push(&self[tool].ident());
        path.set_extension("toml");
        let mut tool_file = conf.open_file_writer(path.as_path()).chain_err(|| {
            format!(
                "while creating file `{}`, data file for `{}`",
                conf.sad(path.to_str().expect("non-UTF8 path")),
                conf.emph(&self[tool].ident())
            )
        })?;

        writeln!(tool_file, "timeout = \"{}\"", conf.timeout.as_sec_str())?;
        writeln!(tool_file)?;

        writeln!(tool_file, "[tool]")?;
        writeln!(tool_file, "{}", self[tool].to_toml_str()?)?;

        writeln!(tool_file, "[codes]")?;
        conf.codes().toml_write(&mut tool_file)?;

        writeln!(tool_file, "[data]")?;

        Ok(tool_file)
    }

    /// Initializes the validator for some tool.
    #[inline]
    pub fn init_validator(&self, conf: &Arc<RunConf>, tool: ToolIdx) -> Res<()> {
        use std::os::unix::fs::PermissionsExt;
        if let Some(path) = conf.validator_path_of(&self[tool]) {
            let mut file = conf
                .open_file_writer_exe(path.as_path(), true)
                .chain_err(|| {
                    format!(
                        "while creating validator for `{}`",
                        conf.sad(&self[tool].ident())
                    )
                })?;
            file.write(::consts::validator::pref.as_bytes())
                .chain_err(|| {
                    format!(
                        "while while writing to validator file for `{}`",
                        conf.sad(&self[tool].ident())
                    )
                })?;

            conf.codes().bash_write(&mut file)?;

            if let Some(s) = self[tool].validator() {
                file.write(s.as_bytes()).chain_err(|| {
                    format!(
                        "while while writing to validator file for `{}`",
                        conf.sad(&self[tool].ident())
                    )
                })?;
                file.metadata()
                    .chain_err(|| "could chmod validator file to executable")?
                    .permissions()
                    .set_mode(0o744)
            }
        }
        Ok(())
    }

    /// Initializes everything for the tools. Returns the data files for all the
    /// tools. Runs the input function on each `ToolConf` in a fold manner.
    ///
    /// - tool dir, err dir, out dir
    /// - data file
    /// - validators if any
    pub fn init_tools<T, F>(
        &self,
        conf: &Arc<RunConf>,
        init: T,
        fold_fun: F,
    ) -> Res<(ToolMap<File>, T)>
    where
        F: Fn(&mut T, &NewToolConf),
    {
        let mut tool_files = ToolMap::with_capacity(self.tool_len());
        let mut fold_data = init;
        // Tool init: output dirs, validators, data file, mastre data init.
        for tool in self.tools() {
            // Output dirs.
            self.mk_err_dir(&conf, tool)?;
            if conf.log_stdout {
                self.mk_out_dir(&conf, tool)?
            };

            // Data file.
            let tool_file = self.init_data_file_and_validator(&conf, tool)?;
            tool_files.push(tool_file);

            // Folding.
            fold_fun(&mut fold_data, &self[tool])
        }

        Ok((tool_files, fold_data))
    }

    /// Checks if a bench index is the last one.
    #[inline]
    pub fn is_last_bench(&self, bench: BenchIdx) -> bool {
        *bench + 1 >= self.benchs.len()
    }

    /// If any, runs the validator for a tool on some benchmark.
    pub fn validate(
        &self,
        conf: &Arc<RunConf>,
        tool: ToolIdx,
        bench: BenchIdx,
        status: ExitStatus,
    ) -> Res<Option<ExitStatus>> {
        use wait_timeout::ChildExt;

        if let Some(path) = conf.validator_path_of(&self[tool]) {
            use std::process::Stdio;
            let status = if let Some(code) = status.code() {
                code
            } else {
                bail!("could not retrieve exit status")
            };
            let out_path = self.out_path_of(conf, tool, bench);
            let err_path = self.err_path_of(conf, tool, bench);
            let mut kid = Command::new(path.as_os_str())
                .stdin(Stdio::null())
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .arg(&self[bench])
                .arg(&format!("{}", status))
                .arg(out_path)
                .arg(err_path)
                .spawn()
                .chain_err(|| {
                    format!(
                        "while running validator for `{}` on benchmark `{}`",
                        conf.sad(&self[tool].ident()),
                        conf.sad(format!("{}", bench))
                    )
                })?;
            let res = kid.wait_timeout(conf.timeout)?;
            if res.is_none() {
                kid.kill()?;
            }

            Ok(res)
        } else {
            Ok(None)
        }
    }
}

impl Index<BenchIdx> for Instance {
    type Output = String;
    #[inline]
    fn index(&self, index: BenchIdx) -> &String {
        &self.benchs[index]
    }
}

impl Index<ToolIdx> for Instance {
    type Output = NewToolConf;
    #[inline]
    fn index(&self, index: ToolIdx) -> &NewToolConf {
        &self.tools[index]
    }
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
