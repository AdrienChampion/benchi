//! Common types and functions used by `benchi`.

use std::fmt ;
use std::ops::{ Index, IndexMut, Deref } ;

pub use std::process::{ Command, ExitStatus } ;
pub use std::fs::File ;
pub use std::io::{ Lines, Write, BufRead, BufReader } ;
pub use std::time::{ Instant, Duration } ;
pub use std::path::{ Path, PathBuf } ;
pub use std::iter::Iterator ;
pub use std::sync::Arc ;
pub use std::collections::HashMap ;

pub use pbr::{ ProgressBar, MultiBar } ;

use ansi::{ Style, Colour } ;

use errors::* ;

/// Unimplemented.
macro_rules! bail_unimpl {
  ($conf:expr, $stuff:tt) => (
    bail!(
      format!("{} is not implemented yet", $conf.emph($stuff))
    )
  ) ;
}

/// Log macro.
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

  (
    $conf:expr => $($stuff:tt)+
  ) => ({
    if ! $conf.quiet() {
      log!( |internal| "" => $($stuff)+ ; )
    }
  }) ;

  (
    $conf:expr , verb => $($stuff:tt)+
  ) => ({
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
      log!{
        |internal| $conf.sad("| ") => $($stuff)+ ;
      }
      println!("{}", $conf.sad("|===|")) ;
      println!("")
    }
  )
}

pub mod run ;
use common::run::* ;
pub mod plot ;
use common::plot::* ;
pub mod inspect ;
// use common::inspect::* ;

/// Creates a directory if not already there.
#[inline]
pub fn mk_dir<P: AsRef<Path>>(path: P) -> Res<()> {
  ::std::fs::DirBuilder::new().recursive(true).create(path).map_err(
    |e| e.into()
  )
}

/// Checks that a file exists.
#[inline]
pub fn file_exists<P: AsRef<Path>>(path: P) -> bool {
  let path = path.as_ref() ;
  path.exists() && path.is_file()
}

/// Clap result.
pub enum Clap {
  /// Run mode.
  Run(RunConf, Vec<ToolConf>),
  /// Plot mode.
  Plot(PlotConf, PlotKind),
  /// Conf mode (explanation). Second parameter is the file to dump the
  /// example configuration to.
  Conf(GConf, String),
}


/// Can color things.
pub trait ColorExt {
  /// The styles in the colorizer: emph, happy, sad, and bad.
  #[inline]
  fn styles(& self) -> & Styles ;
  /// String emphasis.
  #[inline]
  fn emph<S: AsRef<str>>(& self, s: S) -> String {
    format!("{}", self.styles().emph.paint(s.as_ref()))
  }
  /// Happy string.
  #[inline]
  fn happy<S: AsRef<str>>(& self, s: S) -> String {
    format!("{}", self.styles().hap.paint(s.as_ref()))
  }
  /// Sad string.
  #[inline]
  fn sad<S: AsRef<str>>(& self, s: S) -> String {
    format!("{}", self.styles().sad.paint(s.as_ref()))
  }
  /// Bad string.
  #[inline]
  fn bad<S: AsRef<str>>(& self, s: S) -> String {
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
  fn default() -> Self { Styles::mk(true) }
}
impl ColorExt for Styles {
  fn styles(& self) -> & Styles { self }
}
impl Styles {
  /// Creates some styles.
  pub fn mk(colored: bool) -> Self {
    Styles {
      emph: if colored {
        Style::new().bold()
      } else { Style::new() },
      hap: if colored {
        Colour::Green.normal().bold()
      } else { Style::new() },
      sad: if colored {
        Colour::Yellow.normal().bold()
      } else { Style::new() },
      bad: if colored {
        Colour::Red.normal().bold()
      } else { Style::new() },
    }
  }
}


/// Has a verbosity setting.
pub trait VerbExt {
  /// Access to the verbosity.
  #[inline]
  fn verb(& self) -> & Verb ;
  /// True if quiet.
  #[inline]
  fn quiet(& self) -> bool { * self.verb() == Verb::Quiet }
  /// True if normal.
  #[inline]
  fn normal(& self) -> bool { * self.verb() == Verb::Normal }
  /// True if verbose.
  #[inline]
  fn verbose(& self) -> bool { * self.verb() == Verb::Verbose }
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
  fn default() -> Self { Verb::Normal }
}
impl VerbExt for Verb {
  fn verb(& self) -> & Verb { self }
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
  fn eq(& self, other: & Self) -> bool {
    self.verb == other.verb &&
    self.colored == other.colored &&
    self.ow_files == other.ow_files
  }
}
impl GConfExt for GConf {
  fn gconf(& self) -> & GConf { self }
}
impl GConf {
  /// Creates a configuration.
  #[inline]
  pub fn mk(verb: Verb, colored: bool, ow_files: bool) -> Self {
    GConf { verb, colored, styles: Styles::mk(colored), ow_files }
  }
}


/// Has a global conf.
pub trait GConfExt: ColorExt {
  /// The global conf.
  #[inline]
  fn gconf(& self) -> & GConf ;
  /// Opens a file in write mode. Creates parent directory if necessary.
  #[inline]
  fn open_file_writer_exe<P: AsRef<Path>>(
    & self, path: P, executable: bool
  ) -> Res<File> {
    use std::os::unix::fs::OpenOptionsExt ;
    // Create parent directory if necessary.
    {
      let mut buf = path.as_ref().to_path_buf() ;
      if buf.pop() {
        mk_dir(& buf).chain_err(
          || "while creating parent directory"
        ) ?
      }
    }
    let conf = self.gconf() ;
    let mut options = ::std::fs::OpenOptions::new() ;
    options.write(true) ;
    if executable {
      options.mode(0o744) ;
    }
    if conf.ow_files {
      options.create(true).truncate(true) ;
    } else {
      options.create_new(true) ;
    }
    options.open( path.as_ref() ).map_err(
      |e| match e.kind() {
        ::std::io::ErrorKind::AlreadyExists => {
          ErrorKind::Msg(
            format!(
              "file exists, not overwriting without {}", self.emph("-f")
            )
          ).into()
        },
        _ => e.into(),
      }
    )
  }
  /// Opens a file in write mode. Creates parent directory if necessary.
  #[inline]
  fn open_file_writer<P: AsRef<Path>>(& self, path: P) -> Res<File> {
    self.open_file_writer_exe(path, false)
  }
}
impl<T: GConfExt> ColorExt for T {
  fn styles(& self) -> & Styles { & self.gconf().styles }
}
impl<T: GConfExt> VerbExt for T {
  fn verb(& self) -> & Verb { & self.gconf().verb }
}









/// A tool configuration.
#[derive(Clone, Debug)]
pub struct ToolConf {
  /// Tool name.
  pub name: String,
  /// Short name.
  pub short: String,
  /// Graph name.
  pub graph: String,
  /// Command (lines).
  pub cmd: Vec<String>,
  /// Optional validator.
  pub validator: Option<String>,
}
unsafe impl Sync for ToolConf {}
impl ToolConf {
  /// Dumps info to a writer.
  pub fn dump_info<W: Write>(
    & self, timeout: & Duration, w: & mut W
  ) -> ::std::io::Result<()> {
    use consts::dump::* ;

    writeln!(w, "{} {}", & * cmt_pref, self.name) ? ;
    writeln!(w, "{}{}", * short_name_key, self.short) ? ;
    writeln!(w, "{}{}", * graph_name_key, self.graph) ? ;
    write!(  w, "{}", * cmd_key) ? ;
    for s in & self.cmd {
      write!(w, "{} ", s) ?
    }
    writeln!(w, "") ? ;
    writeln!(w, "{}{}", * timeout_key, timeout.as_sec_str()) ? ;
    writeln!(w, "{}", & * cmt_pref)
  }
  /// Loads a tool configuration from a dump.
  pub fn from_dump<Br: BufRead>(lines: & mut Lines<Br>) -> Res<Self> {
    use consts::dump::* ;

    let name = if let Some(line) = lines.next() {
      let line = line ? ;
      let len = cmt_pref.len() ;
      if line.len() < len + 1 || & line[0..len] != & * cmt_pref {
        bail!(
          format!(
            "illegal dump file, first line should be `{} <tool name>`",
            & * cmt_pref
          )
        )
      } else {
        line[len..].trim().to_string()
      }
    } else {
      bail!( format!("expected tool name, found nothing") )
    } ;
    let short = if let Some(line) = lines.next() {
      let line = line ? ;
      let len = short_name_key.len() ;
      if line.len() < len + 1 || & line[0..len] != * short_name_key {
        bail!(
          format!(
            "illegal dump file, second line should be \
            `{}<tool short name>`", * short_name_key
          )
        )
      } else {
        line[len..].trim().to_string()
      }
    } else {
      bail!( format!("expected tool ({}) short name, found nothing", name) )
    } ;
    let graph = if let Some(line) = lines.next() {
      let line = line ? ;
      let len = graph_name_key.len() ;
      if line.len() < len + 1 || & line[0..len] != * graph_name_key {
        bail!(
          format!(
            "illegal dump file, third line should be \
            `{}<tool graph name>`", * graph_name_key
          )
        )
      } else {
        line[len..].trim().to_string()
      }
    } else {
      bail!( format!("expected tool ({}) graph name, found nothing", name) )
    } ;
    let cmd = if let Some(line) = lines.next() {
      let line = line ? ;
      let len = cmd_key.len() ;
      if line.len() < len + 1 || & line[0..len] != * cmd_key {
        bail!(
          format!(
            "illegal dump file, fourth line should be \
            `{}<tool cmd>`", * cmd_key
          )
        )
      } else {
        lazy_static!{
          static ref cmd_regex: ::regex::Regex = ::regex::Regex::new(
            r"([^\s]*)"
          ).unwrap() ;
        }

        let mut cmd = vec![] ;
        let mut iter = cmd_regex.find_iter( line[len..].trim() ) ;
        if let Some(first) = iter.next() {
          cmd.push( first.as_str().to_string() ) ;
          for next in iter {
            cmd.push( next.as_str().to_string() )
          }
          cmd
        } else {
          bail!(
            format!(
              "command for tool `{}` is illegal", name
            )
          )
        }
      }
    } else {
      bail!( format!("expected tool ({}) cmd, found nothing", name) )
    } ;
    
    Ok( ToolConf { name, short, graph, cmd, validator: None } )
  }
}









/// The index of a tool, just a usize.
///
/// Can **only** be created by a `ToolRange`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ToolIndex {
  /// The index.
  n: usize,
}
impl Deref for ToolIndex {
  type Target = usize ;
  fn deref(& self) -> & usize { & self.n }
}

/// A range of tool indices.
///
/// Can only be created by an `Instance`.
pub struct ToolRange {
  /// Cursor.
  current: usize,
  /// Exclusive upper-bound.
  max: usize
}
impl Iterator for ToolRange {
  type Item = ToolIndex ;
  #[inline]
  fn next(& mut self) -> Option<ToolIndex> {
    if self.current >= self.max { None } else {
      let res = ToolIndex { n: self.current } ;
      self.current += 1 ;
      Some(res)
    }
  }
}


/// The index of a bench, just a usize.
#[derive(
  Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash
)]
pub struct BenchIndex {
  /// The index.
  n: usize,
}
impl BenchIndex {
  /// Iterator over indices. **Exclusive** upper bound.
  pub fn iter(max: usize) -> BenchRange {
    BenchRange { current: 0, max }
  }
}
impl Deref for BenchIndex {
  type Target = usize ;
  fn deref(& self) -> & usize { & self.n }
}
impl From<usize> for BenchIndex {
  fn from(n: usize) -> BenchIndex {
    BenchIndex { n }
  }
}
impl fmt::Display for BenchIndex {
  fn fmt(& self, fmt: & mut fmt::Formatter) -> fmt::Result {
    write!(fmt, "{}", self.n)
  }
}

/// A range of bench indices.
///
/// Can only be created by an `Instance`.
pub struct BenchRange {
  /// Cursor.
  current: usize,
  /// Exclusive upper-bound.
  max: usize
}
impl Iterator for BenchRange {
  type Item = BenchIndex ;
  #[inline]
  fn next(& mut self) -> Option<BenchIndex> {
    if self.current >= self.max { None } else {
      let res = BenchIndex { n: self.current } ;
      self.current += 1 ;
      Some(res)
    }
  }
}


/// Store the tools and the path to the benchmarks. Should **always** be
/// immutable.
pub struct Instance {
  /// The tools.
  tools: Vec<ToolConf>,
  /// The benchmarks.
  benchs: Vec<String>,
}
unsafe impl Sync for Instance {}
impl Instance {
  /// Creates an instance.
  #[inline]
  pub fn mk(tools: Vec<ToolConf>, benchs: Vec<String>) -> Self {
    Instance { tools, benchs }
  }
  /// Iterator over the tool indices of the instance.
  #[inline]
  pub fn tools(& self) -> ToolRange {
    ToolRange { current: 0, max: self.tools.len() }
  }
  /// Iterator over the bench indices of the instance.
  #[inline]
  pub fn benchs(& self) -> BenchRange {
    BenchRange { current: 0, max: self.benchs.len() }
  }
  /// Number of tools.
  #[inline]
  pub fn tool_len(& self) -> usize {
    self.tools.len()
  }
  /// Number of benchs.
  #[inline]
  pub fn bench_len(& self) -> usize {
    self.benchs.len()
  }
  /// String of a bench.
  #[inline]
  pub fn str_of_bench(& self, index: BenchIndex) -> & str {
    & self.benchs[* index]
  }
  /// Safe name for a bench, unique and can be used as file id.
  #[inline]
  pub fn safe_name_for_bench(& self, index: BenchIndex) -> String {
    format!("{:0>1$}", * index, format!("{}", self.benchs.len()).len())
  }

  /// Path to the directory of a tool.
  #[inline]
  pub fn path_of_tool(
    & self, conf: & Arc<RunConf>, tool: ToolIndex
  ) -> PathBuf {
    let mut path = PathBuf::from(& conf.out_dir) ;
    path.push( & self[tool].short ) ;
    path
  }

  /// Stderr directory path of a tool.
  #[inline]
  pub fn err_path_of_tool(
    & self, conf: & Arc<RunConf>, tool: ToolIndex
  ) -> PathBuf {
    let mut path = self.path_of_tool(conf, tool) ;
    path.push("err") ;
    path
  }
  /// Stdout directory path of a tool.
  #[inline]
  pub fn out_path_of_tool(
    & self, conf: & Arc<RunConf>, tool: ToolIndex
  ) -> PathBuf {
    let mut path = self.path_of_tool(conf, tool) ;
    path.push("out") ;
    path
  }

  /// Path to the stderr of a tool on a bench.
  #[inline]
  pub fn err_path_of(
    & self, conf: & Arc<RunConf>, tool: ToolIndex, bench: BenchIndex
  ) -> PathBuf {
    let mut path = self.err_path_of_tool(conf, tool) ;
    path.push( self.safe_name_for_bench(bench) ) ;
    path
  }

  /// Path to the stdout of a tool on a bench.
  #[inline]
  pub fn out_path_of(
    & self, conf: & Arc<RunConf>, tool: ToolIndex, bench: BenchIndex
  ) -> PathBuf {
    let mut path = self.out_path_of_tool(conf, tool) ;
    path.push( self.safe_name_for_bench(bench) ) ;
    path
  }

  /// Creates the error path of a tool.
  #[inline]
  pub fn mk_err_dir(
    & self, conf: & Arc<RunConf>, tool: ToolIndex
  ) -> Res<()> {
    mk_dir( self.err_path_of_tool(conf, tool) ).chain_err(
      || format!(
        "while creating err directory for {}",
        conf.emph( & self[tool].name )
      )
    )
  }
  /// Creates the output path of a tool.
  #[inline]
  pub fn mk_out_dir(
    & self, conf: & Arc<RunConf>, tool: ToolIndex
  ) -> Res<()> {
    mk_dir( self.out_path_of_tool(conf, tool) ).chain_err(
      || format!(
        "while creating output directory for {}",
        conf.emph( & self[tool].name )
      )
    )
  }
  /// Initializes the data file for some tool.
  #[inline]
  pub fn init_data_file(
    & self, conf: & Arc<RunConf>, tool: ToolIndex
  ) -> Res<File> {
    let mut path = PathBuf::new() ;
    path.push(& conf.out_dir) ;
    path.push(& self[tool].short) ;
    path.set_extension("data") ;
    let mut tool_file = conf.open_file_writer(
      path.as_path()
    ).chain_err(
      || format!(
        "while creating file `{}`, data file for `{}`",
        conf.sad(
          path.to_str().expect("non-UTF8 path")
        ),
        conf.emph( & self[tool].name )
      )
    ) ? ;
    self[tool].dump_info(& conf.timeout, & mut tool_file).chain_err(
      || format!(
        "while dumping info for tool `{}`",
        conf.emph( & self[tool].name )
      )
    ) ? ;
    Ok(tool_file)
  }
  /// Initializes the validator for some tool.
  #[inline]
  pub fn init_validator(
    & self, conf: & Arc<RunConf>, tool: ToolIndex
  ) -> Res<()> {
    use std::os::unix::fs::PermissionsExt ;
    if let Some(path) = conf.validator_path_of( & self[tool] ) {
      let mut file = conf.open_file_writer_exe(
        path.as_path(), true
      ).chain_err(
        || format!(
          "while creating validator for `{}`", conf.sad(& self[tool].name)
        )
      ) ? ;
      file.write( ::consts::validator::pref.as_bytes() ).chain_err(
        || format!(
          "while while writing to validator file for `{}`",
          conf.sad(& self[tool].name)
        )
      ) ? ;
      for (code, info) in conf.validators_iter() {
        file.write(
          format!("# {}\n{}=\"{}\"\n", info.desc, info.alias, code).as_bytes()
        ).chain_err(
          || format!(
            "while while writing to validator file for `{}`",
            conf.sad(& self[tool].name)
          )
        ) ? ;
      }
      if let Some(s) = self[tool].validator.as_ref() {
        file.write( s.as_bytes() ).chain_err(
          || format!(
            "while while writing to validator file for `{}`",
            conf.sad(& self[tool].name)
          )
        ) ? ;
        file.metadata().chain_err(
          || format!("could chmod validator file to executable")
        ) ? .permissions().set_mode(0o744)
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
  pub fn init_tools<T, F: Fn(& mut T, & ToolConf)>(
    & self, conf: & Arc<RunConf>, init: T, fold_fun: F
  ) -> Res< (ToolVec<File>, T) > {
    let mut tool_files = ToolVec::with_capacity( self.tool_len() ) ;
    let mut fold_data = init ;
    // Tool init: output dirs, validators, data file, mastre data init.
    for tool in self.tools() {
      // Output dirs.
      self.mk_err_dir(& conf, tool) ? ;
      if conf.log_stdout {
        self.mk_out_dir(& conf, tool) ?
      } ;

      // Data file.
      let tool_file = self.init_data_file(& conf, tool) ? ;
      tool_files.push( tool_file ) ;

      // Validator.
      self.init_validator(conf, tool) ? ;

      // Folding.
      fold_fun(& mut fold_data, & self[tool])
    }

    Ok((tool_files, fold_data))
  }

  /// Checks if a bench index is the last one.
  #[inline]
  pub fn is_last_bench(& self, bench: BenchIndex) -> bool {
    * bench + 1 >= self.benchs.len()
  }

  /// If any, runs the validator for a tool on some benchmark.
  pub fn validate(
    & self, conf: & Arc<RunConf>,
    tool: ToolIndex, bench: BenchIndex, status: & ExitStatus
  ) -> Res< Option<ExitStatus> > {
    if let Some(path) = conf.validator_path_of( & self[tool] ) {
      use std::process::Stdio ;
      let status = if let Some(code) = status.code() {
        code
      } else {
        bail!("could not retrieve exit status")
      } ;
      let out_path = self.out_path_of(conf, tool, bench) ;
      let err_path = self.err_path_of(conf, tool, bench) ;
      Command::new(
        path.as_os_str()
      ).stdin(
        Stdio::null()
      ).stdout( Stdio::null() ).stderr( Stdio::null() ).arg(
        & self[bench]
      ).arg(
        & format!("{}", status)
      ).arg(out_path).arg(err_path).status().chain_err(
        || format!(
          "while running validator for `{}` on benchmark `{}`",
          conf.sad(& self[tool].name), conf.sad(format!("{}", bench))
        )
      ).map(|s| Some(s))
    } else {
      Ok( None )
    }
  }
}
impl Index<BenchIndex> for Instance {
  type Output = String ;
  #[inline]
  fn index(& self, index: BenchIndex) -> & String {
    & self.benchs[* index]
  }
}
impl Index<ToolIndex> for Instance {
  type Output = ToolConf ;
  #[inline]
  fn index(& self, index: ToolIndex) -> & ToolConf {
    & self.tools[* index]
  }
}

/// Vector indexed by `ToolIndex`.
pub struct ToolVec<T> {
  /// The vector.
  vec: Vec<T>,
}
impl<T> ToolVec<T> {
  /// Creates a new vector.
  #[inline]
  pub fn with_capacity(n: usize) -> Self {
    ToolVec { vec: Vec::with_capacity(n) }
  }
  /// Push.
  #[inline]
  pub fn push(& mut self, elem: T) {
    self.vec.push(elem)
  }
  /// Pop.
  #[inline]
  pub fn pop(& mut self) -> Option<T> {
    self.vec.pop()
  }
}
impl<T> Index<ToolIndex> for ToolVec<T> {
  type Output = T ;
  fn index(& self, index: ToolIndex) -> & T {
    & self.vec[* index]
  }
}
impl<T> IndexMut<ToolIndex> for ToolVec<T> {
  fn index_mut(& mut self, index: ToolIndex) -> & mut T {
    & mut self.vec[* index]
  }
}









/// A spanned thing.
#[derive(Clone, Debug)]
pub struct Spnd<T> {
  val: T,
  start: usize,
  len: usize,
}
unsafe impl<T: Sync> Sync for Spnd<T> {}
impl AsRef<str> for Spnd<String> {
  fn as_ref(& self) -> & str {
    & self.val
  }
}
impl<T: fmt::Display> fmt::Display for Spnd<T> {
  fn fmt(& self, fmt: & mut fmt::Formatter) -> fmt::Result {
    write!(fmt, "{}", self.val)
  }
}
impl<T> Spnd<T> {
  /// Creates a spanned thing.
  #[inline]
  pub fn mk(val: T, start: usize, len: usize) -> Self {
    Spnd { val, start, len }
  }
  /// Extracts the value.
  #[inline]
  pub fn xtract(self) -> T {
    self.val
  }
  /// Yields the internal value.
  #[inline]
  pub fn get(& self) -> & T {
    & self.val
  }
  /// Length.
  #[inline]
  pub fn len(& self) -> usize {
    self.len
  }
  /// Start position.
  #[inline]
  pub fn start(& self) -> usize {
    self.start
  }
  /// Map.
  #[inline]
  pub fn map<U, F: Fn(T) -> U>(self, f: F) -> Spnd<U> {
    Spnd { val: f(self.val), start: self.start, len: self.len }
  }
  /// Changes the value.
  #[inline]
  pub fn replace(& mut self, val: T) {
    self.val = val
  }
}
impl<T> ::std::ops::Deref for Spnd<T> {
  type Target = T ;
  fn deref(& self) -> & T {
    & self.val
  }
}


/// Extends `Duration`.
pub trait DurationExt {
  /// Time in seconds with nanosecond precision as string.
  #[inline]
  fn as_sec_str(& self) -> String ;
  /// Zero duration.
  #[inline]
  fn zero() -> Duration { Duration::new(0, 0) }
}
impl DurationExt for Duration {
  fn as_sec_str(& self) -> String {
    format!(
      "{}.{:0>9}", self.as_secs(), self.subsec_nanos()
    )
  }
}



/// Extends string types with a substitution function.
pub trait StrExt {
  /// Replaces all non-ovelapping matches of a regex with something.
  fn subst(& self, regex: & ::regex::Regex, something: & str) -> String ;
  /// Performs path substitutions: `today` and `now`.
  fn path_subst(& self) -> String {
    use chrono::{ Local, Datelike, Timelike } ;
    use consts::subst::{ today_re, now_re } ;
    let today = Local::today() ;
    let today = & format!(
      "{}_{:0>2}_{:0>2}", today.year(), today.month(), today.day()
    ) ;
    let now = Local::now() ;
    let now = & format!(
      "{:0>2}_{:0>2}", now.hour(), now.minute()
    ) ;
    let res = self.subst(& * today_re, today) ;
    res.subst(& * now_re, now)
  }
}
impl StrExt for str {
  fn subst(& self, regex: & ::regex::Regex, something: & str) -> String {
    format!( "{}", regex.replace_all(self, something) )
  }
}
impl StrExt for String {
  fn subst(& self, regex: & ::regex::Regex, something: & str) -> String {
    (self as & str).subst(regex, something)
  }
}



/// Extension for `ExitStatus`.
pub trait ExitStatusExt {
  /// Data string version of the code. `?` if none.
  fn as_data_str(& self) -> String ;
}
impl ExitStatusExt for ExitStatus {
  fn as_data_str(& self) -> String {
    if let Some(code) = self.code() {
      format!("{}", code)
    } else { "?".into() }
  }
}



/// Dumps the example configuration file somewhere.
pub fn example_conf_file(conf: & GConf, file: String) -> Res<()> {
  use std::io::Write ;
  log!{ conf => "Opening `{}`...", conf.emph(& file) }
  let mut writer = conf.open_file_writer(& file).chain_err(
    || format!(
      "while opening example configuration file `{}` (write)",
      conf.emph(& file)
    )
  ) ? ;
  log!{ conf => "  writing example configuration..." }
  writer.write( ::consts::ex_conf_file.as_bytes() ).chain_err(
    || format!(
      "while writing example configuration to file `{}`", conf.emph(& file)
    )
  ) ? ;
  log!{ conf => "Done" ; "" }

  let mut path = PathBuf::from(& file) ;
  let was_okay = path.set_extension("benchs") ;
  let bench_file = path.to_str().and_then(
    |res| if was_okay { Some(res) } else { None }
  ).ok_or_else::<Error,_>(
    || format!(
      "`{}` is not a valid file name", conf.emph(& file)
    ).into()
  ) ? ;
  log!{ conf => "Opening `{}`...", conf.emph(bench_file) }
  let mut writer = conf.open_file_writer(bench_file).chain_err(
    || format!(
      "while opening benchmark list file `{}`",
      conf.emph(bench_file)
    )
  ) ? ;
  log!{ conf => "  writing benchmark list file..." }
  writer.write( ::consts::ex_bench_file.as_bytes() ).chain_err(
    || format!(
      "while writing example configuration to file `{}`", conf.emph(bench_file)
    )
  ) ? ;
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