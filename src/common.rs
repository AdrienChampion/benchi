//! Common types and functions used by `benchi`.

use std::fmt ;
use std::ops::{ Index, IndexMut, Deref } ;

pub use std::process::Command ;
pub use std::fs::File ;
pub use std::io::{ Write, BufRead, BufReader } ;
pub use std::time::{ Instant, Duration } ;
pub use std::path::{ Path, PathBuf } ;
pub use std::iter::Iterator ;
pub use std::sync::Arc ;

pub use pbr::{ ProgressBar, MultiBar } ;

use ansi::{ Style, Colour } ;

use errors::* ;

/// Log macro, deactivated if `$conf`.quiet is `true`.
macro_rules! log {

  ( | internal | => ) => (()) ;

  ( | internal | => ; $($tail:tt)* ) => (
    log!(| internal | => $($tail)*)
  ) ;

  ( | internal | => let $p:pat = $e:expr ; $($tail:tt)* ) => ({
    let $p = $e ;
    log!(| internal | => $($tail)*)
  }) ;

  ( | internal | => { $($head:tt)+ } $($tail:tt)* ) => ({
    { $($head)+ }
    log!(| internal | => $($tail)*)
  }) ;

  ( | internal | => $($head:expr),* ; $($tail:tt)* ) => ({
    println!($($head),*) ;
    log!(| internal | => $($tail)*)
  }) ;

  (
    $conf:expr => $($stuff:tt)+
  ) => ({
    if ! $conf.quiet {
      log!( |internal| => $($stuff)+ ; )
    }
  }) ;

  (
    $conf:expr , $verb:ident => $($stuff:tt)+
  ) => ({
    if $conf.$verb {
      log!( |internal| => $($stuff)+ ; )
    }
  }) ;
}

/// Opens a file in write mode.
#[inline]
pub fn open_file_writer<P: AsRef<Path>>(path: P) -> Res<File> {
  ::std::fs::OpenOptions::new()
  .write(true)
  .create_new(true)
  .open( path.as_ref() )
  .map_err( |e| e.into() )
}

/// Creates a directory if not already there.
#[inline]
pub fn mk_dir<P: AsRef<Path>>(path: P) -> Res<()> {
  ::std::fs::DirBuilder::new().recursive(true).create(path).map_err(
    |e| e.into()
  )
}

/// Configuration structure.
#[derive(Debug)]
pub struct Conf {
  /// Number of parallel bench runs.
  pub bench_par: usize,
  /// Number of parallel tool runs.
  pub tool_par: usize,
  /// Timeout.
  pub timeout: Duration,
  /// Output directory.
  pub out_dir: String,
  /// Tool configuration file.
  pub tool_file: String,
  /// Benchmark file.
  pub bench_file: String,
  /// Quiet mode?
  pub quiet: bool,
  /// Verbose mode?
  pub verb: bool,
  /// Log output?
  pub log_output: bool,
  /// Trying?
  pub try: Option<usize>,
  /// Emphasis style.
  emph: Style,
  /// Happy style.
  hap: Style,
  /// Sad style.
  sad: Style,
  /// Bad style.
  bad: Style,
}
unsafe impl Sync for Conf {}
impl Default for Conf {
  fn default() -> Self {
    Conf {
      bench_par: 1,
      tool_par: 1,
      timeout: Duration::new(60, 0),
      out_dir: ".".into(),
      tool_file: "tools.conf".into(),
      bench_file: "bench.file".into(),
      quiet: false,
      verb: false,
      log_output: false,
      try: None,
      emph: Style::new().bold(),
      hap: Colour::Green.normal(),
      sad: Colour::Yellow.normal(),
      bad: Colour::Red.normal(),
    }
  }
}
impl Conf {
  /// Creates a configuration.
  #[inline]
  pub fn mk(
    bench_par: usize, tool_par: usize,
    timeout: Duration,
    out_dir: String, tool_file: String, bench_file: Option<String>,
    quiet: bool, verb: bool, log_output: bool, try: Option<usize>,
    colored: bool,
  ) -> Self {
    let mut conf = Conf {
      bench_par, tool_par, timeout,
      out_dir, tool_file, bench_file: bench_file.expect(
        "optional bench file is unimplemented"
      ),
      quiet, verb, log_output, try,
      emph: Style::new(),
      hap: Style::new(),
      sad: Style::new(),
      bad: Style::new(),
    } ;
    conf.coloring(colored) ;
    conf
  }
  /// Sets the coloring.
  pub fn coloring(& mut self, colored: bool) {
    self.emph = if colored {
      Style::new().bold()
    } else { Style::new() } ;
    self.hap = if colored {
      Colour::Green.normal().bold()
    } else { Style::new() } ;
    self.sad = if colored {
      Colour::Yellow.normal().bold()
    } else { Style::new() } ;
    self.bad = if colored {
      Colour::Red.normal().bold()
    } else { Style::new() } ;
  }
  /// String emphasis.
  #[inline]
  pub fn emph<S: AsRef<str>>(& self, s: S) -> String {
    format!("{}", self.emph.paint(s.as_ref()))
  }
  /// Happy string.
  #[inline]
  pub fn happy<S: AsRef<str>>(& self, s: S) -> String {
    format!("{}", self.hap.paint(s.as_ref()))
  }
  /// Sad string.
  #[inline]
  pub fn sad<S: AsRef<str>>(& self, s: S) -> String {
    format!("{}", self.sad.paint(s.as_ref()))
  }
  /// Bad string.
  #[inline]
  pub fn bad<S: AsRef<str>>(& self, s: S) -> String {
    format!("{}", self.bad.paint(s.as_ref()))
  }
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
  // /// Optional validator.
  // pub validator: ()
}
unsafe impl Sync for ToolConf {}









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
#[derive(Clone, Copy, Debug)]
pub struct BenchIndex {
  /// The index.
  n: usize,
}
impl Deref for BenchIndex {
  type Target = usize ;
  fn deref(& self) -> & usize { & self.n }
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
  /// Time in seconds with microsecond precision as string.
  #[inline]
  fn as_sec_str(& self) -> String ;
}
impl DurationExt for Duration {
  fn as_sec_str(& self) -> String {
    format!(
      "{}.{:0>6}", self.as_secs(), self.subsec_nanos() / 1_000u32
    )
  }
}