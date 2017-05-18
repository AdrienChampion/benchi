//! Common types and functions used by `benchi`.

use std::fmt ;
use std::ops::{ Index, Deref } ;


pub use std::time::{ Instant, Duration } ;
pub use std::path::Path ;
pub use std::iter::Iterator ;

use ansi::{ Style, Colour } ;

/// Configuration structure.
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
      emph: Style::new().bold(),
      hap: Colour::Green.normal(),
      sad: Colour::Yellow.normal(),
      bad: Colour::Red.normal(),
    }
  }
}
impl Conf {
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
  pub name: Spnd<String>,
  /// Short name.
  pub short: Spnd<String>,
  /// Graph name.
  pub graph: Spnd<String>,
  /// Command (lines).
  pub cmd: Spnd<String>,
  // /// Optional validator.
  // pub validator: ()
}
unsafe impl Sync for ToolConf {}









/// The index of a tool, just a usize.
///
/// Can **only** be created by a `ToolRange`.
#[derive(Clone, Copy, Debug)]
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
  /// String of a bench.
  #[inline]
  pub fn str_of_bench(& self, index: BenchIndex) -> & str {
    & self.benchs[* index]
  }
}
impl Index<BenchIndex> for Instance {
  type Output = Path ;
  #[inline]
  fn index(& self, index: BenchIndex) -> & Path {
    Path::new( & self.benchs[* index] )
  }
}
impl Index<ToolIndex> for Instance {
  type Output = ToolConf ;
  #[inline]
  fn index(& self, index: ToolIndex) -> & ToolConf {
    & self.tools[* index]
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
}
impl<T> ::std::ops::Deref for Spnd<T> {
  type Target = T ;
  fn deref(& self) -> & T {
    & self.val
  }
}