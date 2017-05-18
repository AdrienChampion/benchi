//! Common types and functions used by `benchi`.

pub use std::time::Duration ;
pub use std::path::Path ;
use std::fmt ;

use ansi::{ Style, Colour } ;

/// If-let-some macro.
#[macro_export]
macro_rules! if_some {
  ($p:pat = $e:expr => $then:block else $else:block) => (
    if let Some($p) = $e $then else $else
  ) ;
  ($p:pat = $e:expr => $then:expr $(, $else:expr)* $(,)*) => (
    if_some!{ $p = $e => {$then} $(else {$else})* }
  )
}

/// Configuration structure.
pub struct Conf {
  /// Parallel on files?
  pub file_par: bool,
  /// Parallel on tools?
  pub tool_par: bool,
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
impl Default for Conf {
  fn default() -> Self {
    Conf {
      file_par: false,
      tool_par: false,
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

/// A spanned thing.
#[derive(Clone)]
pub struct Spnd<T> {
  val: T,
  start: usize,
  len: usize,
}
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