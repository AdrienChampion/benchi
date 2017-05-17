//! Common types and functions used by `benchi`.

pub use std::time::Duration ;
pub use std::path::Path ;

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
}
impl Default for Conf {
  fn default() -> Self {
    Conf {
      file_par: false,
      tool_par: false,
      timeout: Duration::new(60, 0),
      out_dir: ".".into(),
      tool_file: "tools.conf".into(),
    }
  }
}