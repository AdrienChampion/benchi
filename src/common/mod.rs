//! Common types and functions used by `benchi`.

use std::fmt ;
use std::ops::{ Index, IndexMut, Deref } ;


pub use std::str::FromStr ;
pub use std::process::{ Command, ExitStatus } ;
pub use std::fs::File ;
pub use std::io::{ Lines, Read, Write, BufRead, BufReader } ;
pub use std::time::{ Instant, Duration } ;
pub use std::path::{ Path, PathBuf } ;
pub use std::iter::{ Iterator, FromIterator } ;
pub use std::sync::Arc ;
pub use std::collections::HashMap ;
pub use std::sync::mpsc::{ Sender, Receiver } ;

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
      warn!{ $conf, line => $($stuff)+ ; }
      println!("{}", $conf.sad("|===|")) ;
      println!("")
    }
  ) ;
  ($conf:expr, line => $($stuff:tt)+) => (
    if ! $conf.quiet() {
      log!{
        |internal| $conf.sad("| ") => $($stuff)+ ;
      }
    }
  )
}

pub mod res ;
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






/// Validation code info.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValdCode {
  /// Alias.
  pub alias: String,
  /// Description.
  pub desc: String,
  /// Color (optional).
  pub color: Option<String>,
}




/// Validator configuration.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValdConf {
  /// Success codes.
  succ: HashMap<i32, ValdCode>,
}
impl ValdConf {
  /// True if the valdconf is empty.
  pub fn is_empty(& self) -> bool {
    self.succ.is_empty()
  }
  /// Creates an empty validator configuration.
  pub fn empty() -> Self {
    ValdConf { succ: HashMap::new() }
  }
  /// Info of an exit code.
  pub fn get(& self, code: i32) -> Option<& ValdCode> {
    self.succ.get(& code)
  }

  /// Adds a new success validation code.
  pub fn add_succ(mut self, code: i32, info: ValdCode) -> Res<Self> {
    if let Some( ValdCode { ref desc, .. } ) = self.succ.insert(code, info) {
      Err(
        format!(
          "code `{}` is already registered as success `{}`", code, desc
        ).into()
      )
    } else {
      Ok(self)
    }
  }
  /// Dumps itself to a writer.
  pub fn dump<W: Write>(& self, w: & mut W) -> Res<()> {
    for (code, info) in & self.succ {
      write!(
        w, "# success: {}, {}, {}\n",
        code, info.alias, info.desc
      ).chain_err(
        || "while dumping validator information"
      ) ?
    }
    Ok(())
  }

  // /// Parses a validation configuration from a dump.
  // pub fn of_dump<B: BufRead>(lines: LinesIter<B>) -> Res<Self> {
    
  // }
}
/// Validator configuration extensions.
pub trait ValdConfExt {
  /// Accessor.
  fn vald_conf(& self) -> & ValdConf ;

  /// Checks whether an exit status is a success.
  ///
  /// Returns true if no success code is registered and `status.success()`, or
  /// the status is `Some(code)` and `code` is registered as a success.
  fn check_succ(& self, status: & ExitStatus) -> bool {
    let vald_conf = self.vald_conf() ;
    if vald_conf.succ.is_empty() {
      status.success()
    } else if let Some(code) = status.code() {
      vald_conf.succ.get(& code).is_some()
    } else {
      false
    }
  }

  /// Iterator over the success codes declared.
  fn validators_iter(& self) -> ::std::collections::hash_map::Iter<
    i32, ValdCode
  > {
    self.vald_conf().succ.iter()
  }
}
impl ValdConfExt for ValdConf {
  fn vald_conf(& self) -> & ValdConf { self }
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
    & self, conf: & Arc<RunConf>, w: & mut W
  ) -> ::std::io::Result<()> {
    use consts::dump::* ;

    writeln!(w, "{} {{", self.name) ? ;
    writeln!(
      w, "  {}: {}", short_name_key, self.short
    ) ? ;
    writeln!(
      w, "  {}: {}", graph_name_key, self.graph
    ) ? ;
    write!(  w, "  {}: \"", cmd_key) ? ;
    let mut iter = self.cmd.iter() ;
    if let Some(s) = iter.next() {
      write!(w, "{}", s) ? ;
      for s in iter {
        write!(w, " {}", s) ?
      }
    }
    writeln!(w, "\"") ? ;
    if let Some(path) = conf.validator_path_of(& self) {
      writeln!(w, "  {}: ```", vald_key) ? ;
      let file = ::std::fs::OpenOptions::new().read(true).open(& path) ? ;
      for line in BufReader::new(file).lines() {
        writeln!(w, "{}", line ?) ?
      }
      writeln!(w, "  ```") ? ;
    }
    writeln!(w, "}}") ? ;

    if ! conf.vald_conf().is_empty() {
      writeln!(w, "{} {{", vald_conf_key) ? ;
      for (code, info) in conf.validators_iter() {
        writeln!(
          w, "  {}: {}, {}, {}", vald_conf_suc_key, code, info.alias, info.desc
        ) ?
      }
      writeln!(w, "}}") ? ;
    }

    writeln!(
      w, "{}: {}", timeout_key, conf.timeout.as_sec_str()
    ) ? ;
    writeln!(w, "{}", & * cmt_pref)
  }
}









/// The index of a tool, just a usize.
///
/// Can **only** be created by a `ToolRange`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
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
  /// Initializes the data file and the validator for some tool.
  #[inline]
  pub fn init_data_file_and_validator(
    & self, conf: & Arc<RunConf>, tool: ToolIndex
  ) -> Res<File> {
    self.init_validator(conf, tool) ? ;
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
    self[tool].dump_info(conf, & mut tool_file).chain_err(
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
      let tool_file = self.init_data_file_and_validator(& conf, tool) ? ;
      tool_files.push( tool_file ) ;

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
  /// Clears the vector.
  pub fn into_iter(self) -> ::std::vec::IntoIter<T> {
    self.vec.into_iter()
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
impl<T> FromIterator<T> for ToolVec<T> {
  fn from_iter< Iter: IntoIterator<Item = T> >(iter: Iter) -> Self {
    ToolVec { vec: iter.into_iter().collect() }
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


// /// Extends `io`'s `Line`.
// pub trait LinesExt {
//   /// Returns the next line that's not a comment.
//   fn non_cmt_next(& mut self) -> Option< Result<String> > ;
// }
// impl<Br: BufRead> LinesExt for ::std::io::Lines<Br> {
//   fn non_cmt_next(& mut self) -> Option< Result<String> > {
//     while let Some(line_res) = self.next() {
//       let line = line_res ? ;
//       if ::consts::dump::empty_cmt_re.is_match(& line) {
//         Some( Ok(line) )
//       }
//     }
//     None
//   }
// }



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


/// The optional exit code of a tool run.
pub type ExitCode = Option<i32> ;

/// Type returned by a `channel` function: a sender and a receiver.
pub type Channel<T> = (Sender<T>, Receiver<T>) ;


/// Line iterator buffer over a file.
pub type FileLiter = LinesIter< BufReader<File> > ;

/// Line iterator buffer. Counts lines.
///
/// Pushing lines that were not already there is a logical error and can cause
/// an underflow of the line counter.
pub struct LinesIter<B> {
  /// Actual lines.
  lines: Lines<B>,
  /// Buffer that can be pushed to.
  vec: Vec<String>,
  /// Line counter.
  line_count: usize,
}
impl<B: BufRead> LinesIter<B> {
  /// Creates a line iterator.
  pub fn mk(buf_read: B) -> Self {
    LinesIter { lines: buf_read.lines(), vec: vec![], line_count: 0 }
  }

  /// Index of the **last line** popped.
  pub fn last_line_index(& self) -> usize {
    self.line_count
  }

  // /// Checks if there is a next line.
  // pub fn has_next(& self) -> bool {
  //   if self.vec.is_empty() {
  //     if let Some(line) = self.lines.next() {
  //       self.vec.push(line) ;
  //       true
  //     } else {
  //       false
  //     }
  //   } else {
  //     true
  //   }
  // }

  /// Pushes a line.
  pub fn push(& mut self, line: String) {
    self.line_count += 1 ;
    self.vec.push(line)
  }
}
impl<B: ::std::io::BufRead> Iterator for LinesIter<B> {
  type Item = ::std::io::Result<String> ;
  fn next(& mut self) -> Option< ::std::io::Result<String> > {
    let maybe_line = if let Some(line) = self.vec.pop() {
      Some( Ok(line) )
    } else {
      self.lines.next()
    } ;
    if maybe_line.is_some() {
      self.line_count += 1
    }
    maybe_line
  }
}