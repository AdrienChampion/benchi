//! Common types and functions used by `benchi`.

use std::fmt ;
use std::ops::{ Index, IndexMut, Deref } ;

pub use std::process::Command ;
pub use std::fs::File ;
pub use std::io::{ Lines, Write, BufRead, BufReader } ;
pub use std::time::{ Instant, Duration } ;
pub use std::path::{ Path, PathBuf } ;
pub use std::iter::Iterator ;
pub use std::sync::Arc ;

pub use pbr::{ ProgressBar, MultiBar } ;

use ansi::{ Style, Colour } ;

use errors::* ;

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

/// Creates a directory if not already there.
#[inline]
pub fn mk_dir<P: AsRef<Path>>(path: P) -> Res<()> {
  ::std::fs::DirBuilder::new().recursive(true).create(path).map_err(
    |e| e.into()
  )
}


/// Clap result.
pub enum Clap {
  /// Run mode.
  Run(RunConf, Vec<ToolConf>),
  /// Cumulative plot.
  CumulPlot(PlotConf, Vec<String>),
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


/// Has a global conf.
pub trait GConfExt {
  /// The global conf.
  #[inline]
  fn gconf(& self) -> & GConf ;
  /// Opens a file in write mode.
  #[inline]
  fn open_file_writer<P: AsRef<Path>>(& self, path: P) -> Res<File> {
    let conf = self.gconf() ;
    let mut options = ::std::fs::OpenOptions::new() ;
    options.write(true) ;
    if conf.ow_files {
      options.create(true).truncate(true) ;
    } else {
      options.create_new(true) ;
    }
    options.open( path.as_ref() ).map_err( |e| e.into() )
  }
}
impl<T: GConfExt> ColorExt for T {
  fn styles(& self) -> & Styles { & self.gconf().styles }
}
impl<T: GConfExt> VerbExt for T {
  fn verb(& self) -> & Verb { & self.gconf().verb }
}

/// Global configuration.
#[derive(Debug, Default, Clone)]
pub struct GConf {
  /// Verbosity.
  verb: Verb,
  /// Styles.
  styles: Styles,
  /// Overwrite files when present.
  pub ow_files: bool,
}
impl GConfExt for GConf {
  fn gconf(& self) -> & GConf { self }
}
impl GConf {
  /// Creates a configuration.
  #[inline]
  pub fn mk(verb: Verb, colored: bool, ow_files: bool) -> Self {
    GConf { verb, styles: Styles::mk(colored), ow_files }
  }
}


/// Plot configuration.
pub struct PlotConf {
  /// Output file.
  pub file: String,
  /// Generate pdf?
  pub pdf: bool,
  /// Command to run.
  pub then: Option<String>,
  /// Global conf.
  gconf: GConf,
}
impl GConfExt for PlotConf {
  fn gconf(& self) -> & GConf { & self.gconf }
}
impl PlotConf {
  /// Creates a plot conf.
  #[inline]
  pub fn mk(
    file: String, pdf: bool, then: Option<String>, gconf: GConf
  ) -> Self {
    PlotConf { file, pdf, then, gconf }
  }
}


/// Run configuration.
#[derive(Debug)]
pub struct RunConf {
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
  /// Trying?
  pub try: Option<usize>,
  /// Global configuration.
  gconf: GConf,
}
impl GConfExt for RunConf {
  fn gconf(& self) -> & GConf { & self.gconf }
}
impl Default for RunConf {
  fn default() -> Self {
    RunConf {
      bench_par: 1, tool_par: 1,
      timeout: Duration::new(60, 0),
      try: None,
      out_dir: ".".into(),
      tool_file: "tools.conf".into(),
      bench_file: "bench.file".into(),
      gconf: GConf::default(),
    }
  }
}
impl RunConf {
  /// Creates a configuration.
  #[inline]
  pub fn mk(
    bench_par: usize, tool_par: usize,
    timeout: Duration, try: Option<usize>,
    out_dir: String, tool_file: String, bench_file: String,
    gconf: GConf,
  ) -> Self {
    RunConf {
      bench_par, tool_par, timeout, try,
      out_dir, tool_file, bench_file,
      gconf
    }
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
static name_dump_pref:  & str = "# " ;
static short_dump_pref: & str = "# short: " ;
static graph_dump_pref: & str = "# graph: " ;
static cmd_dump_pref:   & str = "# cmd: " ;
unsafe impl Sync for ToolConf {}
impl ToolConf {
  /// Dumps info to a writer.
  pub fn dump_info<W: Write>(& self, w: & mut W) -> ::std::io::Result<()> {
    writeln!(w, "{}{}", name_dump_pref, self.name) ? ;
    writeln!(w, "{}{}", short_dump_pref, self.short) ? ;
    writeln!(w, "{}{}", graph_dump_pref, self.graph) ? ;
    write!(  w, "{}", cmd_dump_pref) ? ;
    for s in & self.cmd {
      write!(w, "{} ", s) ?
    }
    writeln!(w, "") ? ;
    writeln!(w, "#")
  }
  /// Loads a tool configuration from a dump.
  pub fn from_dump<Br: BufRead>(lines: & mut Lines<Br>) -> Res<Self> {
    let name = if let Some(line) = lines.next() {
      let line = line ? ;
      let len = name_dump_pref.len() ;
      if line.len() < len + 1 || & line[0..len] != name_dump_pref {
        bail!(
          format!(
            "illegal dump file, first line should be `{}<tool name>`",
            name_dump_pref
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
      let len = short_dump_pref.len() ;
      if line.len() < len + 1 || & line[0..len] != short_dump_pref {
        bail!(
          format!(
            "illegal dump file, second line should be \
            `{}<tool short name>`", short_dump_pref
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
      let len = graph_dump_pref.len() ;
      if line.len() < len + 1 || & line[0..len] != graph_dump_pref {
        bail!(
          format!(
            "illegal dump file, second line should be \
            `{}<tool graph name>`", graph_dump_pref
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
      let len = cmd_dump_pref.len() ;
      if line.len() < len + 1 || & line[0..len] != cmd_dump_pref {
        bail!(
          format!(
            "illegal dump file, second line should be \
            `{}<tool cmd>`", cmd_dump_pref
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
    
    Ok( ToolConf { name, short, graph, cmd } )
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
  /// Safe name for a bench, unique and can be used as file id.
  #[inline]
  pub fn safe_name_for_bench(& self, index: BenchIndex) -> String {
    format!("{:0>1$}", * index, format!("{}", self.benchs.len()).len())
  }

  /// Stderr directory path of a tool.
  #[inline]
  pub fn err_path_of_tool(
    & self, conf: & Arc<RunConf>, tool: ToolIndex
  ) -> PathBuf {
    let mut path = PathBuf::from( & conf.out_dir ) ;
    path.push( & self[tool].short ) ;
    path.push("err") ;
    path
  }
  /// Stdout directory path of a tool.
  #[inline]
  pub fn out_path_of_tool(
    & self, conf: & Arc<RunConf>, tool: ToolIndex
  ) -> PathBuf {
    let mut path = PathBuf::from( & conf.out_dir ) ;
    path.push( & self[tool].short ) ;
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

  /// Checks if a bench index is the last one.
  #[inline]
  pub fn is_last_bench(& self, bench: BenchIndex) -> bool {
    * bench + 1 >= self.benchs.len()
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


/// A plot command.
pub enum Plot {
  /// Compares two tools benchmark per benchmark (scatterplot).
  Comparative {
    /// The file to write the plot to.
    file: Option<String>,
    /// The first short tool name.
    tool_1: String,
    /// The second short tool name.
    tool_2: String,
  },

  /// Cumulative plot for some tools.
  Cumulative {
    /// The file to write the plot to.
    file: Option<String>,
    /// Some tools to compare.
    tools: Vec<String>,
  },
}
impl Plot {
  /// Creates a comparative plot command.
  #[inline]
  pub fn comparative(
    file: Option<String>, tool_1: String, tool_2: String
  ) -> Self {
    Plot::Comparative { file, tool_1, tool_2 }
  }
  /// Creates a cumulative plot command.
  #[inline]
  pub fn cumulative(
    file: Option<String>, tools: Vec<String>
  ) -> Self {
    Plot::Cumulative { file, tools }
  }
}