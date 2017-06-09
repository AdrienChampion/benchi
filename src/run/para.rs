/*! Parallel framework for runners.

Note that `ToolRun`s communicate the results directly to the master. All they
send to `BenchRun`s is that they're done.
*/

use common::* ;
use errors::* ;

pub use std::thread::{ spawn, sleep, JoinHandle } ;
pub use std::process::{ Child, Command, Stdio, ExitStatus } ;
pub use std::sync::mpsc::{
  channel, Sender, Receiver, RecvError, TryRecvError
} ;

/// Sets the pipes for a command.
#[cfg(not(windows))]
pub fn set_pipes(
  cmd: & mut Command, out_file: Option<File>, err_file: File
) {
  use std::os::unix::io::{ IntoRawFd, FromRawFd } ;
  if let Some(out_file) = out_file {
    let out = unsafe { Stdio::from_raw_fd(out_file.into_raw_fd()) } ;
    cmd.stdout(out) ;
  } else {
    cmd.stdout( Stdio::null() ) ;
  }
  let err = unsafe { Stdio::from_raw_fd(err_file.into_raw_fd()) } ;
  cmd.stderr(err) ;
  cmd.stdin( Stdio::null() ) ;
}


/// Kills a process from its pid.
#[cfg(not(windows))]
pub fn kill_process(pid: u32) {
  let _ = Command::new("kill").arg("-s").arg("9").arg(
    format!("-{}", pid)
  ).output() ;
}

/// Kills a process from its pid.
#[cfg(windows)]
pub fn kill_process(pid: u32) {
  let _ = Command::new("taskkill").arg("/t").arg("/pid").arg(
    format!("{}", pid)
  ).output() ;
}


/// Channel from master to bench runs.
pub fn master_to_bench_channel() -> (
  Sender< BenchIndex >, Receiver< BenchIndex >
) {
  channel()
}

/// Channel from bench runs to tool runs.
pub fn bench_to_tool_channel() -> (
  Sender< (ToolIndex, BenchIndex) >, Receiver< (ToolIndex, BenchIndex) >
) {
  channel()
}

/// Output of a process.
pub type Output = Option< (ExitStatus, Res<Option<ExitStatus>>) > ;

/// Result of a run.
#[derive(Debug)]
pub struct RunRes<T> {
  /// Tool index.
  pub tool: ToolIndex,
  /// Bench index.
  pub bench: BenchIndex,
  /// Result.
  pub res: Res<T>,
}
/// Channel from tool runs to master.
pub fn tool_to_master_channel<T>() -> (
  Sender< RunRes<T> >, Receiver< RunRes<T> >
) {
  channel()
}

/// Channel from bench runs to master.
pub fn bench_to_master_channel() -> (
  Sender< Res<usize> >, Receiver< Res<usize> >
) {
  channel()
}

/// Channel from tool runs to bench runs.
///
/// Not a `Res`: the tool run communicates failures to the master directly.
pub fn tool_to_bench_channel() -> (
  Sender< usize >, Receiver< usize >
) {
  channel()
}
