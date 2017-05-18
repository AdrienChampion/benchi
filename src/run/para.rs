/*! Parallel framework for runners.

Note that `ToolRun`s communicate the results directly to the master. All they
send to `BenchRun`s is that they're done.
*/

use common::* ;
use errors::* ;

pub use std::thread::spawn ;
pub use std::sync::Arc ;
pub use std::sync::mpsc::{
  channel, Sender, Receiver, RecvError, TryRecvError
} ;

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

/// Result of a run.
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
