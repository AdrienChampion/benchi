//! Utils for running benchmarks.
//!
//! Note that `ToolRun`s communicate the results directly to the master. All
//! they send to `BenchRun`s is that they're done.

use common::*;

pub use std::process::{Child, Stdio};
pub use std::sync::mpsc::{channel, RecvError, TryRecvError};
pub use std::thread::{sleep, spawn, JoinHandle};

/// Sets the pipes for a command.
pub fn set_pipes(cmd: &mut Command, out_file: Option<File>, err_file: File) {
    use std::os::unix::io::{FromRawFd, IntoRawFd};
    if let Some(out_file) = out_file {
        let out = unsafe { Stdio::from_raw_fd(out_file.into_raw_fd()) };
        cmd.stdout(out);
    } else {
        cmd.stdout(Stdio::null());
    }
    let err = unsafe { Stdio::from_raw_fd(err_file.into_raw_fd()) };
    cmd.stderr(err);
    cmd.stdin(Stdio::null());
}

/// Sender / receiver from bench runs to tool runs.
pub type BenchToToolChannel = (Sender<(ToolIdx, BenchIdx)>, Receiver<(ToolIdx, BenchIdx)>);

/// Channel from master to bench runs.
pub fn master_to_bench_channel() -> (Sender<BenchIdx>, Receiver<BenchIdx>) {
    channel()
}

/// Channel from bench runs to tool runs.
pub fn bench_to_tool_channel() -> BenchToToolChannel {
    channel()
}

/// Different things a benchmark run can yield.
#[derive(Debug)]
pub enum BenchRes {
    /// Runtime was longer than timeout.
    Timeout(ExitStatus),
    /// Successful.
    Success(Duration, ExitStatus),
    /// Error.
    Error(Duration, ExitStatus),
    /// Error in `benchi`.
    BenchiError(Error),
}

/// Result of a run.
#[derive(Debug)]
pub struct RunRes {
    /// Tool index.
    pub tool: ToolIdx,
    /// Bench index.
    pub bench: BenchIdx,
    /// Result.
    pub res: BenchRes,
}
/// Channel from tool runs to master.
pub fn tool_to_master_channel() -> (Sender<RunRes>, Receiver<RunRes>) {
    channel()
}

/// Channel from bench runs to master. Communicates the uid of the bench
/// run so that the master can send it a new job. Also sends a hashmap of
/// disagreeing exit codes if there was some disagreement.
pub fn bench_to_master_channel() -> Channel<Res<usize>> {
    channel()
}

/// Channel from tool runs to bench runs.
pub fn tool_to_bench_channel() -> Channel<usize> {
    channel()
}
