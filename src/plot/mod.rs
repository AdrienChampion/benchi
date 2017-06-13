//! Plot generation.

pub mod cumul ;
pub mod compare ;

use common::plot::* ;
use errors::* ;

/// Plot things.
pub fn work(conf: & PlotConf, kind: PlotKind) -> Res<()> {
  match kind {
    PlotKind::Cumul {
      files
    } => cumul::work(conf, files).chain_err(
      || "during cumulative plot generation"
    ),
    PlotKind::Compare {
      file_1, file_2
    } => compare::work(conf, file_1, file_2).chain_err(
      || "during comparative plot generation"
    ),
  }
}