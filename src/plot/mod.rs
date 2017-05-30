//! Plot generation.

pub mod cumul ;
pub mod compare ;

use common::* ;
use errors::* ;

/// Plot things.
pub fn work(conf: & PlotConf, kind: PlotKind) -> Res<()> {
  match kind {
    PlotKind::Cumul {
      files
    } => cumul::work(conf, files),
    PlotKind::Compare {
      file_1, file_2
    } => compare::work(conf, file_1, file_2),
  }
}