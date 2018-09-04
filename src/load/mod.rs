//! Data loading.

use std::path::Path ;

use common::{ *, res::{ ToolRes, RunRes } } ;

mod run ;
mod res ;

pub use self::run::{
    NewToolConf, NewToolConfs, NewCode, NewCodes,
} ;
pub use self::res::NewBenchRes ;


/// Run configuration loader.
pub fn run<P>(gconf: & GConf, file: P) -> Res<(
    Option<String>, NewToolConfs, NewCodes
)>
where P: AsRef<Path> {
    run::toml(gconf, file).map(
        |res| res.destroy()
    )
}


/// Result data file loader.
pub fn res<P>(gconf: & GConf, run_res: & mut RunRes, file: P) -> Res<ToolRes>
where P: AsRef<Path> {
    res::toml(gconf, run_res, file)
}

/// Handles a serde load error.
fn serde_error(gconf: & GConf, e: & ::toml::de::Error, txt: & str) -> Error {
    let mut error = format!("{}", e) ;

    if let Some((l,c)) = e.line_col() {
        for (cnt, line) in txt.lines().enumerate() {
            if cnt == l {
                let line_count = format!("{}", l + 1) ;
                error += & format!(
                    "\n{} |", " ".repeat( line_count.len() )
                ) ;
                error += & format!("\n
                    {} | {}", line_count, line
                ) ;
                error += & format!(
                    "{} | {}{}",
                    " ".repeat( line_count.len() ),
                    " ".repeat( c ),
                    gconf.bad("^")
                ) ;
                break
            }
        }
    }

    error.into()
}