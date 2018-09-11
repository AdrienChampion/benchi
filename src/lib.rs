//! Careful benchmarking and plotting for research papers.

#![forbid(missing_docs)]
#![allow(non_upper_case_globals)]

extern crate chrono;
#[macro_use]
extern crate clap as clap_lib;
extern crate ansi_term as ansi;
extern crate pbr;
extern crate regex;
#[macro_use]
extern crate error_chain;
#[macro_use]
extern crate lazy_static;
extern crate rayon;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate toml;
extern crate wait_timeout;
#[macro_use]
extern crate mylib;
extern crate atty;
extern crate open;

pub mod consts;
pub mod errors;
#[macro_use]
pub mod common;
pub mod clap;
// pub mod parse ;
pub mod inspect;
pub mod load;
pub mod plot;
pub mod run;
