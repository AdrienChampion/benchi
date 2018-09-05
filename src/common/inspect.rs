//! Inspection basic types and helpers.

/// The inspection subcommand.
pub fn inspect_subcommand<'a, 'b>() -> ::clap_lib::App<'a, 'b> {
    use clap_lib::*;

    SubCommand::with_name("inspect")
        .about("Interactive data inspection. (UNIMPLEMENTED)")
        .about("UNIMPLEMENTED")
}
