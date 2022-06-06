//! Plot generation.

prelude!(
    common::plot::{PlotConf, PlotKind},
);

pub mod compare;
pub mod cumul;

/// Runs `gnuplot` and the user's command depending on the configuration.
pub fn run_stuff(conf: &PlotConf, final_file: String) -> Res<()> {
    if conf.gnuplot {
        log! {
          conf, verb =>
            "  running `{} {}`...", conf.emph("gnuplot"), conf.emph(& conf.file)
        }
        // Run gnuplot.
        let status = Command::new("gnuplot")
            .arg(&conf.file)
            .status()
            .chain_err(|| {
                format!(
                    "while running gnuplot command on `{}`",
                    conf.emph(&conf.file)
                )
            })?;

        if !status.success() {
            bail!(format!(
                "gnuplot failed on plot file `{}`",
                conf.emph(&conf.file)
            ))
        }

        if let Some(ref cmd) = conf.then {
            log! { conf, verb =>
              "  running `{} {}`...", conf.emph(cmd), conf.emph(& final_file)
            }
            let status = Command::new(cmd).arg(&final_file).status().chain_err(|| {
                format!(
                    "while running `{} {}` (user-provided command)",
                    conf.emph(cmd),
                    conf.emph(&final_file)
                )
            })?;
            if !status.success() {
                bail!(format!(
                    "failure on user-provided command `{} {}`",
                    conf.emph(cmd),
                    conf.emph(&final_file)
                ))
            }
        }
    }
    Ok(())
}

/// Plot things.
pub fn work(conf: &PlotConf, kind: PlotKind) -> Res<()> {
    let final_file = match kind {
        PlotKind::Cumul { files } => {
            log! {
              conf =>
                "Generating cumulative plot for {} tools...", files.len()
            }
            cumul::work(conf, files).chain_err(|| "during cumulative plot generation")
        }
        PlotKind::Compare { file_1, file_2 } => {
            log! {
              conf => "Generating comparative scatterplot..." ; {
                log!{ conf, verb => "  loading data files..." }
              }
            }
            compare::work(conf, file_1, file_2).chain_err(|| "during comparative plot generation")
        }
    }?;

    if let Some(final_file) = final_file {
        run_stuff(conf, final_file)?
    }

    log! { conf => "Done" }

    Ok(())
}
