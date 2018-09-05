//! Data loading.

use std::path::Path;

use common::{
    res::{RunRes, ToolRes},
    *,
};

mod res;
mod run;

/// Map from strings to something.
type StrMap<T> = Map<String, T>;

/// Run configuration loader.
///
/// Returns
///
/// - the options declared in the file, if any
/// - the **active** tool configuration parsed
/// - the exit codes parsed
pub fn run<P, Conf>(gconf: &Conf, file: P) -> Res<(Option<String>, ToolInfos, CodeInfos)>
where
    P: AsRef<Path>,
    Conf: GConfExt,
{
    run::toml(gconf, file)
}

/// Result data file loader.
pub fn res<P, Conf>(gconf: &Conf, run_res: &mut RunRes, file: P) -> Res<ToolRes>
where
    P: AsRef<Path>,
    Conf: GConfExt,
{
    res::toml(gconf, run_res, file)
}

/// Handles a serde load error.
fn serde_error<Conf>(gconf: &Conf, e: &::toml::de::Error, txt: &str) -> Error
where
    Conf: GConfExt,
{
    let mut error = format!("{}", e);

    if let Some((l, c)) = e.line_col() {
        for (cnt, line) in txt.lines().enumerate() {
            if cnt == l {
                let line_count = format!("{}", l + 1);
                error += &format!("\n{} |", " ".repeat(line_count.len()));
                error += &format!(
                    "\n
                    {} | {}",
                    line_count, line
                );
                error += &format!(
                    "{} | {}{}",
                    " ".repeat(line_count.len()),
                    " ".repeat(c),
                    gconf.bad("^")
                );
                break;
            }
        }
    }

    error.into()
}

/// Validator exit code right after loading.
///
/// Used by `load::run` and `load::res`.
#[derive(Debug, Serialize, Deserialize)]
struct LCodeInfo {
    /// Exit code.
    code: i64,
    /// Graph name.
    graph: Option<String>,
}

/// Creates a `CodeInfos` from some `LCodeInfo`s.
///
/// Used by `load::run` and `load::res`.
fn new_codes<Conf>(gconf: &Conf, map: StrMap<LCodeInfo>) -> Res<CodeInfos>
where
    Conf: GConfExt,
{
    let mut codes = CodeInfos::new();

    for (name, code) in map {
        // Create graph name.
        let graph = code.graph.unwrap_or_else(|| name.clone());

        // Transcribe to i32.
        use std::str::FromStr;
        let val_code = match Code::from_str(&format!("{}", code.code)) {
            Ok(code) => code,
            Err(e) => bail!("exit code for {} is not valid: {}", gconf.bad(&name), e),
        };

        // Build exit code structure.
        let code = CodeInfo::new(name.clone(), val_code, graph);

        codes.insert(gconf, code)?
    }

    Ok(codes)
}
