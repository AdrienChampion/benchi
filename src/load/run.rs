//! Run configuration loader.

use std::path::Path;

use common::*;

use super::{new_codes, serde_error, LCodeInfo, StrMap};

/// Loads a toml run configuration file.
///
/// Returns
///
/// - the options declared in the file, if any
/// - the **active** tool configuration parsed
/// - the exit codes parsed
pub fn toml<P, Conf>(gconf: &Conf, file: P) -> Res<(Option<String>, ToolInfos, CodeInfos)>
where
    P: AsRef<Path>,
    Conf: GConfExt,
{
    let file = file.as_ref();

    let mut txt = String::new();
    File::open(file)?.read_to_string(&mut txt)?;

    let conf = match ::toml::from_str::<LRunConf>(&txt) {
        Ok(res) => res,
        Err(e) => bail!(serde_error(gconf, &e, &txt)),
    };

    conf.finalize(gconf)
}

/// A run configuration right after loading.
#[derive(Debug, Serialize, Deserialize)]
struct LRunConf {
    /// Default options provided in the configuration file.
    options: Option<String>,
    /// Active tools.
    run: Vec<String>,
    /// Actual tool configurations.
    tools: StrMap<LToolInfo>,
    /// Validator exit codes.
    codes: StrMap<LCodeInfo>,
    /// Validators.
    validators: StrMap<String>,
}

/// A tool configuration right after loading.
#[derive(Debug, Serialize, Deserialize)]
struct LToolInfo {
    /// Command.
    cmd: String,
    /// Graph name.
    graph: Option<String>,
    /// Optional validator.
    validator: Option<String>,
}

impl LRunConf {
    /// Checks and finalizes an `LRunConf`.
    fn finalize<Conf>(mut self, gconf: &Conf) -> Res<(Option<String>, ToolInfos, CodeInfos)>
    where
        Conf: GConfExt,
    {
        let options = self.options;

        // This will only store active tools.
        let mut tools = ToolInfos::new();

        for tool in self.run {
            // Retrieve tool configuration.
            let tool_conf = if let Some(conf) = self.tools.remove(&tool) {
                conf
            } else {
                bail!(
                    "unknown tool {} in the list of active tools",
                    gconf.bad(&tool)
                )
            };

            // Retrieve validator if any.
            let validator = if let Some(v) = tool_conf.validator {
                if let Some(validator) = self.validators.get(&v) {
                    Some(validator.clone())
                } else {
                    bail!(
                        "tool {} uses unknown validator {}",
                        gconf.sad(&tool),
                        gconf.bad(&v)
                    )
                }
            } else {
                None
            };

            let graph = tool_conf.graph.unwrap_or_else(|| tool.clone());

            // Build actual tool configuration.
            let mut conf = ToolInfo::new(tool, tool_conf.cmd, graph);
            if let Some(validator) = validator {
                conf.set_validator(validator)
            }

            tools.insert(gconf, conf)?
        }

        // Work on exit codes.
        let codes = new_codes(gconf, self.codes)?;

        Ok((options, tools, codes))
    }
}
