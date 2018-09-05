//! Run configuration loader.

use std::path::Path;

use common::*;

use super::serde_error;

/// Loads a toml run configuration file.
///
/// Returns
///
/// - the options declared in the file, if any
/// - the **active** tool configuration parsed
/// - the exit codes parsed
pub fn toml<P>(gconf: &GConf, file: P) -> Res<(Option<String>, NewToolConfs, NewCodes)>
where
    P: AsRef<Path>,
{
    let file = file.as_ref();

    let mut txt = String::new();
    File::open(file)?.read_to_string(&mut txt)?;

    let conf = match ::toml::from_str::<LRunConf>(&txt) {
        Ok(res) => res,
        Err(e) => bail!(serde_error(gconf, &e, &txt)),
    };

    conf.finalize(gconf).map(|res| res.destroy())
}

/// A run configuration.
#[derive(Debug, Clone)]
pub struct NewRunConf {
    /// Default options provided in the configuration file.
    options: Option<String>,
    /// Active tool configurations.
    tools: NewToolConfs,
    /// Validator exit codes.
    codes: NewCodes,
}

impl NewRunConf {
    /// Detructor.
    pub fn destroy(self) -> (Option<String>, NewToolConfs, NewCodes) {
        (self.options, self.tools, self.codes)
    }
}

/// A tool configuration.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NewToolConf {
    /// Name of the tool.
    ident: String,
    /// Command.
    cmd: String,
    /// Graph name.
    graph: String,
    /// Optional validator.
    validator: Option<String>,
}
impl ToToml for NewToolConf {}

impl NewToolConf {
    /// Tool's identifier.
    pub fn ident(&self) -> &str {
        &self.ident
    }

    /// Tool's validator.
    pub fn validator(&self) -> Option<&String> {
        self.validator.as_ref()
    }

    /// Tool's command.
    pub fn cmd(&self) -> &str {
        &self.cmd
    }

    /// Tool's graph name.
    pub fn graph_name(&self) -> &str {
        &self.graph
    }
}

/// Tool configurations.
#[derive(Debug, Clone)]
pub struct NewToolConfs {
    /// Actual tools.
    tools: ToolMap<NewToolConf>,
}

impl ::std::ops::Deref for NewToolConfs {
    type Target = ToolMap<NewToolConf>;
    fn deref(&self) -> &ToolMap<NewToolConf> {
        &self.tools
    }
}

impl NewToolConfs {
    /// Constructor.
    fn new() -> Self {
        NewToolConfs {
            tools: ToolMap::new(),
        }
    }

    /// Insert a new tool.
    fn insert(&mut self, gconf: &GConf, tool: NewToolConf) -> Res<()> {
        // Do we already know this tool?
        if self.tools.iter().any(|t| t.ident == tool.ident) {
            bail!("tool {} appears as active twice", gconf.bad(&tool.ident))
        }

        // Make sure there's no clash in graph names.
        for t in &self.tools {
            if tool.graph == t.graph {
                bail!(
                    "tools {} and {} have the same graph name {}",
                    gconf.sad(&tool.ident),
                    gconf.sad(&t.ident),
                    gconf.bad(&tool.graph)
                )
            }
        }

        self.tools.push(tool);

        Ok(())
    }
}

/// Exit code.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct NewCode {
    /// Identifier.
    pub name: String,
    /// Exit code.
    code: Validation,
    /// Graph name.
    pub graph: String,
}

/// A map from code values to exit codes.
#[derive(Debug, Clone, PartialEq)]
pub struct NewCodes {
    /// Error code and graph name.
    err: Option<(Validation, String)>,
    /// Timeout code and graph name.
    tmo: Option<(Validation, String)>,
    /// Regular codes.
    codes: Map<Validation, NewCode>,
}

impl NewCodes {
    /// Writes itself in TOML.
    pub fn toml_write<W>(&self, w: &mut W) -> Res<()>
    where
        W: Write,
    {
        if let Some((code, graph)) = self.err.as_ref() {
            writeln!(w, "error = {{ code = {}, graph = \"{}\" }}", code, graph)?
        }
        if let Some((code, graph)) = self.tmo.as_ref() {
            writeln!(w, "timeout = {{ code = {}, graph = \"{}\" }}", code, graph)?
        }

        for info in self.codes.values() {
            writeln!(
                w,
                "{} = {{ code = {}, graph = \"{}\" }}",
                info.name, info.code, info.graph
            )?
        }

        if !self.codes.is_empty() || self.err.is_some() || self.tmo.is_some() {
            writeln!(w)?
        }

        Ok(())
    }

    /// True if their are no success codes.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
    /// Number of success codes defined.
    pub fn len(&self) -> usize {
        self.codes.len()
    }

    /// Constructor.
    fn new() -> NewCodes {
        NewCodes {
            err: None,
            tmo: None,
            codes: Map::new(),
        }
    }

    /// Sets the error code and its graph name.
    fn set_err(&mut self, code: Validation, name: String) -> Res<()> {
        if self.err.is_some() {
            bail!("trying to set error code twice")
        }
        self.err = Some((code, name));
        Ok(())
    }

    /// Sets the error code and its graph name.
    fn set_tmo(&mut self, code: Validation, name: String) -> Res<()> {
        if self.tmo.is_some() {
            bail!("trying to set timeout code twice")
        }
        self.tmo = Some((code, name));
        Ok(())
    }

    /// Inserts a new code.
    fn insert(&mut self, gconf: &GConf, code: NewCode) -> Res<()> {
        if code.name == "error" {
            return self.set_err(code.code, code.graph);
        }
        if code.name == "timeout" {
            return self.set_tmo(code.code, code.graph);
        }

        // Make sure code name is not reserved.
        if ::consts::validator::reserved.contains(&code.name as &str) {
            bail!(
                "illegal validator code name {}: this name is reserved",
                gconf.bad(&code.name)
            )
        }

        let prev = self.codes.insert(code.code, code);

        if let Some(prev) = prev {
            bail!(
                "validator codes {} and {} have the same code {}",
                gconf.sad(&self.codes[&prev.code].name),
                gconf.sad(&prev.name),
                gconf.bad(&format!("{}", prev.code))
            )
        }

        Ok(())
    }

    /// Defines all the codes bash-style.
    pub fn bash_write<W>(&self, w: &mut W) -> Res<()>
    where
        W: Write,
    {
        if let Some((code, name)) = self.err.as_ref() {
            if name != "error" {
                writeln!(w, "# {}", name)?
            }
            writeln!(w, "error=\"{}\"", code)?
        }
        if let Some((code, name)) = self.tmo.as_ref() {
            if name != "timeout" {
                writeln!(w, "# {}", name)?
            }
            writeln!(w, "timeout=\"{}\"", code)?
        }

        for (code, info) in &self.codes {
            if info.graph != info.name {
                writeln!(w, "# {}", info.graph)?
            }
            writeln!(w, "{}=\"{}\"", info.name, code)?
        }

        Ok(())
    }

    /// True if the code is register as a success.
    pub fn is_succ(&self, status: ExitStatus) -> bool {
        if let Some(code) = status.code() {
            self.codes.contains_key(&code)
        } else {
            false
        }
    }

    /// Retrieves the info of an exit code.
    pub fn get(&self, code: Validation) -> Option<&NewCode> {
        self.codes.get(&code)
    }
}

/// A configuration right after loading.
#[derive(Debug, Serialize, Deserialize)]
pub struct LRunConf {
    /// Default options provided in the configuration file.
    options: Option<String>,
    /// Active tools.
    run: Vec<String>,
    /// Actual tool configurations.
    tools: StrMap<LTool>,
    /// Validator exit codes.
    codes: StrMap<LCode>,
    /// Validators.
    validators: StrMap<String>,
}

/// A tool configuration right after loading.
#[derive(Debug, Serialize, Deserialize)]
pub struct LTool {
    /// Command.
    cmd: String,
    /// Graph name.
    graph: Option<String>,
    /// Optional validator.
    validator: Option<String>,
}

/// Validator exit code right after loading.
#[derive(Debug, Serialize, Deserialize)]
pub struct LCode {
    /// Exit code.
    code: i64,
    /// Graph name.
    graph: Option<String>,
}

/// Creates a `NewCodes` from some `LCode`s.
pub fn new_codes(gconf: &GConf, map: StrMap<LCode>) -> Res<NewCodes> {
    let mut codes = NewCodes::new();

    for (name, code) in map {
        // Create graph name.
        let graph = code.graph.unwrap_or_else(|| name.clone());

        // Transcribe to i32.
        use std::str::FromStr;
        let val_code = match Validation::from_str(&format!("{}", code.code)) {
            Ok(code) => code,
            Err(e) => bail!("exit code for {} is not valid: {}", gconf.bad(&name), e),
        };

        // Build exit code structure.
        let code = NewCode {
            name: name.clone(),
            code: val_code,
            graph,
        };

        codes.insert(gconf, code)?
    }

    Ok(codes)
}

impl LRunConf {
    /// Checks and finalizes an `LRunConf`.
    fn finalize(mut self, gconf: &GConf) -> Res<NewRunConf> {
        let options = self.options;

        // This will only store active tools.
        let mut tools = NewToolConfs::new();

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
            let conf = NewToolConf {
                ident: tool,
                cmd: tool_conf.cmd,
                graph,
                validator,
            };

            tools.insert(gconf, conf)?
        }

        // Work on exit codes.
        let codes = new_codes(gconf, self.codes)?;

        Ok(NewRunConf {
            options,
            tools,
            codes,
        })
    }
}
