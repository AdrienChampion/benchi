//! Validator-related types.

use common::*;

/// Exit code.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CodeInfo {
    /// Identifier.
    pub name: String,
    /// Exit code.
    code: Code,
    /// Graph name.
    pub graph: String,
}

impl CodeInfo {
    /// Constructor.
    pub fn new(name: String, code: Code, graph: String) -> Self {
        CodeInfo { name, code, graph }
    }
}

/// A map from code values to exit codes.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct CodeInfos {
    /// Error code and graph name.
    err: Option<(Code, String)>,
    /// Timeout code and graph name.
    tmo: Option<(Code, String)>,
    /// Regular codes.
    codes: CodeMap<CodeInfo>,
}

impl CodeInfos {
    /// Constructor.
    pub fn new() -> CodeInfos {
        CodeInfos {
            err: None,
            tmo: None,
            codes: CodeMap::new(),
        }
    }

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

    /// Sets the error code and its graph name.
    pub fn set_err(&mut self, code: Code, name: String) -> Res<()> {
        if self.err.is_some() {
            bail!("trying to set error code twice")
        }
        self.err = Some((code, name));
        Ok(())
    }

    /// Sets the error code and its graph name.
    pub fn set_tmo(&mut self, code: Code, name: String) -> Res<()> {
        if self.tmo.is_some() {
            bail!("trying to set timeout code twice")
        }
        self.tmo = Some((code, name));
        Ok(())
    }

    /// Inserts a new code.
    pub fn insert<Conf>(&mut self, gconf: &Conf, code: CodeInfo) -> Res<()>
    where
        Conf: GConfExt,
    {
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
    pub fn get(&self, code: Code) -> Option<&CodeInfo> {
        self.codes.get(&code)
    }
}

/// Grants access to validation codes.
pub trait CodesExt {
    /// Validation codes.
    fn codes(&self) -> &CodeInfos;
}
