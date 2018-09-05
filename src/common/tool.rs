//! Tool-related types.

use common::*;

/// A tool configuration.
///
/// Stores the
///
/// - identifier of the tool: should be a legal file name, and unique among other tools
/// - command use to run the tool
/// - name used for the graphs
/// - an optional validator (bash script)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolInfo {
    /// Name of the tool.
    ident: String,
    /// Command.
    cmd: String,
    /// Graph name.
    graph: String,
    /// Optional validator.
    validator: Option<String>,
}
impl ToToml for ToolInfo {}

impl ToolInfo {
    /// Constructor.
    pub fn new(ident: String, cmd: String, graph: String) -> Self {
        ToolInfo {
            ident,
            cmd,
            graph,
            validator: None,
        }
    }

    /// Sets the validator.
    pub fn set_validator(&mut self, validator: String) {
        self.validator = Some(validator)
    }

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

/// Some tool configurations.
///
/// Simply writes a `ToolMap<ToolInfo>`.
#[derive(Debug, Clone, Default)]
pub struct ToolInfos {
    /// Actual tools.
    tools: ToolMap<ToolInfo>,
}

impl ::std::ops::Deref for ToolInfos {
    type Target = ToolMap<ToolInfo>;
    fn deref(&self) -> &ToolMap<ToolInfo> {
        &self.tools
    }
}

impl ToolInfos {
    /// Constructor.
    pub fn new() -> Self {
        ToolInfos {
            tools: ToolMap::new(),
        }
    }

    /// Insert a new tool.
    ///
    /// Returns an error if two tools have the same identifier, graph name or command.
    pub fn insert<Conf>(&mut self, gconf: &Conf, tool: ToolInfo) -> Res<()>
    where
        Conf: GConfExt,
    {
        // Do we already know this tool?
        if self.tools.iter().any(|t| t.ident == tool.ident) {
            bail!("tool {} appears as active twice", gconf.bad(&tool.ident))
        }

        // Make sure there's no clash in graph names or commands.
        for t in &self.tools {
            if tool.graph == t.graph {
                bail!(
                    "tools {} and {} have the same graph name {}",
                    gconf.sad(&tool.ident),
                    gconf.sad(&t.ident),
                    gconf.bad(&tool.graph)
                )
            }
            if tool.cmd == t.cmd {
                bail!(
                    "tools {} and {} have the same command {}",
                    gconf.sad(&tool.ident),
                    gconf.sad(&t.ident),
                    gconf.bad(&tool.cmd)
                )
            }
        }

        self.tools.push(tool);

        Ok(())
    }
}

/// Tool statistics.
#[derive(Debug, Clone, Copy, Default)]
pub struct ToolStat {
    /// Total runtime.
    pub totl: Duration,
    /// Number of successes.
    pub slvd: usize,
    /// Number of errors.
    pub errs: usize,
    /// Number of timeouts.
    pub tmos: usize,
}

impl ToolStat {
    /// Constructor.
    pub fn new() -> Self {
        ToolStat {
            totl: Duration::new(0, 0),
            slvd: 0,
            errs: 0,
            tmos: 0,
        }
    }

    /// Registers a solved benchmark.
    pub fn slv(&mut self, time: Duration) {
        self.totl += time;
        self.slvd += 1
    }

    /// Registers an error.
    pub fn err(&mut self) {
        self.errs += 1
    }

    /// Registers a timeout.
    pub fn tmo(&mut self) {
        self.tmos += 1
    }
}

/// Tool statistics for all tools.
#[derive(Debug, Clone, Default)]
pub struct ToolStats {
    /// Map from tools to stats.
    map: ToolMap<ToolStat>,
}

impl ToolStats {
    /// Constructor.
    pub fn new(size: usize) -> Self {
        ToolStats {
            map: vec![ToolStat::new(); size].into(),
        }
    }
}

impl ::std::ops::Deref for ToolStats {
    type Target = ToolMap<ToolStat>;
    fn deref(&self) -> &ToolMap<ToolStat> {
        &self.map
    }
}

impl ::std::ops::DerefMut for ToolStats {
    fn deref_mut(&mut self) -> &mut ToolMap<ToolStat> {
        &mut self.map
    }
}

struct MaxWidth {
    name: usize,
    runtime: usize,
    average: usize,
    solved: usize,
    errs: usize,
    tmos: usize,
}

macro_rules! max_update {
    ($max:expr => $field:ident $($tail:tt)*) => ({
        if $field.len() > $max.$field {
            $max.$field = $field.len()
        }
        max_update!($max => $($tail)*)
    }) ;

    ($max:expr =>) => (()) ;
}

struct Row {
    name: String,
    runtime: String,
    average: String,
    solved: String,
    errs: String,
    tmos: String,
}
impl Row {
    fn new<S>(max: &mut MaxWidth, name: S, stats: ToolStat) -> Self
    where
        S: Into<String>,
    {
        let name = name.into();
        let runtime = stats.totl.as_sec_str();
        let average = if stats.slvd == 0 {
            "--".to_string()
        } else {
            (stats.totl / stats.slvd as u32).as_sec_str()
        };
        let solved = format!("{}", stats.slvd);
        let errs = format!("{}", stats.errs);
        let tmos = format!("{}", stats.tmos);

        max_update! {
            max => name runtime average solved errs tmos
        }
        Row {
            name,
            runtime,
            average,
            solved,
            errs,
            tmos,
        }
    }

    fn header() -> (Row, MaxWidth) {
        let row = Row {
            name: "tool".into(),
            runtime: "runtime (s)".into(),
            average: "average (s)".into(),
            solved: "solved".into(),
            errs: "erros".into(),
            tmos: "timeouts".into(),
        };
        let width = MaxWidth {
            name: row.name.len(),
            runtime: row.runtime.len(),
            average: row.average.len(),
            solved: row.solved.len(),
            errs: row.errs.len(),
            tmos: row.tmos.len(),
        };
        (row, width)
    }

    fn push_to<Conf>(&self, conf: &Conf, s: &mut String, max: &MaxWidth)
    where
        Conf: GConfExt,
    {
        s.push_str(" ");
        s.push_str(&" ".repeat(max.name - self.name.len()));
        s.push_str(&conf.emph(&self.name));

        s.push_str(" || ");

        s.push_str(&" ".repeat(max.solved - self.solved.len()));
        s.push_str(&conf.happy(&self.solved));

        s.push_str(" | ");

        s.push_str(&" ".repeat(max.runtime - self.runtime.len()));
        s.push_str(&self.runtime);

        s.push_str(" | ");

        s.push_str(&" ".repeat(max.average - self.average.len()));
        s.push_str(&self.average);

        s.push_str(" | ");

        s.push_str(&" ".repeat(max.tmos - self.tmos.len()));
        if &self.tmos == "0" {
            s.push_str(&self.tmos);
        } else {
            s.push_str(&conf.sad(&self.tmos));
        }

        s.push_str(" | ");

        s.push_str(&" ".repeat(max.errs - self.errs.len()));
        if &self.errs == "0" {
            s.push_str(&self.errs);
        } else {
            s.push_str(&conf.bad(&self.errs));
        }
        s.push_str(" ")
    }

    fn push_header_sep(s: &mut String, max: &MaxWidth) {
        s.push_str(&format!(
            "={0:=>1$}=||={2:=>3$}=|={4:=>5$}=|={6:=>7$}=|={8:=>9$}=|={10:=>11$}=",
            "",
            max.name,
            "",
            max.solved,
            "",
            max.runtime,
            "",
            max.average,
            "",
            max.tmos,
            "",
            max.errs,
        ))
    }
}

impl ToolStats {
    /// Returns a pretty multi-line representation of the stats.
    pub fn to_string_pretty<Conf>(&self, conf: &Conf, instance: &Instance) -> String
    where
        Conf: GConfExt,
    {
        let mut res = String::new();

        let (header, mut max) = Row::header();
        let mut rows = vec![];

        for tool in instance.tools() {
            rows.push(Row::new(&mut max, instance[tool].ident(), self[tool]))
        }

        header.push_to(conf, &mut res, &max);
        res += "\n";
        Row::push_header_sep(&mut res, &max);
        for row in rows {
            res += "\n";
            row.push_to(conf, &mut res, &max)
        }

        res
    }
}
