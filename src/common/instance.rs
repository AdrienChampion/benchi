//! An instance stores the tool infos and the paths to the benchmarks.

use common::*;

/// Store the tools and the path to the benchmarks. Should **always** be
/// immutable.
pub struct Instance {
    /// The tools.
    tools: ToolInfos,
    /// The benchmarks.
    benchs: BenchMap<String>,
}
unsafe impl Sync for Instance {}
impl Instance {
    /// Creates an instance.
    #[inline]
    pub fn new(tools: ToolInfos, benchs: BenchMap<String>) -> Self {
        Instance { tools, benchs }
    }
    /// Iterator over the tool indices of the instance.
    #[inline]
    pub fn tools(&self) -> ToolRng {
        ToolRng::zero_to(self.tools.len())
    }
    /// Iterator over the bench indices of the instance.
    #[inline]
    pub fn benchs(&self) -> BenchRng {
        BenchRng::zero_to(self.benchs.len())
    }
    /// Number of tools.
    #[inline]
    pub fn tool_len(&self) -> usize {
        self.tools.len()
    }
    /// Number of benchs.
    #[inline]
    pub fn bench_len(&self) -> usize {
        self.benchs.len()
    }
    /// String of a bench.
    #[inline]
    pub fn str_of_bench(&self, index: BenchIdx) -> &str {
        &self.benchs[index]
    }

    /// Safe name for a bench, unique and can be used as file id.
    ///
    /// ```rust
    /// # use benchi::common::{ Instance, ToolInfos, BenchMap } ;
    /// let benchs: BenchMap<_> = vec![ "unused".to_string() ; 150 ].into() ;
    /// let tools = ToolInfos::new() ;
    /// let instance = Instance::new(tools, benchs) ;
    /// assert_eq! { & instance.safe_name_for_bench(23.into()), "023" }
    ///
    /// let benchs: BenchMap<_> = vec![ "unused".to_string() ; 1500 ].into() ;
    /// let tools = ToolInfos::new() ;
    /// let instance = Instance::new(tools, benchs) ;
    /// assert_eq! { & instance.safe_name_for_bench(23.into()), "0023" }
    /// ```
    #[inline]
    pub fn safe_name_for_bench(&self, index: BenchIdx) -> String {
        format!("{:0>1$}", *index, format!("{}", self.benchs.len()).len())
    }

    /// Path to the directory of a tool.
    #[inline]
    pub fn path_of_tool(&self, conf: &Arc<RunConf>, tool: ToolIdx) -> PathBuf {
        let mut path = PathBuf::from(&conf.out_dir);
        path.push(&self[tool].ident());
        path
    }

    /// Stderr directory path of a tool.
    #[inline]
    pub fn err_path_of_tool(&self, conf: &Arc<RunConf>, tool: ToolIdx) -> PathBuf {
        let mut path = self.path_of_tool(conf, tool);
        path.push("err");
        path
    }
    /// Stdout directory path of a tool.
    #[inline]
    pub fn out_path_of_tool(&self, conf: &Arc<RunConf>, tool: ToolIdx) -> PathBuf {
        let mut path = self.path_of_tool(conf, tool);
        path.push("out");
        path
    }

    /// Path to the stderr of a tool on a bench.
    #[inline]
    pub fn err_path_of(&self, conf: &Arc<RunConf>, tool: ToolIdx, bench: BenchIdx) -> PathBuf {
        let mut path = self.err_path_of_tool(conf, tool);
        path.push(self.safe_name_for_bench(bench));
        path
    }

    /// Path to the stdout of a tool on a bench.
    #[inline]
    pub fn out_path_of(&self, conf: &Arc<RunConf>, tool: ToolIdx, bench: BenchIdx) -> PathBuf {
        let mut path = self.out_path_of_tool(conf, tool);
        path.push(self.safe_name_for_bench(bench));
        path
    }

    /// Creates the error path of a tool.
    #[inline]
    pub fn mk_err_dir(&self, conf: &Arc<RunConf>, tool: ToolIdx) -> Res<()> {
        mk_dir(self.err_path_of_tool(conf, tool)).chain_err(|| {
            format!(
                "while creating err directory for {}",
                conf.emph(&self[tool].ident())
            )
        })
    }
    /// Creates the output path of a tool.
    #[inline]
    pub fn mk_out_dir(&self, conf: &Arc<RunConf>, tool: ToolIdx) -> Res<()> {
        mk_dir(self.out_path_of_tool(conf, tool)).chain_err(|| {
            format!(
                "while creating output directory for {}",
                conf.emph(&self[tool].ident())
            )
        })
    }
    /// Initializes the data file and the validator for some tool.
    #[inline]
    pub fn init_data_file_and_validator(&self, conf: &Arc<RunConf>, tool: ToolIdx) -> Res<File> {
        self.init_validator(conf, tool)?;
        let mut path = PathBuf::new();
        path.push(&conf.out_dir);
        path.push(&self[tool].ident());
        path.set_extension("toml");
        let mut tool_file = conf.open_file_writer(path.as_path()).chain_err(|| {
            format!(
                "while creating file `{}`, data file for `{}`",
                conf.sad(path.to_str().expect("non-UTF8 path")),
                conf.emph(&self[tool].ident())
            )
        })?;

        writeln!(tool_file, "timeout = \"{}\"", conf.timeout.as_sec_str())?;
        writeln!(tool_file)?;

        writeln!(tool_file, "[tool]")?;
        writeln!(tool_file, "{}", self[tool].to_toml_str()?)?;

        writeln!(tool_file, "[codes]")?;
        conf.codes().toml_write(&mut tool_file)?;

        writeln!(tool_file, "[data]")?;

        Ok(tool_file)
    }

    /// Initializes the validator for some tool.
    #[inline]
    pub fn init_validator(&self, conf: &Arc<RunConf>, tool: ToolIdx) -> Res<()> {
        use std::os::unix::fs::PermissionsExt;
        if let Some(path) = conf.validator_path_of(&self[tool]) {
            let mut file = conf
                .open_file_writer_exe(path.as_path(), true)
                .chain_err(|| {
                    format!(
                        "while creating validator for `{}`",
                        conf.sad(&self[tool].ident())
                    )
                })?;
            file.write(::consts::validator::pref.as_bytes())
                .chain_err(|| {
                    format!(
                        "while while writing to validator file for `{}`",
                        conf.sad(&self[tool].ident())
                    )
                })?;

            conf.codes().bash_write(&mut file)?;

            if let Some(s) = self[tool].validator() {
                file.write(s.as_bytes()).chain_err(|| {
                    format!(
                        "while while writing to validator file for `{}`",
                        conf.sad(&self[tool].ident())
                    )
                })?;
                file.metadata()
                    .chain_err(|| "could chmod validator file to executable")?
                    .permissions()
                    .set_mode(0o744)
            }
        }
        Ok(())
    }

    /// Initializes everything for the tools. Returns the data files for all the
    /// tools. Runs the input function on each `ToolConf` in a fold manner.
    ///
    /// - tool dir, err dir, out dir
    /// - data file
    /// - validators if any
    pub fn init_tools<T, F>(
        &self,
        conf: &Arc<RunConf>,
        init: T,
        fold_fun: F,
    ) -> Res<(ToolMap<File>, T)>
    where
        F: Fn(&mut T, &ToolInfo),
    {
        let mut tool_files = ToolMap::with_capacity(self.tool_len());
        let mut fold_data = init;
        // Tool init: output dirs, validators, data file, mastre data init.
        for tool in self.tools() {
            // Output dirs.
            self.mk_err_dir(&conf, tool)?;
            if conf.log_stdout {
                self.mk_out_dir(&conf, tool)?
            };

            // Data file.
            let tool_file = self.init_data_file_and_validator(&conf, tool)?;
            tool_files.push(tool_file);

            // Folding.
            fold_fun(&mut fold_data, &self[tool])
        }

        Ok((tool_files, fold_data))
    }

    /// Checks if a bench index is the last one.
    #[inline]
    pub fn is_last_bench(&self, bench: BenchIdx) -> bool {
        *bench + 1 >= self.benchs.len()
    }

    /// If any, runs the validator for a tool on some benchmark.
    pub fn validate(
        &self,
        conf: &Arc<RunConf>,
        tool: ToolIdx,
        bench: BenchIdx,
        status: ExitStatus,
    ) -> Res<Option<ExitStatus>> {
        use wait_timeout::ChildExt;

        if let Some(path) = conf.validator_path_of(&self[tool]) {
            use std::process::Stdio;
            let status = if let Some(code) = status.code() {
                code
            } else {
                bail!("could not retrieve exit status")
            };
            let out_path = self.out_path_of(conf, tool, bench);
            let err_path = self.err_path_of(conf, tool, bench);
            let mut kid = Command::new(path.as_os_str())
                .stdin(Stdio::null())
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .arg(&self[bench])
                .arg(&format!("{}", status))
                .arg(out_path)
                .arg(err_path)
                .spawn()
                .chain_err(|| {
                    format!(
                        "while running validator for `{}` on benchmark `{}`",
                        conf.sad(&self[tool].ident()),
                        conf.sad(format!("{}", bench))
                    )
                })?;
            let res = kid.wait_timeout(conf.timeout)?;
            if res.is_none() {
                kid.kill()?;
            }

            Ok(res)
        } else {
            Ok(None)
        }
    }
}

impl Index<BenchIdx> for Instance {
    type Output = String;
    #[inline]
    fn index(&self, index: BenchIdx) -> &String {
        &self.benchs[index]
    }
}

impl Index<ToolIdx> for Instance {
    type Output = ToolInfo;
    #[inline]
    fn index(&self, index: ToolIdx) -> &ToolInfo {
        &self.tools[index]
    }
}
