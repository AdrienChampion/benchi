/*! Runners for the different level of running.

The master dispatches the benchmarks over `BenchRun`s. They in turn dispatch
tool to run to `ToolRun`s.

# TODO

- naming convention is retarded, change it
- make the bench run say it's waiting only if it has at least one waiting tool
  run
*/

use errors::* ;
use common::* ;
use common::run::* ;

pub mod utils ;
use self::utils::* ;

/// Lowest level in the hierarchy, runs a tool on a benchmark.
pub struct ToolRun {
  /// The configuration.
  pub conf: Arc<RunConf>,
  /// The instance.
  pub instance: Arc<Instance>,
  /// Index of this tool run for the bench run responsible for it.
  index: usize,
  /// Sender to master.
  master: Sender<RunRes>,
  /// Sender to bench run.
  bench_run: Sender< usize >,
  /// Receiver from bench run.
  from_bench_run: Receiver< (ToolIdx, BenchIndex) >
}
unsafe impl Send for ToolRun {}
impl ToolRun {
  /// Creates (but does not run) a tool run.
  #[inline]
  fn mk(
    conf: Arc<RunConf>, instance: Arc<Instance>, index: usize,
    master: Sender<RunRes>,
    bench_run: Sender< usize >,
    from_bench_run: Receiver< (ToolIdx, BenchIndex) >,
  ) -> Self {
    ToolRun {
      conf, instance, index, master, bench_run, from_bench_run
    }
  }

  /// Launches the tool run listen/run loop.
  pub fn launch(self) {

    'work: loop {

      // Notify bench run we're waiting.
      if self.bench_run.send(self.index).is_err() {
        // Disconnected, can happen if we're done. (Maybe? Nothing we can
        // do anyways.)
        break 'work
      }

      // Waiting for something to do.
      match self.from_bench_run.recv() {
        Ok( (tool, bench) ) => {
          // Work...
          let res = match self.run(tool, bench) {
            Ok(res) => res,
            Err(e) => BenchRes::BenchiError(e),
          } ;
          // Delete error file if empty.
          let path = self.instance.err_path_of(
            & self.conf, tool, bench
          ) ;
          if let Ok(data) = path.as_path().metadata() {
            if data.len() == 0 {
              let _ = ::std::fs::remove_file(path) ;
            }
          }
          // Send to master.
          if self.master.send(
            RunRes { tool, bench, res }
          ).is_err() {
            // Disconnected, should not happen.
            break 'work
          }
        },
        // Disconnected, we're done...
        Err( RecvError ) => break 'work,
      }

    }
  }

  /// Runs a tool on a bench.
  fn run(
    & self, tool_idx: ToolIdx, bench_idx: BenchIndex
  ) -> Res<BenchRes> {
    let bench = & self.instance[bench_idx] ;
    let kid_cmd = & self.instance[tool_idx].cmd() ;

    assert!( ! kid_cmd.is_empty() ) ;

    let mut cmd = Command::new("timeout") ;
    cmd.arg(& format!("{}", self.conf.timeout.as_secs())).arg(& kid_cmd) ;
    let mut cmd_str = kid_cmd.to_string() ;
    cmd.arg(bench) ;
    cmd_str.push(' ') ;
    cmd_str.push_str(bench) ;
    let (stdout_file, stderr_file) = (
      if self.conf.log_stdout {
        Some( self.open_stdout_file_for(tool_idx, bench_idx)? )
      } else {
        None
      },
      self.open_stderr_file_for(tool_idx, bench_idx)?
    ) ;
    set_pipes(& mut cmd, stdout_file, stderr_file) ;

    let (mut kid, start) = (
      cmd.spawn().chain_err(
        || format!("while running command `{}`", cmd_str)
      ) ?, Instant::now()
    ) ;

    let status = kid.wait().chain_err(
      || format!("while waiting for `{}`", cmd_str)
    ) ? ;
    let time = Instant::now() - start ;

    if time >= self.conf.timeout
    || Some(124) == status.code()
    || Some(137) == status.code() {
      Ok( BenchRes::Timeout(status) )
    } else {
      let status = if let Some(vald_status) = self.instance.validate(
        & self.conf, tool_idx, bench_idx, status
      ) ? {
        vald_status
      } else {
        status
      } ;

      if self.conf.codes().is_succ(status) {
        Ok( BenchRes::Success(time, status) )
      } else {
        Ok( BenchRes::Error(time, status) )
      }
    }
  }

  /// Opens the stderr file for a bench for a tool.
  fn open_stderr_file_for(
    & self, tool: ToolIdx, bench: BenchIndex
  ) -> Res<File> {
    let path = self.instance.err_path_of(& self.conf, tool, bench) ;
    self.conf.open_file_writer(path).chain_err(
      || format!(
        "while opening error file to write stderr \
        of {} running on {}",
        self.conf.emph( & self.instance[tool].ident() ),
        self.conf.emph( self.instance.str_of_bench(bench) )
      )
    )
  }
  /// Opens the stdout file for a bench for a tool.
  fn open_stdout_file_for(
    & self, tool: ToolIdx, bench: BenchIndex
  ) -> Res<File> {
    let path = self.instance.out_path_of(& self.conf, tool, bench) ;
    self.conf.open_file_writer(& path).chain_err(
      || format!(
        "while opening file `{}` to write stdout \
        of {} running on {}",
        path.to_str().map(|s| s.to_string()).unwrap_or_else(
          || format!("{:?}", path)
        ),
        self.conf.emph( & self.instance[tool].ident() ),
        self.conf.emph( self.instance.str_of_bench(bench) )
      )
    )
  }
}



/// Intermediary level in the hierarchy, is in charge of running some tools on
/// a particular benchmark.
pub struct BenchRun {
  /// The configuration.
  pub conf: Arc<RunConf>,
  /// The instance.
  pub instance: Arc<Instance>,
  /// The index of this bench run for the master.
  pub index: usize,
  /// Sender to master.
  master: Sender< Res<usize> >,
  /// Receiver from master.
  from_master: Receiver< BenchIndex >,
  /// Senders to tool runs.
  tool_runs: Vec< Sender< (ToolIdx, BenchIndex) > >,
  /// Receiver from tool runs.
  from_tool_runs: Receiver< usize >
}
impl BenchRun {
  /// Creates (but does not run) a tool run.
  ///
  /// `tool_to_master` will be given to the `ToolRun`s spawned.
  #[inline]
  fn mk(
    conf: Arc<RunConf>, instance: Arc<Instance>, index: usize,
    master: Sender< Res<usize> >,
    from_master: Receiver< BenchIndex >,
    tool_to_master: & Sender<RunRes>,
  ) -> Self {
    let mut tool_runs = Vec::with_capacity( conf.tool_par ) ;
    // Channel to `self`, shared by all tool runs.
    let (t2b_s, from_tool_runs) = tool_to_bench_channel() ;

    // Spawn tool runs.
    for index in 0..conf.tool_par {
      // Bench to tool personal channel.
      let (b2t_s, b2t_r) = bench_to_tool_channel() ;
      // Remember sender.
      tool_runs.push(b2t_s) ;
      // Initialize tool run.
      let tool_run = ToolRun::mk(
        conf.clone(), instance.clone(), index,
        tool_to_master.clone(), t2b_s.clone(), b2t_r
      ) ;
      // Launch.
      spawn( move || tool_run.launch() ) ;
      ()
    }

    BenchRun {
      conf, instance, index,
      master, from_master, tool_runs, from_tool_runs
    }
  }

  /// Launches the listen/dispatch loop.
  pub fn launch(mut self) {
    'work: loop {

      // Notify master we're waiting.
      if self.master.send( Ok(self.index) ).is_err() {
        // Disconnected, can happen if we're done. (Maybe? Nothing we can
        // do anyways.)
        break 'work
      }

      // Waiting for something to do.
      match self.from_master.recv() {
        Ok( bench ) => {
          // Work...
          if let Err(e) = self.run(bench) {
            // Send to master.
            if self.master.send( Err(e) ).is_err() {
              // Disconnected, should not happen.
              break 'work
            }
          }
        },
        // Disconnected, we're done...
        Err( RecvError ) => break 'work,
      }

    }
  }


  /// Dispatches all the tools on a bench.
  pub fn run(
    & mut self, bench: BenchIndex
  ) -> Res<()> {
    'dispatch_tool: for tool in self.instance.tools() {

      'find_idle: loop {
        match self.from_tool_runs.recv() {
          // Someone's ready.
          Ok(index) => {
            if let Ok(()) = self.tool_runs[index].send(
              (tool, bench)
            ) {
              continue 'dispatch_tool
            } else {
              // Disconnected, keep going.
              continue 'find_idle
            }
          },
          // Disconnected, no more tool runs alive.
          Err(RecvError) => bail!(
            format!(
              "Bench run, on bench {}: no more tool runs alive",
              self.conf.emph( self.instance.str_of_bench(bench) )
            )
          ),
        }
      }

    }
    Ok(())
  }
}


/// Master, iterates dispatches benchmarks.
pub struct Master {
  /// The configuration.
  pub conf: Arc<RunConf>,
  /// The instance.
  pub instance: Arc<Instance>,
  /// Senders to bench runs (intermediary level).
  bench_runs: Vec< Sender<BenchIndex> >,
  /// Receiver from bench runs (intermediary level).
  from_bench_runs: Receiver< Res<usize> >,
  /// Receiver from tool runs (lowest level).
  from_tool_runs: Receiver<RunRes>,
  /// Files in write mode to write the results to.
  tool_files: ToolMap<File>,
  /// Progress bar.
  pbar: Option< ProgressBar< ::std::io::Stdout > >,
  /// Number of errors.
  pub errors: usize,
  /// Number of timeouts.
  pub timeouts: usize,
  /// Average runtime (does not include timeouts and errors).
  ///
  /// Second element is the number of benchmarks solved.
  pub avg_runtime: ToolMap<(Duration, u32)>,
  /// Stores the exit status for each tool on the current benchmark. `None` if
  /// timeout or error.
  pub codes: HashMap< BenchIndex, ToolMap< Option<i32> > >,
  /// Number of inconsistent results obtained.
  pub inconsistencies: usize
}
impl Master {
  /// Creates (but does not run) a master. Initializes everything.
  pub fn mk(conf: Arc<RunConf>, instance: Arc<Instance>) -> Res<Self> {
    let mut bench_runs = Vec::with_capacity(conf.bench_par) ;
    // Channel to `self` shared by all **tool runs**.
    let (t2m_s, from_tool_runs) = tool_to_master_channel() ;
    // Channel to `self` shared by all **bench runs**.
    let (b2m_s, from_bench_runs) = bench_to_master_channel() ;

    // Spawn bench runs.
    for index in 0..conf.bench_par {
      // Master to bench personal channel.
      let (m2b_s, m2b_r) = master_to_bench_channel() ;
      // Remember sender.
      bench_runs.push(m2b_s) ;
      // Initialize bench run.
      let bench_run = BenchRun::mk(
        conf.clone(), instance.clone(), index,
        b2m_s.clone(), m2b_r, & t2m_s,
      ) ;
      // Launch.
      spawn( move || bench_run.launch() ) ;
      ()
    }

    let pbar = if conf.quiet() { None } else {
      let mut pbar = ProgressBar::new(
        (instance.tool_len() as u64) * (instance.bench_len() as u64)
      ) ;
      pbar.format("|##-|") ;
      pbar.tick_format("\\|/-") ;
      pbar.show_time_left = false ;
      pbar.show_speed = false ;
      pbar.show_tick = true ;
      Some(pbar)
    } ;

    let (tool_files, avg_runtime) = instance.init_tools(
      & conf,
      // Fold init for average runtime.
      ToolMap::with_capacity( instance.tool_len() ),
      // Fold function for average runtime.
      | avg_runtime: & mut ToolMap<_>, _ | {
        avg_runtime.push( (Duration::from_secs(0), 0u32) ) ;
      }
    ) ? ;

    let codes = HashMap::with_capacity( instance.bench_len() ) ;

    Ok(
      Master {
        conf, instance,
        bench_runs,
        from_tool_runs,
        from_bench_runs,
        tool_files,
        pbar,
        errors: 0,
        timeouts: 0,
        avg_runtime,
        codes,
        inconsistencies: 0,
      }
    )
  }

  /// Launches the listen/dispatch loop.
  pub fn run(& mut self) -> Res<Duration> {
    log!{ self.conf => "" }
    self.internal_run()
  }

  /// Launches the listen/dispatch loop.
  fn internal_run(& mut self) -> Res<Duration> {
    let start = Instant::now() ;
    'dispatch_bench: for bench in self.instance.benchs() {

      loop {

        // Consume all messages.
        //
        // Continue `'dispatch_bench` if someone was not busy.
        'bench_run_msgs: loop {
          match self.from_bench_runs.try_recv() {
            // Someone's ready.
            Ok( Ok(index) ) => if let Ok(()) = self.bench_runs[index].send(
              bench
            ) {
              // We want to feed everyone asap, dispatching next bench now.
              continue 'dispatch_bench
            } else {
              // Disconnected, keep going.
              print_err(
                & * self.conf,
                & format!("lost contact with bench run {}", index).into(),
                false
              ) ;
              println!() ;
              continue 'bench_run_msgs
            },
            Ok( Err(e) ) => {
              print_err(& * self.conf, & e, false) ;
              println!() ;
              continue 'bench_run_msgs
            },
            // No one's available, moving on to consume tool runs' messages.
            Err(TryRecvError::Empty) => break 'bench_run_msgs,
            // Disconnected, no more bench runs alive.
            Err(TryRecvError::Disconnected) => bail!(
              format!(
                "Bench run, on bench {}: \
                no more bench runs alive there's still benchs to run",
                self.conf.emph( self.instance.str_of_bench(bench) )
              )
            ),
          }
        }

        // Only reachable if no bench run's ready.

        // A failure here is fatal.
        let all_dead = try!( self.recv_results() ) ;
        if all_dead {
          bail!("no more tool runs alive, but there's still benchs to run")
        }
        sleep( Duration::from_millis(100) )

      }

    }

    // All the benchs have been dispatched.

    // Drop `bench_runs`, they will be disconnected time they try to receive.
    while let Some(bench_run) = self.bench_runs.pop() {
      ::std::mem::drop( bench_run )
    }
    // So they will die, drop `tool_runs`, causing the tool runs to be
    // disconnected next time they try to receive.
    //
    // Hence eventually the `self.recv_results()` will be true (all dead) and
    // we will be done.

    // Consume all remaining results.
    while ! try!( self.recv_results() ) {
      //          ^^^^^all^dead^^^^^^
      // Sleep for 100 ms to avoid burning CPU.
      sleep( Duration::from_millis(100) )
    }

    Ok(Instant::now() - start)
  }

  /// Updates the average run time of a tool, iff it's below the timeout.
  fn update_avg_runtime(& mut self, tool: ToolIdx, time: Duration) {
    if time < self.conf.timeout {
      let (ref mut avg, ref mut cnt) = self.avg_runtime[tool] ;
      // Incremental average computation.
      * cnt += 1 ;
      * avg = (*avg * (*cnt - 1) + time) / *cnt
    }
  }

  /// Receives some results from the tool runs. Returns `true` if disconnected.
  /// Non-blocking.
  fn recv_results(& mut self) -> Res<bool> {
    'recv: loop {
      match self.from_tool_runs.try_recv() {

        Ok( RunRes { tool, bench, res } ) => {
          self.pbar.as_mut().map( |b| b.inc() ) ;

          // If that was the last bench, try to delete err directory if empty.
          if self.instance.is_last_bench(bench) {
            self.cleanup(tool)
          }

          let mut data_line_end = String::new() ;

          match res {

            BenchRes::Success(time, status) => {
              self.update_avg_runtime(tool, time) ;
              if let Err(e) = self.register_result(
                bench, tool, status
              ).chain_err(
                || format!(
                  "during registration and consistency checking of {} on {}",
                  self.conf.emph( & self.instance[tool].ident() ),
                  self.conf.emph( self.instance.str_of_bench(bench) )
                )
              ) {
                println!() ;
                print_err(& * self.conf, & e, false) ;
                println!()
              }
              data_line_end += ", time = \"" ;
              data_line_end += & time.as_sec_str() ;
              data_line_end += "\"" ;
              if let Some(code) = status.code() {
                data_line_end += ", code = " ;
                data_line_end += & format!("{}", code)
              }
            },

            BenchRes::Timeout(status) => {
              self.timeouts += 1 ;
              if let Some(code) = status.code() {
                data_line_end += ", code = " ;
                data_line_end += & format!("{}", code)
              }
            },

            BenchRes::Error(time, status) => {
              self.errors += 1 ;
              data_line_end += ", time = \"" ;
              data_line_end += & time.as_sec_str() ;
              data_line_end += "\"" ;
              if let Some(code) = status.code() {
                data_line_end += ", code = " ;
                data_line_end += & format!("{}", code)
              }
            },

            BenchRes::BenchiError(e) => {
              if let Err(e) = (Err(e) as Res<()>).chain_err(
                || format!(
                  "internal error while running `{}` on `{}`",
                  self.conf.sad(& self.instance[tool].ident()),
                  self.conf.sad( self.instance.str_of_bench(bench) )
                )
              ) {
                print_err(& * self.conf, & e, false)
              }
              continue 'recv
            },
          }

          writeln!(
            & mut self.tool_files[tool],
            "{} = {{ bench = \"{}\"{} }}",
            self.instance.safe_name_for_bench(bench),
            self.instance.str_of_bench(bench),
            data_line_end
          ).chain_err(
            || format!(
              "while writing result of {} running on {}",
              self.conf.emph( & self.instance[tool].ident() ),
              self.conf.emph( self.instance.str_of_bench(bench) )
            )
          ) ?
        },

        Err( TryRecvError::Empty ) => {
          if let Some(pbar) = self.pbar.as_mut() {
            pbar.tick()
          }
          break 'recv
        },

        Err( TryRecvError::Disconnected ) => {
          if let Some(pbar) = self.pbar.as_mut() {
            pbar.tick()
          }
          return Ok(true)
        },
      }
    }
    Ok(false)
  }

  /// Register a tool run on a bench, and checks that all the tools agree on
  /// the exit code, if any.
  fn register_result(
    & mut self, bench: BenchIndex, tool: ToolIdx, status: ExitStatus
  ) -> Res<()> {
    {
      let instance = self.instance.clone() ; // `Arc` clone, not real clone.
      let codes = self.codes.entry(bench).or_insert_with(
        || instance.tools().map(
          |_| None
        ).collect()
      ) ;
      codes[tool] = status.code()
    }

    if let Some(code) = status.code() {
      let codes = self.codes.get(& bench).ok_or_else::<Error, _>(
        || "unreachable: register_result".into()
      ) ? ;
      
      let mut disagree = HashMap::new() ;

      for other_tool in self.instance.tools() {
        if tool != other_tool {
          if let Some(nu_code) = codes[other_tool] {
            if code != nu_code {
              let prev = disagree.insert(other_tool, nu_code) ;
              assert!( prev.is_none() )
            }
          }
        }
      }

      if ! disagree.is_empty() {
        let code_str = if let Some(
          vald_conf
        ) = self.conf.codes().get(code) {
          & vald_conf.name
        } else { "<unknown exit code>" } ;
        warn!(
          self.conf =>
            "Some tools disagree on benchmark `{}`:", self.conf.bad(
              self.instance.str_of_bench(bench)
            ) ;
            "result for {} validated with {}, but",
            self.conf.emph( & self.instance[tool].ident() ),
            self.conf.sad( code_str ) ;
            {
              for (tool, code) in disagree {
                let code_str = if let Some(
                  vald_conf
                ) = self.conf.codes().get(code) {
                  & vald_conf.name
                } else {
                  "<unknown exit code>"
                } ;
                warn!(
                  self.conf, line =>
                    "  result for {} validated with {}",
                    self.conf.emph( & self.instance[tool].ident() ),
                    self.conf.bad( code_str )
                )
              }
            }
        ) ;
        self.inconsistencies += 1
      }
    }
    Ok(())
  }

  /// Deletes the directories of a tool if they're not empty.
  ///
  /// First tries the error directory, and then the tool's directory.
  fn cleanup(& self, tool: ToolIdx) {
    let deleted_err_dir = self.try_delete_err_dir(tool) ;
    if deleted_err_dir {
      self.try_delete_dir(tool) ;
    }
  }

  /// Deletes the error directory of some tool if it's empty.
  fn try_delete_err_dir(& self, tool: ToolIdx) -> bool {
    let path = self.instance.err_path_of_tool(& self.conf, tool) ;
    let empty = path.as_path().read_dir().map(
      |mut rd| rd.next().is_none()
    ).unwrap_or(false) ;
    if empty {
      let _ = ::std::fs::remove_dir(path) ;
    }
    empty
  }

  /// Deletes the directory of some tool if it's empty.
  fn try_delete_dir(& self, tool: ToolIdx) -> bool {
    let path = self.instance.path_of_tool(& self.conf, tool) ;
    let empty = path.as_path().read_dir().map(
      |mut rd| rd.next().is_none()
    ).unwrap_or(false) ;
    if empty {
      let _ = ::std::fs::remove_dir(path) ;
    }
    empty
  }

}

