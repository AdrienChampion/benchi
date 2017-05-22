/*! Runners for the different level of running.

The master dispatches the benchmarks over `BenchRun`s. They in turn dispatch
tool to run to `ToolRun`s.

# TODO

- naming convention is retarded, change it
- make the bench run say it's waiting only if it has at least one waiting tool
  run
*/

use common::* ;
use errors::* ;

pub mod para ;
use self::para::* ;

enum Run {
  Id(u32),
  Done(
    ::std::io::Result<::std::process::Output>,
    Duration
  ),
}

/// Lowest level in the hierarchy, runs a tool on a benchmark.
pub struct ToolRun {
  /// The configuration.
  pub conf: Arc<Conf>,
  /// The instance.
  pub instance: Arc<Instance>,
  /// Index of this tool run for the bench run responsible for it.
  index: usize,
  /// Sender to master.
  master: Sender< RunRes<(Duration, Output)> >,
  /// Sender to bench run.
  bench_run: Sender< usize >,
  /// Receiver from bench run.
  from_bench_run: Receiver< (ToolIndex, BenchIndex) >
}
unsafe impl Send for ToolRun {}
impl ToolRun {
  /// Creates (but does not run) a tool run.
  #[inline]
  fn mk(
    conf: Arc<Conf>, instance: Arc<Instance>, index: usize,
    master: Sender< RunRes<(Duration, Output)> >,
    bench_run: Sender< usize >,
    from_bench_run: Receiver< (ToolIndex, BenchIndex) >,
  ) -> Self {
    ToolRun {
      conf, instance, index, master, bench_run, from_bench_run
    }
  }

  /// Launches the tool run listen/run loop.
  pub fn launch(self) {
    'work: loop {

      // Notify bench run we're waiting.
      if let Err(_) = self.bench_run.send( self.index ) {
        // Disconnected, can happen if we're done. (Maybe? Nothing we can
        // do anyways.)
        break 'work
      }

      // Waiting for something to do.
      match self.from_bench_run.recv() {
        Ok( (tool, bench) ) => {
          // Work...
          let res = self.run(tool, bench) ;
          // Send to master.
          if let Err(_) = self.master.send( RunRes { tool, bench, res } ) {
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
    & self, tool_idx: ToolIndex, bench_idx: BenchIndex
  ) -> Res< (Duration, Output) > {
    let bench = self.instance[bench_idx].clone() ;
    let vec_cmd = & self.instance[tool_idx].cmd ;
    let kid_cmd = vec_cmd.clone() ;

    assert!( vec_cmd.len() > 0 ) ;

    // Command will be ran asynchonously for timeouts.
    let (sender, recver) = channel() ;

    spawn(
      move || {
        let mut cmd = Command::new(& kid_cmd[0]) ;
        for arg in & kid_cmd[1..] {
          cmd.arg(arg) ;
        }
        cmd.arg(bench) ;
        cmd.stdin( Stdio::null() ) ;
        cmd.stdout( Stdio::piped() ) ;
        cmd.stderr( Stdio::piped() ) ;
        let start = Instant::now() ;
        let child = cmd.spawn().expect("error spawning command") ;
        let _ = sender.send( Run::Id(child.id()) ) ;
        let output = child.wait_with_output() ;
        // .map(
        //   |out| (out.status, out.stdout, out.stderr)
        // ) ;
        let time = Instant::now() - start ;
        let _ = sender.send( Run::Done(output, time) ) ;
        ()
      }
    ) ;
    let start = Instant::now() ;
    
    let mut kid_id = None ;

    'receiving: loop {

      // Is the kid done?
      match recver.try_recv() {
        Ok( Run::Id(id) ) => {
          kid_id = Some(id) ;
          continue 'receiving
        },
        // Done.
        Ok( Run::Done(output, time) ) => {
          return output.chain_err(
            || {
              let mut s = format!("running command") ;
              for arg in vec_cmd.iter() {
                s = format!("{} {}", s, arg)
              }
              s
            }
          ).chain_err(
            || ErrorKind::ToolRun(
              self.conf.clone(),
              self.instance[tool_idx].clone(),
              self.instance.str_of_bench(bench_idx).into()
            )
          ).map(
            |out| (time, out)
          )
        },

        // Not yet...
        Err(TryRecvError::Empty) => (),

        // This should not happen.
        Err(TryRecvError::Disconnected) => bail!({
          let mut s = format!("disconnect from kid running command") ;
          for arg in vec_cmd.iter() {
            s = format!("{} {}", s, arg)
          }
          s
        }),
      }

      // Timeout reached?
      if Instant::now() - start > self.conf.timeout {
        if let Some(id) = kid_id {
          kill_process(id)
        }
        continue 'receiving
      }

      // Wait a 100ms to avoid burning cpu.
      sleep( Duration::from_millis(100) )
    }
  }
}



/// Intermediary level in the hierarchy, is in charge of running some tools on
/// a particular benchmark.
pub struct BenchRun {
  /// The configuration.
  pub conf: Arc<Conf>,
  /// The instance.
  pub instance: Arc<Instance>,
  /// The index of this bench run for the master.
  pub index: usize,
  /// Sender to master.
  master: Sender< Res<usize> >,
  /// Receiver from master.
  from_master: Receiver< BenchIndex >,
  /// Senders to tool runs.
  tool_runs: Vec< Sender< (ToolIndex, BenchIndex) > >,
  /// Receiver from tool runs.
  from_tool_runs: Receiver< usize >
}
impl BenchRun {
  /// Creates (but does not run) a tool run.
  ///
  /// `tool_to_master` will be given to the `ToolRun`s spawned.
  #[inline]
  fn mk(
    conf: Arc<Conf>, instance: Arc<Instance>, index: usize,
    master: Sender< Res<usize> >,
    from_master: Receiver< BenchIndex >,
    tool_to_master: Sender< RunRes<(Duration, Output)> >,
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
      if let Err(_) = self.master.send( Ok( self.index ) ) {
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
            if let Err(_) = self.master.send( Err(e) ) {
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
          Ok( index ) => if let Ok(()) = self.tool_runs[index].send(
            (tool, bench)
          ) {
            continue 'dispatch_tool
          } else {
            // Disconnected, keep going.
            continue 'find_idle
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
  pub conf: Arc<Conf>,
  /// The instance.
  pub instance: Arc<Instance>,
  /// Senders to bench runs (intermediary level).
  bench_runs: Vec< Sender<BenchIndex> >,
  /// Receiver from bench runs (intermediary level).
  from_bench_runs: Receiver< Res<usize> >,
  /// Receiver from tool runs (lowest level).
  from_tool_runs: Receiver< RunRes<(Duration, Output)> >,
  /// Files in write mode to write the results to.
  tool_files: ToolVec<File>,
  /// Progress bar.
  bar: Option< ProgressBar< ::std::io::Stdout >,
  >,
}
impl Master {
  /// Creates (but does not run) a master.
  pub fn mk(conf: Arc<Conf>, instance: Arc<Instance>) -> Res<Self> {
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
        b2m_s.clone(), m2b_r, t2m_s.clone(),
      ) ;
      // Launch.
      spawn( move || bench_run.launch() ) ;
      ()
    }

    let bar = if conf.quiet { None } else {
      let mut bar = ProgressBar::new(
        (instance.tool_len() as u64) * (instance.bench_len() as u64)
      ) ;
      bar.format("|##-|") ;
      bar.tick_format("\\|/-") ;
      bar.show_time_left = false ;
      bar.show_speed = false ;
      bar.show_tick = true ;
      Some(bar)
    } ;

    let mut tool_files = ToolVec::with_capacity( instance.tool_len() ) ;
    // Open output files.
    for tool in instance.tools() {
      let mut path = PathBuf::new() ;
      path.push(& conf.out_dir) ;
      path.push( format!("{}.data", instance[tool].short) ) ;
      tool_files.push(
        try!(
          open_file_writer( path.as_path() ).chain_err(
            || format!(
              "while creating file `{}`, data file for `{}`",
              conf.sad(
                path.to_str().expect("non-UTF8 path")
              ),
              conf.emph( & instance[tool].name )
            )
          )
        )
      )
    }

    Ok(
      Master {
        conf, instance,
        bench_runs,
        from_tool_runs,
        from_bench_runs,
        tool_files,
        bar,
      }
    )
  }

  /// Launches the listen/dispatch loop.
  pub fn run(mut self) -> Res<Duration> {
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
                format!("lost contact with bench run {}", index).into(),
                false
              ) ;
              println!("") ;
              continue 'bench_run_msgs
            },
            Ok( Err(e) ) => {
              print_err(& * self.conf, e, false) ;
              println!("") ;
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

  /// Receives some results from the tool runs. Returns `true` if disconnected.
  fn recv_results(& mut self) -> Res<bool> {
    'recv: loop {
      match self.from_tool_runs.try_recv() {
        Ok( RunRes { tool, bench, res } ) => {
          self.bar.as_mut().map( |b| b.inc() ) ;
          match res.chain_err(
            || format!(
              "while running {} on {}",
              self.instance[tool].name,
              self.conf.emph( self.instance.str_of_bench(bench) )
            )
          ) {
            // Ok(None) => try!(
            //   writeln!(
            //     & mut self.tool_files[tool],
            //     "{} {}", self.instance.str_of_bench(bench),
            //     format!("timeout({})", self.conf.timeout.as_sec_str())
            //   ).chain_err(
            //     || format!(
            //       "while writing result of {} running on {}",
            //       self.conf.emph( & self.instance[tool].name ),
            //       self.conf.emph( self.instance.str_of_bench(bench) )
            //     )
            //   )
            // ),
            Ok( (time, output) ) => {
              if ! output.stderr.is_empty() {
                try!( self.dump_stderr(tool, bench, & output) ) ;
                try!( self.dump_stdout(tool, bench, & output) )
              } else {
                if self.conf.log_output {
                  try!( self.dump_stdout(tool, bench, & output) )
                }
              }
              let res = if time > self.conf.timeout {
                format!( "timeout({})", self.conf.timeout.as_sec_str() )
              } else if ! (
                output.stderr.is_empty() && output.status.success()
              ) {
                "error".to_string()
              } else {
                time.as_sec_str()
              } ;
              try!(
                writeln!(
                  & mut self.tool_files[tool],
                  "{} {}", self.instance.str_of_bench(bench), res
                ).chain_err(
                  || format!(
                    "while writing result of {} running on {}",
                    self.conf.emph( & self.instance[tool].name ),
                    self.conf.emph( self.instance.str_of_bench(bench) )
                  )
                )
              ) ;
              // let time = res.map(
              //   |(time, _)| format!(
              //     "{}.{:0>6}", time.as_secs(), time.subsec_nanos() / 1_000u32
              //   )
              // ).unwrap_or("timeout".to_string()) ;
              // println!(
              //   "{} {} {}",
              //   self.conf.happy("|===|"), self.instance[tool].name,
              //   self.conf.emph( self.instance.str_of_bench(bench) )
              // ) ;
              // println!(
              //   "{} done in {}", self.conf.happy("|"), self.conf.emph(& time)
              // ) ;
              // println!( "{}", self.conf.happy("|===|") ) ;
              // println!("")
            },
            Err(e) => {
              print_err(& * self.conf, e, false) ;
              println!("")
            },
          }
        },
        Err( TryRecvError::Empty ) => {
          self.bar.as_mut().map( |b| b.tick() ) ;
          break 'recv
        },
        Err( TryRecvError::Disconnected ) => {
          self.bar.as_mut().map( |b| b.tick() ) ;
          return Ok(true)
        },
      }
    }
    Ok(false)
  }

  /// Creates the error path of a tool.
  #[inline]
  pub fn mk_err_dir(& self, tool: ToolIndex) -> Res<PathBuf> {
    let mut path = PathBuf::from( & self.conf.out_dir ) ;
    path.push( & self.instance[tool].short ) ;
    path.push("err") ;
    mk_dir(& path).chain_err(
      || format!(
        "while creating err directory for {}",
        self.conf.emph( & self.instance[tool].name )
      )
    ).map(|()| path)
  }
  /// Creates the output path of a tool.
  #[inline]
  pub fn mk_out_dir(& self, tool: ToolIndex) -> Res<PathBuf> {
    let mut path = PathBuf::from( & self.conf.out_dir ) ;
    path.push( & self.instance[tool].short ) ;
    path.push("out") ;
    mk_dir(& path).chain_err(
      || format!(
        "while creating output directory for {}",
        self.conf.emph( & self.instance[tool].name )
      )
    ).map(|_| path)
  }

  /// Dumps the stderr of some output for a tool on a benchmark.
  fn dump_stderr(
    & self, tool: ToolIndex, bench: BenchIndex, output: & Output
  ) -> Res<()> {
    let mut path = try!( self.mk_err_dir(tool) ) ;
    path.push( self.instance.str_of_bench(bench) ) ;
    let mut file = try!(
      open_file_writer(path).chain_err(
        || format!(
          "while opening error file to write stderr \
          of {} running on {}",
          self.conf.emph( & self.instance[tool].name ),
          self.conf.emph( self.instance.str_of_bench(bench) )
        )
      )
    ) ;
    file.write_all(& output.stderr).chain_err(
      || format!(
        "while writing stderr of {} running on {}",
        self.conf.emph( & self.instance[tool].name ),
        self.conf.emph( self.instance.str_of_bench(bench) )
      )
    )
  }

  /// Dumps the stdout of some output for a tool on a benchmark.
  fn dump_stdout(
    & self, tool: ToolIndex, bench: BenchIndex, output: & Output
  ) -> Res<()> {
    let mut path = try!( self.mk_out_dir(tool) ) ;
    path.push( self.instance.str_of_bench(bench) ) ;
    let mut file = try!(
      open_file_writer(path).chain_err(
        || format!(
          "while opening output file to write stdout \
          of {} running on {}",
          self.conf.emph( & self.instance[tool].name ),
          self.conf.emph( self.instance.str_of_bench(bench) )
        )
      )
    ) ;
    file.write_all(& output.stdout).chain_err(
      || format!(
        "while writing output of {} running on {}",
        self.conf.emph( & self.instance[tool].name ),
        self.conf.emph( self.instance.str_of_bench(bench) )
      )
    )
  }
}

