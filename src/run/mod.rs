/*! Runners for the different level of running.

The master dispatches the benchmarks over `BenchRun`s. They in turn dispatch
tool to run to `ToolRun`s.

# TODO

- naming convention is retarded, change it
- make the bench run say it's waiting only if it has at least one waiting tool
  run
*/

use std::process::Command ;

use common::* ;
use errors::* ;

pub mod para ;
use self::para::* ;

/// Lowest level in the hierarchy, runs a tool on a benchmark.
pub struct ToolRun {
  /// The configuration.
  pub conf: Arc<Conf>,
  /// The instance.
  pub instance: Arc<Instance>,
  /// Index of this tool run for the bench run responsible for it.
  index: usize,
  /// Sender to master.
  master: Sender< RunRes<Duration> >,
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
    master: Sender< RunRes<Duration> >,
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
  ) -> Res<Duration> {
    let bench = & self.instance[bench_idx] ;
    let cmd = self.instance[tool_idx].cmd.get() ;

    let mut cmd = Command::new(cmd) ;
    cmd.arg(bench) ;

    let start = Instant::now() ;
    let output = cmd.output() ;
    let time = Instant::now() - start ;
    output.chain_err(
      || ErrorKind::ToolRun(
        self.instance[tool_idx].clone(),
        self.instance.str_of_bench(bench_idx).into()
      )
    ).map(
      |_| time
    )
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
    tool_to_master: Sender< RunRes<Duration> >,
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
  from_tool_runs: Receiver< RunRes<Duration> >,
}
impl Master {
  /// Creates (but does not run) a master.
  pub fn mk(conf: Arc<Conf>, instance: Arc<Instance>) -> Self {
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

    Master {
      conf, instance,
      bench_runs,
      from_tool_runs,
      from_bench_runs,
    }
  }

  /// Launches the listen/dispatch loop.
  pub fn run(mut self) -> Res<Duration> {
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
        let all_dead = self.recv_results() ;
        if all_dead {
          bail!("no more tool runs alive, but there's still benchs to run")
        }

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
    while ! self.recv_results() {
      //    ^^^^^all^dead^^^^^^
      // Sleep for 500 ms to avoid burning CPU.
      ::std::thread::sleep( Duration::new(0, 500_000_000) )
    }

    Ok(Instant::now() - start)
  }

  /// Receives some results from the tool runs. Returns `true` if disconnected.
  fn recv_results(& self) -> bool {
    'recv: loop {
      match self.from_tool_runs.try_recv() {
        Ok( RunRes { tool, bench, res } ) => {
          match res.chain_err(
            || format!(
              "while running {} on {}",
              self.instance[tool].name,
              self.conf.emph( self.instance.str_of_bench(bench) )
            )
          ) {
            Ok(time) => {
              println!(
                "{} {} {}",
                self.conf.happy("|===|"), self.instance[tool].name,
                self.conf.emph( self.instance.str_of_bench(bench) )
              ) ;
              let time = format!(
                "{}.{}", time.as_secs(), time.subsec_nanos() / 1_000_000u32
              ) ;
              println!(
                "{} done in {}", self.conf.happy("|"), self.conf.emph(& time)
              ) ;
              println!( "{}", self.conf.happy("|===|") ) ;
              println!("")
            },
            Err(e) => {
              print_err(& * self.conf, e, false) ;
              println!("")
            },
          }
        },
        Err( TryRecvError::Empty ) => break 'recv,
        Err( TryRecvError::Disconnected ) => return true,
      }
    }
    false
  }
}