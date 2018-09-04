//! `benchi` runs benchmarks.

#![forbid(missing_docs)]
#![allow(non_upper_case_globals)]

extern crate chrono ;
#[macro_use]
extern crate clap as clap_lib ;
extern crate pbr ;
extern crate ansi_term as ansi ;
extern crate regex ;
#[macro_use]
extern crate error_chain ;
#[macro_use]
extern crate lazy_static ;
extern crate rayon ;
extern crate serde ;
#[macro_use]
extern crate serde_derive ;
extern crate toml ;
extern crate wait_timeout ;
#[macro_use]
extern crate mylib ;

pub mod errors ;
pub mod consts ;
#[macro_use]
pub mod common ;
pub mod clap ;
// pub mod parse ;
pub mod load ;
pub mod run ;
pub mod plot ;
pub mod inspect ;

use common::* ;
use common::run::* ;


/// Entry point.
fn main() {

  match clap::work() {
    Ok( Clap::Run(mut conf, tools) ) => {

      log!{ conf, verb =>
        "{}:", conf.emph("Configuration") ;
        "           timeout: {}s", conf.timeout.as_secs() ;
        "           out dir: {}", conf.happy(& conf.out_dir) ;
        "      benchs in //: {}", conf.bench_par ;
        "       tools in //: {}", conf.tool_par ;
        "  max threads used: {}",
        conf.emph(& format!("{}", conf.bench_par * conf.tool_par)) ;
        {
          if let Some(max) = conf.try {
            log!{ conf, verb => "               try: {}", max }
          }
        }
        "" ;
        "Loading instance..."
      }

      let instance = match load_instance(& mut conf, tools) {
        Ok(instance) => instance,
        Err(e) => {
          print_err(& conf, & e, true) ;
          unreachable!()
        },
      } ;

      log!{
        conf, verb => "done"
      }

      let (conf, instance) = (
        Arc::new(conf), Arc::new(instance)
      ) ;

      if let Err(e) = work( & conf, & instance ) {
        print_err(& * conf, & e, true)
      }
    },

    Ok( Clap::Plot(conf, kind) ) => {
      if let Err(e) = plot::work(& conf, kind) {
        print_err(& conf, & e, true)
      }
    },

    Ok( Clap::Conf(conf, file) ) => {
      if let Err(e) = common::example_conf_file(& conf, & file) {
        print_err(& conf, & e, true)
      }
    },

    Err(e) => print_err(& GConf::default(), & e, true)
  }

  ::std::process::exit(0)
}


fn load_instance(conf: & mut RunConf, tools: NewToolConfs) -> Res<Instance> {
  let mut benchs = {

    let buff_read = try!(
      File::open(& conf.bench_file).map(BufReader::new).chain_err(
        || "while opening benchmark listing file"
      )
    ) ;
    let mut benchs = Vec::with_capacity( 200 ) ;

    for maybe_line in buff_read.lines() {
      benchs.push( try!(maybe_line) )
    }
    benchs

  } ;

  if let Some(max) = conf.try {
    benchs.truncate(max)
  }

  // let tools = tools.into_iter().map(
  //   |tool| tool.to_tool_conf()
  // ).collect() ;

  let instance = Instance::mk(tools, benchs) ;

  if instance.tool_len() < conf.tool_par {
    conf.tool_par = instance.tool_len()
  }
  if instance.bench_len() < conf.bench_par {
    conf.bench_par = instance.bench_len()
  }

  Ok(instance)
}



fn work(conf: & Arc<RunConf>, instance: & Arc<Instance>) -> Res<()> {
  if instance.tool_len() == 0 || instance.bench_len() == 0 {
    return Ok(())
  }

  // Create output directory if it doesn't already exist.
  try!(
    mk_dir(& conf.out_dir).chain_err(
      || format!(
        "while creating output directory `{}`", conf.emph(& conf.out_dir)
      )
    )
  ) ;

  let mut master = try!(
    run::Master::mk(conf.clone(), instance.clone())
  ) ;

  log!(
    conf =>
      { log!( conf, verb => "" ) }
      "Running {} tools on {} benchmarks...",
      instance.tool_len(), instance.bench_len()
  ) ;

  let time = try!( master.run() ) ;

  log!(
    conf =>
      let time = format!(
        "{}.{}", time.as_secs(), time.subsec_millis()
      ) ;
      " " ;
      let pref = if master.errors > 0 || master.timeouts > 0 {
        " with "
      } else {
        ""
      } ;
      let timeouts = if master.timeouts > 0 {
        format!("{} timeouts", master.timeouts)
      } else {
        "".into()
      } ;
      let sep = if master.errors > 0 && master.timeouts > 0 {
        " and "
      } else {
        ""
      } ;
      let errors = if master.errors > 0 {
        format!("{} errors", master.errors)
      } else {
        "".into()
      } ;
      "Done in {}s{}{}{}{}",
      conf.emph(& time),
      pref, conf.sad(& timeouts), sep, conf.bad(& errors) ;
      "" ;
      "Tools:" ;
      {
        for tool in instance.tools() {
          let (avg, cnt) = master.avg_runtime[tool] ;
          log!{
            conf =>
              "  {}: {} solved, average runtime: {}",
              conf.emph( instance[tool].ident() ), cnt,
              if cnt > 0 { avg.as_sec_str() } else {
                "no benchmark passed".into()
              }
          }
        }
      } {
        if master.inconsistencies > 0 {
          log!{
            conf =>
            "" ; "{}", conf.bad(
              & format!(
                "Found {} inconsistenc{}",
                master.inconsistencies,
                if master.inconsistencies > 1 { "ies" } else { "y" }
              )
            )
          }
        }
      }
  ) ;

  Ok(())
}