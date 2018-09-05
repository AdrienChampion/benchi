//! `benchi` runs benchmarks.

#![forbid(missing_docs)]
#![allow(non_upper_case_globals)]

#[macro_use]
extern crate benchi;

use benchi::{common::run::*, common::*, run::Master};

/// Entry point.
fn main() {
    match ::benchi::clap::work() {
        Ok(Clap::Run(mut conf, tools)) => {
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

            let instance = match load_instance(&mut conf, *tools) {
                Ok(instance) => instance,
                Err(e) => {
                    print_err(&conf, &e, true);
                    unreachable!()
                }
            };

            log! { conf, verb => "done" }

            let (conf, instance) = (Arc::new(conf), Arc::new(instance));

            if let Err(e) = work(&conf, &instance) {
                print_err(&*conf, &e, true)
            }
        }

        Ok(Clap::Plot(conf, kind)) => {
            if let Err(e) = ::benchi::plot::work(&conf, kind) {
                print_err(&conf, &e, true)
            }
        }

        Ok(Clap::Conf(conf, file)) => {
            if let Err(e) = example_conf_file(&conf, &file) {
                print_err(&conf, &e, true)
            }
        }

        Err(e) => print_err(&GConf::default(), &e, true),
    }

    ::std::process::exit(0)
}

fn load_instance(conf: &mut RunConf, tools: ToolInfos) -> Res<Instance> {
    let mut benchs = {
        let buff_read = try!(
            File::open(&conf.bench_file)
                .map(BufReader::new)
                .chain_err(|| "while opening benchmark listing file")
        );
        let mut benchs = BenchMap::with_capacity(200);

        for maybe_line in buff_read.lines() {
            benchs.push(try!(maybe_line))
        }
        benchs
    };

    if let Some(max) = conf.try {
        benchs = benchs.into_iter().take(max).collect()
    }

    // let tools = tools.into_iter().map(
    //   |tool| tool.to_tool_conf()
    // ).collect() ;

    let instance = Instance::new(tools, benchs);

    if instance.tool_len() < conf.tool_par {
        conf.tool_par = instance.tool_len()
    }
    if instance.bench_len() < conf.bench_par {
        conf.bench_par = instance.bench_len()
    }

    Ok(instance)
}

fn work(conf: &Arc<RunConf>, instance: &Arc<Instance>) -> Res<()> {
    if instance.tool_len() == 0 || instance.bench_len() == 0 {
        return Ok(());
    }

    // Create output directory if it doesn't already exist.
    mk_dir(&conf.out_dir).chain_err(|| {
        format!(
            "while creating output directory `{}`",
            conf.emph(&conf.out_dir)
        )
    })?;

    let mut master = Master::new(conf.clone(), instance.clone())?;

    log! { conf =>
        { log!( conf, verb => "" ) }
        "Running {} tools on {} benchmarks...",
        instance.tool_len(), instance.bench_len()
    }

    let time = master.run()?;

    log! { conf =>
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

        "{}\n", master.stats.to_string_pretty(conf.as_ref(), & instance) ;

        {
            if master.inconsistencies > 0 {
                log!{ conf =>
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
    }

    Ok(())
}
