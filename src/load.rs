//! Run configuration loader.

use common::* ;

/// A run configuration.
#[derive(Debug, Clone)]
pub struct NewRunConf {
  /// Default options provided in the configuration file.
  options: Option<String>,
  /// Active tool configurations.
  tools: StrMap<NewToolConf>,
  /// Validator exit codes.
  codes: Map<i64, NewCode>,
}


/// A tool configuration.
#[derive(Debug, Clone)]
pub struct NewToolConf {
  /// Command.
  cmd: String,
  /// Graph name.
  graph: String,
  /// Optional validator.
  validator: Option<String>,
}


/// Exit code.
#[derive(Debug, Clone)]
pub struct NewCode {
  /// Identifier.
  name: String,
  /// Exit code.
  code: i64,
  /// Graph name.
  graph: String,
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


impl LRunConf {
  /// Checks and finalizes an `LRunConf`.
  pub fn finalize(mut self, gconf: & GConf) -> Res<NewRunConf> {
    let options = self.options ;

    // This will only store active tools.
    let mut tools: StrMap<NewToolConf> = Map::new() ;

    for tool in self.run {

      // Do we already know this tool?
      if tools.contains_key(& tool) {
        bail!("tool {} appears as active twice", gconf.bad(& tool))
      }

      // Retrieve tool configuration.
      let tool_conf = if let Some(conf) = self.tools.remove(& tool) {
        conf
      } else {
        bail!(
          "unknown tool {} in the list of active tools", gconf.bad(& tool)
        )
      } ;

      // Retrieve validator if any.
      let validator = if let Some(v) = tool_conf.validator {
        if let Some(validator) = self.validators.get(& v) {
          Some( validator.clone() )
        } else {
          bail!(
            "tool {} uses unknown validator {}",
            gconf.sad(& tool), gconf.bad(& v)
          )
        }
      } else { None } ;

      // Build actual tool configuration.
      let conf = NewToolConf {
        cmd: tool_conf.cmd,
        graph: tool_conf.graph.unwrap_or_else( || tool.clone() ),
        validator,
      } ;

      // Make sure there's no clash in graph names.
      for (t, c) in & tools {
        if conf.graph == c.graph {
          bail!(
            "tools {} and {} have the same graph name {}",
            gconf.sad(& tool), gconf.sad(& t), gconf.bad(& conf.graph)
          )
        }
      }

      // Tool should be new by construction.
      let prev = tools.insert(tool, conf) ;

      assert! { prev.is_none() }
    }

    // Work on exit codes.
    let mut codes: Map<i64, NewCode> = Map::new() ;

    for (name, code) in self.codes {

      // Make sure code name is not reserved.
      if ::consts::validator::reserved.contains(& name as & str) {
        bail!(
          "illegal validator code name {}: name is reserved",
          gconf.bad(& name)
        )
      }

      // Create graph name.
      let graph = code.graph.unwrap_or_else( || name.clone() ) ;

      // Build exit code structure.
      let code = NewCode {
        name: name.clone(), code: code.code, graph
      } ;

      // Code should be new by construction.
      let prev = codes.insert(code.code, code) ;
      if let Some(prev) = prev {
        bail!(
          "validator codes {} and {} have the same code {}",
          gconf.sad(& name), gconf.sad(& prev.name), gconf.bad(
            & format!("{}", prev.code)
          )
        )
      }
    }

    Ok(
      NewRunConf { options, tools, codes }
    )
  }
}




/// Tmp.
pub fn test(file: & str) {
  let gconf = & GConf::mk(Verb::Normal, true, false) ;
  if let Err(e) = test_inner(gconf, file) {
    print_err(gconf, & e, true)
  }
}



/// Tmp.
pub fn test_inner(gconf: & GConf, file: & str) -> Res<()> {
  let mut txt = String::new() ;
  File::open(file).unwrap().read_to_string(& mut txt).unwrap() ;

  macro_rules! try_parse {
    ($t:ty) => (
      match ::toml::from_str::<$t>(& txt) {
        Ok(res) => res,
        Err(e) => {
          println!("failed to parse input file {}", file) ;
          println!("{}", e) ;
          if let Some((l,c)) = e.line_col() {
            for (cnt, line) in txt.lines().enumerate() {
              if cnt == l {
                let line_count = format!("{}", l + 1) ;
                println!("{} |", " ".repeat( line_count.len() )) ;
                println!("{} | {}", line_count, line) ;
                println!(
                  "{} | {}^",
                  " ".repeat( line_count.len() ),
                  " ".repeat( c ),
                )
              }
            }
          }
          panic!("failure")
        },
      }
    ) ;
  }

  let res = try_parse!(LRunConf) ;

  println!("{}", ::toml::to_string_pretty(& res).unwrap()) ;

  let res = res.finalize(gconf) ? ;

  println!("{:#?}", res) ;

  panic!("done")
}