//! Run configuration loader.

use std::collections::BTreeMap ;

use toml::Value ;

use common::* ;
use errors::* ;

/// Deconstructs a TOML value.
macro_rules! explore {
  ($variant:tt => $e:expr, $($tail:tt)*) => (
    if let Value::$variant(inner) = $e {
      inner
    } else {
      explore! { fail $e, $($tail)* }
    }
  ) ;

  (fail $e:expr, $( $tail:tt )* ) => ({
    let s = format!( $($tail)* ) + & format!(
      "found a {}", $e.type_str()
    ) ;
    let e: Error = s.into() ;
    bail!(e)
  }) ;
}



/// A configuration right after loading.
#[derive(Debug, Serialize, Deserialize)]
pub struct LConf {
  /// Default options provided in the configuration file.
  options: Option<String>,
  /// Active tools.
  tools: Vec<String>,
}

///
#[derive(Debug, Serialize, Deserialize)]
pub struct LValidConf {
  /// Codes used in the validator.
  codes: BTreeMap<String, (i64, String)>,
  /// Error code.
  error: Option<(i64, String)>,
  /// Timeout code.
  tmo: Option<(i64, String)>,
  /// Actual validators.
  validators: BTreeMap<String, String>,
}


fn code_of_val(val: & Value) -> Res<(i64, Option<String>)> {
  match val {
    Value::Integer(code) => Ok((* code, None)),
    Value::Table(table) => {
      let (mut code, mut graph): (Option<i64>, Option<String>) = (None, None) ;
      for (key, val) in table {
        if key == "code" {
          code = Some(
            * explore!( Integer => val, "" )
          )
        } else if key == "graph" {
          graph = Some(
            explore!(
              String => val,
              "while retrieving the graph name for code {}",
              code.map(
                |code| format!("{}", code)
              ).unwrap_or_else(|| "<unknown>".into())
            ).clone()
          )
        } else {
          bail!("unexpected key `{}`", key)
        }
      }

      if let Some(code) = code {
        Ok((code, graph))
      } else {
        bail!("illegal validator code does not define the actual code")
      }
    },
    _ => explore! (
      fail val, "expected an integer or a table, "
    ),
  }
}


impl LValidConf {
  fn empty() -> Self {
    LValidConf {
      codes: BTreeMap::new(),
      error: None,
      tmo: None,
      validators: BTreeMap::new(),
    }
  }

  ///
  pub fn new(value: & Value) -> Res<Self> {
    let table = value.get(& "validators").and_then(
      |val| val.as_table()
    ).ok_or(
      "no `validators` section"
    ) ? ;

    let mut conf = Self::empty() ;

    for (key, val) in table {
      if key == "codes" {
        let codes = val.as_table().ok_or_else(
          || format!(
            "illegal `codes` section, expecting table, found {}",
            val.type_str()
          )
        ) ? ;
        for (key, val) in codes {
          let (code, graph) = code_of_val(val) ? ;
          if key == "error" {
            conf.error = Some((
              code, graph.unwrap_or_else( || "Error".into() )
            ))
          } else if key == "timeout" {
            conf.tmo = Some((
              code, graph.unwrap_or_else( || "Timeout".into() )
            ))
          } else {
            let prev = conf.codes.insert(
              key.clone(), (
                code, graph.unwrap_or_else( || key.clone() )
              )
            ) ;

            debug_assert! { prev.is_none() }
          }
        }
      } else {

        let validator = explore!(
          String => val, "expected a validator script, "
        ).clone() ;

        let prev = conf.validators.insert(key.clone(), validator) ;

        debug_assert! { prev.is_none() }
      }
    }

    Ok(conf)
  }
}

/// Tmp.
pub fn test(file: & str) {
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

  let res = try_parse!(Value) ;

  println!("{:#?}", res) ;

  println!("{}", ::toml::to_string_pretty(& res).unwrap()) ;

  let lconf = try_parse!(LConf) ;

  println!("{:#?}", lconf) ;

  let lconf = LValidConf::new(& res).unwrap() ;

  println!("{:#?}", lconf)
}