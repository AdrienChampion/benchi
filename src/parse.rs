//! Parser used by `benchi`.

use std::str::from_utf8 ;

use nom::{ IResult, multispace } ;

use common::* ;
use common::res::* ;
use consts::{ data, dump } ;


/// Empty result, used as the rest for some parser results.
#[cfg(test)]
static nothing: [ u8 ; 0 ] = [] ;

/// Checks that a parser is successful on some input.
#[cfg(test)]
macro_rules! test_success {
  ($(;)*) => (()) ;
  ($p:ident $e:expr => $res:expr ; $($tail:tt)*) => ({
    use nom::IResult::* ;
    let data = $e ;
    println!("  testing `{}`", data) ;
    assert_eq!{
      $p( data.as_bytes() ), Done(& nothing as & [u8], $res)
    }
    test_success!{ $($tail)* }
  }) ;
  ($p:ident $e:expr => $rest:expr, $res:expr ; $($tail:tt)*) => ({
    use nom::IResult::* ;
    let data = $e ;
    println!("  testing `{}`", data) ;
    assert_eq!{
      $p( data.as_bytes() ), Done($rest.as_bytes(), $res)
    }
    test_success!{ $($tail)* }
  }) ;
}



/// Tool configuration builder.
#[derive(Clone, Debug)]
pub struct ToolConfBuilder {
  name: String,
  short: Option<String>,
  graph: Option<String>,
  cmd: Option< Vec<String> >,
  validator: Option<String>,
}
impl ToolConfBuilder {
  /// Builder from a name.
  pub fn of_name(name: String) -> Self {
    ToolConfBuilder {
      name, short: None, graph: None, cmd: None, validator: None
    }
  }
  /// Sets the short name.
  pub fn set_short(& mut self, short: String) -> Res<()> {
    if let Some(ref old) = self.short {
      bail!(
        format!(
          "trying to set the short name for {} twice: `{}` and `{}`",
          self.name, old, short
        )
      )
    } else {
      self.short = Some(short) ;
      Ok(())
    }
  }
  /// Sets the graph name.
  pub fn set_graph(& mut self, graph: String) -> Res<()> {
    if let Some(ref old) = self.graph {
      bail!(
        format!(
          "trying to set the graph name for {} twice: `{}` and `{}`",
          self.name, old, graph
        )
      )
    } else {
      self.graph = Some(graph) ;
      Ok(())
    }
  }
  /// Sets the command name.
  pub fn set_cmd(& mut self, cmd: Vec<String>) -> Res<()> {
    if self.cmd.is_some() {
      bail!(
        format!(
          "trying to set the command for {} twice", self.name
        )
      )
    } else {
      self.cmd = Some(cmd) ;
      Ok(())
    }
  }
  /// Sets the validator name.
  pub fn set_validator(& mut self, validator: String) -> Res<()> {
    if self.validator.is_some() {
      bail!(
        format!(
          "trying to set the validator for {} twice", self.name
        )
      )
    } else {
      self.validator = Some(validator) ;
      Ok(())
    }
  }

  /// Extracts a tool configuration.
  pub fn into_conf(self) -> Res<ToolConf> {
    let name = self.name ;
    let short = self.short.ok_or_else::<Error, _>(
      || format!("no short name given for `{}`", name).into()
    ) ? ;
    let graph = self.graph ;
    let cmd_lst = self.cmd.ok_or_else::<Error, _>(
      || format!("no command given for `{}`", name).into()
    ) ? ;
    let validator = self.validator ;
    let mut cmd = String::new() ;
    let mut post = "" ;
    for line in cmd_lst {
      cmd += post ;
      cmd += & line ;
      post = "\n"
    }
    Ok(
      ToolConf { name, short, graph, cmd, validator }
    )
  }
}

named!{
  #[doc = "Comment parser."],
  comment, re_bytes_find!(r"^#[^\n]*\n*")
}






#[test]
fn t_spc_cmt() {
  test_success!{
    spc_cmt "#   " => () ;
    spc_cmt "  \t#   " => () ;
    spc_cmt "  \t#  \n " => () ;
    spc_cmt "  \t#  \n       blah" => "blah", () ;
  }
}

named!{
  #[doc = "Space and comments parser"],
  spc_cmt<()>, map!(
    many0!(
      alt_complete!(
        comment | multispace
      )
    ), |_| ()
  )
}







#[test]
fn t_ident() {
  test_success!{
    ident "blah" => "blah".to_string() ;
    ident "bl7_ah" => "bl7_ah".to_string() ;
  }
}

named!{
  #[doc = "Ident parser."],
  ident<String>, map_res!(
    re_bytes_find!(r"^[a-zA-Z][a-zA-Z0-9_]*"),
    |bytes: & 'a [u8]| from_utf8(bytes).map( |s| s.to_string() )
  )
}






#[test]
fn t_string() {
  test_success!{
    string "blah" => "blah".to_string() ;
    string "anything &]+)=[]![+)&[]+'\t |?^`8%~" =>
      "anything &]+)=[]![+)&[]+'\t |?^`8%~".to_string() ;
    string "     the   string gets cropped \t  " =>
      "the   string gets cropped".to_string() ;
    string "anything but# pound sign" =>
      "# pound sign", "anything but".to_string() ;
    string "anything but\n new line" =>
      "\n new line", "anything but".to_string() ;
    string "anything but\" double quote" =>
      "\" double quote", "anything but".to_string() ;
    string "anything but{ curly brace" =>
      "{ curly brace", "anything but".to_string() ;
    string "anything but} curly brace" =>
      "} curly brace", "anything but".to_string() ;
    string "anything but} curly brace" =>
      "} curly brace", "anything but".to_string() ;
  }
}

named!{
  #[doc = r#"Unquoted string parser. Parses anything but `#\n{}"`."#],
  string<String>, map_res!(
    re_bytes_find!(r#"^[^#\n{}"]+"#),
    |bytes: & 'a [u8]| from_utf8(bytes).map(
      |s: & 'a str| s.trim().to_string()
    )
  )
}







#[test]
fn t_quoted_string() {
  test_success!{
    quoted_string "\"blah\"" =>
      vec![ "blah".to_string() ] ;
    quoted_string "\"anything &]+)=[]![+)&[]+'\t |?^`8%~\"" =>
      vec![ "anything &]+)=[]![+)&[]+'\t |?^`8%~".to_string() ] ;
    quoted_string "\"stops at\" double quote\"" =>
      " double quote\"", vec![ "stops at".to_string() ] ;
    quoted_string "\"     the   string gets cropped \t  \"" =>
      vec![ "the   string gets cropped".to_string() ] ;
    quoted_string "\"  multi lines  \n is fine\nand cropped\t \t\"" =>
      vec![
        "multi lines".to_string(),
        "is fine".to_string(),
        "and cropped".to_string(),
      ] ;
  }
}

named!{
  #[doc = r#"Quoted string parser, parses anything but `"`."#],
  quoted_string< Vec<String> >, delimited!(
    char!('"'),
    many0!(
      terminated!(
        map_res!(
          is_not!("\n\""), |bytes| from_utf8(bytes).map(
            |s| s.trim().to_string()
          )
        ),
        opt!( char!('\n') )
      )
    ),
    char!('"')
  )
}




#[test]
fn t_raw_string() {
  test_success!{
    raw_string "``````" => "".to_string() ;
    raw_string "```yeah```" => "yeah".to_string() ;
    raw_string "```   something\n\nsomething else :)#  ```" =>
      "   something\n\nsomething else :)#  ".to_string() ;
    raw_string r#"``` everything goes %756342[!})*={![&#&$!&[{(]/\|^?_Z'"" :)# ,.p;,;:<P'j`%~786% ```"# =>
      r#" everything goes %756342[!})*={![&#&$!&[{(]/\|^?_Z'"" :)# ,.p;,;:<P'j`%~786% "#.to_string() ;
  }
}

named!{
  #[doc = "Raw string parser."],
  raw_string<String>, map_res!(
    do_parse!(
      tag!("```") >>
      raw: take_until_s!("```") >>
      tag!("```") >> (raw)
    ), |bytes| from_utf8(bytes).map(|s| s.to_string())
  )
}


named!{
  #[doc = "Parses some options."],
  options< Vec<String> >, preceded!(
    do_parse!(
      tag!("options") >>
      spc_cmt >>
      char!(':') >>
      spc_cmt >> ()
    ),
    quoted_string
  )
}



/// Parses a signed integer.
named!{
  signed_int< Res<i32> >, do_parse!(
    sign: opt!( char!('-') ) >>
    spc_cmt >>
    num: re_bytes_find!( r"^[0-9][0-9]*" ) >> (
      from_utf8(num).chain_err(
        || "non utf8 string during signed int parsing"
      ).and_then(
        |num| {
          use ::std::str::FromStr ;
          i32::from_str(num).chain_err(
            || format!("expected integer, found `{}`", num)
          )
        }
      ).map(
        |num| if sign.is_none() { num } else { - num }
      )
    )
  )
}




/// Parses a validator conf.
fn validator_conf(bytes: & [u8]) -> IResult< & [u8], Res<ValdConf> > {
  let mut vald_conf = Ok( ValdConf::empty() ) ;
  map!(
    bytes,
    opt!(
      do_parse!(
        tag!(dump::vald_conf_key) >>
        spc_cmt >>
        char!('{') >>
        spc_cmt >>
        many0!(
          do_parse!(
            tag!(dump::vald_conf_suc_key) >>
            spc_cmt >>
            char!(':') >>
            spc_cmt >>
            code: signed_int >>
            spc_cmt >>
            char!(',') >>
            spc_cmt >>
            alias: ident >>
            spc_cmt >>
            char!(',') >>
            spc_cmt >>
            desc: string >>
            spc_cmt >> (
              match code {
                Ok(code) => {
                  vald_conf = vald_conf.and_then(
                    |conf| conf.add_succ(
                      code, ValdCode { alias, desc, color: None }
                    )
                  )
                },
                Err(e) => vald_conf = Err(e),
              }
            )
          )
        ) >>
        char!('}') >> ()
      )
    ),
    |_| vald_conf
  )
}


/// Parses a tool conf field.
fn tool_conf_field<'a, 'b>(
  bytes: & 'a [u8], builder: & 'b mut ToolConfBuilder
) -> IResult< & 'a [u8], Res<()> > {
  alt_complete!(
    bytes,

    do_parse!(
      tag!(dump::short_name_key) >>
      spc_cmt >>
      char!(':') >>
      spc_cmt >>
      short: ident >> (
        builder.set_short( short )
      )
    ) |

    do_parse!(
      tag!(dump::cmd_key) >>
      spc_cmt >>
      char!(':') >>
      spc_cmt >>
      cmd: quoted_string >> ({
        let cmd = {
          let mut iter = cmd.into_iter() ;
          if let Some(mut s) = iter.next() {
            for next in iter {
              s = format!("{} {}", s, next)
            }
            vec![s]
          } else {
            vec![]
          }
        } ;
        builder.set_cmd(cmd)
      })
    ) |

    do_parse!(
      tag!(dump::graph_name_key) >>
      spc_cmt >>
      char!(':') >>
      spc_cmt >>
      graph: string >> (
        builder.set_graph( graph )
      )
    ) |

    do_parse!(
      tag!(dump::vald_key) >>
      spc_cmt >>
      char!(':') >>
      spc_cmt >>
      validator: raw_string >> (
        builder.set_validator( validator )
      )
    )

  )
}

/// Parses a tool configuration.
fn tool_conf<'a>(bytes: & 'a [u8]) -> IResult< & 'a [u8], Res<ToolConf> > {
  let mut builder = None ;
  do_parse!(
    bytes,
    map!(
      string, |name: String| builder = Some(
        ToolConfBuilder::of_name(name)
      )
    ) >>
    spc_cmt >>
    char!('{') >>
    spc_cmt >>
    many0!(
      terminated!(
        apply!(
          tool_conf_field, builder.as_mut().unwrap()
        ),
        spc_cmt
      )
    ) >>
    char!('}') >> (
      builder.unwrap().into_conf()
    )
  )
}

/// Parses several tool configurations.
fn tool_confs<'a>(
  bytes: & 'a [u8]
) -> IResult<
  & 'a [u8], ( Vec<String>, Res<ValdConf>, Vec<Res<ToolConf>> )
> {
  do_parse!(
    bytes,
    spc_cmt >>
    opts: opt!(options) >>
    spc_cmt >>
    vald_conf: validator_conf >>
    spc_cmt >>
    vec: many1!(
      terminated!(tool_conf, spc_cmt)
    ) >> (
      ( opts.unwrap_or_else( Vec::new ), vald_conf, vec )
    )
  )
}

/// Parses tool configurations from some bytes.
pub fn work<'a>(_conf: & GConf, bytes: & 'a [u8]) -> Res<
  ( Vec<String>, ValdConf, Vec< ToolConf > )
> {
  match tool_confs(bytes) {
    IResult::Done(rest, (opts, vald_conf, tools)) => {
      let vald_conf = vald_conf ? ;
      if rest.is_empty() {
        let mut tool_confs = Vec::with_capacity(tools.len()) ;
        for tool in tools {
          let mut tool_conf = tool ? ;
          tool_confs.push(tool_conf)
        }
        Ok( (opts, vald_conf, tool_confs) )
      } else {
        bail!("conf file parse error: could not parse whole file")
      }
    },
    IResult::Error(e) => bail!(
      format!("conf file parse error: `{:?}`", e)
    ),
    IResult::Incomplete(_) => bail!("conf file parse error: incomplete"),
  }
}






named!{
  #[doc = "Parses an unsigned integer, accepts sequences of zeros."],
  uint<Res<& str>>, alt_complete!(
    map!(
      re_bytes_find!("[0-9][0-9]*"),
      |bytes| from_utf8(bytes).chain_err(
        || "expected integer"
      )
    ) |
    map!( tag!(""), |_| Err( "expected integer".into() ))
  )
}

named!{
  #[doc = "Parses a duration."],
  duration< Res<Duration> >, do_parse!(
    secs: uint >>
    char!('.') >>
    nanos: uint >> (
      secs.and_then(
        |secs| nanos.map(
          |nanos| (secs, nanos)
        )
      ).and_then(
        |(secs, nanos)| u64::from_str(secs).chain_err(
          || format!("expected integer (u64, seconds), got `{}`", secs)
        ).and_then(
          |secs| u32::from_str(nanos).chain_err(
            || format!("expected integer (u32, nanos), got `{}`", secs)
          ).map( |nanos| Duration::new(secs, nanos) )
        )
      )
    )
  )
}

named!{
  #[doc = "Parses an exit code."],
  code_opt< Res< Option<i32> > >, alt_complete!(
    map!( char!('?'), |_| Ok(None) ) |
    map!( signed_int, |int: Res<i32>| int.map(Some) )
  )
}

named!{
  #[doc = "Parses a BenchRes."],
  bench_res< Res<(usize, String, BenchRes)> >, do_parse!(
    index: uint >>
    spc_cmt >>
    char!('"') >>
    name: map!(
      re_bytes_find!(r#"^[^"]*"#),
      |bytes| from_utf8(bytes).chain_err(
        || "error during conversion to string"
      )
    ) >>
    char!('"') >>
    spc_cmt >>
    bench_res: alt_complete!(
      map!(
        tag!(data::timeout_res), |_| Ok(BenchRes::Timeout)
      ) |
      map!(
        tag!(data::error_res), |_| Ok(BenchRes::Error)
      ) |
      map!(
        duration, |d: Res<Duration>| d.map(
          |d| BenchRes::Success(d, None)
        )
      )
    ) >>
    spc_cmt >>
    code: code_opt >> (
      index.and_then(
        |index| usize::from_str(index).chain_err(
          || "expected integer"
        )
      ).and_then(
        |index| name.map(|name| (index, name))
      ).and_then(
        |(index, name)| code.map(
          |code| (index, name, code)
        )
      ).and_then(
        |(index, name, code)| bench_res.map(
          |mut res| {
            if let Some(code) = code {
              res.set_code(code)
            }
            (index, name.into(), res)
          }
        )
      )
    )
  )
}





/// Parses a dump file.
fn parse_dump<'a>(
  bytes: & 'a [u8], file: String, run_res: & mut RunRes
) -> IResult<& 'a [u8], Res<ToolRes>> {
  let mut benchs: HashMap<BenchIndex,_> = HashMap::new() ;
  do_parse!(
    bytes,
    spc_cmt >>
    tool: tool_conf >>
    spc_cmt >>
    vald_conf: validator_conf >>
    spc_cmt >>
    tag!(dump::timeout_key) >>
    spc_cmt >>
    char!(':') >>
    spc_cmt >>
    timeout: duration >>
    spc_cmt >>
    bench_lines: many0!(
      do_parse!(
        data: bench_res >>
        spc_cmt >> (
          data.and_then(
            |(index, name, res)| run_res.check_bench_index(
              index.into(), name
            ).map(
              |()| (index, res)
            )
          ).and_then(
            |(index, res)| {
              let prev = benchs.insert(index.into(), res) ;
              if prev.is_some() {
                Err(
                  format!("found 2 benchmarks with index `{}`", index).into()
                )
              } else {
                Ok(())
              }
            }
          )
        )
      )
    ) >> ({
      let mut res = Ok(()) ;
      for r in bench_lines {
        if res.is_err() { break }
        res = r
      }
      res.and_then(
        |_| timeout.and_then(
          |timeout| tool.and_then(
            |tool| vald_conf.map(
              |vald_conf| ToolRes::mk(
                tool, timeout, file, benchs, vald_conf
              )
            )
          )
        )
      )
    })
  )
}


/// Parses a dump file from some bytes.
pub fn dump(
  bytes: & [u8], file: String, run_res: & mut RunRes
) -> Res<ToolRes> {
  match parse_dump(bytes, file, run_res) {
    IResult::Done(rest, tool_res) => {
      if rest.is_empty() {
        tool_res
      } else {
        bail!(
          format!(
            "dump parse error: could not parse whole file: `{}`",
            String::from_utf8_lossy(rest)
          )
        )
      }
    },
    IResult::Error(e) => bail!(
      format!("dump parse error: `{:?}`", e)
    ),
    IResult::Incomplete(_) => bail!("dump parse error: incomplete"),
  }
}


