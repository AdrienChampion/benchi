//! Parser used by `benchi`.

use std::str::from_utf8 ;

use regex::Regex ;
use nom::{ IResult, multispace } ;

use common::* ;
use errors::* ;

/// A tool configuration (parsing version).
#[derive(Clone, Debug)]
pub struct ToolConfParsing {
  /// Tool name.
  pub name: Spnd<String>,
  /// Short name.
  pub short: Spnd<String>,
  /// Graph name.
  pub graph: Spnd<String>,
  /// Command (lines).
  pub cmd: Spnd< Vec<String> >,
  /// Optional validator.
  pub validator: Option< Spnd<String> >,
}
impl ToolConfParsing {
  /// Translates to the normal `ToolConf` structure.
  pub fn to_tool_conf(self) -> ToolConf {
    ToolConf {
      name: self.name.xtract(),
      short: self.short.xtract(),
      graph: self.graph.xtract(),
      cmd: self.cmd.xtract(),
      validator: self.validator.map(|v| v.xtract()),
    }
  }
}

macro_rules! byte_cnt {
  ($i:expr, $( $stuff:tt )+ ) => (
    map!( $i, $($stuff)+, |bytes: & [u8]| bytes.len() )
  ) ;
}

named!{
  comment<usize>, byte_cnt!( re_bytes_find!(r"^//[^\n]*\n") )
}


fn spc_cmt(
  bytes: & [u8]
) -> IResult<& [u8], usize> {
  let mut len = 0 ;
  map!(
    bytes,
    many1!(
      map!(
        alt_complete!(
          comment | byte_cnt!(multispace)
        ),
        |add| len += add
      )
    ),
    |_| len
  )
}

named!{
  opt_spc_cmt<usize>, map!(
    opt!( complete!(spc_cmt) ), |opt: Option<usize>| opt.unwrap_or(0)
  )
}

/// Raw string parser.
fn raw_string(bytes: & [u8], cnt: usize) -> IResult<& [u8], Spnd<String>> {
  map!(
    bytes,
    map_res!(
      do_parse!(
        tag!("```") >>
        raw: take_until_s!("```") >>
        tag!("```") >> (raw)
      ), |bytes| from_utf8(bytes).map(|s| (s, bytes.len()))
    ),
    |(s, len): (& str, usize)| Spnd::mk(s.to_string(), cnt, len + 6)
  )
}

/// Ident parser.
fn ident<'a>(
  bytes: & 'a [u8], cnt: usize
) -> IResult<& 'a [u8], Spnd<String>> {
  map_res!(
    bytes,
    re_bytes_find!(r"^[a-zA-Z0-9_\-.]+"),
    |bytes: & 'a [u8]| from_utf8(bytes).map(
      |s: & 'a str| ( Spnd::mk(s.to_string(), cnt, bytes.len()) )
    )
  )
}

/// Unquoted string parser. Parses anything but `\n{}"/`.
fn string<'a>(
  bytes: & 'a [u8], cnt: usize
) -> IResult<& 'a [u8], Spnd<String>> {
  map_res!(
    bytes,
    re_bytes_find!(r#"^[^\n{}"/]+"#),
    |bytes: & 'a [u8]| from_utf8(bytes).map(
      |s: & 'a str| ( Spnd::mk(s.trim().to_string(), cnt, bytes.len()) )
    )
  )
}

/// Quoted string parser.
fn quoted_string<'a>(
  bytes: & 'a [u8], cnt: usize
) -> IResult< & 'a [u8], Spnd< Vec<String> > > {
  let mut len = 2 ; // `2` because of the surrounding quotes.
  map!(
    bytes,
    delimited!(
      char!('"'),
      many0!(
        map_res!(
          do_parse!(
            bytes: is_not!("\n\"") >>
            nl: opt!( char!('\n') ) >> ({
              len += nl.map(|_| 1).unwrap_or(0) + bytes.len() ;
              bytes
            })
          ), |bytes| from_utf8(bytes).map(|s| s.trim().to_string())
        )
      ),
      char!('"')
    ),
    |vec: Vec<String>| Spnd::mk(vec, cnt, len)
  )
}

/// Parses some options.
fn options<'a>(
  bytes: & 'a [u8], cnt: usize
) -> IResult< & 'a [u8], Spnd< Vec<String> > > {
  let mut len = 0 ;
  do_parse!(
    bytes,
    map!( tag!("options"), |bytes: & [u8]| len += bytes.len() ) >>
    map!( opt_spc_cmt, |add| len += add ) >>
    map!( char!(':'), |_| len += 1 ) >>
    map!( opt_spc_cmt, |add| len += add ) >>
    opts: map!(
      apply!( quoted_string, cnt+len ),
      |q: Spnd< Vec<String> >| { len += q.len() ; q.xtract() }
    ) >> (
      Spnd::mk(opts, cnt, len)
    )
  )
}

/// Parses a tool conf field.
fn tool_conf_field<'a, 'b>(
  bytes: & 'a [u8], builder: & 'b mut ToolConfBuilder, cnt: usize
) -> IResult< & 'a [u8], Res<usize> > {
  let mut len = 0 ;
  map!(
    bytes,
    alt_complete!(

      do_parse!(
        map!( tag!("short"), |b: & 'a [u8]| len = b.len() ) >>
        map!( opt_spc_cmt, |add| len += add ) >>
        map!( char!(':'), |_| len += 1 ) >>
        map!( opt_spc_cmt, |add| len += add ) >>
        short: apply!(ident, cnt + len) >> ({
          len += short.len() ;
          builder.set_short( short.xtract() )
        })
      ) |

      do_parse!(
        map!( tag!("cmd"), |b: & 'a [u8]| len = b.len() ) >>
        map!( opt_spc_cmt, |add| len += add ) >>
        map!( char!(':'), |_| len += 1 ) >>
        map!( opt_spc_cmt, |add| len += add ) >>
        cmd: apply!(quoted_string, cnt + len) >> ({
          len += cmd.len() ;
          let cmd = {
            let mut iter = cmd.xtract().into_iter() ;
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
        map!( tag!("graph"), |b: & 'a [u8]| len = b.len() ) >>
        map!( opt_spc_cmt, |add| len += add ) >>
        map!( char!(':'), |_| len += 1 ) >>
        map!( opt_spc_cmt, |add| len += add ) >>
        graph: apply!(string, cnt + len) >> ({
          len += graph.len() ;
          builder.set_graph( graph.xtract() )
        })
      ) |

      do_parse!(
        map!( tag!("validator"), |b: & 'a [u8]| len = b.len() ) >>
        map!( opt_spc_cmt, |add| len += add ) >>
        map!( char!(':'), |_| len += 1 ) >>
        map!( opt_spc_cmt, |add| len += add ) >>
        validator: apply!(raw_string, cnt + len) >> ({
          len += validator.len() ;
          builder.set_validator( validator.xtract() )
        })
      )

    ),
    |res: Res<()>| res.map(|()| len)
  )
}

/// Parses a tool configuration.
fn tool_conf<'a>(
  bytes: & 'a [u8], cnt: usize
) -> IResult< & 'a [u8], (Res<ToolConf>, usize) > {
  let mut len = 0 ;
  let mut builder = None ;
  do_parse!(
    bytes,
    map!(
      apply!(string, cnt + len),
      |name: Spnd<String>| {
        len += name.len() ;
        builder = Some(
          ToolConfBuilder::of_name( name.xtract() )
        )
      }
    ) >>
    map!( opt_spc_cmt, |add| len += add ) >>
    map!( char!('{'), |_| len += 1 ) >>
    map!( opt_spc_cmt, |add| len += add ) >>
    many0!(
      terminated!(
        apply!(
          tool_conf_field, builder.as_mut().unwrap(), cnt + len
        ),
        map!( opt_spc_cmt, |add| len += add )
      )
    ) >>
    map!( char!('}'), |_| len += 1 ) >> (
      (builder.unwrap().to_conf(), len)
    )
  )
}

/// Parses several tool configurations.
fn tool_confs<'a>(
  bytes: & 'a [u8]
) -> IResult<
  & 'a [u8], ( Vec<String>, Vec<Res<ToolConf>> )
> {
  let mut cnt = 0 ;
  do_parse!(
    bytes,
    map!( opt_spc_cmt, |add| cnt += add ) >>
    opts: opt!(
      map!(
        apply!( options, cnt ),
        |opts: Spnd< Vec<String> >| { cnt += opts.len() ; opts }
      )
    ) >>
    map!( opt_spc_cmt, |add| cnt += add ) >>
    vec: many1!(
      map!(
        terminated!(
          apply!(tool_conf, cnt),
          map!( opt_spc_cmt, |add| cnt += add )
        ), |(tool, len)| { cnt += len ; tool }
      )
    ) >> (
      ( opts.map(|o| o.xtract()).unwrap_or(vec![]), vec )
    )
  )
}


lazy_static!{
  static ref cmd_regex: Regex = Regex::new(
    r"([^\s]*)"
  ).unwrap() ;
}

/// Parses tool configurations from some bytes.
pub fn work<'a>(conf: & GConf, bytes: & 'a [u8]) -> Res<
  ( Vec<String>, Vec< ToolConf > )
> {
  match tool_confs(bytes) {
    IResult::Done(rest, (opts, tools)) => {
      if rest.is_empty() {
        let mut tool_confs = Vec::with_capacity(tools.len()) ;
        for tool in tools.into_iter() {
          let mut tool_conf = tool ? ;
          let cmd = {
            let mut cmd = vec![] ;
            assert_eq!(tool_conf.cmd.len(), 1) ;
            let str_cmd = & tool_conf.cmd[0] ;
            let mut iter = cmd_regex.find_iter(str_cmd) ;
            if let Some(first) = iter.next() {
              cmd.push( first.as_str().to_string() ) ;
              for next in iter {
                cmd.push( next.as_str().to_string() )
              }
            } else {
              bail!(
                format!(
                  "command for tool {} is illegal", conf.emph(& tool_conf.name)
                )
              )
            }
            cmd
          } ;
          tool_conf.cmd = cmd ;
          tool_confs.push(tool_conf)
        }
        Ok( (opts, tool_confs) )
      } else {
        bail!("conf file parse error: could not parse whole file")
      }
    },
    IResult::Error(e) => bail!(
      format!("conf file parse error: `{:?}`", e)
    ),
    IResult::Incomplete(_) => bail!(
      format!("conf file parse error: incomplete")
    ),
  }
}
