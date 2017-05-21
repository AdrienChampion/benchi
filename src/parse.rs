//! Parser used by `benchi`.

use std::str::from_utf8 ;

use regex::Regex ;
use nom::{ IResult, multispace } ;

use common::* ;
use errors::* ;

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
    opt!(spc_cmt), |opt: Option<usize>| opt.unwrap_or(0)
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

/// Parses a tool configuration.
fn tool_conf<'a>(
  bytes: & 'a [u8], cnt: usize
) -> IResult< & 'a [u8], (ToolConf, usize) > {
  let mut len = 0 ;
  do_parse!(
    bytes,
    name: map!(
      apply!(string, cnt + len),
      |name: Spnd<String>| {
        len += name.len() ;
        name
      }
    ) >>
    map!( opt_spc_cmt, |add| len += add ) >>
    map!( char!('{'), |_| len += 1 ) >>
    map!( opt_spc_cmt, |add| len += add ) >>
    map!( tag!("short"), |b: & 'a [u8]| len += b.len() ) >>
    map!( opt_spc_cmt, |add| len += add ) >>
    map!( char!(':'), |_| len += 1 ) >>
    map!( opt_spc_cmt, |add| len += add ) >>
    short: map!(
      apply!(ident, cnt + len),
      |short: Spnd<String>| {
        len += short.len() ;
        short
      }
    ) >>
    map!( opt_spc_cmt, |add| len += add ) >>
    graph: opt!(
      do_parse!(
        map!( tag!("graph"), |b: & 'a [u8]| len += b.len() ) >>
        map!( opt_spc_cmt, |add| len += add ) >>
        map!( char!(':'), |_| len += 1 ) >>
        map!( opt_spc_cmt, |add| len += add ) >>
        graph: apply!(string, cnt + len) >>
        map!( opt_spc_cmt, |add| len += add ) >>
        ({ len += graph.len() ; graph })
      )
    ) >>
    map!( tag!("cmd"), |b: & 'a [u8]| len += b.len() ) >>
    map!( opt_spc_cmt, |add| len += add ) >>
    map!( char!(':'), |_| len += 1 ) >>
    map!( opt_spc_cmt, |add| len += add ) >>
    cmd: map!(
      apply!(quoted_string, cnt + len),
      |cmd: Spnd< Vec<String> >| {
        len += cmd.len() ;
        cmd.map(
          |vec| {
            let mut iter = vec.into_iter() ;
            if let Some(mut s) = iter.next() {
              for next in iter {
                s = format!("{} {}", s, next)
              }
              vec![s]
            } else {
              vec![]
            }
          }
        )
      }
    ) >>
    map!( opt_spc_cmt, |add| len += add ) >>
    map!( char!('}'), |_| len += 1 ) >> ({
      let graph = graph.unwrap_or(name.clone()) ;
      (
        ToolConf { name, short, graph, cmd }, len
      )
    })
  )
}

/// Parses several tool configurations.
fn tool_confs<'a>(
  bytes: & 'a [u8]
) -> IResult< & 'a [u8], Vec< ToolConf > > {
  let mut cnt = 0 ;
  do_parse!(
    bytes,
    map!( opt_spc_cmt, |add| cnt += add ) >>
    vec: many1!(
      terminated!(
        map!(
          apply!(tool_conf, cnt),
          |(tool, len)| { cnt += len ; tool }
        ),
        map!( opt_spc_cmt, |add| cnt += add )
      )
    ) >> (vec)
  )
}


lazy_static!{
  static ref cmd_regex: Regex = Regex::new(
    r"([^\s]*)"
  ).unwrap() ;
}

/// Parses tool configurations from some bytes.
pub fn work<'a>(conf: & Conf, bytes: & 'a [u8]) -> Res<
  Vec< ToolConf >
> {
  match tool_confs(bytes) {
    IResult::Done(rest, mut res) => {
      if rest.is_empty() {
        for tool_conf in res.iter_mut() {
          if tool_conf.cmd.is_empty() {
            bail!(
              format!(
                "command for tool {} is empty", conf.emph(& tool_conf.name)
              )
            )
          }
          let cmd = {
            let mut cmd = vec![] ;
            assert_eq!(tool_conf.cmd.get().len(), 1) ;
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
          tool_conf.cmd.replace(cmd)
        }
        Ok(res)
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