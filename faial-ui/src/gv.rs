use std::str::FromStr;
use std::fs::File;
use std::io::{self, BufRead};
use std::num::ParseIntError;
use std::convert::TryFrom;
use std::convert::From;
use std::io::Lines;

#[derive(Debug,Clone,Eq,PartialEq)]
pub enum GvResult {
    Pass,
    Fail(String)
}

impl FromStr for GvResult {
    type Err = ();
    fn from_str(line: &str) -> Result<Self, Self::Err> {
        if line.starts_with("//xfail:") {
            let mut fail_it = line.splitn(2, ':');
            fail_it.next().unwrap();
            let arg:&str = fail_it.next().unwrap();
            Ok(GvResult::Fail(arg.to_string()))
        } else if line == "//pass" {
            Ok(GvResult::Pass)
        } else {
            Err(())
        }
    }
}

#[test]
fn test_gv_ok_err() {
    assert_eq!("//pass".parse::<GvResult>(), Ok(GvResult::Pass));
    assert_eq!("//passx".parse::<GvResult>(), Err(()));
    assert_eq!("//xfail:FOO".parse::<GvResult>(), Ok(GvResult::Fail("FOO".to_string())));
    assert_eq!("//xfail:HELLO WORLD".parse::<GvResult>(), Ok(GvResult::Fail("HELLO WORLD".to_string())));
}

#[derive(Debug,Clone,Eq,PartialEq)]
pub struct GvArgs {
    pub block_dim: Vec<usize>,
    pub grid_dim: Vec<usize>,
    pub defines: Vec<String>,
    pub unknown: Vec<String>,
}

impl GvArgs {
    fn new() -> Self {
        GvArgs {
            block_dim: Vec::new(),
            grid_dim: Vec::new(),
            defines: Vec::new(),
            unknown: Vec::new(),
        }
    }
}

fn parse_dims(mut arg:&str) -> Result<Vec<usize>,String> {
    if let Ok(res) = arg.parse::<usize>() {
        return Ok(vec![res]);
    }
    if let Some(rest) = arg.strip_prefix("[") {
        if let Some(rest) = rest.strip_suffix("]") {
            arg = rest;
        } else {
            return Err(format!("parse_dims: parse error: {}", arg));
        }
    } else {
        return Err(format!("parse_dims: parse error: {}", arg));
    }
    let parts:Result<Vec<usize>,ParseIntError> = arg
        .split(",")
        .map(|x| x.trim().parse::<usize>())
        .collect::<Result<Vec<usize>,ParseIntError>>();
    if let Ok(parts) = parts {
        Ok(parts)
    } else {
        Err(format!("Unexpected arguments: {}", arg))
    }
}

#[test]
fn test_parse_dims() {
    assert_eq!(parse_dims("[64,64,64]"), Ok(vec![64,64,64]));
    assert_eq!(parse_dims("1"), Ok(vec![1]));
    assert_eq!(parse_dims("1024"), Ok(vec![1024]));
    assert_eq!(parse_dims("[1,1]"), Ok(vec![1,1]));
}

impl From<Vec<String>> for GvArgs {
    fn from(args:Vec<String>) -> Self {
        let mut result = GvArgs::new();
        let mut it = args.iter();
        while let Some(arg) = it.next() {
            if let Some(arg1) = arg.strip_prefix("--gridDim=") {
                if let Ok(arg1) = parse_dims(arg1) {
                    result.grid_dim = arg1;
                    continue;
                }
            } else if let Some(arg1) = arg.strip_prefix("--blockDim=") {
                if let Ok(arg1) = parse_dims(arg1) {
                    result.block_dim = arg1;
                    continue;
                }
            } else if let Some(arg) = arg.strip_prefix("-D") {
                result.defines.push(arg.to_string());
                continue;
            }
            result.unknown.push(arg.to_string());
        }
        result
    }
}

#[test]
fn test_gv_args_from() {
    assert_eq!(GvArgs::from(Vec::new()), GvArgs::new());
    let mut args = Vec::new();
    args.push("--gridDim=[1,1,1]".to_string());
    args.push("--blockDim=[1024,1,1]".to_string());
    args.push("-DUNROLL_REDUCTION".to_string());
    args.push("-D__BLOCK_DIM_1=32".to_string());
    args.push("--no-inline".to_string());
    let expected = GvArgs {
        grid_dim: vec![1,1,1],
        block_dim: vec![1024,1,1],
        unknown: vec!["--no-inline".to_string()],
        defines: vec!["UNROLL_REDUCTION".to_string(), "__BLOCK_DIM_1=32".to_string()],
    };
    assert_eq!(GvArgs::from(args), expected);
}

impl From<&str> for GvArgs {
    fn from(args:&str) -> Self {
        if let Some(args) = shlex::split(args) {
            GvArgs::from(args)
        } else {
            let mut result = GvArgs::new();
            result.unknown.push(args.to_string());
            result
        }
    }
}

#[test]
fn test_parse_gv_args() {
    let gv = GvArgs{
        grid_dim:vec![256],
        block_dim:vec![256],
        unknown:vec!["--warp-sync=32".to_string()],
        defines:vec!["UNROLL_REDUCTION".to_string()],
    };
    let given = GvArgs::from("--gridDim=256 --blockDim=256 --warp-sync=32 -DUNROLL_REDUCTION");
    assert_eq!(given, gv);
}

#[derive(Debug,Clone,Eq,PartialEq)]
pub struct GvMetadata {
    pub expect: GvResult,
    pub args: GvArgs,
}

impl GvMetadata {
    fn parse_lines<B: BufRead>(mut count:usize, mut it:Lines<B>) -> Result<Self, ()> {
        while let Some(Ok(line)) = it.next() {
            if count <= 0 {
                break;
            }
            let line:&str = line.trim();
            if let Ok(gvr) = line.parse::<GvResult>() {
                if let Some(Ok(args)) = it.next() {
                    if let Some(args) = args.strip_prefix("//") {
                        // get second line
                        return Ok(GvMetadata{
                            expect: gvr,
                            args: GvArgs::from(args),
                        });
                    }
                }
            }
            count -= 1;
        }
        Err(())
    }
}

impl<B: BufRead> TryFrom<Lines<B>> for GvMetadata {
    type Error = ();
    fn try_from(lines:Lines<B>) -> Result<Self, Self::Error> {
        GvMetadata::parse_lines(2, lines)
    }
}

#[test]
fn test_parse_metadata() {
    let cursor = io::Cursor::new(b"//pass\n//--gridDim=256 --blockDim=256 --warp-sync=32 -DUNROLL_REDUCTION\nbar");
    let gv = GvMetadata {
        expect : GvResult::Pass,
        args: GvArgs{
            grid_dim:vec![256],
            block_dim:vec![256],
            unknown:vec!["--warp-sync=32".to_string()],
            defines:vec!["UNROLL_REDUCTION".to_string()],
        }
    };
    assert_eq!(GvMetadata::try_from(cursor.lines()).unwrap(), gv);
    let cursor = io::Cursor::new(b"//hello world!\n//pass\n//--gridDim=256 --blockDim=256 --warp-sync=32 -DUNROLL_REDUCTION\nbar");
    let gv = GvMetadata {
        expect : GvResult::Pass,
        args: GvArgs{
            grid_dim:vec![256],
            block_dim:vec![256],
            unknown:vec!["--warp-sync=32".to_string()],
            defines:vec!["UNROLL_REDUCTION".to_string()],
        }
    };
    assert_eq!(GvMetadata::parse_lines(5, cursor.lines()).unwrap(), gv);
}

impl GvMetadata {
    pub fn parse_file(filename:String) ->Option<GvMetadata> {
        let file = File::open(filename).unwrap();
        let reader = io::BufReader::new(file);
        let it = reader.lines();
        if let Ok(gv) = GvMetadata::try_from(it) {
            Some(gv)
        } else {
            None
        }
    }
}
