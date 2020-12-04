#[macro_use]
extern crate prettytable;

use std::fs::File;
use std::io::{self, BufRead};
use std::collections::HashMap;
use std::str::FromStr;
use std::process::ExitStatus;

use clap::{App,Arg,ArgMatches};
use duct::{Expression, cmd};

mod parser;
mod gv;
use parser::DataRaceFreedom;

#[derive(Debug)]
pub struct Proofs<R> {
    reader: R,
}

impl<R> Proofs<R> {
    pub fn new(reader: R) -> Proofs<R> {
        Proofs {reader: reader }
    }
}

impl<B: BufRead> Iterator for Proofs<B> {
    type Item = io::Result<String>;

    fn next(&mut self) -> Option<io::Result<String>> {
        let mut buf = String::new();
        loop {
            match self.reader.read_line(&mut buf) {
                Ok(n) => {
                    if n == 0 {
                        // No more bytes available
                        if buf.len() == 0 {
                            // Nothing read before, we are done
                            return None
                        } else {
                            // We are done!
                            return Some(Ok(buf))
                        }
                    }
                    if buf.ends_with("(pop 1)\n") || buf.ends_with("(pop 1)\r\n") {
                        return Some(Ok(buf));
                    }
                }
                Err(e) => return Some(Err(e)),
            }
        }
    }
}

#[test]
fn proofs() {
    let buf = io::Cursor::new(&b"hello\n(pop 1)\nworld"[..]);
    let mut s = Proofs::new(buf);
    assert_eq!(s.next().unwrap().unwrap(), "hello\n(pop 1)\n".to_string());
    assert_eq!(s.next().unwrap().unwrap(), "world".to_string());
    assert!(s.next().is_none());
}

#[allow(dead_code)]
fn get_proofs(filename:&str) -> Vec<String> {
    let file = File::open(filename).unwrap();
    let reader = io::BufReader::new(file);
    return Proofs::new(reader).collect::<Result<_, _>>().unwrap();
}

#[allow(dead_code)]
fn chunk_lines (lines:Vec<String>, count:usize) -> Vec<String> {
    let count = lines.len() / count;
    return lines
        .chunks(count)
        .map(|x| x.join(""))
        .collect();
}

#[derive(Debug)]
enum AnalysisStatus {
    Pass,
    Abort(i32),
}

impl From<ExitStatus> for AnalysisStatus {
    fn from(status:ExitStatus) -> Self {
        if status.success() {
            AnalysisStatus::Pass
        } else {
            if let Some(x) = status.code() {
                AnalysisStatus::Abort(x)
            } else {
                AnalysisStatus::Abort(255)
            }
        }
    }
}

#[derive(Debug)]
struct Cmd {
    args: Vec<String>,
    unchecked: bool,
}

impl Cmd {

    fn to_string(self:&Self) -> String {
        self.args.join(" ")
    }

    fn checked(args:Vec<String>) -> Self {
        Cmd {
            args: args,
            unchecked: false,
        }
    }

    fn unchecked(args:Vec<String>) -> Self {
        Cmd {
            args: args,
            unchecked: true,
        }
    }

    fn to_expr(self:&Self) -> Expression {
        let mut it = self.args.iter();
        let head = it.next().unwrap();
        let rest:Vec<String> = it.map(|x| x.clone()).collect();
        let expr = cmd(head, rest);
        if self.unchecked { expr.unchecked() } else { expr }
    }
}


trait CommandBuilder {
    type Error;
    fn new(exec:Vec<Cmd>) -> Self;
    fn spawn(self) -> Result<CommandOutput, Self::Error>;
}

struct CommandOutput {
    stdout: String,
    stderr: String,
    success: bool,
}


fn cmd_to_string(children:&Vec<Cmd>) -> String {
    children.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(" | ")
}

struct DuctBuilder(Expression);

impl CommandBuilder for DuctBuilder {
    type Error = io::Error;

    fn new(children:Vec<Cmd>) -> Self {
        let mut it = children.iter();
        let mut c = it.next().unwrap().to_expr();
        while let Some(elem) = it.next() {
            c = c.pipe(elem.to_expr());
        }
        DuctBuilder(c)
    }

    fn spawn(self) -> Result<CommandOutput, Self::Error> {
        match self.0.stdout_capture().run() {
            Ok(output) => Ok(CommandOutput {
                stdout: String::from_utf8_lossy(&output.stdout).into_owned(),
                stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
                success: output.status.success(),
            }),
            Err(e) => Err(e),
        }
    }
}

#[derive(Debug,Clone,Eq,PartialEq)]
enum Stage {
    Parse,
    Infer,
    Analyze,
    Solve,
}

struct StageIter(Option<Stage>);

impl Iterator for StageIter {
    type Item = Stage;

    fn next(&mut self) -> Option<Stage> {
        match &self.0 {
            Some(Stage::Parse) =>
                std::mem::replace(&mut self.0, Some(Stage::Infer)),
            Some(Stage::Infer) =>
                std::mem::replace(&mut self.0, Some(Stage::Analyze)),
            Some(Stage::Analyze) =>
                std::mem::replace(&mut self.0, Some(Stage::Solve)),
            Some(Stage::Solve) =>
                std::mem::replace(&mut self.0, None),
            None => None
        }
    }
}

impl Stage {
    fn iter(&self) -> StageIter {
        StageIter(Some(self.clone()))
    }
}

#[derive(Debug,Eq,PartialEq)]
enum InputType {
    CUDA,
    PROTO,
    CJSON,
    PJSON,
    SMT
}

impl InputType {
    fn from_filename(filename:&str) -> Self {
        let curr_ext = filename.rsplitn(2, ".").collect::<Vec<_>>();
        let curr_ext = curr_ext.get(0);
        if curr_ext.is_none() {
            return InputType::CUDA;
        }
        let mut curr_ext = String::from(*curr_ext.unwrap());
        curr_ext.make_ascii_lowercase();
        let curr_ext = curr_ext;
        for ext in &["cu", "c", "cpp", "h", "hpp"] {
            if curr_ext.as_str() == *ext {
                return InputType::CUDA;
            }
        }
        if curr_ext == "proto" {
            return InputType::PROTO;
        }
        if curr_ext == "smt2" || curr_ext == "smt" {
            return InputType::SMT;
        }
        if curr_ext == "json" {
            return InputType::CJSON;
        }
        return InputType::CUDA;
    }

    fn as_stage(&self) -> Stage {
        match self {
            InputType::CUDA => Stage::Parse,
            InputType::CJSON => Stage::Infer,
            InputType::PJSON | InputType::PROTO => Stage::Analyze,
            InputType::SMT => Stage::Solve,
        }
    }

    fn as_str(&self) -> &str {
        match self {
            InputType::CUDA => "cuda",
            InputType::PROTO => "proto",
            InputType::CJSON => "cjson",
            InputType::PJSON => "pjson",
            InputType::SMT => "smt",
        }
    }

    fn values() -> [InputType; 5] {
        [
            InputType::CUDA,
            InputType::PROTO,
            InputType::CJSON,
            InputType::PJSON,
            InputType::SMT,
        ]
    }
}


impl FromStr for InputType {
    type Err = String;
    fn from_str(data: &str) -> Result<Self, String> {
        match data {
            "cuda" => Ok(InputType::CUDA),
            "proto" => Ok(InputType::PROTO),
            "cjson" => Ok(InputType::CJSON),
            "pjson" => Ok(InputType::PJSON),
            "smt" => Ok(InputType::SMT),
            x => Err(format!("Unknown format: {}", x)),
        }
    }
}


#[derive(Debug)]
struct Faial {
    solve_only: bool,
    expect_race: bool,
    expect_invalid: bool,
    input: Option<String>,
    analyze_only: bool,
    infer_only: bool,
    stage: Stage,
    analyze_json: bool,
    skip_typecheck: bool,
    infer_output_json: bool,
    block_dim: Vec<usize>,
    grid_dim: Vec<usize>,
    verbose: bool,
    defines: HashMap<String,u32>,
    input_type: InputType,
    dry_run: bool,
    internal_steps: Option<u8>,
}

impl Faial {
    fn get_command(&self, stage:Stage, filename:Option<String>) -> Cmd {
        match stage {
            Stage::Parse => {
                let mut cmd = Vec::new();
                cmd.push("cu-to-json".to_string());
                if let Some(filename) = filename {
                    cmd.push(filename);
                } else {
                    cmd.push("/dev/stdin".to_string());
                }
                Cmd::checked(cmd)
            },
            Stage::Infer => {
                let mut cmd = Vec::new();
                cmd.push("faial-infer".to_string());
                if self.infer_only {
                    if let Some(lvl) = self.internal_steps {
                        // Set the level of the analysis, example: -3
                        cmd.push("-X".to_string());
                        cmd.push(format!("{}", lvl));
                    }
                    if self.infer_output_json {
                        cmd.push("--provenance".to_string());
                        cmd.push("-t".to_string());
                        cmd.push("json".to_string());
                    }
                } else {
                    cmd.push("--provenance".to_string());
                    cmd.push("-t".to_string());
                    cmd.push("json".to_string());
                }
                if let Some(filename) = filename {
                    cmd.push(filename);
                } else {
                    cmd.push("-".to_string());
                }
                Cmd::checked(cmd)
            },
            Stage::Analyze => {
                let mut cmd = vec!["faial-bin".to_string()];
                if self.analyze_only {
                    if let Some(lvl) = self.internal_steps {
                        // Set the level of the analysis, example: -3
                        cmd.push(format!("-{}", lvl));
                    }
                }
                if self.expect_invalid {
                    cmd.push("--expect-invalid".to_string());
                }
                let field = vec!["x", "y", "z"];
                for (idx, d) in self.grid_dim.iter().enumerate() {
                    cmd.push(format!("-DgridDim.{}={}", field.get(idx).unwrap(), d));
                }
                for (idx, d) in self.block_dim.iter().enumerate() {
                    cmd.push(format!("-DblockDim.{}={}", field.get(idx).unwrap(), d));
                }
                for (k,v) in &self.defines {
                    cmd.push(format!("-D{}={}", k, v));
                }
                if self.skip_typecheck {
                    cmd.push("--skip-type-check".to_string());
                }
                if self.analyze_json {
                    cmd.push("--json".to_string());
                }
                if let Some(filename) = filename {
                    cmd.push(filename);
                }
                Cmd::checked(cmd)
            },
            Stage::Solve => {
                let mut args = vec!["z3".to_string()];
                if let Some(filename) = filename {
                    args.push("--".to_string());
                    args.push(filename);
                } else {
                    args.push("-in".to_string());
                }
                Cmd::unchecked(args)
            },
        }
    }

    fn get_stages(&self) -> Vec<Stage> {
        let mut last = Stage::Solve;
        if self.infer_only {
            last = Stage::Infer;
        } else if self.analyze_only || self.expect_invalid {
            last = Stage::Analyze;
        }
        let mut stages = Vec::new();
        for x in self.stage.clone().iter() {
            if x == last {
                stages.push(x.clone());
                break;
            } else {
                stages.push(x.clone());
            }
        }
        return stages;
    }

    fn last_stage(&self) -> Stage {
        let stages = self.get_stages();
        stages.get(stages.len() - 1).unwrap().clone()
    }

    fn get_pipe(&self) -> Vec<Cmd> {
        let mut pipe = Vec::new();
        for stage in self.get_stages() {
            let filename = if stage == self.stage {
                self.input.clone()
            } else {
                None
            };
            pipe.push(self.get_command(stage, filename));
        }
        pipe
    }
}

fn get_vec<'a,T>(matches:&ArgMatches<'a>, name:&str) -> Result<Vec<T>,T::Err>
where T : FromStr {
    match matches.values_of(name) {
        Some(x) => x.map(|x| x.to_string().parse::<T>()).collect(),
        None => Ok(Vec::new()),
    }
}

fn can_parse<T>(value:String) -> Result<(),String>
where T : FromStr {
    if value.parse::<T>().is_ok() {
        Ok(())
    } else {
        Err(format!("Could not parse argument"))
    }
}

#[allow(dead_code)]
fn parse<'a,T>(matches:&ArgMatches<'a>, name:&str) -> T
where T : FromStr {
    let x = matches.value_of(name).unwrap();
    x.parse::<T>().ok().unwrap()
}

fn parse_opt<'a,T>(matches:&ArgMatches<'a>, name:&str) -> Option<T>
where T : FromStr {
    matches.value_of(name).map(|x| x.parse::<T>().ok().unwrap())
}

fn is_key_val(v: String) -> Result<(), String> {
    if v.matches("=").count() != 1 {
        return Err(
            String::from(
                "Expecting exactly one equals sign, example: 'key=13'."
            )
        );
    }
    let vs = v.split("=").collect::<Vec<_>>();
    let key = vs.get(0).unwrap();
    if key.len() == 0 {
        return Err(String::from("Key must be nonempty."));
    }
    let value = vs.get(1).unwrap();
    if value.parse::<u32>().is_ok() {
        return Ok(());
    }
    Err(String::from("Value assigned to key must be an unsigned integer."))
}

fn parse_key_val<'a>(matches:&ArgMatches<'a>, name:&str) -> HashMap<String, u32> {
    match matches.values_of(name) {
        Some(vs) => {
            let mut kvs = HashMap::new();
            for kv in vs {
                let kv = kv.split("=").collect::<Vec<_>>();
                let key = String::from(*kv.get(0).unwrap());
                let value = String::from(*kv.get(1).unwrap()).parse::<u32>().ok().unwrap();
                kvs.insert(key, value);
            }
            kvs
        },
        None => HashMap::new(),
    }
}


impl Faial {

    fn new() -> Self {
        let inp_choices = InputType::values();
        let inp_choices : Vec<&str> = inp_choices.iter().map(|x| x.as_str()).collect();
        let version = format!("1.0-rev-{}", env!("VERGEN_SHA"));
        let app = App::new("faial")
                .version(version.as_str())
                .about("Checks if a GPU code is data-race free")
                .author("UMB-SVL research group: https://umb-svl.gitlab.io/")
                .arg(Arg::with_name("expect_race")
                    .long("expect-race")
                    .help("Sets exit status according to finding data-races.")
                    .conflicts_with("solve_only")
                    .conflicts_with("infer_only")
                    .conflicts_with("analyze_only")
                )
                .arg(Arg::with_name("steps")
                    .long("steps")
                    .takes_value(true)
                    .validator(can_parse::<u8>)
                    .help("Each internal phase (eg, inference) can have multiple internal steps. Use this option to control how many steps to run. Default: run the whole stage until the end.")
                )
                .arg(Arg::with_name("expect_invalid")
                    .long("expect-invalid")
                    .help("Sets exit status according to finding invalid code.")
                    .conflicts_with("solve_only")
                    .conflicts_with("infer_only")
                )
                .arg(Arg::with_name("analyze_only")
                    .long("analyze-only")
                    .short("A")
                    .help("Halts after analysis")
                    .conflicts_with("solve_only")
                    .conflicts_with("infer_only")
                )
                .arg(Arg::with_name("solve_only")
                    .long("solve-only")
                    .short("S")
                    .help("Halts after invoking solver")
                    .conflicts_with("infer_only")
                    .conflicts_with("analyze_only")
                )
                .arg(Arg::with_name("infer_only")
                    .long("infer-only")
                    .short("I")
                    .help("Halts after model inference")
                    .conflicts_with("solve_only")
                    .conflicts_with("analyze_only")
                )
                .arg(Arg::with_name("verbose")
                    .long("verbose")
                    .help("Shows more information")
                )
                .arg(Arg::with_name("dry_run")
                    .long("dry-run")
                    .help("Prints the sequence of programs being run internally and exits")
                )
                .arg(Arg::with_name("infer_output_json")
                    .long("--infer-output-json")
                    .help("Outputs the result of inference as a PJSON format.")
                )
                .arg(Arg::with_name("input_type")
                    .long("type")
                    .short("t")
                    .help("Start at a given point of the pipeline")
                    .takes_value(true)
                    .possible_values(inp_choices.as_slice())
                )
                .arg(Arg::with_name("grid_dim")
                    .help("Sets the 'gridDim' variable (first 'x', then 'y', then 'z')")
                    .long("grid-dim")
                    .multiple(true)
                    .value_delimiter(",")
                    .short("g")
                    .takes_value(true)
                    .min_values(0)
                    .max_values(3)
                    .conflicts_with("infer_only")
                )
                .arg(Arg::with_name("block_dim")
                    .help("Sets the 'blockDim' variable (first 'x', then 'y', then 'z')")
                    .long("block-dim")
                    .short("b")
                    .value_delimiter(",")
                    .multiple(true)
                    .takes_value(true)
                    .min_values(0)
                    .max_values(3)
                    .conflicts_with("infer_only")
                )
                .arg(Arg::with_name("parse_gv_args")
                    .long("parse-gv-args")
                    .help("Try to parse GPUVerify arguments present in the input file.")
                )
                .arg(Arg::with_name("defines")
                    .help("Sets a variable")
                    .short("-D")
                    .long("define")
                    .multiple(true)
                    .takes_value(true)
                    .validator(is_key_val)
                    .min_values(0)
                )
                .arg(Arg::with_name("input")
                    .help("The code being checked for data-race freedom")
                    .takes_value(true)
                    .index(1)
                );

        let matches = app.clone().get_matches();
        let input : Option<String> = matches.value_of("input").map(|x| x.to_string());
        let guessed = InputType::from_filename(input.clone().unwrap_or(String::from("")).as_str());
        let input_type = parse_opt::<InputType>(&matches, "input_type").unwrap_or(guessed);
        let stage = input_type.as_stage();
        if stage == Stage::Parse && input.is_none() {
            eprintln!("Error: filename required when parsing a CUDA file. Change file type or supply filename.\n");
            let mut out = io::stderr();
            app.write_help(&mut out).expect("failed to write to stdout");
            std::process::exit(255);
        }
        let analyze_json = match stage {
            Stage::Infer | Stage::Parse => true,
            _ => input_type == InputType::CJSON,
        };
        let mut opts = Faial {
            expect_race: matches.is_present("expect_race"),
            expect_invalid: matches.is_present("expect_invalid"),
            solve_only: matches.is_present("solve_only"),
            input: input,
            analyze_only: matches.is_present("analyze_only"),
            infer_only: matches.is_present("infer_only"),
            grid_dim: get_vec(&matches, "grid_dim").unwrap(),
            block_dim: get_vec(&matches, "block_dim").unwrap(),
            stage: stage,
            internal_steps: parse_opt::<u8>(&matches, "steps"),
            infer_output_json: matches.is_present("infer_output_json"),
            analyze_json: analyze_json,
            verbose: matches.is_present("verbose"),
            defines: parse_key_val(&matches, "defines"),
            input_type: input_type,
            dry_run: matches.is_present("dry_run"),
            skip_typecheck: true,
        };
        if matches.is_present("parse_gv_args") && opts.input_type == InputType::CUDA {
            let opts = &mut opts;
            if let Some(args) = gv::GvMetadata::parse_file(String::from(opts.input.as_ref().unwrap())) {
                if opts.block_dim.len() == 0 {
                    opts.block_dim = args.args.block_dim;
                }
                if opts.grid_dim.len() == 0 {
                    opts.grid_dim = args.args.grid_dim;
                }
            }
        }
        opts
    }

    fn handle_data(self, data:CommandOutput) {
        match self.last_stage() {
            Stage::Solve => {
                let buffer = data.stdout;
                if self.solve_only {
                    print!("{}", buffer);
                    eprint!("{}", data.stderr);
                } else {
                    if buffer.len() > 0 {
                        match buffer.parse::<DataRaceFreedom>() {
                            Ok(d) => {
                                d.print();
                                if self.expect_race ^ d.is_drf() {
                                    return
                                }
                            },
                            Err(e) => eprintln!("Error parsing solver output: {}", e.to_string()),
                        }
                    } else {
                        eprint!("{}", data.stderr);
                    }
                }
                std::process::exit(1);
            },
            _ => {
                if ! self.expect_invalid {
                    print!("{}", data.stdout);
                }
                eprint!("{}", data.stderr);
                if ! data.success {
                    std::process::exit(255);
                }
            }
        }
    }
}

fn main() {
    let faial = Faial::new();
    let pipe = faial.get_pipe();
    let pipe_str = cmd_to_string(&pipe);
    let pipe = DuctBuilder::new(pipe);
    if faial.dry_run || faial.verbose {
        eprintln!("RUN {}", pipe_str);
        if faial.dry_run {
            return
        }
    }
    match pipe.spawn() {
        Ok(data) => faial.handle_data(data),
        Err(e) => {
            eprintln!("Internal error running: {}\nReason: {}", pipe_str, e);
            std::process::exit(255);
        },
    }
}
