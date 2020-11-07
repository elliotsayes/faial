use std::fs::File;
use std::io::{self, BufRead};
use std::io::prelude::*;
use std::process::{Command, Stdio, ChildStdout, Child};
use std::thread;
use std::collections::HashMap;
use sexp::{Sexp,Atom};
use std::str::FromStr;
use clap::{App,Arg,ArgMatches};
use std::convert::TryFrom;
use subprocess::{Exec,Redirection,ExitStatus, Pipeline, Popen, CaptureData, PopenError};

#[macro_use]
extern crate prettytable;
use prettytable::{Table, Row, Cell};

#[derive(Debug)]
pub struct Proofs<R> {
    reader: R,
}

impl<R> Proofs<R> {
    pub fn new(reader: R) -> Proofs<R> {
        Proofs {reader: reader }
    }
}
fn read_file(mut file:File) -> String {
    //file.sync_data().unwrap();
    //file.seek(std::io::SeekFrom::Start(0)).unwrap();
    let mut reader = io::BufReader::new(file);
    let mut buffer = String::new();
    reader.read_to_string(&mut buffer).expect("Failed loading output");
    buffer
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

fn get_proofs(filename:&str) -> Vec<String> {
    let file = File::open(filename).unwrap();
    let reader = io::BufReader::new(file);
    return Proofs::new(reader).collect::<Result<_, _>>().unwrap();
}

fn chunk_lines (lines:Vec<String>, count:usize) -> Vec<String> {
    let count = lines.len() / count;
    return lines
        .chunks(count)
        .map(|x| x.join(""))
        .collect();
}

#[derive(Debug)]
enum InputData {
    FromProc(ChildStdout),
    FromMemory(String),
    FromFile(String, File),
    FromStdin,
}

impl InputData {
    fn get_filename(&self) -> Option<String> {
        match self {
            | InputData::FromMemory(_)
            | InputData::FromProc(_)
            | InputData::FromStdin
            => None,
            InputData::FromFile(x, _) => Some(x.clone())
        }
    }

    fn get_stdio(self) -> (Stdio, Option<String>) {
        match self {
            InputData::FromMemory(data) => (Stdio::piped(), Some(data)),
            InputData::FromProc(p) => (Stdio::from(p), None),
            InputData::FromFile(p, f) => (Stdio::from(f), None),
            InputData::FromStdin => (Stdio::inherit(), None),
        }
    }

    fn update_exec(self, exec:Exec) -> Exec {
        match self {
            InputData::FromMemory(data) => exec.stdin(data.as_str()),
            InputData::FromProc(p) => exec,
            InputData::FromFile(p, f) => {
                exec.stdin(Redirection::File(f))
            },
            InputData::FromStdin => exec,
        }
    }
}

#[derive(Debug)]
enum AnalysisStatus {
    Fail,
    Pass,
    Abort(i32),
}

impl AnalysisStatus {
    fn get_status(self) -> i32 {
        match self {
            AnalysisStatus::Fail => 1,
            AnalysisStatus::Pass => 0,
            AnalysisStatus::Abort(x) => x,
        }
    }
}

impl From<std::process::ExitStatus> for AnalysisStatus {
    fn from(status:std::process::ExitStatus) -> Self {
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

#[derive(Debug, Clone, PartialEq)]
enum StreamCapture {
    Capture,
    Silent,
    Echo,
}

impl StreamCapture {
    fn as_stdio(self) -> Stdio {
        match self {
            StreamCapture::Capture => Stdio::piped(),
            StreamCapture::Silent => Stdio::null(),
            StreamCapture::Echo => Stdio::inherit(),
        }
    }
    fn as_redirection(&self) -> Option<Redirection> {
        match self {
            StreamCapture::Capture => Some(Redirection::Pipe),
            StreamCapture::Silent => Some(Redirection::None),
            StreamCapture::Echo => None,
        }
    }
}

#[derive(Debug)]
struct Run {
    args: Vec<String>,
    stdout: StreamCapture,
    stderr: StreamCapture,
}

fn echo_child(child:&mut Child, echo_stdout:bool, echo_stderr:bool) {
    if echo_stdout {
        let mut stdout = child.stdout.take().expect("Failed capturing output");
        let mut buffer = String::new();
        stdout.read_to_string(&mut buffer).expect("Failed loading output");
        print!("{}", buffer);
    }
    if echo_stderr {
        let mut stderr = child.stderr.take().expect("Failed capturing output");
        let mut buffer = String::new();
        stderr.read_to_string(&mut buffer).expect("Failed loading output");
        eprint!("{}", buffer);
    }
}
impl Run {
    fn new(args:Vec<String>) -> Self {
        Run {
            args: args,
            stdout: StreamCapture::Echo,
            stderr: StreamCapture::Echo,
        }
    }

    fn as_command(self) -> Command {
        let mut it = self.args.iter();
        let exe = it.next().expect("Run.args cannot be empty");
        let mut cmd = Command::new(exe);
        cmd.stdout(self.stdout.as_stdio());
        for arg in it {
            cmd.arg(arg);
        }
        cmd
    }

    fn as_exec(&self) -> Exec {
        let mut it = self.args.iter();
        let exe = it.next().expect("Run.args cannot be empty");
        let mut cmd = Exec::cmd(exe);
        for arg in it {
            cmd = cmd.arg(arg);
        }
        cmd
    }

    fn spawn(self, data:InputData) -> Child {
        eprintln!("RUN {}", &self.args.join(" "));
        let (sio, data) = data.get_stdio();
        let mut cmd = self.as_command();
        cmd.stdin(sio);
        let mut child = cmd.spawn().expect("Failed to spawn");
        if let Some(data) = data {
            // there is something to write
            let stdin = child.stdin.as_mut().expect("Failed to open stdin");
            stdin.write_all(data.as_bytes()).expect("Failed to write to stdin");
        }
        child
    }

    fn call(self, data:InputData) -> (std::process::ExitStatus, Child) {
        let mut child = self.spawn(data);
        let result = child.wait().expect("Failed waiting for child");
        (result, child)
    }

    fn check_call(self, data:InputData) -> Result<Child,AnalysisStatus> {
        let echo_on_err = self.stdout == StreamCapture::Capture;
        let (result, mut child) = self.call(data);
        if result.success() {
            Ok(child)
        } else {
            echo_child(&mut child, echo_on_err, false);
            Err(AnalysisStatus::from(result))
        }
    }

    fn check_call_and(self, data:InputData, halt_after_spawn:bool) -> Result<InputData, AnalysisStatus> {
        let child = self.check_call(data)?;
        if halt_after_spawn {
            Err(AnalysisStatus::Pass)
        } else {
            let inp = child.stdout.expect("Failed to obtain stdout");
            Ok(InputData::FromProc(inp))
        }
    }

}

struct Pipe {
    children: Vec<Vec<String>>,
    stdout: StreamCapture,
    stderr: StreamCapture,
}


#[derive(Debug)]
enum PipelineState {
    One(Exec),
    Many(Pipeline),
}

impl PipelineState {
    fn new(exec:Exec) -> Self {
        PipelineState::One(exec)
    }

    fn add(self, exec:Exec) -> PipelineState {
        match self {
            PipelineState::One(prev) => {
                PipelineState::Many(prev | exec)
            },
            PipelineState::Many(prev) => PipelineState::Many(prev | exec),
        }
    }

    fn stdout(self, stdout: Redirection) -> Self {
        match self {
            PipelineState::One(prev) => PipelineState::One(prev.stdout(stdout)),
            PipelineState::Many(prev) => PipelineState::Many(prev.stdout(stdout)),
        }
    }

    fn join(self) -> subprocess::Result<ExitStatus> {
        match self {
            PipelineState::One(prev) => prev.join(),
            PipelineState::Many(prev) => prev.join(),
        }
    }

    fn popen(self) -> subprocess::Result<Vec<Popen>> {
        match self {
            PipelineState::One(prev) => {
                let x = prev.popen()?;
                Ok(vec![x])
            },
            PipelineState::Many(prev) => prev.popen(),
        }
    }
    fn capture(self) -> subprocess::Result<subprocess::CaptureData> {
        match self {
            PipelineState::One(prev) => {
                prev.capture()
            },
            PipelineState::Many(prev) => {
                prev.capture()
            },
        }
    }
}

impl Pipe {
    fn add(mut self, args:Vec<String>) {
        self.children.push(args);
    }

    fn new(children:Vec<Vec<String>>, stdout:StreamCapture, stderr: StreamCapture) -> Self {
        Pipe {
            children: children,
            stdout: stdout,
            stderr: stderr,
        }
    }

    fn build(&self) -> Vec<Run> {
        let mut runs = Vec::new();
        let last_idx = self.children.len() - 1;
        for (idx, args) in self.children.iter().enumerate() {
            let stdout;
            let stderr;
            if idx == last_idx {
                stdout = self.stdout.clone();
                stderr = self.stderr.clone();
            } else {
                stdout = StreamCapture::Capture;
                stderr = StreamCapture::Capture;
            }
            runs.push(Run {
                args: args.clone(),
                stdout: stdout,
                stderr: stderr,
            });
        }
        runs
    }

    fn spawn(self, mut data:InputData) -> subprocess::Result<CaptureData> {
        let runs : Vec<Run> = self.build();
        let mut it = runs.iter();
        let exec : Exec = it.next().unwrap().as_exec();
//        let exec = data.update_exec(exec);
        let mut state = PipelineState::new(exec);
        for run in it {
            state = state.add(run.as_exec());
        }
        if let Some(r) = self.stdout.as_redirection() {
            state = state.stdout(r);
        }
        state.capture()
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

#[derive(Debug)]
struct Opts {
    solve_only: bool,
    expect_race: bool,
    input: Option<String>,
    analyze_only: Option<i32>,
    infer_only: Option<i32>,
    stage: Stage,
    infer_json: bool,
    block_dim: Vec<usize>,
    grid_dim: Vec<usize>,
    verbose: bool,
}

impl Opts {
    fn get_command(&self, stage:Stage, filename:Option<String>) -> Vec<String> {
        match stage {
            Stage::Parse => {
                let mut cmd = Vec::new();
                cmd.push("cu-to-json".to_string());
                if let Some(filename) = filename {
                    cmd.push(filename);
                } else {
                    cmd.push("/dev/stdin".to_string());
                }
                cmd
            },
            Stage::Infer => {
                let mut cmd = Vec::new();
                cmd.push("faial-infer".to_string());
                if let Some(lvl) = self.infer_only {
                    // Set the level of the analysis, example: -3
                    cmd.push("-X".to_string());
                    cmd.push(format!("{}", lvl));
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
                cmd
            },
            Stage::Analyze => {
                let mut cmd = vec!["faial-bin".to_string()];
                if let Some(lvl) = self.analyze_only {
                    // Set the level of the analysis, example: -3
                    cmd.push(format!("-{}", lvl));
                }
                let field = vec!["x", "y", "z"];
                for (idx, d) in self.grid_dim.iter().enumerate() {
                    cmd.push(format!("-DgridDim.{}={}", field.get(idx).unwrap(), d));
                }
                for (idx, d) in self.grid_dim.iter().enumerate() {
                    cmd.push(format!("-DblockDim.{}={}", field.get(idx).unwrap(), d));
                }
                if self.infer_json {
                    cmd.push("--json".to_string());
                }
                if let Some(filename) = filename {
                    cmd.push(filename);
                }
                cmd
            },
            Stage::Solve => {
                let mut args = vec!["z3".to_string()];
                if let Some(filename) = filename {
                    args.push("--".to_string());
                    args.push(filename);
                } else {
                    args.push("-in".to_string());
                }
                args
            },
        }
    }

    fn get_stages(&self) -> Vec<Stage> {
        let mut last = Stage::Solve;
        if self.infer_only.is_some() {
            last = Stage::Infer;
        } else if self.analyze_only.is_some() {
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

    fn get_pipe(&self) -> Vec<Vec<String>> {
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

impl Opts {
    fn parse_args() -> Opts {
        let matches = App::new("faial")
                .version("1.0")
                .arg(Arg::with_name("expect_race")
                    .long("expect-race")
                    .help("Sets exit status according to finding data-races.")
                    .conflicts_with("solve_only")
                    .conflicts_with("infer_only")
                    .conflicts_with("analyze_only")
                )
                .arg(Arg::with_name("analyze_only")
                    .long("analyze-only")
                    .short("A")
                    .takes_value(true)
                    .conflicts_with("solve_only")
                    .conflicts_with("infer_only")
                )
                .arg(Arg::with_name("solve_only")
                    .long("solve-only")
                    .short("S")
                    .help("Runs until it output of the SMT solver and exits.")
                    .conflicts_with("infer_only")
                    .conflicts_with("analyze_only")
                )
                .arg(Arg::with_name("infer_only")
                    .long("infer-only")
                    .short("I")
                    .takes_value(true)
                    .conflicts_with("solve_only")
                    .conflicts_with("analyze_only")
                )
                .arg(Arg::with_name("verbose")
                    .long("verbose")
                )
                .arg(Arg::with_name("grid_dim")
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
                    .long("block-dim")
                    .short("b")
                    .value_delimiter(",")
                    .multiple(true)
                    .takes_value(true)
                    .min_values(0)
                    .max_values(3)
                    .conflicts_with("infer_only")
                )
                .arg(Arg::with_name("input")
                    .takes_value(true)
                    .index(1)
                )
                .get_matches();
        let stage = Stage::Parse;
        let infer_json = match stage {
            Stage::Infer | Stage::Parse => true,
            _ => false,
        };
        Opts {
            expect_race: matches.is_present("expect_race"),
            solve_only: matches.is_present("solve_only"),
            input: matches.value_of("input").map(|x| x.to_string()),
            analyze_only: matches.value_of("analyze_only").map(|x| x.parse::<i32>().unwrap()),
            infer_only: matches.value_of("infer_only").map(|x| x.parse::<i32>().unwrap()),
            grid_dim: get_vec(&matches, "grid_dim").unwrap(),
            block_dim: get_vec(&matches, "block_dim").unwrap(),
            stage: stage,
            infer_json: infer_json,
            verbose: matches.is_present("verbose"),
        }
    }

    fn handle_data(self, data:CaptureData) {
        match self.last_stage() {
            Stage::Solve => {
                let buffer = data.stdout_str();
                if buffer.len() > 0 {
                    match buffer.parse::<DataRaceFreedom>() {
                        Ok(d) => {
                            render_drf(&d);
                            if self.expect_race ^ d.is_drf() {
                                return
                            }
                        },
                        Err(e) => eprintln!("Error parsing solver output: {}", e.to_string()),
                    }
                } else {
                    eprint!("{}", data.stderr_str());
                }
                std::process::exit(1);
            },
            _ => {
                print!("{}", data.stdout_str());
                eprint!("{}", data.stderr_str());
                if ! data.exit_status.success() {
                    std::process::exit(255);
                }
            }
        }
    }
}

fn main() {
    let opts = Opts::parse_args();
    let in_data = match &opts.input {
        Some(x) => InputData::FromFile(x.clone(), File::open(x).unwrap()),
        None => InputData::FromStdin,
    };
    let pipe = opts.get_pipe();
    let pipe_str = pipe.iter().map(|x| x.join(" ")).collect::<Vec<_>>().join(" | ");
    let pipe = Pipe::new(pipe, StreamCapture::Capture, StreamCapture::Capture);
    if opts.verbose {
        eprintln!("RUN {}", pipe_str);
    }
    match pipe.spawn(in_data) {
        Ok(data) => opts.handle_data(data),
        Err(e) => {
            eprintln!("Could exec: {}\nReason: {}", pipe_str, e);
            std::process::exit(255);
        },
    }
}

/*
fn main2() {
    let filename = "./file.smt2";
    let mut count = num_cpus::get();
    if count <= 0 {
        count = 1;
    }
    let mut idx = 0;
    let mut procs:Vec<thread::JoinHandle<_>> = Vec::with_capacity(count);
    for data in chunk_lines(get_proofs(filename), count) {
        idx += 1;
        procs.push(thread::spawn(move || {
            println!("{}", idx);
//            solve(data.as_str());
        }));
    }
    for p in procs {
        p.join().unwrap();
    }
}
*/

fn sexp_list(s:&Sexp) -> Option<Vec<Sexp>> {
    match s {
        Sexp::List(l) => Some(l.clone()),
        Sexp::Atom(_) => None,
    }
}

#[derive(Debug,PartialEq)]
struct Decl {
    name:String,
    value:Atom,
}

impl TryFrom<Sexp> for Decl {
    type Error = String;

    /*
        Parses:
            (define-fun $T1$mode () Int
                1)
        As:
            ("$T1$mode", Atom::I(1))

    */
    fn try_from(s:Sexp) -> Result<Self,Self::Error> {
        match s {
            Sexp::List(l) => match l.as_slice() {
                //  ["define-fun", "$T1$mode", "()", "Int", 1]
                | [_, Sexp::Atom(Atom::S(k)), _, _, Sexp::Atom(v)] =>
                    Ok(Decl{
                        name:k.clone(),
                        value:v.clone()
                    }),
                | _ => Err(format!("Error parsing variable declaration: expecting a list with 5 elements, bug got:  {:?}", l)),
            },
            Sexp::Atom(a) => Err(format!("Error parsing variable declaration: expecting a list, bug got an atom: {:?}", a)),
        }
    }

}


#[test]
fn test_parse_decl() {
    let l = sexp::parse(r#"(define-fun $T1$mode () Int 100)"#).unwrap();
    let d = Decl::try_from(l).unwrap();
    assert_eq!(d.name, "$T1$mode");
    assert_eq!(d.value, Atom::I(100));
}

/*
(model
  (define-fun $T1$mode () Int
    1)
  (define-fun $T1$idx$0 () Int
    0)
  (define-fun $T2$mode () Int
    1)
  (define-fun $T2$idx$0 () Int
    0)
  (define-fun SIZE () Int
    0)
*/

#[derive(Debug,PartialEq, Clone)]
struct Model {
    data: HashMap<String,Atom>,
}

impl Model {
    fn get(&self, name:&str) -> Option<&Atom> {
        self.data.get(name)
    }
}

impl TryFrom<Sexp> for Model {
    type Error = String;
    fn try_from(model:Sexp) -> Result<Self, Self::Error> {
        match model {
            Sexp::Atom(a) => Err(format!("Expecting list, got atom: {:?}", a)),
            Sexp::List(model) => {
                let mut result = HashMap::new();
                let mut it = model.iter();
                it.next().unwrap();
                for elem in it {
                    let d = Decl::try_from(elem.clone())?;
                    result.insert(d.name, d.value);
                }
                Ok(Model{data:result})
            },
        }
    }
}


#[test]
fn test_parse_model() {
    let l = sexp::parse(
        r#"(model
        (define-fun $T1$mode () Int
            1)
        (define-fun $T1$idx$0 () Int
            99)
        (define-fun $T2$mode () Int
            1)
        (define-fun $T2$idx$0 () Int
            0)
        (define-fun SIZE () Int
            0))"#
    ).unwrap();
    let m = Model::try_from(l).unwrap();
    assert_eq!(m.data.len(), 5);
    assert_eq!(m.data.get(&"$T1$idx$0".to_string()), Some(&Atom::I(99)));
    assert_eq!(m.data.get(&"SIZE".to_string()), Some(&Atom::I(0)));
}


#[derive(Debug,PartialEq)]
enum CommandResponse {
    Sat(),
    Unsat(),
    Unknown(),
    Model(Model),
    Error(String),
}

impl CommandResponse {
    fn parse_error(data:Sexp) -> Result<CommandResponse,String> {
        match data {
            Sexp::List(ref l) =>
                match l.as_slice() {
                    [Sexp::Atom(Atom::S(tag)), Sexp::Atom(Atom::S(msg))] => {
                        if tag == "error" {
                            Ok(CommandResponse::Error(String::from(msg)))
                        } else {
                            Err(format!("While parsing an error command: expected a tag 'error', but got: {}", tag))
                        }
                    },
                    _ => Err(format!("While parsing an error command: expecting a list of 2 elements, but got: {:?}", data))
                },
            Sexp::Atom(ref x) => Err(format!("While parsing an error command: expecting a list, but got atom: {:?}", data)),
        }
    }
}

impl TryFrom<Sexp> for CommandResponse {
    type Error = String;
    fn try_from(row:Sexp) -> Result<Self,String> {
        match row {
            Sexp::Atom(Atom::S(x)) => {
                match x.as_str() {
                    "sat" => Ok(CommandResponse::Sat()),
                    "unsat" => Ok(CommandResponse::Unsat()),
                    "unknown" => Ok(CommandResponse::Unknown()),
                    _ => Err(format!("Unexpected tag: {}", x)),
                }
            },
            Sexp::List(ref l)  => {
                match l.get(0) {
                    Some(Sexp::Atom(Atom::S(tag))) => {
                        match tag.as_str() {
                            "model" => Ok(CommandResponse::Model(Model::try_from(row)?)),
                            "error" => CommandResponse::parse_error(row),
                            _ => Err(format!("Unexpected tag: {}", tag)),
                        }
                    },
                    _ => Err(format!("Unexpected tag: {:?}", row)),
                }
            },
            _ => Err(format!("Unexpected sexp: {:?}", row)),
        }
    }
}


#[test]
fn test_decision_parse_one() {
    assert_eq!(CommandResponse::try_from(sexp::parse("sat").unwrap()), Ok(CommandResponse::Sat()));
    assert_eq!(CommandResponse::try_from(sexp::parse("unsat").unwrap()), Ok(CommandResponse::Unsat()));
    let l = r#"
(model
  (define-fun $T1$mode () Int
    1)
  (define-fun $T1$idx$0 () Int
    0)
  (define-fun $T2$mode () Int
    1)
  (define-fun $T2$idx$0 () Int
    0)
  (define-fun SIZE () Int
    0)
    )
"#;
    match CommandResponse::try_from(sexp::parse(l).unwrap()) {
        Ok(CommandResponse::Model(_)) => {},
        _ => assert!(false),
    }
}

// ====================================================

#[derive(Debug)]
enum SexpParseError {
    SyntaxError(sexp::Error),
    FormatError(String),
}

impl SexpParseError {
    fn to_string(self) -> String {
        match self {
            SexpParseError::SyntaxError(e) => format!("Line {} Col {} Index {}: {}", e.line, e.column, e.index, e.message),
            SexpParseError::FormatError(e) => e,
        }
    }
}

#[derive(Debug,PartialEq)]
struct Smtlib2Response(Vec<CommandResponse>);

impl TryFrom<Sexp> for Smtlib2Response {
    type Error = String;
    fn try_from(l:sexp::Sexp) -> Result<Self,Self::Error> {
        match l {
            Sexp::List(l) => {
                let l: Result<Vec<_>,_> = l
                    .into_iter()
                    .map(|x| CommandResponse::try_from(x))
                    .collect();
                Ok(Smtlib2Response(l?))
            },
            Sexp::Atom(x) => Err(format!("Expecting a list, but got: {:?}", x)),
        }
    }
}

impl From<Box<sexp::Error>> for SexpParseError {
    fn from(src:Box<sexp::Error>) -> Self {
        SexpParseError::SyntaxError(*src)
    }
}

impl From<String> for SexpParseError {
    fn from(msg:String) -> Self {
        SexpParseError::FormatError(msg)
    }
}

impl FromStr for Smtlib2Response {
    type Err = SexpParseError;
    fn from_str(data: &str) -> Result<Self, SexpParseError> {
        let mut buff = String::from("(");
        buff.push_str(data);
        buff.push(')');
        let s = sexp::parse(buff.as_str())?;
        let r = Smtlib2Response::try_from(s)?;
        Ok(r)
    }
}

#[test]
fn decision_parse_many() {
    let l = r#"
unsat
(error "line 192692 column 10: model is not available")
"#;
    let l = l.parse::<Smtlib2Response>().unwrap();
    let l = l.0.as_slice();
    match l {
        [CommandResponse::Unsat(), CommandResponse::Error(e)] => {
            assert_eq!(e.as_str(), "line 192692 column 10: model is not available");
        },
        _ => assert!(false),
    }
}

#[test]
fn sexp() {
    let example = r#"sat
    unknown
"#;
    let l = example.parse::<Smtlib2Response>().unwrap();
    let l = l.0.as_slice();
    assert_eq!(l, &[CommandResponse::Sat(), CommandResponse::Unknown()]);
}


#[derive(Debug,PartialEq)]
enum Tid {
    T1,
    T2,
}

impl Tid {
    fn is_task(name: &str) -> bool {
        return name == "T1" || name == "T2";
    }

    fn get(&self) -> i32 {
        match self {
            &Tid::T1 => 1,
            &Tid::T2 => 2,
        }
    }

    fn as_str(&self) -> &str {
        match self {
            &Tid::T1 => "T1",
            &Tid::T2 => "T2",
        }
    }
}

impl FromStr for Tid {
    type Err = String;
    fn from_str(name: &str) -> Result<Tid, String> {
        match name {
            "T1" => Ok(Tid::T1),
            "T2" => Ok(Tid::T2),
            x => Err(format!("Expecting 'T1' or 'T2', got: {}", x)),
        }
    }
}

#[derive(Debug,PartialEq)]
enum AccessMode {
    Read,
    Write,
}

impl AccessMode {
    fn to_string(&self) -> String {
        match self {
            AccessMode::Read => "R".to_string(),
            AccessMode::Write => "W".to_string(),
        }
    }
}

impl TryFrom<Atom> for AccessMode {
    type Error = String;
    fn try_from(a:Atom) -> Result<Self,Self::Error> {
        match a {
            Atom::I(0) => return Ok(AccessMode::Read),
            Atom::I(1) => return Ok(AccessMode::Write),
            a => Err(format!("Unexpected atom: {}", a))
        }
    }
}

#[derive(Debug,PartialEq)]
struct TaskBuilder {
    tid:Tid,
    prefix:String,
    indices:HashMap<i32, i32>,
    variables:HashMap<String,Atom>,
    mode: Option<AccessMode>,
}

impl TaskBuilder {
    fn new(t:Tid) -> TaskBuilder {
        let mut prefix = String::from("$T");
        prefix.push_str(t.get().to_string().as_str());
        prefix.push_str("$");
        TaskBuilder {
            tid:t,
            prefix: prefix,
            variables: HashMap::new(),
            indices: HashMap::new(),
            mode: None,
        }
    }

    fn is_match(&self, s:&str) -> bool {
        self.tid.as_str() == s
    }

    fn add_variable(&mut self, key:String, value:Atom) {
        self.variables.insert(key, value);
    }

    fn set_mode(&mut self, m:AccessMode) {
        self.mode = Some(m)
    }

    fn add_index(&mut self, index:i32, value:i32) {
        self.indices.insert(index, value);
    }

    fn get_indices(&self) -> Result<Vec<i32>,String> {
        let mut indices : Vec<i32> = Vec::new();
        for i in 0..self.indices.len() {
            match &self.indices.get(&(i as i32)) {
                Some(n) => indices.push(*n.clone()),
                None => return Err(format!("When building task {:?} missing index {}", self.tid, i)),
            }
        }
        Ok(indices)
    }
}

#[derive(Debug,PartialEq)]
struct Task {
    mode: AccessMode,
    variables: HashMap<String, Atom>
}

impl TaskBuilder {
    fn build(self) -> Result<Task,String> {
        let mut indices : Vec<i32> = Vec::new();
        for i in 0..self.indices.len() {
            match &self.indices.get(&(i as i32)) {
                Some(n) => indices.push(*n.clone()),
                None => return Err(format!("When building task {:?} missing index {}", self.tid, i)),
            }
        }
        match self.mode {
            Some(m) => Ok (Task{
                mode: m,
                variables: self.variables,
            }),
            None => Err(format!("When building task {:?} mode unset", self.tid))
        }
    }
}

#[derive(Debug,PartialEq)]
struct DataRace {
    t1: Task,
    t2: Task,
    indices: Vec<i32>,
    globals: HashMap<String, Atom>,
}

struct DataRaceBuilder {
    t1: TaskBuilder,
    t2: TaskBuilder,
    globals: HashMap<String, Atom>,
}

impl DataRaceBuilder {
    fn new() -> DataRaceBuilder {
        DataRaceBuilder {
            t1:TaskBuilder::new(Tid::T1),
            t2:TaskBuilder::new(Tid::T2),
            globals: HashMap::new(),
        }
    }

    fn get(&mut self, t:Tid) -> &mut TaskBuilder {
        match t {
            Tid::T1 => &mut self.t1,
            Tid::T2 => &mut self.t2,
        }
    }

    fn add_local(&mut self, t:Tid, name:String, value:Atom) {
        self.get(t).add_variable(name, value);
    }

    fn add_global(&mut self, name:String, value:Atom) {
        self.globals.insert(name, value);
    }

    fn set_mode(&mut self, t:Tid, m:AccessMode) {
        self.get(t).set_mode(m);
    }

    fn add_index(&mut self, t:Tid, index:i32, value:i32) {
        self.get(t).add_index(index, value);
    }

    fn build(self) -> Result<DataRace,String> {
        let idx1 = self.t1.get_indices()?;
        Ok(DataRace {
            t1: self.t1.build()?,
            t2: self.t2.build()?,
            indices: idx1,
            globals: self.globals,
        })
    }
}

impl TryFrom<Model> for DataRace {
    type Error = String;
    fn try_from(m:Model) -> Result<Self,Self::Error> {
        let mut b = DataRaceBuilder::new();
        for (name, val) in &m.data {
            let name = name.clone();
            let name = name.split('$').collect::<Vec<_>>();
            match name.as_slice() {
                // (define-fun count () Int 3)
                &[name] => {
                    b.add_global(name.to_string(), val.clone());
                },
                // (define-fun i$T2 () Int 1)
                &[name, tid] => {
                    let t:Tid = tid.parse()?;
                    b.add_local(t, name.to_string(), val.clone());
                },
                // (define-fun $T1$mode () Int 1)
                &[_, tid, _] => {
                    let m = AccessMode::try_from(val.clone())?;
                    let t:Tid = tid.parse()?;
                    b.set_mode(t, m);
                },
                // (define-fun $T2$idx$0 () Int 1)
                &[_, tid, _, num] => {
                    let t:Tid = tid.parse()?;
                    match (num.parse::<i32>(), val) {
                        (Ok(idx), Atom::I(v))  => b.add_index(t, idx, *v as i32),
                        (x,y) => return Err(format!("Error parsing index task={:?} index={:?} value={}", t, x, y)),
                    }
                },
                // Otherwise
                x => return Err(format!("Error parsing index={:?}", x)),
            }
        }
        b.build()
    }
}


#[derive(Debug,PartialEq)]
enum AnalysisError {
    Race(DataRace),
    Unknown,
}

#[derive(Debug,PartialEq)]
struct DataRaceFreedom(Vec<AnalysisError>);

impl DataRaceFreedom {
    fn is_drf(&self) -> bool {
        self.0.len() == 0
    }
}

impl TryFrom<Smtlib2Response> for DataRaceFreedom {
    type Error = String;
    fn try_from(elems:Smtlib2Response) -> Result<DataRaceFreedom, String> {
        let mut it = elems.0.iter();
        let mut result = Vec::new();
        let mut idx = 1;
        while let Some(v1) = it.next() {
            if let Some(v2) = it.next() {
                match (v1,v2) {
                    (CommandResponse::Unsat(), CommandResponse::Error(_)) => {
                        // OK
                    },
                    (CommandResponse::Sat(), CommandResponse::Model(ref m)) => {
                        let dr = DataRace::try_from(m.clone())?;
                        result.push(AnalysisError::Race(dr));
                    },
                    (CommandResponse::Unknown(), CommandResponse::Error(_)) => {
                        result.push(AnalysisError::Unknown);
                    },
                    (v1, v2) => {return Err(format!("Error handling index {}: {:?} {:?}", idx, v1, v2))},
                }
            } else {
                return Err("Expecting pairs".to_string());
            }
            idx += 2;
        }
        Ok(DataRaceFreedom(result))
    }

}

impl FromStr for DataRaceFreedom {
    type Err = SexpParseError;
    fn from_str(data: &str) -> Result<DataRaceFreedom, SexpParseError> {
        let d : Smtlib2Response = data.parse()?;
        let d = DataRaceFreedom::try_from(d)?;
        Ok(d)
    }
}

fn render_drf(drf:&DataRaceFreedom) {
    for x in &drf.0 {
        match x {
            AnalysisError::Race(m) => {
                println!("*** DATA RACE ERROR ***");
                println!("");
                render_data_race(m)
            },
            AnalysisError::Unknown => println!("I DONT'T KNOW!"),
        }
    }
    if drf.is_drf() {
        println!("Program is data-race free!");
    }
}

fn render_data_race(dr:&DataRace) {
    let mut table = Table::new();
    /*
    t1: Task,
    t2: Task,
    indices: Vec<i32>,
    globals: HashMap<String, Atom>,
    */
    table.add_row(row![b->"Globals", b->"Value"]);
    table.add_row(
        Row::new(vec![
            Cell::new("(index)"),
            Cell::new(format!("{:?}", dr.indices).as_str()),
        ])
    );
    for (k, v) in &dr.globals {
        table.add_row(
            Row::new(vec![
                Cell::new(k.as_str()),
                Cell::new(v.to_string().as_str()),
            ])
        );
    }
    table.printstd();

    let mut locals = HashMap::new();
    for (k, v1) in &dr.t1.variables {
        if let Some(v2) = dr.t2.variables.get(k.as_str()) {
            locals.insert(k, (v1, v2));
        }
    }

    let mut table = Table::new();
    table.add_row(row![b->"Locals", b->"T1", b->"T2"]);
    table.add_row(
        Row::new(vec![
            Cell::new("(mode)"),
            Cell::new(dr.t1.mode.to_string().as_str()),
            Cell::new(dr.t2.mode.to_string().as_str()),
        ])
    );
    for (k, (v1,v2)) in locals {
        table.add_row(
            Row::new(vec![
                Cell::new(k.as_str()),
                Cell::new(v1.to_string().as_str()),
                Cell::new(v2.to_string().as_str()),
            ])
        );
    }
    table.printstd();
}

#[test]
fn proofs() {
    let buf = io::Cursor::new(&b"hello\n(pop 1)\nworld"[..]);
    let mut s = Proofs::new(buf);
    assert_eq!(s.next().unwrap().unwrap(), "hello\n(pop 1)\n".to_string());
    assert_eq!(s.next().unwrap().unwrap(), "world".to_string());
    assert!(s.next().is_none());
}
