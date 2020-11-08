use std::fs::File;
use std::io::{self, BufRead};
use std::collections::HashMap;
use sexp::{Sexp,Atom};
use std::str::FromStr;
use clap::{App,Arg,ArgMatches};
use std::convert::TryFrom;
use subprocess::{Exec, Pipeline, CaptureData};

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

#[derive(Debug)]
struct Run {
    args: Vec<String>,
}

impl Run {
    fn new(args:Vec<String>) -> Self {
        Run {
            args: args,
        }
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
}

struct Pipe {
    children: Vec<Vec<String>>,
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

    fn new(children:Vec<Vec<String>>) -> Self {
        Pipe {
            children: children,
        }
    }
    fn as_str(&self) -> String {
        self.children.iter().map(|x| x.join(" ")).collect::<Vec<_>>().join(" | ")
    }

    fn build(&self) -> Vec<Run> {
        let mut runs = Vec::new();
        for args in self.children.iter() {
            runs.push(Run::new(args.clone()));
        }
        runs
    }

    fn spawn(self) -> subprocess::Result<CaptureData> {
        let runs : Vec<Run> = self.build();
        let mut it = runs.iter();
        let exec : Exec = it.next().unwrap().as_exec();
        let mut state = PipelineState::new(exec);
        for run in it {
            state = state.add(run.as_exec());
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
            InputType::CUDA=>"cuda",
            InputType::PROTO=>"proto",
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
    input: Option<String>,
    analyze_only: Option<i32>,
    infer_only: Option<i32>,
    stage: Stage,
    infer_json: bool,
    block_dim: Vec<usize>,
    grid_dim: Vec<usize>,
    verbose: bool,
    defines: HashMap<String,u32>,
    input_type: InputType,
    dry_run: bool,
}

impl Faial {
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
                for (k,v) in &self.defines {
                    cmd.push(format!("-D{}={}", k, v));
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

    fn get_pipe(&self) -> Pipe {
        let mut pipe = Vec::new();
        for stage in self.get_stages() {
            let filename = if stage == self.stage {
                self.input.clone()
            } else {
                None
            };
            pipe.push(self.get_command(stage, filename));
        }
        Pipe::new(pipe)
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
                "Expecting exaclty one equals sign, example: 'key=13'."
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
        let app = App::new("faial")
                .version("1.0")
                .about("Checks if a GPU code is data-race free")
                .author("UMB-SVL research group: https://umb-svl.gitlab.io/")
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
                    .help("Halts after analysis")
                    .validator(can_parse::<i32>)
                    .takes_value(true)
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
                    .takes_value(true)
                    .validator(can_parse::<i32>)
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
        let infer_json = match stage {
            Stage::Infer | Stage::Parse => true,
            _ => input_type == InputType::CJSON,
        };
        Faial {
            expect_race: matches.is_present("expect_race"),
            solve_only: matches.is_present("solve_only"),
            input: input,
            analyze_only: parse_opt::<i32>(&matches, "analyze_only"),
            infer_only: parse_opt::<i32>(&matches, "infer_only"),
            grid_dim: get_vec(&matches, "grid_dim").unwrap(),
            block_dim: get_vec(&matches, "block_dim").unwrap(),
            stage: stage,
            infer_json: infer_json,
            verbose: matches.is_present("verbose"),
            defines: parse_key_val(&matches, "defines"),
            input_type: input_type,
            dry_run: matches.is_present("dry_run"),
        }
    }

    fn handle_data(self, data:CaptureData) {
        match self.last_stage() {
            Stage::Solve => {
                let buffer = data.stdout_str();
                if self.solve_only {
                    print!("{}", data.stdout_str());
                    eprint!("{}", data.stderr_str());
                } else {
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
    let faial = Faial::new();
    let pipe = faial.get_pipe();
    let pipe_str = pipe.as_str();
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

#[allow(dead_code)]
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

#[allow(dead_code)]
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
            data => Err(format!("While parsing an error command: expecting a list, but got atom: {:?}", data)),
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

    fn get(&self) -> i32 {
        match self {
            &Tid::T1 => 1,
            &Tid::T2 => 2,
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
