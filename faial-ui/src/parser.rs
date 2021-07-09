use std::collections::HashMap;
use sexp::{Sexp,Atom};
use std::str::FromStr;
use std::convert::TryFrom;
use ansi_term::Colour::Red;
use std::collections::BTreeMap;

use prettytable::format::{TableFormat};
use prettytable::{Table, Row, Cell, Attr, format};

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
    ty: String,
    value:Atom,
}

impl TryFrom<Sexp> for Decl {
    type Error = Sexp;

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
                | [_, Sexp::Atom(Atom::S(k)), _, Sexp::Atom(Atom::S(ty)), Sexp::Atom(v)] =>
                    Ok(Decl{
                        name:k.clone(),
                        value:v.clone(),
                        ty:ty.clone(),
                    }),
                | s => Err(Sexp::List(Vec::from(s))),
            },
            s => Err(s),
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
                if let Some(Sexp::Atom(Atom::S(tag))) = model.get(0) {
                    if tag == "model" {
                        it.next().unwrap();
                    }
                }
                for elem in it {
                    match Decl::try_from(elem.clone()) {
                        Ok(d) => { result.insert(d.name, d.value); ()},
                        // If we can't parse it
                        Err(_) => (),
                    }
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
    Str(String),
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
            Sexp::Atom(Atom::S(s)) => Ok(CommandResponse::Str(s)),
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
                    x => Ok(CommandResponse::Str(x.to_string())),
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
                    _ => Ok(CommandResponse::Model(Model::try_from(row)?)),
//                    _ => Err(format!("Unexpected tag: {:?}", row)),
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
    let l = r#"
(
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
pub enum SexpParseError {
    SyntaxError(sexp::Error),
    FormatError(String),
}

impl SexpParseError {
    pub fn to_string(self) -> String {
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
    location_id: Option<usize>,
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
            location_id: None,
        }
    }

    fn add_variable(&mut self, key:String, value:Atom) {
        self.variables.insert(key, value);
    }

    fn set_location_id(&mut self, lid:usize) {
        self.location_id = Some(lid)
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
    location_id: Option<usize>,
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
                location_id: self.location_id,
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
    array: String,
    indices: Vec<i32>,
    globals: HashMap<String, Atom>,
}

struct DataRaceBuilder {
    t1: TaskBuilder,
    t2: TaskBuilder,
    array: Option<String>,
    globals: HashMap<String, Atom>,
}

impl DataRaceBuilder {
    fn new() -> DataRaceBuilder {
        DataRaceBuilder {
            t1:TaskBuilder::new(Tid::T1),
            t2:TaskBuilder::new(Tid::T2),
            array: None,
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

    fn set_location_id(&mut self, t:Tid, lid:usize) {
        self.get(t).set_location_id(lid);
    }

    fn set_array(&mut self, array:String) {
        self.array = Some(array);
    }

    fn build(self) -> Result<DataRace,String> {
        let idx1 = self.t1.get_indices()?;
        let array = match self.array {
            Some(x) => x,
            None => return Err("No array set".to_string()),
        };
        Ok(DataRace {
            t1: self.t1.build()?,
            t2: self.t2.build()?,
            array: array,
            indices: idx1,
            globals: self.globals,
        })
    }
}

impl TryFrom<Model> for DataRace {
    type Error = String;
    fn try_from(m:Model) -> Result<Self,String> {
        let mut b = DataRaceBuilder::new();
        for (name, val) in &m.data {
            let name = name.clone();
            let name = name.split('$').collect::<Vec<_>>();
            match name.as_slice() {
                // (define-fun count () Int 3)
                &[name] => {
                    b.add_global(name.to_string(), val.clone());
                },
                // (define-fun $name () String "foo")
                // (define-fun i$T2 () Int 1)
                &[name, tid] => {
                    if name == "" && tid == "array" {
                        match val {
                            Atom::S(x) => {
                                b.set_array(x.to_string());
                            },
                            val => return Err(format!("Expecting a string, but got: {}", val))
                        }
                    } else {
                        let t:Tid = tid.parse()?;
                        b.add_local(t, name.to_string(), val.clone());
                    }
                },
                // (define-fun $T1$mode () Int 1)
                &[_, tid, tag] => {
                    let t:Tid = tid.parse()?;
                    if tag == "mode" {
                        let m = AccessMode::try_from(val.clone())?;
                        b.set_mode(t, m);
                    } else if tag == "loc" {
                        let loc_idx:usize = match val.clone() {
                            Atom::I(idx) => idx as usize,
                            val => return Err(format!("Expected an integer, got: {:?}", val)),
                        };
                        b.set_location_id(t, loc_idx);
                    } else {
                        return Err(format!("Unknown tag: {}", tag))
                    }
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
pub struct DataRaceFreedom {
    errors: Vec<AnalysisError>,
    locations: Vec<String>,
}

impl DataRaceFreedom {
    pub fn is_drf(&self) -> bool {
        self.errors.len() == 0
    }
}

impl TryFrom<Smtlib2Response> for DataRaceFreedom {
    type Error = String;
    fn try_from(elems:Smtlib2Response) -> Result<DataRaceFreedom, String> {
        let mut it = elems.0.iter();
        let mut result = Vec::new();
        let mut idx = 1;
        let mut locations:Vec<String> = Vec::new();
        while let Some(v1) = it.next() {
            match v1 {
                CommandResponse::Str(v1) => {
                    locations.push(v1.clone());
                    break;
                },
                v1 => {
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
                },
            }
        }
        while let Some(v1) = it.next() {
            match v1 {
                CommandResponse::Str(v1) => {
                    locations.push(v1.clone());
                },
                v1 => return Err(format!("Expecting a string, got: {:?}", v1)),
            }
        }
        Ok(DataRaceFreedom{errors:result, locations:locations})
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

impl DataRaceFreedom {
    pub fn print(self:&DataRaceFreedom) {
        for x in &self.errors {
            match x {
                AnalysisError::Race(m) => {
                    println!("{}", Red.bold().paint("*** DATA RACE ERROR ***"));
                    println!("");
                    render_data_race(m, &self.locations)
                },
                AnalysisError::Unknown => println!("I DONT'T KNOW!"),
            }
        }
        if self.is_drf() {
            println!("Program is data-race free!");
        }
    }
}

fn render_data_race(dr:&DataRace, locs:&Vec<String>) {
    let mut f = TableFormat::new();
    f.column_separator(' ');
    let mut table = Table::new();
    table.set_format(f);
    table.add_row(
        Row::new(vec![
            Cell::new("Array:").with_style(Attr::Bold),
            Cell::new(
                format!("{}{:?}",
                    dr.array,
                    dr.indices
                ).as_str()
            ),
        ])
    );
    if let  (Some(lid1), Some(lid2)) = (dr.t1.location_id, dr.t2.location_id) {
        if let (Some(lid1), Some(lid2)) = (locs.get(lid1), locs.get(lid2)) {
            table.add_row(
                Row::new(vec![
                    Cell::new("T1 location:").with_style(Attr::Bold),
                    Cell::new(format!("{}", lid1.as_str()).as_str()),
                ])
            );
            table.add_row(
                Row::new(vec![
                    Cell::new("T2 location:").with_style(Attr::Bold),
                    Cell::new(format!("{}", lid2.as_str()).as_str()),
                ])
            );
        }
    }
    table.add_row(
        Row::new(vec![
            Cell::new("T1 mode:").with_style(Attr::Bold),
            Cell::new(dr.t1.mode.to_string().as_str()),
        ])
    );
    table.add_row(
        Row::new(vec![
            Cell::new("T2 mode:").with_style(Attr::Bold),
            Cell::new(dr.t2.mode.to_string().as_str()),
        ])
    );
    table.printstd();
    println!("");

    if dr.globals.len() > 0 {
        let mut table = Table::new();
        table.set_format(*format::consts::FORMAT_NO_COLSEP);
        table.add_row(row![b->"Globals", b->"Value"]);
        let globals:BTreeMap<_,_> = dr.globals.iter().collect();
        for (k, v) in &globals {
            table.add_row(
                Row::new(vec![
                    Cell::new(k.as_str()),
                    Cell::new(v.to_string().as_str()),
                ])
            );
        }
        table.printstd();
        println!("");
    }

    let mut locals = BTreeMap::new();
    for (k, v1) in &dr.t1.variables {
        if let Some(v2) = dr.t2.variables.get(k.as_str()) {
            locals.insert(k, (v1, v2));
        }
    }

    if locals.len() > 0 {
        let mut table = Table::new();
        table.set_format(*format::consts::FORMAT_NO_COLSEP);
        table.add_row(row![b->"Locals", b->"T1", b->"T2"]);
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
        println!("");
    }
}
