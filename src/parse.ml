open Proto
open Sexplib

exception ParseError of (string list)

let parse_error (cause:string list) msg data =
  raise (ParseError (( "Error parsing '" ^ msg ^"': " ^ Sexp.to_string_hum data)::cause))

let call msg f data =
  let o = (try f data with ParseError l -> parse_error l msg data) in
  match o with
  | Some m -> m
  | None ->  parse_error [] msg data

let is_some o =
  match o with
  | Some _ -> true
  | None -> false

type 'a parser = {is_valid: Sexp.t -> bool; run: Sexp.t -> 'a}

let make name f = {
  is_valid = (fun x -> is_some (f x));
  run = call name f;
}

let parse_nbin = make "nbin" (fun m ->
  match m with
  | Sexp.Atom "+" -> Some Plus
  | Sexp.Atom "-" -> Some Minus
  | Sexp.Atom "*"  -> Some Mult
  | Sexp.Atom "div" -> Some Div
  | Sexp.Atom "mod" -> Some Mod
  | _ -> None
)

let bind o1 o2 =
  match o1 with
  | Some x -> o2 x
  | None -> None

let rec parse_nexp (a:Sexp.t) : nexp option =
  match a with
  | Sexp.Atom x -> Some (begin
      try Num (int_of_string x)
      with Failure _ -> Var x
    end)
  | Sexp.List [m; o1; o2] when (parse_nbin.is_valid m) ->
    bind (parse_nexp o1) (fun n1 ->
      bind (parse_nexp o2) (fun n2 ->
        Some (Bin (parse_nbin.run m, n1, n2))))
  | _ -> None

let parse_nexp = make "nexp" parse_nexp

let parse_nrel = make "nrel" (fun m ->
  match m with
  | Sexp.Atom "=" -> Some NEq
  | Sexp.Atom "<=" -> Some NLe
  | Sexp.Atom "<"  -> Some NLt
  | _ -> None
)

let parse_brel = make "brel" (fun m ->
  match m with
  | Sexp.Atom "or" -> Some BOr
  | Sexp.Atom "and" -> Some BAnd
  | _ -> None
)

let rec parse_bexp (s:Sexp.t) : bexp option =
  match s with
  | Sexp.Atom "true" -> Some (Bool true)
  | Sexp.Atom "false" -> Some (Bool false)
  | Sexp.List [o; n1; n2] when parse_nrel.is_valid o ->
    Some (NRel (parse_nrel.run o, parse_nexp.run n1, parse_nexp.run n2))
  | Sexp.List [o; b1; b2] ->
    bind (parse_bexp b1) (fun b1 ->
      bind (parse_bexp b2) (fun b2 ->
        Some (BRel (parse_brel.run o, b1, b2))
      )
    )
  | Sexp.List [Sexp.Atom "not"; b] ->
    bind (parse_bexp b) (fun b ->
      Some (BNot b)
    )
  | _ -> None

let parse_bexp = make "bexp" parse_bexp

let parse_range = make "range" (fun s ->
  match s with
  | Sexp.List [Sexp.Atom "range"; Sexp.Atom x; n] ->
    Some {
      range_var = x;
      range_upper_bound = parse_nexp.run n;
    }
  | _ -> None
)

let parse_access = make "access" (fun s ->
  let mk_acc m s =
    match s with
    | [n1; b] ->
      Some {
        access_index=parse_nexp.run n1;
        access_cond=parse_bexp.run b;
        access_mode = m;
      }
    | _ -> None
  in
  match s with
  | Sexp.List ((Sexp.Atom "ro")::s) -> mk_acc R s
  | Sexp.List ((Sexp.Atom "rw")::s) -> mk_acc W s
  | _ -> None
)

let rec parse_proto s =
  match s with
  | Sexp.Atom "skip" -> Some Skip
  | Sexp.Atom "sync" -> Some Sync
  | Sexp.List [Sexp.Atom "begin"] -> Some Skip
  | Sexp.List (Sexp.Atom "begin" :: p :: l) ->
    bind (parse_proto p) (fun p1 ->
      bind (parse_proto (Sexp.List (Sexp.Atom "begin" :: l))) (fun p2 ->
        Some (Seq (p1, p2))
      )
    )
  | Sexp.List [Sexp.Atom "loop"; r; p] ->
    bind (parse_proto p) (fun p ->
      Some (Loop (parse_range.run r, p))
    )
  | Sexp.List [Sexp.Atom "loc"; Sexp.Atom x; a] ->
    Some (Acc (x, parse_access.run a))
  | _ -> None

let parse_proto = make "proto" parse_proto

let parse_timed = make "timed" (fun s ->
  match s with
  | Sexp.List [Sexp.Atom "timed"; n; a] ->
    Some {
      timed_phase = parse_nexp.run n;
      timed_data = parse_access.run a;
    }
  | _ -> None
)

let parse_string_list l =
  List.mapi (fun idx elem ->
    match elem with
    | Sexp.Atom s -> s
    | _ ->
      let msg = ("string list (index=" ^ string_of_int idx ^")") in
      parse_error [] msg elem
  ) l

let parse_kernel = make "kernel" (fun s->
  match s with
  | Sexp.List [Sexp.Atom "kernel";
      Sexp.List (Sexp.Atom "locations"::locs);
      Sexp.List (Sexp.Atom "local"::ls);
      Sexp.List (Sexp.Atom "global"::gs);
      p
    ] ->
    Some {
      kernel_locations = parse_string_list locs;
      kernel_local_variables = parse_string_list ls;
      kernel_global_variables = parse_string_list gs;
      kernel_code = parse_proto.run p;
    }
  | _ -> None
)

let parse_step = make "step" (fun s ->
  match s with
  | Sexp.List [Sexp.Atom x; o] ->
    Some (x, parse_timed.run o)
  | _ -> None
)

let parse_stream = make "stream" (fun s ->
  match s with
  | Sexp.List l -> Some (List.map parse_step.run l)
  | _ -> None
)
