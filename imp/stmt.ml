open Stage0
open Protocols

module StringMap = Common.StringMap
module StringMapUtil = Common.StringMapUtil
module StringSet = Common.StringSet

type t =
  | Sync of Location.t option
  | Assert of Assert.t
  | Read of Read.t
  | Atomic of Atomic_write.t
  | Write of Write.t
  | Block of (t list)
  | LocationAlias of Alias.t
  | Decl of Decl.t list
  | Assign of {var: Variable.t; data: Exp.nexp; ty: C_type.t}
  | If of (Exp.bexp * t * t)
  | For of (Range.t * t)
  | Star of t
  | Call of Call.t

let filter (f:t -> bool) : t -> t =
  let rec filter (s:t) : t =
    if not (f s) then Block [] else
    match s with
    | Block l -> Block (List.map filter l)
    | If (b, p, q) -> If (b, filter p, filter q)
    | For (r, p) -> For (r, filter p)
    | Star p -> Star (filter p)
    | Sync _ | Read _ | Atomic _ | Write _  | LocationAlias _
    | Decl _ | Assign _ | Call _ | Assert _ -> s
  in
  filter

let filter_asserts (f:Assert.t -> bool) : t -> t =
  filter (
    function
    | Assert a -> f a
    | _ -> true
  )

let is_for : t -> bool =
  function
  | For _ -> true
  | _ -> false

let is_if : t -> bool =
  function
  | If _ -> true
  | _ -> false

type prog = t list

let rec has_sync : t -> bool =
  function
  | Sync _ -> true
  | If (_, p, q) -> has_sync p || has_sync q
  | Atomic _ | Read _ | Write _ | Assert _ | LocationAlias _
  | Decl _ | Call _ | Assign _ -> false
  | Block l -> List.exists has_sync l
  | For (_, p) | Star p -> has_sync p

let calls : t -> StringSet.t =
  let rec calls (cs:StringSet.t) : t -> StringSet.t =
    function
    | Decl _ | LocationAlias _ | Sync _ | Assert _
    | Read _ | Write _ | Atomic _ | Assign _ ->
      cs
    | Block l -> List.fold_left calls cs l
    | If (_, s1, s2) -> calls (calls cs s1) s2
    | For (_, s) | Star s -> calls cs s
    | Call c -> StringSet.add (Call.unique_id c) cs
  in
  calls StringSet.empty


let fold : 'a. (t -> 'a -> 'a) -> t -> 'a -> 'a =
  fun (f: t -> 'a -> 'a) (p:t) (init:'a) ->
    let rec fold_i (s:t) (init:'a) : 'a =
      let init : 'a = f s init in
      match s with
      | Sync _
      | Assert _
      | Read _
      | Atomic _
      | Write _
      | Decl _
      | Assign _
      | LocationAlias _
      | Call _ ->
        init
      | Block l ->
        fold_p l init
      | If (_, s1, s2) ->
        let init = fold_i s1 init in
        fold_i s2 init
      | For (_, s)
      | Star s ->
        fold_i s init

    and fold_p (l:prog) (init:'a) : 'a =
      List.fold_right fold_i l init
    in
    fold_i p init

let find_all_map (f: t -> 'a option) (s: t) : 'a Seq.t =
  let g (e:t) (r:'a Seq.t) : 'a Seq.t =
    match f e with
    | Some x -> Seq.cons x r
    | None -> r
  in
  fold g s Seq.empty

let find_all (f: t -> bool) : t -> t Seq.t =
  find_all_map (fun x -> if f x then Some x else None)

let s_block (l:t list) : t =
  Block (
    List.filter (function
      | Block [] -> false
      | Decl [] -> false
      | _ -> true
    ) l
  )

let s_for (r:Range.t) (s:t) : t =
  match s with
  | Block [] -> Block []
  | Decl [] -> Decl []
  | _ -> For (r, s)

let s_if (b:Exp.bexp) (p1:t) (p2:t) : t =
  match b, p1, p2 with
  | (Bool false, _, p)
  | (Bool true, p, _)
    -> p
  | (_, Block [], Block []) -> Block []
  | _ -> If (b, p1, p2)

let assign (ty:C_type.t) (var:Variable.t) (data:Exp.nexp) : t =
  Assign {ty; var; data}

let to_s: t -> Indent.t list =
  let rec stmt_to_s : t -> Indent.t list =
    function
    | Call c -> [Line (Call.to_string c)]
    | Sync _ -> [Line "sync;"]
    | Assert b -> [Line (Assert.to_string b ^ ";")]
    | Atomic r -> [Line (C_type.to_string r.ty ^ " " ^ Variable.name r.target ^ " = atomic " ^ Variable.name r.array ^ Access.index_to_string r.index ^ ";")]
    | Read r ->
      let ty = C_type.to_string r.ty in
      let x = Variable.name r.target in
      let a = Variable.name r.array in
      let idx = Access.index_to_string r.index in
      [
        Line (ty ^ " " ^ x ^ " = rd " ^ a ^ idx ^ ";")
      ]
    | Write w ->
      let payload :string = match w.payload with
        | None -> ""
        | Some x -> " = " ^ string_of_int x
      in
      [Line ("wr " ^ Variable.name w.array ^ Access.index_to_string w.index ^ payload ^ ";")]
    | Block [] -> []
    | Block l -> [Line "{"; Block (List.map stmt_to_s l |> List.flatten); Line "}"]
    | Assign a -> [Line (Variable.name a.var ^ " = " ^ Exp.n_to_string a.data ^ ";")]
    | LocationAlias l ->
      [Line ("alias " ^ Alias.to_string l)]
    | Decl [] -> []
    | Decl l ->
      let entries = Common.join "," (List.map Decl.to_string l) in
      [Line ("decl " ^ entries ^ ";")]

    | If (b, s1, Block []) -> [
        Line ("if (" ^ Exp.b_to_string b ^ ") {");
        Block (stmt_to_s s1);
        Line "}";
      ]

    | If (b, s1, s2) -> [
        Line ("if (" ^ Exp.b_to_string b ^ ") {");
        Block (stmt_to_s s1);
        Line "} else {";
        Block (stmt_to_s s2);
        Line "}"
      ]
    | Star s -> [
        Line ("foreach (?) {");
        Block (stmt_to_s s);
        Line ("}")
      ]
    | For (r, s) -> [
        Line ("foreach (" ^ Range.to_string r ^ ") {");
        Block (stmt_to_s s);
        Line ("}")
      ]
  in
  stmt_to_s

let to_string (s: t) : string =
  Indent.to_string (to_s s)
