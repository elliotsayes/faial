open Stage0
open Protocols

module StringMap = Common.StringMap
module StringMapUtil = Common.StringMapUtil
module StringSet = Common.StringSet

type t =
  | Sync of Location.t option
  | Assert of Exp.bexp
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
    | Assert b -> [Line ("assert (" ^ Exp.b_to_string b ^ ");")]
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

type stateful = (int * Params.t) -> int * Params.t * Scoped.t


let unknown_range (x:Variable.t) : Range.t =
  Range.{
    var=Variable.from_name "?";
    dir=Increase;
    lower_bound=Num 1;
    upper_bound=Var x;
    step=Step.plus (Num 1);
    ty=C_type.int;
  }

let to_scoped : Params.t * t -> Params.t * Scoped.t =
  let unknown (x:int) : Variable.t =
    Variable.from_name ("__loop_" ^ string_of_int x)
  in
  let ret (p:Scoped.t) : stateful =
    fun (curr_id, globals) ->
      (curr_id, globals, p)
  in
  let bind (f:stateful) (g:Scoped.t -> stateful) : stateful =
    fun (curr_id, globals) ->
    let (curr_id, globals, s1) = f (curr_id, globals) in
    g s1 (curr_id, globals)
  in
  let rec imp_to_scoped_s : t -> (int * Params.t) -> int * Params.t * Scoped.t =
    function
    | Sync l -> ret (Scoped.Sync l)
    | Write e -> ret (Acc (e.array, {index=e.index; mode=Write e.payload}))
    | Read e ->
      fun (curr_id, globals) ->
        let rd = Scoped.Acc (e.array, {index=e.index; mode=Read}) in
        (curr_id, globals, Decl (Decl.unset ~ty:e.ty e.target, rd))
    | Atomic e ->
      fun (curr_id, globals) ->
        let rd = Scoped.Acc (e.array, {index=e.index; mode=Atomic e.atomic}) in
        (curr_id, globals, Decl (Decl.unset ~ty:e.ty e.target, rd))
    | Call _ -> imp_to_scoped_p []
    | Block p -> imp_to_scoped_p p
    | If (b, s1, s2) ->
      bind (imp_to_scoped_p [s1]) (fun s1 ->
        bind (imp_to_scoped_p [s2]) (fun s2 ->
          ret (Scoped.If (b, s1, s2))
        )
      )
    | For (r, s) ->
      bind (imp_to_scoped_p [s]) (fun s -> ret (Scoped.For (r, s)))
    | Star s ->
      let synchronized = has_sync s in
      bind (imp_to_scoped_p [s]) (fun s (curr_id, globals) ->
        let x = unknown curr_id in
        let r = unknown_range x in
        let s : Scoped.t = For (r, s) in
        if synchronized then
          (curr_id + 1, Params.add x C_type.char globals, s)
        else
          (curr_id, globals, Decl (Decl.unset x, s))
      )
    (* Handled in the context of a prog *)
    | Assert _ | LocationAlias _ | Decl _ | Assign _ ->
      failwith "unsupported"

  and imp_to_scoped_p : prog -> int*Params.t -> int * Params.t * Scoped.t =
    function
    | [] -> ret Skip
    | Assert b :: p ->
      bind (imp_to_scoped_p p) (fun p ->
        ret (Scoped.If (b, p, Skip))
      )
    | LocationAlias e :: p ->
      bind (imp_to_scoped_p p) (fun p ->
       ret (Scoped.loc_subst e p)
      )
    | Decl [] :: p -> imp_to_scoped_p p
    | Assign {var; data; ty;} :: p ->
      bind (imp_to_scoped_p p) (fun s ->
        ret (Scoped.Assign {var; data; ty; body=s})
      )
    | Decl (d::l) :: p ->
      bind (imp_to_scoped_p (Decl l :: p)) (fun s ->
        ret (Scoped.Decl (d, s))
      )
    | s :: p ->
      bind (imp_to_scoped_s s) (fun s ->
        bind (imp_to_scoped_p p) (fun p ->
          ret (Seq (s, p))
        )
      )
  in
  fun (globals, s) ->
    let (_, globals, p) = imp_to_scoped_s (Block [s]) (1, globals) in
    (globals, p)
