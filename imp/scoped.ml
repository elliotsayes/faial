open Stage0
open Protocols

type t =
  | Skip
  | Sync of Location.t option
  | Assert of (Exp.bexp * t)
  | Acc of (Variable.t * Access.t)
  | If of (Exp.bexp * t * t)
  | For of (Range.t * t)
  | Assign of {var: Variable.t; ty: C_type.t; data: Exp.nexp; body: t}
  | Decl of (Decl.t * t)
  | Seq of t * t

let to_string: t -> string =
  let rec to_s : t -> Indent.t list =
    function
    | Skip -> [Line "skip;"]
    | Sync _ -> [Line "sync;"]
    | Assert (b, s) ->
      [
        Line ("assert (" ^ Exp.b_to_string b ^ ") {");
        Block (to_s s);
        Line ("}");
      ]
    | Acc (x, e) -> [Line (Access.to_string ~name:(Variable.name x) e)]
    | Assign a -> [
        Line (Variable.name a.var ^ " = " ^ Exp.n_to_string a.data ^ " {");
        Block (to_s a.body);
        Line "}";
      ]
    | Decl (d, p) ->
      [
        Line ("decl " ^ Decl.to_string d ^ " {");
        Block (to_s p);
        Line "}";
      ]

    | If (b, s1, s2) -> [
        Line ("if (" ^ Exp.b_to_string b ^ ") {");
        Block (to_s s1);
        Line "} else {";
        Block (to_s s2);
        Line "}"
      ]

    | For (r, s) -> [
        Line ("foreach (" ^ Range.to_string r ^ ") {");
        Block (to_s s);
        Line ("}")
      ]
    | Seq (p, q) ->
      to_s p @ to_s q
  in
  fun p -> to_s p |> Indent.to_string

let loc_subst (alias:Alias.t) : t -> t =
  let rec loc_subst : t -> t =
    function
    | Acc (x, a) as i ->
      if Variable.equal x alias.target
      then (
        match a.index with
        | [n] ->
          (* use the inlined variable but with the location of the alias,
            so that the error message appears in the right place. *)
          let new_x = { alias.source with location = x.location } in
          Acc (new_x, { a with index = [Exp.n_plus alias.offset n] })
        | _ ->
          let idx = List.length a.index |> string_of_int in
          failwith ("Expecting an index with dimension 1, but got " ^ idx)
      )
      else i
    | Assert _ as i -> i
    | Decl (d, l) -> Decl (d, loc_subst l)
    | Assign a -> Assign {a with body = loc_subst a.body}
    | If (b, s1, s2) -> If (b, loc_subst s1, loc_subst s2)
    | For (r, s) -> For (r, loc_subst s)
    | Sync l -> Sync l
    | Skip -> Skip
    | Seq (p, q) ->
      Seq (loc_subst p, loc_subst q)
  in
  fun s ->
    if Alias.is_trivial alias then
      s
    else
      loc_subst s

module SubstMake(S:Subst.SUBST) = struct
  module M = Subst.Make(S)

  let o_subst (st:S.t): Exp.nexp option -> Exp.nexp option =
    function
    | Some n -> Some (M.n_subst st n)
    | None -> None

  let rec subst (st:S.t) : t -> t =
    function
    | Sync l -> Sync l
    | Skip -> Skip
    | Acc (x, a) -> Acc (x, M.a_subst st a)
    | Assert (b, s) -> Assert (M.b_subst st b, subst st s)
    | Decl (d, p) ->
      let d = Decl.map (M.n_subst st) d in
      Decl (d, M.add st d.var (function
        | Some st' -> subst st' p
        | None -> p
        )
      )
    | Assign a ->
      Assign { a with
        data = M.n_subst st a.data;
        body = M.add st a.var (function
          | Some st' -> subst st' a.body
          | None -> a.body
        );
      }
    | If (b, p1, p2) ->
      If (M.b_subst st b, subst st p1, subst st p2)
    | For (r, p) ->
      For (M.r_subst st r,
        M.add st r.var (function
        | Some st -> subst st p
        | None -> p
        )
      )
    | Seq (p, q) -> Seq (subst st p, subst st q)

end

module ReplacePair = SubstMake(Subst.SubstPair)
let subst = ReplacePair.subst

let filter_locs (locs:Memory.t Variable.Map.t) : t -> t =
  let rec filter : t -> t =
    function
    | Acc (x, _) as i ->
      if Variable.Map.mem x locs then i else Skip
    | Assert _ as i -> i
    | Skip -> Skip
    | Sync l -> Sync l
    | If (b, p1, p2) -> If (b, filter p1, filter p2)
    | For (r, p) -> For (r, filter p)
    | Decl (d, p) -> Decl (d, filter p)
    | Assign a -> Assign {a with body = filter a.body}
    | Seq (p1, p2) -> Seq (filter p1, filter p2)
  in
    filter

let vars_distinct : t -> t =
  let rec distinct (vars:Variable.Set.t) (p: t) : Variable.Set.t * t =
    match p with
    | Acc _ | Skip | Sync _ | Assert _ -> vars, p
    | Seq (p, q) ->
      let (vars, p) = distinct vars p in
      let (vars, q) = distinct vars q in
      vars, Seq (p, q)
    | If (b, p, q) ->
      let (vars, p) = distinct vars p in
      let (vars, q) = distinct vars q in
      vars, If (b, p, q)
    | Assign a ->
      let (vars, body) = distinct vars a.body in
      vars, Assign { a with body}
    | Decl (d, p) ->
      let x = d.var in
      if Variable.Set.mem x vars then (
        let new_x : Variable.t = Variable.fresh vars x in
        let vars = Variable.Set.add new_x vars in
        let p = subst (x, Var new_x) p in
        let (vars, p) = distinct vars p in
        vars, Decl ({ d with var=new_x;}, p)
      ) else
        let (vars, p) = distinct (Variable.Set.add x vars) p in
        vars, Decl (d, p)
    | For (r, p) ->
      let x = Range.var r in
      if Variable.Set.mem x vars then (
        let new_x : Variable.t = Variable.fresh vars x in
        let vars = Variable.Set.add new_x vars in
        let p = subst (x, Var new_x) p in
        let (vars, p) = distinct vars p in
        vars, For ({ r with var = new_x }, p)
      ) else
        let (vars, p) = distinct (Variable.Set.add x vars) p in
        vars, For (r, p)
  in
  fun p ->
    distinct Variable.Set.empty p |> snd

(* Rewrite assigns that cannot be represented as lets *)
let fix_assigns : t -> t =
  let decl (assigns:Params.t) (p:t) : t =
    Params.to_list assigns
    |> List.fold_left (fun p (x, ty) -> Decl (Decl.unset x ~ty, p)) p
  in
  let rec fix_assigns (defined:Params.t) (i:t) : Params.t * t =
    match i with
    | Skip | Sync _ | Acc _ | Assert _ -> (Params.empty, i)
    | If (b, p, q) ->
      let (assigns_1, p) = fix_assigns Params.empty p in
      let (assigns_2, q) = fix_assigns Params.empty q in
      (Params.union_left assigns_1 assigns_2, If (b, p, q))
    | Assign a ->
      let (assigns, body) = fix_assigns defined a.body in
      let assigns =
        if Params.mem a.var defined then
          (* already defined, so no need to record outstanding
              assignment *)
          assigns
        else
          Params.add a.var a.ty assigns
      in
      (assigns, Assign {a with body})
    | Decl (d, p) ->
      let defined = Params.add d.var d.ty defined in
      let (assigns, p) = fix_assigns defined p in
      (assigns, Decl (d, p))
    | Seq (If _ as p, q)
    | Seq (For _ as p, q) ->
      let (assigns_1, p) = fix_assigns defined p in
      let (assigns_2, q) = fix_assigns defined q in
      (assigns_2, Seq (p, decl assigns_1 q))
    | Seq (p, q) ->
      let (assigns_1, p) = fix_assigns defined p in
      let (assigns_2, q) = fix_assigns defined q in
      (Params.union_left assigns_1 assigns_2, Seq (p, q))

    | For (r, p) ->
      let (assigns, p) = fix_assigns Params.empty p in
      (* convert assigns to decls *)
      (assigns, For (r, decl assigns p))
  in
  fun s ->
    s
    |> fix_assigns Params.empty
    |> snd


type stateful = (int * Params.t) -> int * Params.t * t

let unknown_range (x:Variable.t) : Range.t =
  Range.{
    var=Variable.from_name "?";
    dir=Increase;
    lower_bound=Num 1;
    upper_bound=Var x;
    step=Step.plus (Num 1);
    ty=C_type.int;
  }

let from_stmt : Params.t * Stmt.t -> Params.t * t =
  let unknown (x:int) : Variable.t =
    Variable.from_name ("__loop_" ^ string_of_int x)
  in
  let ret (p:t) : stateful =
    fun (curr_id, globals) ->
      (curr_id, globals, p)
  in
  let bind (f:stateful) (g:t -> stateful) : stateful =
    fun (curr_id, globals) ->
    let (curr_id, globals, s1) = f (curr_id, globals) in
    g s1 (curr_id, globals)
  in
  let rec imp_to_scoped_s : Stmt.t -> (int * Params.t) -> int * Params.t * t =
    function
    | Sync l -> ret (Sync l)
    | Write e -> ret (Acc (e.array, {index=e.index; mode=Write e.payload}))
    | Assert b -> ret (Assert (b, Skip))
    | Call _ -> imp_to_scoped_p []
    | Block p -> imp_to_scoped_p p
    | If (b, s1, s2) ->
      bind (imp_to_scoped_p [s1]) (fun s1 ->
        bind (imp_to_scoped_p [s2]) (fun s2 ->
          ret (If (b, s1, s2))
        )
      )
    | For (r, s) ->
      bind (imp_to_scoped_p [s]) (fun s -> ret (For (r, s)))
    | Star s ->
      let synchronized = Stmt.has_sync s in
      bind (imp_to_scoped_p [s]) (fun s (curr_id, globals) ->
        let x = unknown curr_id in
        let r = unknown_range x in
        let s : t = For (r, s) in
        if synchronized then
          (curr_id + 1, Params.add x C_type.char globals, s)
        else
          (curr_id, globals, Decl (Decl.unset x, s))
      )
    (* Handled in the context of a prog *)
    | LocationAlias _ | Decl _ | Assign _ | Read _ | Atomic _ ->
      failwith "unsupported"

  and imp_to_scoped_p : Stmt.prog -> int*Params.t -> int * Params.t * t =
    function
    | [] -> ret Skip
    | Assert e :: p ->
      bind (imp_to_scoped_p p) (fun p ->
        ret (Assert (e, p))
      )
    | LocationAlias e :: p ->
      bind (imp_to_scoped_p p) (fun p ->
       ret (loc_subst e p)
      )
    | Decl [] :: p -> imp_to_scoped_p p
    | Assign {var; data; ty;} :: p ->
      bind (imp_to_scoped_p p) (fun s ->
        ret (Assign {var; data; ty; body=s})
      )
    | Decl (d::l) :: p ->
      bind (imp_to_scoped_p (Decl l :: p)) (fun s ->
        ret (Decl (d, s))
      )
    | Read e :: p ->
      bind (imp_to_scoped_p p) (fun s ->
        let rd = Acc (e.array, {index=e.index; mode=Read}) in
        ret (Decl (Decl.unset ~ty:e.ty e.target, Seq (rd, s)))
      )
    | Atomic e :: p ->
      bind (imp_to_scoped_p p) (fun s ->
        let rd = Acc (e.array, {index=e.index; mode=Atomic e.atomic}) in
        ret (Decl (Decl.unset ~ty:e.ty e.target, Seq (rd, s)))
      )
    | s :: p ->
      bind (imp_to_scoped_s s) (fun s ->
        bind (imp_to_scoped_p p) (fun p ->
          ret (Seq (s, p))
        )
      )
  in
  fun (globals, s) ->
    let (_, globals, p) = imp_to_scoped_s s (1, globals) in
    (globals, p)
