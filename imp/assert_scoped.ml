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

let inline_assigns (known:Variable.Set.t) : t -> t =
  let n_subst (st:Subst.SubstAssoc.t) (n:Exp.nexp): Exp.nexp =
    if Subst.SubstAssoc.is_empty st
    then n
    else Subst.ReplaceAssoc.n_subst st n
  in
  let b_subst (st:Subst.SubstAssoc.t) (b:Exp.bexp): Exp.bexp = if Subst.SubstAssoc.is_empty st
    then b
    else Subst.ReplaceAssoc.b_subst st b
  in
  let a_subst (st:Subst.SubstAssoc.t) (a:Access.t): Access.t =
    if Subst.SubstAssoc.is_empty st
    then a
    else Subst.ReplaceAssoc.a_subst st a
  in
  let r_subst (st:Subst.SubstAssoc.t) (r:Range.t): Range.t =
    if Subst.SubstAssoc.is_empty st
    then r
    else Subst.ReplaceAssoc.r_subst st r
  in
  let rec inline (known:Variable.Set.t) (st:Subst.SubstAssoc.t) (i:t) : t =
    let add_var (x:Variable.t) : Variable.t * Variable.Set.t * Subst.SubstAssoc.t =
      let x, st =
        if Variable.Set.mem x known
        then (
          let new_x = Variable.fresh known x in
          (new_x, Subst.SubstAssoc.put st x (Var new_x))
        ) else (x, st)
      in
      let known = Variable.Set.add x known in
      (x, known, st)
    in
    match i with
    | Sync l -> Sync l
    | Assert (b, s) -> Assert (b_subst st b, inline known st s)
    | Acc (x,e) -> Acc (x, a_subst st e)
    | Skip -> Skip
    | If (b, p1, p2) ->
      let b = b_subst st b in
      If (b, inline known st p1, inline known st p2)

    | Decl ({var=x; init=Some n; _}, p)
    | Assign {var=x; data=n; body=p; _} ->
      let n = n_subst st n in
      let st = Subst.SubstAssoc.put st x n  in
      inline known st p
    | Decl ({init=None; _} as d, p) ->
      Decl (d, inline known st p)
    | For (r, p) ->
      let r = r_subst st r in
      let (x, known, st) = add_var r.var in
      For ({r with var = x}, inline known st p)
    | Seq (p1, p2) ->
      Seq (inline known st p1, inline known st p2)
  in
  fun p ->
    p
    |> vars_distinct
    |> inline known (Subst.SubstAssoc.make [])

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

module AssertionTree = struct
  open Exp

  type t =
    | True
    | Cond of bexp
    | Append of t * t
    | And of Exp.bexp * t
    | Implies of Exp.bexp * t

  let true_ : t = True

  let rec from_bexp : bexp -> t =
    let open Exp in
    function
    | Bool true -> True
    | BRel (BAnd, e1, e2) ->
      and_ e1 (from_bexp e2)
    | e -> Cond e

  and and_ (e1:bexp) (e2:t) : t =
    match e1, e2 with
    | Bool true, e -> e
    | e, True -> from_bexp e
    | Bool false, _ -> Cond (Bool false)
    | _, _ -> And (e1, e2)

  let append (e1:t) (e2:t) : t =
    match e1, e2 with
    | True, e | e, True -> e
    | _, _ -> Append (e1, e2)


  let implies (e1:bexp) (e2:t) =
    match e1, e2 with
    | Bool true, e -> e
    | Bool false, _ -> True
    | _, _ -> Implies (e1, e2)

  let rec to_bexp : t -> Exp.bexp =
    let open Exp in
    function
    | True -> Bool true
    | Cond b -> b
    | Append (a, b) -> b_and (to_bexp a) (to_bexp b)
    | And (b, p) -> b_and b (to_bexp p)
    | Implies (b, p) -> b_impl b (to_bexp p)

  let retain (x:Variable.t) : t -> bexp =
    let b_retain (b:bexp) : bexp =
      if Exp.b_mem x b then b else Bool true
    in
    let rec retain : t -> t =
      function
      | True -> True
      | Cond b ->
        if Exp.b_mem x b then (Cond b) else True
      | Append (a, b) ->
        append (retain a) (retain b)
      | And (a, b) ->
        and_ (b_retain a) (retain b)
      | Implies (a, b) ->
        implies (b_retain a) (retain b)
    in
    fun e ->
      retain e |> to_bexp

  let remove (x:Variable.t) : t -> t =
    let open Exp in
    let b_remove (e:bexp) : bexp =
      if Exp.b_mem x e then (Bool true) else e
    in
    let rec remove : t -> t =
      function
      | True -> True
      | Cond b -> if Exp.b_mem x b then True else (Cond b)
      | And (a, b) ->
        and_ (b_remove a) (remove b)
      | Implies (a, b) ->
        implies (b_remove a) (remove b)
      | Append (a, b) ->
        append (remove a) (remove b)
    in
    remove

end

let to_scoped : t -> Scoped.t =
  let open Exp in
  let rec to_scoped :  t -> Scoped.t * AssertionTree.t =
    function
    | Skip -> Scoped.Skip, AssertionTree.true_
    | Acc (x, e) -> Scoped.Acc (x, e), AssertionTree.true_
    | Seq (p, q) ->
      let p, a1 = to_scoped p in
      let q, a2 = to_scoped q in
      (Scoped.Seq (p, q), AssertionTree.append a1 a2)
    | Assert (e, p) ->
      let p, a = to_scoped p in
      p, AssertionTree.and_ e a
    | If (b, then_s, else_s) ->
      let then_s, a1 = to_scoped then_s in
      let else_s, a2 = to_scoped else_s in
      let a = AssertionTree.(append
        (implies b a1)
        (implies (b_not b) a2)
      ) in
      Scoped.If (b, then_s, else_s), a
    | For (r, p) ->
      let p, a = to_scoped p in
      let guard = AssertionTree.retain (Range.var r) a in
      let a = AssertionTree.remove (Range.var r) a in
      (
        Scoped.For (r, Scoped.if_ guard p),
        AssertionTree.implies (Range.has_next r) a
      )
    | Sync e -> Scoped.Sync e, AssertionTree.true_
    | Assign e ->
      let p, a = to_scoped e.body in
      Scoped.Assign {var=e.var; ty=e.ty; data=e.data; body=p}, a
    | Decl (e, p) ->
      let p, a = to_scoped p in
      let guard = AssertionTree.retain e.var a in
      let a = AssertionTree.remove e.var a in
      Decl (e, Scoped.if_ guard p), a
  in
  fun e ->
    let p, a = to_scoped e in
    Scoped.if_ (AssertionTree.to_bexp a) p
