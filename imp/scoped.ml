open Stage0
open Protocols

type t =
  | Skip
  | Sync of Location.t option
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

let vars_distinct : t -> t =
  let rec distinct (vars:Variable.Set.t) (p: t) : Variable.Set.t * t =
    match p with
    | Acc _ | Skip | Sync _ -> vars, p
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

let if_ (b:Exp.bexp) (s:t) : t =
  match b with
  | Bool true -> s
  | Bool false -> Skip
  | _ -> If (b, s, Skip)

let rec to_proto : t -> Proto.Code.t =
  function
  | Sync l -> Sync l
  | Acc (x,e) -> Acc (x, e)
  | Skip -> Skip
  | If (b, p1, p2) ->
    Proto.Code.seq
      (Proto.Code.cond b (to_proto p1))
      (Proto.Code.cond (Exp.b_not b) (to_proto p2))
  | For (r, p) ->
    Loop (r, to_proto p)
  | Assign _ as i ->
    failwith ("Run inline_decl first: " ^ to_string i)
  | Decl ({init=Some _; _}, _) as i ->
    failwith ("Run inline_decl first: " ^ to_string i)
  | Decl ({var=x; init=None; ty}, p) -> Proto.Code.decl ~ty x (to_proto p)
  | Seq (i, p) ->
    Proto.Code.seq (to_proto i) (to_proto p)

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

let from_assert_scoped : Assert_scoped.t -> t =
  let open Exp in
  let rec to_scoped : Assert_scoped.t -> t * AssertionTree.t =
    function
    | Skip -> Skip, AssertionTree.true_
    | Acc (x, e) -> Acc (x, e), AssertionTree.true_
    | Seq (p, q) ->
      let p, a1 = to_scoped p in
      let q, a2 = to_scoped q in
      (Seq (p, q), AssertionTree.append a1 a2)
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
      If (b, then_s, else_s), a
    | For (r, p) ->
      let p, a = to_scoped p in
      let guard = AssertionTree.retain (Range.var r) a in
      let a = AssertionTree.remove (Range.var r) a in
      (
        For (r, if_ guard p),
        AssertionTree.implies (Range.has_next r) a
      )
    | Sync e -> Sync e, AssertionTree.true_
    | Assign e ->
      let p, a = to_scoped e.body in
      Assign {var=e.var; ty=e.ty; data=e.data; body=p}, a
    | Decl (e, p) ->
      let p, a = to_scoped p in
      let guard = AssertionTree.retain e.var a in
      let a = AssertionTree.remove e.var a in
      Decl (e, if_ guard p), a
  in
  fun e ->
    let p, a = to_scoped e in
    if_ (AssertionTree.to_bexp a) p
