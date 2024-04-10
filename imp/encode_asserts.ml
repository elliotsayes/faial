open Stage0
open Protocols

type t =
  | Acc of Variable.t * Access.t
  | Sync of Location.t option
  | If of Exp.bexp * t * t
  | For of Range.t * t
  | Seq of t * t
  | Skip
  | Decl of {var: Variable.t; ty:C_type.t; body: t}

let to_string: t -> string =
  let rec to_s : t -> Indent.t list =
    function
    | Skip -> [Line "skip;"]
    | Sync _ -> [Line "sync;"]
    | Acc (x, e) -> [Line (Access.to_string ~name:(Variable.name x) e)]
    | Decl d ->
      [
        Line (C_type.to_string d.ty ^ " " ^ Variable.name d.var ^ " {");
        Block (to_s d.body);
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
  | Decl {var=x; ty; body=p} -> Proto.Code.decl ~ty x (to_proto p)
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

let from_encode_assigns : Encode_assigns.t -> t =
  let open Exp in
  let rec from : Encode_assigns.t -> t * AssertionTree.t =
    function
    | Skip -> Skip, AssertionTree.true_
    | Acc (x, e) -> Acc (x, e), AssertionTree.true_
    | Seq (p, q) ->
      let p, a1 = from p in
      let q, a2 = from q in
      (Seq (p, q), AssertionTree.append a1 a2)
    | Assert e -> Skip, AssertionTree.from_bexp e
    | If (b, then_s, else_s) ->
      let then_s, a1 = from then_s in
      let else_s, a2 = from else_s in
      let a = AssertionTree.(append
        (implies b a1)
        (implies (b_not b) a2)
      ) in
      If (b, then_s, else_s), a
    | For (r, p) ->
      let p, a = from p in
      let guard = AssertionTree.retain (Range.var r) a in
      let a = AssertionTree.remove (Range.var r) a in
      (
        For (r, if_ guard p),
        AssertionTree.implies (Range.has_next r) a
      )
    | Sync e -> Sync e, AssertionTree.true_
    | Decl {var; ty; body=p}->
      let p, a = from p in
      let guard = AssertionTree.retain var a in
      let a = AssertionTree.remove var a in
      Decl {var; ty; body=if_ guard p}, a
  in
  fun e ->
    let p, a = from e in
    if_ (AssertionTree.to_bexp a) p
