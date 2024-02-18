open Protocols
open Stage0

type poly_ht = (int, Exp.nexp) Hashtbl.t

type t =
  | Exp0 of Exp.nexp
  | Exp1 of {constant: Exp.nexp; coefficient: Exp.nexp}
  | Many of poly_ht

let max_exponent : t -> int =
  function
  | Exp0 _ -> 0
  | Exp1 _ -> 1
  | Many ht -> Hashtbl.to_seq_keys ht |> Seq.fold_left max 0

let min_exponent : t -> int =
  function
  | Exp0 _ -> 0
  | Exp1 _ -> 0
  | Many ht ->
    Hashtbl.to_seq_keys ht |> Seq.fold_left min 0

let update_ht (ht:('a, 'b) Hashtbl.t) (k:'a)  (f:'b option -> 'b)  : unit =
  Hashtbl.replace ht k (f (Hashtbl.find_opt ht k))

let map (f:Exp.nexp -> int -> Exp.nexp) : t -> t =
  function
  | Exp0 n -> Exp0 (f n 0)
  | Exp1 {constant=n0; coefficient=n1} ->
    Exp1 {constant=f n0 0; coefficient=f n1 1}
  | Many ht ->
    let ht' = Hashtbl.copy ht in
    Hashtbl.filter_map_inplace (fun expi coef ->
      Some (f coef expi)
    ) ht';
    Many ht'

let map1 (f:Exp.nexp -> Exp.nexp) : t -> t =
  map (fun e _ -> f e)

let exponent_to_string (n:int) : string =
  String.fold_right (fun c a ->
    let c = match c with
    | '0' -> "⁰"
    | '1' -> "¹"
    | '2' -> "²"
    | '3' -> "³"
    | '4' -> "⁴"
    | '5' -> "⁵"
    | '6' -> "⁶"
    | '7' -> "⁷"
    | '8' -> "⁸"
    | '9' -> "⁹"
    | '-' -> "⁻"
    | c -> String.make 1 c
    in
    c ^ a
  ) (string_of_int n) ""

let poly_update_ht (ht:poly_ht) (k:int) (f:Exp.nexp -> Exp.nexp) : unit =
  update_ht ht k (function | Some v -> f v | None -> f Exp.n_zero)

let poly_add_ht (src:poly_ht) (dst:poly_ht) : unit =
  Hashtbl.iter (fun i n ->
    let open Exp in
    poly_update_ht dst i (n_plus n)
  ) src

let copy_hashtbl ?(expected_size=10) : t -> poly_ht =
  function
  | Exp0 n ->
    let ht = Hashtbl.create expected_size in
    Hashtbl.add ht 0 n;
    ht
  | Exp1 {constant=n0; coefficient=n1} ->
    let ht = Hashtbl.create expected_size in
    Hashtbl.add ht 0 n0;
    Hashtbl.add ht 1 n1;
    ht
  | Many ht ->
    Hashtbl.copy ht

let hashtbl ?(expected_size=10) : t -> poly_ht =
  function
  | Exp0 n ->
    let ht = Hashtbl.create expected_size in
    Hashtbl.add ht 0 n;
    ht
  | Exp1 {constant=n0; coefficient=n1} ->
    let ht = Hashtbl.create expected_size in
    Hashtbl.add ht 0 n0;
    Hashtbl.add ht 1 n1;
    ht
  | Many ht ->
    ht

let make ?(expected_size=10) (e:Exp.nexp) (n:int) : t =
  if n = 0 then
    Exp0 e
  else if n = 1 then
    Exp1 {constant=Exp.n_zero; coefficient=e}
  else
    let ht = Hashtbl.create expected_size in
    Hashtbl.add ht n e;
    Many ht

let constant (k:int) = make (Num k) 0

let zero : t = constant 0

let to_list : t -> (Exp.nexp * int) list =
  function
  | Exp0 x ->
    [(x, 0)]
  | Exp1 {constant=n1; coefficient=n2} ->
    [(n1, 0); (n2, 1)]
  | Many ht ->
    Common.hashtbl_elements ht
    |> List.map (fun (x, y) -> (y, x))

let add (e1: t) (e2: t) : t =
  let open Exp in
  match e1, e2 with
  | Exp0 n1, Exp0 n2 -> Exp0 (n_plus n1 n2)
  | Exp0 n1, Exp1 {constant=n2; coefficient=n3}
  | Exp1 {constant=n2; coefficient=n3}, Exp0 n1 ->
    Exp1 {constant=n_plus n2 n1; coefficient=n3}
  | Exp1 {constant=n1; coefficient=n2}, Exp1 {constant=n3; coefficient=n4} ->
    Exp1 {constant=n_plus n1 n3; coefficient=n_plus n2 n4}
  | Exp0 n1, Many ht
  | Many ht, Exp0 n1 ->
    let ht = Hashtbl.copy ht in
    poly_update_ht ht 0 (n_plus n1);
    Many ht
  | Exp1 {constant=n1; coefficient=n2}, Many ht
  | Many ht, Exp1 {constant=n1; coefficient=n2} ->
    let ht = Hashtbl.copy ht in
    poly_update_ht ht 0 (n_plus n1);
    poly_update_ht ht 1 (n_plus n2);
    Many ht
  | Many ht1, Many ht2 ->
    let ht2 = Hashtbl.copy ht2 in
    poly_add_ht ht1 ht2;
    Many ht2

let map_exponent (f:int -> int) : t -> t =
  function
  | Exp0 n when f 0 = 0 -> Exp0 n
  | p ->
    let ht =
      p
      |> copy_hashtbl
      |> Common.hashtbl_elements
      |> List.map (fun (x, y) -> (f x, y))
      |> Common.hashtbl_from_list
    in
    Many ht

let add_exponent (amount:int) : t -> t =
  map_exponent (fun x -> x + amount)

let to_seq : t -> (Exp.nexp*int) Seq.t =
  function
  | Exp0 x -> Seq.return (x, 0)
  | Exp1 {constant=n1; coefficient=n2} -> [n1, 0; n2, 1] |> List.to_seq
  | Many ht ->
    Common.hashtbl_elements ht
    |> List.to_seq
    |> Seq.map (fun (x, y) -> (y, x))

(* Given an exponent, return the coefficient *)

let get_opt (exponent:int) : t -> Exp.nexp option =
  function
  | Exp0 n when exponent = 0 -> Some n
  | Exp0 _ -> None
  | Exp1 {constant=n; _} when exponent = 0 -> Some n
  | Exp1 {coefficient=n; _} when exponent = 1 -> Some n
  | Exp1 _ -> None
  | Many ht -> Hashtbl.find_opt ht exponent

let get (exponent:int) (p: t) : Exp.nexp =
  Option.value (get_opt exponent p) ~default:Exp.n_zero

let compare ((_:Exp.nexp), (exp1:int)) ((_:Exp.nexp), (exp2:int)) : int =
  compare exp1 exp2

let to_seq_ord (p: t) : (Exp.nexp*int) Seq.t =
  p
  |> to_list
  |> List.sort compare
  |> List.to_seq

let uminus : t -> t =
  map1 Exp.n_uminus

let mult1 (e:Exp.nexp) : t -> t =
  map1 (Exp.n_mult e)

let mult2 (coefficient:Exp.nexp) (exponent:int) (p:t) : t =
  let p = mult1 coefficient p in
  if exponent = 0 then
    p
  else
    add_exponent exponent p

let mult (e1:t) (e2:t) : t =
  let open Exp in
  match e1, e2 with
  | Exp0 n1, Exp0 n2 ->
    Exp0 (n_mult n1 n2)

  | Exp0 n1, Exp1 {constant=n2; coefficient=n3}
  | Exp1 {constant=n2; coefficient=n3}, Exp0 n1 ->
    Exp1 {constant=n_mult n1 n2; coefficient=n_mult n1 n3}

  | Exp0 n1, p
  | p, Exp0 n1 ->
    mult1 n1 p

  | Exp1 {constant=n1; coefficient=n2}, p
  | p, Exp1 {constant=n1; coefficient=n2} ->
    let p1 = mult1 n1 p in
    let p2 = mult2 n2 1 p in
    add p1 p2

  | Many ht1, Many ht2 ->
    let (ht1, ht2) =
      if Hashtbl.length ht1 < Hashtbl.length ht2 then
        (ht1, ht2)
      else
        (ht2, ht1)
    in
    let p2 = Many ht2 in
    Hashtbl.fold (fun exponent coefficient r ->
      add (mult2 coefficient exponent p2) r
    ) ht1 zero

let div1 (p:t) (e:Exp.nexp) : t =
  p
  |> map1 (fun x ->
    if x <> Num 0 then
      Bin (Div, x, e)
    else
      x
  )


let from_list : (Exp.nexp * int) list -> t =
  function
  | [ (e, 0); (Num 0, _) ]
  | [ (Num 0, _); (e, 0) ]
  | [ (e, 0) ] -> Exp0 e
  | [ (n0, 0); (n1, 1) ]
  | [ (n1, 1); (n0, 0) ] -> Exp1 {constant=n0; coefficient=n1}
  | l ->
    let ht =
      l
      |> List.filter (fun (coef, _) -> let open Exp in coef <> Num 0)
      |> List.map (fun (x, y) -> (y, x))
      |> Common.hashtbl_from_list
    in
    Many ht

let optimize (p: t) : t =
  p
  |> map1 Constfold.n_opt
  |> to_list
  |> from_list

let inverse (p:t) : t =
  let inv : Exp.nexp -> Exp.nexp =
    function
    | Num 0 -> Num 0
    | n -> Bin (Div, Num 1, n)
  in
  match p with
  | Exp0 n -> Exp0 (inv n)
  | _ ->
    to_list p
    |> List.map (fun (coef, pow) -> (inv coef, (-pow)))
    |> from_list

let div (e1:t) (e2:t) : t =
  mult e1 (inverse e2)

let rec filter (to_keep: Exp.nexp -> int -> bool) (p: t) : t =
  match p with
  | Exp0 n when to_keep n 0 -> Exp0 n
  | Exp0 _ -> Exp0 (Exp.n_zero)
  | Exp1 {coefficient=n1; constant=n0} when not (to_keep n1 1) ->
    filter to_keep (Exp0 n0)
  | Exp1 {coefficient=n1; constant=n0} when not (to_keep n0 0) ->
    Exp1 {coefficient=n1; constant=Exp.n_zero}
  | Exp1 _ -> p
  | Many ht ->
    let ht' = Hashtbl.copy ht in
    Hashtbl.filter_map_inplace (fun expi coef ->
      if to_keep coef expi then
        Some coef
      else None
    ) ht';
    Many ht'

let pow (base:Exp.nexp) (exponent: int) : Exp.nexp =
  let rec pow : int -> Exp.nexp = function
    | 0 -> Num 1
    | 1 -> base
    | n -> Bin (Mult, base, pow (n - 1))
  in
  if exponent < 0 then
    Bin (Div, Num 1, pow (-exponent))
  else
    pow exponent

let to_string ?(skip_zero=true) ?(sort=true) ?(var="x") (p: t) : string =
  let p = to_list p in
  let handle b f l = if b then f l else l in
  let result =
    p
    |> handle sort (List.sort compare)
    |> handle skip_zero (List.filter (fun (coef, _) -> coef <> Exp.Num 0))
    |> List.map (fun (coef, pow) ->
      let open Exp in
      let s_coef = match coef with
        | Num _
        | Var _ -> Exp.n_to_string coef
        | _ -> "(" ^ Exp.n_to_string coef ^ ")"
      in
      if pow = 0 then
        Exp.n_to_string coef
      else if pow = 1 then
        s_coef ^ "·" ^ var
      else
        s_coef ^ "·" ^ var ^ exponent_to_string pow
    )
    |> String.concat " + "
  in
  if result = "" then "0" else result

let to_nexp (x:Variable.t) : t -> Exp.nexp =
  let open Exp in
  function
  | Exp1 {constant=n; coefficient=Num 0}
  | Exp0 n -> n
  | Exp1 {constant=Num 0; coefficient=n1} -> Bin (Mult, n1, Var x)
  | Exp1 {constant=n0; coefficient=n1} ->
    Bin (Plus, n0, Bin (Mult, n1, Var x))
  | Many ht ->
    Hashtbl.fold (fun exponent coef accum ->
      Bin (Plus, Bin (Mult, coef, pow (Var x) exponent),
             accum)
    ) ht (Num 0)

let equal (p1 : t) (p2: t) : bool =
  let p1 = optimize p1 in
  let p2 = optimize p2 in
  match p1, p2 with
  | Exp0 n0, Exp0 n0' -> n0 = n0'
  | Exp1 {constant=n0; coefficient=n1},
    Exp1 {constant=n0'; coefficient=n1'} ->
      n0 = n0' && n1 = n1'
  | Many ht, Many ht' ->
    if Hashtbl.length ht = Hashtbl.length ht' then
      Hashtbl.fold (fun k v p ->
        p && Hashtbl.find_opt ht k = Some v
      ) ht' true
    else
      false
  | _, _ -> false

let rec from_nexp (x:Variable.t) (n:Exp.nexp) : t option =
  let open Exp in
  let (let*) = Option.bind in
  match n with
  | Var y when Variable.equal x y ->
    Some (Exp1 {constant=Num 0; coefficient=Num 1})
  | Bin (Plus, e1, e2) ->
    let* e1 = from_nexp x e1 in
    let* e2 = from_nexp x e2 in
    Some (add e1 e2)
  | Bin (Minus, e1, e2) ->
    let* e1 = from_nexp x e1 in
    let* e2 = from_nexp x e2 in
    Some (add e1 (uminus e2))
  | Bin (Mult, e1, e2) ->
    let* e1 = from_nexp x e1 in
    let* e2 = from_nexp x e2 in
    Some (mult e1 e2)
  | Bin (Div, e1, e2) ->
    let* e1 = from_nexp x e1 in
    let* e2 = from_nexp x e2 in
    Some (div e1 e2)
  | Num _
  | Var _ -> Some (Exp0 n)
  | Bin _
  | NCall _
  | Other _
  | NIf _ -> None
