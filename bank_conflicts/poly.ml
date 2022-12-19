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

let make (e:Exp.nexp) (n:int) : t =
  if n = 0 then
    Exp0 e
  else if n = 1 then
    Exp1 {constant=Num 0; coefficient=e}
  else
    let ht = Hashtbl.create 1 in
    Hashtbl.add ht n e;
    Many ht

let update_ht (ht:('a, 'b) Hashtbl.t) (k:'a)  (f:'b option -> 'b)  : unit =
  Hashtbl.replace ht k (f (Hashtbl.find_opt ht k))

let poly_update_ht (ht:poly_ht) (k:int) (f:Exp.nexp -> Exp.nexp) : unit =
  update_ht ht k (function | Some v -> f v | None -> f (Num 0))

let poly_add_ht (src:poly_ht) (dst:poly_ht) : unit =
  Hashtbl.iter (fun i n ->
    poly_update_ht dst i (Exp.n_plus n)
  ) src

(* Given an exponent, return the coefficient *)
let get (exponent:int) : t -> Exp.nexp =
  function
  | Exp0 n
  | Exp1 {constant=n; _} -> n
  | Many ht ->
    Option.value (Hashtbl.find_opt ht exponent) ~default:(Num 0)

let add (e1:t) (e2:t) : t =
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

let uminus : t -> t =
  let open Exp in
  let u_minus = n_mult (Num (-1)) in
  map (fun e _ -> u_minus e)

let rec mult (e1:t) (e2:t) : t =
  let open Exp in
  let mult_ht (src:poly_ht) ((i1,n1):int*nexp) : poly_ht =
    (* z * x * (a + b*x + c*x^2) = a * z * x + z * b * x ^ 2 ... *)
    let dst = Hashtbl.create (Hashtbl.length src) in
    Hashtbl.iter (fun i2 n2 ->
      Hashtbl.add dst (i1 + i2) (n_mult n1 n2)
    ) src;
    dst
  in
  let mk_poly_ht (n1:nexp) (n2:nexp) : poly_ht =
    let ht = Hashtbl.create 2 in
    Hashtbl.add ht 0 n1;
    Hashtbl.add ht 1 n2;
    ht
  in
  match e1, e2 with
  | Exp0 n1, Exp0 n2 ->
    Exp0 (n_mult n1 n2)

  | Exp0 n1, Exp1 {constant=n2; coefficient=n3}
  | Exp1 {constant=n2; coefficient=n3}, Exp0 n1 ->
    Exp1 {constant=n_mult n1 n2; coefficient=n_mult n1 n3}

  | Exp1 {constant=n1; coefficient=n2}, Exp1 {constant=n3; coefficient=n4} ->
      let ht' = mult_ht (mk_poly_ht n3 n4) (1, n2) in
      add (mult (Exp0 n1) e2) (Many ht')

  | Exp0 n1, Many ht
  | Many ht, Exp0 n1 ->
    Common.hashtbl_elements ht
    |> List.map (fun (i, n) -> (i, n_mult n n1))
    |> Common.hashtbl_from_list
    |> (fun ht -> Many ht)

  | Exp1 {constant=n1; coefficient=n2}, Many ht
  | Many ht, Exp1 {constant=n1; coefficient=n2}
    -> mult (Many (mk_poly_ht n1 n2)) (Many ht)
  | Many ht1, Many ht2 ->
    let ht = Hashtbl.create ((Hashtbl.length ht1) * (Hashtbl.length ht2)) in
    Common.hashtbl_elements ht1
    |> List.map (mult_ht ht2)
    |> List.iter (fun src ->
      poly_add_ht src ht
    );
    Many ht

let rec filter (to_keep: Exp.nexp -> int -> bool) (p: t) : t =
  match p with
  | Exp0 n when to_keep n 0 -> Exp0 n
  | Exp0 _ -> Exp0 (Num 0)
  | Exp1 {coefficient=n1; constant=n0} when not (to_keep n1 1) ->
    filter to_keep (Exp0 n0)
  | Exp1 {coefficient=n1; constant=n0} when not (to_keep n0 0) ->
    Exp1 {coefficient=n1; constant=Num 0}
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
  let open Exp in
  let rec pow : int -> nexp = function
    | 0 -> Num 0
    | 1 -> base
    | n -> n_mult base (pow (n - 1))
  in
  if exponent < 0 then
    invalid_arg "exponent can not be negative"
  else
    pow exponent

let to_nexp (x:Variable.t) : t -> Exp.nexp =
  let open Exp in
  function
  | Exp0 n -> n
  | Exp1 {constant=n0; coefficient=n1} ->
    n_plus n0 (n_mult n1 (Var x))
  | Many ht ->
    Hashtbl.fold (fun exponent coef accum ->
      n_plus (n_mult coef (pow (Var x) exponent))
             accum
    ) ht (Num 0)

let to_seq : t -> (Exp.nexp*int) Seq.t =
  function
  | Exp0 x -> Seq.return (x, 0)
  | Exp1 {constant=n1; coefficient=n2} -> [n1, 0; n2, 1] |> List.to_seq
  | Many ht ->
    Common.hashtbl_elements ht
    |> List.to_seq
    |> Seq.map (fun (x, y) -> (y, x))

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
    | c -> String.make 1 c
    in
    c ^ a
  ) (string_of_int n) ""

let to_string ?(var="x") (p: t) : string =
  to_seq p
  |> Seq.map (fun (coef, pow) ->
    Serialize.PPrint.n_to_s coef ^ "·" ^ var ^ exponent_to_string pow
  )
  |> List.of_seq
  |> Common.join " + "
(*
let to_string ?(var="x") (p:t) =
  let open Serialize in
  let open PPrint in
  match p with
  | Exp0 n -> n_to_s n
  | Exp1 {constant=n1; coefficient=n2} -> n_par n1 ^ " + " ^ n_par n2 ^ " * " ^ var
  | Many ht ->
    Common.hashtbl_elements ht
    |> List.map (fun (k, v) -> n_par v ^ " * " ^ var ^ "^" ^ (string_of_int k))
    |> Common.join " + "
*)
let rec from_nexp (x:Variable.t) (n:Exp.nexp) : t =
  let open Exp in
  match n with
  | Var y when Variable.equal x y ->
    Exp1 {constant=Num 0; coefficient=Num 1}
  | Bin (Plus, e1, e2) ->
    add (from_nexp x e1) (from_nexp x e2)
  | Bin (Minus, e1, e2) ->
    add (from_nexp x e1) (uminus (from_nexp x e2))
  | Bin (Mult, e1, e2) ->
    mult (from_nexp x e1) (from_nexp x e2)
  | Bin (Div, e1, e2) ->
    mult (from_nexp x e1) (from_nexp x e2 |> map
      (fun x _ -> Bin (Div, (Num 1), x))
    )
  | Var _
  | Bin _
  | Num _
  | Proj _
  | NCall _
  | NIf _ -> Exp0 n

