open Stage0
open Protocols

let pointwise (f:'a -> 'a -> 'b) (l1:'a list) (l2:'a list) :
  'b list
=
  assert (List.length l1 = List.length l2);
  List.map2 f l1 l2


let list_to_string (l:string list) : string =
  "[" ^ Common.join ", " l ^ "]"

type 'a index = {index:int; value:'a}

module NMap : sig
  type t
  val make : int -> (int -> int) -> t
  val constant : count:int -> value:int -> t
  val pointwise: (int -> int -> int) -> t -> t -> t
  val to_string : t -> string
  val map : (int -> int) -> t -> t
  val max : t -> int index
  val get : int -> t -> int
  val to_array: t -> int array
  val from_array: int array -> t
end = struct
  type t = int array

  let make (count:int) (f:int -> int) : t  = Array.init count f

  let constant ~count ~value : t = Array.make count value

  let pointwise : (int -> int -> int) -> t -> t -> t =
    Array.map2

  let to_string (l: t) : string =
    l |> Array.map string_of_int |> Array.to_list |> list_to_string

  let map f v = Array.map f v
  let get idx a = Array.get a idx
  let max (x:t) : int index =
    x
    |> Array.mapi (fun idx v -> {index=idx; value=v})
    |> Array.fold_left (fun p1 p2 ->
      if p1.value >= p2.value then p1 else p2
    ) {index=0;value=Array.get x 0}

  let to_array x = x

  let from_array x = x
end

module BMap : sig
  type t
  val make : int -> (int -> bool) -> t
  val constant : count:int -> value:bool -> t
  val pointwise: (bool -> bool -> bool) -> t -> t -> t
  val to_string : t -> string
  val some_true : t -> bool
  val all_false : t -> bool
  val map : (bool -> bool) -> t -> t
  val get : int -> t -> bool
  val to_array: t -> bool array
  val from_array: bool array -> t
end = struct
  type t = bool array

  let make (count:int) (f:int -> bool) : t = Array.init count f

  let constant ~count ~value : t = Array.make count value

  let pointwise : (bool -> bool -> bool) -> t -> t -> t =
    Array.map2

  let to_string (l: t) : string =
    l
    |> Array.map (fun x -> if x then "true" else "false")
    |> Array.to_list
    |> list_to_string

  let some_true : t -> bool =
    Array.exists (fun v -> v)

  let all_false : t -> bool =
    Array.for_all (fun v -> not v)

  let get (x: int) (v:t) : bool =
    Array.get v x

  let map = Array.map

  let to_array x = x
  let from_array x = x
end

let n_map3
  (f:bool -> int -> int -> int)
  (b:BMap.t)
  (n1:NMap.t)
  (n2:NMap.t)
:
  NMap.t
=
  let n1 : int array = NMap.to_array n1 in
  let n2 : int array = NMap.to_array n2 in
  let b : bool array = BMap.to_array b in
  Array.map2 (fun b (x1, x2) -> f b x1 x2) b (Array.combine n1 n2)
  |> NMap.from_array

let n_map2 (f:int -> int -> bool) (n1:NMap.t) (n2:NMap.t) : BMap.t =
  let n1 = NMap.to_array n1 in
  let n2 = NMap.to_array n2 in
  Array.map2 f n1 n2 |> BMap.from_array

module IntMap = Map.Make(struct type t = int let compare = compare end)
module IntSet = Set.Make(struct type t = int let compare = compare end)

type t = {
  bank_count: int;
  warp_count: int;
  cond: Exp.bexp;
  env: NMap.t Variable.Map.t;
  use_array: Variable.t -> bool
}

let make ~bank_count ~warp_count ~use_array : t = {
  cond = Exp.Bool true;
  env = Variable.Map.empty;
  bank_count;
  warp_count;
  use_array;
}

let restrict (b:Exp.bexp) (ctx:t) : t =
  let open Exp in
  { ctx with cond = b_and ctx.cond b }

let put (x:Variable.t) (v:NMap.t) (ctx:t) : t =
  { ctx with env = Variable.Map.add x v ctx.env }

let zero_cost (ctx:t) : NMap.t =
  NMap.constant ~count:ctx.bank_count ~value:0

let put_tids (block_dim:Dim3.t) (ctx:t) : t =
  let wids = NMap.make ctx.warp_count (fun x -> x) in
  let n_tidx = NMap.map (fun id ->
    id mod block_dim.x) wids
  in
  let n_tidy = NMap.map (fun id ->
    (id / block_dim.x) mod block_dim.y) wids
  in
  let n_tidz = NMap.map (fun id ->
    (id / (block_dim.x * block_dim.y)) mod block_dim.z) wids
  in
  ctx
  |> put Variable.tidx n_tidx
  |> put Variable.tidy n_tidy
  |> put Variable.tidz n_tidz

let rec n_eval (n: Exp.nexp) (ctx:t) : NMap.t =
  match n with
  | Var x ->
    (match Variable.Map.find_opt x ctx.env with
    | Some x -> x
    | None -> failwith ("n_eval: undefined: " ^ Variable.name x))

  | Num n -> NMap.constant ~count:ctx.warp_count ~value:n
  | Bin (o, n1, n2) ->
    let o = Exp.eval_nbin o in
    let n1 = n_eval n1 ctx in
    let n2 = n_eval n2 ctx in
    (try
      NMap.pointwise o n1 n2
    with
      Division_by_zero ->
        failwith ("n_eval: division by zero: " ^ Exp.n_to_string n))
  | NCall (x,_) -> failwith ("n_eval: call " ^ x)
  | NIf (b, n1, n2) ->
    n_map3
      (fun b x1 x2 -> if b then x1 else x2)
      (b_eval b ctx)
      (n_eval n1 ctx)
      (n_eval n2 ctx)

and b_eval (b: Exp.bexp) (ctx:t) : BMap.t =
  match b with
  | Bool b -> BMap.constant ~count:ctx.bank_count ~value:b

  | NRel (o, n1, n2) ->
    let o = Exp.eval_nrel o in
    let n1 = n_eval n1 ctx in
    let n2 = n_eval n2 ctx in
    n_map2 o n1 n2

  | BRel (o, b1, b2) ->
    let o = Exp.eval_brel o in
    let b1 = b_eval b1 ctx in
    let b2 = b_eval b2 ctx in
    BMap.pointwise o b1 b2

  | BNot b ->
    b_eval b ctx |> BMap.map (fun x -> not x)

  | ThreadEqual _ ->
    failwith ("b_eval: thread_equal")
  | Pred (x, _) ->
    failwith ("b_eval: pred " ^ x)

let access ?(verbose=false) (index:Exp.nexp) (ctx:t) : NMap.t =
  let idx_bid =
    let idx = n_eval index ctx |> NMap.to_array in
    (* We handle "gracefully" index out of bounds *)
    let bid = Array.map (fun x -> Common.modulo x ctx.bank_count) idx in
    Array.combine idx bid
  in
  (* Create a mutable array *)
  let tsx = Array.make ctx.bank_count IntSet.empty in
  b_eval ctx.cond ctx
  |> BMap.to_array
  |> Array.combine idx_bid
  |> Array.iter (fun ((idx, bid), enabled) ->
    if enabled then
      let elems = IntSet.add idx (Array.get tsx bid) in
      Array.set tsx bid elems
    else ()
  );
  let tsx = Array.map IntSet.cardinal tsx |> NMap.from_array in
  (if verbose then (
      let msg =
        Exp.n_to_string index ^ " " ^
        "if (" ^ Exp.b_to_string ctx.cond ^ ")"
      in
      print_endline (
        msg ^ " " ^
        "max: " ^ string_of_int ((NMap.max tsx).value) ^ " " ^
        "\n\t" ^ NMap.to_string tsx
      ))
    else ()
  );
  tsx

let add = NMap.pointwise (+)

let eval ?(verbose=true) : Proto.Code.t -> t -> NMap.t =
  let rec eval (cost:NMap.t) (p:Proto.Code.t) (ctx:t) : NMap.t =
    match p with
    | Sync _
    | Skip -> cost
    | Acc (x, {index=[n]; _}) ->
      if ctx.use_array x then
        add cost (access ~verbose n ctx)
      else
        cost
    | Acc _ ->
      failwith ("Unsupported access")
    | Decl (_, p) -> eval cost p ctx
    | Cond (b, p) ->
      restrict b ctx |> eval cost p
    | Loop (r, body) ->
      let has_next = Range.has_next r in
      if b_eval has_next ctx |> BMap.some_true then
        let lo = n_eval r.lower_bound ctx in
        let r = Range.next r in
        (* run one iteration *)
        let cost = eval cost body (
            ctx
            |> restrict has_next
            |> put r.var lo
          )
        in
        (* run the rest of the loop *)
        eval cost (Loop (r, body)) ctx
      else
        cost
    | Seq (p, q) ->
      let cost = eval cost p ctx in
      eval cost q ctx
  in
  fun p ctx ->
    eval (zero_cost ctx) p ctx



