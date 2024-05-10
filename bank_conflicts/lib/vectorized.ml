open Stage0
open Protocols
module IntSet = Common.IntSet

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
  val random : int -> unit -> t
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

  let random (count:int) () =
    make count (fun _ -> Random.nativebits () |> Nativeint.to_int)

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

type t = {
  bank_count: int;
  warp_count: int;
  cond: Exp.bexp;
  env: NMap.t Variable.Map.t;
  use_array: Variable.t -> bool
}

let to_string (ctx:t) : string =
  let env =
    ctx.env
    |> Variable.Map.bindings
    |> List.map (fun (x, y) -> "  " ^ Variable.name x ^ "=" ^ NMap.to_string y)
    |> String.concat "\n"
  in
  "env:\n" ^ env

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

let get (x:Variable.t) (ctx:t) : NMap.t option =
  Variable.Map.find_opt x ctx.env

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
  |> put Variable.tid_x n_tidx
  |> put Variable.tid_y n_tidy
  |> put Variable.tid_z n_tidz

let tid_opt (ctx:t) : Dim3.t array option =
  let ( let* ) = Option.bind in
  let* tid_x = get Variable.tid_x ctx |> Option.map NMap.to_array in
  let* tid_y = get Variable.tid_y ctx |> Option.map NMap.to_array in
  let* tid_z = get Variable.tid_z ctx |> Option.map NMap.to_array in
  let tid_xy = Array.combine tid_x tid_y in
  let tid_xyz = Array.combine tid_xy tid_z in
  Some (
    tid_xyz
    |> Array.map (fun ((x, y), z) ->
      Dim3.{x; y; z}
    )
  )

let tid (ctx:t) : Dim3.t array =
  tid_opt ctx |> Option.get

let from_config
  (params:Config.t)
:
  t
=
  make
  ~bank_count:params.num_banks
  ~warp_count:params.warp_count
  ~use_array:(fun _ -> true)
  |> put_tids params.block_dim

let ( let* ) = Result.bind

let rec n_eval_res (n: Exp.nexp) (ctx:t) : (NMap.t, string) Result.t =
  match n with
  | Var x ->
    (match Variable.Map.find_opt x ctx.env with
    | Some x -> Ok x
    | None -> Error ("undefined variable: " ^ Variable.name x))

  | Num n -> Ok (NMap.constant ~count:ctx.warp_count ~value:n)

  | CastInt (CastBool n) ->
    n_eval_res n ctx

  | CastInt e ->
    let* e = b_eval_res e ctx in
    Ok (
      e
      |> BMap.to_array
      |> Array.map (fun v -> if v then 1 else 0)
      |> NMap.from_array
    )

  | Unary (o, e) ->
    let* n = n_eval_res e ctx in
    Ok (NMap.map (N_unary.eval o) n)

  | Binary (o, n1, n2) ->
    let o = N_binary.eval o in
    let* n1 = n_eval_res n1 ctx in
    let* n2 = n_eval_res n2 ctx in
    (try
      Ok (NMap.pointwise o n1 n2)
    with
      Division_by_zero ->
        Error ("division by zero: " ^ Exp.n_to_string n))

  | NIf (b, n1, n2) ->
    let* b = b_eval_res b ctx in
    let* n1 = n_eval_res n1 ctx in
    let* n2 = n_eval_res n2 ctx in
    Ok (
      n_map3
        (fun b x1 x2 -> if b then x1 else x2)
        b
        n1
        n2
    )

  | NCall (x,_) -> Error ("unknown function call: " ^ x)

  | Other _ -> Error ("cannot evaluate other: " ^ Exp.n_to_string n)

and b_eval_res (b: Exp.bexp) (ctx:t) : (BMap.t, string) Result.t =
  match b with
  | Bool b -> Ok (BMap.constant ~count:ctx.bank_count ~value:b)

  | CastBool (CastInt e) ->
    b_eval_res e ctx

  | CastBool e ->
    let* e = n_eval_res e ctx in
    Ok (
      e
      |> NMap.to_array
      |> Array.map (fun v -> v <> 0)
      |> BMap.from_array
    )

  | NRel (o, n1, n2) ->
    let o = N_rel.eval o in
    let* n1 = n_eval_res n1 ctx in
    let* n2 = n_eval_res n2 ctx in
    Ok (n_map2 o n1 n2)

  | BRel (o, b1, b2) ->
    let o = B_rel.eval o in
    let* b1 = b_eval_res b1 ctx in
    let* b2 = b_eval_res b2 ctx in
    Ok (BMap.pointwise o b1 b2)

  | BNot b ->
    let* b = b_eval_res b ctx in
    Ok (BMap.map (fun x -> not x) b)

  | Pred (x, _) ->
    Error ("cannot evaluate predicate: " ^ x)

let n_eval (e:Exp.nexp) (ctx:t) : NMap.t =
  n_eval_res e ctx |> Result.get_ok

let b_eval (e:Exp.bexp) (ctx:t) : BMap.t =
  b_eval_res e ctx |> Result.get_ok

module Warp = struct
  module Task = struct
    type t = {
      id: Dim3.t;
      index: int;
    }
    let to_string (e:t) : string =
      "[" ^ string_of_int e.index ^ "]:" ^ Dim3.to_string e.id
  end

  module Transaction = struct
    type t = {
      bank: int;
      accesses: Task.t list;
      indices: IntSet.t;
      count: int;
    }

    let to_string (e:t) : string =
      let accs =
        e.accesses
        |> List.map Task.to_string
        |> String.concat ", "
      in
      let bc = IntSet.cardinal e.indices |> string_of_int in
      let bank = string_of_int e.bank in
      bc ^ " @ " ^ bank ^ " " ^ accs

    let make (bank:int) : t =
      {bank; accesses=[]; indices=IntSet.empty; count=0}

    let zero : t =
      make 0

    let count (e:t) : int =
      IntSet.cardinal e.indices

    let max (lhs:t) (rhs:t) : t =
      if lhs.count >= rhs.count then
        lhs
      else
        rhs

    let add (task:Task.t) (tsx:t) : t =
      let indices = IntSet.add task.index tsx.indices in
      { tsx with
        accesses = task :: tsx.accesses;
        indices;
        count = IntSet.cardinal indices;
      }
  end

  type t = Transaction.t array

  let max : t -> Transaction.t =
    Array.fold_left Transaction.max Transaction.zero

  let transaction_count (w:t) : int =
    max w |> Transaction.count

  let to_string (w:t) : string =
    max w |> Transaction.to_string

  let make
    (bank_count:int)
    (indices:int array)
    (enabled:bool array)
    (tids:Dim3.t array)
  : t =
    let banks = Array.init bank_count Transaction.make in
    enabled
    |> Array.combine indices
    |> Array.iteri (fun id (index, enabled) ->
      if enabled then
        let bid = Common.modulo index bank_count in
        let id = Array.get tids id in
        let tsx = Transaction.add Task.{id; index} (Array.get banks bid) in
        Array.set banks bid tsx
      else ()
    );
    banks

end

let to_warp
  (index:Exp.nexp)
  (ctx:t)
:
  (Warp.t, string) Result.t
=
  let* idx = n_eval_res index ctx in
  let* enabled = b_eval_res ctx.cond ctx in
  let idx = NMap.to_array idx in
  let enabled = BMap.to_array enabled in
  let is_valid : bool =
    Array.combine idx enabled
    |> Array.for_all (fun (idx, enabled) -> not enabled || idx >= 0)
  in
  if is_valid then
    let tids = tid ctx in
    Ok (Warp.make ctx.bank_count idx enabled tids)
  else
    Error "index out of bounds"

let transactions_res
  (index:Exp.nexp)
  (ctx:t)
:
  (IntSet.t array, string) Result.t
=
  let* idx = n_eval_res index ctx in
  let idx_bid =
    let idx = idx |> NMap.to_array in
    (* We handle "gracefully" index out of bounds *)
    let bid = Array.map (fun x -> Common.modulo x ctx.bank_count) idx in
    Array.combine idx bid
  in
  (* Create a mutable array *)
  let tsx = Array.make ctx.bank_count IntSet.empty in
  let* b = b_eval_res ctx.cond ctx in
  b
  |> BMap.to_array
  |> Array.combine idx_bid
  |> Array.iter (fun ((idx, bid), enabled) ->
    if enabled then
      let elems = IntSet.add idx (Array.get tsx bid) in
      Array.set tsx bid elems
    else ()
  );
  Ok tsx

let max_transactions_res ?(verbose=false) (index:Exp.nexp) (ctx:t) : (NMap.t, string) Result.t =
  let* tsx = transactions_res index ctx in
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
  Ok tsx

let max_transactions ?(verbose=false) (index:Exp.nexp) (ctx:t) : NMap.t =
  max_transactions_res ~verbose index ctx |> Result.get_ok

let add = NMap.pointwise (+)

type loop =
  | Next of Range.t * t
  | End

let iter_res (r:Range.t) (ctx:t) : (loop, string) Result.t =
  let has_next = Range.has_next r in
  let* b = b_eval_res has_next ctx in
  if BMap.some_true b then
    let* lo = n_eval_res r.lower_bound ctx in
    let r = Range.next r in
    (* run one iteration *)
    Ok (Next (r,
      ctx
      |> restrict has_next
      |> put r.var lo
    ))
  else
    Ok End

let iter (r:Range.t) (ctx:t) : loop =
  iter_res r ctx |> Result.get_ok

let eval ?(verbose=true) : Proto.Code.t -> t -> NMap.t =
  let rec eval (cost:NMap.t) (p:Proto.Code.t) (ctx:t) : NMap.t =
    match p with
    | Sync _
    | Skip -> cost
    | Acc (x, {index=[n]; _}) ->
      if ctx.use_array x then
        add cost (max_transactions ~verbose n ctx)
      else
        cost
    | Acc _ ->
      failwith ("Unsupported access")
    | Decl {body=p; _} -> eval cost p ctx
    | If (b, p, q) ->
      let cost = restrict b ctx |> eval cost p in
      restrict (Exp.b_not b) ctx |> eval cost q
    | Loop (r, body) ->
      (match iter r ctx with
      | Next (r, ctx') ->
        let cost = eval cost body ctx' in
        (* run the rest of the loop *)
        eval cost (Loop (r, body)) ctx
      | End ->
        (* Loop is done *)
        cost
      )
    | Seq (p, q) ->
      let cost = eval cost p ctx in
      eval cost q ctx
  in
  fun p ctx ->
    eval (zero_cost ctx) p ctx



