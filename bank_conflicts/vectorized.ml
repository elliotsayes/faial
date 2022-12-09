open Stage0
open Protocols

let pointwise (f:'a -> 'a -> 'b) (l1:'a list) (l2:'a list) :
  'b list
=
  List.fold_right2 (fun x y accum -> (f x y) :: accum) l1 l2 []


let list_to_string (l:string list) : string =
  "[" ^ Common.join ", " l ^ "]"


module NMap = struct
  type t = int list

  let make (count:int) (f:int -> int) : t  = List.init count f

  let merge : (int -> int -> int) -> t -> t -> t =
    pointwise

  let to_string (l: t) : string =
    l |> List.map string_of_int |> list_to_string

  let max (x:t) : int * int =
    x
    |> List.mapi (fun idx v -> (idx, v))
    |> List.fold_left (fun p1 p2 ->
      if snd p1 > snd p2 then p1 else p2
    ) (0, (List.nth x 0))
end

module BMap = struct
  type t = bool list

  let make (count:int) (f:int -> bool) : t  = List.init count f

  let merge : (bool -> bool -> bool) -> t -> t -> t =
    pointwise

  let to_string (l: t) : string =
    l
    |> List.map (fun x -> if x then "true" else "false")
    |> list_to_string

  let some_true : t -> bool =
    List.exists (fun v -> v)

  let all_false : t -> bool =
    List.for_all (fun v -> not v)

  let get (x: int) (v:t) : bool =
    List.nth v x

  let get_or (idx:int) (b:bool) (x:t) : bool =
    match List.nth_opt x idx with
    | Some b -> b
    | None -> b
end

module IntMap = Map.Make(struct type t = int let compare = compare end)
module IntSet = Set.Make(struct type t = int let compare = compare end)

type t = {
  bank_count: int;
  tid_count: int;
  cond: Exp.bexp;
  env: NMap.t Variable.Map.t;
}

let make ~bank_count ~tid_count : t = {
  cond = Exp.Bool true;
  env = Variable.MapUtil.from_list [
    Variable.from_name "threadIdx.x", NMap.make tid_count (fun tid -> tid);
  ];
  bank_count;
  tid_count;
}

let restrict (b:Exp.bexp) (ctx:t) : t =
  print_endline ("Restrict: " ^ Serialize.PPrint.b_to_s b);
  let open Exp in
  { ctx with cond = b_and ctx.cond b }

let put (x:Variable.t) (v:NMap.t) (ctx:t) : t =
  { ctx with env = Variable.Map.add x v ctx.env }

let zero (ctx:t) : NMap.t =
  NMap.make ctx.bank_count (fun _ -> 0)

let rec n_eval (n: Exp.nexp) (ctx:t) : NMap.t =
  match n with
  | Var x ->
    (match Variable.Map.find_opt x ctx.env with
    | Some x -> x
    | None -> failwith ("n_eval: undefined: " ^ Variable.name x))

  | Num n -> NMap.make ctx.tid_count (fun _ -> n)
  | Bin (o, n1, n2) ->
    let o = Exp.eval_nbin o in
    let n1 = n_eval n1 ctx in
    let n2 = n_eval n2 ctx in
    pointwise o n1 n2
  | Proj _ -> failwith ("n_eval: proj")
  | NCall (x,_) -> failwith ("n_eval: call " ^ x)
  | NIf (b, n1, n2) ->
    let b = b_eval b ctx in
    let n1 = n_eval n1 ctx in
    let n2 = n_eval n2 ctx in
    Common.zip n1 n2
    |> List.map2 (fun b (x1, x2) -> if b then x1 else x2) b

and b_eval (b: Exp.bexp) (ctx:t) : BMap.t =
  match b with
  | Bool b -> BMap.make ctx.tid_count (fun _ -> b)
  | NRel (o, n1, n2) ->
    let o = Exp.eval_nrel o in
    let n1 = n_eval n1 ctx in
    let n2 = n_eval n2 ctx in
    pointwise o n1 n2
  | BRel (o, b1, b2) ->
    let o = Exp.eval_brel o in
    let b1 = b_eval b1 ctx in
    let b2 = b_eval b2 ctx in
    pointwise o b1 b2
  | BNot b ->
    b_eval b ctx
    |> List.map (fun x -> not x)
  | Pred (x, _) ->
    failwith ("b_eval: pred " ^ x)

let access (index:Exp.nexp) (ctx:t) =
  let msg =
    "[" ^ Serialize.PPrint.n_to_s index ^ "] " ^
    "if (" ^ Serialize.PPrint.b_to_s ctx.cond ^ ")"
  in
  let index = n_eval index ctx in
  let index = List.mapi (fun tid idx -> (tid, idx)) index in
  let cond = b_eval ctx.cond ctx in
  let in_cond (x:int) : bool =
    BMap.get_or x false cond
  in
  let bank (bid:int) : int =
    List.fold_left
      (fun accum (tid, idx) ->
        if Common.modulo idx ctx.bank_count = bid && in_cond tid
        then IntSet.add idx accum
        else accum)
      IntSet.empty
      index
    |> IntSet.cardinal
  in
  let tsx = List.init ctx.bank_count bank in
  print_endline (
    msg ^ " " ^
    "max: " ^ string_of_int (snd (NMap.max tsx)) ^ " " ^
    "\n\t" ^ NMap.to_string tsx
  );
  tsx

let add = pointwise (+)

let rec eval (p: Proto.prog) (ctx:t) : NMap.t =
  List.fold_left (fun cost (i:Proto.inst) ->
    match i with
    | Acc (_, {access_index=[n]; _}) ->
      add cost (access n ctx)
    | Acc _ -> failwith ("Unsupported access")
    | Sync -> zero ctx
    | Cond (b, p) ->
      restrict b ctx
      |> eval p
    | Loop (r, body) ->
      let has_next = Exp.range_has_next r in
      if b_eval has_next ctx |> BMap.some_true then
        let lo = n_eval r.range_lower_bound ctx in
        let cost =
          eval body (
            ctx
            |> restrict has_next
            |> put r.range_var lo
          )
          |> add cost
        in
        eval [Loop (Predicates.range_inc r, body)] ctx
        |> add cost
      else
        zero ctx
  ) (zero ctx) p



