module StringOT = struct
  type t = string
  let compare = Stdlib.compare
end

module StringSet = Set.Make(StringOT)
module StringMap = Map.Make(StringOT)

let list_to_string_map (l:(string * 'a) list) : 'a StringMap.t =
  List.fold_left (fun m (k,v) ->
    StringMap.add k v m
  ) StringMap.empty l

let append_tr (l1:'a list) (l2:'a list) : 'a list =
  let rec app (ret:'a list -> 'a list) (l:'a list) : 'a list =
    match l with
    | [] -> ret l2
    | x:: l -> app (fun new_l -> ret (x :: new_l)) l
  in
    app (fun x -> x) l1

let rec append_rev (l1:'a list) (l2:'a list): 'a list =
  match l1 with
  | [] -> l2
  | x :: l1 -> append_rev l1 (x::l2)

let rec repeat (s:string) n : string =
  if n <= 0 then ""
  else s ^ repeat s (n - 1)

let join sep elems =
  let on_elem accum x =
    if String.equal accum ""
    then x
    else x ^ sep ^ accum
  in
  List.fold_left on_elem "" elems

let rsplit (c:char) (s:string) :  (string * string) option =
  match String.rindex_opt s c with
  | Some idx -> Some (String.sub s 0 idx, String.sub s (idx + 1) (String.length s - idx - 1))
  | None -> None

let split (c:char) (s:string) :  (string * string) option =
  match String.index_opt s c with
  | Some idx -> Some (String.sub s 0 idx, String.sub s (idx + 1) (String.length s - idx - 1))
  | None -> None


let list_is_empty (l:'a list) =
  match l with
  | [] -> true
  | _ -> false

let enumerate l =
  let rec iter idx l =
    match l with
    | h::t -> (idx,h) :: iter (idx + 1) t
    | _ -> []
  in
  iter 0 l

let opt_to_list = function
  | Some x -> [x]
  | None -> []

let flatten_opt l =
  List.map opt_to_list l |> List.flatten

let either_split (l: ('a, 'b) Either.t list) : 'a list * 'b list =
  List.partition_map (fun a -> a) l

let map_opt f l =
  List.map f l |> flatten_opt

let starts_with txt prefix =
  let txt_len = String.length txt in
  let pre_len = String.length prefix in
  if txt_len < pre_len then false
  else
  String.equal (String.sub txt 0 pre_len) prefix

let ends_with suffix s =
  let suffix_len = String.length suffix in
  let s_len = String.length s in
  (suffix_len = 0) ||
  (s_len >= suffix_len && String.sub s (s_len - suffix_len) suffix_len = suffix)

let hashtbl_elements t =
  Hashtbl.fold (fun k v accum ->
    (k,v)::accum
  ) t []

let hashtbl_update ht kvs =
  List.iter (fun (k,v) -> Hashtbl.add ht k v) kvs

let hashtbl_from_list kvs =
  let ht = Hashtbl.create (List.length kvs) in
  hashtbl_update ht kvs;
  ht

let rec zip l1 l2 =
  match l1, l2 with
  | [], _ | _, [] -> []
  | x::l1, y::l2 -> (x,y) :: (zip l1 l2)

let range (i:int) (j:int) : int list =
  let rec iter n acc =
    if n < i then acc else iter (n-1) (n :: acc)
  in
  iter j []

(* https://stackoverflow.com/a/46759007/2327050 *)
let modulo x y =
  let result = x mod y in
  if result >= 0 then result
  else result + y

exception ParseError of Buffer.t

let string_to_buffer (s:string) =
  let b = Buffer.create (String.length s) in
  Buffer.add_string b s;
  b

let buffer_prepend b s =
  let result = Buffer.create (Buffer.length b + String.length s) in
  Buffer.add_string result s;
  Buffer.add_buffer result b;
  result

let mk_parse_error_s s = ParseError (string_to_buffer s)

let mk_parse_error_l ss =
  let b = Buffer.create (16 * List.length ss) in
  List.iter (fun x ->
    Buffer.add_string b x;
    Buffer.add_char b '\n'
  ) ss;
  b


module StackTrace = struct

  type 'a stack_trace =
  | RootCause of 'a
  | Because of 'a * 'a stack_trace
  type 'a t = 'a  stack_trace

  let rec fold_because (accum:'a -> 'b -> 'a) (init:'a) (s:'b t) : 'a =
    match s with
    | RootCause x ->
      accum init x
    | Because (x, s) ->
      fold_because accum (accum init x) s

  let rec fold_root_cause (accum:'a -> 'b -> 'a) (s:'a t) (init:'b) : 'b =
    match s with
    | RootCause x ->
      accum x init
    | Because (x, s) ->
      accum x (fold_root_cause accum s init)


  let iter (f:'a -> unit) : 'a t -> unit =
    fold_because (fun _ x -> f x) ()

  let rev_iter (f:'a -> unit) (s: 'a t) : unit =
    fold_root_cause (fun x _ -> f x) s ()

  let count (s:'a t) : int =
    fold_because (fun count _ -> 1 + count) 0 s

  let iteri (f:int -> 'a -> unit) (s: 'a t) : unit =
    let _ = fold_because (fun c x -> f c x; c - 1) (count s - 1) s in
    ()

  let rev_iteri (f:int -> 'a -> unit) (s: 'a t) : unit =
    let _ = fold_root_cause (fun x c -> f c x; 1 + c) s 0 in
    ()

  let to_list (s: 'a t) : 'a list =
    fold_because (fun l x -> x::l) [] s

  let rec map (f:'a -> 'b) : 'a t -> 'b t =
    function
    | RootCause x -> RootCause (f x)
    | Because (x, e) -> Because(f x, map f e)

end

(*
  Takes an ok_handler and an error_handler.
  Applies ok_handler to v and if there's an error, route it to
  the error_handler.
  *)
let wrap
  (ok_handler:'a -> ('b,'e) Result.t )
  (error_handler:'e -> ('b,'e) Result.t)
  (v:'a)
: ('b,'e) Result.t =
  match ok_handler v with
  | Ok bv -> Ok bv
  | Error (e:'e) -> error_handler e

(* Convert an optional boolean into a boolean, where None represents false *)
let unwrap_or (default:'a): ('a, 'e) Result.t -> 'a =
  function
  | Ok v -> v
  | Error _ -> default