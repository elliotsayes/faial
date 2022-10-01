module StringOT = struct
  type t = string
  let compare = Stdlib.compare
end

module MapUtil (M:Map.S) = struct
  let from_list (l:(M.key * 'a) list) : 'a M.t =
    List.fold_left (fun m (k,v) ->
      M.add k v m
    ) M.empty l
end

module MapSetUtil (S:Set.S) (M:Map.S with type key = S.elt) = struct
  let set_to_map (s:S.t) (f:S.elt -> 'a option) : 'a M.t =
    S.fold (fun k m ->
      match f k with
      | Some v -> M.add k v m
      | None -> m
    ) s M.empty

  let map_to_set (m:'a M.t) : S.t =
    M.bindings m |> List.map fst |> S.of_list

end

module StringSet = Set.Make(StringOT)
module StringMap = Map.Make(StringOT)
module StringMapUtil = MapUtil(StringMap)
module StringMapSetUtil = MapSetUtil(StringSet)(StringMap)

let append_tr (l1:'a list) (l2:'a list) : 'a list =
  let rec app (ret:'a list -> 'a list) (l:'a list) : 'a list =
    match l with
    | [] -> ret l2
    | x:: l -> app (fun new_l -> ret (x :: new_l)) l
  in
    app (fun x -> x) l1

(* Concatenates a reversed version of lhs to rhs *)
let rec append_rev1 (l1:'a list) (l2:'a list): 'a list =
  match l1 with
  | [] -> l2
  | x :: l1 -> append_rev1 l1 (x::l2)

(* Repeats string s n times *)
let rec repeat (s:string) (n:int) : string =
  if n <= 0 then ""
  else s ^ repeat s (n - 1)

let join (sep:string) (elems:string list) : string =
  let on_elem accum x =
    if String.equal accum ""
    then x
    else x ^ sep ^ accum
  in
  List.fold_left on_elem "" (List.rev elems)

let rsplit (c:char) (s:string) :  (string * string) option =
  match String.rindex_opt s c with
  | Some idx -> Some (String.sub s 0 idx, String.sub s (idx + 1) (String.length s - idx - 1))
  | None -> None

let split (c:char) (s:string) :  (string * string) option =
  match String.index_opt s c with
  | Some idx -> Some (String.sub s 0 idx, String.sub s (idx + 1) (String.length s - idx - 1))
  | None -> None


let list_is_empty (l:'a list) : bool =
  match l with
  | [] -> true
  | _ -> false

let enumerate (l:'a list) : (int * 'a) list =
  let rec iter idx l =
    match l with
    | h::t -> (idx,h) :: iter (idx + 1) t
    | _ -> []
  in
  iter 0 l

let flatten_opt : 'a option list -> 'a list =
  fun (l:'a option list) -> List.concat_map Option.to_list l

let either_split (l: ('a, 'b) Either.t list) : 'a list * 'b list =
  List.partition_map (fun a -> a) l

let contains ~needle:(needle:string) (s:string) : bool =
  let n_len = String.length needle in
  let s_len = String.length s in
  if n_len = 0 then true
  else if n_len > s_len then false 
  else (
    (* Has at least one character *)
    let ch = needle.[0] in
    (* Find the every character that starts with `ch`
       extract a substring with as many chars than needle.
       Return true if neele matches substring. *)
    let rec contains (offset:int) : bool =
      match String.index_from_opt s offset ch with
      | Some idx ->
        if s_len - idx >= n_len then (
          (* Check if we have enough characters to search *)
          let sub = String.sub s idx n_len in
          (* Substring matches needle, we are done *)
          if String.equal sub needle then true
          (* Try again *)
          else contains (idx + 1)
        (* Otherwise, not enough characters, impossible
           to find neelde. *)
        ) else false
      | None -> false
    in
    contains 0 
  )

let hashtbl_elements (t: ('a, 'b) Hashtbl.t) : ('a * 'b) list =
  Hashtbl.fold (fun k v accum ->
    (k,v)::accum
  ) t []

let hashtbl_update (ht: ('a, 'b) Hashtbl.t) (kvs:('a * 'b) list) : unit =
  List.iter (fun (k,v) -> Hashtbl.add ht k v) kvs

let hashtbl_from_list (kvs: ('a * 'b) list) : ('a, 'b) Hashtbl.t =
  let ht = Hashtbl.create (List.length kvs) in
  hashtbl_update ht kvs;
  ht

let rec zip (l1:'a list) (l2:'b list) : ('a * 'b) list =
  match l1, l2 with
  | [], _ | _, [] -> []
  | x::l1, y::l2 -> (x,y) :: (zip l1 l2)

let range ?(from=0) (until:int) : int list =
  let rec iter (curr:int) (acc:int list) : int list =
    if curr < from then acc else iter (curr - 1) (curr :: acc)
  in
  iter until []

(* https://stackoverflow.com/a/46759007/2327050 *)
let modulo (x:int) (y:int) : int =
  let result = x mod y in
  if result >= 0 then result
  else result + y

exception ParseError of Buffer.t

let string_to_buffer (s:string) =
  let b = Buffer.create (String.length s) in
  Buffer.add_string b s;
  b

let buffer_prepend (b:Buffer.t) (s:string) : Buffer.t =
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

(* Read a range of lines: offset has base 0 *)
let get_lines ~offset ~count (filename:string) : string list =
  (* Skip the first n-lines *)
  let rec skip_n ic count =
    if count <= 0 then ()
    else begin
      let _ = input_line ic in
      skip_n ic (count - 1)
    end
  in
  (* Return the first n-lines *)
  let yield_n ic count =
    List.init count (fun _ -> input_line ic)
  in
  let ic = open_in filename in
  skip_n ic offset;
  let lines = yield_n ic count in
  close_in ic;
  lines

(* Read a single line *)
let get_line offset filename =
  match get_lines filename ~offset ~count:1 with
  | [l] -> l
  | _ -> failwith "Unexpected output"
