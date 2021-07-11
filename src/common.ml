module StringOT = struct
  type t = string
  let compare = Stdlib.compare
end

module StringSet = Set.Make(StringOT)

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

let parse_dim (x:string) : int list =
  (*
    let ex3 = "[8][8]" in
    assert (parse_dim ex3 = Some [8; 8]);
  *)
  String.split_on_char '[' x
  |> List.concat_map (fun (x:string) ->
      if String.length x = 0 then []
      else [String.sub x 0 (String.length x - 1)]
    )
  |>
  List.map int_of_string

let parse_array_type_opt (x:string) : string list option =
  match rsplit ' ' x with
  | Some (x, "*") ->
    let x = begin match split '[' x with
    | Some (x, _) -> x
    | None -> x
    end in
    Some (String.split_on_char ' ' x
      |> List.filter (fun x -> String.length x > 0)
    )
  | _ -> None

let parse_array_dim_opt (x:string) : int list option =
  match rsplit ' ' x with
  | Some (x, "*") ->
    begin match rsplit ' ' x with
    | Some (_, x) ->
      (try Some (parse_dim x) with
        Failure _ -> None)
    | _ -> None
    end
  | _ -> None



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

let map_opt f l =
  List.map f l |> flatten_opt

let starts_with txt prefix =
  let txt_len = String.length txt in
  let pre_len = String.length prefix in
  if txt_len < pre_len then false
  else
  String.equal (String.sub txt 0 pre_len) prefix

let ends_with s suffix =
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
