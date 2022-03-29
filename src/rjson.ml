type j_object = (string * Yojson.Basic.t) list
type j_list = Yojson.Basic.t list

(* --- Helpers for the option type --- *)

(* Monadic let *)
let (let*) = Result.bind
(* Monadic pipe *)
let (>>=) = Result.bind

(* Convert an optional boolean into a boolean, where None represents false *)
let unwrap_or (default:'a): ('a, 'e) Result.t -> 'a =
  function
  | Ok v -> v
  | Error _ -> default

let unless (first:('a, 'e) Result.t) (second:('a, 'e) Result.t) : ('a, 'e) Result.t =
  if Result.is_ok first then first else second


(* --- Helpers for Yojson --- *)

let type_name: Yojson.Basic.t -> string =
  function
  | `Assoc _ -> "object"
  | `Bool _ -> "bool"
  | `Float _
  | `Int _ -> "number"
  | `List _ -> "list"
  | `Null -> "null"
  | `String _ -> "string" 

type j_error =
| RootCause of string * Yojson.Basic.t
| Because of string * Yojson.Basic.t * j_error

let pp_js data =
  let result = Yojson.Basic.to_string data in
  let size = 300 in
  let len = String.length result in
  if len > size then
    (String.sub result 0 size) ^ " …"
  else
    result

let rec print_j_error =
  function
  | RootCause (s, j) ->
    prerr_endline s;
    prerr_endline (Yojson.Basic.pretty_to_string j)
  | Because (s, j, e) ->
    prerr_endline (s ^ ": " ^ (pp_js j));
    print_j_error e;

type 'a j_result = ('a, j_error) Result.t

let root_cause (msg:string) (j:Yojson.Basic.t) : 'a j_result =
  Result.Error (RootCause (msg, j))

let type_mismatch ty j =
  root_cause ("type mismatch: expecting " ^ ty ^ ", but got " ^ type_name j) j

let map_all (f:'a -> ('b, 'e) Result.t) (err:int -> 'a -> 'e -> 'e) (l:'a list) : ('b list, 'e) Result.t =
  let rec map (idx:int) (l: 'a list) =
    match l with
    | x :: l ->
      (match f x with
      | Ok x ->
        let* l = map (idx + 1) l in
        Ok (x :: l)
      | Error e -> Error (err idx x e))
    | [] -> Ok []
  in
  map 0 l

let cast_object (j:Yojson.Basic.t) : j_object j_result =
  let open Yojson.Basic.Util in
  match j with
  | `Assoc l -> Ok l
  | _ -> type_mismatch "object" j 

let cast_string (j:Yojson.Basic.t) : string j_result =
  let open Yojson.Basic.Util in
  match j with
  | `String v -> Ok v
  | _ -> type_mismatch "string" j 

let cast_bool (j:Yojson.Basic.t) : bool j_result =
  let open Yojson.Basic.Util in
  match j with
  | `Bool v -> Ok v
  | _ -> type_mismatch "bool" j 

let cast_int (j:Yojson.Basic.t) : int j_result =
  let open Yojson.Basic.Util in
  match j with
  | `Int v -> Ok v
  | _ -> type_mismatch "int" j 

let cast_float (j:Yojson.Basic.t) : float j_result =
  let open Yojson.Basic.Util in
  match j with
  | `Float v -> Ok v
  | _ -> type_mismatch "float" j 

let cast_list (j:Yojson.Basic.t) : Yojson.Basic.t list j_result =
  match j with
  | `List l -> Ok l
  | _ -> type_mismatch "list" j 

let get_field (k:string) (kv: j_object) : Yojson.Basic.t j_result =
  match List.assoc_opt k kv with
  | Some v -> Ok v
  | None ->
    let avail_keys = kv
      |> List.map (fun (k, _) -> "'" ^ k ^ "'")
      |> Common.join ", "
      |> fun k -> "[" ^ k ^ "]"
    in
    root_cause
      ("Field missing '" ^ k ^ "', available keys: " ^ avail_keys)
      (`Assoc kv) 

(* Related to our c-to-json representation *)

let because_get_field (k:string) (kv:j_object) (r: 'a j_result) : 'a j_result = 
  Result.map_error (fun e -> Because ("field: " ^ k, `Assoc kv, e)) r

let with_field (k:string) (f:Yojson.Basic.t -> 'a j_result) (kv:j_object): 'a j_result =
  let* field = get_field k kv in
  f field |> because_get_field k kv

let with_field_or (k:string) (f:Yojson.Basic.t -> 'a j_result) (default:'a) (kv:j_object): 'a j_result =
  match List.assoc_opt k kv with
  | Some field -> f field |> because_get_field k kv
  | None -> Ok default


let get_index (i:int) (l: j_list) : Yojson.Basic.t j_result =
  match List.nth_opt l i with
  | Some v -> Ok v
  | None ->
    let i = string_of_int (i + 1) in
    let len = string_of_int (List.length l) in
    let msg = "Index out of bounds, retriving position #" ^ i ^
     ", but list has length " ^ len
    in
    root_cause msg (`List l)

let because_get_index (i:int) (l:Yojson.Basic.t list): 'a j_result -> 'a j_result = 
  Result.map_error (fun e ->
    Because ("Position #" ^ (string_of_int (i + 1)), `List l, e)
  )

let with_index (index:int) (f:Yojson.Basic.t -> 'a j_result) (l:j_list): 'a j_result =
  let* field = get_index index l in
  f field |> because_get_index index l

let get_kind (o:j_object) : string j_result = with_field "kind" cast_string o

let has_kind (ks:string list) (o:j_object) : bool =
  get_kind o
  (* Check if the kind is in 'ks' *)
  |> Result.map (fun (k:string) -> List.mem k ks)
  (* Convert bool option to bool *)
  |> unwrap_or false

