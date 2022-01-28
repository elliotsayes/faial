open Exp
open Imp
open Common

let pp_js data =
  let result = Yojson.Basic.to_string data in
  let size = 300 in
  let len = String.length result in
  if len > size then
    (String.sub result 0 size) ^ " …"
  else
    result

let parse_error (b:Buffer.t) msg data =
  Buffer.add_string b ("Error parsing '" ^ msg ^"': " ^ pp_js data ^ "\n");
  raise (Common.ParseError b)

let abort_error msg data =
  raise (msg ^ "\n" ^ pp_js data ^ "\n"|> Common.mk_parse_error_s)

let call msg f data =
  let o = (try f data with Common.ParseError b -> parse_error b msg data) in
  match o with
  | Some m -> m
  | None ->  parse_error (Buffer.create 0) msg data

let is_some o =
  match o with
  | Some _ -> true
  | None -> false

type 'a builder = Yojson.Basic.t -> 'a option

type 'a parser = {is_valid: Yojson.Basic.t -> bool; run: Yojson.Basic.t -> 'a}

let make name f = {
  is_valid = (fun x -> is_some (f x));
  run = call name f;
}

let parse_nbin = make "nbin" (fun m ->
  let open Yojson.Basic in
  match m with
  | `String "+" -> Some Plus
  | `String "-" -> Some Minus
  | `String "*"  -> Some Mult
  | `String "/" -> Some Div
  | `String "%" -> Some Mod
  | `String ">>" -> Some RightShift
  | `String "<<" -> Some LeftShift
  | `String "^" -> Some BitXOr
  | `String "|" -> Some BitOr
  | `String "&" -> Some BitAnd
  | `String x ->
    prerr_endline ("WARNING: Can't handle '(int, int) int' operator '"^ x ^"' converting it to +");
    Some Plus
  | _ -> None
)

let bind o1 f =
  match o1 with
  | Some x -> f x
  | None -> None

(* Monadic let *)
let (let*) = bind
(* Monadic pipe *)
let (>>=) = bind

let bind_all l f =
  let rec aux l accum =
    match l with
    | [] -> f (List.rev accum)
    | x::l -> bind x (fun x -> aux l (x::accum))
  in
  aux l []

type j_object = (string * Yojson.Basic.t) list

module Ojson = struct
  let (let*) = Option.bind
  let (>>=) = Option.bind

  let get_object (j:Yojson.Basic.t) : j_object option =
    let open Yojson.Basic.Util in
    match j with
    | `Assoc l -> Some l
    | _ -> None

  let get_string (j:Yojson.Basic.t) : string option =
    let open Yojson.Basic.Util in
    match j with
    | `String v -> Some v
    | _ -> None

  let get_field (k:string) (kv: j_object) : Yojson.Basic.t option =
    List.assoc_opt k kv

  let get_list (j:Yojson.Basic.t) : Yojson.Basic.t list option =
    match j with
    | `List l -> Some l
    | _ -> None

  let get_nth (i:int) (l: 'a list) : 'a option =
    List.nth_opt l i

  let get_kind (o:j_object) : string option =
    get_field "kind" o >>= get_string

  let to_bool (o:bool option) =
    match o with
    | Some b -> b
    | None -> false

  let to_list (o:'a list option) : 'a list =
    match o with
    | Some l -> l
    | None -> []

  let has_kind (ks:string list) (o:j_object) : bool =
    get_kind o
    (* Check if the kind is in 'ks' *)
    |> Option.map (fun (k:string) -> List.mem k ks)
    (* Convert bool option to bool *)
    |> to_bool
end



let member_opt k (j:Yojson.Basic.t) : Yojson.Basic.t option =
  let open Yojson.Basic.Util in
  match j with
  | `Assoc _ -> Some (member k j)
  | _ -> None


let get_kind_res (j:Yojson.Basic.t) : (string, string) Result.t =
  let open Yojson.Basic.Util in
  match j with
  | `Assoc _ ->
    begin match member "kind" j with
    | `String k -> Ok k
    | _ -> Result.Error "expecting JSON object's field 'kind' to be a string"
    end
  | _ -> Result.Error "expecting a JSON object"


let get_kind (j:Yojson.Basic.t) : string =
  match get_kind_res j with
  | Result.Ok k -> k
  | Result.Error msg -> abort_error ("get_kind: " ^ msg ^ " but got: ") j

let get_fields fields (j:Yojson.Basic.t) : Yojson.Basic.t list  =
  let open Yojson.Basic.Util in
  let kv = List.map (fun x -> let open Ojson in (x, get_object j >>= get_field x)) fields in
  let missing = List.filter (fun (x,y) -> y = None) kv |> List.split |> fst in
  if List.length missing > 0 then
    let fields = join ", " fields in
    let missing = join ", " missing in
    abort_error ("Getting fields [" ^ fields ^ "], but object is missing fields [" ^ missing ^ "]") j
  else
    List.split kv |> snd |> flatten_opt

type 'a choose_one_of_handler = string list * (Yojson.Basic.t list -> 'a option)

let b_either
  (f1: 'a builder)
  (f2: 'a builder)
  : 'a builder
=
  fun j ->
  match f1 j with
  | Some r -> Some r
  | None -> f2 j

let b_map
  (f: 'a -> 'b)
  (a: 'a builder)
  :
  'b builder
=
  fun j ->
  match a j with
  | Some o -> Some (f o)
  | None -> None

let choose_one_of (l:(string * 'a choose_one_of_handler) list)
  : 'a builder
  =
  fun j ->
  match get_kind_res j with
  | Result.Ok k ->
    List.find_map (fun (k', ((fields:string list), kont)) ->
      if k = k' then
        get_fields fields j |> kont
      else None
    ) l
  | _ -> None

let is_shared o : bool =
  let open Yojson.Basic in
  let open Yojson.Basic.Util in
  match member "shared" o with
  | `Bool true -> true
  | _ -> false

module Ctype = struct
  (* Type-safe representation of a CType *)
  type t = CType: string -> t

  let make (ty:string) : t =
    CType ty

  let to_string (c:t) : string =
    match c with
    | CType x -> x

  let get_array_length (c:t) : int list =
    to_string c
    |> parse_array_dim_opt
    |> Ojson.to_list

  let get_array_type (c:t) : string list =
    to_string c
    |> parse_array_type_opt
    |> Ojson.to_list
end

let get_type (o:j_object) : Ctype.t option =
  let open Ojson in
  get_field "type" o
  >>= get_object
  >>= get_field "qualType"
  >>= get_string
  |> Option.map Ctype.make

(* XXX: what about int [128] *)
let is_array_type o : bool =
  let open Yojson.Basic in
  let open Yojson.Basic.Util in
  match member "qualType" o with
  | `String x -> ends_with x "*"
  | _ -> false

let is_int_type o =
  let open Yojson.Basic in
  let open Yojson.Basic.Util in
  match member "qualType" o with
  | `String "int"
  | `String "const int"
  | `String "unsigned int"
  | `String "const unsigned int"
  | `String "short"
  | `String "const short"
  | `String "uint"
  | `String "const uint"
   -> true
  | _ -> false

let has_type j ty =
  let open Yojson.Basic in
  let open Yojson.Basic.Util in
  match j with
  | `Assoc _ -> ty (member "type" j)
  | _ -> false

let parse_task = function
  | 0 -> Some Task1
  | 1 -> Some Task2
  | _ -> None

let binary_operator (f:Yojson.Basic.t -> Yojson.Basic.t -> Yojson.Basic.t -> 'a option) : string * 'a choose_one_of_handler =
  "BinaryOperator", (["opcode"; "lhs"; "rhs"], function
    | [o; n1; n2] -> f o n1 n2
    | _ -> None
  )

let get_fields fields f j =
  match j with
  | `Assoc j ->
    Common.map_opt (fun field -> List.assoc_opt field j) fields
    |> f
  | _ -> None

let has_type_choice types j =
  let open Yojson.Basic in
  let open Yojson.Basic.Util in
  match j with
  | `Assoc _ ->
    types
    |> List.map (fun x -> `String x)
    |> List.mem (member "kind" j)
  | _ -> false

let rec build_position : Sourceloc.position builder =
  fun j ->
    let open Sourceloc in
    let open Yojson.Basic in
    let open Yojson.Basic.Util in
    j |> get_fields ["line"; "col"; "file"] (function
    | [`Int line; `Int col] -> Some {
        pos_line = line;
        pos_column = col;
        pos_filename = "";
      }
    | [`Int line; `Int col; `String filename] -> Some {
        pos_line = line;
        pos_column = col;
        pos_filename = filename
      }
    | _ ->
      get_fields ["expansionLoc"] (function
      | [j] -> build_position j
      | _ -> None
      ) j
    )


let parse_position : Sourceloc.position parser =
  make "position" build_position

let build_loc : Sourceloc.location builder =
  fun j ->
    let open Sourceloc in
    let open Yojson.Basic in
    let open Yojson.Basic.Util in
    j |> get_fields ["begin"; "end"] (function
    | [s; e] ->
      Some {
        loc_start = parse_position.run s;
        loc_end = parse_position.run e;
      }
    | _ -> None
  )

let parse_loc : Sourceloc.location parser = make "location" build_loc

let variable_kind : string list = [
    "NonTypeTemplateParmDecl";
    "FunctionDecl";
    "VarDecl";
    "ParmVarDecl";
  ]


let build_var : variable builder =
  fun j ->
    if has_type_choice variable_kind j then (
      j
      |> get_fields ["name"; "range"] (function
      | [`String name; l] -> Some (LocVariable (parse_loc.run l, name))
      | [`String name] -> Some (Variable name)
      | _ -> None
      )
    ) else None

let parse_var : variable parser = make "variable" build_var

let parse_nrel_opt : nrel builder =
  fun j ->
  let open Yojson.Basic in
  match j with
  | `String "==" -> Some NEq
  | `String "!=" -> Some NNeq
  | `String "<=" -> Some NLe
  | `String "<"  -> Some NLt
  | `String ">=" -> Some NGe
  | `String ">"  -> Some NGt
  | _ -> None

let parse_nrel : nrel parser = make "nrel" parse_nrel_opt

let parse_brel : brel parser = make "brel" (fun m ->
  let open Yojson.Basic in
  match m with
  | `String "||" -> Some BOr
  | `String "&&" -> Some BAnd
  | `String x ->
    prerr_endline ("WARNING: brel: Can't handle " ^ x ^ " converting it to |");
    Some BOr
  | _ -> None
)

let do_call f k msg =
  try f k with Common.ParseError l -> parse_error l msg k

let do_parse f k msg =
  match do_call f k msg with
  | Some p -> p
  | None -> abort_error msg k

let rec build_nexp : nexp builder =
  let open Yojson.Basic in
  (fun j ->
  b_either (build_var |> b_map (fun x -> Var x))
  (choose_one_of [
    binary_operator (fun o n1 n2 ->
      let n1 = do_parse build_nexp n1 "nbin.lhs" in
      let n2 = do_parse build_nexp n2 "nbin.rhs" in
      Some (n_bin (parse_nbin.run o) n1 n2)
    );
    "FloatingLiteral", (["value"], function
      | [`Int n] -> (* no conversion needed *) Some (Num n)
      | [`Float n] ->
        prerr_endline ("WARNING: converting float '" ^ Float.to_string n ^ " to integer");
        Some (Num (Float.to_int n))
      | _ -> None
    );
    "UnaryOperator", (["subExpr"; "opcode"], function
      | [n; `String "~"] ->
        prerr_endline ("WARNING: ignoring bitwise negation");
        build_nexp n
      | _ -> None
    );
    "IntegerLiteral", (["value"], function
      | [`Int n] -> Some (Num n)
      | _ -> None
    );
    "ProjExpr", (["task"; "child"], function
      | [`Int n; `String x] ->
        bind (parse_task n) (fun t -> Some (Proj (t, var_make x)))
      | _ -> None
    );
    "ConditionalOperator", (["cond"; "thenExpr"; "elseExpr"], function
      | [b; then_expr; else_expr] ->
        bind (build_bexp b) (fun b ->
          bind (build_nexp then_expr) (fun n1 ->
            bind (build_nexp else_expr) (fun n2 ->
              Some (n_if b n1 n2)
            )
          )
        )
      | _ -> None
    );
    "FunctionDecl", (["name"], function
      | [`String x] ->
        prerr_endline ("WARNING: rewriting function pointer '" ^ x ^ "' into 0");
        Some (Num 0)
      | _ -> None
    );
    "MemberExpr", (["name"], function
      | [`String x] ->
        prerr_endline ("WARNING: rewriting lookup of field '" ^ x ^ "' into 0");
        Some (Num 0)
      | _ -> None
    );
    "DependentScopeDeclRefExpr", ([], function
      | [] ->
        prerr_endline ("WARNING: rewriting lookup of field into 0");
        Some (Num 0)
      | _ -> None
    );
    "CXXDependentScopeMemberExpr", ([], function
      | [] ->
        prerr_endline ("WARNING: rewriting lookup of field into 0");
        Some (Num 0)
      | _ -> None
    );
    "CallExpr", (["func"; "args"], function
    | [f; `List l] ->
      let x = parse_var.run f in
      begin match (var_name x), l with
      | "min", [n1;n2] ->
        let n1 = do_parse build_nexp n1 "min.lhs" in
        let n2 = do_parse build_nexp n2 "min.rhs" in
        Some (n_if (n_lt n1 n2) n1 n2)
      | "max", [n1;n2] ->
        let n1 = do_parse build_nexp n1 "max.lhs" in
        let n2 = do_parse build_nexp n2 "max.rhs" in
        Some (n_if (n_gt n1 n2) n1 n2)
      | _, _ ->
        prerr_endline ("WARNING: rewriting function call to '" ^ var_name x ^ "' into 0");
        Some (Num 0)
      end
    | _ -> None
    );
  ]) j)

and build_bexp : bexp builder =
  let open Yojson.Basic in
  fun j ->
  choose_one_of [
    binary_operator (fun o e1 e2 ->
        match parse_nrel_opt o with
        | Some n ->
          bind (build_nexp e1) (fun n1 ->
            bind (build_nexp e2) (fun n2 ->
              Some (n_rel n n1 n2)
            )
          )
        | None ->
          bind (build_bexp e1) (fun b1 ->
            bind (build_bexp e2) (fun b2 ->
              Some (b_rel (parse_brel.run o) b1 b2)))
    );
    "BinaryOperator", (["opcode"], function
      | [`String o] ->
        prerr_endline ("WARNING: boolean binary operation " ^  o ^ " was not inferred correctly, rewrite to false");
        Some (Bool false)
      | _ ->
        prerr_endline ("WARNING: a boolean binary operation was not inferred correctly, rewrite to false");
        Some (Bool false)
    );
    "IntegerLiteral", (["value"], function
      | [`Int n] -> Some (Bool (n != 0))
      | _ -> None
    );
    "UnaryOperator", (["subExpr"; "opcode"], function
      | [b; `String "!"] ->
        bind (build_bexp b) (fun b -> Some (b_not b))
      | _ -> None
    );
    "PredicateExpr", (["subExpr"; "opcode"], function
      | [n; `String opcode] ->
        bind (build_nexp n) (fun n ->
          Some (Pred (opcode, n))
        )
      | _ -> None
    );
    "DistinctExpr", (["args"], function
      | [`List l] ->
        let on_elem (idx, n) =
          let idx = string_of_int (idx + 1) in
          do_call parse_var.run n ("When parsing a DistinctExpr, error parsing argument #" ^ idx)
        in
        Some (enumerate l |> List.map on_elem |> distinct)
      | _ -> None
    );
    "FunctionDecl", (["name"], function
    | [`String x] ->
      prerr_endline ("WARNING: variable (function) '" ^ x ^ "' being used in a boolean context, rewrite to false");
      Some (Bool false)
    | _ -> None
    );
    "NonTypeTemplateParmDecl", (["name"], function
    | [`String x] ->
      prerr_endline ("WARNING: variable '" ^ x ^ "' being used in a boolean context, rewrite to false");
      Some (Bool false)
    | _ -> None
    );
    "VarDecl", (["name"], function
    | [`String x] ->
      prerr_endline ("WARNING: variable '" ^ x ^ "' being used in a boolean context, rewrite to false");
      Some (Bool false)
    | _ -> None
    );
    "ParmVarDecl", (["name"],
      function [`String x] ->
      prerr_endline ("WARNING: variable '" ^ x ^ "' being used in a boolean context, rewrite to false");
      Some (Bool false)
      | _ -> None
    );
    "CallExpr", (["func"], function
    | [f] ->
      let x = parse_var.run f in
      prerr_endline ("WARNING: rewriting boolean function call to '" ^ var_name x ^ "' into 0");
      Some (Bool false)
    | _ -> None
    );
    "CXXBoolLiteralExpr", (["value"], function
    | [`Bool b] ->
      Some (Bool b)
    | _ -> None
    );
    "MemberExpr", ([], function
    | [] ->
      prerr_endline ("WARNING: field lookup converted into 0");
      Some (Bool true)
    | _ -> None
    );
  ] j

let parse_nexp = make "nexp" build_nexp

let parse_bexp = make "bexp" build_bexp

let parse_range_kind (n:nexp) =
  let open Yojson.Basic in
  make "range kind" (fun s ->
    match s with
    | `String "+" -> Some (Default n)
    | `String x -> Some (StepName x)
    | _ -> None
  )

let parse_range (x:variable) = make "range" (fun s ->
    choose_one_of [
      "RangeExpr", (["init"; "upper_bound"; "step"; "opcode"], function
      | [init; ub; step; k] ->
        let n_parse = parse_nexp.run in
        Some {
          range_var = x;
          range_lower_bound = n_parse init;
          range_upper_bound = n_parse ub;
          range_step = (parse_range_kind (n_parse step)).run k;
        }
      | _ -> None
      )
    ] s
  )

let parse_mode =
  let open Yojson.Basic in
  make "mode" (fun j ->
    match j with
    | `String "ro" -> Some R
    | `String "rw" -> Some W
    | _ -> None
  )

let is_var o =
  let k = get_kind_res o in
  k = Result.Ok "VarDecl" || k = Result.Ok "ParmVarDecl"

let rec build_stmt : stmt builder =
  let open Yojson.Basic in
  let open Yojson.Basic.Util in
  fun j ->
  let s_parse = do_parse build_stmt in
  let n_parse = parse_nexp.run in
  let b_parse = parse_bexp.run in
  let v_parse = parse_var.run in
  choose_one_of [
    "SyncStmt", ([], fun _ -> Some (Inst ISync));
    "AccessStmt", (["location"; "mode"; "index"], function
      | [x; m; `List idx] ->
        Some (Inst (IAcc (v_parse x, {
          access_mode = parse_mode.run m;
          access_index = List.map n_parse idx;
        })))
      | _ -> None);
    "AssertStmt", (["cond"], function
      | [b] -> Some (Inst (IAssert (b_parse b)))
      | _ -> None
    );
    "LocationAliasStmt", (["source"; "target"; "offset"], function
      | [src; target; offset] ->
        Some (LocationAlias {
          alias_source = v_parse src;
          alias_target = v_parse target;
          alias_offset = n_parse offset;
        })
      | _ -> None
    );
    "IfStmt", (["cond"], function
      | [cond] ->
        let get_branch (o:Yojson.Basic.t) k =
          let msg = "When parsing IfStmt, could not parse branch " ^ k in
          match member k j with
            | `Assoc _ as o ->
              s_parse o k
            | `Null -> Block []
            | _ -> abort_error msg j
        in
        let cond = b_parse cond in
        let then_stmt = get_branch j "thenStmt" in
        let else_stmt = get_branch j "elseStmt" in
        Some (s_if cond then_stmt else_stmt)
      | _ -> None
    );
    "CompoundStmt", (["inner"], function
      | [`Assoc _ as i] -> build_stmt i
      | [`List l] ->
        let on_elem (idx, j) : stmt =
          let idx = string_of_int (idx + 1) in
          let msg = "When parsing CompoundStmt, could not parse #" ^ idx in
          s_parse j msg
        in
        Some (enumerate l |> List.map on_elem |> s_block)
      | _ -> None
    );
    "ForEachStmt", (["var"; "range"; "body"], function
      | [v; r; body] ->
        bind (build_stmt body) (fun body ->
          let x = v_parse v in
          Some (s_for ((parse_range x).run r) body)
        )
      | _ -> None
    );
    "DeclStmt", (["inner"], function
      | [`List [(`Assoc _ ) as j] ] when is_var j && has_type j is_int_type ->
        let o = match member "inner" j with
          | `List [e] -> Some (n_parse e)
          | _ -> None
        in
        Some (Decl (v_parse j, Local, o))
      | _ ->
        (* XXX: Silently ignore any unrecognized declaration*)
        Some (Block [])
    );
    "CXXOperatorCallExpr", ([], function
      | [] -> Some (Block [])
      | _ -> None
    );
    "UnaryOperator", ([], function
      | [] -> Some (Block [])
      | _ -> None
    );
    "CallExpr", ([], function
      | [] -> Some (Block [])
      | _ -> None
    );
    "BinaryOperator", (["lhs"; "rhs"; "type"; "opcode"], function
      | [lhs; rhs; ty; `String "="] when is_var lhs && is_int_type ty ->
         Some (Decl (v_parse lhs, Local, Some (n_parse rhs)))
      | [_; _; _; _] ->
         Some (Block [])
      | _ -> None
    );
    "DoStmt", (["body"], function
      | [body] ->
        bind (build_stmt body) (fun body ->
          Some (s_loop body)
        )
      | _ -> None
    );
    "WhileStmt", (["body"], function
      | [body] ->
        bind (build_stmt body) (fun body ->
          Some (s_loop body)
        )
      | _ -> None
    );
    "ForStmt", (["body"], function
      | [body] ->
        bind (build_stmt body) (fun body ->
          Some (s_loop body)
        )
      | _ -> None
    );
    "SwitchStmt", ([], function
      | [] ->
        (* We currently do not interpret switch statments *)
        Some (Block [])
      | _ -> None
    );
    "GotoStmt", ([], function
      | [] ->
        (* We currently do not interpret goto statments *)
        Some (Block [])
      | _ -> None
    );
    "BreakStmt", ([], function
      | [] ->
        (* We currently do not interpret return statments *)
        Some (Block [])
      | _ -> None
    );
    "ReturnStmt", ([], function
      | [] ->
        (* We currently do not interpret return statments *)
        Some (Block [])
      | _ -> None
    );
    "CXXUnresolvedConstructExpr", ([], function
      | [] ->
        Some (Block [])
      | _ -> None
    );
    "VarDecl", ([], function
    | [] ->
      Some (Block [])
    | _ -> None
    );
  ] j

let parse_stmt = make "statement" build_stmt


let mk_array (h:hierarchy_t) (ty:Ctype.t) : array_t =
  {
    array_hierarchy = h;
    array_size = Ctype.get_array_length ty;
    array_type = Ctype.get_array_type ty;
  }

let parse_kernel (shared: (variable * array_t) list) =
  make "kernel" (fun k ->
    let open Yojson.Basic in
    let open Yojson.Basic.Util in
    let v_parse = parse_var.run in
    let b_parse = parse_bexp.run in
    let s_parse = parse_stmt.run in
    choose_one_of [
      "FunctionDecl", (["body"; "pre"; "params"; "name"], function
        | [body; pre; `List func_params; `String name] ->
          begin
            let is_used j =
              member "isReferenced" j = `Bool true
              || member "isUsed" j = `Bool true
            in
            let is_param p l =
              match get_kind_res l, is_used l, member "type" l with
              | Result.Ok "NonTypeTemplateParmDecl", true, ty -> p ty
              | Result.Ok "ParmVarDecl", true, ty -> p ty
              | _, _, _ -> false
            in
            let params : VarSet.t =
              List.filter (is_param is_int_type) func_params
                |> List.map v_parse
                |> VarSet.of_list
            in
            let arrays : array_t VarMap.t =
              let parse_array a : (variable * array_t) option =
                let open Proto in
                let k = v_parse a in
                let h = if is_shared a
                  then SharedMemory
                  else GlobalMemory
                in
                let* ty = (Ojson.get_object a >>= get_type) in
                Some (k, mk_array h ty)
              in
              List.filter (is_param is_array_type) func_params
              |> Common.map_opt parse_array
              |> List.append shared
              |> list_to_var_map
            in
            let body : stmt = match body with
            | `Null -> Block []
            | _ -> s_parse body
            in
            Some {
              p_kernel_name = name;
              p_kernel_pre = b_parse pre;
              p_kernel_arrays = arrays;
              p_kernel_params = params;
              p_kernel_code = body;
            }
          end
        | _ -> None
      );
    ] k
  )

(* kind: AnnotateAttr
    spelling: annotate
    value: ' __attribute__((annotate("shared")))'
*)
let is_shared_attr (o:j_object) : bool =
  let open Ojson in
  (* kind: AnnotateAttr *)
  let is_attr = has_kind ["AnnotateAttr"] o in
  (* value: ' __attribute__((annotate("shared"))) *)
  let is_shared =
    get_field "value" o
    >>= get_string
    |> Option.map (fun k -> k = " __attribute__((annotate(\"shared\")))")
    |> to_bool
  in
  is_attr && is_shared

(*
inner:
- kind: AnnotateAttr
  spelling: annotate
  value: ' __attribute__((annotate("shared")))'
isUsed: true
kind: VarDecl
name: smem
type:
  qualType: int [128]
*)
let has_shared_attr (o:j_object) : bool =
  let open Ojson in
  get_field "inner" o
  >>= get_list
  >>= get_nth 0
  >>= get_object
  |> Option.map is_shared_attr
  |> to_bool

let is_shared_array (o:j_object) : bool =
  let open Ojson in
  has_kind variable_kind o && has_shared_attr o

let filter_shared_decl (j:Yojson.Basic.t) : (variable * array_t) list =
  let open Yojson.Basic in
  let open Yojson.Basic.Util in
  let parse_array (j: Yojson.Basic.t) : (variable * array_t) option =
    let open Proto in
    let* k = build_var j in
    let* ty = Ojson.get_object j >>= get_type in
    Some (k, mk_array SharedMemory ty)
  in
  let open Ojson in
  match get_list j with
  | None -> []
  | Some l ->
    List.filter (fun j ->
      get_object j
      |> Option.map is_shared_array
      |> to_bool
    ) l
    |> Common.map_opt parse_array

let parse_kernels =
  make "kernels" (fun s ->
    let shared = filter_shared_decl s in
    let open Yojson.Basic in
    let open Yojson.Basic.Util in
    choose_one_of [
      "TranslationUnitDecl", (["inner"], function
        | [`List l] ->
          let is_kernel x =
            match get_kind_res x, member "is_kernel" x with
            | Result.Ok "FunctionDecl", `Bool true -> true
            | _, _ -> false
          in
          Some (List.map (parse_kernel shared).run (List.filter is_kernel l))
        | _ -> None)
    ] s
  )
