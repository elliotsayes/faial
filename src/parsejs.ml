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

let parse_error (cause:string list) msg data =
  raise (Common.ParseError (( "Error parsing '" ^ msg ^"': " ^ pp_js data)::cause))

let abort_error msg data =
  raise (Common.ParseError [msg ^ "\n" ^ pp_js data])

let call msg f data =
  let o = (try f data with Common.ParseError l -> parse_error l msg data) in
  match o with
  | Some m -> m
  | None ->  parse_error [] msg data

let is_some o =
  match o with
  | Some _ -> true
  | None -> false

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
  | _ -> None
)

let bind o1 f =
  match o1 with
  | Some x -> f x
  | None -> None

let bind_all l f =
  let rec aux l accum =
    match l with
    | [] -> f (List.rev accum)
    | x::l -> bind x (fun x -> aux l (x::accum))
  in
  aux l []

let member_opt k (j:Yojson.Basic.t) =
  let open Yojson.Basic.Util in
  match j with
  | `Assoc _ -> Some (member k j)
  | _ -> None

let get_kind_opt (j:Yojson.Basic.t) =
  let open Yojson.Basic.Util in
  match j with
  | `Assoc _ ->
    j |> member "kind" |> to_string_option
  | _ -> None

let get_kind (j:Yojson.Basic.t) : string =
  let open Yojson.Basic.Util in
  match j with
  | `Assoc _ ->
    begin match member "kind" j with
    | `String k -> k
    | _ -> abort_error "When getting the AST node kind from a JSON object, expecting the type to be a string, but got:" j
    end
  | _ -> abort_error "When getting the AST node kind from a JSON value, expecting a JSON object, but got:" j

let get_fields fields (j:Yojson.Basic.t) : Yojson.Basic.t list  =
  let open Yojson.Basic.Util in
  let kv = List.map (fun x -> (x, j |> member_opt x)) fields in
  let missing = List.filter (fun (x,y) -> y = None) kv |> List.split |> fst in
  if List.length missing > 0 then
    let fields = join ", " fields in
    let missing = join ", " missing in
    abort_error ("Getting fields [" ^ fields ^ "], but object is missing fields [" ^ missing ^ "]") j
  else
    List.split kv |> snd |> flatten_opt

type 'a choose_one_of_handler = string list * (Yojson.Basic.t list -> 'a option)

let choose_one_of (l:(string * 'a choose_one_of_handler) list) (j:Yojson.Basic.t) : 'a option =
  match List.assoc_opt (get_kind j) l with
  | Some ((fields:string list), kont) -> get_fields fields j |> kont
  | None ->
      let keys = List.split l |> fst |> join ", " in
      abort_error ("Expecting an AST 'kind' in [" ^ keys ^ "], but got:") j


let is_array_type o =
  let open Yojson.Basic in
  let open Yojson.Basic.Util in
  match member "qualType" o with
  | `String x -> ends_with x " *"
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

let binary_operator f =
  "BinaryOperator", (["opcode"; "lhs"; "rhs"], function
    | [o; n1; n2] -> f o n1 n2
    | _ -> None
  )

let parse_var = make "variable" (fun j ->
    let open Yojson.Basic in
    choose_one_of [
      "VarDecl", (["name"],
        function [`String x] -> Some (var_make x) | _ -> None
      );
      "ParmVarDecl", (["name"],
        function [`String x] -> Some (var_make x) | _ -> None
      );
    ] j
  )

let parse_nrel_opt m =
  let open Yojson.Basic in
  match m with
  | `String "==" -> Some NEq
  | `String "!=" -> Some NNeq
  | `String "<=" -> Some NLe
  | `String "<"  -> Some NLt
  | `String ">=" -> Some NGe
  | `String ">"  -> Some NGt
  | _ -> None

let parse_nrel = make "nrel" parse_nrel_opt

let parse_brel = make "brel" (fun m ->
  let open Yojson.Basic in
  match m with
  | `String "||" -> Some BOr
  | `String "&&" -> Some BAnd
  | _ -> None
)

let do_call f k msg =
  try f k with Common.ParseError l -> parse_error l msg k

let do_parse f k msg =
  match do_call f k msg with
  | Some p -> p
  | None -> abort_error msg k

let rec parse_nexp n : nexp option =
  let open Yojson.Basic in
  choose_one_of [
    "VarDecl", (["name"],
      function [`String x] -> Some (Var (var_make x)) | _ -> None
    );
    "ParmVarDecl", (["name"],
      function [`String x] -> Some (Var (var_make x)) | _ -> None
    );
    binary_operator (fun o n1 n2 ->
      bind (parse_nexp n1) (fun n1 ->
        bind (parse_nexp n2) (fun n2 ->
            Some (n_bin (parse_nbin.run o) n1 n2)
          ))
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
        bind (parse_bexp b) (fun b ->
          bind (parse_nexp then_expr) (fun n1 ->
            bind (parse_nexp else_expr) (fun n2 ->
              Some (NIf (b, n1, n2))
            )
          )
        )
      | _ -> None
    );
    "CallExpr", (["func"], function
    | [`String x] ->
      print_endline ("ERROR: How to handle function " ^ x );
      Some (Num 0)
    | _ -> None
    )
  ] n

and parse_bexp b : bexp option =
  let open Yojson.Basic in
  choose_one_of [
    binary_operator (fun o e1 e2 ->
        match parse_nrel_opt o with
        | Some n ->
          bind (parse_nexp e1) (fun n1 ->
            bind (parse_nexp e2) (fun n2 ->
              Some (n_rel n n1 n2)
            )
          )
        | None ->
          bind (parse_bexp e1) (fun b1 ->
            bind (parse_bexp e2) (fun b2 ->
              Some (b_rel (parse_brel.run o) b1 b2)))
    );
    "IntegerLiteral", (["value"], function
      | [`Int n] -> Some (Bool (n != 0))
      | _ -> None
    );
    "UnaryOperator", (["subExpr"; "opcode"], function
      | [b; `String "!"] ->
        bind (parse_bexp b) (fun b -> Some (b_not b))
      | _ -> None
    );
    "PredicateExpr", (["subExpr"; "opcode"], function
      | [n; `String opcode] ->
        bind (parse_nexp n) (fun n ->
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
      | _ -> None)
  ] b

let parse_nexp = make "nexp" parse_nexp

let parse_bexp = make "bexp" parse_bexp

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
      | [init; ub; step; k] -> Some {
          range_var = x;
          range_lower_bound = parse_nexp.run init;
          range_upper_bound = parse_nexp.run ub;
          range_step = (parse_range_kind (parse_nexp.run step)).run k;
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
  let k = get_kind_opt o in
  k = Some "VarDecl" || k = Some "ParmVarDecl"

let j_to_var j =
  let open Yojson.Basic.Util in
  member "name" j |> to_string |> var_make

let rec parse_stmt j =
  let open Yojson.Basic in
  let open Yojson.Basic.Util in
  let do_parse_prog = do_parse parse_stmt in

  choose_one_of [
    "SyncStmt", ([], fun _ -> Some (Inst ISync));
    "AccessStmt", (["location"; "mode"; "index"], function
      | [`String loc; m; `List idx] -> Some (Inst (IAcc (var_make loc, {
          access_index = List.map parse_nexp.run idx;
          access_mode = parse_mode.run m;
        })))
      | _ -> None);
    "AssertStmt", (["cond"], function
      | [b] -> Some (Inst (IAssert (parse_bexp.run b)))
      | _ -> None
    );
    "IfStmt", (["cond"], function
      | [cond] ->
        let get_branch (o:Yojson.Basic.t) k =
          let msg = "When parsing IfStmt, could not parse branch " ^ k in
          match member k j with
            | `Assoc _ as o ->
              do_parse_prog o k
            | `Null -> Block []
            | _ -> abort_error msg j
        in
        let cond = parse_bexp.run cond in
        let then_stmt = get_branch j "thenStmt" in
        let else_stmt = get_branch j "elseStmt" in
        Some (s_if cond then_stmt else_stmt)
      | _ -> None
    );
    "CompoundStmt", (["inner"], function
      | [`List l] ->
        let on_elem (idx, j) : stmt =
          let idx = string_of_int (idx + 1) in
          let msg = "When parsing CompoundStmt, could not parse #" ^ idx in
          do_parse_prog j msg
        in
        Some (enumerate l |> List.map on_elem |> s_block)
      | _ -> None
    );
    "ForEachStmt", (["var"; "range"; "body"], function
      | [v; r; body] ->
        bind (parse_stmt body) (fun body ->
          let x = parse_var.run v in
          Some (For ((parse_range x).run r, body))
        )
      | _ -> None
    );
    "DeclStmt", (["inner"], function
      | [`List [(`Assoc _ ) as j] ] when is_var j && has_type j is_int_type ->
        let o = match member "inner" j with
          | `List [e] -> Some (parse_nexp.run e)
          | _ -> None
        in
        Some (Decl (j_to_var j, Local, o))
      | _ ->
        (* XXX: Silently ignore any unrecognized declaration*)
        Some (Block [])
    );
    "CXXOperatorCallExpr", ([], function
      | [] -> Some (Block [])
      | _ -> None
    );
    "CallExpr", ([], function
      | [] -> Some (Block [])
      | _ -> None
    );
    "BinaryOperator", (["lhs"; "rhs"; "type"; "opcode"], function
      | [lhs; rhs; ty; `String "="] when is_var lhs && is_int_type ty ->
         Some (Decl (j_to_var lhs, Local, Some (parse_nexp.run rhs)))
      | [_; _; _; _] ->
         Some (Block [])
      | _ -> None
    );
  ] j

let parse_stmt = make "statement" parse_stmt

let parse_kernel = make "kernel" (fun k ->
  let open Yojson.Basic in
  let open Yojson.Basic.Util in
  choose_one_of [
    "FunctionDecl", (["body"; "pre"; "params"], function
      | [body; pre; `List params] ->
        begin
          let is_param p l =
            match get_kind_opt l, member "isUsed" l, member "type" l with
            | Some "ParmVarDecl", `Bool true, ty -> p ty
            | _, _, _ -> false
          in
          let get_params p =
            List.filter (is_param p) params
              |> List.map parse_var.run
              |> VarSet.of_list
          in
          Some {
            p_kernel_pre = parse_bexp.run pre;
            p_kernel_locations = get_params is_array_type;
            p_kernel_params = get_params is_int_type;
            p_kernel_code = parse_stmt.run body;
          }
        end
      | _ -> None
    )
  ] k
  )

let parse_kernels = make "kernels" (fun s ->
  let open Yojson.Basic in
  let open Yojson.Basic.Util in
  choose_one_of [
    "TranslationUnitDecl", (["inner"], function
      | [`List l] ->
        let is_kernel x =
          match get_kind_opt x, member "is_kernel" x with
          | Some "FunctionDecl", `Bool true -> true
          | _, _ -> false
        in
        Some (List.map parse_kernel.run (List.filter is_kernel l))
      | _ -> None)
  ] s
)
