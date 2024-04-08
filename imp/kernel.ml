open Stage0
open Protocols

module StringMap = Common.StringMap
module StringMapUtil = Common.StringMapUtil
module StringSet = Common.StringSet


let (@) = Common.append_tr

open Exp

type var_type = Location | Index

type access_expr = {access_index: nexp list; access_mode: Access.Mode.t}

(*
  Translation goals:
  1. lexical scoping is contained in the AST term (simplifies substitution)
  2. inline assignments given by Imp.Decl
  3. inline array alias
  4. inline asserts

  1. In Imp, the lexical scoping of a variable binding is the sequence
  of statements that succeed that statement. In Scoped, the lexical scoping is
  always  _contained_ in the variable binding operator.
  
  For instance a variable declaration in Imp:
    var x; s1; ...; sn
  Becomes
    var x { s1; ...; sn }
  
  2. In Imp we can have local variable assignments. We inline such assignments
  in Scoped. However, variable declaration still remains in Scoped.
  
  In Imp:
    local x = 1; s1; ...; sn
  becomes in Scoped:
    local x {s1[x=1] ; ... sn[x=1]}
*)

type t = {
  (* The kernel name *)
  name: string;
  (* The type signature of the kernel *)
  ty: string;
  (* The shared locations that can be accessed in the kernel. *)
  arrays: Memory.t Variable.Map.t;
  (* The internal variables are used in the code of the kernel.  *)
  params: Params.t;
  (* The code of a kernel performs the actual memory accesses. *)
  code: Stmt.t;
  (* Visibility *)
  visibility: Proto.Kernel.visible;
}

(* Generate a unique id that pairs the name and type. *)
let unique_id (k:t) : string =
  Call.kernel_id ~kernel:k.name ~ty:k.ty

let to_s (k:t) : Indent.t list =
  [
    Line (
      k.name ^
      " (" ^ Memory.map_to_string k.arrays ^ ", " ^
      Params.to_string k.params ^ ") {");
    Block (Stmt.to_s k.code);
    Line "}"
  ]

let print (k: t) : unit =
  Indent.print (to_s k)

let compile (k:t) : Proto.Code.t Proto.Kernel.t =
  let globals = k.params in
  let (globals, p) = Scoped.from_stmt (globals, k.code) in
  let p =
    p
    |> Scoped.filter_locs k.arrays (* Remove unknown arrays *)
    |> Scoped.fix_assigns
    (* Inline local variable assignment and ensure variables are distinct*)
    |> Scoped.inline_assigns (Params.to_set k.params)
    |> Encode_asserts.from_scoped
    |> Encode_asserts.to_proto
  in
  let (p, locals, pre) =
    let rec inline_header :
      (Proto.Code.t * Params.t * bexp)
      ->
      (Proto.Code.t * Params.t * bexp)
    =
      fun (p, locals, pre) ->
      match p with
      | Cond (b, p) -> inline_header (p, locals, b_and b pre)
      | Decl {var=x; body=p; ty} -> inline_header (p, Params.add x ty locals, pre)
      | _ -> (p, locals, pre)
    in
    inline_header (p, Params.empty, Bool true)
  in
  (*
    1. We rename all variables so that they are all different
    2. We break down for-loops and variable declarations
    *)
  {
    name = k.name;
    pre = pre;
    arrays = k.arrays;
    local_variables = locals;
    global_variables = globals;
    code = p;
    visibility = k.visibility;
  }

let calls (k:t) : StringSet.t =
  Stmt.calls k.code

let apply (args : (Variable.t * Arg.t) list) (k:t) : Stmt.t =
  List.fold_left (fun s (x, a) ->
    let i =
      let open Arg in
      match a with
      | Scalar e -> Stmt.Decl [Decl.set x e]
      | Unsupported -> Stmt.Decl [Decl.unset x]
      | Array u -> Stmt.LocationAlias {
          target = x;
          source = u.address;
          offset = u.offset;
        }
    in
    Stmt.Block [i; s]
  ) k.code args

let inline (funcs:t StringMap.t) (k:t) : t =
  let rec inline (s:Stmt.t) : Stmt.t =
    match s with
    | Call c ->
      (match StringMap.find_opt (Call.unique_id c) funcs with
      | Some k -> apply c.args k
      | None -> s
      )
    | Sync _ | Assert _ | Read _ | Write _ | Atomic _ | Decl _
    | LocationAlias _ | Assign _ ->
      s
    | Block l -> Block (List.map inline l)
    | If (b, s1, s2) -> If (b, inline s1, inline s2)
    | For (r, s) -> For (r, inline s)
    | Star s -> Star (inline s)
  in
  { k with code = inline k.code }
