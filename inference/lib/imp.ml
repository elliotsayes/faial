open Stage0
open Protocols

let (@) = Common.append_tr

open Exp
open Proto
open Subst

type var_type = Location | Index

type locality = Global | Local

type access_expr = {access_index: nexp list; access_mode: Access.Mode.t}

type alias_expr = {alias_source: Variable.t; alias_target: Variable.t; alias_offset: nexp}

type stmt =
| Sync
| Assert of bexp
| Acc of (Variable.t * Access.t)
| Block of (stmt list)
| LocationAlias of alias_expr
| Decl of (Variable.t * locality * nexp option) list
| If of (bexp * stmt * stmt)
| For of (Range.t * stmt)

type prog = stmt list


module Post = struct
  type inst =
  | Skip
  | Sync
  | Acc of (Variable.t * Access.t)
  | If of (bexp * inst list * inst list)
  | For of (Range.t * inst list)
  | Decl of (Variable.t * locality * nexp option * inst list)

  type prog = inst list


  let prog_to_s: prog -> string =
    let rec stmt_to_s : inst -> Indent.t list =
      function
      | Skip -> []
      | Sync -> [Line "sync;"]
      | Acc (x, e) -> [Line (Access.to_string ~name:(Variable.name x) e)]
      | Decl (x, l, n, p) ->
        let entry =
          (match l with | Global -> "global" | Local ->  "local") ^ " " ^
          Variable.name x ^
          (match n with | Some n -> " = " ^ n_to_string n | None -> "")
        in
        [
          Line ("decl " ^ entry ^ " {");
          Block (prog_to_s p);
          Line "}";
        ]

      | If (b, s1, s2) -> [
          Line ("if (" ^ b_to_string b ^ ") {");
          Block (prog_to_s s1);
          Line "} else {";
          Block (prog_to_s s2);
          Line "}"
        ]

      | For (r, s) -> [
          Line ("foreach (" ^ Range.to_string r ^ ") {");
          Block (prog_to_s s);
          Line ("}")
        ]
    and prog_to_s : prog -> Indent.t list =
      fun p -> List.map stmt_to_s p |> List.flatten
    in
    fun p -> prog_to_s p |> Indent.to_string

  let rec loc_subst_i (alias:alias_expr) (i:inst) : inst =
    match i with
    | Acc (x, a) ->
      if Variable.equal x alias.alias_target
      then (
        match a.index with
        | [n] ->
          Acc (x, { a with index = [n_plus alias.alias_offset n] })
        | _ ->
          let idx = List.length a.index |> string_of_int in
          failwith ("Expecting an index with dimension 1, but got " ^ idx)
      )
      else i
    | Decl (x, h, o, l) -> Decl (x, h, o, loc_subst_p alias l)
    | If (b, s1, s2) -> If (b, loc_subst_p alias s1, loc_subst_p alias s2)
    | For (r, s) -> For (r, loc_subst_p alias s)
    | Sync -> Sync
    | Skip -> Skip
  and loc_subst_p (alias:alias_expr) (p:prog) : prog =
    match p with
    | [] -> []
    | i :: p -> loc_subst_i alias i :: loc_subst_p alias p

  

  module SubstMake(S:Subst.SUBST) = struct
    module M = Subst.Make(S)
    let o_subst (st:S.t): nexp option -> nexp option =
      function
      | Some n -> Some (M.n_subst st n)
      | None -> None
      
    let rec subst_i (st:S.t) (s:inst) : inst =
      match s with
      | Sync -> Sync
      | Skip -> Skip
      | Acc (x, a) -> Acc (x, M.a_subst st a)
      | Decl (x, h, o, p) ->
        Decl (x, h, Option.map (M.n_subst st) o,
          M.add st x (function
          | Some st' -> subst_p st' p
          | None -> p
          )
        )
      | If (b, p1, p2) -> If (M.b_subst st b, subst_p st p1, subst_p st p2)
      | For (r, p) ->
        For (M.r_subst st r,
          M.add st r.var (function
          | Some st -> subst_p st p
          | None -> p
          )
        )

    and subst_p (st:S.t) (p:prog) : prog =
      List.map (subst_i st) p
  
  end

  module ReplacePair = SubstMake(Subst.SubstPair)
  let subst_i = ReplacePair.subst_i
  let subst_p = ReplacePair.subst_p

  let filter_locs (locs:Memory.t Variable.Map.t) : prog -> prog =
    let rec filter_i (i:inst) : inst =
      match i with
      | Acc (x, _) -> if Variable.Map.mem x locs then i else Skip
      | Skip -> Skip
      | Sync -> Sync
      | If (b, p1, p2) -> If (b, filter_p p1, filter_p p2)
      | For (r, p) -> For (r, filter_p p)
      | Decl (x, l, o, p) -> Decl (x, l, o, filter_p p)
    and filter_p (p: prog) : prog =
      List.map filter_i p
    in
      filter_p

  let get_decls (globals:Variable.Set.t) (p:prog) : Variable.Set.t * Variable.Set.t =
    let rec get_decls_i (p:inst) (locals,globals:Variable.Set.t * Variable.Set.t) : Variable.Set.t * Variable.Set.t =
      match p with
      | Acc _
      | Skip
      | Sync -> locals, globals
      | If (_, p1, p2) -> get_decls_p p1 (locals, globals) |> get_decls_p p2
      | Decl (x, h, _, p) ->
        let locals, globals = match h with
        | Local -> Variable.Set.add x locals, globals
        | Global -> locals, Variable.Set.add x globals
        in
        get_decls_p p (locals, globals)
      | For (_, p) -> get_decls_p p (locals,globals)
    and get_decls_p (p:prog) (locals,globals:Variable.Set.t * Variable.Set.t) : Variable.Set.t * Variable.Set.t =
      List.fold_right get_decls_i p (locals,globals)
    in
    get_decls_p p (Variable.Set.empty, globals)

  let inline_decls (known:Variable.Set.t) : prog -> prog =
    let n_subst (st:SubstAssoc.t) (n:nexp): nexp =
      if SubstAssoc.is_empty st
      then n
      else ReplaceAssoc.n_subst st n
    in
    let b_subst (st:SubstAssoc.t) (b:bexp): bexp = if SubstAssoc.is_empty st
      then b
      else ReplaceAssoc.b_subst st b
    in
    let a_subst (st:SubstAssoc.t) (a:Access.t): Access.t =
      if SubstAssoc.is_empty st
      then a
      else ReplaceAssoc.a_subst st a
    in
    let r_subst (st:SubstAssoc.t) (r:Range.t): Range.t =
      if SubstAssoc.is_empty st
      then r
      else ReplaceAssoc.r_subst st r
    in
    let rec inline_i (known:Variable.Set.t) (st:SubstAssoc.t) (i:inst) : prog =
      let add_var (x:Variable.t) : Variable.t * Variable.Set.t * SubstAssoc.t =
        let x, st =
          if Variable.Set.mem x known
          then (
            let new_x = Variable.fresh known x in
            (new_x, SubstAssoc.put st x (Var new_x)) 
          ) else (x, st)
        in
        let known = Variable.Set.add x known in
        (x, known, st)
      in
      match i with
      | Sync -> [Sync]
      | Acc (x,e) -> [Acc (x, a_subst st e)]
      | Skip -> []
      | If (b, p1, p2) ->
        let b = b_subst st b in
        [If (b, inline_p known st p1, inline_p known st p2)]
      | Decl (x, _, Some n, p) ->
        let n = n_subst st n in
        let st = SubstAssoc.put st x n  in
        inline_p known st p
      | Decl (x, h, None, p) ->
        let (x, known, st) = add_var x in
        [Decl (x, h, None, inline_p known st p)]
      | For (r, p) ->
        let r = r_subst st r in
        let (x, known, st) = add_var r.var in
        [For ({r with var = x}, inline_p known st p)]
    and inline_p (known:Variable.Set.t) (st:SubstAssoc.t): prog -> prog =
      List.concat_map (inline_i known st)
    in
    inline_p known (SubstAssoc.make [])


end


(*
  Translation goals:
  1. lexical scoping is contained in the AST term (simplifies substitution)
  2. inline assignments given by Imp.Decl
  3. inline array alias
  4. inline asserts

  1. In Imp, the lexical scoping of a variable binding is the sequence
  of statements that succeed that statement. In Post, the lexical scoping is
  always  _contained_ in the variable binding operator.
  
  For instance a variable declaration in Imp:
    var x; s1; ...; sn
  Becomes
    var x { s1; ...; sn }
  
  2. In Imp we can have local variable assignments. We inline such assignments
  in Post. However, variable declaration still remains in Post.
  
  In Imp:
    local x = 1; s1; ...; sn
  becomes in Post:
    local x {s1[x=1] ; ... sn[x=1]}
*)
let imp_to_post (s:stmt) : Post.prog =
  let rec imp_to_post_s (s:stmt) : Post.prog =
    match s with
    | Sync -> [Post.Sync]
    | Acc e -> [Post.Acc e]
    | Block p -> imp_to_post_p p
    | If (b, s1, s2) -> [Post.If (b, imp_to_post_p [s1], imp_to_post_p [s2])]
    | For (r, s) -> [Post.For (r, imp_to_post_p [s])]
    (* Handled in the context of a prog *)
    | Assert _ -> failwith "unsupported"
    | LocationAlias _ -> failwith "unsupported"
    | Decl _ -> failwith "unsupported"
  and imp_to_post_p (p:prog) : Post.prog =
    match p with
    | [] -> []
    | Assert b :: p -> [Post.If (b, imp_to_post_p p, [])]
    | LocationAlias e :: p -> imp_to_post_p p |> Post.loc_subst_p e
    | Decl [] :: p -> imp_to_post_p p
    | Decl ((x,v,o)::l) :: p ->
      [Post.Decl (x, v, o, imp_to_post_p (Decl l :: p))]
    | s :: p ->
      imp_to_post_s s @ imp_to_post_p p
  in
  imp_to_post_s (Block [s])

let post_to_proto (p: Post.prog) : Proto.prog =
  let rec post_to_proto_i (i:Post.inst) : Proto.prog =
    let open Post in
    match i with
    | Sync -> [Proto.Sync]
    | Acc (x,e) -> [Proto.Acc (x, e)]
    | Skip -> []
    | If (b, p1, p2) ->
      Proto.p_cond b (post_to_proto_p p1) @ Proto.p_cond (b_not b) (post_to_proto_p p2)
    | For (r, p) ->
      [Proto.Loop (r, post_to_proto_p p)]
    | Decl (_, _, Some _, _) ->
      failwith ("Run inline_decl first: " ^ Post.prog_to_s [i])
    | Decl (_, _, None, p) -> post_to_proto_p p
  and post_to_proto_p (p:Post.prog) : Proto.prog =
    match p with
    | [] -> []
    | i::p -> 
      post_to_proto_i i @ post_to_proto_p p
  in
  post_to_proto_p p


let s_block l =
  Block (
    List.filter (function
      | Block [] -> false
      | Decl [] -> false
      | _ -> true
    ) l
  )

let s_for (r:Range.t) (s:stmt) =
  match s with
  | Block [] -> Block []
  | Decl [] -> Decl []
  | _ -> For (r, s)

let s_if (b:bexp) (p1:stmt) (p2:stmt) : stmt =
  match b, p1, p2 with
  | (Bool false, _, p)
  | (Bool true, p, _)
    -> p
  | (_, Block [], Block []) -> Block []
  | _ -> If (b, p1, p2)

type p_kernel = {
  (* The kernel name *)
  p_kernel_name: string;
  (* A kernel precondition of every phase. *)
  p_kernel_pre: bexp;
  (* The shared locations that can be accessed in the kernel. *)
  p_kernel_arrays: Memory.t Variable.Map.t;
  (* The internal variables are used in the code of the kernel.  *)
  p_kernel_params: Variable.Set.t;
  (* The code of a kernel performs the actual memory accesses. *)
  p_kernel_code: stmt;
}

let stmt_to_s: stmt -> Indent.t list =
  let rec stmt_to_s : stmt -> Indent.t list =
    function
    | Sync -> [Line "sync;"]
    | Assert b -> [Line ("assert (" ^ b_to_string b ^ ");")]
    | Acc (x, e) -> [Line (Access.to_string ~name:(Variable.name x) e)]
    | Block [] -> []
    | Block l -> [Line "{"; Block (List.map stmt_to_s l |> List.flatten); Line "}"]
    | LocationAlias l ->
      [Line ("alias " ^
        Variable.name l.alias_target ^ " = " ^
        Variable.name l.alias_source ^ " + " ^
        n_to_string l.alias_offset ^ ";"
      )]
    | Decl [] -> []
    | Decl l ->
      let entry (x, l, n) =
        (match l with | Global -> "global" | Local ->  "local") ^ " " ^
        Variable.name x ^
        (match n with | Some n -> " = " ^ n_to_string n | None -> "")
      in
      let entries = Common.join "," (List.map entry l) in
      [Line ("decl " ^ entries ^ ";")]

    | If (b, s1, Block []) -> [
        Line ("if (" ^ b_to_string b ^ ") {");
        Block (stmt_to_s s1);
        Line "}";
      ]
    
    | If (b, s1, s2) -> [
        Line ("if (" ^ b_to_string b ^ ") {");
        Block (stmt_to_s s1);
        Line "} else {";
        Block (stmt_to_s s2);
        Line "}"
      ]
    | For (r, s) -> [
        Line ("foreach (" ^ Range.to_string r ^ ") {");
        Block (stmt_to_s s);
        Line ("}")
      ]
  in
  stmt_to_s

let kernel_to_s (k:p_kernel) : Indent.t list =
  let pre = match k.p_kernel_pre with
  | Bool true -> ""
  | _ -> " if (" ^ b_to_string k.p_kernel_pre ^ ")"
  in
  [
    Line (
      k.p_kernel_name ^
      " (" ^ Memory.map_to_string k.p_kernel_arrays ^ ", " ^
      Variable.set_to_string k.p_kernel_params ^ ")" ^
      pre ^ " {");
    Block (stmt_to_s k.p_kernel_code);
    Line "}"
  ]

let print_kernel (k: p_kernel) : unit =
  Indent.print (kernel_to_s k)

let compile (k:p_kernel) : Proto.prog kernel =
  let p : Post.prog = imp_to_post k.p_kernel_code
    |> Post.filter_locs k.p_kernel_arrays (* Remove unknown arrays *)
    (* Inline local variable assignment and ensure variables are distinct*)
    |> Post.inline_decls k.p_kernel_params
  in
  let (locals, globals) = Post.get_decls k.p_kernel_params p in
  let p = post_to_proto p in
  let rec pre_from_body (l:Proto.prog) : (bexp * Proto.prog) =
    match l with
    | [Cond(b,[Cond(b',l)])] -> pre_from_body [Cond(b_and b b', l)]
    | [Cond(b, l)] -> (b, l)
    | l -> (Bool true, l)
  in
  let (more_pre, p) = p |> pre_from_body in
  let pre = b_and k.p_kernel_pre more_pre in
  (*
    1. We rename all variables so that they are all different
    2. We break down for-loops and variable declarations
    *)
  {
    kernel_name = k.p_kernel_name;
    kernel_pre = pre;
    kernel_arrays = k.p_kernel_arrays;
    kernel_local_variables = locals;
    kernel_global_variables = globals;
    kernel_code = p;
  }
