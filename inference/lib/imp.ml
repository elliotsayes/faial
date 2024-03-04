open Stage0
open Protocols

module StringMap = Common.StringMap
module StringMapUtil = Common.StringMapUtil
module StringSet = Common.StringSet


let (@) = Common.append_tr

open Exp
open Proto
open Subst

type var_type = Location | Index

type access_expr = {access_index: nexp list; access_mode: Access.Mode.t}

let kernel_id ~kernel ~ty : string =
  kernel ^ ":" ^ ty

module Alias = struct
  type t = {source: Variable.t; target: Variable.t; offset: nexp}

  let is_trivial (a:t) : bool =
    Variable.equal a.source a.target &&
    a.offset = Num 0

  let to_string (l:t) : string =
    Variable.name l.target ^ " = " ^
    Variable.name l.source ^ " + " ^
    n_to_string l.offset ^ ";"
end

type read = {target: Variable.t; ty: C_type.t; array: Variable.t; index: nexp list}
type atomic = {target: Variable.t; ty:C_type.t; atomic: Atomic.t; array: Variable.t; index: nexp list}
type write = {array: Variable.t; index: nexp list; payload: int option}

let read_to_acc (r:read) : Variable.t * Access.t =
  (r.array, Access.{index=r.index; mode=Read})

let atomic_to_acc (a:atomic) : Variable.t * Access.t =
  (a.array, Access.{index=a.index; mode=Atomic a.atomic})

let write_to_acc (w:write) : Variable.t * Access.t =
  (w.array, Access.{index=w.index; mode=Write w.payload})

module ArrayUse = struct
  type t = {address: Variable.t; offset: nexp}
  let to_string (l:t) : string =
    Variable.name l.address ^ ":(" ^ Exp.n_to_string l.offset ^ ")"
end

module Arg = struct
  type t =
    | Scalar of nexp
    | Array of ArrayUse.t
    | Unsupported

  let to_string : t -> string =
    function
    | Unsupported -> "_"
    | Scalar e -> Exp.n_to_string e
    | Array l -> ArrayUse.to_string l
end

module Call = struct
  type t = {
    kernel: string;
    ty: string;
    args : (Variable.t * Arg.t) list
  }

  let unique_id (c:t) : string =
    kernel_id ~kernel:c.kernel ~ty:c.ty

  let to_string (c:t) : string =
    let args =
      c.args
      |> List.map (fun (k, a) ->
        Variable.name k ^ "=" ^ Arg.to_string a
      )
      |> String.concat ", "
    in
    c.kernel ^ "(" ^ args ^ ")"

end

module Decl = struct
  type t = {var: Variable.t; ty:C_type.t; init: nexp option}

  let set ?(ty=C_type.int) (var: Variable.t) (init:nexp) : t =
    {init=Some init; ty; var}

  let unset ?(ty=C_type.int) (var: Variable.t) : t =
    {init=None; ty; var}

  let map (f:nexp -> nexp) (d:t) : t =
    { d with init = Option.map f d.init }

  let to_string (d:t) : string =
    let ty = C_type.to_string d.ty in
    let x = Variable.name d.var in
    let init =
      d.init
      |> Option.map (fun n -> " = " ^ n_to_string n)
      |> Option.value ~default:""
    in
    ty ^ " " ^ x ^ init
end

module Stmt = struct

  type t =
    | Sync of Location.t option
    | Assert of bexp
    | Read of read
    | Atomic of atomic
    | Write of write
    | Block of (t list)
    | LocationAlias of Alias.t
    | Decl of Decl.t list
    | If of (bexp * t * t)
    | For of (Range.t * t)
    | Star of t
    | Call of Call.t

  let is_for : t -> bool =
    function
    | For _ -> true
    | _ -> false

  let is_if : t -> bool =
    function
    | If _ -> true
    | _ -> false

  type prog = t list

  let rec has_sync : t -> bool =
    function
    | Sync _ -> true
    | If (_, p, q) -> has_sync p || has_sync q
    | Atomic _ | Read _ | Write _ | Assert _ | LocationAlias _ | Decl _ | Call _ -> false
    | Block l -> List.exists has_sync l
    | For (_, p) | Star p -> has_sync p

  let calls : t -> StringSet.t =
    let rec calls (cs:StringSet.t) : t -> StringSet.t =
      function
      | Decl _ | LocationAlias _ | Sync _ | Assert _
      | Read _ | Write _ | Atomic _ ->
        cs
      | Block l -> List.fold_left calls cs l
      | If (_, s1, s2) -> calls (calls cs s1) s2
      | For (_, s) | Star s -> calls cs s
      | Call c -> StringSet.add (Call.unique_id c) cs
    in
    calls StringSet.empty


  let fold : 'a. (t -> 'a -> 'a) -> t -> 'a -> 'a =
    fun (f: t -> 'a -> 'a) (p:t) (init:'a) ->
      let rec fold_i (s:t) (init:'a) : 'a =
        let init : 'a = f s init in
        match s with
        | Sync _
        | Assert _
        | Read _
        | Atomic _
        | Write _
        | Decl _
        | LocationAlias _
        | Call _ ->
          init
        | Block l ->
          fold_p l init
        | If (_, s1, s2) ->
          let init = fold_i s1 init in
          fold_i s2 init
        | For (_, s)
        | Star s ->
          fold_i s init

      and fold_p (l:prog) (init:'a) : 'a =
        List.fold_right fold_i l init
      in
      fold_i p init

  let find_all_map (f: t -> 'a option) (s: t) : 'a Seq.t =
    let g (e:t) (r:'a Seq.t) : 'a Seq.t =
      match f e with
      | Some x -> Seq.cons x r
      | None -> r
    in
    fold g s Seq.empty

  let find_all (f: t -> bool) : t -> t Seq.t =
    find_all_map (fun x -> if f x then Some x else None)

  let s_block (l:t list) : t =
    Block (
      List.filter (function
        | Block [] -> false
        | Decl [] -> false
        | _ -> true
      ) l
    )

  let s_for (r:Range.t) (s:t) : t =
    match s with
    | Block [] -> Block []
    | Decl [] -> Decl []
    | _ -> For (r, s)

  let s_if (b:bexp) (p1:t) (p2:t) : t =
    match b, p1, p2 with
    | (Bool false, _, p)
    | (Bool true, p, _)
      -> p
    | (_, Block [], Block []) -> Block []
    | _ -> If (b, p1, p2)


  let to_s: t -> Indent.t list =
    let rec stmt_to_s : t -> Indent.t list =
      function
      | Call c -> [Line (Call.to_string c)]
      | Sync _ -> [Line "sync;"]
      | Assert b -> [Line ("assert (" ^ b_to_string b ^ ");")]
      | Atomic r -> [Line (Variable.name r.target ^ " = atomic " ^ Variable.name r.array ^ Access.index_to_string r.index ^ ";")]
      | Read r -> [Line (Variable.name r.target ^ " = rd " ^ Variable.name r.array ^ Access.index_to_string r.index ^ ";")]
      | Write w ->
        let payload :string = match w.payload with
          | None -> ""
          | Some x -> " = " ^ string_of_int x
        in
        [Line ("wr " ^ Variable.name w.array ^ Access.index_to_string w.index ^ payload ^ ";")]
      | Block [] -> []
      | Block l -> [Line "{"; Block (List.map stmt_to_s l |> List.flatten); Line "}"]
      | LocationAlias l ->
        [Line ("alias " ^ Alias.to_string l)]
      | Decl [] -> []
      | Decl l ->
        let entries = Common.join "," (List.map Decl.to_string l) in
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
      | Star s -> [
          Line ("foreach (?) {");
          Block (stmt_to_s s);
          Line ("}")
        ]
      | For (r, s) -> [
          Line ("foreach (" ^ Range.to_string r ^ ") {");
          Block (stmt_to_s s);
          Line ("}")
        ]
    in
    stmt_to_s

  let to_string (s: t) : string =
    Indent.to_string (to_s s)
end

module Post = struct
  type t =
  | Skip
  | Sync of Location.t option
  | Acc of (Variable.t * Access.t)
  | If of (bexp * t * t)
  | For of (Range.t * t)
  | Decl of (Decl.t * t)
  | Seq of t * t

  let to_string: t -> string =
    let rec to_s : t -> Indent.t list =
      function
      | Skip -> [Line "skip;"]
      | Sync _ -> [Line "sync;"]
      | Acc (x, e) -> [Line (Access.to_string ~name:(Variable.name x) e)]
      | Decl (d, p) ->
        [
          Line ("decl " ^ Decl.to_string d ^ " {");
          Block (to_s p);
          Line "}";
        ]

      | If (b, s1, s2) -> [
          Line ("if (" ^ b_to_string b ^ ") {");
          Block (to_s s1);
          Line "} else {";
          Block (to_s s2);
          Line "}"
        ]

      | For (r, s) -> [
          Line ("foreach (" ^ Range.to_string r ^ ") {");
          Block (to_s s);
          Line ("}")
        ]
      | Seq (p, q) ->
        to_s p @ to_s q
    in
    fun p -> to_s p |> Indent.to_string

  let loc_subst (alias:Alias.t) : t -> t =
    let rec loc_subst : t -> t =
      function
      | Acc (x, a) as i ->
        if Variable.equal x alias.target
        then (
          match a.index with
          | [n] ->
            (* use the inlined variable but with the location of the alias,
              so that the error message appears in the right place. *)
            let new_x = { alias.source with location = x.location } in
            Acc (new_x, { a with index = [n_plus alias.offset n] })
          | _ ->
            let idx = List.length a.index |> string_of_int in
            failwith ("Expecting an index with dimension 1, but got " ^ idx)
        )
        else i
      | Decl (d, l) -> Decl (d, loc_subst l)
      | If (b, s1, s2) -> If (b, loc_subst s1, loc_subst s2)
      | For (r, s) -> For (r, loc_subst s)
      | Sync l -> Sync l
      | Skip -> Skip
      | Seq (p, q) ->
        Seq (loc_subst p, loc_subst q)
    in
    fun s ->
      if Alias.is_trivial alias then
        s
      else
        loc_subst s

  module SubstMake(S:Subst.SUBST) = struct
    module M = Subst.Make(S)

    let o_subst (st:S.t): nexp option -> nexp option =
      function
      | Some n -> Some (M.n_subst st n)
      | None -> None
      
    let rec subst (st:S.t) : t -> t =
      function
      | Sync l -> Sync l
      | Skip -> Skip
      | Acc (x, a) -> Acc (x, M.a_subst st a)
      | Decl (d, p) ->
        let d = Decl.map (M.n_subst st) d in
        Decl (d, M.add st d.var (function
          | Some st' -> subst st' p
          | None -> p
          )
        )
      | If (b, p1, p2) ->
        If (M.b_subst st b, subst st p1, subst st p2)
      | For (r, p) ->
        For (M.r_subst st r,
          M.add st r.var (function
          | Some st -> subst st p
          | None -> p
          )
        )
      | Seq (p, q) -> Seq (subst st p, subst st q)
  
  end

  module ReplacePair = SubstMake(Subst.SubstPair)
  let subst = ReplacePair.subst

  let filter_locs (locs:Memory.t Variable.Map.t) : t -> t =
    let rec filter : t -> t =
      function
      | Acc (x, _) as i ->
        if Variable.Map.mem x locs then i else Skip
      | Skip -> Skip
      | Sync l -> Sync l
      | If (b, p1, p2) -> If (b, filter p1, filter p2)
      | For (r, p) -> For (r, filter p)
      | Decl (d, p) -> Decl (d, filter p)
      | Seq (p1, p2) -> Seq (filter p1, filter p2)
    in
      filter

  let vars_distinct : t -> t =
    let rec distinct (vars:Variable.Set.t) (p: t) : Variable.Set.t * t =
      match p with
      | Acc _ | Skip | Sync _ -> vars, p
      | Seq (p, q) ->
        let (vars, p) = distinct vars p in
        let (vars, q) = distinct vars q in
        vars, Seq (p, q)
      | If (b, p, q) ->
        let (vars, p) = distinct vars p in
        let (vars, q) = distinct vars q in
        vars, If (b, p, q)
      | Decl (d, p) ->
        let x = d.var in
        if Variable.Set.mem x vars then (
          let new_x : Variable.t = Variable.fresh vars x in
          let vars = Variable.Set.add new_x vars in
          let p = subst (x, Var new_x) p in
          let (vars, p) = distinct vars p in
          vars, Decl ({ d with var=new_x;}, p)
        ) else
          let (vars, p) = distinct (Variable.Set.add x vars) p in
          vars, Decl (d, p)
      | For (r, p) ->
        let x = Range.var r in
        if Variable.Set.mem x vars then (
          let new_x : Variable.t = Variable.fresh vars x in
          let vars = Variable.Set.add new_x vars in
          let p = subst (x, Var new_x) p in
          let (vars, p) = distinct vars p in
          vars, For ({ r with var = new_x }, p)
        ) else
          let (vars, p) = distinct (Variable.Set.add x vars) p in
          vars, For (r, p)
    in
    fun p ->
      distinct Variable.Set.empty p |> snd
  let inline_assigns (known:Variable.Set.t) : t -> t =
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
    let rec inline (known:Variable.Set.t) (st:SubstAssoc.t) (i:t) : t =
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
      | Sync l -> Sync l
      | Acc (x,e) -> Acc (x, a_subst st e)
      | Skip -> Skip
      | If (b, p1, p2) ->
        let b = b_subst st b in
        If (b, inline known st p1, inline known st p2)
      | Decl ({var=x; init=Some n; _}, p) ->
        let n = n_subst st n in
        let st = SubstAssoc.put st x n  in
        inline known st p
      | Decl ({init=None; _} as d, p) ->
        Decl (d, inline known st p)
      | For (r, p) ->
        let r = r_subst st r in
        let (x, known, st) = add_var r.var in
        For ({r with var = x}, inline known st p)
      | Seq (p1, p2) ->
        Seq (inline known st p1, inline known st p2)
    in
    fun p ->
      p
      |> vars_distinct
      |> inline known (SubstAssoc.make [])


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

let unknown_range (x:Variable.t) : Range.t =
  Range.{
    var=Variable.from_name "?";
    dir=Increase;
    lower_bound=Num 1;
    upper_bound=Var x;
    step=Step.plus (Num 1);
    ty=C_type.int;
  }

type stateful = (int * Variable.Set.t) -> int * Variable.Set.t * Post.t

let imp_to_post : Variable.Set.t * Stmt.t -> Variable.Set.t * Post.t =
  let unknown (x:int) : Variable.t =
    Variable.from_name ("__loop_" ^ string_of_int x)
  in
  let ret (p:Post.t) : stateful =
    fun (curr_id, globals) ->
      (curr_id, globals, p)
  in
  let bind (f:stateful) (g:Post.t -> stateful) : stateful =
    fun (curr_id, globals) ->
    let (curr_id, globals, s1) = f (curr_id, globals) in
    g s1 (curr_id, globals)
  in
  let rec imp_to_post_s : Stmt.t -> (int * Variable.Set.t) -> int * Variable.Set.t * Post.t =
    function
    | Sync l -> ret (Post.Sync l)
    | Write e -> ret (Acc (e.array, {index=e.index; mode=Write e.payload}))
    | Read e ->
      fun (curr_id, globals) ->
        let rd = Post.Acc (e.array, {index=e.index; mode=Read}) in
        (curr_id, globals, Decl (Decl.unset ~ty:e.ty e.target, rd))
    | Atomic e ->
      fun (curr_id, globals) ->
        let rd = Post.Acc (e.array, {index=e.index; mode=Atomic e.atomic}) in
        (curr_id, globals, Decl (Decl.unset ~ty:e.ty e.target, rd))
    | Call _ -> imp_to_post_p []
    | Block p -> imp_to_post_p p
    | If (b, s1, s2) ->
      bind (imp_to_post_p [s1]) (fun s1 ->
        bind (imp_to_post_p [s2]) (fun s2 ->
          ret (Post.If (b, s1, s2))
        )
      )
    | For (r, s) ->
      bind (imp_to_post_p [s]) (fun s -> ret (Post.For (r, s)))
    | Star s ->
      let synchronized = Stmt.has_sync s in
      bind (imp_to_post_p [s]) (fun s (curr_id, globals) ->
        let x = unknown curr_id in
        let r = unknown_range x in
        let s : Post.t = For (r, s) in
        if synchronized then
          (curr_id + 1, Variable.Set.add x globals, s)
        else
          (curr_id, globals, Decl (Decl.unset x, s))
      )
    (* Handled in the context of a prog *)
    | Assert _ -> failwith "unsupported"
    | LocationAlias _ -> failwith "unsupported"
    | Decl _ -> failwith "unsupported"
  and imp_to_post_p : Stmt.prog -> int*Variable.Set.t -> int * Variable.Set.t * Post.t =
    function
    | [] -> ret Skip
    | Assert b :: p ->
      bind (imp_to_post_p p) (fun p ->
        ret (Post.If (b, p, Skip))
      )
    | LocationAlias e :: p ->
      bind (imp_to_post_p p) (fun p ->
       ret (Post.loc_subst e p)
      )
    | Decl [] :: p -> imp_to_post_p p
    | Decl (d::l) :: p ->
      bind (imp_to_post_p (Decl l :: p)) (fun s ->
        ret (Post.Decl (d, s))
      )
    | s :: p ->
      bind (imp_to_post_s s) (fun s ->
        bind (imp_to_post_p p) (fun p ->
          ret (Seq (s, p))
        )
      )
  in
  fun (globals, s) ->
    let (_, globals, p) = imp_to_post_s (Block [s]) (1, globals) in
    (globals, p)

let rec post_to_proto : Post.t -> Proto.Code.t =
  let open Post in
  function
  | Sync l -> Sync l
  | Acc (x,e) -> Acc (x, e)
  | Skip -> Skip
  | If (b, p1, p2) ->
    Proto.Code.seq
      (Proto.Code.cond b (post_to_proto p1))
      (Proto.Code.cond (b_not b) (post_to_proto p2))
  | For (r, p) ->
    Loop (r, post_to_proto p)
  | Decl ({init=Some _; _}, _) as i ->
    failwith ("Run inline_decl first: " ^ Post.to_string i)
  | Decl ({var=x; init=None; ty}, p) -> Proto.Code.decl ~ty x (post_to_proto p)
  | Seq (i, p) ->
    Proto.Code.seq (post_to_proto i) (post_to_proto p)

module Kernel = struct
  type t = {
    (* The kernel name *)
    name: string;
    (* The type signature of the kernel *)
    ty: string;
    (* The shared locations that can be accessed in the kernel. *)
    arrays: Memory.t Variable.Map.t;
    (* The internal variables are used in the code of the kernel.  *)
    params: Variable.Set.t;
    (* The code of a kernel performs the actual memory accesses. *)
    code: Stmt.t;
    (* Visibility *)
    visibility: Proto.Kernel.visible;
  }

  (* Generate a unique id that pairs the name and type. *)
  let unique_id (k:t) : string =
    kernel_id ~kernel:k.name ~ty:k.ty

  let to_s (k:t) : Indent.t list =
    [
      Line (
        k.name ^
        " (" ^ Memory.map_to_string k.arrays ^ ", " ^
        Variable.set_to_string k.params ^ ") {");
      Block (Stmt.to_s k.code);
      Line "}"
    ]

  let print (k: t) : unit =
    Indent.print (to_s k)

  let compile (arch:Architecture.t) (k:t) : Proto.Code.t Proto.Kernel.t =
    let globals = k.params in
    let (globals, p) = imp_to_post (globals, k.code) in
    let p : Post.t =
      p
      |> Post.filter_locs k.arrays (* Remove unknown arrays *)
      (* Inline local variable assignment and ensure variables are distinct*)
      |> Post.inline_assigns k.params
    in
    let p = post_to_proto p in
    let (p, locals, pre) =
      let rec inline_header :
        (Proto.Code.t * Variable.Set.t * bexp)
        ->
        (Proto.Code.t * Variable.Set.t * bexp)
      =
        fun (p, locals, pre) ->
        match p with
        | Cond (b, p) -> inline_header (p, locals, b_and b pre)
        | Decl {var=x; body=p; _} -> inline_header (p, Variable.Set.add x locals, pre)
        | _ -> (p, locals, pre)
      in
      inline_header (p, Variable.Set.empty, Bool true)
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
    } |> Proto.Kernel.apply_arch arch

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
      | Sync _ | Assert _ | Read _ | Write _ | Atomic _ | Decl _ | LocationAlias _ ->
        s
      | Block l -> Block (List.map inline l)
      | If (b, s1, s2) -> If (b, inline s1, inline s2)
      | For (r, s) -> For (r, inline s)
      | Star s -> Star (inline s)
    in
    { k with code = inline k.code }
end

let string_set (s:StringSet.t) =
    "[" ^ (StringSet.elements s |> String.concat ", ") ^ "]"

module Inliner = struct
  type t = {
    kernels: Kernel.t StringMap.t; (* Kernel name to kernel *)
    targets: StringSet.t StringMap.t; (* For each kernel which other kernels it is calling *)
    visited: StringSet.t;
  }

  let key_set (s:'a StringMap.t) : StringSet.t =
    s
    |> StringMap.bindings
    |> List.map fst
    |> StringSet.of_list

  let to_string (s:t) : string =
    "{\n" ^
    "\tkernels = " ^ (key_set s.kernels |> string_set) ^ "\n" ^
    "\ttargets = " ^ (String.concat ", " (s.targets |> StringMap.bindings |> List.map (fun (k,v) -> k ^"=" ^ string_set v))) ^ "\n" ^
    "\tvisited = " ^ string_set s.visited ^ "\n" ^
    "}"

  let inline_kernels (kernels:StringSet.t) (s:t) : t =
    (* Get the code of the kernels to call *)
    let leaves =
      StringMap.filter (fun k _ -> StringSet.mem k kernels) s.kernels
    in
    let leaf_set = key_set leaves in
    (* Get the set of all kernels that call `kernel` *)
    let to_inline : StringSet.t =
      s.targets
        |> StringMap.filter (fun _ x ->
        (* any kernel that depends on a leaf *)
        not (StringSet.is_empty (StringSet.inter leaf_set x))
      )
      |> key_set
    in
    {
      (* inline each call to a leaf *)
      kernels = StringMap.mapi (fun name k ->
        (* if this kernel calls any of the leaves *)
        if StringSet.mem name to_inline then
          (* Inline leaves in k *)
          Kernel.inline leaves k
        else
          (* nothing to do, leave kernel as is *)
          k
      ) s.kernels;
      (* remove the leaves from all dependencies *)
      targets =
        StringMap.map (fun s -> StringSet.diff s leaf_set) s.targets
      ;
      (* add leaves to the set of all visited *)
      visited = StringSet.union leaf_set s.visited;
    }

  (* Calculate the set of next possible kernels to inline *)
  let next (s:t) : StringSet.t =
    let possible =
      s.targets
      |> StringMap.filter (fun _ ts -> StringSet.is_empty ts)
      |> key_set
    in
    StringSet.diff possible s.visited

  let from_list (ks:Kernel.t list) : t =
    {
      targets =
        ks
        |> List.map (fun k -> (Kernel.unique_id k, Kernel.calls k))
        |> StringMapUtil.from_list
      ;
      kernels =
        ks
        |> List.map (fun k -> (Kernel.unique_id k, k))
        |> StringMapUtil.from_list
      ;
      visited = StringSet.empty;
    }

  let kernel_list (s:t) : Kernel.t list =
    s.kernels
    |> StringMap.bindings
    |> List.map snd

  let rec inline_all (s:t) : t =
    let n = next s in
    if StringSet.is_empty n then
      (* we are done *)
      s
    else
      (* inline more *)
      inline_all (inline_kernels n s)
end

let inline_calls (l:Kernel.t list) : Kernel.t list =
  l
  |> Inliner.from_list
  |> Inliner.inline_all
  |> Inliner.kernel_list

