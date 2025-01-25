open Stage0
open Protocols
open Subst

(**
  This code is syntactically limited so that PreCode.seq is a total function
  *)
module PreCode = struct
  type t =
    | Sync of Unsync.t
    | SeqLoop of t * loop
    | Seq of t * t
  and loop = {range: Range.t; body: t}

  module Make (S:SUBST) = struct
    module M = Subst.Make(S)
    module U = Unsync.Make(S)

    let rec subst (s:S.t) (i:t) : t =
      match i with
      | Seq (p, q) -> Seq (subst s p, subst s q)
      | Sync c -> Sync (U.subst s c)
      | SeqLoop (p, {range=r; body=q}) ->
        let q = M.add s r.var (function
          | Some s -> subst s q
          | None -> q
        ) in
        SeqLoop (subst s p, {range=M.r_subst s r; body=q})

  end

  module S1 = Make(SubstPair)
  let subst = S1.subst

  let rec seq (c:Unsync.t) : t -> t =
    function
    | Sync c' -> Sync (Unsync.seq c c')
    | SeqLoop (p, l) -> SeqLoop (seq c p, l)
    | Seq (p, q) -> Seq (seq c p, q)

  let align (w:Sync.t) : t =
    let rec align : Sync.t -> t * Unsync.t =
      function
      | Sync c -> (Sync c, Skip)
      | SeqLoop (c1, {range=r; body=p, c2}) ->
        let (q, c3) = align p in
        let q1 = seq c1 (subst (r.var, Range.first r) q) in
        let c = Unsync.seq c3 c2 in
        let r' = Range.next r in
        let x = r.var in
        let x_dec = Range.prev r in
        (SeqLoop (q1, {range=r'; body=seq (Unsync.subst (x, x_dec) c) q}),
          Unsync.subst (x, Range.lossy_last r) c)
      | Seq (i, p) ->
        let (i, c1) = align i in
        let (q, c2) = align p in
        Seq (i, seq c1 q), c2
    in
    match align w with
      (p, c) -> Seq (Sync c, p)

end

(**
  We first translate using PreCode and then we simplify the data-type.
  *)
module Code = struct
  type t =
    | Sync of Unsync.t
    | Loop of {range: Range.t; body: t}
    | Seq of t * t

  let rec to_s: t -> Indent.t list =
    function
    | Seq (p, q) -> to_s p @ to_s q
    | Sync e -> Unsync.to_s e @ [Line "sync;"]
    | Loop {range=r; body=q} ->
      [
        Line ("foreach* (" ^ Range.to_string r ^ ") {");
        Block (to_s q);
        Line "}"
      ]

  let rec from_pre : PreCode.t -> t =
    function
    | Sync u -> Sync u
    | SeqLoop (p, {range; body}) ->
      Seq (from_pre p, Loop {range; body=from_pre body})
    | Seq (p, q) ->
      Seq (from_pre p, from_pre q)

  let align (w:Sync.t) : t =
    w
    |> PreCode.align
    |> from_pre
end

module Kernel = struct

  type t = {
    (* The kernel name *)
    name : string;
    (* The internal variables are used in the code of the kernel.  *)
    global_variables: Params.t;
    (* The internal variables are used in the code of the kernel.  *)
    local_variables: Params.t;
    (* The modifiers of each array *)
    arrays: Memory.t Variable.Map.t;
    (* A thread-local pre-condition that is true on all phases. *)
    pre: Exp.bexp;
    (* The code of a kernel performs the actual memory accesses. *)
    code: Code.t;
    (* The kernel's visibility *)
    visibility : Visibility.t;
    (* Number of blocks *)
    grid_dim: Dim3.t option;
    (* Number of blocks *)
    block_dim: Dim3.t option;
  }

  let to_s (k:t) : Indent.t list =
    [
      Line ("name: " ^ k.name ^ ";");
      Line ("arrays: " ^ Memory.map_to_string k.arrays ^ ";");
      Line ("globals: " ^ Params.to_string k.global_variables ^ ";");
      Line ("locals: " ^ Params.to_string k.local_variables ^ ";");
      Line ("invariant:");
      Block (Exp.b_to_s k.pre);
      Line ";";
      Line "code:";
      Block (Code.to_s k.code);
      Line "; end of code"
    ]

  let print (k:t) : unit =
    Indent.print (to_s k)


  let from_wellformed (k: Wellformed.Kernel.t) : t =
    {
      name = k.name;
      global_variables = k.global_variables;
      local_variables = k.local_variables;
      arrays = k.arrays;
      pre = k.pre;
      code = Code.align k.code;
      visibility = k.visibility;
      grid_dim = k.grid_dim;
      block_dim = k.block_dim;
    }

end

let translate (ks: Wellformed.Kernel.t Streamutil.stream) : Kernel.t Streamutil.stream =
  Streamutil.map Kernel.from_wellformed ks

let print_kernels (ks : Kernel.t Streamutil.stream) : unit =
  print_endline "; a-lang";
  let count = ref 0 in
  Streamutil.iter (fun k ->
    let curr = !count + 1 in
    count := curr;
    print_endline ("; a-prog " ^ (string_of_int curr));
    Kernel.print k
  ) ks;
  print_endline "; end of a-lang"
