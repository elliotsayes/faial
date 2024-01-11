open Protocols

module Code = struct
  type t =
    | Decl of Variable.t * t
    | Loop of Range.t * t
    | Cond of Exp.bexp * t
    | Barrier

  let rec from_proto : Proto.Code.t -> t Seq.t =
    function
    | Skip
    | Acc _ -> Seq.empty
    | Sync -> Seq.return Barrier
    | Decl (x, s) -> from_proto s |> Seq.map (fun s -> Decl (x, s))
    | Cond (b, s) -> from_proto s |> Seq.map (fun s -> Cond (b, s))
    | Seq (s1, s2) -> from_proto s1 |> Seq.append (from_proto s2)
    | Loop (r, s) -> from_proto s |> Seq.map (fun s -> Loop (r, s))
(*
  let n_is_uniform (thread_locals:Variable.Set.t) (e:Exp.nexp) : bool =
    let fns = Freenames.free_names_nexp e Variable.Set.empty in
    Variable.Set.inter fns thread_locals |> Variable.Set.is_empty
*)
  let b_is_uniform (thread_locals:Variable.Set.t) (e:Exp.bexp) : bool =
    let fns = Freenames.free_names_bexp e Variable.Set.empty in
    Variable.Set.inter fns thread_locals |> Variable.Set.is_empty

  let r_is_uniform (thread_locals:Variable.Set.t) (e:Range.t) : bool =
    let fns = Freenames.free_names_range e Variable.Set.empty in
    Variable.Set.inter fns thread_locals |> Variable.Set.is_empty

  let rec is_uniform (thread_locals:Variable.Set.t) : t -> bool =
    function
    | Barrier -> true
    | Decl (x, s) -> is_uniform (Variable.Set.add x thread_locals) s
    | Loop (r, s) -> r_is_uniform thread_locals r && is_uniform thread_locals s
    | Cond (b, s) -> b_is_uniform thread_locals b && is_uniform thread_locals s
end

module Kernel = struct
  type t = {
    (* The kernel name *)
    name : string;
    (* The internal variables are used in the code of the kernel.  *)
    global_variables: Variable.Set.t;
    (* The internal variables are used in the code of the kernel.  *)
    local_variables: Variable.Set.t;
    (* The code of a kernel performs the actual memory accesses. *)
    code: Code.t list;
  }

  let from_proto (k : Proto.Code.t Proto.Kernel.t) : t =
    {
      name = k.name;
      global_variables = k.global_variables;
      local_variables = k.local_variables;
      code = Code.from_proto k.code |> List.of_seq;
    }

  let is_uniform (k:t) : bool =
    let vars = Variable.Set.union k.local_variables Variable.tid_var_set in
    List.for_all (Code.is_uniform vars) k.code
end
