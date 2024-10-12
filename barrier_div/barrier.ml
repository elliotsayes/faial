open Protocols
open Stage0

module Code = struct
  type t =
    | Decl of {ty:C_type.t; var: Variable.t; body: t}
    | Loop of Range.t * t
    | Cond of Exp.bexp * t
    | Barrier of Location.t option

  let rec from_proto : Protocols.Code.t -> t Seq.t =
    function
    | Skip
    | Acc _ -> Seq.empty
    | Sync l -> Seq.return (Barrier l)
    | Decl {ty; var; body=s} ->
      from_proto s |> Seq.map (fun body -> Decl {ty; body; var})
    | If (b, p, q) ->
      Seq.append
        (from_proto p |> Seq.map (fun p -> Cond (b, p)))
        (from_proto q |> Seq.map (fun q -> Cond (Exp.b_not b, q)))
    | Seq (s1, s2) -> from_proto s1 |> Seq.append (from_proto s2)
    | Loop (r, s) -> from_proto s |> Seq.map (fun s -> Loop (r, s))

  let b_is_uniform (thread_locals:Variable.Set.t) (e:Exp.bexp) : bool =
    let fns = Exp.b_free_names e Variable.Set.empty in
    Variable.Set.inter fns thread_locals |> Variable.Set.is_empty

  let r_is_uniform (thread_locals:Variable.Set.t) (e:Range.t) : bool =
    let fns = Range.free_names e Variable.Set.empty in
    Variable.Set.inter fns thread_locals |> Variable.Set.is_empty

  let rec is_uniform (thread_locals:Variable.Set.t) : t -> bool =
    function
    | Barrier _ -> true
    | Decl {var=x; body=s; _} -> is_uniform (Variable.Set.add x thread_locals) s
    | Loop (r, s) -> r_is_uniform thread_locals r && is_uniform thread_locals s
    | Cond (b, s) -> b_is_uniform thread_locals b && is_uniform thread_locals s

  let rec location : t -> Location.t option =
    function
    | Barrier l -> l
    | Decl {body=s; _} | Cond (_, s) | Loop (_, s) -> location s

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

  let from_proto (k : Protocols.Kernel.t) : t =
    {
      name = k.name;
      global_variables = Params.to_set k.global_variables;
      local_variables = Params.to_set k.local_variables;
      code = Code.from_proto k.code |> List.of_seq;
    }

  let divergent (k:t) : Location.t option list =
    let vars = Variable.Set.union k.local_variables Variable.tid_set in
    List.filter_map
      (fun c -> if Code.is_uniform vars c then None else Some (Code.location c))
      k.code

  let is_uniform (k:t) : bool =
    let vars = Variable.Set.union k.local_variables Variable.tid_set in
    List.for_all (Code.is_uniform vars) k.code
end
