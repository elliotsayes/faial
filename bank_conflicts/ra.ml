open Stage0
open Protocols

type t =
  | Skip
  | Tick of int
  | Loop of Range.t * t
  | Seq of t * t

let to_environ (s:t) : Environ.t =
  let rec fvs (s:t) (env:Environ.Fvs.t) : Environ.Fvs.t =
    match s with
    | Skip
    | Tick _ -> env
    | Loop (r, p) ->
      env
      |> fvs p
      |> Environ.Fvs.add_var r.var
      |> Environ.Fvs.add_exp r.lower_bound
      |> Environ.Fvs.add_exp r.upper_bound
      |> Environ.Fvs.add_exp (Range.stride r)
    | Seq (p, q) ->
      fvs p env
      |> fvs q
  in
  Environ.Fvs.empty |> fvs s |> Environ.from_fvs

module Make (S:Subst.SUBST) = struct
  module M = Subst.Make(S)
  let rec subst (s: S.t) : t -> t =
    function
    | Skip -> Skip
    | Tick k -> Tick k
    | Seq (p, q) -> Seq (subst s p, subst s q)
    | Loop (r, p) ->
      let r = M.r_subst s r in
      M.add s r.var (function
        | Some s -> Loop (r, subst s p)
        | None -> Loop (r, p)
      )
end

module PSubstAssoc = Make(Subst.SubstAssoc)
module PSubstPair = Make(Subst.SubstPair)

let subst : (Variable.t * Exp.nexp) -> t -> t = PSubstPair.subst

let rec indent : t -> Indent.t list =
  let open Indent in
  function
  | Tick k -> [Line ("tick(" ^ string_of_int k ^ ");")]
  | Skip -> [Line "skip;"]
  | Seq (p, q) -> indent p @ indent q
  | Loop (r, p) ->
    [
      Line ("foreach (" ^ Range.to_string r ^ ") {");
      Block (indent p);
      Line "}"
    ]

let rec simplify : t -> t =
  function
  | Skip -> Skip
  | Tick 0 -> Skip
  | Tick k -> Tick k
  | Loop (r,p) ->
    (match simplify p with
    | Skip -> Skip
    | p -> Loop (Constfold.r_opt r, simplify p))
  | Seq (p, q) ->
    match simplify p, simplify q with
    | Skip, p
    | p, Skip -> p
    | p, q -> Seq (p, q)

let seq (p:t) (q:t) : t =
  match p, q with
  | Skip, p
  | p, Skip -> p
  | _, _ -> Seq (p, q)

let to_string (x:t) : string =
  indent x |> Indent.to_string

let print (x:t) : unit =
  indent x |> Indent.print

let from_kernel (num_banks:int) (block_dim:Vec3.t) (k: Proto.prog Proto.kernel) : t =
  let shared = Shared_access.shared_memory k.kernel_arrays in
  let idx_analysis : Exp.nexp -> int =
    Index_analysis.analyze num_banks block_dim k.kernel_local_variables
  in
  let rec from_i : Proto.inst -> t =
    function
    | Acc (x, {index=l; _}) ->
      (* Flatten n-dimensional array and apply word size *)
      (match Variable.Map.find_opt x shared with
        | Some v ->
          let e =
            l
            |> Shared_access.byte_count_multiplier v.byte_count
            |> Shared_access.flatten_multi_dim v.dim
          in
          Tick (idx_analysis e)
        | None -> Skip)
    | Sync -> Skip
    | Cond (_, p) -> from_p p
    | Loop (r, p) ->
      let r = Shared_access.uniform block_dim r |> Ojson.unwrap_or r in
      Loop (r, from_p p)

  and from_p (l: Proto.prog) : t =
    List.fold_right (fun i p -> Seq (from_i i, p)) l Skip
  in
  from_p k.kernel_code
