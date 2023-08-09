open Stage0
open Protocols
open Exp
open Flatacc

module Code = struct
  type t = CondAccess.t * CondAccess.t
  let make (c1:CondAccess.t) (c2:CondAccess.t) : t option =
    if Access.is_read (c1.access) && Access.is_read (c2.access) then
      None
    else
      Some (c1, c2)

  let from_flat : Flatacc.Code.t -> t Streamutil.stream =
    let rec pair_with (c:CondAccess.t) : Flatacc.Code.t -> t Streamutil.stream =
      function
      | Skip -> Streamutil.empty
      | Cond c2 ->
        (match make c c2 with
        | Some p -> Streamutil.one p
        | None -> Streamutil.empty)
      | Seq (p, q) ->
        let s1 = pair_with c p in
        let s2 = pair_with c q in
        Streamutil.sequence s1 s2
    in
    let rec cond : Flatacc.Code.t -> (CondAccess.t * Flatacc.Code.t) option =
      function
      | Skip -> None
      | Cond a -> Some (a, Skip)
      | Seq (p, q) ->
        (match cond p with
        | Some (c, p) -> Some (c, Seq (p, q))
        | None -> cond q)
    in
    let rec make (f: Flatacc.Code.t) : t Streamutil.stream =
      match cond f with
      | Some (c, f) ->
        let s1 = pair_with c f in
        let s2 = make f in
        Streamutil.sequence s1 s2
      | None -> Streamutil.empty
    in
    make

end

module Kernel = struct
  type t = {
    name: string;
    array_name: string;
    local_variables: Variable.Set.t;
    code: Code.t;
    pre: bexp;
  }
  let from_flat (k:Flatacc.Kernel.t) : t Streamutil.stream =
    k.code
    |> Code.from_flat
    |> Streamutil.map (fun c ->
      {
        name = k.name;
        array_name = k.array_name;
        local_variables = k.local_variables;
        code = c;
        pre = k.pre;
      }
    )
end


