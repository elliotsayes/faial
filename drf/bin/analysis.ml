open Protocols
open Drf

type t = {
  kernel: Proto.Code.t Proto.Kernel.t;
  report: (Symbexp.Proof.t * Z3_solver.Solution.t) list;
}

let is_safe (a:t) : bool =
  a.report
  |> List.map snd
  |> List.for_all Z3_solver.Solution.is_safe
