open Protocols

type t = {
  kernel: Proto.Code.t Proto.Kernel.t;
  report: Solve_drf.Solution.t list;
}

let is_safe (a:t) : bool =
  a.report
  |> List.for_all Solve_drf.Solution.is_safe
