open Protocols

type t = {
  kernel: Proto.Code.t Proto.Kernel.t;
  report: Z3_solver.Solution.t list;
}

let is_safe (a:t) : bool =
  a.report
  |> List.for_all Z3_solver.Solution.is_safe
