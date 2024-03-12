open Protocols
open Drf

type t = {
  kernel: Proto.Code.t Proto.Kernel.t;
  report: (Symbexp.Proof.t * Z3_solver.Solution.t) list;
}
