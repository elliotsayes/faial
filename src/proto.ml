open Exp

(* Access expression *)
type acc_expr = variable * access

(* The source instruction uses the base defined above *)
type inst =
  | Acc of acc_expr
  | Sync
  | Cond of bexp * inst list
  | Loop of range * inst list
(* The source program *)
type prog = inst list

type 'a kernel = {
  (* The shared locations that can be accessed in the kernel. *)
  kernel_locations: VarSet.t;
  (* The internal variables are used in the code of the kernel.  *)
  kernel_global_variables: VarSet.t;
  (* The internal variables are used in the code of the kernel.  *)
  kernel_local_variables: VarSet.t;
  (* A thread-local pre-condition that is true on all phases. *)
  kernel_pre: bexp;
  (* The code of a kernel performs the actual memory accesses. *)
  kernel_code: 'a;
}
