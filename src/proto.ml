open Exp

type 'a  base_inst =
  | Base of 'a
  | Cond of bexp * 'a  base_inst list
  | Loop of range * 'a  base_inst list

(* Changes the base of a base instruction *)
let rec base_inst_map (f: 'a -> 'b) : 'a base_inst -> 'b base_inst =
  function
  | Base a -> Base (f a)
  | Cond (b, l) -> Cond (b, List.map (base_inst_map f) l)
  | Loop (r, l) -> Loop (r, List.map (base_inst_map f) l)

(* A base program is a list of base instructions *)
type 'a base_prog = ('a base_inst) list

(* Change the base of a program *)
let base_prog_map (f: 'a -> 'b) : 'a base_prog -> 'b base_prog =
  List.map (base_inst_map f)

(* Regular access expression *)
type expr_acc = (variable * access)

(* A simple instruction *)
type 'a a_inst =
  | Goal of bexp
  | Acc of 'a

(* A simple instruction with regular accesses *)
type acc_inst = expr_acc a_inst

(* In a regular program the base is either a barrier or an unsync *)
type sync_unsync =
  | Sync
  | Unsync of acc_inst

(* The source instruction uses the base defined above *)
type inst = sync_unsync base_inst
(* The source program *)
type prog = inst list

(* The unsynchronized fragment *)
type u_inst = acc_inst base_inst
type u_prog = u_inst list

(* The synchronized fragment (phased) *)
type s_inst = u_prog base_inst
type s_prog = s_inst list

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
