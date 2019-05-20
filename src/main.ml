open Proto

let translate (p:proto) =
  (* 0. Make sure each loop has a synchronization step going on *)
  (* XXX *)
  (* 1. Make sure each loops as a unique variable *)
  let p = Varnorm.norm p in
  (* 2. Flatten outer loops *)
  let steps = Proto2phaseord.remove_loops p in
  (* 3. Make the owner of each access explicit *)
  let steps = Spmd2binary.split steps in
  (* 4. Perform a constant-fold optimization, to reduce the problem space *)
  let owned_steps = Constfold.stream_opt steps in
  (* 5. Convert a flat owned-accs-program into a Hoare Logic program,
     yielding a formula *)
  (* 6. Flatten out our theory into First-Order Logic with numbers. *)
  (* 7. Generate Z3 *)
  steps
  