open Proto
open Sexplib

let check (p:proto) =
  (* 0. Make sure each loop has a synchronization step going on *)
  (* XXX *)
  (* 1. Make sure each loops as a unique variable *)
  let (p, _) = Loops.normalize_variables p in
  let _ = Loops.get_declarations p in
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
  owned_steps

let () =
  let s : Sexp.t = Sexp.input_sexp stdin in
    try
      let elems = Parse.parse_proto.run s
        |> check
        |> Serialize.stream_ser
      in
      match elems with
      | Sexp.Atom x -> print_endline x
      | Sexp.List l ->
        List.iter (fun x ->
          Sexp.to_string_hum x |> print_endline
        ) l
    with
    | Parse.ParseError l ->
      List.iter (fun x ->
        print_endline x
      ) l
