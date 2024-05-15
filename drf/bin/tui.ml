open Z3_solver
open Protocols
open Stage0

module T = ANSITerminal

(*
  Renders the local state as a PrintBox
  *)
module LocalState = struct
  type t = {
    ident: string;
    control_dependent: bool;
    data_dependent: bool;
    state: string * string;
  }

  let parse_structs (w:Witness.t) : t list =
    let t1_s = Environ.parse_structs (fst w.tasks).locals in
    let t2_s = Environ.parse_structs (snd w.tasks).locals in
    (* Merge both maths, so we get identifier to struct, where a struct
       is a map from field to value. A value is a pair for each thread's local
       state *)
    StringMap.merge (fun _ v1 v2 ->
    match v1, v2 with
    | Some v, None -> Some (v, StringMap.empty)
    | None, Some v -> Some (StringMap.empty, v)
    | Some v1, Some v2 -> Some (v1, v2)
    | None, None -> None
    ) t1_s t2_s
    |> StringMap.bindings
    (* At this point we have the map of structs *)
    |> List.map (fun (ident, (s1, s2)) ->
      (* Calculuate the local variables, of a particular struct *)
      let vars (s:string StringMap.t) : Variable.Set.t =
        s
        |> StringMap.bindings
        |> List.map (fun (field, _) ->
          ident ^ "." ^ field
          |> Variable.from_name
        )
        |> Variable.Set.of_list
      in
      let all_vars = Variable.Set.union (vars s1) (vars s2) in
      let is_in (vs:Variable.Set.t) : bool =
        Variable.Set.cardinal (Variable.Set.inter vs all_vars) > 0
      in
      let to_string s =
        s
        |> StringMap.bindings
        |> List.sort (fun (k1, _) (k2, _) -> String.compare k1 k2)
        |> List.map (fun (f, v) ->
          f ^ " = " ^ v
        )
        |> String.concat " | "
      in
      {
        ident;
        control_dependent = is_in w.control_approx;
        data_dependent = is_in w.data_approx;
        state=(to_string s1, to_string s2)
      }
    )

  let compare (x1:t) (x2:t) : int =
    String.compare x1.ident x2.ident

  let parse_scalars (w:Witness.t) : t list =
    let (t1, t2) = w.tasks in
    t1.locals
    |> Environ.remove_structs
    |> Environ.variables
    |> List.map (fun (k, v1) ->
      let ident = Option.value ~default:k (Environ.label k t1.locals) in
      let is_in = Variable.Set.mem (Variable.from_name k) in
      {
        ident;
        control_dependent = is_in w.control_approx;
        data_dependent = is_in w.data_approx;
        state = (v1, Environ.get k t2.locals |> Option.value ~default:"?")
      }
    )

  let from_witness (w:Witness.t) : t list =
    parse_structs w
    @
    parse_scalars w

  let render (ls : t) : PrintBox.t array =
    let open PrintBox in
    let is_approx = ls.data_dependent || ls.control_dependent in
    let style = if is_approx then Style.bold else Style.default in
    let ident =
      if is_approx then
        let msg = if ls.control_dependent then "C" else "" in
        let msg = msg ^ (if ls.data_dependent then "D" else "") in
        ls.ident ^ " (" ^ msg ^ ")"
      else
        ls.ident
    in
    let (s1, s2) = ls.state in
    [| text_with_style style ident; text s1; text s2; |]

  let to_print_box (data: t list) : PrintBox.t =
    data
    |> List.sort compare
    |> List.map render
    |> Array.of_list
    |> PrintBox.grid
    |> PrintBox.frame

end

module GlobalState = struct
  module Row = struct
    type t = {
      ident: string;
      control_dependent: bool;
      data_dependent: bool;
      state: string;
    }

    let parse_structs (w:Witness.t) : t list =
      w.globals
      |> Environ.parse_structs
      |> StringMap.bindings
      (* At this point we have the map of structs *)
      |> List.map (fun (ident, s) ->
        (* Calculuate the local variables, of a particular struct *)
        let all_vars : Variable.Set.t =
          s
          |> StringMap.bindings
          |> List.map (fun (field, _) ->
            ident ^ "." ^ field
            |> Variable.from_name
          )
          |> Variable.Set.of_list
        in
        let is_in (vs:Variable.Set.t) : bool =
          Variable.Set.cardinal (Variable.Set.inter vs all_vars) > 0
        in
        let state =
          s
          |> StringMap.bindings
          |> List.sort (fun (k1, _) (k2, _) -> String.compare k1 k2)
          |> List.map (fun (f, v) ->
            f ^ " = " ^ v
          )
          |> String.concat " | "
        in
        {
          ident;
          control_dependent = is_in w.control_approx;
          data_dependent = is_in w.data_approx;
          state=state
        }
      )

    let parse_scalars (w:Witness.t) : t list
    =
      w.globals
      |> Environ.remove_structs
      |> Environ.variables
      |> List.map (fun (k, state) ->
        (* flag whether CI/DI *)
        let is_in = Variable.Set.mem (Variable.from_name k) in
        {
          (* get a nice label, rather than internal id if possible *)
          ident = Option.value ~default:k (Environ.label k w.globals);
          control_dependent = is_in w.control_approx;
          data_dependent = is_in w.data_approx;
          state;
        }
      )

    let from_witness (w:Witness.t) : t list =
      parse_structs w @ parse_scalars w

    let compare (x1:t) (x2:t) : int =
      String.compare x1.ident x2.ident

    let render (ls : t) : PrintBox.t array =
      let open PrintBox in
      let is_approx = ls.data_dependent || ls.control_dependent in
      let style = if is_approx then Style.bold else Style.default in
      let ident =
        if is_approx then
          let msg = if ls.control_dependent then "C" else "" in
          let msg = msg ^ (if ls.data_dependent then "D" else "") in
          ls.ident ^ " (" ^ msg ^ ")"
        else
          ls.ident
      in
      [| text_with_style style ident; text ls.state; |]
  end

  type t = {rows: Row.t list; index: string; state: string }

  let from_witness (w:Witness.t) : t =
    let brackets =
      List.map (fun _ -> "[]") w.indices
      |> Common.join ""
    in
    {
      index = w.array_name ^ brackets;
      state = Common.join " │ " w.indices;
      rows = Row.from_witness w;
    }

  let render_index (s:t) : PrintBox.t array =
    let open PrintBox in
    [| text_with_style Style.bold s.index; text s.state |]

  let to_print_box (s: t) : PrintBox.t =
    let rows =
      s.rows
      |> List.sort Row.compare
      |> List.map Row.render
    in
    render_index s :: rows
    |> Array.of_list
    |> PrintBox.grid
    |> PrintBox.frame

end

let print_box: PrintBox.t -> unit =
  PrintBox_text.output stdout


let render (output: Analysis.t list) : unit =
  let total = ref 0 in
  output
  |> List.iter (fun solution ->
    let kernel_name =
      let open Analysis in
      solution.kernel.name in
    let errors =
      solution.report
      |> List.filter_map (fun s ->
        let open Z3_solver.Solution in
        match s.outcome with
        | Drf -> None
        | Unknown -> Some (Either.Left s.proof)
        | Racy w -> Some (Either.Right (s.proof, w))
      )
    in
    let print_errors errs =
      errs |> List.iteri (fun i (w:Witness.t) ->
        let is_cd = Variable.Set.cardinal w.control_approx > 0 in
        let is_dd = Variable.Set.cardinal w.data_approx > 0 in
        let is_exact = not is_cd && not is_dd in
        let lbl = " (" ^
          (if is_cd then "CD" else "CI") ^
          (if is_dd then "DD" else "DI") ^
          ")"
        in
        T.print_string [T.Bold; T.Foreground T.Blue] ("\n~~~~ Data-race " ^ string_of_int (i + 1) ^ lbl ^ " ~~~~\n\n");
        let (t1, t2) = w.tasks in
        let locs =
          let l = [t1.location; t2.location] |> Common.flatten_opt in
          match l with
          | [x1; x2] when x1 = x2 -> [x1]
          | [x1; x2] when x2 < x1 -> [x2; x1]
          | _ -> l
        in
        (match locs with
        | [x] -> Tui_helper.LocationUI.print x
        | [x1; x2] -> Tui_helper.LocationUI.print2 x1 x2
        | _ -> failwith "??"
        );
        print_endline "";
        T.print_string [T.Bold] ("Globals\n");
        w |> GlobalState.from_witness |> GlobalState.to_print_box |> print_box;
        T.print_string [T.Bold] ("\n\nLocals\n");
        w |> LocalState.from_witness |> LocalState.to_print_box |> print_box;
        (if is_exact then
          T.print_string [T.Bold; T.Underlined; T.Foreground T.Red] ("\nTrue alarm detected!\n")
        else
          ());
        (if is_dd then
          T.print_string [T.Bold; T.Underlined; T.Foreground T.Yellow] ("\nWARNING: potential alarm, index depends on input, see variables with (D).\n")
        else
          ());
        (if is_cd then
          (T.print_string [T.Bold; T.Underlined; T.Foreground T.Yellow] ("\nWARNING: potential alarm, control-flow depends on input, see variables with (C).\n");
          )
        else
          ());
        print_endline "";
        T.print_string [T.Underlined] ("(proof #" ^ string_of_int w.proof_id ^ ")\n");
      );
    in
    match Common.either_split errors with
    | [], [] ->
      T.print_string [T.Bold; T.Foreground T.Green] ("Kernel '" ^ kernel_name ^ "' is DRF!\n")
    | unk, errs ->
      let has_unknown = List.length unk > 0 in
      let errs = List.split errs |> snd in
      let err_count = List.length errs |> string_of_int in
      let dr = "data-race" ^ (if err_count = "1" then "" else "s") in
      T.print_string [T.Bold; T.Foreground T.Red] ("Kernel '" ^ kernel_name ^ "' has " ^ err_count ^ " " ^ dr ^ ".\n");
      print_errors errs;
      if has_unknown then
        T.print_string [T.Foreground T.Red] ("A portion of the kernel was not analyzable. Try to increasing the timeout.\n")
      else ();
      if err_count <> "0" || has_unknown then (
        total := !total + 1;
      ) else
        ()
  )
  ;
  if !total > 0 then
    exit 1
  else
    ()
