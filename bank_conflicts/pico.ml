open Stage0
open Inference
open Bank_conflicts
open Protocols

(* Main function *)

let cost
  ?(skip_zero=true)
  ?(use_maxima=false)
  ?(use_absynth=false)
  ?(use_cofloco=false)
  ?(use_koat=false)
  ?(explain=true)
  ?(num_banks=32)
  ?(absynth_exe="absynth")
  ?(cofloco_exe="cofloco")
  ?(koat_exe="koat2")
  ?(show_code=false)
  (thread_count:Vec3.t)
  (k : Proto.prog Proto.kernel)
:
  string
=
  let subst x n p =
    Proto.PSubstPair.p_subst (Variable.from_name x, Num n) p in
  let p =
    k.kernel_code
    |> subst "blockDim.x" thread_count.x
    |> subst "blockDim.y" thread_count.y
    |> subst "blockDim.z" thread_count.z
  in
  let render_s ?(show_code=false) (s: Symbolic.t) : string =
    if use_maxima then
      Symbolic.run_maxima ~verbose:show_code s
    else if use_absynth then
      Absynth.run_symbolic ~verbose:show_code ~exe:absynth_exe s
    else if use_cofloco then
      Symbolic.run_cofloco ~verbose:show_code ~exe:cofloco_exe s
    else if use_koat then
      Symbolic.run_koat ~verbose:show_code ~exe:koat_exe s
    else
      Symbolic.simplify s
  in
  let handle_slice =
    if explain then
      Seq.filter_map (fun s ->
        (* Convert a slice into an expression *)
        let s1 = Symbolic.from_slice num_banks thread_count k.kernel_local_variables s in
        if skip_zero && Symbolic.is_zero s1 then
          None
        else Some (
          (* Flatten the expression *)
          let simplified_cost = render_s s1 in
          ANSITerminal.(print_string [Bold; Foreground Blue] ("\n~~~~ Bank-conflict ~~~~\n\n"));
          s |> Shared_access.location |> Tui.LocationUI.print;
          print_endline "";
          let blue = PrintBox.Style.(set_bold true (set_fg_color Blue default)) in
          PrintBox.(
            tree (s |> Shared_access.to_string |> String.cat "▶ Context: " |> text)
            [
              tree ("▶ Cost: "  ^ Symbolic.to_string s1 |> text)
              [
                tree ("▶ Cost (simplified):" |> text_with_style blue)
                [
                  text_with_style blue simplified_cost |> hpad 1
                ]
              ]
            ]
          ) |> PrintBox_text.output stdout;
          print_endline "\n";
          s1
        )
      )
    else
      Seq.map (Symbolic.from_slice num_banks thread_count k.kernel_local_variables)
  in
  (* 1. break a kernel into slices *)
  let total = Shared_access.from_kernel thread_count { k with kernel_code = p }
    |> handle_slice
    |> List.of_seq
    |> Symbolic.add
  in
  let total = render_s ~show_code total in
  PrintBox.(
    text total
    |> hpad 1
    |> frame
  )
  |> PrintBox_text.to_string


let pico
  (fname : string)
  (thread_count:Vec3.t)
  (use_maxima:bool)
  (use_absynth:bool)
  (use_cofloco:bool)
  (use_koat:bool)
  (show_all:bool)
  (explain:bool)
  (show_code:bool)
  (absynth_exe:string)
  (cofloco_exe:string)
  (koat_exe:string)
=
  try
    let parsed_json = Cu_to_json.cu_to_json fname in
    let c_ast = parsed_json |> C_lang.parse_program |> Result.get_ok in
    let d_ast = c_ast |> D_lang.rewrite_program in
    let imp = d_ast |> D_to_imp.Silent.parse_program |> Result.get_ok in
    let proto = imp |> List.map Imp.compile in
    List.iter (fun k ->
      let cost_of_proto =
        cost
          ~explain
          ~use_maxima
          ~use_absynth
          ~use_cofloco
          ~use_koat
          ~show_code
          ~skip_zero:(not show_all)
          ~absynth_exe
          ~cofloco_exe
          ~koat_exe
          thread_count
          k
      in
      print_string (k.kernel_name ^ ":\n");
      cost_of_proto |> print_endline
    ) proto
  with
  | Common.ParseError b ->
      Buffer.output_buffer stderr b;
      exit (-1)

(* Command-line interface *)

open Cmdliner

let get_fname = 
  let doc = "The path $(docv) of the GPU program." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILENAME" ~doc)

let vec3 : Vec3.t Cmdliner.Arg.conv =
  let parse =
    fun s ->
    try
      match Yojson.Basic.from_string s with
      | `List [`Int x; `Int y; `Int z] -> Ok (Vec3.make ~x ~y ~z)
      | `List [`Int x; `Int y] -> Ok (Vec3.make ~x ~y ~z:1)
      | `List [`Int x] | `Int x -> Ok (Vec3.make ~x:x ~y:1 ~z:1)
      | _ -> Error (`Msg "Expecting a number of a list of up to 3 numbers (eg, [x,y,z])")
    with
      _ -> Error (`Msg ("Error parsing vec3"))
  in
  let print : Vec3.t Cmdliner.Arg.printer =
    fun ppf v -> Format.fprintf ppf "%s" (Vec3.to_string v)
  in
  Arg.conv (parse, print)

let thread_count =
  let doc = "Set the number of threads per block.\nExamples:\n--blockDim 1024\n--blockDim [16,16]." in
  Arg.(value & opt vec3 (Vec3.make ~x:1024 ~y:1 ~z:1) & info ["b"; "block-dim"; "blockDim"] ~docv:"BLOCK_DIM" ~doc)

let absynth_exe =
  let doc = "Sets the path to the absynth executable." in
  Arg.(value & opt string "absynth" & info ["absynth-exe"] ~doc)

let cofloco_exe =
  let doc = "Sets the path to the CoFloCo executable." in
  Arg.(value & opt string "cofloco" & info ["cofloco-exe"] ~doc)

let koat_exe =
  let doc = "Sets the path to the KoAT2 executable." in
  Arg.(value & opt string "koat2" & info ["koat-exe"] ~doc)

let use_maxima =
  let doc = "Uses maxima to simplify the cost of each access." in
  Arg.(value & flag & info ["maxima"] ~doc)

let use_absynth =
  let doc = "Uses absynth to simplify the cost of each access." in
  Arg.(value & flag & info ["absynth"] ~doc)

let use_cofloco =
  let doc = "Uses CoFloCo to simplify the cost of each access." in
  Arg.(value & flag & info ["cofloco"] ~doc)

let use_koat =
  let doc = "Uses KoAT2 to simplify the cost of each access." in
  Arg.(value & flag & info ["koat"] ~doc)

let show_all =
  let doc = "By default we skip accesses that yield 0 bank-conflicts." in
  Arg.(value & flag & info ["show-all"] ~doc)

let explain =
  let doc = "Show bank-conflicts per location." in
  Arg.(value & flag & info ["explain"] ~doc)

let show_code =
  let doc = "Show the code being sent to the solver if any." in
  Arg.(value & flag & info ["show-code"] ~doc)

let pico_t = Term.(
  const pico
  $ get_fname
  $ thread_count
  $ use_maxima
  $ use_absynth
  $ use_cofloco
  $ use_koat
  $ show_all
  $ explain
  $ show_code
  $ absynth_exe
  $ cofloco_exe
  $ koat_exe
)

let info =
  let doc = "Static analysis of bank-conflicts for GPU programs" in
  Cmd.info "faial-bc" ~version:"%%VERSION%%" ~doc

let () =
  Cmd.v info pico_t
  |> Cmd.eval
  |> exit
