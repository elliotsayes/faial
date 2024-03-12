open Stage0
open Z3_solver
open Protocols
open Drf

let render (output: Analysis.t list) : unit =
  let kernels =
    output
    |> List.map (fun analysis ->
      let open Analysis in
      let kernel_name = analysis.kernel.name in
      let solutions = analysis.report in
      let unknowns, errors =
        solutions
        |> List.filter_map (
          let open Z3_solver.Solution in
          function
          | _, Drf -> None
          | p, Unknown -> Some (Either.Left p)
          | p, Racy w -> Some (Either.Right (p, w))
        )
        |> Common.either_split
      in
      let approx_analysis (w:Witness.t) =
        let dd = if Variable.Set.cardinal w.data_approx > 0 then "DD" else "DI" in
        let cd = if Variable.Set.cardinal w.control_approx > 0 then "CD" else "CI" in
        cd ^ dd
      in
      let is_ok = (List.length unknowns + List.length errors) = 0 in
      `Assoc [
        "kernel_name", `String kernel_name;
        "status", `String (if is_ok then "drf" else "racy");
        "unknowns", `List (List.map Symbexp.Proof.to_json unknowns);
        "errors", `List (List.map (fun (p, w) ->
          `Assoc [
            "summary", Symbexp.Proof.to_json p;
            "counter_example", Witness.to_json w;
            "approx_analysis", `String (approx_analysis w);
          ]
        ) errors);
      ]
    )
  in
  `Assoc [
    "kernels", `List kernels;
    "argv", `List (Sys.argv |> Array.to_list |> List.map (fun x -> `String x));
    "executable_name", `String Sys.executable_name;
    "z3_version", `String (Z3.Version.to_string);
  ]
  |> Yojson.Basic.to_string
  |> print_endline
