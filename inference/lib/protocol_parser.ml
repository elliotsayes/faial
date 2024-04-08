open Stage0
open Protocols


type imp_kernel = Imp.Kernel.t
type proto_kernel = Proto.Code.t Proto.Kernel.t

type 'a t = {options: Gv_parser.t; kernels: 'a list}

module Make (L:Logger.Logger) = struct
  module D = D_to_imp.Make(L)

  let to_imp
    ?(abort_on_parsing_failure=true)
    ?(block_dim=None)
    ?(grid_dim=None)
    ?(includes=[])
    ?(exit_status=2)
    (fname:string)
  :
    imp_kernel t
  =
    let j = Cu_to_json.cu_to_json
      ~ignore_fail:(not abort_on_parsing_failure)
      ~on_error:(fun _ -> exit exit_status)
      ~includes
      fname
    in
    let options : Gv_parser.t = match Gv_parser.parse fname with
      | Some gv ->
        Logger.Colors.info ("Found GPUVerify args in source file: " ^ Gv_parser.to_string gv);
        gv
      | None -> Gv_parser.make ()
    in
    (* Override block_dim/grid_dim if they user provided *)
    let options = { options with
      block_dim = (match block_dim with
      | Some b -> b
      | None -> options.block_dim
      );
      grid_dim = (match grid_dim with
      | Some g -> g
      | None -> options.grid_dim
      );
    }
    in
    match C_lang.parse_program j with
    | Ok k1 ->
      let k2 = D_lang.rewrite_program k1 in
        (match D.parse_program k2 with
        | Ok kernels ->
          Stdlib.flush_all ();
          {options; kernels}
        | Error e ->
          C_lang.Program.print k1;
          print_endline "------";
          D_lang.Program.print k2;
          print_endline "-------";
          D_to_imp.print_error e;
          exit(exit_status)
        )

    | Error e ->
      Rjson.print_error e;
      exit(exit_status)

  let to_proto
    ?(abort_on_parsing_failure=true)
    ?(block_dim=None)
    ?(grid_dim=None)
    ?(includes=[])
    ?(exit_status=2)
    ?(inline_calls=true)
    ?(only_globals=true)
    (fname:string)
  :
    proto_kernel t
  =
    let parsed =
      to_imp
      ~abort_on_parsing_failure
      ~block_dim
      ~grid_dim
      ~includes
      ~exit_status
      fname
    in
    { parsed with
      kernels =
      parsed.kernels
      |> (if inline_calls then Imp.Inline_calls.inline_calls else fun x -> x)
      |> List.map Imp.Kernel.compile
      |> List.filter (fun k ->
        not only_globals || (only_globals && Proto.Kernel.is_global k)
      )
    }
end

module Default = Make(Logger.Colors)

module Silent = Make(Logger.Silent)
