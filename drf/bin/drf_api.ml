open Protocols

let faial_drf_call_wgsl wgsl_json_str =
  let archs = [Architecture.Grid; Architecture.Block] in (* Equivalent to `all_levels` *)
  let app = App.parse
    ~wgsl_json:wgsl_json_str
    ~timeout:None
    ~show_proofs:false
    ~show_proto:false
    ~show_wf:false
    ~show_align:false
    ~show_phase_split:false
    ~show_loc_split:false
    ~show_flat_acc:false
    ~show_symbexp:false
    ~logic:None
    ~ge_index:[]
    ~le_index:[]
    ~eq_index:[]
    ~only_array:None
    ~thread_idx_1:None
    ~thread_idx_2:None
    ~block_idx_1:None
    ~block_idx_2:None
    ~archs
    ~inline_calls:true
    ~ignore_parsing_errors:false
    ~includes:[]
    ~block_dim:None
    ~grid_dim:None
    ~params:[]
    ~only_kernel:None
    ~only_true_data_races:false
    ~macros:[]
    ~cu_to_json:""
    ~all_dims:true (* Important for true DRF analysis *)
    ~ignore_asserts:false
  in
  App.run app |> Rui.render

let () = 
  Callback.register "faial_drf_call_wgsl" faial_drf_call_wgsl
