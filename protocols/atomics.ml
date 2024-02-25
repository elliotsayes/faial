let atomic_inc : Variable.t = Variable.from_name "atomicInc"

let list : Variable.t list =
  [
    "atomicInc";
    "atomicInc_block";
    "atomicInc_system";

    "atomicDec";
    "atomicDec_block";
    "atomicDec_system";

    "atomicAdd";
    "atomicAdd_block";
    "atomicAdd_system";

    "atomicCAS";
    "atomicCAS_block";
    "atomicCAS_system";

    "atomicAnd";
    "atomicAnd_block";
    "atomicAnd_system";

    "atomicOr";
    "atomicOr_block";
    "atomicOr_system";

    "atomicXor";
    "atomicXor_block";
    "atomicXor_system";

    "atomicMin";
    "atomicMin_block";
    "atomicMin_system";

    "atomicMax";
    "atomicMax_block";
    "atomicMax_system";

    "atomicExch";
    "atomicExch_block";
    "atomicExch_system";

    "atomicSub";
    "atomicSub_block";
    "atomicSub_system";
  ]
  |> List.map Variable.from_name

let set : Variable.Set.t =
  list
  |> Variable.Set.of_list

let is_atomic (x:Variable.t) : bool =
  Variable.Set.mem x set
