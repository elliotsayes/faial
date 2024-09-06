open Stage0
open Protocols

module StackTrace = Common.StackTrace
type json = Yojson.Basic.t
type j_object = Rjson.j_object
type 'a j_result = 'a Rjson.j_result

(* Monadic let *)
let (let*) = Result.bind
(* Monadic pipe *)
let (>>=) = Result.bind

module ScalarKind = struct
  type t =
    | Sint
    | Uint
    | Float
    | Bool
    | AbstractInt
    | AbstractFloat

  let parse (j:json) : t j_result =
    let open Rjson in
    let* n = cast_string j in
    Ok (
      match n with
      | "Sint" -> Sint
      | "Uint" -> Uint
      | "Float" -> Float
      | "Bool" -> Bool
      | "AbstractInt" -> AbstractInt
      | "AbstractFloat" -> AbstractFloat
      | _ -> failwith ("ScalarKind.parse: " ^ n)
    )

  let to_string : t -> string =
    function
    | Sint -> "i"
    | Uint -> "u"
    | Float -> "f"
    | Bool -> "bool"
    | AbstractInt -> "abstract i"
    | AbstractFloat -> "abstract f"

end

module Scalar = struct
  type t = {kind: ScalarKind.t; width: int}

  let to_string (s:t) : string =
    ScalarKind.to_string s.kind ^ string_of_int (8 * s.width)

  let parse (j:json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* kind = with_field "kind" ScalarKind.parse o in
    let* width = with_field "width" cast_int o in
    Ok {kind; width}
end

module VectorSize = struct
  type t =
    | Bi
    | Tri
    | Quad

  let to_int : t -> int =
    function
    | Bi -> 2
    | Tri -> 3
    | Quad -> 4

  let from_int (n:int) : t option =
    match n with
    | 2 -> Some Bi
    | 3 -> Some Tri
    | 4 -> Some Quad
    | _ -> None

  let to_string (s: t) : string =
    to_int s |> string_of_int

  let parse (j:json) : t j_result =
    let open Rjson in
    let* n = cast_int j in
    match from_int n with
    | Some n -> Ok n
    | None -> root_cause "VectorSize.parse: invalid JSON" j
end

module StorageAccess = struct
  type t =
    | ReadWrite
    | ReadOnly
    | WriteOnly

  let to_string : t -> string =
    function
    | ReadWrite -> "read_write"
    | ReadOnly -> "read"
    | WriteOnly -> "write"

  let parse (j:json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* load = with_field "load" cast_bool o in
    let* store = with_field "store" cast_bool o in
    match load, store with
    | true, true -> Ok ReadWrite
    | true, false -> Ok ReadOnly
    | false, true -> Ok WriteOnly
    | false, false -> root_cause ("StorageAccess.parse: false, false") j
end

module AddressSpace = struct
  type t =
    | Function
    | Private
    | WorkGroup
    | Uniform
    | Storage of StorageAccess.t
    | Handle
    | PushConstant

  let to_string : t -> string =
    function
    | Function -> "function"
    | Private -> "private"
    | WorkGroup -> "workgroup"
    | Uniform -> "uniform"
    | Storage a -> "storage, " ^ StorageAccess.to_string a
    | Handle -> ""
    | PushConstant -> "push_constant"

  let parse (j:json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* kind = get_kind o in
    match kind with
    | "Function" -> Ok Function
    | "Private" -> Ok Private
    | "WorkGroup" -> Ok WorkGroup
    | "Uniform" -> Ok Uniform
    | "Storage" ->
      let* a = with_field "access" StorageAccess.parse o in
      Ok (Storage a)
    | "Handle" -> Ok Handle
    | "PushConstant" -> Ok PushConstant
    | _ -> root_cause ("Unknown kind:" ^ kind) j

end

module ImageDimension = struct
  type t =
    | D1
    | D2
    | D3
    | Cube
end

module ImageClass = struct
  type t =
  | Sampled of {
        kind: ScalarKind.t;
        multi: bool;
    }
  | Depth of {multi: bool}
  | Storage of {
        format: string;
        access: StorageAccess.t;
    }
end

module ArraySize = struct
  type t =
    | Constant of int
    | Dynamic
end

module Interpolation = struct
  type t = string
end

module Sampling = struct
  type t = string
end

module BuiltIn = struct
  type t =
    | PositionInvariant
    | PositionVariant
    | ViewIndex
    | InstanceIndex
    | VertexIndex
    | FragDepth
    | FrontFacing
    | PrimitiveIndex
    | SampleIndex
    | SampleMask
    | GlobalInvocationId
    | LocalInvocationId
    | LocalInvocationIndex
    | WorkGroupId
    | NumWorkGroups
    | NumSubgroups
    | SubgroupId
    | SubgroupSize
    | SubgroupInvocationId

  let parse (j:json) : t j_result =
    let open Rjson in
    let* name = cast_string j in
    match name with
    | "PositionInvariant" -> Ok PositionInvariant
    | "PositionVariant" -> Ok PositionVariant
    | "ViewIndex" -> Ok ViewIndex
    | "InstanceIndex" -> Ok InstanceIndex
    | "VertexIndex" -> Ok VertexIndex
    | "FragDepth" -> Ok FragDepth
    | "FrontFacing" -> Ok FrontFacing
    | "PrimitiveIndex" -> Ok PrimitiveIndex
    | "SampleIndex" -> Ok SampleIndex
    | "SampleMask" -> Ok SampleMask
    | "GlobalInvocationId" -> Ok GlobalInvocationId
    | "LocalInvocationId" -> Ok LocalInvocationId
    | "LocalInvocationIndex" -> Ok LocalInvocationIndex
    | "WorkGroupId" -> Ok WorkGroupId
    | "NumWorkGroups" -> Ok NumWorkGroups
    | "NumSubgroups" -> Ok NumSubgroups
    | "SubgroupId" -> Ok SubgroupId
    | "SubgroupSize" -> Ok SubgroupSize
    | "SubgroupInvocationId" -> Ok SubgroupInvocationId
    | _ -> root_cause ("Unknown kind: " ^ name) j


  let to_string : t -> string =
    function
    | VertexIndex -> "vertex_index"
    | InstanceIndex -> "instance_index"
    | PositionVariant -> "position"
    | PositionInvariant -> "position_invariant"
    | FrontFacing -> "front_facing"
    | FragDepth -> "frag_depth"
    | LocalInvocationId -> "local_invocation_id"
    | LocalInvocationIndex -> "local_invocation_index"
    | GlobalInvocationId -> "global_invocation_id"
    | WorkGroupId -> "workgroup_id"
    | NumWorkGroups -> "num_workgroups"
    | SampleIndex -> "sample_index"
    | SampleMask -> "sample_mask"
    | PrimitiveIndex -> "primitive_index"
    | ViewIndex -> "view_index"
    | NumSubgroups -> "num_subgroups"
    | SubgroupId -> "subgroup_id"
    | SubgroupSize -> "subgroup_size"
    | SubgroupInvocationId -> "subgroup_invocation_id"
end

module Binding = struct
  type t =
    | BuiltIn of BuiltIn.t
    | Location of {
          location: int;
          second_blend_source: bool;
          interpolation: Interpolation.t option;
          sampling: Sampling.t option;
      }

  let parse (j:json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* kind = get_kind o in
    match kind with
    | "BuiltIn" ->
      let* b = with_field "value" BuiltIn.parse o in
      Ok (BuiltIn b)
    | "Location" ->
      failwith "Binding.parse: location"
    | _ ->
      root_cause ("Unsupported kind: " ^ kind) j

  let to_string : t -> string =
    function
    | BuiltIn b -> "builtin(" ^ BuiltIn.to_string b ^ ")"
    | Location _ -> failwith "Binding.to_string: Location"
end

module Type = struct

  type t = {
      name: string option;
      inner: inner;
    }
  and inner =
    | Scalar of Scalar.t
    | Vector of {
        size: VectorSize.t;
        scalar: Scalar.t;
      }
    | Matrix of {
        columns: VectorSize.t;
        rows: VectorSize.t;
        scalar: Scalar.t;
      }
    | Atomic of Scalar.t
    | Pointer of {
        base: t;
        space: AddressSpace.t;
      }
    | ValuePointer of {
        size: VectorSize.t option;
        scalar: Scalar.t;
        space: AddressSpace.t;
      }
    | Array of {
        base: t;
        size: int option;
      }
    | Struct of {
        members: struct_member list;
        span: int;
      }
    | Image of {
        dim: ImageDimension.t;
        arrayed: bool;
        image_class: ImageClass.t;
      }
    | Sampler of {comparison: bool}
    | AccelerationStructure
    | RayQuery
    | BindingArray of {
        base: t;
        size: ArraySize.t;
      }
    and struct_member = {
        name: string option;
        ty: t;
        binding: Binding.t option;
        offset: int;
      }

    let kind : inner -> string =
      function
      | Scalar _ -> "Scalar"
      | Vector _ -> "Vector"
      | Matrix _ -> "Matrix"
      | Atomic _ -> "Atomic"
      | Pointer _ -> "Pointer"
      | ValuePointer _ -> "ValuePointer"
      | Array _ -> "Array"
      | Struct _ -> "Struct"
      | Image _ -> "Image"
      | Sampler _ -> "Sampler"
      | AccelerationStructure -> "AccelerationStructure"
      | RayQuery -> "RayQuery"
      | BindingArray _ -> "BindingArray"

    let rec inner_to_string : inner -> string =
      function
      | Scalar s ->
        Scalar.to_string s
      | Array a ->
        let size =
          match a.size with
          | Some i -> ", " ^ string_of_int i
          | None -> ""
        in
        "array<" ^ to_string a.base ^ size ^ ">"
      | Vector v ->
        "vec" ^ VectorSize.to_string v.size ^ "<" ^ Scalar.to_string v.scalar ^ ">"
      | k -> failwith ("inner_to_string: unsupported kind:" ^ kind k)

  and to_string (e:t) : string =
    let name =
      match e.name with
      | Some n -> n ^ "#"
      | None -> ""
    in
    name ^ inner_to_string e.inner

  let rec parse (j:json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* name = with_field "name" (cast_option cast_string) o in
    let* inner = with_field "inner" inner_parse o in
    Ok {name; inner}

  and inner_parse (j:json) : inner j_result =
    let open Rjson in
    let* o = cast_object j in
    let* kind = get_kind o in
    match kind with
    | "Array" ->
      let* base = with_field "base" parse o in
      let* size = with_field "size" (cast_option cast_int) o in
      Ok (Array {base; size})
    | "Scalar" ->
      let* s = with_field "value" Scalar.parse o in
      Ok (Scalar s)
    | "Vector" ->
      let* size = with_field "size" VectorSize.parse o in
      let* scalar = with_field "scalar" Scalar.parse o in
      Ok (Vector {size; scalar})

    | _ -> root_cause ("inner_parse: unsupported kind: " ^ kind) j

end

module FunctionArgument = struct
  type t = {
    name: string;
    ty: Type.t;
    binding: Binding.t option
  }

  let parse (j:json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* name = with_field "name" cast_string o in
    let* ty = with_field "ty" Type.parse o in
    let* binding = with_field "binding" (cast_option Binding.parse) o in
    Ok {ty; binding; name}

  let to_string (e:t) : string =
    let binding =
      e.binding
      |> Option.map (fun b -> "@" ^ Binding.to_string b ^ " ")
      |> Option.value ~default:""
    in
    binding ^  e.name ^ ": " ^ Type.to_string e.ty

end

module FunctionResult = struct
  type t = {
    ty: Type.t;
    binding: Binding.t option;
  }

  let parse (j:json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* ty = with_field "ty" Type.parse o in
    let* binding = with_field "binding" (cast_option Binding.parse) o in
    Ok {ty; binding}

end

module Statement = struct
  type t =
(*     | Emit(Range<Expression>), *)
    | Block of t list
    | If of {
(* TODO:         condition: Expression; *)
        accept: t list;
        reject: t list;
      }
    | Switch (*of {
        selector: Expression;
        cases: SwitchCase;
      }*)
    | Loop of {
        body: t list;
        continuing: t list;
(*         break_if: Option<Handle<Expression>>, *)
      }
    | Break
    | Continue
    | Return (*of {
         value: Option<Handle<Expression>>
      }*)
    | Kill
    | Barrier (*Barrier*)
    | Store (*of {
        pointer: Handle<Expression>,
        value: Handle<Expression>,
      }*)
    | ImageStore (* {
        image: Handle<Expression>,
        coordinate: Handle<Expression>,
        array_index: Option<Handle<Expression>>,
        value: Handle<Expression>,
      } *)
    | Atomic (* {
        pointer: Handle<Expression>,
        fun: AtomicFunction,
        value: Handle<Expression>,
        result: Option<Handle<Expression>>,
      } *)
    | WorkGroupUniformLoad (*{
        pointer: Handle<Expression>,
        result: Handle<Expression>,
      }*)
    | Call (* {
        function: Handle<Function>,
        arguments: Vec<Handle<Expression>>,
        result: Option<Handle<Expression>>,
      }*)
    | SubgroupBallot (* {
        result: Handle<Expression>,
        predicate: Option<Handle<Expression>>,
      }*)
    | SubgroupGather (*{
        mode: GatherMode,
        argument: Handle<Expression>,
        result: Handle<Expression>,
      }*)
    | SubgroupCollectiveOperation (*{
        op: SubgroupOperation,
        collective_op: CollectiveOperation,
        argument: Handle<Expression>,
        result: Handle<Expression>,
      }*)

  let rec parse (j:json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* kind = get_kind o in
    match kind with
      | "Block" ->
        let* l = with_field "block" (cast_map parse) o in
        Ok (Block l)
      | "If" ->
        let* accept = with_field "accept" (cast_map parse) o in
        let* reject = with_field "reject" (cast_map parse) o in
        Ok (If {accept; reject})
      | "Switch" -> Ok Switch
      | "Loop" ->
        let* body = with_field "body" (cast_map parse) o in
        let* continuing = with_field "continuing" (cast_map parse) o in
        Ok (Loop {body; continuing})
      | "Break" -> Ok Break
      | "Continue" -> Ok Continue
      | "Return" -> Ok Return
      | "Kill" -> Ok Kill
      | "Barrier" -> Ok Barrier
      | "Store" -> Ok Store
      | "ImageStore" -> Ok ImageStore
      | "Atomic" -> Ok Atomic
      | "WorkGroupUniformLoad" -> Ok WorkGroupUniformLoad
      | "Call" -> Ok Call
      | "SubgroupBallot" -> Ok SubgroupBallot
      | "SubgroupGather" -> Ok SubgroupGather
      | "SubgroupCollectiveOperation" -> Ok SubgroupCollectiveOperation
      | _ -> failwith kind

  let rec to_s : t -> Indent.t list =
    function
    | Block l ->
      [
        Line "{";
        Block (block_to_s l);
        Line "}"
      ]
    | If {accept; reject; } ->
      let open Indent in
      [
(* TODO:         condition: Expression; *)
        Line "if (TODO) {";
        Block (block_to_s accept);
      ]
      @
      (if reject = [] then
        []
      else
        [
          Line "} else {";
          Block (block_to_s reject);
        ]
      )
      @
      [ Line "}" ]

    | Switch (*of {
        selector: Expression;
        cases: SwitchCase;
      }*) ->
      [Line "switch (TODO) {TODO}"]
    | Loop {body; continuing;} ->
(*         break_if: Option<Handle<Expression>>, *)
      [
        Line "loop {";
        Block (block_to_s body);
        Line "} continuing {";
        Block (block_to_s continuing);
        Line "}"
      ]
    | Break -> [Line "break;"]
    | Continue -> [Line "continue;"]
    | Return (*of {
         value: Option<Handle<Expression>>
      }*) ->
      [Line "return TODO;"]
    | Kill -> [Line "kill;"]
    | Barrier ->
      [Line "barrier;"]
    | Store ->
      [Line "TODO = TODO;"]

    | ImageStore (* {
        image: Handle<Expression>,
        coordinate: Handle<Expression>,
        array_index: Option<Handle<Expression>>,
        value: Handle<Expression>,
      } *)
      ->
      [Line "textureStore(TODO);"]
    | Atomic (* {
        pointer: Handle<Expression>,
        fun: AtomicFunction,
        value: Handle<Expression>,
        result: Option<Handle<Expression>>,
      } *)
      ->
      [Line "atomic(TODO);"]
    | WorkGroupUniformLoad (*{
        pointer: Handle<Expression>,
        result: Handle<Expression>,
      }*)
      ->
      [Line "workgroupUniformLoad(TODO);"]
    | Call (* {
        function: Handle<Function>,
        arguments: Vec<Handle<Expression>>,
        result: Option<Handle<Expression>>,
      }*)
      ->
      [Line "call(TODO);"]
    | SubgroupBallot (* {
        result: Handle<Expression>,
        predicate: Option<Handle<Expression>>,
      }*) ->
      [Line "subgroupBallot(TODO)"]
    | SubgroupGather (*{
        mode: GatherMode,
        argument: Handle<Expression>,
        result: Handle<Expression>,
      }*) ->
      [Line "subgroupGather(TODO);"]
    | SubgroupCollectiveOperation (*{
        op: SubgroupOperation,
        collective_op: CollectiveOperation,
        argument: Handle<Expression>,
        result: Handle<Expression>,
      }*)
      ->
      [Line "subgroupCollective(TODO);"]
  and block_to_s l = List.concat_map to_s l

end

module Function = struct
  type t = {
    name: string;
    arguments: FunctionArgument.t list;
    result: FunctionResult.t option;
    body: Statement.t list;
  }

  let parse (j:json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* name = with_field "name" cast_string o in (* XXX: in Naga this is an optional string *)
    let* result = with_field "result" (cast_option FunctionResult.parse) o in
    let* arguments = with_field "arguments" (cast_map FunctionArgument.parse) o in
    let* body = with_field "body" (cast_map Statement.parse) o in
    Ok {name; result; arguments; body}
end

module ShaderStage = struct
  type t =
    | Vertex
    | Fragment
    | Compute

  let to_string : t -> string =
    function
    | Vertex -> "vertex"
    | Fragment -> "fragment"
    | Compute -> "compute"

  let parse (j:json) : t j_result =
    let open Rjson in
    let* name = cast_string j in
    match name with
    | "Vertex" -> Ok Vertex
    | "Fragment" -> Ok Fragment
    | "Compute" -> Ok Compute
    | _ -> root_cause ("ShaderStage.parse: unknown name: " ^ name) j

end

module EntryPoint = struct
  type t = {
    name: string;
    function_: Function.t;
    workgroup_size: Dim3.t;
    (*
    TODO: early_depth_test: EarlyDepthTest.t option;
    *)
    stage: ShaderStage.t;
  }

  let parse (j:json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* name = with_field "name" cast_string o in
    let* stage = with_field "stage" ShaderStage.parse o in
    let* workgroup_size =
      with_field "workgroup_size" (fun j ->
        match Dim3.from_json j with
        | Ok d -> Ok d
        | Error e -> root_cause e j
      ) o
    in
    let* function_ = with_field "function" Function.parse o in
    Ok {name; workgroup_size; stage; function_}

  let to_string (e:t) : string =
    e.name

  let to_s (d:t) : Indent.t list =
    let args =
      d.function_.arguments
      |> List.map FunctionArgument.to_string
      |> Common.join ", "
    in
    [
      Line (
        "@" ^ ShaderStage.to_string d.stage ^
        " @workgroup_size(" ^ Dim3.to_string d.workgroup_size ^ ")" ^
        " fn " ^ d.name ^ "(" ^ args ^ ") {"
      );
      Block (Statement.block_to_s d.function_.body);
      Line "}"
    ]
end

module ResourceBinding = struct
  type t = {
    group: int;
    binding: int;
  }

  let to_string (r:t) : string =
    "@group(" ^ string_of_int r.group ^
    ") @binding(" ^ string_of_int r.binding ^ ")"

  let parse (j:json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* group = with_field "group" cast_int o in
    let* binding = with_field "binding" cast_int o in
    Ok {group; binding}

end

module Decl = struct
  type t = {
    name: string;
    space: AddressSpace.t;
    binding: ResourceBinding.t;
    ty: Type.t;
    (* TODO: init *)
  }
  let parse (j:json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* name = with_field "name" cast_string o in
    let* ty = with_field "ty" Type.parse o in
    let* space = with_field "space" AddressSpace.parse o in
    let* binding = with_field "binding" ResourceBinding.parse o in
    Ok {ty; space; name; binding}

  let to_string (d:t) : string =
    d.name

  let to_s (d:t) : Indent.t list =
    [
    Line (ResourceBinding.to_string d.binding ^ " var<" ^ AddressSpace.to_string d.space ^ "> " ^ d.name ^": " ^ Type.to_string d.ty ^";")
    ]
end

module Def = struct
  type t =
    | EntryPoint of EntryPoint.t
    | Declaration of Decl.t

  let parse (j:json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* kind = get_kind o in
    match kind with
    | "GlobalDeclaration" ->
      let* d = Decl.parse j in
      Ok (Declaration d)
    | "EntryPoint" ->
      let* e = EntryPoint.parse j in
      Ok (EntryPoint e)

    | _ ->
      root_cause ("Unknown kind: " ^ kind) j

  let to_string : t -> string =
    function
    | EntryPoint e -> EntryPoint.to_string e
    | Declaration d -> Decl.to_string d

  let to_s : t -> Indent.t list =
    function
    | EntryPoint e -> EntryPoint.to_s e
    | Declaration e -> Decl.to_s e
end

module Program = struct
  type t = Def.t list

  let parse (j:json) : t j_result =
    let open Rjson in
    let* l = cast_map Def.parse j in
    Ok l

  let to_s : t -> Indent.t list =
    List.concat_map Def.to_s

  let to_string (l:t) : string =
    Indent.to_string (to_s l)
end
