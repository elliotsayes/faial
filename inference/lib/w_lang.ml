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

  let is_int : t -> bool =
    function
    | Sint
    | Uint
    | AbstractInt -> true
    | Float
    | Bool
    | AbstractFloat -> false

  let parse (j:json) : t j_result =
    let open Rjson in
    let* n = cast_string j in
    match n with
    | "Sint" -> Ok Sint
    | "Uint" -> Ok Uint
    | "Float" -> Ok Float
    | "Bool" -> Ok Bool
    | "AbstractInt" -> Ok AbstractInt
    | "AbstractFloat" -> Ok AbstractFloat
    | _ -> root_cause ("ScalarKind.parse: unknown kind: " ^ n) j

  let to_string : t -> string =
    function
    | Sint -> "i"
    | Uint -> "u"
    | Float -> "f"
    | Bool -> "bool"
    | AbstractInt -> "abstract i"
    | AbstractFloat -> "abstract f"

  let is_unsigned (x:t) : bool =
    x = Uint
end

module Scalar = struct
  type t = {kind: ScalarKind.t; width: int}

  let u32 : t = {kind=ScalarKind.Uint; width=4}
  let u64 : t = {kind=ScalarKind.Uint; width=8}
  let i32 : t = {kind=ScalarKind.Sint; width=4}
  let i64 : t = {kind=ScalarKind.Sint; width=8}
  let f32 : t = {kind=ScalarKind.Float; width=4}
  let f64 : t = {kind=ScalarKind.Float; width=8}

  let bool : t = {kind=ScalarKind.Bool; width=1}
  let int : t = {kind=ScalarKind.AbstractInt; width=8}
  let float : t = {kind=ScalarKind.AbstractFloat; width=8}

  let is_int (s:t) : bool =
    ScalarKind.is_int s.kind

  let is_unsigned (s:t) : bool =
    ScalarKind.is_unsigned s.kind

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

  let parse (j:json) : t j_result =
    let open Rjson in
    let* data = cast_string j in
    match data with
    | "D1" -> Ok D1
    | "D2" -> Ok D2
    | "D3" -> Ok D3
    | "Cube" -> Ok Cube
    | _ -> root_cause ("ImageDimension.parse: unknown: " ^ data) j

  let to_string : t -> string =
    function
    | D1 -> "1d"
    | D2 -> "2d"
    | D3 -> "3d"
    | Cube -> "cube"
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


  let multisampled : t -> bool =
    function
    | Sampled {multi; _}
    | Depth {multi;} -> multi
    | Storage _ -> false

  let parse (j:json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* kind = get_kind o in
    match kind with
    | "Sampled" ->
      let* kind = with_field "scalar_kind" ScalarKind.parse o in
      let* multi = with_field "multi" cast_bool o in
      Ok (Sampled {kind; multi;})
    | "Depth" ->
      let* multi = with_field "multi" cast_bool o in
      Ok (Depth {multi})
    | "Storage" ->
      let* format = with_field "format" cast_string o in
      let* access = with_field "access" StorageAccess.parse o in
      Ok (Storage {format; access;})
    | _ ->
      root_cause "Unsupported kind" j

  let to_string : t -> string =
    function
    | _ -> "ImageClass"
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
    | _ -> root_cause ("BuiltIn.parse: Unknown kind: " ^ name) j


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

  let workgroup_id : t = BuiltIn BuiltIn.WorkGroupId

  let global_invocation_id : t = BuiltIn BuiltIn.GlobalInvocationId

  let num_workgroups : t = BuiltIn BuiltIn.NumWorkGroups

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
        name: string;
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

    let i_vec3_u32 : inner = Vector {size=VectorSize.Tri; scalar=Scalar.u32}

    let to_scalar (ty:t) : Scalar.t option =
      match ty.inner with
      | Scalar s -> Some s
      | _ -> None

    let is_array (ty:t) : bool =
      match ty.inner with
      | Array _ -> true
      | _ -> false

    let is_int (ty:t) : bool =
      ty
      |> to_scalar
      |> Option.map Scalar.is_int
      |> Option.value ~default:false

    let is_vec3_u32 (ty:t) : bool =
      ty.inner = i_vec3_u32

    let vector_field : string list = ["x"; "y"; "z"; "w"]

    let lookup_field (index:int) (ty:t) : string option =
      match ty.inner with
      | Vector _ ->
          List.nth_opt vector_field index
      | Struct {members; _} ->
        index
        |> List.nth_opt members
        |> Option.map (fun (m:struct_member) -> m.name)
      | _ -> None

    let deref (ty:t) : t option =
      match ty.inner with
      | Array {base; _} -> Some base
      | _ -> None

    let make (inner:inner) : t = {name=None; inner}

    let rec inner_to_string (name:string option) : inner -> string =
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
      | Image {dim; arrayed; image_class} ->
        let dim = ImageDimension.to_string dim in
        let arrayed = if arrayed then "_array" else "" in
        let (klass, format, storage) =
          match image_class with
          | Sampled {kind; _;} ->
            let scalar =
              {kind; width=4}
              |> Scalar.to_string
            in
            ("", scalar, "")
          | Depth _ ->
            ("depth_", "", "")
          | Storage {format; access;} ->
            ("storage_", format, StorageAccess.to_string access)
        in
        let multi = if ImageClass.multisampled image_class then "multisampled_" else "" in
        let addendum =
          if format <> "" then
            "<" ^ format ^ storage ^ ">"
          else
            ""
        in
        "texture_" ^ klass ^ multi ^ dim ^ arrayed ^ addendum
      | Struct {members=m; _} ->
        let name =
          match name with
          | Some name -> name ^ " "
          | None -> ""
        in
        "struct " ^ name ^ "{" ^ (List.map struct_to_string m |> Common.join ", ") ^ "};"
      | Matrix {columns; rows; scalar} ->
        let columns = VectorSize.to_string columns in
        let rows = VectorSize.to_string rows in
        "mat" ^ rows ^ "x" ^ columns ^ "<" ^ Scalar.to_string scalar ^ ">"
      | k -> failwith ("inner_to_string: unsupported kind:" ^ kind k)

  and to_string (e:t) : string =
    match e.name with
    | Some n -> n
    | None -> inner_to_string None e.inner

  and struct_to_string (s:struct_member) : string =
    let binding =
      match s.binding with
      | Some b -> Binding.to_string b ^ " "
      | None -> ""
    in
    binding ^ s.name ^ " : " ^ to_string s.ty

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
    | "Image" ->
      let* dim = with_field "dim" ImageDimension.parse o in
      let* arrayed = with_field "arrayed" cast_bool o in
      let* image_class = with_field "class" ImageClass.parse o in
      Ok (Image {dim; arrayed; image_class;})
    | "Struct" ->
      let* span = with_field "span" cast_int o in
      let* members = with_field "members" (cast_map struct_parse) o in
      Ok (Struct {span; members;})
    | "Matrix" ->
      let* rows = with_field "rows" VectorSize.parse o in
      let* columns = with_field "columns" VectorSize.parse o in
      let* scalar = with_field "scalar" Scalar.parse o in
      Ok (Matrix {rows; columns; scalar})
    | _ -> root_cause ("inner_parse: unsupported kind: " ^ kind) j

  and struct_parse (j:json) : struct_member j_result =
    let open Rjson in
    let* o = cast_object j in
    let* name = with_field "name" cast_string o in
    let* ty = with_field "ty" parse o in
    let* binding = with_field "binding" (cast_option Binding.parse) o in
    let* offset = with_field "offset" cast_int o in
    Ok {name; ty; binding; offset}
end

module IdentKind = struct
  type t =
    | FunctionArgument of Binding.t option
    | GlobalVariable
    | LocalVariable
    | CallResult

  let is_thread_idx (k:t) : bool =
    k = FunctionArgument (Some Binding.global_invocation_id)

  let is_block_idx (k:t) : bool =
    k = FunctionArgument (Some Binding.workgroup_id)

  let is_grid_dim (k:t) : bool =
    k = FunctionArgument (Some Binding.num_workgroups)

end

let parse_location (j:json) : Location.t j_result =
  let open Rjson in
  let* o = cast_object j in
  let* filename = with_field "filename" cast_string o in
  let* line_number = with_field "line_number" cast_int o in
  let* line_position = with_field "line_position" cast_int o in
  let* length = with_field "length" cast_int o in
  Ok {
    Location.filename = filename;
    line = Index.from_base1 line_number;
    interval =
      Interval.from_range
        ~start:(Index.from_base1 line_position)
        ~length;
  }

let parse_var (o:Rjson.j_object) : Variable.t j_result =
  let open Rjson in
  let* name = with_field "name" cast_string o in
  Ok (Variable.from_name name)

module Ident = struct
  type t = {
    var: Variable.t;
    ty: Type.t;
    kind: IdentKind.t;
  }

  let var (i:t) : Variable.t =
    i.var

  let add_suffix (suffix:string) (x:t) =
    { x with var = Variable.add_suffix suffix x.var }

  let inline_field (index:int) (a:t) : t =
    let var =
      if IdentKind.is_thread_idx a.kind then
        List.nth_opt Variable.tid_list index
      else if IdentKind.is_block_idx a.kind then
        List.nth_opt Variable.bid_list index
      else if IdentKind.is_grid_dim a.kind then
        List.nth_opt Variable.gdim_list index
      else
        None
    in
    match var with
    | Some var ->
      { a with
        var =
          a.var
          (* When the variable is pretty-printed, use original variable's name *)
          |> Variable.set_label a.var.name
          (* When the variable is used internally, use our internal name *)
          |> Variable.set_name var.name
      }
    | None ->
      a.ty
      |> Type.lookup_field index
      |> Option.map (fun f -> add_suffix ("." ^ f) a)
      |> Option.value ~default:(add_suffix (string_of_int index ^ ".") a)

  let parse (j:json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* ty = with_field "ty" Type.parse o in
    let* kind = get_kind o in
    let* kind : IdentKind.t =
      let open IdentKind in
      match kind with
      | "FunctionArgument" ->
        let* binding = with_field "binding" (cast_option Binding.parse) o in
        Ok (FunctionArgument binding)
      | "GlobalVariable" ->
        Ok GlobalVariable
      | "LocalVariable" ->
        Ok LocalVariable
      | "CallResult" ->
        Ok CallResult
      | _ ->
        root_cause ("Indent.parse: unknown kind: " ^ kind) j
    in
    let* var =
      if kind = CallResult && List.assoc_opt "name" o  = Some `Null then
        (* Naga will set name to null when the result is the result
            of the previous call performed. We use @Call to represent
            the contents of the last write. *)
        Ok (Variable.from_name "@Call")
      else
        parse_var o
    in
    Ok {ty; var; kind}

  let to_string (x:t) : string =
    x.var |> Variable.name

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

module BinaryOperator = struct
  type t =
    | Add
    | Subtract
    | Multiply
    | Divide
    | Modulo
    | Equal
    | NotEqual
    | Less
    | LessEqual
    | Greater
    | GreaterEqual
    | And
    | ExclusiveOr
    | InclusiveOr
    | LogicalAnd
    | LogicalOr
    | ShiftLeft
    | ShiftRight

  let to_string : t -> string =
    function
    | Add -> "+"
    | Subtract -> "-"
    | Multiply -> "*"
    | Divide -> "/"
    | Modulo -> "%"
    | Equal -> "=="
    | NotEqual -> "!="
    | Less -> "<"
    | LessEqual -> "<="
    | Greater -> ">"
    | GreaterEqual -> ">="
    | And -> "&"
    | ExclusiveOr -> "^"
    | InclusiveOr -> "|"
    | LogicalAnd -> "&&"
    | LogicalOr -> "||"
    | ShiftLeft -> "<<"
    | ShiftRight -> ">>"

  let parse (j:json) : t j_result =
    let open Rjson in
    let* name = cast_string j in
    match name with
    | "Add" -> Ok Add
    | "Subtract" -> Ok Subtract
    | "Multiply" -> Ok Multiply
    | "Divide" -> Ok Divide
    | "Modulo" -> Ok Modulo
    | "Equal" -> Ok Equal
    | "NotEqual" -> Ok NotEqual
    | "Less" -> Ok Less
    | "LessEqual" -> Ok LessEqual
    | "Greater" -> Ok Greater
    | "GreaterEqual" -> Ok GreaterEqual
    | "And" -> Ok And
    | "ExclusiveOr" -> Ok ExclusiveOr
    | "InclusiveOr" -> Ok InclusiveOr
    | "LogicalAnd" -> Ok LogicalAnd
    | "LogicalOr" -> Ok LogicalOr
    | "ShiftLeft" -> Ok ShiftLeft
    | "ShiftRight" -> Ok ShiftRight
    | _ -> failwith name
end

module Literal = struct
  type t =
    | F32 of float
    | F64 of float
    | U32 of int
    | U64 of int
    | I32 of int
    | I64 of int
    | AbstractInt of int
    | AbstractFloat of float
    | Bool of bool

  let to_bool : t -> bool option =
    function
    | F64 _
    | F32 _
    | U32 _
    | I32 _
    | U64 _
    | I64 _
    | AbstractInt _
    | AbstractFloat _ -> None
    | Bool b -> Some b

  let to_int : t -> int option =
    function
    | U32 v
    | I32 v
    | U64 v
    | I64 v
    | AbstractInt v ->
      Some v
    | F64 _
    | F32 _
    | AbstractFloat _
    | Bool _ ->
      None

  let to_float : t -> float option =
    function
    | Bool _
    | U32 _
    | I32 _
    | U64 _
    | I64 _
    | AbstractInt _ ->
      None
    | F64 v
    | F32 v
    | AbstractFloat v ->
      Some v

  let to_string : t -> string =
    function
    | F32 v -> Float.to_string v
    | F64 v -> Float.to_string v
    | U32 v -> Int.to_string v
    | I32 v -> Int.to_string v
    | U64 v -> Int.to_string v
    | I64 v -> Int.to_string v
    | Bool v -> Bool.to_string v
    | AbstractInt v -> Int.to_string v
    | AbstractFloat v -> Float.to_string v

  let parse (j:json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* kind = get_kind o in
    match kind with
    | "f64" ->
      let* v = with_field "value" cast_float o in
      Ok (F64 v)
    | "f32" ->
      let* v = with_field "value" cast_float o in
      Ok (F32 v)
    | "u32" ->
      let* v = with_field "value" cast_int o in
      Ok (U32 v)
    | "i32" ->
      let* v = with_field "value" cast_int o in
      Ok (I32 v)
    | "u64" ->
      let* v = with_field "value" cast_int o in
      Ok (U64 v)
    | "i64" ->
      let* v = with_field "value" cast_int o in
      Ok (I64 v)
    | "bool" ->
      let* v = with_field "value" cast_bool o in
      Ok (Bool v)
    | "int" ->
      let* v = with_field "value" cast_int o in
      Ok (AbstractInt v)
    | "float" ->
      let* v = with_field "value" cast_float o in
      Ok (AbstractFloat v)
    | _ ->
      failwith kind

end

module MathFunction = struct
  type t =
    | Abs
    | Min
    | Max
    | Clamp
    | Saturate
    | Cos
    | Cosh
    | Sin
    | Sinh
    | Tan
    | Tanh
    | Acos
    | Asin
    | Atan
    | Atan2
    | Asinh
    | Acosh
    | Atanh
    | Radians
    | Degrees
    | Ceil
    | Floor
    | Round
    | Fract
    | Trunc
    | Modf
    | Frexp
    | Ldexp
    | Exp
    | Exp2
    | Log
    | Log2
    | Pow
    | Dot
    | Cross
    | Distance
    | Length
    | Normalize
    | FaceForward
    | Reflect
    | Refract
    | Sign
    | Fma
    | Mix
    | Step
    | SmoothStep
    | Sqrt
    | InverseSqrt
    | Transpose
    | Determinant
    | CountTrailingZeros
    | CountLeadingZeros
    | CountOneBits
    | ReverseBits
    | ExtractBits
    | InsertBits
    | FindLsb
    | FindMsb
    | Pack4x8snorm
    | Pack4x8unorm
    | Pack2x16snorm
    | Pack2x16unorm
    | Pack2x16float
    | Pack4xI8
    | Pack4xU8
    | Unpack4x8snorm
    | Unpack4x8unorm
    | Unpack2x16snorm
    | Unpack2x16unorm
    | Unpack2x16float
    | Unpack4xI8
    | Unpack4xU8

  let parse (j:json) : t j_result =
    let open Rjson in
    let* kind = cast_string j in
    match kind with
    | "Abs" -> Ok Abs
    | "Min" -> Ok Min
    | "Max" -> Ok Max
    | "Clamp" -> Ok Clamp
    | "Saturate" -> Ok Saturate
    | "Cos" -> Ok Cos
    | "Cosh" -> Ok Cosh
    | "Sin" -> Ok Sin
    | "Sinh" -> Ok Sinh
    | "Tan" -> Ok Tan
    | "Tanh" -> Ok Tanh
    | "Acos" -> Ok Acos
    | "Asin" -> Ok Asin
    | "Atan" -> Ok Atan
    | "Atan2" -> Ok Atan2
    | "Asinh" -> Ok Asinh
    | "Acosh" -> Ok Acosh
    | "Atanh" -> Ok Atanh
    | "Radians" -> Ok Radians
    | "Degrees" -> Ok Degrees
    | "Ceil" -> Ok Ceil
    | "Floor" -> Ok Floor
    | "Round" -> Ok Round
    | "Fract" -> Ok Fract
    | "Trunc" -> Ok Trunc
    | "Modf" -> Ok Modf
    | "Frexp" -> Ok Frexp
    | "Ldexp" -> Ok Ldexp
    | "Exp" -> Ok Exp
    | "Exp2" -> Ok Exp2
    | "Log" -> Ok Log
    | "Log2" -> Ok Log2
    | "Pow" -> Ok Pow
    | "Dot" -> Ok Dot
    | "Cross" -> Ok Cross
    | "Distance" -> Ok Distance
    | "Length" -> Ok Length
    | "Normalize" -> Ok Normalize
    | "FaceForward" -> Ok FaceForward
    | "Reflect" -> Ok Reflect
    | "Refract" -> Ok Refract
    | "Sign" -> Ok Sign
    | "Fma" -> Ok Fma
    | "Mix" -> Ok Mix
    | "Step" -> Ok Step
    | "SmoothStep" -> Ok SmoothStep
    | "Sqrt" -> Ok Sqrt
    | "InverseSqrt" -> Ok InverseSqrt
    | "Transpose" -> Ok Transpose
    | "Determinant" -> Ok Determinant
    | "CountTrailingZeros" -> Ok CountTrailingZeros
    | "CountLeadingZeros" -> Ok CountLeadingZeros
    | "CountOneBits" -> Ok CountOneBits
    | "ReverseBits" -> Ok ReverseBits
    | "ExtractBits" -> Ok ExtractBits
    | "InsertBits" -> Ok InsertBits
    | "FindLsb" -> Ok FindLsb
    | "FindMsb" -> Ok FindMsb
    | "Pack4x8snorm" -> Ok Pack4x8snorm
    | "Pack4x8unorm" -> Ok Pack4x8unorm
    | "Pack2x16snorm" -> Ok Pack2x16snorm
    | "Pack2x16unorm" -> Ok Pack2x16unorm
    | "Pack2x16float" -> Ok Pack2x16float
    | "Pack4xI8" -> Ok Pack4xI8
    | "Pack4xU8" -> Ok Pack4xU8
    | "Unpack4x8snorm" -> Ok Unpack4x8snorm
    | "Unpack4x8unorm" -> Ok Unpack4x8unorm
    | "Unpack2x16snorm" -> Ok Unpack2x16snorm
    | "Unpack2x16unorm" -> Ok Unpack2x16unorm
    | "Unpack2x16float" -> Ok Unpack2x16float
    | "Unpack4xI8" -> Ok Unpack4xI8
    | "Unpack4xU8" -> Ok Unpack4xU8
    | _ -> root_cause "MathFunction" j

  let to_string : t -> string =
    function
    | Abs -> "abs"
    | Min -> "min"
    | Max -> "max"
    | Clamp -> "clamp"
    | Saturate -> "saturate"
    | Cos -> "cos"
    | Cosh -> "cosh"
    | Sin -> "sin"
    | Sinh -> "sinh"
    | Tan -> "tan"
    | Tanh -> "tanh"
    | Acos -> "acos"
    | Asin -> "asin"
    | Atan -> "atan"
    | Atan2 -> "atan2"
    | Asinh -> "asinh"
    | Acosh -> "acosh"
    | Atanh -> "atanh"
    | Radians -> "radians"
    | Degrees -> "degrees"
    | Ceil -> "ceil"
    | Floor -> "floor"
    | Round -> "round"
    | Fract -> "fract"
    | Trunc -> "trunc"
    | Modf -> "modf"
    | Frexp -> "frexp"
    | Ldexp -> "ldexp"
    | Exp -> "exp"
    | Exp2 -> "exp2"
    | Log -> "log"
    | Log2 -> "log2"
    | Pow -> "pow"
    | Dot -> "dot"
    | Cross -> "cross"
    | Distance -> "distance"
    | Length -> "length"
    | Normalize -> "normalize"
    | FaceForward -> "faceForward"
    | Reflect -> "reflect"
    | Refract -> "refract"
    | Sign -> "sign"
    | Fma -> "fma"
    | Mix -> "mix"
    | Step -> "step"
    | SmoothStep -> "smoothstep"
    | Sqrt -> "sqrt"
    | InverseSqrt -> "inverseSqrt"
    | Transpose -> "transpose"
    | Determinant -> "determinant"
    | CountTrailingZeros -> "countTrailingZeros"
    | CountLeadingZeros -> "countLeadingZeros"
    | CountOneBits -> "countOneBits"
    | ReverseBits -> "reverseBits"
    | ExtractBits -> "extractBits"
    | InsertBits -> "insertBits"
    | FindLsb -> "firstTrailingBit"
    | FindMsb -> "firstLeadingBit"
    | Pack4x8snorm -> "pack4x8snorm"
    | Pack4x8unorm -> "pack4x8unorm"
    | Pack2x16snorm -> "pack2x16snorm"
    | Pack2x16unorm -> "pack2x16unorm"
    | Pack2x16float -> "pack2x16float"
    | Pack4xI8 -> "pack4xI8"
    | Pack4xU8 -> "pack4xU8"
    | Unpack4x8snorm -> "unpack4x8snorm"
    | Unpack4x8unorm -> "unpack4x8unorm"
    | Unpack2x16snorm -> "unpack2x16snorm"
    | Unpack2x16unorm -> "unpack2x16unorm"
    | Unpack2x16float -> "unpack2x16float"
    | Unpack4xI8 -> "unpack4xI8"
    | Unpack4xU8 -> "unpack4xU8"
end

module Expression = struct
  type t =
    | Literal of Literal.t
    | Constant (*Handle<Constant>*)
    | Override (*Handle<Override>*)
    | ZeroValue of Type.t
    | Compose of {
        ty: Type.t;
        components: t list;
      }
    | Access of {
        base: t;
        index: t;
        location: Location.t;
      }
    | AccessIndex of {
        base: t;
        index: int;
        location: Location.t;
      }
    | Splat of {
        size: VectorSize.t;
        value: t;
      }
    | Swizzle of {
        size: VectorSize.t;
        vector: t;
        pattern: string list;
      }
    | Ident of Ident.t
    | Load of t
    | ImageSample of {
        image: t;
        sampler: t;
(*         gather: Option<SwizzleComponent>, *)
        coordinate: t;
        array_index: t option;
        offset: t option;
(*         level: SampleLevel, *)
        depth_ref: t option;
      }
    | ImageLoad of {
        image: t;
        coordinate: t;
        array_index: t option;
        sample: t option;
        level: t option;
      }
    | ImageQuery of {
        image: t;
        query: image_query;
      }
    | Unary of {
(*         op: UnaryOperator, *)
        expr: t;
      }
    | Binary of {
        op: BinaryOperator.t;
        left: t;
        right: t;
      }
    | Select of {
        condition: t;
        accept: t;
        reject: t;
      }
    | Derivative of {
(*         axis: DerivativeAxis, *)
(*         ctrl: DerivativeControl, *)
        expr: t;
      }
    | Relational of {
(*         fun: RelationalFunction, *)
        argument: t;
      }
    | Math of {
        fun_: MathFunction.t;
        args: t list;
      }
    | As of {
        expr: t;
        kind: ScalarKind.t;
        convert: int option;
      }
    | AtomicResult of {
        ty: Type.t;
        comparison: bool;
      }
    | WorkGroupUniformLoadResult of Type.t
    | ArrayLength of t
    | RayQueryProceedResult
    | RayQueryGetIntersection of {
        query: t;
        committed: bool;
      }
    | SubgroupBallotResult
    | SubgroupOperationResult of Type.t

  and image_query =
    | Size of t option
    | NumLevels
    | NumLayers
    | NumSamples

  let rec to_string : t -> string =
    function
    | Literal l -> Literal.to_string l
    | Constant -> (*TODO*) "Constant(TODO)"
    | Override -> (*TODO*) "Override(TODO)"
    | ZeroValue ty -> Type.to_string ty ^ "()"
    | Compose {ty; components} ->
      let components =
        components
        |> List.map to_string
        |> Common.join ", "
      in
      Type.to_string ty ^ "(" ^ components ^ ")"
    | Access {base; index; location=_;} ->
      to_string base ^ "[" ^ to_string index ^ "]"
    | AccessIndex {base; index; location=_;} ->
      to_string base ^ "." ^ string_of_int index
    | Splat {size; value} -> "vec" ^ VectorSize.to_string size ^ "(" ^ to_string value ^ ")"
    | Swizzle {vector; pattern; size} ->
      let pattern =
        Slice.from_finish (VectorSize.to_int size)
        |> Slice.sublist pattern
      in
      to_string vector ^ "." ^ (pattern |> Common.join "")
    | Ident i -> Ident.to_string i
    | Load e ->
      "load(" ^ to_string e ^")"
    | ImageSample _ -> (*TODO*) "ImageSample"
    | ImageLoad {image; coordinate; array_index; sample; level;} ->
      let args =
        let index =
          let index =
            Option.to_list sample
            @ Option.to_list level
          in
          Slice.from_start 1
          |> Slice.sublist index
        in
        [image; coordinate]
        @ Option.to_list array_index
        @ index
        |> List.map to_string
        |> Common.join ", "
      in
      "textureLoad(" ^ args ^")"
    | ImageQuery {image; query} ->
      let func =
        match query with
        | Size _ -> "textureDimensions"
        | NumLevels -> "textureNumLevels"
        | NumLayers -> "textureNumLayers"
        | NumSamples -> "textureNumSamples"
      in
      let arg =
        match query with
        | Size (Some e) -> ", " ^ to_string e
        | _ -> ""
      in
      func ^ "(" ^ to_string image ^ arg ^ ")"
    | Unary _ -> (* TODO *) "Unary"
    | Binary b ->
      Printf.sprintf
        "(%s) %s (%s)"
        (to_string b.left)
        (BinaryOperator.to_string b.op)
        (to_string b.right)
    | Select _ -> (*TODO*) "Select"
    | Derivative _ -> (*TODO*) "Derivative"
    | Relational _ -> (*TODO*) "Relational"
    | Math {fun_; args} ->
      let args = args |> List.map to_string |> Common.join ", " in
      MathFunction.to_string fun_ ^ "(" ^ args ^ ")"
    | As {expr; kind; convert} ->
      let ty =
        match convert with
        | Some n -> Scalar.{kind=kind; width=n} |> Scalar.to_string
        | None -> ScalarKind.to_string kind
      in
       ty ^ "(" ^ to_string expr ^ ")"
    | AtomicResult _ -> (*TODO*) "AtomicResult"
    | WorkGroupUniformLoadResult _ -> "WorkGroupUniformLoadResult"
    | ArrayLength e -> "arrayLength(" ^ to_string e ^ ")"
    | RayQueryProceedResult -> "RayQueryProceedResult"
    | RayQueryGetIntersection _ -> "RayQueryGetIntersection"
    | SubgroupBallotResult -> "SubgroupBallotResult"
    | SubgroupOperationResult _ -> "SubgroupOperationResult"

  let rec parse (j:json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* kind = get_kind o in
    match kind with
    | "Literal" ->
      let* value = with_field "value" Literal.parse o in
      Ok (Literal value)
    | "Constant" -> Ok Constant
    | "Override" -> Ok Override
    | "ZeroValue" ->
      let* ty = with_field "ty" Type.parse o in
      Ok (ZeroValue ty)
    | "Compose" ->
      let* ty = with_field "ty" Type.parse o in
      let* components = with_field "components" (cast_map parse) o in
      Ok (Compose {ty; components;})
    | "Access" ->
      let* base = with_field "base" parse o in
      let* index = with_field "index" parse o in
      let* location = with_field "location" parse_location o in
      Ok (Access {base; index; location;})
    | "AccessIndex" ->
      let* base = with_field "base" parse o in
      let* index = with_field "index" cast_int o in
      let* location = with_field "location" parse_location o in
      Ok (AccessIndex {base; index; location;})
    | "Splat" ->
      let* value = with_field "value" parse o in
      let* size = with_field "size" VectorSize.parse o in
      Ok (Splat {value; size})
    | "Swizzle" ->
      let* size = with_field "size" VectorSize.parse o in
      let* vector = with_field "vector" parse o in
      let* pattern = with_field "pattern" (cast_map cast_string) o in
      Ok (Swizzle {size; vector; pattern;})
    | "CallResult"
    | "FunctionArgument"
    | "GlobalVariable"
    | "LocalVariable" ->
      let* i = Ident.parse j in
      Ok (Ident i)
    | "Load" ->
      let* value = with_field "pointer" parse o in
      Ok (Load value)
    | "ImageSample" ->
      let* image = with_field "image" parse o in
      let* sampler = with_field "sampler" parse o in
      let* coordinate = with_field "coordinate" parse o in
      let* array_index = with_field "array_index" (cast_option parse) o in
      let* offset = with_field "offset" (cast_option parse) o in
      let* depth_ref = with_field "depth_ref" (cast_option parse) o in
      Ok (ImageSample {
        image;
        sampler;
        coordinate;
        array_index;
        offset;
        depth_ref;
      })
    | "ImageLoad" ->
      let* image = with_field "image" parse o in
      let* coordinate = with_field "coordinate" parse o in
      let* array_index = with_field "array_index" (cast_option parse) o in
      let* sample = with_field "sample" (cast_option parse) o in
      let* level = with_field "level" (cast_option parse) o in
      Ok (ImageLoad {
        image;
        coordinate;
        array_index;
        sample;
        level;
      })
    | "ImageQuery" ->
      let* image = with_field "image" parse o in
      let* query = with_field "query" parse_image_query o in
      Ok (ImageQuery {
        query;
        image;
      })
    | "Unary" ->
      let* expr = with_field "expr" parse o in
      Ok (Unary {
        expr;
      })
    | "Binary" ->
      let* left = with_field "left" parse o in
      let* right = with_field "right" parse o in
      let* op = with_field "op" BinaryOperator.parse o in
      Ok (Binary {
        op;
        left;
        right;
      })
    | "Select" ->
      let* condition = with_field "condition" parse o in
      let* accept = with_field "accept" parse o in
      let* reject = with_field "reject" parse o in
      Ok (Select {
        condition;
        accept;
        reject;
      })
    | "Derivative" ->
      let* expr = with_field "expr" parse o in
      Ok (Derivative {
        expr;
      })
    | "Relational" ->
      let* argument = with_field "argument" parse o in
      Ok (Relational {
        argument;
      })
    | "Math" ->
      let* fun_ = with_field "fun" MathFunction.parse o in
      let* args = with_field "args" (cast_map parse) o in
      Ok (Math {fun_; args;})
    | "As" ->
      let* expr = with_field "expr" parse o in
      let* kind = with_field "scalar_kind" ScalarKind.parse o in
      let* convert = with_field "convert" (cast_option cast_int) o in
      Ok (As {expr; kind; convert;})
    | "AtomicResult" ->
      let* ty = with_field "ty" Type.parse o in
      let* comparison = with_field "comparison" cast_bool o in
      Ok (AtomicResult {
        ty;
        comparison;
      })
    | "WorkGroupUniformLoadResult" ->
      let* ty = with_field "ty" Type.parse o in
      Ok (WorkGroupUniformLoadResult ty)
    | "ArrayLength" ->
      let* e = with_field "array" parse o in
      Ok (ArrayLength e)
    | "RayQueryProceedResult" -> Ok RayQueryProceedResult
    | "RayQueryGetIntersection" ->
      let* query = with_field "query" parse o in
      let* committed = with_field "committed" cast_bool o in
      Ok (RayQueryGetIntersection {
        query;
        committed;
      })
    | "SubgroupBallotResult" -> Ok SubgroupBallotResult
    | "SubgroupOperationResult" ->
      let* ty = with_field "ty" Type.parse o in
      Ok (SubgroupOperationResult ty)
      | _ -> failwith kind
  and parse_image_query (j:json) : image_query j_result =
    let open Rjson in
    let* o = cast_object j in
    let* kind = get_kind o in
    match kind with
    | "Size" ->
      let* e = with_field "level" (cast_option parse) o in
      Ok (Size e)
    | "NumLevels" -> Ok NumLevels
    | "NumLayers" -> Ok NumLayers
    | "NumSamples" -> Ok NumSamples
    | _ -> root_cause "parse_image_query" j

end

module LocalDeclaration = struct
  type t = {
    var: Variable.t;
    init: Expression.t option;
    ty: Type.t;
  }

  let parse (j:json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* var = parse_var o in
    let* init = with_field "init" (cast_option Expression.parse) o in
    let* ty = with_field "ty" Type.parse o in
    Ok {var; init; ty}

  let to_string (d:t) : string =
    let init =
      match d.init with
      | Some init -> " = " ^ Expression.to_string init
      | None -> ""
    in
    Variable.name d.var ^ " : " ^ Type.to_string d.ty ^ init
end

module Statement = struct
  type t =
    | Block of t list
    | If of {
        condition: Expression.t;
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
    | Return of Expression.t option
    | Kill
    | Barrier of {storage: bool; work_group: bool; sub_group: bool;}
    | Store of {
        pointer: Expression.t;
        value: Expression.t;
      }
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
    | Call of {
        function_: string;
        arguments: Expression.t list;
        result: Ident.t option;
      }
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
        let* l = with_field "body" (cast_map parse) o in
        Ok (Block l)
      | "If" ->
        let* condition = with_field "condition" Expression.parse o in
        let* accept = with_field "accept" (cast_map parse) o in
        let* reject = with_field "reject" (cast_map parse) o in
        Ok (If {accept; reject; condition;})
      | "Switch" -> Ok Switch
      | "Loop" ->
        let* body = with_field "body" (cast_map parse) o in
        let* continuing = with_field "continuing" (cast_map parse) o in
        Ok (Loop {body; continuing})
      | "Break" -> Ok Break
      | "Continue" -> Ok Continue
      | "Return" ->
        let* e = with_field "value" (cast_option Expression.parse) o in
        Ok (Return e)
      | "Kill" -> Ok Kill
      | "Barrier" ->
        let* b = with_field "value" cast_object o in
        let* storage = with_field "storage" cast_bool b in
        let* work_group = with_field "work_group" cast_bool b in
        let* sub_group = with_field "sub_group" cast_bool b in
        Ok (Barrier {storage; work_group; sub_group;})
      | "Store" ->
        let* pointer = with_field "pointer" Expression.parse o in
        let* value = with_field "value" Expression.parse o in
        Ok (Store {pointer; value;})
      | "ImageStore" -> Ok ImageStore
      | "Atomic" -> Ok Atomic
      | "WorkGroupUniformLoad" -> Ok WorkGroupUniformLoad
      | "Call" ->
        let* function_ = with_field "function" cast_string o in
        let* arguments = with_field "arguments" (cast_map Expression.parse) o in
        let* result = with_field "result" (cast_option Ident.parse) o in
        Ok (Call{function_; arguments; result})
      | "SubgroupBallot" -> Ok SubgroupBallot
      | "SubgroupGather" -> Ok SubgroupGather
      | "SubgroupCollectiveOperation" -> Ok SubgroupCollectiveOperation
      | _ -> root_cause ("Statement.parse: unknown kind: " ^ kind) j

  let rec to_s : t -> Indent.t list =
    function
    | Block l ->
      [
        Line "{";
        Block (block_to_s l);
        Line "}"
      ]
    | If {accept; reject; condition; } ->
      let open Indent in
      [
        Line ("if (" ^ Expression.to_string condition ^ ") {");
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
    | Return None ->
      [Line "return;"]
    | Return (Some e) ->
      [Line ("return " ^ Expression.to_string e ^ ";")]
    | Kill -> [Line "kill;"]
    | Barrier b ->
      if b.storage then
        [Line "storageBarrier();"]
      else []
      @
      if b.work_group then
        [Indent.Line "workgroupBarrier();"]
      else []
      @
      if b.sub_group then
        [Indent.Line "subgroupBarrier();"]
      else []
    | Store {pointer; value}->
      let line =
        Printf.sprintf "%s = %s;"
          (Expression.to_string pointer)
          (Expression.to_string value)
      in
      [Line line]

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
    | Call {function_; arguments; result}
      ->
      let result =
        result
        |> Option.map (fun e -> "let " ^ Ident.to_string e ^ " = ")
        |> Option.value ~default:""
      in
      let arguments =
        arguments
        |> List.map Expression.to_string
        |> Common.join ", "
      in
      [Line (result ^ function_ ^"(" ^ arguments ^ ");")]
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

  let to_string (s:t) : string =
    Indent.to_string (to_s s)
end

module Function = struct
  type t = {
    name: string;
    arguments: FunctionArgument.t list;
    result: FunctionResult.t option;
    locals: LocalDeclaration.t list;
    body: Statement.t list;
  }

  let parse (j:json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* name = with_field "name" cast_string o in (* XXX: in Naga this is an optional string *)
    let* result = with_field "result" (cast_option FunctionResult.parse) o in
    let* arguments = with_field "arguments" (cast_map FunctionArgument.parse) o in
    let* locals = with_field "locals" (cast_map LocalDeclaration.parse) o in
    let* body = with_field "body" (cast_map Statement.parse) o in
    Ok {name; result; arguments; locals; body}


  let to_string (e:t) : string =
    e.name

  let to_s (f:t) : Indent.t list =
    let args =
      f.arguments
      |> List.map FunctionArgument.to_string
      |> Common.join ", "
    in
    let locals : string =
      f.locals
      |> List.map LocalDeclaration.to_string
      |> Common.join ", "
    in
    [
      Line (" fn " ^ f.name ^ "(" ^ args ^ ") {");
      Block (
        (if locals = "" then [] else
          [
            Indent.Line ("var " ^ locals ^ ";");
          ]
        )
        @ Statement.block_to_s f.body
      );
      Line "}"
    ]
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
    let locals : string =
      d.function_.locals
      |> List.map LocalDeclaration.to_string
      |> Common.join ", "
    in
    [
      Line (
        "@" ^ ShaderStage.to_string d.stage ^
        " @workgroup_size(" ^ Dim3.to_string d.workgroup_size ^ ")" ^
        " fn " ^ d.name ^ "(" ^ args ^ ") {"
      );
      Block (
        (if locals = "" then [] else
          [
            Indent.Line ("var " ^ locals ^ ";");
          ]
        )
        @ Statement.block_to_s d.function_.body
      );
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
    binding: ResourceBinding.t option;
    ty: Type.t;
    (* TODO: init *)
  }
  let parse (j:json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* name = with_field "name" cast_string o in
    let* ty = with_field "ty" Type.parse o in
    let* space = with_field "space" AddressSpace.parse o in
    let* binding = with_field "binding" (cast_option ResourceBinding.parse) o in
    Ok {ty; space; name; binding}

  let to_string (d:t) : string =
    d.name

  let to_s (d:t) : Indent.t list =
    let space =
      let space = AddressSpace.to_string d.space in
      if space <> "" then "<" ^ space ^ ">"
      else ""
    in
    let binding =
      d.binding
      |> Option.map (fun x -> " " ^ ResourceBinding.to_string x)
      |> Option.value ~default:""
    in
    [
    Line (binding ^ "var" ^ space ^ " " ^ d.name ^": " ^ Type.to_string d.ty ^";")
    ]
end

module Def = struct
  type t =
    | EntryPoint of EntryPoint.t
    | Declaration of Decl.t
    | Function of Function.t

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
    | "Function" ->
      let* f = Function.parse j in
      Ok (Function f)
    | _ ->
      root_cause ("Def.parse: unknown kind: " ^ kind) j

  let to_string : t -> string =
    function
    | EntryPoint e -> EntryPoint.to_string e
    | Declaration d -> Decl.to_string d
    | Function f -> Function.to_string f

  let to_s : t -> Indent.t list =
    function
    | EntryPoint e -> EntryPoint.to_s e
    | Declaration e -> Decl.to_s e
    | Function f -> Function.to_s f
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
