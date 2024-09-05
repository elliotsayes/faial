open Stage0

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

  let to_string (s: t) : string =
    to_int s |> string_of_int

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
  type t = string
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
    | _ -> failwith ("inner_parse: unsupported kind: " ^ kind)

end

module FunctionResult = struct
  type t = {
    ty: Type.t;
    binding: Binding.t;
  }
end

module FunctionArgument = struct
  type t = {
    name: string;
    ty: Type.t;
    binding: Binding.t option
  }
end

module Function = struct
  type t = {
    name: string option;
    arguments: FunctionArgument.t list;
    result: FunctionResult.t option;
    (* TODO:    body *)
  }
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
(* TODO: function_: Function.t; *)
    (*
    TODO: workgroup_size: Dim3.t;
    TODO: early_depth_test: EarlyDepthTest.t option;
    *)
    stage: ShaderStage.t;
  }

  let parse (j:json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* name = with_field "name" cast_string o in
    let* stage = with_field "stage" ShaderStage.parse o in
    Ok {name; stage}

  let to_string (e:t) : string =
    e.name

  let to_s (d:t) : Indent.t list =
    [
    Line ("@" ^ ShaderStage.to_string d.stage ^ " fn " ^ d.name)
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
