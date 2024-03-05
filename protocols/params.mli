type t
val empty : t
val union_left : t -> t -> t
val union_right : t -> t -> t
val remove_all : Variable.Set.t -> t -> t
val retain_all : Variable.Set.t -> t -> t
val to_string : t -> string
val from_set: C_type.t -> Variable.Set.t -> t
val to_set : t -> Variable.Set.t
val add: Variable.t -> C_type.t -> t -> t
val from_list: (Variable.t * C_type.t) list -> t
val mem : Variable.t -> t -> bool
val to_bexp : t -> Exp.bexp
