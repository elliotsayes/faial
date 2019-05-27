module StringOT = struct
  type t = string
  let compare = Pervasives.compare
end

module StringSet = Set.Make(StringOT)
