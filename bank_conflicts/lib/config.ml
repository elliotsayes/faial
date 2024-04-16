open Protocols

type t = {
  block_dim: Dim3.t;
  grid_dim: Dim3.t;
  num_banks: int;
  warp_count: int;
  word_size: int;
}

let make ?(num_banks=32) ?(warp_count=32) ?(word_size=4) ~block_dim ~grid_dim  () : t =
  {block_dim; grid_dim; warp_count; num_banks; word_size}
