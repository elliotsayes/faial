open Protocols

type t = {
  block_dim: Dim3.t;
  grid_dim: Dim3.t;
  num_banks: int;
  warp_count: int;
  bytes_per_word: int;
}

let make ?(num_banks=32) ?(warp_count=32) ?(bytes_per_word=4) ~block_dim ~grid_dim  () : t =
  {block_dim; grid_dim; warp_count; num_banks; bytes_per_word}
