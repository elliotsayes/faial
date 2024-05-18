open Protocols

type t = {
  block_dim: Dim3.t;
  grid_dim: Dim3.t;
  bank_count: int;
  warp_count: int;
  bytes_per_word: int;
}

let make ?(bank_count=32) ?(warp_count=32) ?(bytes_per_word=4) ~block_dim ~grid_dim  () : t =
  {block_dim; grid_dim; warp_count; bank_count; bytes_per_word}
