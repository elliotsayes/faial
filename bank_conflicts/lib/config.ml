open Protocols

type t = {
  block_dim: Dim3.t;
  grid_dim: Dim3.t;
  threads_per_warp: int;
  bank_count: int;
  bytes_per_word: int;
}

let make ?(bank_count=32) ?(threads_per_warp=32) ?(bytes_per_word=4) ~block_dim ~grid_dim  () : t =
  {block_dim; grid_dim; threads_per_warp; bank_count; bytes_per_word}
