open Protocols

type t = {
  block_dim: Dim3.t;
  grid_dim: Dim3.t;
  threads_per_warp: int;
  bank_count: int;
  bytes_per_word: int;
}

let total_blocks (cfg:t) : int =
  Dim3.total cfg.grid_dim

let total_threads (cfg:t) : int =
  Dim3.total cfg.block_dim * total_blocks cfg

let total_warps (cfg:t) : int =
  total_threads cfg / cfg.threads_per_warp

let make
  ?(bank_count=32)
  ?(threads_per_warp=32)
  ?(bytes_per_word=4)
  ~block_dim
  ~grid_dim
  ()
:
  t
=
  {block_dim; grid_dim; threads_per_warp; bank_count; bytes_per_word}
