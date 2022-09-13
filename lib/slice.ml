(* Accepts Python-slice expression:
  >> "asdf"[1:-1]
  "sd"
*)
type t = {start:int; finish:int option}
let empty = {start=0; finish=None}
let from_start start : t = {start=start; finish=None}
let from_finish finish : t = {start=0; finish=Some finish}
(* Specify start offset and finish offset *)
let from ~start ~finish : t = {start=start; finish=Some finish}
let make ~start ~finish : t = {start=start; finish=finish}
type interval = {index:int; length:int}
let to_interval (total_len:int) (s:t) : interval =
  (* start: *)
  let start = s.start in
  let start = if start < 0 then (total_len + start) else start in
  let start = min start (total_len - 1) in
  let start = max start 0 in
  (* finish: *)
  let finish = s.finish in
  let finish = match finish with
  | None -> total_len
  | Some n when n < 0 -> total_len + n
  | Some n -> n
  in
  let finish = min finish total_len in
  let finish = max finish 0 in
  (* Convert the offset finish to a length *)
  let length = max (finish - start) 0 in
  {index=start; length=length}

let string (x:string) (s:t) : string =
  let o = to_interval  (String.length x) s in
  String.sub x o.index o.length
