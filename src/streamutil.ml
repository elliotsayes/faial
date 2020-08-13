let fold (f:'a -> 'b -> 'b) (stream:'a Stream.t) (init:'b) : 'b =
  let result = ref init in
  Stream.iter
    (fun x -> result := f x !result)
    stream;
  !result

let to_list (s:'a Stream.t) : 'a list =
  fold (fun x y -> x :: y) s [] |> List.rev

let map (f:'a -> 'b) (stream: 'a Stream.t) : 'b Stream.t =
    let rec next i =
      try Some (f (Stream.next stream))
      with Stream.Failure -> None in
    Stream.from next

let take (n:int) (s:'a Stream.t) : 'a list =
  let counter = ref n in
  map (fun x ->
    let c = !counter in
    if c <= 0 then
      raise Stream.Failure
    else
      counter := c - 1;
      x
  ) s |> to_list

let make (x:'a option) : 'a  Stream.t = Stream.from (fun _ -> x)

let always (k:'a) : 'a Stream.t = Stream.from (fun _ -> Some k)

let one (x:'a) : 'a Stream.t = Stream.of_list [x]

let filter (p:'a -> bool) (stream:'a Stream.t) : 'a Stream.t =
  let rec next i =
    try
      let value = Stream.next stream in
      if p value then Some value else next i
    with Stream.Failure -> None in
  Stream.from next

let map_opt (f:'a -> 'b option) (stream: 'a Stream.t) : 'b Stream.t =
  map f stream
  |> filter (function
    | Some _ -> true
    | None -> false
  )
  |> map (function
    | Some x -> x
    | None -> failwith "unexpected"
  )

let sequence (stream1:'a Stream.t) (stream2:'a Stream.t) : 'a Stream.t =
  let pop s =
    try
      Some (Stream.next s)
    with Stream.Failure -> None
  in
  let handle_first = ref true in
  Stream.from (fun i ->
    if !handle_first then
      try
        Some (Stream.next stream1)
      with Stream.Failure -> (
        (* First stream is empty, try again *)
        handle_first := false;
        pop stream2
      )
    else
      pop stream2
  )

let concat (streams:'a Stream.t Stream.t) : 'a Stream.t =
  fold sequence streams (make None)

let zip (stream1:'a Stream.t) (stream2:'b Stream.t) : ('a * 'b) Stream.t =
    let rec next i =
      try Some (Stream.next stream1, Stream.next stream2)
      with Stream.Failure -> None in
    Stream.from next

let flat_map (f:'a -> 'b Stream.t) (stream: 'a Stream.t) : 'b Stream.t =
  map f stream |> concat

(* Consumes the first stream entirely *)
let product (s1:'a Stream.t) (s2:'b Stream.t) : ('a * 'b) Stream.t =
  let xs:'a list = to_list s1 in
  s2
  |> map (fun y ->
    List.map (fun x -> (x, y)) xs |> Stream.of_list
  )
  |> concat
