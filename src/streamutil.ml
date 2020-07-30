let stream_fold f stream init =
  let result = ref init in
  Stream.iter
    (fun x -> result := f x !result)
    stream;
  !result

let stream_to_list (s:'a Stream.t) : 'a list =
  stream_fold (fun x y -> x :: y) s [] |> List.rev

let stream_map f stream =
    let rec next i =
      try Some (f (Stream.next stream))
      with Stream.Failure -> None in
    Stream.from next

let stream_take (n:int) (s:'a Stream.t) : 'a list =
  let counter = ref n in
  stream_map (fun x ->
    let c = !counter in
    if c <= 0 then
      raise Stream.Failure
    else
      counter := c - 1;
      x
  ) s |> stream_to_list

let stream_make (x:'a option) : 'a  Stream.t = Stream.from (fun _ -> x)

let stream_const k = Stream.from (fun _ -> Some k)

let stream_filter p stream =
  let rec next i =
    try
      let value = Stream.next stream in
      if p value then Some value else next i
    with Stream.Failure -> None in
  Stream.from next


let stream_seq stream1 stream2 =
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

let stream_concat streams =
  stream_fold stream_seq streams (stream_make None)

let stream_combine stream1 stream2 =
    let rec next i =
      try Some (Stream.next stream1, Stream.next stream2)
      with Stream.Failure -> None in
    Stream.from next
