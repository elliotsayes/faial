open Stage0

type t =
  | UnexpectedProcessStatus of Unix.process_status
  | UnexpectedOutput of string

let to_string : t -> string =
  function
  | UnexpectedProcessStatus r -> "Unexpected process status: " ^ Common.process_status_to_string r
  | UnexpectedOutput s -> "Could not parse output:\n" ^ s

let handle_result (parse_data:string -> string option) ((r:Unix.process_status), (data:string)) : (string, t) Result.t =
  if r = Unix.WEXITED 0 then
    match parse_data data with
    | Some data -> Ok data
    | None -> Error (UnexpectedOutput data)
  else
    Error (UnexpectedProcessStatus r)

