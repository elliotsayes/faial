open Stage0

module Reason = struct
  type t =
    | UnexpectedProcessStatus of Unix.process_status
    | UnexpectedOutput
    | UnsupportedInput

  let to_string : t -> string =
    function
    | UnexpectedProcessStatus r -> "Unexpected process status: " ^ Subprocess.process_status_to_string r
    | UnexpectedOutput -> "Could not parse tool's output"
    | UnsupportedInput -> "Cannot handle the given RA problem"

end

type t = {output: string; reason: Reason.t}

let to_string (err:t) : string =
  Reason.to_string err.reason ^
  "\nProcess output:\n" ^ err.output

let handle_result (parse_data:string -> string option) ((r:Unix.process_status), (data:string)) : (string, t) Result.t =
  if r = Unix.WEXITED 0 then
    match parse_data data with
    | Some data -> Ok data
    | None -> Error {output=data; reason=UnexpectedOutput}
  else
    Error {output=data; reason=UnexpectedProcessStatus r}

