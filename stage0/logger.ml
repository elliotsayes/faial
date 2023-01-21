module type Logger = sig
  val error: string -> unit
  val warning: string -> unit
  val info: string -> unit
end

module Default : Logger = struct
  let info : string -> unit =
    fun x -> prerr_endline ("INFO: " ^ x)
  let warning : string -> unit =
    fun x -> prerr_endline ("WARNING: " ^ x)
  let error : string -> unit =
    fun x -> prerr_endline ("ERROR: " ^ x)
end

module Colors : Logger = struct
  let info (x: string) : unit =
    let open ANSITerminal in
    prerr_string [Foreground Magenta] ("INFO: " ^ x ^ "\n")

  let warning (x: string) : unit =
    let open ANSITerminal in
    prerr_string [Foreground Yellow] ("WARNING: " ^ x ^ "\n")

  let error (x: string) : unit =
    let open ANSITerminal in
    prerr_string [Bold; Foreground Red] ("ERROR: " ^ x ^ "\n")
end

module Silent : Logger = struct
  let warning : string -> unit = fun (_:string) -> ()
  let info : string -> unit = fun (_:string) -> ()
  let error : string -> unit = fun (_:string) -> ()
end
