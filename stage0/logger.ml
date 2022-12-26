module type Logger = sig
  val warning: string -> unit
end

module Default = struct
  let warning : string -> unit =
    fun x -> prerr_endline ("WARNING: " ^ x)
end

module Silent = struct
  let warning : string -> unit =
    fun (_:string) -> ()
end
