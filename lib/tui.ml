let make_bold =
   ANSITerminal.sprintf [ANSITerminal.Bold; ANSITerminal.Foreground ANSITerminal.Magenta] "%s"

module LocationUI = struct
  let bprint (outx:Buffer.t) (loc:Location.t) : unit =
    let line = Location.read_line loc in
    let (left, mid, right) =
      line
      |> Interval.split (Location.interval loc)
    in
    let lineno = Location.line loc |> Index.to_base1 in
    let prefix = Printf.sprintf "%d | " lineno in
    Printf.bprintf outx "%s%s%s%s\n" prefix left (make_bold mid) right

  let print (l:Location.t) : unit =
    let out = Buffer.create 1024 in
    bprint out l;
    Buffer.output_buffer stdout out

end

let bprint_errors b (errs:(string * Location.t option) list) : bool =
  let print_err (msg,loc:string * Location.t option) =
    match loc with
    | Some loc -> Printf.bprintf b "%s %s" (Location.to_string loc) msg;
      (try
          (Printf.bprintf b "\n\n%a" LocationUI.bprint loc)
        with Sys_error _ -> ())
    | None -> Printf.bprintf b "%s" msg
  in
  List.iter print_err errs;
  List.length errs > 0
