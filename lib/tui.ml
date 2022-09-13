
let make_bold =
   ANSITerminal.sprintf [ANSITerminal.Bold; ANSITerminal.Foreground ANSITerminal.Red] "%s"

module LocationUI = struct
  let bprint (outx:Buffer.t) (loc:Location.t) : unit =
    let (left, mid, right) = Location.get_line loc |> Location.split loc in
    Printf.bprintf outx "%d | %s%s%s\n" loc.line left (make_bold mid) right;

    (* Underline the highlighted text *)
    let spaces =
      let count = Printf.sprintf "%d | " loc.line |> String.length in
      String.make count ' '
    in
    let underline offset count : string =
      let count = if count = 0 then 1 else count in
      (String.make offset ' ') ^ (String.make count '^' |> make_bold)
    in
    Printf.bprintf outx "%s%s\n" spaces (underline (Location.column_offset loc) loc.length)

  let print (l:Location.t) : unit =
    let out = Buffer.create 1024 in
    bprint out l;
    Buffer.output_buffer stdout out

end

let bprint_errors b (errs:(string * Location.t option) list) : bool =
  let print_err (msg,loc:string * Location.t option) =
    match loc with
    | Some loc -> Printf.bprintf b "%s:%d:%d %s" loc.filename loc.line loc.column msg;
      (try
          (Printf.bprintf b "\n\n%a" LocationUI.bprint loc)
        with Sys_error _ -> ())
    | None -> Printf.bprintf b "%s" msg
  in
  List.iter print_err errs;
  List.length errs > 0
