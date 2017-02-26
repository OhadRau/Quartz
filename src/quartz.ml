open Batteries

let compile file =
  let src = IO.read_all @@ File.open_in file in
  print_endline src

let () =
  Array.iteri (fun i f -> if i <> 0 then compile f) Sys.argv
