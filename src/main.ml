open Ast
open Emit

let () =
  let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
  let ast =
    try Parser.ast Lexer.token lexbuf
    with exn ->
      let tok = Lexing.lexeme lexbuf in
      print_endline tok;
      raise exn in
  print_endline @@ "AST:\n" ^ string_of_ast ast;
  print_endline @@ "Output:\n" ^ emit (make_env Filename.(Sys.argv.(1) |> basename |> chop_extension)) ast
