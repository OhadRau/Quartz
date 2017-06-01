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
  print_endline @@ "Output:\n" ^ emit (make_env Filename.(Sys.argv.(1) |> basename |> chop_extension)) ast;
  print_endline @@ "Type:\n" ^ Type.(begin
    string_of_type @@
      TQuant (0, Some (LChoose [ "Start", LApp ("()", []),
                                     LOffer [ "Ok", LApp ("()", []), LApp ("Eps", [])
                                            ; "Err", LApp ("()", [LApp ("String", [])]), LApp ("Eps", [])
                                            ]
                               ]),
        TRec (1,
          TImplicit ("client", TVar 0,
            TOffer ("client", [ "Start", TApp ("()", []),
              TChoose ("client", [ "Ok", TApp ("()", []), TVar 1
                                 ; "Err", TApp ("()", [TApp ("String", [])]), TVar 1
                                 ])
                              ]))))
  end)
