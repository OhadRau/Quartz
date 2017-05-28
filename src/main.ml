open Ast
open Pos
open Type
open Emit

let pos = { file_name = "main.ml"
          ; line_number = 0
          ; column_number = 0
          }

let mk_ast ast =
  { ast_desc = ast
  ; ast_src = None
  ; ast_pos = pos
  }

let mk_stmt stmt =
  { stmt_desc = stmt
  ; stmt_type = Nothing
  ; stmt_pos = pos
  ; stmt_src = None
  }

let mk_expr expr =
  { expr_desc = Left expr
  ; expr_type = Nothing
  ; expr_pos = pos
  ; expr_src = None
  }

let mk_sesh sesh =
  { expr_desc = Right sesh
  ; expr_type = Nothing
  ; expr_pos = pos
  ; expr_src = None
  }

let () =
  let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
  let ast =
    try Parser.ast Lexer.token lexbuf
    with exn ->
      let tok = Lexing.lexeme lexbuf in
      print_endline tok;
      raise exn in
  print_endline @@ string_of_ast ast;
  let ast =
    mk_ast
      [ mk_stmt @@ SLet ("Client",
         [ mk_expr @@ ELam (["server"],
             [ mk_expr @@ ESession
                 ([ mk_sesh @@ ESend (Value "server", "Hello", [],
                      [ mk_sesh @@ EBranch ("server",
                          [ ("Ready", ["my_id"],
                              [ mk_sesh EClose
                              ])
                          ])
                      ])
                 ])
             ])
         ])
      ; mk_stmt @@ SLet ("main",
         [ mk_expr @@ ELam (["f"; "x"],
           [ mk_expr @@ ESpawn (Value "Client",
               [ mk_expr @@ EIdent (Value "x")
               ])
           ])
         ])
      ] in
  print_endline @@ string_of_ast ast;
  print_endline @@ emit (make_env "main") ast
