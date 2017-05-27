open Ast
open Type
open Emit

let mk_ast ast =
  { ast_desc = ast
  ; ast_src = None
  }

let mk_stmt stmt =
  { stmt_desc = stmt
  ; stmt_type_ = Nothing
  ; stmt_pos = (0, 0)
  ; stmt_src = None
  }

let mk_expr expr =
  { expr_desc = Left expr
  ; expr_type_ = Nothing
  ; expr_pos = (0, 0)
  ; expr_src = None
  }

let mk_sesh sesh =
  { expr_desc = Right sesh
  ; expr_type_ = Nothing
  ; expr_pos = (0, 0)
  ; expr_src = None
  }

let () =
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
