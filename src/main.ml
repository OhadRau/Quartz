open Ast
open Type
open Emit

let () =
  let ast =
    { ast_desc =
        [{ stmt_desc =
             SLet ("main", [{ expr_desc = Left (EIdent (Value "hi"))
                            ; expr_type_ = Nothing
                            ; expr_pos = (0, 11)
                            ; expr_src = None
                            }])
         ; stmt_type_ = Nothing
         ; stmt_pos = (0, 0)
         ; stmt_src = None
         }]
    ; ast_src = None
    } in
  print_endline @@ string_of_ast ast;
  print_endline @@ emit (make_env "main") ast
