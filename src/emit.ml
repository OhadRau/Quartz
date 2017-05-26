open Ast

exception SyntaxError of int * int * string

module Scope = Map.Make (String)

type env =
  { module_name : string
  ; exports : (string * int) list
  ; scope : [ `Toplevel | `Local ] Scope.t
  ; state : int
  }

let make_env module_name =
  { module_name; exports = []; scope = Scope.empty; state = 0 }

let prefix_name env name =
  match Scope.find name env.scope with
  | `Toplevel -> "quartz_" ^ name
  | `Local -> "Quartz_" ^ name

let rec name_and_arity : 't stmt -> string * int = function
  | { stmt_desc = SLet (name, [{ expr_desc = Left (ELam (args, _)) }]) } -> (name, List.length args)
  | { stmt_desc = SLet (name, _::rest) } as s -> name_and_arity { s with stmt_desc = SLet (name, rest) }
  | { stmt_desc = SLet (name, _) } -> (name, 0)
  | { stmt_pos = (l, c) } -> raise @@ SyntaxError (l, c, "Invalid top-level declaration")

let rec emit_expr env = function
  | _ -> (env, "")

let rec emit_exprs env = function
  | [] -> ""
  | expr::exprs ->
    let (env', txt) = emit_expr env expr in
    txt ^ "\n" ^ emit_exprs env' exprs

let emit_stmt env stmt =
  match stmt.stmt_desc with
  | SOpen _ -> (env, "")
  | SModule (_, _) -> (env, "")
  | SLet (name, es) ->
    let (params, body, env) =
      match es with
      | [{ expr_desc = Left (ELam (names, body)) }] ->
        let prefixed_names = List.map (fun s -> "Quartz_" ^ s) names in
        let env' = { env with scope = List.fold_right (fun name scope -> Scope.add name `Local scope) names env.scope } in
        (String.concat "," prefixed_names, emit_exprs env' body, env')
      | es -> ("", emit_exprs env es, env) in
    let txt =
      Printf.sprintf
        {erl|
          quartz_%s(%s) ->
            %s.
        |erl}
        name
        params
        body in
    ({ env with scope = Scope.add name `Toplevel env.scope}, txt)

let rec emit_stmts env = function
  | [] -> ""
  | stmt::stmts ->
    let (env', txt) = emit_stmt env stmt in
    txt ^ "\n" ^ emit_stmts env' stmts

let emit env { ast_desc = ast } =
  let exports =
    let is_decl = function
      | { stmt_desc = SLet (_, _) } -> true
      | _ -> false in
    List.filter is_decl ast |> List.map name_and_arity in
  let env = { env with exports } in
  Printf.sprintf
    {erl|
    -module(%s).
    -export([%s]).

    %s
    |erl}
    env.module_name
    (exports |> List.map (fun (name, arity) -> "quartz_" ^ name ^ "/" ^ string_of_int arity) |> String.concat ",")
    (emit_stmts env ast)
