open Ast
open Pos

exception SyntaxError of string

module Scope = Map.Make (String)

type env =
  { module_name : string
  ; exports : (string * int) list
  ; scope : [ `Toplevel | `Local ] Scope.t
  ; session : string
  ; state : int
  }

let make_env module_name =
  { module_name; exports = []; scope = Scope.empty; session = ""; state = 0 }

let prefix_name env name =
  match Scope.find name env.scope with
  | `Toplevel ->
    let id = "quartz_" ^ name in
    begin match List.assoc name env.exports with
      | 0 -> id ^ "()"
      | n -> id
      | exception Not_found -> id
    end
  | `Local -> "Quartz_" ^ name
  | exception Not_found -> "quartz_" ^ name (* Symbol if no ID found? *)

(* FIXME: Should check for arity 0 with imported symbols *)
let rec prefix_identifier env id =
  let rec format_identifier = function
    | Value v -> "quartz_" ^ v
    | Qualify (mod1, Value v) -> mod1 ^ ":quartz_" ^ v
    | Qualify (mod1, Qualify (mod2, id)) -> format_identifier (Qualify (mod1 ^ "." ^ mod2, id)) in
  match id with
  | Value name -> prefix_name env name
  | id -> format_identifier id

let rec name_and_arity = function
  | { stmt_desc = SLet (name, [{ expr_desc = Left (ELam (args, _)) }]) } -> (name, List.length args)
  | { stmt_desc = SLet (name, _::rest) } as s -> name_and_arity { s with stmt_desc = SLet (name, rest) }
  | { stmt_desc = SLet (name, _) } -> (name, 0)
  | { stmt_pos } -> raise @@ SyntaxError ("Invalid top-level declaration on " ^ string_of_pos stmt_pos)

let is_session = function
  | [{ expr_desc = Left (ESession _) }]
  | [{ expr_desc = Left (ELam (_, [{ expr_desc = Left (ESession _) }])) }] -> true
  | _ -> false

let rec emit_expr : type t s. env -> (t, s) expr -> (env * string)
  = fun env expr ->
  match expr.expr_desc with
  | Left desc ->
    begin match desc with
      | ELiteral (Number (n1, n2)) -> (env, string_of_int n1 ^ "." ^ string_of_int n2)
      | ELiteral (Bool b) -> (env, string_of_bool b)
      | ELiteral (String s) -> (env, "\"" ^ s ^ "\"")
      | EIdent id -> (env, prefix_identifier env id)
      | ELet (name, body, context) ->
        let env' = { env with scope = Scope.add name `Local env.scope
                            ; session = if is_session body
                                          then name
                                          else env.session } in
        let txt =
          Printf.sprintf
            {erl|
            Quartz_%s = begin
              %s
            end%s
            %s
            |erl}
            name
            (emit_exprs env' body)
            (if context = [] then "" else ",")
            (emit_exprs env' context) in
        (env, txt)
      | ELam (names, body) ->
        let prefixed_names = List.map (fun s -> "Quartz_" ^ s) names in
        let env' = { env with scope = List.fold_right (fun name scope -> Scope.add name `Local scope) names env.scope } in
        let txt =
          Printf.sprintf
            {erl|
            fun(%s) ->
              %s
            end
            |erl}
            (String.concat "," prefixed_names)
            (emit_exprs env' body) in
        (env', txt)
      | EApp (fn, args) ->
        let txt =
          Printf.sprintf
            {erl|(%s)(%s)|erl}
            (snd @@ emit_expr env fn)
            (emit_exprs env args) in
        (env, txt)
      | ECond (pred, t, f) ->
        let txt =
          Printf.sprintf
            {erl|
            if
              %s -> %s;
              true -> %s
            end
            |erl}
            (snd @@ emit_expr env pred)
            (emit_exprs env t)
            (emit_exprs env f) in
        (env, txt)
      | ESpawn (id, args) ->
        (* FIXME: Support spawning sessions in other modules *)
        let prefixed = prefix_identifier env id in
        let txt =
          Printf.sprintf
            {erl|
            spawn(%s, %s, [%s])
            |erl}
            env.module_name
            prefixed
            (emit_exprs env args) in
        (env, txt)
      | ESession body ->
        let txt =
          Printf.sprintf
            {erl|
            Loop = fun(Loop) ->
              %s
            end,
            Loop(Loop)
            |erl}
            (emit_exprs env body) in
        (env, txt)
    end
  | Right desc ->
    begin match desc with
      | EClose -> (env, "exit(normal)") 
      | ELoop (None) -> (env, "Loop(Loop)")
      | ELoop (Some args) ->
        let fn = env.session in
        let txt =
          Printf.sprintf {erl|%s(%s)|erl}
            (prefix_name env fn)
            (emit_exprs env args) in
        (env, txt)
      | ESend (id, msg, args, context) ->
        let prefixed = prefix_identifier env id in
        let txt =
          Printf.sprintf
            {erl|
            %s!{%s, {%s}, self()}%s
            %s
            |erl}
            prefixed
            (prefix_name env msg)
            (emit_exprs env args)
            (if context = [] then "" else ",")
            (emit_exprs env context) in
        (env, txt)
      | EBranch (id, branches) ->
        begin match Scope.find id env.scope with
          | `Local ->
            let env' = { env with state = env.state + 1 } in
            let emit_branch (msg, names, body) =
              let env'' = { env' with scope = List.fold_right (fun name scope -> Scope.add name `Local scope) names env'.scope } in
              Printf.sprintf
                {erl|
                {quartz_%s, {%s}, Ignore_%s} ->
                  %s
                |erl}
                msg
                (String.concat "," @@ List.map (fun name -> "Quartz_" ^ name) names)
                (string_of_int env.state)
                (emit_exprs env'' body) in
            let txt =
              Printf.sprintf
                {erl|
                receive
                  %s
                end
                |erl}
                (String.concat ";\n" @@ List.map emit_branch branches) in
            (env', txt)
          | `Toplevel ->
            let env' = { env with scope = Scope.add id `Local env.scope } in
            let emit_branch (msg, names, body) =
              let env'' = { env' with scope = List.fold_right (fun name scope -> Scope.add name `Local scope) names env'.scope } in
              Printf.sprintf
                {erl|
                {quartz_%s, {%s}, Quartz_%s} ->
                  %s
                |erl}
                msg
                (String.concat "," @@ List.map (fun name -> "Quartz_" ^ name) names)
                id
                (emit_exprs env'' body) in
            let txt =
              Printf.sprintf
                {erl|
                receive
                  %s
                end
                |erl}
                (String.concat ";\n" @@ List.map emit_branch branches) in
            (env', txt)
        end
    end

and emit_exprs : type t s. env -> (t, s) expr list -> string
  = fun env -> function
  | [] -> ""
  | [expr] -> snd (emit_expr env expr)
  | expr::exprs ->
    let (env', txt) = emit_expr env expr in
    txt ^ ",\n" ^ emit_exprs env' exprs

let rec emit_stmt env stmt =
  match stmt.stmt_desc with
  | SOpen _ -> (env, "")
  | SModule (name, body) ->
    let env' = { env with module_name = name } in
    (env', emit_stmts env' body)
  | SLet (name, es) ->
    let (params, body, env) =
      let env' = { env with scope = Scope.add name `Toplevel env.scope } in
      match es with
      | [{ expr_desc = Left (ELam (names, body)) }] ->
        let prefixed_names = List.map (fun s -> "Quartz_" ^ s) names in
        let env'' = { env' with scope = List.fold_right (fun name scope -> Scope.add name `Local scope) names env'.scope
                            ; session = if is_session body
                                          then name
                                          else env'.session } in
        (String.concat "," prefixed_names, emit_exprs env'' body, env')
      | es -> ("", emit_exprs env' es, env') in
    let txt =
      Printf.sprintf
        {erl|
        quartz_%s(%s) ->
          %s.
        |erl}
        name
        params
        body in
    (env, txt)

and emit_stmts env = function
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
