(* Static maybe type:
   ('a, yes)   static_maybe = 'a
   ('a, maybe) static_maybe = 'a option *)
type yes and maybe
type ('a, 'b) static_maybe =
  | Just : 'a -> ('a, 'b) static_maybe
  | Nothing : ('a, maybe) static_maybe

(* Static either type:
   ('a, 'b, right)  static_either = 'b
   ('a, 'b, either) static_either = ('a, 'b) result *)
type right and either
type ('a, 'b, 'c) static_either =
  | Right : 'b -> ('a, 'b, 'c) static_either
  | Left : 'a -> ('a, 'b, either) static_either

(* Contextual names for static maybe values *)
type typed = yes and untyped = maybe

(* Contextual names for static either values *)
type normal = either and session = right

type literal =
  | Number of float
  | Bool of bool
  | String of string

type identifier =
  | Value of string
  | Qualify of string * identifier

type ('is_typed, 'is_session) expr =
  { expr_desc  : (('is_typed, 'is_session) expr_desc, 'is_typed session_desc, 'is_session) static_either
  ; expr_type_ : (Type.t, 'is_typed) static_maybe
  ; expr_pos   : (int * int)
  ; expr_src   : string option
  }

and ('is_typed, 'is_session) expr_desc =
  | ELiteral of literal
  | EIdent of identifier
  | ELet of string * ('is_typed, 'is_session) expr list * ('is_typed, 'is_session) expr list
  | ELam of string list * ('is_typed, 'is_session) expr list
  | EApp of ('is_typed, 'is_session) expr * ('is_typed, 'is_session) expr list
  | ECond of ('is_typed, 'is_session) expr * ('is_typed, 'is_session) expr list * ('is_typed, 'is_session) expr list
  | ESpawn of identifier * ('is_typed, 'is_session) expr list
  | ESession of ('is_typed, session) expr list

and 'is_typed session_desc =
  | ELoop
  | EClose
  | ESend of identifier * string * ('is_typed, session) expr list * ('is_typed, session) expr list
  | EBranch of string * string list * string * ('is_typed, session) expr list

type 'is_typed stmt =
  { stmt_desc  : 'is_typed stmt_desc
  ; stmt_type_ : (Type.t, 'is_typed) static_maybe
  ; stmt_pos   : (int * int)
  ; stmt_src   : string option
  }

and 'is_typed stmt_desc =
  | SOpen of identifier
  | SModule of string * 'is_typed stmt list
  | SLet of string * ('is_typed, normal) expr list

type 'is_typed ast =
  { ast_desc : 'is_typed stmt list
  ; ast_src  : string option
  }

let indent_string n str =
  String.make n ' ' ^ str

let string_of_literal = function
  | Number n -> string_of_float n
  | Bool b -> string_of_bool b
  | String s -> s

let rec string_of_identifier = function
  | Value s -> s
  | Qualify (s, id) -> s ^ "." ^ string_of_identifier id

let rec string_of_expr : type t s. ?indent:int -> (t, s) expr -> string =
  fun ?(indent=0) -> function
    | { expr_src = Some txt } -> txt
    | { expr_desc = Left desc } ->
      begin match desc with
        | ELiteral lit -> indent_string indent (string_of_literal lit)
        | EIdent id -> indent_string indent (string_of_identifier id)
        | ELet (name, body, context) ->
          indent_string indent ("let " ^ name ^ " =\n  ") ^
          String.concat "\n  " (List.map (string_of_expr ~indent) body) ^ "\n" ^
          indent_string indent "in\n" ^
          String.concat "\n" (List.map (string_of_expr ~indent) body)
        | ELam (names, body) ->
          indent_string indent ("| " ^ String.concat ", " names ^ " |\n  ") ^
          String.concat "\n  " (List.map (string_of_expr ~indent) body) ^ "\n" ^
          indent_string indent "end"
        | EApp (f, args) ->
          indent_string indent (string_of_expr f ^ "(" ^ String.concat ", " (List.map string_of_expr args) ^ ")")
        | ECond (pred, t, f) ->
          indent_string indent ("if " ^ string_of_expr pred ^ " then\n  ") ^
          String.concat "\n  " (List.map (string_of_expr ~indent) t) ^ "\n" ^
          indent_string indent "else\n  " ^
          String.concat "\n  " (List.map (string_of_expr ~indent) f) ^ "\n" ^
          indent_string indent "end"
        | ESpawn (id, args) ->
          indent_string indent (string_of_identifier id ^ "(" ^ String.concat ", " (List.map string_of_expr args) ^ ")")
        | ESession body ->
          indent_string indent "session\n  " ^
          String.concat "\n  " (List.map (string_of_expr ~indent) body) ^ "\n" ^
          indent_string indent "end"
      end
    | { expr_desc = Right desc } ->
      begin match desc with
        | ELoop -> indent_string indent "loop"
        | EClose -> indent_string indent "close"
        | ESend (id, msg, args, context) ->
          indent_string indent (string_of_identifier id ^ "!" ^ msg ^ "(" ^ String.concat ", " (List.map string_of_expr args) ^ ")") ^ "\n" ^
          String.concat "\n" (List.map (string_of_expr ~indent) context)
        | EBranch (msg, args, id, body) ->
          indent_string indent ("branch " ^ msg ^ "(" ^ String.concat ", " args ^ ") from " ^ id) ^ "\n  " ^
          String.concat "\n  " (List.map (string_of_expr ~indent) body) ^ "\n" ^
          indent_string indent "end"
      end

let rec string_of_stmt : type t. ?indent:int -> t stmt -> string =
  fun ?(indent=0) -> function
    | { stmt_src = Some txt } -> txt
    | { stmt_desc } ->
      begin match stmt_desc with
        | SOpen id -> indent_string indent ("open " ^ string_of_identifier id)
        | SModule (name, body) ->
          indent_string indent ("module " ^ name ^ "\n  ") ^
          String.concat "\n  " (List.map (string_of_stmt ~indent) body) ^ "\n" ^
          indent_string indent "end"
        | SLet (name, body) ->
          indent_string indent ("let " ^ name ^ " =\n  ") ^
          String.concat "\n  " (List.map (string_of_expr ~indent) body)
      end

let string_of_ast ?(indent=0) = function
  | { ast_src = Some txt } -> txt
  | { ast_desc } ->
    String.concat "\n" (List.map (string_of_stmt ~indent) ast_desc)
