open Type
open Ident

(* Statements/expressions which are only valid at the top-level *)
type toplevel =
  (* Any expression that is valid at the top-level *)
  [ `TopExpr of top_expr
  (* Require statements (require M.X) *)
  | `Require of modident
  (* Module declarations (module M ... end) *)
  | `Module of string * toplevel list
  (* Native function declarations (native print : string -> unit = "std:print") *)
  | `Native of string * ty * string
  ]

(* Expressions which are also valid at the top-level *)
and top_expr =
  (* Let expressions (let x = ...) *)
  [ `Let of string * expr typed
  (* Function declarations (fun f x ... end) *)
  | `Fun of string * string typed list * expr typed
  (* Session declarations (session S x ... end) *)
  | `Session of string * session_toplevel typed list
  ]

and expr =
  (* Any expression that is valid at the top-level *)
  [ `TopExpr of top_expr
  (* Literal values (-5, 1.2, "hello") *)
  | `Literal of literal typed
  (* Value-level identifiers (my_function) *)
  | `Ident of valident typed
  (* Conditional expressions (if p ... else ... end) *)
  | `If of expr typed * expr typed * expr typed
  (* Function application (f 1 2) *)
  | `Apply of expr typed * expr typed
  (* Message sending (x!f 1 2) *)
  | `Send of expr typed * expr typed * expr typed
  (* Anonymous functions (do |x| ... end) *)
  | `Lambda of string typed list * expr typed
  (* Session spawning (spawn X 1) *)
  | `Spawn of modident typed * expr typed list
  (* Sequence blocks (begin ... end) *)
  | `Sequence of expr typed list
  ]

(* Literal values *)
and literal =
  [ `Bool of bool
  | `Integer of int
  | `Decimal of int * int
  | `String of string
  ]

(* Expressions that can occur only inside of sessions *)
and session_only =
  (* Branch declarations (branch X ... end) *)
  [ `Branch of string * string typed list * session_expr typed
  ]

(* Statements that can occur only at the session top-level *)
and session_toplevel =
  (* Any expression that is valid anywhere in a session *)
  [ `SessionOnly of session_only
  (* Initialization blocks (initialize ... end) *)
  | `Initialize of session_expr typed list
  ]

(* Expressions that can occur inside of sessions *)
and session_expr =
  (* Any expression *)
  [ `Expr expr
  (* Any expression that is valid anywhere in a session *)
  | `SessionOnly session_only
  ]
