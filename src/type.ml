open Ident

type ty =
  (* Type variable (a) *)
  | TVar of string
  (* Constant type (Int) *)
  | TConst of valident
  (* Function type (a -> b) *)
  | TArrow of ty * ty
  (* Universal quantification (forall a. a) *)
  | TQuant of string * ty

type 'a typed =
  { ty : ty
  ; node : 'a
  }
