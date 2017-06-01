type t =
  | TVar of int
  | TApp of string * t list
  | TArrow of t * t
  | TQuant of string * limit option
  | TImplicit of string * t
  | TNamed of string * t
  | TRec of string * t
  | TOffer of string * (string * t * t) list
  | TChoose of string * (string * t * t) list

and limit =
  | LVar of int
  | LApp of string * limit list
  | LArrow of limit * limit
  | LRec of string * limit
  | LOffer of string * (string * limit * limit) list
  | LChoose of string * (string * limit * limit) list
