type t =
  | TVar of int
  | TApp of string * t list
  | TArrow of t list * t
  | TQuant of int * limit option * t
  | TImplicit of string * t * t
  | TNamed of string * t
  | TRec of int * t
  | TOffer of string * (string * t * t) list
  | TChoose of string * (string * t * t) list

and limit =
  | LVar of int
  | LApp of string * limit list
  | LArrow of limit list * limit
  | LRec of string * limit
  | LOffer of (string * limit * limit) list
  | LChoose of (string * limit * limit) list

let rec string_of_type = function
  | TVar i -> "_" ^ string_of_int i
  | TApp (t, args) -> t ^ "(" ^ String.concat ", " (List.map string_of_type args) ^ ")"
  | TArrow (t, u) ->
    let params = String.concat ", " @@ List.map string_of_type t in
    "(" ^ params ^ ") -> " ^ string_of_type u
  | TQuant (v, None, t) -> "forall _" ^ string_of_int v ^ ". " ^ string_of_type t
  | TQuant (v, Some l, t) -> "forall _" ^ string_of_int v ^ " < ..." ^ string_of_limit l ^ ". " ^ string_of_type t
  | TImplicit (s, t, u) -> "[" ^ s ^ " : " ^ string_of_type t ^ "] " ^ string_of_type u
  | TNamed (s, t) -> "(" ^ s ^ " : " ^ string_of_type t ^ ")"
  | TRec (v, t) -> "rec _" ^ string_of_int v ^ ". " ^ string_of_type t
  | TOffer (w, offers) -> "?" ^ w ^ "{" ^ String.concat ", " (List.map string_of_toffer offers) ^ "}"
  | TChoose (w, offers) -> "!" ^ w ^ "{" ^ String.concat ", " (List.map string_of_toffer offers) ^ "}"

and string_of_limit = function
  | LVar i -> "_" ^ string_of_int i
  | LApp (t, args) -> t ^ "(" ^ String.concat ", " (List.map string_of_limit args) ^ ")"
  | LArrow (t, u) ->
    let params = String.concat ", " @@ List.map string_of_limit t in
    "(" ^ params ^ ") -> " ^ string_of_limit u
  | LRec (v, t) -> "rec _" ^ v ^ ". " ^ string_of_limit t
  | LOffer offers -> "?{" ^ String.concat ", " (List.map string_of_loffer offers) ^ "}"
  | LChoose offers -> "!{" ^ String.concat ", " (List.map string_of_loffer offers) ^ "}"

and string_of_toffer (msg, t1, t2) =
  msg ^ string_of_type t1 ^ " ~> " ^ string_of_type t2

and string_of_loffer (msg, l1, l2) =
  msg ^ string_of_limit l1 ^ " ~> ..." ^ string_of_limit l2
