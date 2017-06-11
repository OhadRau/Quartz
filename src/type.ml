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

(* Get the names of all sessions that a type communicates with *)
let neighbors t =
  let module S = Set.Make(String) in
  let fold_set f xs =
    List.fold_right (fun x s -> S.union s (f x)) xs S.empty in
  let rec traverse = function
    | TVar _ -> S.empty
    | TApp (_, xs) -> fold_set traverse xs
    | TArrow (xs, y) -> S.union (fold_set traverse xs) (traverse y)
    | TQuant (_, _, t) -> traverse t
    | TImplicit (_, t, c) -> S.union (traverse t) (traverse c)
    | TNamed (_, t) -> traverse t
    | TRec (_, t) -> traverse t
    | TOffer (w, offers) | TChoose (w, offers) ->
      S.add w (fold_set (fun (_, t, c) -> S.union (traverse t) (traverse c)) offers) in
  traverse t |> S.elements

(* Make a session type binary with respect to the session named w *)
let rec binary w = function
  | TVar i -> TVar i
  | TApp (s, ts) -> TApp (s, List.map (binary w) ts)
  | TArrow (xs, y) -> TArrow (List.map (binary w) xs, binary w y)
  | TQuant (i, l, t) -> TQuant (i, l, binary w t)
  | TImplicit (s, x, y) -> TImplicit (s, binary w x, binary w y)
  | TNamed (s, t) -> TNamed (s, binary w t)
  | TRec (i, t) -> TRec (i, binary w t)
  | TOffer (s, offers) when s = w ->
    TOffer (w, List.map (fun (n, t, c) -> (n, binary w t, binary w c)) offers)
  | TChoose (s, offers) when s = w ->
    TChoose (w, List.map (fun (n, t, c) -> (n, binary w t, binary w c)) offers)
  (* FIXME: This assumes that each offer provides a unifiable continuation *)
  | TOffer (_, (_, _, c)::_) | TChoose (_, (_, _, c)::_) -> binary w c
  | TOffer (_, []) | TChoose (_, []) -> TApp ("End", [])
