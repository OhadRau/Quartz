open Ast
open Type

module Scope = Map.Make(struct
    type t = identifier
    let compare = compare
  end)

module Subst = Map.Make(struct
    type t = Type.t
    let compare = compare
  end)

type env =
  { scope : Type.t Scope.t
  ; subst : Type.t Subst.t
  ; state : int
  }

let fresh env =
  ({ env with state = env.state + 1 }, TVar env.state)

let rec apply env = function
  | TVar _ as tvar ->
    if Subst.mem tvar env.subst
        then apply env (Subst.find tvar env.subst)
        else tvar
  | TApp (f, xs) -> TApp (f, List.map (apply env) xs)
  | TArrow (a, b) -> TArrow (List.map (apply env) a, apply env b)
  | TImplicit (n, t, u) -> TImplicit (n, apply env t, apply env u)
  | TNamed (n, t) -> TNamed (n, apply env t)
  | TRec (r, t) -> TRec (r, apply env t)
  | TOffer (w, offers) -> TOffer (w, List.map (fun (n, p, c) -> (n, apply env p, apply env c)) offers)
  | TChoose (w, offers) -> TChoose (w, List.map (fun (n, p, c) -> (n, apply env p, apply env c)) offers)
  | TQuant (q, l, t) -> TQuant (q, l, apply env t)
