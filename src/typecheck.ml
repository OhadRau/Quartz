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

let rec contains(t2) = function
  | TApp (_, xs) -> List.exists (contains(t2)) xs
  | TArrow (a, b) -> List.exists (contains(t2)) a || contains(t2) b
  | TImplicit (_, a, b) -> contains(t2) a || contains(t2) b
  | TNamed (_, t) -> contains(t2) t
  | TRec (_, t) -> contains(t2) t
  | TOffer (_, offers) | TChoose (_, offers) -> List.exists (fun (_, p, c) -> contains(t2) p || contains(t2) c) offers
  | TQuant (_, _, t) -> contains(t2) t
  | t1 -> t1 = t2

let rec unify env t1 t2 =
  let t1 = apply env t1 and t2 = apply env t2 in
  match t1, t2 with
  | _, _ when t1 = t2 -> env
  | TVar _, _ when not (contains(t2) t1) -> { env with subst = Subst.add t1 t2 env.subst }
  | _, TVar _ when not (contains(t1) t2) -> { env with subst = Subst.add t2 t1 env.subst }
  | TApp (f, xs), TApp (g, ys) when f = g -> List.fold_left2 unify env xs ys
  | TArrow (args1, r1), TArrow (args2, r2) -> unify (List.fold_left2 unify env args1 args2) r1 r2
  | TImplicit (n1, t1, c1), TImplicit (n2, t2, c2) when n1 = n2 -> unify (unify env t1 t2) c1 c2
  | TNamed (n1, t1), TNamed (n2, t2) when n1 = n2 -> unify env t1 t2
  | TRec (r1, t1), TRec (r2, t2) -> unify (unify env (TVar r1) (TVar r2)) t1 t2
  | TQuant (q1, l1, t1), TQuant (q2, l2, t2) when l1 = l2 -> unify (unify env (TVar q1) (TVar q2)) t1 t2
  | TOffer (w1, o1), TOffer (w2, o2) | TChoose (w1, o1), TChoose (w2, o2) when w1 = w2 ->
    begin
      let module Offers = Set.Make(String)
      and name_of_offer (n, _, _) = n in
      let names = Offers.(elements @@ union (of_list @@ List.map name_of_offer o1) (of_list @@ List.map name_of_offer o2)) in
      let rec loop env = function
        | [] -> env
        | name::names ->
          begin
            let matcher = List.find (fun o -> name_of_offer o = name) in
            match (matcher o1, matcher o2) with
              | (_, t1, c1), (_, t2, c2) ->
                let env = unify env t1 t2 in
                let env = unify env c1 c2 in
                loop env names
              | exception Not_found -> loop env names
          end in
      loop env names
    end
  | _, _ -> failwith @@ "Could not unify type " ^ string_of_type t1 ^ " with type " ^ string_of_type t2
