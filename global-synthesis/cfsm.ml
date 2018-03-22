type state = int

type participant = string

type channel = participant * participant

type message = string

type action = Send of channel * message | Recv of channel * message

let string_of_action = function
  | Send ((p, p'), m) -> p ^ "→" ^ p' ^ ":" ^ m
  | Recv ((p, p'), m) -> p' ^ "→" ^ p ^ ":" ^ m

let subj = function
  | Send ((p, q), m) -> p
  | Recv ((p, q), m) -> q

type ty = string

module rec GMsgSet : Set.S with type elt = message * Global.global = Set.Make(struct
  type t = message * Global.global
  let compare (m1, _) (m2, _) =
    String.compare m1 m2
end)

and Global : sig
  type global =
    | GMsg of participant * participant * GMsgSet.t
    | GMIP of participant * participant * message * GMsgSet.t (* Message In Progress *)
    | GRec of ty * global
    | GType of ty
    | GEnd
end = Global

include Global

let global_of_msg i msgs =
  GMsgSet.find (i, GEnd) msgs |> snd

let rec string_of_global = function
  | GMsg (p, p', msgs) ->
    p ^ "→" ^ p' ^ ":{" ^ fmt_global_msgs msgs ^ "}"
  | GMIP (p, p', j, msgs) ->
    p ^ "⇝" ^ p' ^ ":" ^ j ^ "{" ^ fmt_global_msgs msgs ^ "}"
  | GRec (ty, g) ->
    "μ" ^ ty ^ "." ^ string_of_global g
  | GType ty -> ty
  | GEnd -> "end"

(* Lazy coding *)
and fmt_global_msgs set = match GMsgSet.elements set with
  | [] -> ""
  | [(m, g)] -> m ^ "." ^ string_of_global g
  | (m, g)::xs -> m ^ "." ^ string_of_global g ^ ", " ^ fmt_global_msgs (GMsgSet.of_list xs)

let rec gsubst ~target ~replace = function
  | GMsg (p, p', msgs) -> GMsg (p, p', GMsgSet.map (fun (m, g) -> (m, gsubst ~target ~replace g)) msgs)
  | GMIP (p, p', j, msgs) -> GMIP (p, p', j, GMsgSet.map (fun (m, g) -> (m, gsubst ~target ~replace g)) msgs)
  | GRec (t, g) -> GRec (t, gsubst ~target ~replace g)
  | GType t when t = target -> replace
  | g -> g

module rec MsgSet : Set.S with type elt = message * Local.local = Set.Make(struct
  type t = message * Local.local
  let compare (m1, _) (m2, _) =
    String.compare m1 m2
end)

and Local : sig
  type local =
    | LRecv of participant * MsgSet.t
    | LSend of participant * MsgSet.t
    | LRec of ty * local
    | LType of ty
    | LEnd
end = Local

include Local

let local_of_msg i msgs =
  MsgSet.find (i, LEnd) msgs |> snd

let rec string_of_local = function
  | LSend (p, msgs) ->
    p ^ "!{" ^ fmt_local_msgs msgs ^ "}"
  | LRecv (p, msgs) ->
    p ^ "?{" ^ fmt_local_msgs msgs ^ "}"
  | LRec (ty, g) ->
    "μ" ^ ty ^ "." ^ string_of_local g
  | LType ty -> ty
  | LEnd -> "end"

and fmt_local_msgs set = match MsgSet.elements set with
  | [] -> ""
  | [(m, g)] -> m ^ "." ^ string_of_local g
  | (m, g)::xs -> m ^ "." ^ string_of_local g ^ ", " ^ fmt_local_msgs (MsgSet.of_list xs)

let rec lsubst ~target ~replace = function
  | LSend (p, msgs) -> LSend (p, MsgSet.map (fun (m, l) -> (m, lsubst ~target ~replace l)) msgs)
  | LRecv (p, msgs) -> LRecv (p, MsgSet.map (fun (m, l) -> (m, lsubst ~target ~replace l)) msgs)
  | LRec (t, l) -> LRec (t, lsubst ~target ~replace l)
  | LType t when t = target -> replace
  | l -> l

let rec mergeable l1 l2 =
  match (l1, l2) with
  | LRecv (p1, m1), LRecv (p2, m2) when p1 = p2 ->
    let k = m1 and j = m2 in
    let si = MsgSet.inter k j in
    MsgSet.for_all (fun (i, _) -> mergeable (local_of_msg i m1) (local_of_msg i m2)) si
      &&
    let kj = MsgSet.diff k j and jk = MsgSet.diff j k in
    MsgSet.for_all (fun (k, _) ->
      MsgSet.for_all (fun (j, _) ->
        j <> k
      ) jk
    ) kj
  | LSend (p1, m1), LSend (p2, m2) when p1 = p2 ->
    let k = m1 and j = m2 in
    let si = MsgSet.inter k j in
    MsgSet.for_all (fun (i, _) -> mergeable (local_of_msg i m1) (local_of_msg i m2)) si
      &&
    let kj = MsgSet.diff k j and jk = MsgSet.diff j k in
    MsgSet.for_all (fun (k, _) ->
      MsgSet.for_all (fun (j, _) ->
        j <> k
      ) jk
    ) kj
  | _ -> false

let rec merge t1 t2 =
  match (t1, t2) with
  | LRecv (p1, m1), LRecv (p2, m2) when p1 = p2 ->
    let k = m1 and j = m2 in
    let i = MsgSet.inter k j in
    let merged = MsgSet.map (fun (m, _) -> (m, merge (local_of_msg m m1) (local_of_msg m m2))) i
    and kj = MsgSet.diff k j and jk = MsgSet.diff j k in
    LRecv (p1, MsgSet.union merged (MsgSet.union kj jk))
  | LSend (p1, m1), LSend (p2, m2) when p1 = p2 ->
    let k = m1 and j = m2 in
    let i = MsgSet.inter k j in
    let merged = MsgSet.map (fun (m, _) -> (m, merge (local_of_msg m m1) (local_of_msg m m2))) i
    and kj = MsgSet.diff k j and jk = MsgSet.diff j k in
    LSend (p1, MsgSet.union merged (MsgSet.union kj jk))
  | _ -> failwith "Can't merge unmergeable local types"

let rec project global q = match global with
  | GMsg (p, p', msgs) when q = p ->
    LSend (p', project_msgs msgs q)
  | GMsg (p, p', msgs) when q = p' ->
    LRecv (p, project_msgs msgs q)
  | GMsg (p, p', msgs) ->
    let lmsgs = GMsgSet.elements msgs in
    begin match List.map (fun (_, g) -> project g q) lmsgs with
      | x::xs -> List.fold_left merge x xs
      | [] -> LEnd
    end
  | GRec (t, g) when project g q <> LType t ->
    LRec (t, project g q)
  | GRec (t, g) -> LEnd
  | GType t -> LType t 
  | GEnd -> LEnd
  | GMIP (_, _, _, _) -> failwith "Cannot project a message-in-progress"

and project_msgs msgs q =
  let lmsgs = GMsgSet.elements msgs in
  List.map (fun (m, g) -> (m, project g q)) lmsgs |> MsgSet.of_list

let eg_commit =
  let global =
    GRec ("t", GMsg ("A", "B", GMsgSet.of_list [
      "act",  GMsg ("B", "C", GMsgSet.of_list [
        "sig", GMsg ("A", "C", GMsgSet.of_list [
          "commit", GType "t"
        ])
      ]);
      "quit", GMsg ("B", "C", GMsgSet.of_list [
        "save", GMsg ("A", "C", GMsgSet.of_list [
          "finish", GEnd
        ])
      ])
    ])) in
  let local_c = project global "C"
  and local_c_expected =
    LRec ("t", LRecv ("B", MsgSet.of_list [
      "sig", LRecv ("A", MsgSet.of_list [
        "commit", LType "t"
      ]);
      "save", LRecv ("A", MsgSet.of_list [
        "finish", LEnd
      ])
    ])) in
  match local_c = local_c_expected with
    | true  -> print_endline "[PASSED] Project"
    | false -> print_endline "[FAILED] Project";
               print_endline ("When projecting " ^ string_of_global global);
               print_endline ("Got " ^ string_of_local local_c);
               print_endline ("Expected " ^ string_of_local local_c_expected)

let rec advance_glts glts l = match (glts, l) with
  | GMsg (p, p', msgs), Send ((q, q'), m)
    when GMsgSet.mem (m, GEnd) msgs && p = q && p' = q' ->
    GMIP (p, p', m, msgs)
  | GMIP (p, p', j, msgs), Recv ((q, q'), m)
    when j = m && p = q && p' = q' ->
    global_of_msg j msgs
  | GRec (t, g), l ->
    advance_glts (gsubst ~target:t ~replace:(GRec (t, g)) g) l
  | GMsg (p, p', msgs), l -> GMsg (p, p', GMsgSet.map (fun (m, g) -> (m, advance_glts g l)) msgs)
  | GMIP (p, p', j, msgs), l -> GMIP (p, p', j, GMsgSet.map (fun (m, g) -> (m, advance_glts g l)) msgs)
  | glts, t -> glts

let glts_test =
  let g1 = GMsg ("A", "B", GMsgSet.of_list [
    "a", GMsg ("A", "C", GMsgSet.of_list [
      "b", GEnd
    ])
  ])
  and l1 = Send (("A", "B"), "a")
  and l2 = Recv (("A", "B"), "a")
  and l3 = Send (("A", "C"), "b")
  and l4 = Recv (("A", "C"), "b") in
  let r1 =
    advance_glts (advance_glts (advance_glts (advance_glts g1 l1) l2) l3) l4
  and r2 =
    advance_glts (advance_glts (advance_glts (advance_glts g1 l1) l3) l2) l4
  and r3 =
    advance_glts (advance_glts (advance_glts (advance_glts g1 l1) l3) l4) l2 in
  match (r1, r2, r3) with
    | GEnd, GEnd, GEnd -> print_endline "[PASSED] GLTS Test"
    | r1, r2, r3       -> print_endline "[FAILED] GLTS Test"

let rec advance_llts llts l = match (llts, l) with
  | LSend (q, msgs), Send ((_, q'), a)
    when MsgSet.mem (a, LEnd) msgs && q = q' ->
    local_of_msg a msgs
  | LRecv (q, msgs), Recv ((q', _), a)
    when MsgSet.mem (a, LEnd) msgs && q = q' ->
    local_of_msg a msgs
  | LRec (t, local), l ->
    advance_llts (lsubst ~target:t ~replace:(LRec (t, local)) local) l
  | LSend (q, msgs), l ->
    LSend (q, MsgSet.map (fun (m, ll) -> (m, advance_llts ll l)) msgs)
  | LRecv (q, msgs), l ->
    LRecv (q, MsgSet.map (fun (m, ll) -> (m, advance_llts ll l)) msgs)
  | llts, l -> llts

type configuration = local list * message list

(* Def'n 3.4
  advance_configuration c l =
    match l with
    | pq!a ->
      let t'p' = map (if [not p] advance_llts t l) tp
      and w'p'q' = map (if [not p] w ++ [a]) wpq in
      (t'p', w'p'q')
    | pq?a ->
      let t'p' = map (if [not p] advance_llts t l) tp
      and w'p'q' = map (if [not p && not q] a::w) wpq in
      (t'p', w'p'q')
*)

type transition = local * action * local

let string_of_transition (l1, a, l2) =
  string_of_local l1 ^ "--[" ^ string_of_action a ^ "]-->" ^ string_of_local l2

type cfsm = {
  (* Set of states *)
  q  : local list;
  (* Set of channels: (p1, p2) | p1 <> p2 *)
  c  : channel list;
  (* Initial state *)
  q0 : local;
  (* Set of messages *)
  a  : message list;
  (* Set of transitions *)
  d  : transition list
}

let string_of_cfsm cfsm =
  let q  = "q  = " ^ String.concat ",\n     " (List.map string_of_local cfsm.q)
  and c  = "c  = " ^ String.concat ",\n     " (List.map (fun (a, b) -> "(" ^ a ^ ", " ^ b ^ ")") cfsm.c)
  and q0 = "q0 = " ^ string_of_local cfsm.q0
  and a  = "a  = " ^ String.concat ",\n     " cfsm.a
  and d  = "d  = " ^ String.concat ",\n     " (List.map string_of_transition cfsm.d) in
  q ^ "\n" ^ c ^ "\n" ^ q0 ^ "\n" ^ a ^ "\n" ^ d

let state_kind cfsm state =
  let transitions =
    List.filter (fun (a, x, b) -> a = state) cfsm.d in
  let rec identify kind = function
    | [] -> kind
    | (_, Send (_, _), _)::xs when kind = `Receiving ->
      `Mixed
    | (_, Send (_, _), _)::xs ->
      identify `Sending xs
    | (_, Recv (_, _), _)::xs when kind = `Sending ->
      `Mixed
    | (_, Recv (_, _), _)::xs ->
      identify `Receiving xs in
  identify `Final transitions

let rec collect_local_types ?(lst=[]) = function
  | LSend (_, msgs) | LRecv (_, msgs) ->
    let ts = MsgSet.elements msgs |> List.map snd in
    List.fold_left (fun lst t -> collect_local_types ~lst t) (ts @ lst) ts
  | LRec (t, l) -> collect_local_types ~lst:(l::lst) l
  | LType ty -> lst
  | LEnd -> LEnd::lst

module StringSet = Set.Make(String)

let rec collect_participants ?(s=StringSet.empty) = function
  | GMsg (p, q, msgs) | GMIP (p, q, _, msgs) ->
    let s' = StringSet.(add p (add q s)) in
    let lmsgs = GMsgSet.elements msgs in
    List.fold_left (fun s (_, g) -> collect_participants ~s g) s' lmsgs
  | GRec (_, g) -> collect_participants ~s g
  | _ -> s

let rec collect_messages ?(s=StringSet.empty) = function
  | GMsg (_, _, msgs) | GMIP (_, _, _, msgs) ->
    let lmsgs = GMsgSet.elements msgs in
    List.fold_left (fun s (m, t) -> collect_messages ~s:(StringSet.add m s) t) s lmsgs
  | GRec (_, g) -> collect_messages ~s g
  | _ -> s

let rec combinations = function
  | [] -> []
  | h::t ->
    let headed = List.map (fun t -> (h, t)) t
    and unheaded = combinations t in
    headed @ unheaded

module BindingMap = Map.Make(struct
  type t = ty
  let compare = compare
end)

let rec collect_local_bindings ?(m=BindingMap.empty) = function
  | LSend (_, msgs) | LRecv (_, msgs) ->
    let lmsgs = MsgSet.elements msgs in
    List.fold_left (fun m (_, l) -> collect_local_bindings ~m l) m lmsgs
  | LRec (t, l) ->
    let m' = BindingMap.add t l m in
    collect_local_bindings ~m:m' l
  | LType _ -> m
  | LEnd -> m

module TransitionSet = Set.Make(struct
  type t = transition
  let compare = compare
end)

let rec collect_transitions ?(s=TransitionSet.empty) local_bindings p =
  let get_t' = function
    | LType t when BindingMap.mem t local_bindings ->
      BindingMap.find t local_bindings
    | t -> t in
  function
  | LSend (q, msgs) as t ->
    let s' = MsgSet.fold (fun (a, t') s -> TransitionSet.add (t, Send ((p, q), a), get_t' t') s) msgs s in
    let lmsgs = MsgSet.elements msgs in
    List.fold_left (fun s (_, l) -> collect_transitions ~s local_bindings p l) s' lmsgs
  | LRecv (q, msgs) as t ->
    let s' = MsgSet.fold (fun (a, t') s -> TransitionSet.add (t, Recv ((p, q), a), get_t' t') s) msgs s in
    let lmsgs = MsgSet.elements msgs in
    List.fold_left (fun s (_, l) -> collect_transitions ~s local_bindings p l) s' lmsgs
  | LRec (_, t) -> collect_transitions ~s local_bindings p t
  | LType _ -> s
  | LEnd -> s

let cfsm_of_projection g p =
  let t0 = project g p in
  let q = collect_local_types t0
  and c = combinations (StringSet.elements (collect_participants g))
  and q0 = match t0 with
    | LRec (t, l) -> l
    | l -> l
  and a = StringSet.elements (collect_messages g)
  and d = TransitionSet.elements (collect_transitions (collect_local_bindings t0) p t0) in
  { q; c; q0; a; d }

(* Def'n 3.6: Translation from a basic CFSM to a local type
let local_of_cfsm mp =
  let t v q = (* Loop through participant, { !, ? }, etc. for t *)
    let sends =
      List.filter (function (t, Send (_, _), _) when r = q -> true
                          | _ -> false) mp.d
      |> List.map (fun (t, Send ((p, p'), aj), tj) -> aj, tj)
    and recvs =
      List.filter (function (t, Recv (_, _), _) when r = q -> true
                          | _ -> false) mp.d in
    match sends, recvs with
    | (_::_) as s, [] ->
    | [], (_::_) as r ->
  and t' v q =
  t [] mp.q0
*)

let cfsm_test =
  let g =
    GRec ("t", GMsg ("a", "b", GMsgSet.of_list [
      "Hello", GMsg ("b", "c", GMsgSet.of_list [
        "Start", GMsg ("c", "a", GMsgSet.of_list [
          "Hello", GEnd
        ])
      ]);
      "Ping", GMsg ("b", "a", GMsgSet.of_list [
        "Pong", GType "t"
      ])
    ]))
  and p = "a" in
  let cfsm_expected =
    { q  = []
    ; c  = List.sort compare [ ("a", "b"); ("a", "c"); ("b", "c") ]
    ; q0 = LSend ("b", MsgSet.of_list [
             "Hello", LRecv ("c", MsgSet.of_list [
               "Hello", LEnd
             ]);
             "Ping", LRecv ("b", MsgSet.of_list [
               "Pong", LType "t"
             ])
           ])
    ; a  = List.sort compare [ "Hello"; "Ping"; "Pong"; "Start" ]
    ; d  = []
    }
  and cfsm = cfsm_of_projection g p in
  print_endline (string_of_cfsm cfsm);
  match cfsm.q  = cfsm_expected.q
      , cfsm.c  = cfsm_expected.c
      , cfsm.q0 = cfsm_expected.q0
      , cfsm.a  = cfsm_expected.a
      , cfsm.d  = cfsm_expected.d with
  | _, true, true, true, _ -> print_endline "[PASSED] CFSM Test"
  | _                      -> print_endline "[FAILED] CFSM Test"

(* Def'n 3.6: Translation from a basic CFSM to a local type ... *)

let rec binary_compatible = function
  | LRec (t, l) -> LRec (t, binary_compatible l)
  | LSend (p, msgs) -> LRecv (p, MsgSet.map (fun (a, t) -> (a, binary_compatible t)) msgs)
  | LRecv (p, msgs) -> LSend (p, MsgSet.map (fun (a, t) -> (a, binary_compatible t)) msgs)
  | LType t -> LType t
  | LEnd -> LEnd

(* Def'n 4.1
  let merge_cfsms cfsms =
    { q  = List.fold_left (fun l {q} -> q @ l) [] cfsms
    ; c
    ; q0
    ; a
    ; d
    } *)

(* Pre-synth:
     - Take communicating system S, such that forall p in S, exists q in S, s.t. pq!a or pq?a
     - q : ((participant, id) list, local) where id = global name of session
     - Assign to each qn some unique identifier Un
     - Replace all instances referencing participantn/idn in each q with Un
let synth qs =
  let pairs = StringMap.bindings qs |> combinations in
  let rec loop = function
    | [] -> StringMap.bindings qs |> List.map (fun (n, _) -> GType n)
    | ((pn, LSend (qn', qmsgs)), (qn, LRecv (pn', pmsgs)))::pqs when pn = pn' && qn = qn' ->
    | ((pn, LRecv (qn', qmsgs)), (qn, LSend (pn', pmsgs)))::pqs when pn = pn' && qn = qn' -> *)
