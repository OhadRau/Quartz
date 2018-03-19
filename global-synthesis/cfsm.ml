type state = int

type participant = string

type channel = participant * participant

type message = string

type action = Send of channel * message | Recv of channel * message

type transition = state * action * state

type cfsm = {
  (* Set of states *)
  q  : state list;
  (* Set of channels: (p1, p2) | p1 <> p2 *)
  c  : channel list;
  (* Initial state *)
  q0 : state ;
  (* Set of messages *)
  a  : message list;
  (* Set of transitions *)
  d  : transition list
}

let subj = function
  | Send ((p, q), m) -> p
  | Recv ((p, q), m) -> q

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

type configuration = state * message list

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
