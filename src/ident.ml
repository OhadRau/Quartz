(* Module names (M.X.Y) *)
type modident = string list * string

(* Value names (M.X.Y.z) *)
and valident = modident option * string
