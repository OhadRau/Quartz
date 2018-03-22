type ident = string

module StringMap = Map.Make(String)

type quantified = Quant of ident StringMap.s * local

type system = quantified list

type session =
  { params : ident StringMap.s
  ; t      : quantified
  }

type all_session = session StringMap.t

type all_instances = quantified StringMap.t
