open Lexing

type pos =
  { file_name : string
  ; line_number : int
  ; column_number : int
  }

let mk_pos pos =
  { file_name = pos.pos_fname
  ; line_number = pos.pos_lnum
  ; column_number = pos.pos_bol
  }

let string_of_pos p =
  "line " ^ string_of_int p.line_number ^ ", column " ^
  string_of_int p.column_number ^ " in file " ^ p.file_name

type range =
  { start : pos
  ; finish : pos
  }

let make_range startpos endpos =
  { start = mk_pos startpos
  ; finish   = mk_pos endpos
  }

let string_of_range r =
  string_of_pos r.start ^ " to " ^ string_of_pos r.finish
