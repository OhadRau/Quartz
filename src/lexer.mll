{
open Lexing
open Parser

let keyword_table =
  let hash = Hashtbl.create 20 in
  List.iter (fun (keyword, token) -> Hashtbl.add hash keyword token)
    [ "close",   CLOSE
    ; "else",    ELSE
    ; "end",     END
    ; "from",    FROM
    ; "fun",     FUN
    ; "if",      IF
    ; "let",     LET
    ; "loop",    LOOP
    ; "module",  MODULE
    ; "on",      ON
    ; "or",      OR
    ; "require", REQUIRE
    ; "session", SESSION
    ; "spawn",   SPAWN
    ];
  hash
}

let ident_start = ['A'-'Z' 'a'-'z' '_']
let ident_chars = ['A'-'Z' 'a'-'z' '0'-'9' '_']

let digit = ['0'-'9']

rule token = parse
  | [' ' '\t' '\r']
    { token lexbuf }
  | ['\n' ';']
    { NEWLINE }
  | '#'
    { comment lexbuf }
  | '"'
    { string_literal (Buffer.create 100) lexbuf }
  | '('
    { OPEN_PAREN }
  | ')'
    { CLOSE_PAREN }
  | ','
    { COMMA }
  | '|'
    { PIPE }
  | '@'
    { AT }
  | '!'
    { SEND }
  | '='
    { EQUALS }
  | "<-"
    { ASSIGN }
  | digit+ as f | (digit* '.' digit+) as f
    { NUMBER (string_of_float f) }
  | (ident_start ident_chars*) as id
    { if Hashtbl.mem keyword_table id
        then Hashtbl.find keyword_table id
        else IDENT id }
  | eof
    { EOF }

and comment = parse
  | '\n'
    { token lexbuf }
  | eof
    { EOF }
  | _
    { comment lexbuf }

and string_literal strbuf = parse
  | eof
    { EOF }
  | "\\\"" as q
    { string_literal (Buffer.add_string strbuf q) lexbuf }
  | '"'
    { STRING (Buffer.contents strbuf |> String.unescaped) }
  | _ as c
    { string_literal (Buffer.add_char strbuf c) lexbuf }
