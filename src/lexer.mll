{
open Lexing
open Parser

let keyword_table =
  let hash = Hashtbl.create 20 in
  List.iter (fun (keyword, token) -> Hashtbl.add hash keyword token)
    [ "close",   CLOSE
    ; "else",    ELSE
    ; "end",     END
    ; "false",   FALSE
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
    ; "true",    TRUE
    ];
  hash
}

let ident_start = ['A'-'Z' 'a'-'z' '_']
let ident_chars = ['A'-'Z' 'a'-'z' '0'-'9' '_']

let digit = ['0'-'9']

rule token = parse
  | [' ' '\t' '\r']
    { token lexbuf }
  | '\n'+ as ns
    { for i = 0 to String.length ns do
        new_line lexbuf
      done; DELIMIT }
  | ';'
    { DELIMIT }
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
  | '!'
    { SEND }
  | '='
    { EQUALS }
  | digit+ as i
    { NUMBER (int_of_string i, 0) }
  | (digit* as i1) '.' (digit+ as i2)
    { NUMBER (int_of_string i1, int_of_string i2) }
  | (ident_start ident_chars*) as id
    { if Hashtbl.mem keyword_table id
        then Hashtbl.find keyword_table id
        else IDENT id }
  | eof
    { EOF }

and comment = parse
  (* TODO: Test more examples, as this
     may require a more robust solution *)
  | '\n' ['\r' '\t' ' ']* '#'
    { new_line lexbuf; comment lexbuf }
  | '\n'+ as ns
    { let times = String.length ns in
      for i = 1 to times do
        new_line lexbuf
      done; DELIMIT }
  | eof
    { EOF }
  | _
    { comment lexbuf }

and string_literal strbuf = parse
  | eof
    { EOF }
  | "\\\"" as q
    { Buffer.add_string strbuf q;
      string_literal strbuf lexbuf }
  | '"'
    { STRING (Buffer.contents strbuf |> Scanf.unescaped) }
  | '\n'
    { new_line lexbuf;
      Buffer.add_char strbuf '\n';
      string_literal strbuf lexbuf }
  | _ as c
    { Buffer.add_char strbuf c;
      string_literal strbuf lexbuf }
