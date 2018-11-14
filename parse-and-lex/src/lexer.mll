{
open Core
open Parser


exception Eof
exception LexError of string


(* Note:

   Whenever we do `PATTERN as NAME` below, it's also pretty standard to instead
   just do `PATTERN` and then call `Lexer.lexeme lexbuf` inside the brackets ont
   the right-hand-side.

   The "real-world-ocaml" book does that instead of using `as`
*)
}

let digit = ['0'-'9']
let int = '-'? digit+
let white = [' ' '\t']
let newline = '\r' | '\n' | "\r\n"


rule lex_token = parse
  | white
    { lex_token lexbuf }
  | newline
    { Util.next_line lexbuf; lex_token lexbuf }
  | int as raw
    { (INT (Int.of_string raw)) }
  | '+'
    { PLUS }
  | '-'
    { MINUS }
  | '*'
    { TIMES }
  | '/'
    { DIV }
  | '('
    { LPAREN }
  | ')'
    { RPAREN }
  | eof
    { EOF }
  | _
   { raise @@ LexError ("Failed to lex " ^ (Lexing.lexeme lexbuf)) }


{

let lex_to_list lexbuf =
  let rec go accum =
    try
      let token = lex_token lexbuf in
      if token = EOF
      then List.rev (token :: accum)
      else go (token :: accum)
    with
    | LexError(emsg) ->
      fprintf stderr "%a: %s" Util.print_position lexbuf emsg;
      exit 1
  in go []
}
