open Core

module Lexer = Lexer
module Parser = Parser


let parse_buf lexbuf =
  try
    let ast = Parser.parse_ast Lexer.lex_token lexbuf
    in First(ast)
  with
  | Lexer.LexError(emsg) ->
    let msg = Printf.sprintf "%a %s" Util.show_position lexbuf emsg
    in Second(msg)
