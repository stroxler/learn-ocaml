
let main () = begin
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let token = Myparser.Lexer.token lexbuf in
      Format.print_string @@
      "I saw a token: " ^ (Myparser.Lexer.show_token token) ^ "\n"
    done
  with Myparser.Lexer.Eof ->
    Format.print_string "All done now\n";
    exit 0
end ;;


main () ;;
