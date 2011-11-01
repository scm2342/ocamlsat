open Type;;

let rec print_prop a =
    match a with
        | Var a -> print_string a
        | Not a -> print_string "~("; print_prop a; print_string ")"
        | And(a, b) -> print_string "("; print_prop a; print_string " * "; print_prop b; print_string ")"
        | Or(a, b) -> print_string "("; print_prop a; print_string " + "; print_prop b; print_string ")";;

let _ =
    try
      let lexbuf = Lexing.from_channel stdin in
      while true do
          let result = Parser.main Lexer.token lexbuf in
          print_prop result; print_newline(); flush stdout
      done
  with Lexer.Eof ->
      exit 0;;
