{
    open Parser;;
    exception Eof;;
}

rule token = parse
    | [' ' '\t'] { token lexbuf }
    | '\n' { EOL }
    | ['a'-'z' 'A'-'Z' '0'-'9'] { VAR }
    | '*' { AND }
    | '+' { OR }
    | '~' { NOT }
    | '(' { LPAREN }
    | ')' { RPAREN }
    | eof { raise Eof }
