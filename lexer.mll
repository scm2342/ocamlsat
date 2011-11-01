{
    open Parser;;
    exception Eof;;
}

rule token = parse
    | [' ' '\t'] { token lexbuf }
    | '\n' { EOL }
    | ['a'-'z' 'A'-'Z' '0'-'9']+ as name { VAR(name) }
    | '*' { AND }
    | '+' { OR }
    | '~' { NOT }
    | '(' { LPAREN }
    | ')' { RPAREN }
    | eof { raise Eof }
