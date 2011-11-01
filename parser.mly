%{
    open Type;;
%}
%token <string> VAR
%token AND OR
%token LPAREN RPAREN
%token NOT
%token EOL
%left OR
%left AND
%nonassoc NOT
%start main
%type <Type.prop> main
%%
main:
    expr EOL { $1 }
;
expr:
    | VAR { Var $1 }
    | LPAREN expr RPAREN { $2 }
    | expr AND expr { And($1, $3) }
    | expr OR expr { Or($1, $3) }
    | NOT expr { Not $2 }
;
