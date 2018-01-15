%token RETURN

%token LBRACE
%token LPARENT
%token RBRACE
%token RPARENT

%token INT_KEYWORD

%token <string> IDENTIFIER

%token <int> INT_LITERAL

%token SEMICOLON
%token EOF

%start <Ast.abstract_syntax_tree option> program

%%

  program:
| EOF { None }
  | main = method_main { Some (Ast.Funcs [main]) }
;
  method_main:
    INT_KEYWORD; id = IDENTIFIER; LPARENT RPARENT LBRACE;
                li = statement; RBRACE { Ast.FunDecl (Types.Integer 0, id, [], [li]) }
;

(*   statement_list: *)
(*     li = separated_list(SEMICOLON, statement) { li } *)
(* ; *)
  statement:
    RETURN; k = INT_LITERAL; SEMICOLON {Ast.Return (Ast.Constant (Types.Integer k))}
;
