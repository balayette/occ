%token RETURN

%token LBRACE
%token LPARENT
%token RBRACE
%token RPARENT
%token COMMA

%token INT_KEYWORD
%token VOID_KEYWORD

%token <string> IDENTIFIER

%token <int> INT_LITERAL

%token SEMICOLON
%token EOF

%start <Ast.abstract_syntax_tree option> program

%%

  program:
| EOF { None }
  | funcs = nonempty_list(function_declaration); EOF { Some (Ast.Funcs funcs) }
;

  function_declaration:
    t = type_keyword; name = IDENTIFIER; LPARENT; params = param_list; RPARENT LBRACE; li = statement_list; RBRACE {  Ast.FunDecl (t, name, params, li) }
;

  type_keyword:
| INT_KEYWORD {Types.Integer 0}
  | VOID_KEYWORD {Types.Void ()}
;

  parameter:
    t = type_keyword; name = IDENTIFIER { (name, t) }
;

  param_list:
 li = separated_list(COMMA, parameter) { li }
;

  statement_list:
    li = nonempty_list(statement) { li }
;
  statement:
    RETURN; k = INT_LITERAL; SEMICOLON {Ast.Return (Ast.Constant (Types.Integer k))}
;
