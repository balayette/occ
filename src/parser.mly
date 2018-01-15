%token RETURN

%token LBRACE
%token LPARENT
%token RBRACE
%token RPARENT
%token COMMA

%token INT_KEYWORD
%token VOID_KEYWORD
%token STRING_KEYWORD

%token <string> IDENTIFIER
%token EQUAL

%token <int> INT_LITERAL
%token <string> STRING_LITERAL

%token SEMICOLON
%token EOF

%start <Ast.abstract_syntax_tree option> program

%%

program:
  | EOF { None }
| funcs = nonempty_list(function_declaration); EOF { Some (Ast.Funcs funcs) }
;

function_declaration:
  t = type_keyword; name = IDENTIFIER; LPARENT; params = fun_param_list; RPARENT LBRACE; li = statement_list; RBRACE {  Ast.FunDecl (t, name, params, li) }
;

function_call:
  name = IDENTIFIER; LPARENT; params = fun_call_param_list; RPARENT { Ast.FunCall (name, params) }
;

type_keyword:
  | INT_KEYWORD {Types.Integer 0}
| VOID_KEYWORD {Types.Void ()}
| STRING_KEYWORD {Types.String ""}
;

fun_call_param:
  a = INT_LITERAL { Ast.Constant (Types.Integer a) }
| a = STRING_LITERAL {  Ast.Constant (Types.String a) }
| a = function_call { a }
;

fun_call_param_list:
  li = separated_list(COMMA, fun_call_param) {li}
;

fun_parameter:
  t = type_keyword; name = IDENTIFIER { (name, t) }
;

fun_param_list:
  li = separated_list(COMMA, fun_parameter) { li }
;

statement_list:
  li = separated_nonempty_list(SEMICOLON, statement) { li }
;

declaration:
  t = type_keyword; n = IDENTIFIER; EQUAL; s = statement { Ast.Declaration (t, n, s) }
;

statement:
   RETURN SEMICOLON { Ast.Return (Ast.Constant (Types.Void ())) }
  | RETURN; s = statement; SEMICOLON{ Ast.Return s}
  | c = function_call { c }
  | i = INT_LITERAL { Ast.Constant (Types.Integer i)}
  | s = STRING_LITERAL { Ast.Constant (Types.String s)}
  | d = declaration { d }
;
