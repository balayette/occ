%token RETURN

%token STAR
%token LBRACE
%token LPARENT
%token RBRACE
%token RPARENT
%token LBRACKET
%token RBRACKET
%token COMMA

%token INT_KEYWORD
%token VOID_KEYWORD
%token STRING_KEYWORD

%token PLUS
%token MINUS

%token SMALLER
%token GREATER

%token IF
%token ELSE
%token WHILE

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
  | funcs = nonempty_list(function_declaration); EOF { Some (Ast.Toplevel funcs) }
;

function_declaration:
  t = type_keyword; name = IDENTIFIER; LPARENT; params = fun_param_list; RPARENT LBRACE; li = statement_list; RBRACE {  Ast.FunDeclaration (t, name, params, li) }
;

function_call:
  name = IDENTIFIER; LPARENT; params = fun_call_param_list; RPARENT { (Ast.FunCallExpression (name, params)) }
;

type_keyword:
  | INT_KEYWORD {Types.Integer 0}
| VOID_KEYWORD {Types.Void ()}
| STRING_KEYWORD {Types.String ""}
;

fun_call_param:
  e = expression { e }
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
  li = nonempty_list(statement) { li }
;

declaration:
  t = type_keyword; n = IDENTIFIER; EQUAL; s = expression { Ast.DeclarationStatement (t, n, s) }
| t = type_keyword; n = IDENTIFIER; LBRACKET; l = option(INT_LITERAL); RBRACKET; EQUAL; s = expression { Ast.DeclarationStatement((Types.Array (t, [], l)), n, s) }

;

array_access:
  e1 = expression; LBRACKET; e2 = expression; RBRACKET { Ast.ArrayAccess (e1, e2) }
;

var_access:
  s = IDENTIFIER { Ast.VarAccess s }
;

dereference:
  STAR; e = expression { Ast.Dereference e }
;

_if_predicate:
  LPARENT; p = expression; RPARENT { p }
;

_if_body:
  | LBRACE; sl = statement_list; RBRACE; { sl }
  | s = statement { [s] }
;

_else:
  ELSE LBRACE; sl = statement_list; RBRACE { sl }
  | ELSE; s = statement { [s] }
  | { [] }
;

if_stmt:
  IF; p = _if_predicate; sl = _if_body; esl = _else {  Ast.IfStatement (p, sl, esl) }
;

while_stmt:
  WHILE; p = _if_predicate; sl = _if_body { Ast.WhileStatement (p, sl) }
;

arithmetic:
  e1 = expression; PLUS; e2 = expression { Ast.Arithmetic (e1, (Ast.Plus), e2) }
| e1 = expression; MINUS; e2 = expression { Ast.Arithmetic (e1, (Ast.Minus), e2 ) }
;

comparison_op:
    EQUAL EQUAL { Ast.Equal }
  | SMALLER { Ast.Smaller }
  | GREATER { Ast.Greater }
  | SMALLER EQUAL { Ast.SmallerOrEqual }
  | GREATER EQUAL { Ast.GreaterOrEqual }
;

comparison:
  e1 = expression; c = comparison_op; e2 = expression { Ast.Comparison (e1, c, e2) }
;

expression:
  i = INT_LITERAL { Ast.Constant (Types.Integer i)}
  | s = STRING_LITERAL { Ast.Constant (Types.String s)}
  | c = function_call { c }
  | a = array_access { a }
  | v = var_access { v }
  | d = dereference { d }
  | a = arithmetic { a }
  | c = comparison { c }
;

statement:
   RETURN SEMICOLON{ Ast.ReturnStatement (Ast.Constant (Types.Void ())) }
  | RETURN; s = expression; SEMICOLON { Ast.ReturnStatement s}
  | c = function_call; SEMICOLON {  Ast.FunCallStatement c }
  | d = declaration; SEMICOLON { d }
  | i = if_stmt { i }
  | w = while_stmt { w }
;
