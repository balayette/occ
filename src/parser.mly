%token MAIN_FUNC

%token RETURN

%token LBRACE
%token LPARENT
%token RBRACE
%token RPARENT

%token INT_KEYWORD

%token IDENTIFIER

%token <int32> INT_LITERAL

%token SEMICOLON
%token EOF

%start <int32 option> main_function

%%

                 main_function:
| INT_KEYWORD IDENTIFIER LPARENT RPARENT LBRACE RETURN; i = INT_LITERAL; SEMICOLON RBRACE { Some i }
| EOF { None }
