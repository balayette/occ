(** Arithmetic operators **)
type arith_op =
    Minus
  | Plus
  | Mult
  | Divide
[@@deriving show]

(** Comparison operators **)
type comp_op =
    Equal
  | Smaller
  | Greater
  | GreaterOrEqual
  | SmallerOrEqual
[@@deriving show]

(** Retrieve the string associated with the comparison operator **)
val string_of_comp_op : comp_op -> string

(** Retrieve the string associated with the arithmetic operator **)
val string_of_arith_op : arith_op -> string

(** The complete type that can represent any C AST (in theory) **)
type abstract_syntax_tree =
    Toplevel of statement list
and statement =
    ReturnStatement of expression
  | FunDeclaration of Types.builtin_types * string * (string * Types.builtin_types) list * statement list
  | FunCallStatement of expression (* FunCallStatement (FunCallExpression (...)) *)
  | DeclarationStatement of Types.builtin_types * string * expression
  | IfStatement of expression * (statement list) * (statement list)
  | WhileStatement of expression * (statement list)
  | Nop
and expression =
    Constant of Types.builtin_types
  | FunCallExpression of string * (expression list)
  | ArrayAccess of expression * expression
  | VarAccess of string
  | Dereference of expression
  | Arithmetic of expression * arith_op * expression
  | Comparison of expression * comp_op * expression

(** Print the AST to stdout **)
val print_ast : abstract_syntax_tree -> unit
