open Types
open Ast

type t = [ `Toplevel of tstatement list
         ]
and tstatement =
  [ `ReturnStatement of texpression
  | `FunCallStatement of texpression
  | `IfStatement of int * texpression * (tstatement list) * (tstatement list)
  | `WhileStatement of int * texpression * (tstatement list)
  | `FunDeclaration of Function_data.t * tstatement list
  | `Nop
  ]
and texpression =
  [ `Constant of builtin_types
  | `FunCallExpression of string * (texpression list)
  | `ArrayAccess of texpression * texpression
  | `VarAccess of string
  | `Dereference of texpression
  | `Arithmetic of texpression * arith_op * texpression
  | `Comparison of texpression * comp_op * texpression
  ] [@@deriving show]
(** Create a nanocaml language from an ast **)
val ast_to_language : Ast.abstract_syntax_tree -> t
