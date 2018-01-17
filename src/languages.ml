open Types
open Ast

module%language Base = struct
  type ast = [ `Toplevel of lstatement list
             ]
  and lstatement =
    [ `ReturnStatement of lexpression
    | `FunDeclaration of builtin_types * string * (string * builtin_types) list * lstatement list
    | `FunCallStatement of lexpression (* FunCallStatement (FunCallExpression (...)) *)
    | `DeclarationStatement of builtin_types * string * lexpression
    | `IfStatement of lexpression * (lstatement list) * (lstatement list)
    | `WhileStatement of lexpression * (lstatement list)
    ]
  and lexpression =
    [ `Constant of builtin_types
    | `FunCallExpression of string * (lexpression list)
    | `ArrayAccess of lexpression * lexpression
    | `VarAccess of string
    | `Dereference of lexpression
    | `Arithmetic of lexpression * arith_op * lexpression
    | `Comparison of lexpression * comp_op * lexpression
    ]
end

let ast_to_language ast =
  let rec ast_toplevel_to_language ast = match ast with
      Toplevel sl -> `Toplevel (ast_stmt_list_to_language sl)

  and ast_stmt_list_to_language l = List.map (ast_stmt_to_language) l

  and ast_stmt_to_language = function
      ReturnStatement e -> `ReturnStatement (ast_exp_to_language e)
    | FunDeclaration (t, n, args, sl) -> `FunDeclaration (
        t,
        n,
        args,
        (ast_stmt_list_to_language sl)
      )
    | FunCallStatement e -> `FunCallStatement (ast_exp_to_language e)
    | DeclarationStatement (t, n, e) -> `DeclarationStatement (
        t,
        n,
        (ast_exp_to_language e)
      )
    | IfStatement (e, sl, esl) -> `IfStatement (
        (ast_exp_to_language e),
        (ast_stmt_list_to_language sl),
        (ast_stmt_list_to_language esl)
      )
    | WhileStatement (e, sl) -> `WhileStatement (
        (ast_exp_to_language e),
        (ast_stmt_list_to_language sl)
      )
  and ast_exp_list_to_language l = List.map (ast_exp_to_language) l

  and ast_exp_to_language (e : Ast.expression) = match e with
      Constant t -> `Constant t
    | FunCallExpression (n, el) -> `FunCallExpression (
        n,
        (ast_exp_list_to_language el)
      )
    | ArrayAccess (e1, e2) -> `ArrayAccess (
        ast_exp_to_language e1,
        ast_exp_to_language e2
      )
    | VarAccess s -> `VarAccess s
    | Dereference e -> `Dereference (ast_exp_to_language e)
    | Arithmetic (e1, op, e2) -> `Arithmetic (
        (ast_exp_to_language e1),
        op,
        (ast_exp_to_language e2)
      )
    | Comparison (e1, op, e2) -> `Comparison (
        (ast_exp_to_language e1),
        op,
        (ast_exp_to_language e2)
      )
  in
  ast_toplevel_to_language ast
