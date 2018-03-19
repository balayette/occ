open Types
open Ast

(* Nanocaml languages *)

(** 1:1 translation of the ast to a nanocaml language **)
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
    | `Nop
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

(* Label loops, conditions and variables to make them unique **)
module%language LabeledIfVar = struct
  include Base
  type lstatement =
    { del : [ `IfStatement of lexpresion * (lstatement list) * (lstatement list)
            | `WhileStatement of lexpression * (lstatement list)
            | `DeclarationStatement of builtin_types * string * lexpression
            ];
      add : [ `IfStatement of int * lexpression * (lstatement list) * (lstatement list)
            | `WhileStatement of int * lexpression * (lstatement list)
            | `DeclarationStatement of int * builtin_types * string * lexpression
            ]
    }
end

(* Create scopes, check if the function is a leaf, and check that variables
   aren't accessed before they're created *)
module%language WithScope = struct
  include LabeledIfVar
  type lstatement =
    { del : [ `FunDeclaration of builtin_types * string * (string * builtin_types) list * statement list
            | `IfStatement of int * lexpression * (lstatement list) * (lstatement list)
            | `WhileStatement of int * lexpression * (lstatement list)
            ];
      add : [ `FunDeclaration of Function_data.t * Scope.t * lstatement list * bool
            | `IfStatement of int * lexpression * (lstatement list) * (lstatement list) * Scope.t
            | `WhileStatement of int * lexpression * (lstatement list) * Scope.t
            ]
    }
end


(* Passes *)

let[@pass Base => LabeledIfVar] label_base =
  let label_count = ref (-1) and
  var_count = ref (-1) in
  [%passes
    let[@entry] rec ast = function
        `Toplevel (sl [@r] [@l]) -> `Toplevel (sl)
    and lstatement = function
        `IfStatement (e, sl [@r] [@l], esl [@r][@l]) -> (
          label_count := !label_count + 1;
          `IfStatement (!label_count, e, sl, esl)
        )
      | `WhileStatement (e, sl [@r] [@l]) -> (
          label_count := !label_count + 1;
          `WhileStatement (!label_count, e, sl)
        )
      | `DeclarationStatement (t, s, e) -> (
          var_count := !var_count + 1;
          `DeclarationStatement (!var_count, t, s, e)
        )
    and lexpression = function
        `Constant t -> `Constant t
  ]

let[@pass LabeledIfVar => WithScope] scopify =
  let leaf = ref true in
  let scope = ref (Scope.empty ()) in
  [%passes
    let[@entry] rec ast = function
        `Toplevel (sl [@r] [@l]) -> `Toplevel (sl)
    and lstatement = function
        `FunDeclaration (t, n, args, sl [@r] [@l]) -> (
          let data = Function_data.create_fdata t n args in
          let r = `FunDeclaration(data, !scope, sl, !leaf) in
          scope := Scope.empty ();
          leaf := true;
          r
        )
      | `FunCallStatement (e) -> (
          leaf := false;
          `FunCallStatement (e)
        )
      | `IfStatement (i, p, sl [@r] [@l], esl [@r] [@l]) -> (
          let par = !scope in
          scope := Scope.nested par;
          let r = `IfStatement (i, p, sl, esl, !scope) in
          scope := Scope.update_parent par !scope;
          r
        )
      | `WhileStatement (i, p, sl [@r] [@l]) -> (
          let par = !scope in
          scope := Scope.nested par;
          let r = `WhileStatement (i, p, sl, !scope) in
          scope := Scope.update_parent par !scope;
          r
        )
      | `DeclarationStatement (i, t, s, e) -> (
          scope := Scope.add_local !scope (s, t);
          `DeclarationStatement (i, t, s, e)
        )
    and lexpression = function
        `FunCallExpression (s, el) -> (
          leaf := false;
          `FunCallExpression(s, el)
        )
  ]

(* Helpers*)

(** Translate Ast to Base **)
let ast_to_poly_lang ast =
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
    | Nop -> `Nop
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
  in ast_toplevel_to_language ast


(* Public functions and types *)

let ast_to_language ast =
  let lang = ast_to_poly_lang ast in
  let labeled = label_base lang in
  let scoped = scopify labeled in
  scoped

type t = [ `Toplevel of tstatement list
         ]
and tstatement =
  [ `ReturnStatement of texpression
  | `FunCallStatement of texpression
  | `IfStatement of int * texpression * (tstatement list) * (tstatement list) * Scope.t
  | `FunDeclaration of Function_data.t * Scope.t * tstatement list * bool
  | `WhileStatement of int * texpression * (tstatement list) * Scope.t
  | `DeclarationStatement of int * Types.builtin_types * string * texpression
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
