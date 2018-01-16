open Types

type abstract_syntax_tree =
    Toplevel of statement list
and statement =
    ReturnStatement of expression
  | FunDeclaration of builtin_types * string * (string * builtin_types) list * statement list
  | FunCallStatement of expression (* FunCallStatement (FunCallExpression (...)) *)
  | DeclarationStatement of builtin_types * string * expression
  | IfStatement of expression * (statement list) * (statement list)
and expression =
    Constant of builtin_types
  | FunCallExpression of string * (expression list)
  | ArrayAccess of expression * expression
  | VarAccess of string
  | Dereference of expression

let print_ast ast =
  let rec print_toplevel lev = function
    | FunDeclaration (t, id, params, stmts) -> (
        Printf.printf "%sFUNDECL %s %s PARAMS (" lev (string_of_builtin_types t) id;
        List.iter (fun (n, t) -> Printf.printf "%s %s, " (string_of_builtin_types t) n) params;
        print_string ") [\n";
        List.iter (print_statement (lev ^ "  ")) stmts;
        Printf.printf "%s]\n" lev
      )
    | _ -> print_string "Shouldn't be in the toplevel\n"
  and print_statement lev = function
    |  ReturnStatement e -> (
        Printf.printf "%sRETURN " lev;
        print_expression "" e;
        print_endline ";";
      )
    | FunCallStatement e -> (
        match e with
        | FunCallExpression (n, el) -> (
            Printf.printf "%sFUNCALL %s(" lev n;
            List.iter (fun x -> print_expression "" x; print_string ", ") el;
            print_endline ");"
          )
        | _ -> print_string "FunCallStatement can only contain FunCallStatement"
      )
    | DeclarationStatement (t, n, e) -> (
        Printf.printf "%sDECL %s %s = " lev (string_of_builtin_types t) n;
        print_expression "" e;
        print_endline ";"
      )
    | FunDeclaration _ -> print_endline "Can't declare a function inside another function"
    | IfStatement (e, sl, esl) -> (
        Printf.printf "%sIF (" lev;
        print_expression "" e;
        print_endline "){";
        List.iter (print_statement (lev ^ " ")) sl;
        Printf.printf "%s}\n%sELSE{\n" lev lev;
        List.iter (print_statement (lev ^ " ")) esl;
        Printf.printf "%s}\n" lev
     )
  and print_expression lev = function
    | Constant t -> Printf.printf "%s%s" lev (string_of_builtin_types_values t)
    | FunCallExpression (n, el) -> (
        Printf.printf "%sFUNCALL %s(" lev n;
        List.iter (fun x -> print_expression "" x; print_string ", ") el;
        print_string ")"
      )
    | ArrayAccess (e1, e2) -> (
        Printf.printf "%sAR_ACC " lev;
        print_expression "" e1;
        print_string "[";
        print_expression "" e2;
        print_string "]"
      )
    | VarAccess s -> Printf.printf "%sACCESS %s" lev s
    | Dereference e -> (
        Printf.printf "%sDEREF " lev;
        print_expression "" e
      )
  in
  match ast with
  | Toplevel (sl) -> List.iter (print_toplevel "") sl
