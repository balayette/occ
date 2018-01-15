open Types

type abstract_syntax_tree =
    Funcs of toplevel list
and values =
    Constant of builtin_types
and statement =
    Return of values
and toplevel =
    FunDecl of builtin_types * string * (string * builtin_types) list * (statement list)

let print_ast ast =
  let rec print_toplevel lev = function
    | FunDecl (t, id, params, stmts) -> (
        Printf.printf "%sFUNDECL %s %s PARAMS %s [\n" lev (string_of_builtin_types t) id "None";
        List.iter (print_statement (lev ^ "  ")) stmts;
        Printf.printf "%s]" lev
      )
  and print_statement lev = function
    | Return v -> (
        Printf.printf "%sRETURN " lev;
        print_values v;
        print_endline "";
      )
  and print_values = function
    | Constant t -> (
        print_string (string_of_builtin_types_values t)
      )
  in
  match ast with
  | Funcs tl -> (
      List.iter (print_toplevel "") tl
    )
