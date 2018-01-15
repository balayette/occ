open Types

type abstract_syntax_tree =
    Funcs of toplevel list
and statement =
    Return of statement
  | Constant of builtin_types
  | FunCall of string * (statement list)
and toplevel =
    FunDecl of builtin_types * string * (string * builtin_types) list * (statement list)

let print_ast ast =
  let rec print_toplevel lev = function
    | FunDecl (t, id, params, stmts) -> (
        Printf.printf "%sFUNDECL %s %s PARAMS (" lev (string_of_builtin_types t) id;
        List.iter (fun (n, t) -> Printf.printf "%s %s, " (string_of_builtin_types t) n) params;
        print_string ") [\n";
        List.iter (print_statement (lev ^ "  ")) stmts;
        Printf.printf "%s]\n" lev
      )
  and print_statement lev = function
    | Return v -> (
        Printf.printf "%sRETURN " lev;
        print_statement "" v;
        print_endline "";
      )
    | FunCall (n, params) -> (
        Printf.printf "%sFUNCALL %s (" lev n;
        List.iter (fun x -> print_statement "" x; print_string ", ") params;
        print_string ")"
      )
    | Constant t -> (
        print_string (string_of_builtin_types_values t)
      )
  in
  match ast with
  | Funcs tl -> (
      List.iter (print_toplevel "") tl
    )
