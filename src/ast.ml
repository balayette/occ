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
  let rec print_toplevel = function
    | FunDecl (t, id, params, stmts) -> (
        Printf.printf "%s %s(){\n" (string_of_builtin_types t) id;
        List.iter print_statement stmts;
        Printf.printf "\n}"
      )
  and print_statement = function
    | Return v -> (
        print_string "return ";
        print_values v;
        print_string ";"
      )
  and print_values = function
    | Constant t -> (
        print_string (string_of_builtin_types_values t)
      )
  in
  match ast with
  | Funcs tl -> (
      List.iter print_toplevel tl
    )
