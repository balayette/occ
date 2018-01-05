open Batteries

type user_struct = {
  name : string;
  size : int;
  fields : (string * builtin_types) list;
} and
  builtin_types =
    | Integer of int
    | Floating of float
    | Character of char
    | String of string
    | Boolean of bool
    | Struct of user_struct
    | Void of unit

module%language Base = struct
  type basic =
    [ `File of toplevel list
    | `Program of basic list
    ]
  and toplevel =
    [ `PreprocessingDirective of string
    | `GlobalDeclaration of builtin_types * string * values
    | `FunDecl of builtin_types * string * (string * builtin_types) list * (statement list)
    ]
  and values =
    [ `FunCall of string * (builtin_types list)
    | `Constant of builtin_types
    | `Condition of predicate
    ]
  and test =
    [ `If of predicate * (statement list) * test
    | `ElseIf of predicate * (statement list) * test
    | `Else of (statement list)
    | `End
    ]
  and predicate =
    [ `Equality of values * values
    | `Boolean of bool
    ]
  and statement =
    [ `VarDecl of builtin_types * string * values
    | `FunCall of string * (builtin_types list)
    | `VarSet of string * values
    | `Ternary of predicate * statement * statement
    | `Test of test
    ]
end

module%language WithoutElse = struct
  include Base
  type test = {
    del : [`Else of (statement list)]
  }
end

let[@pass Base => WithoutElse] remove_else =
  [%passes
    let[@entry] rec basic = function
      | `Program (bll [@r][@l]) -> `Program (bll)
      | `File (toplvll [@r][@l]) -> `File (toplvll)
    and toplevel = function
      | `FunDecl (a, b, c, stl [@r][@l]) -> `FunDecl (a, b, c, stl)
    and values = function
      | `Condition p -> `Condition p
    and test = function
      | `If (p, stl [@r][@l], t [@r]) -> `If (p, stl, t)
      | `ElseIf (p, stl [@r][@l], t [@r]) -> `ElseIf (p, stl, t)
      | `Else (stl [@r][@l]) -> `ElseIf (`Boolean (true), stl, `End)
    and statement = function
      | `Test (t [@r]) -> `Test t
    and predicate = function
      | `Boolean b -> `Boolean b
  ]

let string_of_builtin_types = function
  | Integer _ -> "int"
  | Floating _ -> "float"
  | Character _ -> "char"
  | String _ -> "string"
  | Boolean _ -> "bool"
  | Struct s -> ("struct " ^ (s.name))
  | Void _ -> "void"

let string_of_struct s =
  let rec aux fields = match fields with
    | [] -> ""
    | e::l -> match e with (n, t) -> (Printf.sprintf "\n%s %s" (string_of_builtin_types t) n) ^ (aux l)
  in
  Printf.sprintf "struct %s {%s\n}\n" s.name (aux s.fields)

let string_of_builtin_types_values = function
  | Integer i -> Printf.sprintf "int:%d" i
  | Floating f -> Printf.sprintf "float:%f" f
  | Character c -> Printf.sprintf "char:%c" c
  | String s -> Printf.sprintf "string:%s" s
  | Boolean b -> if b then "bool:true" else "bool:false"
  | Struct st -> string_of_struct st
  | Void _ -> "void"

let input =
  `Program ([
      `File (
        [
          `FunDecl (Void (), "myfunc", [], [
              `Test (
                `If (`Boolean (false), [
                    `VarDecl (Integer 0, "ifvar", `Constant (Integer 1))
                  ], `Else ([
                    `VarDecl (Integer 0, "elsevar", `Constant (Integer 2))
                  ]))
              )
            ])
        ])
    ])

let expected_output =
  `Program ([
      `File (
        [
          `FunDecl (Void (), "myfunc", [], [
              `Test (
                `If (`Boolean (false), [
                    `VarDecl (Integer 0, "ifvar", `Constant (Integer 1))
                  ], `ElseIf (`Boolean (true), [
                    `VarDecl (Integer 0, "elsevar", `Constant (Integer 2))
                  ], `End))
              )
            ])
        ])
    ])

let () =
  print_endline (dump input);
  let output = remove_else input
  in (print_endline (dump output)); print_string "DONE\n";
  if output = expected_output then
    print_string "PASSED\n"
  else
    print_string "FAILED\n"
