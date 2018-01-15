open Types

module%language Base = struct
  type program = [ `Program of file list
                 ]
  and file =
    [ `File of string * (toplevel list)
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
    let[@entry] rec program = function
      | `Program (bll [@r][@l]) -> `Program (bll)
    and file = function
      | `File (n, toplvll [@r][@l]) -> `File (n, toplvll)
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

module%language FileListToHashTable = struct
  include WithoutElse
  type program = {
    del : [ `Program of file list];
    add : [ `Program of ((string, file) Hashtbl.t)]
  }
end

let[@pass WithoutElse => FileListToHashTable] list_to_htable =
  let tbl_of_files fl =
    let rec aux tbl fl = match fl with
      | e::l -> (
          match e with `File (s, tll) -> (
              Hashtbl.add tbl s (`File (s, tll))
            )
        ); aux tbl l
      | [] -> tbl
    in aux (Hashtbl.create (List.length fl)) fl
  in
  [%passes
    let[@entry] rec program = function
      | `Program (fl) -> (
          `Program (tbl_of_files (fl))
        )
  ]

let input =
  `Program ([
      `File ("main.c",
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
      `File ("main.c",
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
let input = "int main(){\n    return 42;\n}"


let () =
  (* input |> remove_else |> list_to_htable |> Batteries.dump |> print_endline *)
  (* Lexer.lex_string input |> Batteries.dump |> print_endline *)
  let lexbuf = Sedlex_menhir.create_lexbuf ~file:"examples/basic.c" (Sedlexing.Latin1.from_string input) in
  let res = Sedlex_menhir.sedlex_with_menhir Lexer.lex Parser.program lexbuf in
  Batteries.dump res |> print_endline;
  match res with
  | None -> print_endline "Got no AST"
  | Some ast -> Ast.print_ast ast
