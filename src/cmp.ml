type types =
  [ `Integer of int
  | `Floating of float
  | `Character of char
  | `String of string
  | `UserDefined of string
  | `Void
  ]

type punctuator_types =
    OParen
  | CParen
  | Plus
  | Minus
  | Equal

module%language Base = struct
  type statement =
    [ `Assignment of string * statement
    | `Call of string * (statement list)
    | `Constant of types
    | `Condition of condition
    | `FunDecl of types * string * (string list) * (types list) * (statement list)
    | `Predicate of predicate
    ]
  and condition =
    [ `If of predicate * (statement list) * condition
    (*if(predicate){statement list} (else(if))*)

    | `ElseIf of predicate * (statement list) * condition
    (* else if (predicate) {statement list} (else(if)) *)

    | `Else of statement list
    (* else {statement list} equivalent to else if (true) {statement list}*)
    | `End
    ]
  and predicate =
    [ `Equality of statement * statement
    | `LowerThan of statement * statement
    | `Boolean of bool
    ]
end

module%language BaseWithoutElse = struct
  include Base
  type condition = {
    del : [`Else of statement list
          ]
  }
end

let[@pass Base => BaseWithoutElse] remove_else =
  [%passes

    let[@entry] rec statement = function
      | `Assignment (s, st [@r]) -> `Assignment (s, st)
      | `Call (s, st [@r][@l]) -> `Call (s, st)
    and predicate = function
      | `Boolean b -> `Boolean b
    and condition = function
      | `Else (sl [@r][@l]) -> `ElseIf ((`Boolean (true)), sl, `End)
  ]

let print_type = function
  | `Integer (i) -> Printf.printf "int"
  | `Floating (f) -> Printf.printf "float"
  | `Character (c) -> Printf.printf "char"
  | `String (s) -> Printf.printf "string"
  | `UserDefined (s) -> Printf.printf "%s" s
  | `Void -> print_string "void"

let print_constant = function
  | `Integer (i) -> Printf.printf "int:%d" i
  | `Floating (f) -> Printf.printf "float:%f" f
  | `Character (c) -> Printf.printf "char:%c" c
  | `String (s) -> Printf.printf "string:%s" s
  | `UserDefined (s) -> Printf.printf "User defined:%s" s
  | `Void -> print_string "void:()"

let rec print_lang ?(lev="") lang =
  let rec print_l lev = function
    | e::l -> print_lang ~lev:lev e; print_l lev l
    | [] -> ()

  and print_args lev (ns : string list) (ts : types list) = match (ns, ts) with
    | [], [] -> ()
    | e::_, [] -> Printf.printf "%s This function is not valid\n" lev;
    | [], e::_ -> Printf.printf "%s This function is not valid\n" lev;
    | n::l1, t::l2 -> Printf.printf "%s" lev; print_type t; Printf.printf " %s, " n; print_args lev l1 l2
  in
  match lang with
  | `Assignment (s, st) -> Printf.printf "%s%s = " lev s; print_lang ~lev:(lev ^ " ") st;
  | `Call (s, st) -> Printf.printf "%s%s(" lev s; print_l lev st; print_string ")\n"
  | `Constant (t) -> print_constant t;
  | `FunDecl (t, n, argnames, argtypes, stl) -> (
      print_type t; Printf.printf " %s(" n;
      print_args lev argnames argtypes;
      print_string "){\n";
      print_l (lev ^ " ") stl;
      print_string "\n}\n"
    )
  | _ -> print_string "Unknown\n"

let () =
  let input =
    `FunDecl (`Integer (0), "myfunc", ["input"], [`Integer (0)], [
        `Condition (`If  (`Boolean (false)), [
            `Assignment ("a", `Constant (`Integer 5))
          ],
                         `Condition (`Else ([
                             `Assignment ("e", `Constant (`Integer 6))
                           ])))
      ])
  in
  print_lang input;
  let output = remove_else input in
  print_lang output;
  print_string "DONE\n"
