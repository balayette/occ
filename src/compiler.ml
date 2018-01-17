open Languages

type registers =
    EAX
  | EBX
  | ECX
  | EDX

let string_of_register = function
    EAX -> "%eax"
  | EBX -> "%ebx"
  | ECX -> "%ecx"
  | EDX -> "%edx"

let int_of_register = function
    EAX -> 0
  | EBX -> 1
  | ECX -> 2
  | EDX -> 4

let register_of_int = function
    0 -> EAX
  | 1 -> EBX
  | 2 -> ECX
  | 3 -> EDX
  | _ -> failwith "not a valid register"

let string_of_regint x = register_of_int x |> string_of_register

let rec assembly_of_exp reg = function
  | `Constant t -> (
      match t with
      | Types.Integer i -> Printf.sprintf "mov $%d,%s\n" i (reg |> register_of_int |> string_of_register)
      | _ -> failwith "Not supported"
    )
  | `FunCallExpression (s, params) -> (
      Printf.sprintf "call %s\n" s
    )
  | `Arithmetic (e1, op, e2) -> (
      let store = match register_of_int reg with
        | EAX -> EBX
        | EBX -> EAX
        | _ -> failwith "Don't need the other registers for arithmetic"
      in
      String.concat "\n" [
        (assembly_of_exp (int_of_register store) e2);
        (assembly_of_exp reg e1);
        Printf.sprintf "add %s,%s\n" (string_of_register store) (string_of_regint reg)
      ]
    )
  | _ -> "nop"


let rec assembly_of_statement = function
  | `FunDeclaration (t, s, args, sl) -> (
      String.concat "\n" [
        Printf.sprintf ".globl %s" s;
        Printf.sprintf "%s:" s;
        String.concat "\n" (List.map assembly_of_statement sl);
      ]
    )
  | `ReturnStatement e -> (
      Printf.sprintf "%sret\n" (assembly_of_exp (int_of_register EAX) e)
    )
  | `FunCallStatement e -> (assembly_of_exp (int_of_register EAX) e) ^ "\n"
  | _ -> "nop"

let rec compile_lang lang =
  let prog = match lang with
    | `Toplevel sl -> (
        String.concat "\n" (List.map assembly_of_statement sl)
      )
    | _ -> failwith "no"
  in String.concat "\n\n" [
    ".text";
    prog
  ]

let get_ast = function
    None -> failwith "Got no AST"
  | Some ast -> ast

let () =
  let file = open_in Sys.argv.(1) in
  let lexbuf = Sedlex_menhir.create_lexbuf ~file:"examples/basic.c" (Sedlexing.Latin1.from_channel file) in
  let res = Sedlex_menhir.sedlex_with_menhir Lexer.lex Parser.program lexbuf in
  let ast = get_ast res in
  Ast.print_ast ast;
  let compiled = compile_lang (ast_to_language ast) in
  print_endline compiled;
  let output = open_out "output.s" in
  Printf.fprintf output "%s" compiled;
  close_out output
