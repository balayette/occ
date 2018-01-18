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

let operand_of_arithmetic = function
    Ast.Plus -> "addl"
  | Ast.Minus -> "subl"
  | Ast.Mult -> "imull"
  | Ast.Divide -> "idivl"

let rec assembly_of_exp reg = function
  | `Constant t -> (
      match t with
      | Types.Integer i -> Printf.sprintf "movl $%d,%s\n" i (reg |> register_of_int |> string_of_register)
      | _ -> failwith "Not supported"
    )
  | `FunCallExpression (s, params) -> (
      let rec to_push acc l = match l with
        | e::l -> (
            let s = String.concat "\n" [assembly_of_exp (int_of_register EAX) e; "push %eax"] in
            to_push (s::acc) l
          )
        | [] -> acc
      in let precall = String.concat "\n" (to_push [] params) in
      let rec to_pop acc = function
          0 -> acc
        | e -> to_pop ("pop %esp\n"::acc) (e - 1)
      in let postcall = to_pop [] (List.length params) |> String.concat "" in
      String.concat "\n" [
        precall;
        Printf.sprintf "call %s" s;
        postcall
      ]

    )
  (* Optimisation : use registers when possible *)
  | `Arithmetic (e1, op, e2) -> (
      String.concat "\n" [
        (assembly_of_exp reg e1);
        "push %eax";
        (assembly_of_exp reg e2);
        "pop %ebx";
        (match op with
           Ast.Minus -> (
             String.concat "\n" [
               Printf.sprintf "subl %s,%%ebx" (string_of_regint reg);
               Printf.sprintf "movl %%ebx,%s" (string_of_regint reg);
               "\n"
             ]
           )
         | Ast.Divide -> (
             String.concat "\n" [
               "xorl %edx,%edx";
               "xchg %ebx,%eax"; (* put e1 in eax, e2 in ebx *)
               "idivl %ebx"; (* divide by e2, store in %eax *)
               "\n"
             ]
           )
         | _ -> Printf.sprintf "%s %%ebx,%s" (operand_of_arithmetic op) (string_of_regint reg));
      ]
    )
  | _ -> "nopexp"


let rec assembly_of_statement = function
  | `FunDeclaration (t, s, args, sl) -> (
      String.concat "\n" [
        Printf.sprintf ".globl %s" s;
        Printf.sprintf "%s:" s;
        "push %ebp";
        "mov %esp,%ebp";
        String.concat "\n" (List.map assembly_of_statement sl)
      ]
    )
  | `ReturnStatement e -> (
      Printf.sprintf "%s\npop %%ebp\nret\n" (assembly_of_exp (int_of_register EAX) e)
    )
  | `FunCallStatement e -> (assembly_of_exp (int_of_register EAX) e) ^ "\n"
  | _ -> "nopstatement"

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
  Printf.printf "\nAST : \n\n";
  Ast.print_ast ast;
  Printf.printf "------\n";
  let compiled = compile_lang (ast_to_language ast) in
  Printf.printf "\nCOMPILED :\n\n";
  print_endline compiled;
  Printf.printf "------\n";
  let output = open_out "output.s" in
  Printf.fprintf output "%s" compiled;
  close_out output
