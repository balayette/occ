open Languages

type registers =
    RAX
  | RBX
  | RCX
  | RDX

let string_of_register = function
    RAX -> "%rax"
  | RBX -> "%rbx"
  | RCX -> "%rcx"
  | RDX -> "%rdx"

let int_of_register = function
    RAX -> 0
  | RBX -> 1
  | RCX -> 2
  | RDX -> 3

let register_of_int = function
    0 -> RAX
  | 1 -> RBX
  | 2 -> RCX
  | 3 -> RDX
  | _ -> failwith "not a valid register"

let string_of_regint x = register_of_int x |> string_of_register

let operand_of_arithmetic = function
    Ast.Plus -> "addq"
  | Ast.Minus -> "subq"
  | Ast.Mult -> "imulq"
  | Ast.Divide -> "idivq"

let rec assembly_of_exp reg = function
  | `Constant t -> (
      match t with
      | Types.Integer i -> Printf.sprintf "movq $%d,%s\n" i (reg |> register_of_int |> string_of_register)
      | _ -> failwith "Not supported"
    )
  | `FunCallExpression (s, params) -> (
      let rec to_push acc l = match l with
        | e::l -> (
            let s = String.concat "\n" [assembly_of_exp (int_of_register RAX) e; "pushq %rax"] in
            to_push (s::acc) l
          )
        | [] -> acc
      in let precall = String.concat "\n" (to_push [] params) in
      let rec to_pop acc = function
          0 -> acc
        | e -> to_pop ("popq %rsp\n"::acc) (e - 1)
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
        "pushq %rax";
        (assembly_of_exp reg e2);
        "popq %rbx";
        (match op with
           Ast.Minus -> (
             String.concat "\n" [
               Printf.sprintf "subq %s,%%rbx" (string_of_regint reg);
               Printf.sprintf "movq %%rbx,%s" (string_of_regint reg);
               "\n"
             ]
           )
         | Ast.Divide -> (
             String.concat "\n" [
               "xchgq %rbx,%rax"; (* put e1 in eax, e2 in ebx *)
               "cqo"; (* Sign extend rax in rdx *)
               "idivq %rbx"; (* divide by e2, store in %eax. The modulus is in %rdx *)
               "\n"
             ]
           )
         | _ -> Printf.sprintf "%s %%rbx,%s" (operand_of_arithmetic op) (string_of_regint reg));
      ]
    )
  | _ -> "nopexp"


let rec assembly_of_statement = function
  | `FunDeclaration (t, s, args, sl) -> (
      String.concat "\n" [
        Printf.sprintf ".globl %s" s;
        Printf.sprintf "%s:" s;
        "pushq %rbp";
        "movq %rsp,%rbp";
        String.concat "\n" (List.map assembly_of_statement sl)
      ]
    )
  | `ReturnStatement e -> (
      String.concat "\n" [
        (assembly_of_exp (int_of_register RAX) e);
        "movq %rbp,%rsp";
        "popq %rbp";
        "retq\n"
      ]
      (* Printf.sprintf "%s\npopq %%rbp\nret\n" (assembly_of_exp (int_of_register RAX) e) *)
    )
  | `FunCallStatement e -> (assembly_of_exp (int_of_register RAX) e) ^ "\n"
  | `IfStatement (i, e, sl, esl) -> (
      String.concat "\n" [
        (assembly_of_exp (int_of_register RAX) e);
        "cmpq $0,%rax";
        "je ifend" ^ (string_of_int i);
        String.concat "\n" (List.map (assembly_of_statement) sl);
        "ifend" ^ (string_of_int i) ^ ":";
        String.concat "\n" (List.map (assembly_of_statement) esl);
        "\n"
      ]
    )
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
  let res = Sedlex_menhir.parse Lexer.lex Parser.program lexbuf in
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
