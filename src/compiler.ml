open Languages

let get_ast = function
    None -> failwith "Got no AST"
  | Some ast -> ast

let () =
  let file = open_in Sys.argv.(1) in
  let lexbuf = Sedlex_menhir.create_lexbuf ~file:"examples/basic.c" (Sedlexing.Latin1.from_channel file) in
  let res = Sedlex_menhir.sedlex_with_menhir Lexer.lex Parser.program lexbuf in
  let ast = get_ast res in
  Ast.print_ast ast;
