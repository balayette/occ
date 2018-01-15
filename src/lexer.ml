open Parser

let number = [%sedlex.regexp? '0'..'9']
let letter = [%sedlex.regexp? 'a'..'z' | 'A'..'Z']

let rec lex lexbuf =
  let open Sedlex_menhir in
  let buf = lexbuf.stream in
  match%sedlex buf with
  | ";" -> update lexbuf; SEMICOLON
  | "{" -> update lexbuf; LBRACE
  | "}" -> update lexbuf; RBRACE
  | "(" -> update lexbuf; LPARENT
  | ")" -> update lexbuf; RPARENT
  | "return" -> update lexbuf; RETURN
  | Plus number -> (
      let nbr = Sedlexing.Latin1.lexeme buf |> int_of_string in
      update lexbuf; INT_LITERAL nbr
    )
  | "int" -> INT_KEYWORD
  | letter, (Star (letter | number)) -> (
      let id = Sedlexing.Latin1.lexeme buf in
      update lexbuf; IDENTIFIER id
    )
  | eof -> update lexbuf; EOF
  | white_space -> update lexbuf; lex lexbuf
  | "\n" -> update lexbuf; new_line lexbuf; lex lexbuf
  | _ -> failwith "WTF"

let string_of_token = function
  | SEMICOLON -> ";"
  | RPARENT -> ")"
  | RETURN -> "return"
  | RBRACE -> "}"
  | LBRACE -> "{"
  | LPARENT -> "("
  | INT_LITERAL i -> Printf.sprintf "%d" i
  | INT_KEYWORD -> "int"
  | IDENTIFIER s -> Printf.sprintf "%s" s
  | EOF -> "EOF"
  | NEWLINE -> "\\n"
