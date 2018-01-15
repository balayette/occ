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

(* let rec token acc buf = *)
(*   match%sedlex buf with *)
(*   | ";" -> print_string "semicolon\n"; token (SEMICOLON::acc) buf *)
(*   | "{" -> print_string "lbrace\n"; token (LBRACE::acc) buf *)
(*   | "}" -> print_string "rbrace\n"; token (RBRACE::acc) buf *)
(*   | "return" -> print_string "return\n"; token (RETURN::acc) buf *)
(*   | "(" -> print_string "lparen\n"; token (LPARENT::acc) buf *)
(*   | ")" -> print_string "rparen\n"; token (RPARENT::acc) buf *)
(*   | Plus number -> ( *)
(*       print_string "number\n"; *)
(*       let nbr = Sedlexing.Latin1.lexeme buf |> int_of_string in *)
(*       Printf.printf "Found number : %d\n" nbr; *)
(*       token ((INT_LITERAL nbr)::acc) buf *)
(*     ) *)
(*   | "int" -> print_string "int_keyword\n"; token (INT_KEYWORD::acc) buf *)
(*   | letter, (Star (letter | number)) -> ( *)
(*       print_string "identifier\n"; *)
(*       let id = Sedlexing.Latin1.lexeme buf in *)
(*       Printf.printf "Found an identifier : %s\n" id; *)
(*       token ((IDENTIFIER id)::acc) buf *)
(*     ) *)
(*   | eof -> print_string "EOF\n"; EOF::acc *)
(*   | white_space -> print_string "white space\n"; token acc buf *)
(*   | _ -> acc *)

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

(* let input = "int main(){return 5;}" *)

(* let lex_string s = *)
(*   print_endline s; *)
(*   let lexbuf = Sedlexing.Latin1.from_string s in *)
(*   let lexed = [token lexbuf] in *)
(*   List.iter (fun x -> string_of_token x |> Printf.printf "%s ") lexed; lexed *)
