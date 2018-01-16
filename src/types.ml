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
    | Pointer of builtin_types

let rec string_of_builtin_types = function
  | Integer _ -> "int"
  | Floating _ -> "float"
  | Character _ -> "char"
  | String _ -> "string"
  | Boolean _ -> "bool"
  | Struct s -> ("struct " ^ (s.name))
  | Void _ -> "void"
  | Pointer e -> "*" ^ (string_of_builtin_types e)

let string_of_struct s =
  let rec aux fields = match fields with
    | [] -> ""
    | e::l -> match e with (n, t) -> (Printf.sprintf "\n%s %s" (string_of_builtin_types t) n) ^ (aux l)
  in
  Printf.sprintf "struct %s {%s\n}\n" s.name (aux s.fields)

let rec string_of_builtin_types_values = function
  | Integer i -> Printf.sprintf "int:%d" i
  | Floating f -> Printf.sprintf "float:%f" f
  | Character c -> Printf.sprintf "char:%c" c
  | String s -> Printf.sprintf "string:%s" s
  | Boolean b -> if b then "bool:true" else "bool:false"
  | Struct st -> string_of_struct st
  | Void _ -> "void"
  | Pointer e -> "*" ^ (string_of_builtin_types_values e)
