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
    | Array of builtin_types * (builtin_types list) * int option
[@@deriving show]

val string_of_builtin_types : builtin_types -> string

val string_of_struct : user_struct -> string

val string_of_builtin_types_values : builtin_types -> string

val size_of_type : builtin_types -> int
