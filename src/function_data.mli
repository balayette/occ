type t [@@deriving show]

(** Get the name of the function **)
val name : t -> string

(** Get the return type *)
val return_type : t -> Types.builtin_types

(** Get the parameters *)
val parameters : t -> (string * Types.builtin_types) list

(**  name of the function, list of types & variable names **)
val create_fdata : Types.builtin_types -> string
  -> (string * Types.builtin_types) list -> t
