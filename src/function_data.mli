type t [@@deriving show]

(** Get the name of the function **)
val name : t -> string

(**  name of the function, list of types & variable names **)
val create_fdata : Types.builtin_types -> string
  -> (string * Types.builtin_types) list -> (Types.builtin_types * string) list
  -> bool -> t

(** Get the index of the variable **)
val get_index : t -> string -> int

(** Print the variables and their indexes **)
val print_vars : t -> unit

(** Get the variables in a hashtable *)
val variables : t -> (string, int) Hashtbl.t

(** Get the offset needed for all local variables *)
val total_offset : t -> int
