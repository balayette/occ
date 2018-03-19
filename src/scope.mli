(** Holds all the scope information *)
type t [@@deriving show]

(** Create new scope information *)
val empty : unit -> t

(** Create a nested scope *)
val nested : t -> t

(** Update the offset after descending into a nested scope *)
val update_parent : t -> t -> t

(** Add a local variable to the scope *)
val add_local : t -> (string * Types.builtin_types) -> t
