(** Local variable *)
type t

(** Create a local *)
val create : string -> int -> t

(** Get name *)
val name : t -> string

(** Get offset *)
val offset : t -> int
