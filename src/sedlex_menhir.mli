(** The lexbuf type *)
type t

(** Create a lexbuf **)
val create_lexbuf : ?file:string -> Sedlexing.lexbuf -> t

(** Parse the lexbuf **)
val parse : (t -> Parser.token) -> (Parser.token, 'b) MenhirLib.Convert.traditional -> t -> 'b

(** Get the underlying stream **)
val get_stream : t -> Sedlexing.lexbuf

(** Register a new line in the lexer's position. *)
val new_line : ?n:int -> t -> unit

(** Update the position with the stream. *)
val update : t -> unit

(** The last matched word. *)
val lexeme : t -> string
