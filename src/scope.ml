module SS = Set.Make(
  struct
    let compare a b = Pervasives.compare (Local.name a) (Local.name b)
    type t = Local.t
  end )

let print_set fmt set =
  SS.iter (function x -> Format.pp_print_string fmt (
      Local.name x
    ));

type t = {
  offset: int;
  locals: SS.t; [@printer print_set]
} [@@deriving show]

let empty () =
  { offset=0; locals=SS.empty }

let nested parent =
  { offset=parent.offset; locals=parent.locals }

let update_parent parent child =
  { parent with offset=child.offset; }

let add_local scope = function
    (name, t) -> (
      let size = Types.size_of_type t in
      let loc = Local.create name scope.offset in
      { offset=scope.offset + size; locals=(SS.add loc scope.locals) }
    )
