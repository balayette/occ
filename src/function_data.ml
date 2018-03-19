type t =
  { return_t : Types.builtin_types;
    name : string;
    parameters : (string * Types.builtin_types) list;
  } [@@deriving show]

let name d = d.name

let return_type d = d.return_t

let parameters d = d.parameters

let create_fdata return_t name parameters =
  {
    return_t;
    name;
    parameters;
  }
