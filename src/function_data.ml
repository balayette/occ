type t =
  { return_t : Types.builtin_types;
    name : string;
    parameters : (string * Types.builtin_types) list;
    variables : (string, int) Hashtbl.t; [@opaque]
      total_offset : int;
    leaf : bool;
  } [@@deriving show]

let name d = d.name

let variables d = d.variables

let total_offset d = d.total_offset

let build_variables vars =
  let htbl = Hashtbl.create 10 in
  let rec aux id vars = match vars with
      [] -> ()
    | e::l -> (
        match e with
        (*TODO : Compute offsets *)
          (_, n) -> Hashtbl.add htbl n (id + 8); aux (id + 8) l
      )
  in aux 0 vars; htbl

let create_fdata return_t name parameters vars leaf =
  let variables = build_variables vars in
  let total_offset = 10 in
  {
    return_t;
    name;
    parameters;
    variables;
    total_offset;
    leaf;
  }

let get_index d n = Hashtbl.find d.variables n

let print_vars d =
  Hashtbl.iter (fun n i -> (
        Printf.printf "%s : %d\n" n i
      )) d.variables
