type t = {
  name: string;
  offset: int
}

let create name offset = {name; offset}

let name l = l.name

let offset l = l.offset
