module%language Base = struct
  type expr = [ `Method of string * (expr list)
              | `Let of string * expr
              | `Call of string * expr
              | `Value of int
              | `Test of int
              ]
end

module%language Base0 = struct
  include Base
  type expr = {
    del : [`Test of int]
  }
end

let[@pass Base => Base0] test_pass =
  [%passes
    let[@entry] rec expr = function
      | `Test (i) -> (
          Printf.printf "Removing `Test %d\n" i;
          `Value i
        )
  ]

let rec print_lang ?(lev="") exp = match exp with
  | `Method (s, el) -> (
      Printf.printf "%sMethod : %s\n" lev s;
      let rec aux = function
        | e::l -> print_lang ~lev:(lev ^ " ") e; aux l
        | [] -> ()
      in aux el
    )
  | `Let (s, e) -> (
      Printf.printf "%sLet %s = ...\n" lev s;
      print_lang ~lev:(" " ^ lev) e
    )
  | `Value (i) -> (
      Printf.printf "%s= %i\n" lev i
    )
  | `Call (s, e) -> (
      Printf.printf "%s%s(\n" lev s;
      print_lang ~lev:(lev ^ " ") e;
      Printf.printf "%s)\n" lev
    )
  | _ -> ()

let () =
  print_string "CMP\n";
  (* let a = 5
     print a
  *)
  let input =
    `Method ("test",
             [
               (`Let ("a", (`Test 5)));
               (`Call ("print", (`Value 5)))
             ])
  in
  print_lang input;

  let output = test_pass input in
  print_lang output;
  print_string "DONE\n";;
