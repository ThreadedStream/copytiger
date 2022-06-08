open Core_kernel

module S = Symbol 

type t = int * string option 
[@@deriving compare, equal, show { with_path = false }]

type label = Symbol.t 
[@@deriving compare, equal, show  { with_path = false }]

let mk_internal = 
  let idx = ref (-1) in 
  fun name -> incr idx; (!idx, name)

let mk () = mk_internal None 
let mk_named name = mk_internal (Some name)

let mk_label = 
  let idx = ref (-1) in 
  fun name -> 
    match name with 
    | Some s -> 
      S.mk s 
    | None -> 
      incr idx;
      !idx |> Int.to_string |> S.mk

let print_temp (id, name) = 
  match name with 
  | Some s -> s 
  | None -> Int.to_string id 

let print_label s = 
  Symbol.(sprintf "%s <#%d>" s.name s.id)

