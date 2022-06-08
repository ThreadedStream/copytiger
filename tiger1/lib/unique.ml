open Ppx_compare_lib.Builtin

type t = int [@@deriving compare, equal, show]

let mk = 
  let n = ref (-1) in 
  fun () -> incr n; !n 

let to_string = 
  Printf.sprintf "#%d"


