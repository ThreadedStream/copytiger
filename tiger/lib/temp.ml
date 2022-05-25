open Core_kernel

module S = Symbol 

type t = int * string option 
[@@deriving compare, equal, show { with_path = false }]
