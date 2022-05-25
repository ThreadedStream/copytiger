open Core_kernel 

module F = Frame 
module Frag = Fragment.Store 

type expr = 
  | Ex of Ir.expr 
  | Nx of Ir.stmt 
  | Cx of (Temp.label * Temp.label -> Ir.stmt)
[@@deriving show { with_path = false }]

let unEx expr = 
  let open Ir in 
  match expr with 
  | Ex e -> e 
  | Cx cond -> 
    let r = Temp.mk () in 
    let t = Temp.mk_label None in 
    let f = Temp.mk_label None in 
    let stmt = seq 
      [ ~*r <<< ~$1 ]