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
      [ ~*r <<< ~$1 
      ; cond(t, f) 
      ; ~|f
      ; ~*r <<< ~$0
      ; ~|t 
      ] in
    ESeq (stmt, ~*r)
  | Nx s -> ESeq (s, ~$0)

let unNx expr = 
  let open Ir in 
  match expr with
  | Ex e -> Expr e 
  | Nx s -> s 
  | Cx cond -> 
    let l = Temp.mk_label None in 
    let stmt = cond(l, l) in 
    Seq(stmt, ~|l)

let unCx expr = 
  let open Ir in 
  match expr with 
  | Ex (Const 0) -> fun (_, f) -> ~:f <|~ [f]
  | Ex (Const 1) -> fun (t, _) -> ~:t <|~ [t]
  