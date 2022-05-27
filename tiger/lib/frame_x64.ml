open Core_kernel 

type access = 
  | InFrame of int 
  | InReg of Temp.t 
[@@deriving show { with_path = false }]

type t = {
  id: int;
  label: Temp.label;
  formals: access list;
  mutable locals: int;
  instrs: Instruction.t list;
} [@@deriving show { with_path = false }]

let equal x y = x.id = y.id

let (=) = equal 
let (<>) x y = not (equal x y)

let word_size = 64 / 8 

let fp = Temp.mk_named "FP (RBP)"
let sp = Temp.mk_named "SP (RSP)"

let rdi = Temp.mk_named "RDI"
let rsi = Temp.mk_named "RSI"
let rdx = Temp.mk_named "RDX"
let rcx = Temp.mk_named "RCX"
let r8  = Temp.mk_named "R8"
let r9  = Temp.mk_named "R9"
let arg_regs = [rdi; rsi; rdx; r8; r9]

let rv1 = Temp.mk_named "RV1 (RAX)"

let rv2 = Temp.mk_named "RV2 (RDX)"

let rbx = Temp.mk_named "RBX"
let r10 = Temp.mk_named "R10"
let r11 = Temp.mk_named "R11"
let r12 = Temp.mk_named "R12"
let r13 = Temp.mk_named "R13"
let r14 = Temp.mk_named "R14"
let r15 = Temp.mk_named "R15"

let caller_regs = [r10; r11]

let calee_regs  = [rbx; r12; r13; r14; r15]

let access_expr access ~addr = 
  let open Ir in 
  match access with 
    | InFrame k -> addr <+> ~$k 
    | InReg t -> ~*t 

let next_id = 
  let n = ref (-1) in 
  fun () -> incr n; !n 

let mk_formal i = function 
  | true -> InFrame ((i + 1) * (-word_size))
  | false -> InReg (Temp.mk ())

let mk ~label ~formals = 
  let id = next_id () in 
  let formals = List.mapi formals ~f:mk_formal in 
  let locals = 0 in 
  let instrs = [] in 
  { id; label; formals; locals; instrs }

let id { id; _ } = id 

let formals { formals; _ } = formals 

let alloc_local frame ~escapes = 
  match escapes with 
  | true -> 
    frame.locals <- frame.locals + 1;
    let offset = (frame.locals + 1) * -word_size in 
    InFrame offset 
  | false -> 
    InReg (Temp.mk ())

let external_call name args = 
  let open Ir in 
  let r = Temp.mk () in 
  let fn_label = Runtime.label name in 
  let name = Temp.mk_label (Some fn_label) in
  let stmt = seq 
    [ Expr (Call (~:name, args))
      ; ~*r <<< ~*rv1 
    ] in 
  ESeq (stmt, ~*r)

let proc_entry_exit1 (_, body) = 
  body 

module Printer = struct 
  let print_access = function 
    | InFrame offset -> sprintf "F(%d)" offset 
    | InReg temp -> sprintf "R(%s)" (Temp.show temp)
  
  let print_frame frame = 
    let formals = List.map frame.formals ~f:print_access in 
    let lines = 
      ["       id: " ^ Int.to_string frame.id
       ; "  label: " ^ Temp.print_label frame.label
       ; "formals: " ^ String.concat formals ~sep:" "
       ; " locals: " ^ Int.to_string frame.locals
      ]
    in String.concat lines ~sep:"\n"
end 