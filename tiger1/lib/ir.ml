open Ppx_compare_lib.Builtin 

module Sy = Syntax 

type expr = 
  | Const of int 
  | Name of Temp.label 
  | Temp of Temp.t 
  | BinOp of expr * binop * expr 
  | Mem of expr 
  | Call of expr * expr list 
  | ESeq of stmt * expr 
[@@deriving compare, equal, show { with_path = false}]

and stmt = 
  | Move of expr * expr 
  | Expr of expr 
  | Jump of expr * Temp.label list 
  | CJump of cjump 
  | Seq of stmt * stmt 
  | Label of Temp.label 
[@@deriving compare, equal, show { with_path = false }]

and cjump = {
  op : relop;
  left : expr;
  right : expr;
  t : Temp.label;
  f : Temp.label;
} [@@deriving compare, equal, show { with_path = false }]

and binop = 
  | Plus | Minus | Mul | Div 
  | And | Or | Xor 
  | LShift | RShift | ARShift 
[@@deriving compare, equal, show { with_path = false }]

and relop = 
  | Eq | Ne 
  | Lt | Gt | Le | Ge 
  | Ult | Ugt | Ule | Uge 
[@@deriving compare, equal, show { with_path = false }]

let binop_of_op = function 
  | Sy.Plus -> Plus 
  | Sy.Minus -> Minus 
  | Sy.Times -> Mul 
  | Sy.Divide -> Div 
  | op -> failwith @@ 
    "Invalid integer arithmetic operator: " ^ 
    (Syntax_printer.print_op_sym op)

let relop_of_op = function 
  | Sy.Ge -> Ge 
  | Sy.Gt -> Gt 
  | Sy.Le -> Le 
  | Sy.Lt -> Lt 
  | Sy.Eq -> Eq 
  | Sy.Neq -> Ne 
  | op -> failwith @@ 
    "Invalid relational operator: " ^ 
    (Syntax_printer.print_op_sym op)

let not_rel = function 
  | Eq -> Ne
  | Ne -> Eq 
  | Lt -> Ge 
  | Ge -> Lt 
  | Gt -> Lt 
  | Le -> Gt 
  | Ult -> Uge 
  | Uge -> Ult 
  | Ule -> Ugt 
  | Ugt -> Ule 

let (|+|) l r = BinOp (l, Plus, r)
let (|-|) l r = BinOp (l, Minus, r)
let (|*|) l r = BinOp (l, Mul, r)

let (~$) k = Const k 
let (~:) l = Name l 
let (~@) e = Mem e 
let (~*) t = Temp t 

let (~|) l = Label l 

let (<<<) e1 e2 = Move (e1, e2)
let (<|~) e labels = Jump (e, labels)

let (<+>) l r = ~@(l |+| r)
let (<->) l r = ~@(l |-| r)

let indexed e i w = ~@(~@e |+| (i |*| ~$w))

let cjump left op right t f = 
  CJump { op = relop_of_op op; left; right; t; f }

let nop = Expr ~$0

let rec seq = function 
  | s :: [] -> s 
  | s :: ss -> Seq (s, seq ss)
  | [] -> nop 