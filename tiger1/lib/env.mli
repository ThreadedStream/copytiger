module T = Type 
module Tr = Translate 
module L = Location
module S = Symbol 
module ST = Symbol_table 

type var_entry = {
    access: Tr.access;
    ty: T.t 
}

type fun_entry = {
    level: Tr.level;
    label: Temp.label;
    formals: T.t list;
    result: T.t
}

type ventry = 
    | VarEntry of var_entry
    | FunEntry of fun_entry
[@@deriving show]

type venv = ventry ST.t 
type tenv = T.t ST.t 

type t = {
    tenv : tenv;
    venv : venv;
    level : Tr.level;
    path : (Syntax.expr L.t) list;
    break : Temp.label option;
}

val mk : unit -> t

val enter_expr : t -> Syntax.expr L.t -> t 

val enter_loop : t -> Temp.label * t

val base_venv : venv

val base_tenv : tenv 