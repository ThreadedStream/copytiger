type expr [@@deriving show]

type level = {
  parent: level option;
  frame: Frame.t;
} [@@deriving show]

val parent_frames : level -> Frame.t list 
val frames_path : level -> int list 
val nesting_depth : level -> int

type access = level * Frame.access [@@deriving show]

val outermost : level 

val new_level 
  : parent:level option 
  -> label:Temp.label 
  -> formals:bool list 
  -> level 

val formals : level -> access list 

val alloc_local : level:level -> escapes:bool -> access 

module Sl : sig 
  val follow : cur:level -> def:level -> Ir.expr 
end 

module Printer : sig 
  val print_expr : expr -> string 
end 

val e_unit : expr 
val e_nil : expr 
val e_int : int -> expr 
val e_string : string -> expr 
val e_binop : expr * Syntax.op * expr -> expr 
val e_relop : expr * Syntax.op * expr -> expr 
val e_simple_var : access * level -> expr 
val e_subscript_var : expr -> expr -> expr 
val e_field_var : expr -> int -> expr 
val e_record : expr list -> expr 
val e_array : expr * expr -> expr 
val e_cond : expr * expr * expr option -> expr 
val e_loop : expr * expr * Temp.label -> expr 
val e_break : Temp.label -> expr 

val e_call 
  : Temp.label * expr list 
  -> level * level 
  -> bool 
  -> expr 

val e_assign : expr * expr -> expr 
val e_seq : expr list -> expr 
val e_let : expr list * expr -> expr 

val e_dummy : unit -> expr 

val proc_entry_exit : level * expr -> unit 