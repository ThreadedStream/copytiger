module type S = sig 
  type t [@@deriving show]

  val (=) : t -> t -> bool
  val (<>) : t -> t -> bool

  type access [@@deriving show]

  val fp : Temp.t 

  val sp : Temp.t 

  val rv1 : Temp.t 

  val rv2 : Temp.t 

  val access_expr : access -> addr:Ir.expr -> Ir.expr 

  val word_size : int 

  val mk : label:Temp.label -> formals:bool list -> t 

  val id : t -> int 

  val formals : t -> access list 

  val alloc_local : t -> escapes:bool -> access 

  val external_call : string -> Ir.expr list -> Ir.expr 

  val proc_entry_exit1 : t * Ir.stmt -> Ir.stmt 

  module Printer : sig 
    val print_frame : t -> string 
    val print_access : access -> string 
  end 
end 
