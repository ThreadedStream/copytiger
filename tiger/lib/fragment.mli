type t = 
  | Proc of proc 
  | String of Temp.label * string 
[@@deriving show]

and proc = {
  frame: Frame.t;
  body: Ir.stmt;
} [@@deriving show]

val print : t -> string

module Store : sig 
  val push_proc : proc -> unit

  val push_string : string -> Ir.expr 

  val reset : unit -> unit 

  val result : unit -> t list
end 
