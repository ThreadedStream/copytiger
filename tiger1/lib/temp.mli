type t [@@deriving compare, equal, show]

type label = Symbol.t [@@deriving compare, equal, show]

val mk : unit -> t 

val mk_named : string -> t 

val mk_label : string option -> label 

val print_temp : t -> string 

val print_label : label -> string 

