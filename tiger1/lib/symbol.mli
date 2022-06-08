type t = {
  id: int;
  name: string;
} [@@deriving compare, equal, sexp, show]

val mk : string -> t

val mk_unique : string -> t 
 
val (=) : t -> t -> bool 
val (<>) : t -> t -> bool

val to_string : t -> string 
val to_string_loc : t Location.t -> string
