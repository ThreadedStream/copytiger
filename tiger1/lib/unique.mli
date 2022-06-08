type t [@@deriving compare, equal, show]

val mk : unit -> t

val to_string : t -> string
