module U = Unique 
module S = Symbol 

type t = 
  | Int 
  | String 
  | Record of (S.t * t) list * U.t 
  | Array of t * U.t 
  | Nil 
  | Unit 
  | Name of S.t * t option ref 
[@@deriving show]


let compare x y = 
  match x,y with 
  | Record(_, u1), Record (_, u2) -> 
    compare u1 u2 
  | Record _, Nil -> 0
  | Nil, Record _ -> 0
  | Array (_, u1), Array (_, u2) -> 
    compare u1 u2 
  | Name (sx, _), Name (sy, _) -> 
    S.(compare sx sy)
  | x, y ->
    compare x y

let rec actual = function
    | Name (sym, { contents = None }) -> 
      Err.type_error (Location.dummy sym) @@ 
      Printf.sprintf "type %s is undefined" sym.name
    | Name (_, { contents = Some t }) -> 
      actual t 
    | t -> t

let (=) x y = compare x y = 0
let (<>) x y = not (x = y)
let (~!) x = actual x

let assignable x y = 
  match ~!x, ~!y with 
  | Record _, Nil -> true 
  | a, b -> a = b

let (@==) x y  = assignable x y
let (@<>) x y  = not (assignable x y)

let rec to_string x = 
  let open Core_kernel in 
  match x with 
    | Int -> "int"
    | String -> "string"
    | Nil -> "nil"
    | Unit -> "()"
    | Name (s, _) -> s.name 
    | Array (t, u) -> sprintf "[%s]<#%s>" (to_string t) (U.to_string u)
    | Record (_, u) -> sprintf "record<#%s>" (U.to_string u)
