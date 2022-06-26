
type number = 
  | Int of int 
  | Float of float
[@@deriving show]

let (+) v1 v2 = match (v1, v2) with  
  | (Int lhs, Int rhs) -> Int(lhs+rhs)
  | (Float lhs, Float rhs) -> Float(lhs +. rhs)
  | _ -> failwith "type coercions are not supported yet"

let (-) v1 v2 = match (v1, v2) with  
  | (Int lhs, Int rhs) -> Int(lhs-rhs)
  | (Float lhs, Float rhs) -> Float(lhs -. rhs)
  | _ -> failwith "type coercions are not supported yet"


let ( * ) v1 v2 = match (v1, v2) with 
  | (Int lhs, Int rhs) -> Int(lhs * rhs)
  | (Float lhs, Float rhs) -> Float(lhs *. rhs)
  | _ -> failwith "type coercions are not supported yet"

let (/) v1 v2 = match (v1, v2) with 
  | (Int lhs, Int rhs) -> Int(lhs / rhs)
  | (Float lhs, Float rhs) -> Float(lhs /. rhs)
  | _ -> failwith "type coercions are not supported yet"

