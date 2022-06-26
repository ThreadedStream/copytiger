type expr =  
  | Expression of string
  | ExpressionList of expr list
  | ExplicitConstant of explicit_constant 

and explicit_constant = 
  | ExplicitIntegerConst of int 
  | ExplicitRealConst of float 
  | ExplicitStringConst of string 
  | True  
  | False