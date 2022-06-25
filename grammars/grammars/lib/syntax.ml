type addop = 
  | OpPlus 
  | OpMinus
[@@deriving show]

type multop = 
  | OpTimes 
  | OpDiv 
[@@deriving show]

type expr =  
  expression

and expression = 
  | Term of term 
  | AdditiveExpression of expression * addop * term 
  | AssignExpression of name * int

and term =  
  | Factor of factor 
  | MultiplicativeExpression of term * multop * factor

and factor =  
  | Expression of expression 
  | Literal of literal
  | Name of name 

and name = 
  Id of string 

and literal = 
  | IntLit of int
  | FloatLit of float 