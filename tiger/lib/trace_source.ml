open Ppx_compare_lib.Builtin 

type target = 
  | Stdout
  | File of string 
[@@deriving compare, equal, show]

type t = 
  | SymbolTable of target list 
  | SemanticAnalysis of target list 
  | StackFrame of target list 
  | Translation of target list 
  | Escaping of target list 
[@@deriving compare, equal, show]