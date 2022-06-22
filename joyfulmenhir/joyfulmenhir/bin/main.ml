open Joyfulmenhir
open Joyfulmenhir.Syntax 

let rec interp (e : expr) = match e with 
  | {loc=_; value=ELiteral i} -> i 
  | {loc=_; value=EBinOp(l, op, r)} -> (
      match op with 
        | OpPlus -> (interp l) + (interp r)
        | OpMinus -> (interp l) - (interp r)
        | OpTimes -> (interp l) * (interp r)
        | OpDiv -> (interp l) * (interp r)
  )
  | {loc=_; value=EUnOp(_, e)} -> -(interp e)

let process (line : string) = 
  let linebuf = Lexing.from_string line in 
  try  
    Printf.printf "%d\n%!" (interp (Parser.main Lexer.token linebuf))
  with 
    | Lexer.Error msg -> 
      Printf.fprintf stderr "%s%!" msg
    | Parser.Error -> 
      Printf.fprintf stderr "At offset %d: syntax error\n" (Lexing.lexeme_start linebuf)
  
let process (line : string option) = match line with 
  | None -> () 
  | Some line -> process line 

let rec repeat chan = 
  let opt_line, continue = Lexer.line chan in 
  process opt_line;
  if continue then 
    repeat chan 

let () =  
  let file_name = Sys.argv.(1) in 
  let chan = Lexing.from_channel (open_in file_name) in 
  repeat chan 