open Grammars 
open Printf 

let display_number n = match n with 
  | Value.Int i -> Printf.printf "%d" i 
  | Value.Float f ->
    print_endline "flooooats";

let rec interp (e: Syntax.expr) = match e with 
  | Syntax.Term t -> interp_term t 
  | Syntax.AdditiveExpression(e, aop, t) -> interp_addexpr e aop t

and interp_term (t: Syntax.term) = match t with 
  | Syntax.Factor f -> interp_factor f 
  | Syntax.MultiplicativeExpression(t, mop, f) -> interp_multexpr t mop f

and interp_factor (f: Syntax.factor) = match f with 
  | Syntax.Expression e -> interp e 
  | Syntax.Literal lit -> interp_lit lit  

and interp_lit (lit: Syntax.literal) = match lit with 
  | IntLit i -> Value.Int(i) 
  | FloatLit f -> Value.Float(f)

and interp_addexpr (e: Syntax.expr) (aop: Syntax.addop) (t: Syntax.term) = match aop with 
  | Syntax.OpPlus -> 
    Value.(+) (interp e) (interp_term t) 
  | Syntax.OpMinus -> 
    Value.(-) (interp e) (interp_term t)

and interp_multexpr t mop f = match mop with  
  | Syntax.OpTimes -> 
    Value.( * ) (interp_term t) (interp_factor f)
  | Syntax.OpDiv -> 
    Value.(/) (interp_term t) (interp_factor f)

let process (line: string) = 
  let linebuf = Lexing.from_string line in 
  try 
    display_number (interp (Parser.main Lexer.token linebuf))
  with 
    | Lexer.Error msg -> 
      Printf.fprintf stderr "%s%!" msg 
    | Parser.Error -> 
      Printf.fprintf stderr "At offset %d: syntax error\n" (Lexing.lexeme_start linebuf)

let process (line: string option) = match line with 
    | None -> ()
    | Some l -> process l 

let rec repeat chan =
  let opt_line, continue = Lexer.line chan in 
  process opt_line;
  if continue then 
    repeat chan 

let () = 
  let chan = Lexing.from_channel stdin in 
  let env = Base.Map.empty (module Base.String) in 
  repeat chan 