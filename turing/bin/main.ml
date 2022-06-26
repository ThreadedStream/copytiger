open Turing 

let rec walk_ast (e: Syntax.expr) = match e with 
  | Syntax.ExpressionList el -> 
    let _ = List.map (walk_ast) el in 
    ()
  | Syntax.Expression e -> 
    print_endline e  
  | Syntax.ExplicitConstant ec -> 
    match ec with 
      | ExplicitIntegerConst i -> Printf.printf "ExplicitIntegerConst (%d)\n" i 
      | ExplicitRealConst f -> Printf.printf "ExplicitRealConst (%f)\n" f 
      | ExplicitStringConst s -> Printf.printf "ExplicitStringConst (%s)\n" s
      | True  -> Printf.printf "True\n" 
      | False -> Printf.printf "False\n"  
    

let tok_to_string = function 
  | Parser.EXPLICIT_STRING_CONST s -> Printf.printf "EXPLICIT_STRING_CONST with value %s" s
  | Parser.EXPLICIT_UNSIGNED_REAL_CONST r -> Printf.printf "EXPLICIT_UNSIGNED_REAL_CONST with value %f" r
  | Parser.TRUE -> Printf.printf "TRUE"
  | Parser.FALSE -> Printf.printf "FALSE"
  | _ -> print_endline "unknown"

let () = 
  let input_prog = "32.123e2, 232.2, 45, true, false" in 
  let chan = Lexing.from_string input_prog in
  let prog = Parser.program Lexer.token chan in 
  walk_ast prog
  