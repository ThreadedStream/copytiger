open Lexer 

exception Parse_exception 

type parser = { tok_stream: token list; mutable pos: int}

let next p = match List.nth p.tok_stream p.pos with 
  | EOF -> None
  | tok -> Some tok 

let match_tok p tok = 
  if List.nth p.tok_stream p.pos == tok then 
    p.pos <- p.pos + 1   
  else 
    raise Parse_exception

let[@inline] new_parser tok_stream = {tok_stream=tok_stream; pos=0 }

let rec parse p =   
  assert (List.nth p.tok_stream 0 == Hello);
  p.pos <- p.pos + 1;
  match List.nth p.tok_stream p.pos with 
    | Let -> parse_let p 
    | Write -> parse_write p 
    | Read -> parse_read p
    | Id x -> parse_id x 

  and parse_let p = 
    let ids = ref [] in 
    let tok = next p in 
     


    while match_tok p Comma do
    done    

  and parse_write p = 
    failwith "TODO!"

  and parse_read p = 
    failwith "TODO!"

  and parse_id x = 
    failwith "TODO!"
