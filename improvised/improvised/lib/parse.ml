open Lexer 


exception Parse_exception 

type parser = { tok_stream: token list; mutable pos: int}

let[@inline] next p = match List.nth p.tok_stream p.pos with 
  | EOF -> None
  | tok -> p.pos <- p.pos + 1; Some tok 

let match_tok p tok = 
  if List.nth p.tok_stream p.pos == tok then 
    p.pos <- p.pos + 1   
  else 
    raise Parse_exception

let[@inline] peek p = List.nth p.tok_stream p.pos 

let[@inline] new_parser tok_stream = {tok_stream=tok_stream; pos=0 }

let rec parse p =   
  assert (List.nth p.tok_stream 0 == Hello);
  let nodes = ref [] in 
  p.pos <- p.pos + 1;
  match List.nth p.tok_stream p.pos with 
    | Let -> let _ = next p in parse_let p 
    | Write -> parse_write p 
    | Read -> parse_read p
    | Id x -> parse_id x 

  and parse_let p = 
    let ids = ref [] in 
    let tok = next p in 
    match tok with 
      | Some t -> 
        ( match t with 
          | Id x -> 
            ids := (parse_id x) :: !ids;
            while peek p == Comma do
              let _ = next p in
              match peek p with 
                | Id x -> ids := (parse_id x) :: !ids
                | _ -> failwith "expected id"
            done;
            match_tok p Semicolon;
            ids := List.rev !ids;
            Ast.LetStmt !ids
        )
      | None -> failwith "expected id"

  and parse_write p = 
    Ast.WriteStmt []

  and parse_read p = 
    Ast.ReadStmt []

  and parse_id x = 
    Ast.IdExpr(Symbol.make x)
