{
  open Parser 

  exception Error of string 
}

rule line = parse 
  | ([^'\n']* '\n') as line 
    { Some line, true }
  | eof 
    { None, false }
  | ([^'\n']+ as line) eof 
    { Some (line ^ "\n"), false }

and token = parse 
  | [' ' '\t']
    { token lexbuf }
  | '\n'
    { EOL }
  | ['0'-'9']+ as i 
    { INT (int_of_string i)}
  | ['0'-'9']*['.']['0'-'9']+ as f 
    { FLOAT (float_of_string f)}
  | '+'
    { PLUS }
  | '-'
    { MINUS }
  | '*'
    { TIMES }
  | '/'
    { DIV }
  | '('
    { LPAREN }
  | ')'
    { RPAREN }
  | '='
    { ASSIGN }
  | ['a'-'z''A'-'Z']+['a'-'z''A'-'Z''0'-'9']* as id 
    { ID id}