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
  (* ignoring whitespace *)
  | ['\r''\t''\n'' ']+ 
    { token lexbuf }
  (* ignoring comments *)
  | ['%'] [^'\r''\n'] ['\r''\n']*
    { token lexbuf }
  | eof
    { EOF }
  | '.'
    { DOT }
  | ".."
    { DOTDOT }
  | ':'
    { COLON }
  | '+'
    { PLUS }
  | '-'
    { MINUS }
  | '*'
    { TIMES }
  | '/'
    { DIV }
  | "div"
    { DIV }
  | "mod"
    { MOD }
  | "**"
    { POWER }
  | '<'
    { LESS_THAN }
  | '>' 
    { GREATER_THAN }
  | '='
    { ASSIGN }
  | ">="
    { GREATER_THAN_OR_EQ }
  | "<="
    { LESS_THAN_OR_EQ }
  | "not="
    { NOT_EQ }
  | "not"
    { NOT }
  | "and" 
    { AND }
  | "or" 
    { OR }
  | '('
    { LPAREN }
  | ')'
    { RPAREN }
  | '='
    { ASSIGN }
  | ','
    { COMMA }
  | "close" 
    { CLOSE }
  | "type"
    { TYPE }
  | "end"
    { END }
  | "record"  
    { RECORD }
  | "var"
   { VAR }
  | "const" 
    { CONST }
  | "of"
    { OF }
  | "int"
    { INT }
  | "real"
    { REAL }
  | "boolean"
    { BOOLEAN }
  | "string"
    { STRING }
  | "array"
    { ARRAY } 
  | "get"
    { GET }
  | "put"
    { PUT }
  | "skip"
    { SKIP }
  | "open"
    { OPEN }
  | "true"
    { TRUE }
  | "false"
    { FALSE }
  | ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']* as id 
    { ID id}
  | '\"'[^'\"']*'\"' as str_const
    { EXPLICIT_STRING_CONST str_const}
  | ('+'|'-')? ['0'-'9']+ as int_const
    { EXPLICIT_UNSIGNED_INTEGER_CONST (int_of_string int_const) }
  | ('+'|'-')? (['0'-'9']+ ['.'])? ['0'-'9']+ (['e'] ['0'-'9']+)? as real_const 
    { EXPLICIT_UNSIGNED_REAL_CONST (float_of_string real_const) }
