open Improvised 

type token = Let
  | Read
  | Write
  | Hello
  | Bye 
  | Id of string 
  | Comma
  | Semicolon
  | LParen
  | RParen 
  | ErrTok
  | EOF 
  [@@deriving show]


type stream = {  mutable last_chr: char option; mutable line_num: int; chan: in_channel }
 
let read_file filename = 
  let lines = ref [] in 
  let chan = open_in filename in 
  try 
    while true; do 
      lines := input_line chan :: !lines 
    done; !lines 
  with End_of_file -> 
    close_in chan;
    List.rev !lines

let explode s = 
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in 
  exp (String.length s - 1) []


let open_stream filename = { last_chr=None; line_num=0; chan=open_in filename}

let close_stream stream = close_in stream.chan 

let is_digit c =
  let code = Char.code c in 
  (code >= Char.code('0')) && (code <= Char.code('9'))

let is_alpha c = 
  let code = Char.code c in 
  (code >= Char.code('A') && code <= Char.code('Z')) ||
  (code >= Char.code('a') && code <= Char.code('z'))

let is_alphanum c = is_digit c || is_alpha c

let is_id token = 
  List.map (is_alphanum) token

let is_space c = (c == '\t') || (c == '\n') || (c == '\r') || (c == ' ')

let advance stream = stream.last_chr <- In_channel.input_char stream.chan 

let rec eat_space stream =
  match stream.last_chr with 
    | None -> () 
    | Some c -> 
      if (is_space c) then 
        advance stream
      else 
        () 
  
let is_keyword id = 
  id == "write" || id == "read" || id == "let"

let is_delim c = 
  c == ',' || c == ';' || c == '(' || c == ')'

let eof = '\000'

let tokenize_id id = match id with 
  | "write" -> Write 
  | "read" -> Read
  | "let"  -> Let 
  | "hello" -> Hello 
  | "bye"  -> Bye 
  | _ -> Id(id)

let rec scan s = 
  s.last_chr <- In_channel.input_char s.chan;
  match s.last_chr with 
  | Some c -> 
    (match c with 
      | 'a'..'z' | 'A'..'Z' ->   
        let token = ref "" in 
        while is_alphanum (Option.get s.last_chr) do 
          token := !token ^ (Char.escaped (Option.get s.last_chr));
          s.last_chr <- In_channel.input_char s.chan
        done; 
        tokenize_id !token
      | '(' -> LParen 
      | ')' -> RParen 
      | ';' -> Semicolon 
      | ',' -> Comma 
      | _ -> 
        if is_space c then 
          (eat_space s; scan s)
        else 
          ErrTok
    )
  | None -> 
    (* Run out of symbols to process *)
    EOF

let () = 
  (* let filename = Sys.argv.(1) in 
  let stream = open_stream filename in
  let tok = ref ErrTok in 
  tok := scan stream;
  while not (!tok == EOF) do
    print_endline (show_token !tok);
    tok := scan stream
  done *)
  let open Ast in 
  let nodes = [LetStmt([Id(Symbol.make "x"); Id(Symbol.make "y")]); 
               ReadStmt([Id(Symbol.make "x"); Id(Symbol.make "y")]); 
              WriteStmt([Id(Symbol.make "x"); Id(Symbol.make "y")])] in 
  tr_stmts nodes