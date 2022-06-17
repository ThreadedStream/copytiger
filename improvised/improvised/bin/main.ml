
type token = Let
  | Read
  | Write
  | Hello
  | Bye 
  | Id of string 
  | Comma
  | Semicolon
  | ErrTok
  [@@deriving show]

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

type stream = {  mutable chr: char option; mutable line_num: int; chan: in_channel }

let open_stream filename = 
  let chan = open_in filename in 
  { chr=None; line_num=0; chan=chan}

let close_stream stream = close_in stream.chan 

let read_char stream = match stream.chr with 
  | None -> let c = input_char stream.chan in 
          if c == '\n' then 
            let _ = stream.line_num <- stream.line_num + 1 in c
          else c 
  | Some c -> stream.chr <- None; c

let unread_char stream c = stream.chr <- c

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

let is_keyword id = 
  id == "write" || id == "read" || id == "let"

let eof = '\000'

let tokenize_id id = match id with 
  | "write" -> Write 
  | "read" -> Read
  | "let"  -> Let 
  | _ -> Id(id)

let tokenize stream =
  let token = ref "" in
  let c = ref '0' in 
  c := read_char stream;
  match !c with 
  | 'a'..'z' | 'A'..'Z' -> 
    token := String.concat "" [!token; Char.escaped !c];
    c := read_char stream; 
    while not (is_space(!c)) do
      token := String.concat "" [!token; Char.escaped !c];
      c := read_char stream
    done;
    tokenize_id !token
  | eof -> print_endline "eof"; Id("eof")

let () = 
  let filename = Sys.argv.(1) in 
  let stream = open_stream filename in
  let tok = ref ErrTok in 
  tok := tokenize stream;
  while not (!tok == Id("eof")) do
    print_endline (show_token !tok);
    tok := tokenize stream
  done
