open Improvised 

let () = 
  let open Lexer in
  let open Parse in 
  let filename = Sys.argv.(1) in 
  let stream = open_stream filename in
  let tok = ref ErrTok in
  let tok_stream = ref [] in 
  stream.last_chr <- In_channel.input_char stream.chan;
  tok := scan stream;
  tok_stream := !tok :: !tok_stream;
  while not (!tok == EOF) do
    tok := scan stream;
    tok_stream := !tok :: !tok_stream
  done;
  tok_stream := List.rev !tok_stream; 
  (* parsing *)
  let parser = new_parser !tok_stream in
  parse parser
  ()
