
let header = "
  #include <stdio.h>
  #include <stdlib.h>

  int main() {

"

let trailer = "
  }"

type buffer = { mutable buf: string }


type stmt = LetStmt of stmt list 
  | WriteStmt of stmt list
  | ReadStmt of stmt list 
  | Id of Symbol.t 

let rec tr_stmt context buf s = match s with  
  | LetStmt bs -> tr_let buf bs 
  | WriteStmt args -> tr_write buf args 
  | ReadStmt args -> tr_read buf args 
  | Id id -> tr_id context buf id
  | _ -> failwith "unknown node"

  and tr_let buf bs = 
    let _ = List.map (tr_stmt "let" buf) bs in 
    ()

  and tr_write buf args = 
    buf.buf <- buf.buf ^ "printf(\"";
    for i = 0 to List.length args - 1 do 
      buf.buf <- buf.buf ^ "%d,"
    done; 
    buf.buf <- String.sub buf.buf 0 (String.length buf.buf - 1);
    buf.buf <- buf.buf ^ "\",";
    let _ = List.map (tr_stmt "write" buf) args in
    buf.buf <- String.sub buf.buf 0 (String.length buf.buf - 1);
    buf.buf <- buf.buf ^ ");\n"

  and tr_read buf args =  
    buf.buf <- buf.buf ^ "scanf(\"";
    for i = 0 to List.length args - 1 do 
      buf.buf <- buf.buf ^ "%d,"
    done;
    buf.buf <- String.sub buf.buf 0 (String.length buf.buf - 1);
    buf.buf <- buf.buf ^ "\",";
    let _ = List.map (tr_stmt "read" buf) args in
    buf.buf <- String.sub buf.buf 0 (String.length buf.buf - 1);
    buf.buf <- buf.buf ^ ");\n";
    ()

  and tr_id context buf id = match context with 
    | "let" -> 
      buf.buf <- buf.buf ^ "int " ^ (Symbol.sym id) ^ " = 0;\n";
    | "write" | "read" -> 
      buf.buf <- buf.buf ^ (Symbol.sym id) ^ ","
    | _ -> failwith "unknown context, must be one of (let, write, read)"

let tr_stmts stmts = 
  let buf = {buf=""} in 
  buf.buf <- buf.buf ^ header;
  let _ = List.map (tr_stmt "" buf) stmts in 
  buf.buf <- buf.buf ^ trailer;
  print_endline buf.buf