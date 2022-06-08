module S = Symbol 
module L = Location 


let rewrite_for var lo hi body escapes = 
  let open Syntax in 
  let open L in 
  Trace.SyntaxRewriting.rewrite_for var lo hi body escapes;
  let i = SimpleVar var in
  let limit_sym = ~?(S.mk "limit") in 
  let limit = SimpleVar limit_sym in 

  let i_dec = 
    { var_name = var;
      var_typ = None;
      init = lo;
      escapes;
    } in 
  let limit_dec = 
    { var_name = limit_sym; 
      var_typ = None;
      init = hi;
      escapes = ref false; 
    } in 
  let decs =  
    [ VarDec ~?i_dec 
     ; VarDec ~?limit_dec
    ] in 
  let cond = Op (~?(Var ~?i), ~?Le, ~?(Var ~?limit)) in
  let incr = Op (~?(Var ~?i), ~?Plus, ~?(Int ~?1)) in 
  let assign = Assign (~?i, ~?incr) in 
  let body = Seq [body; ~?assign] in 
  let loop = While (~?cond, ~?body) in 
  ~?(Let (decs, ~?loop))