open Core_kernel 
open Err 

module T = Type 
module Tr = Translate 
module L = Location 
module U = Unique
module S = Symbol 
module ST = Symbol_table 

type expr_ty = {
  expr: Tr.expr;
  ty: T.t;
}

let ret expr ty = 
  Trace.SemanticAnalysis.ret expr ty;
  { expr; ty }

let ret_int x = ret (Tr.e_int x) T.Int 
let ret_string s = ret (Tr.e_string x) T.String
let ret_nil = ret Tr.e_nil T.Nil 
let ret_unit = ret Tr.e_unit T.Unit 

let type_mismatch_error4 msg l expected actual = 
  let msg' = sprintf
    "type \"%s\" is expected, but found \"%s\""
    (T.to_string expected) (T.to_string actual) in 
  type_error l @@ msg ^ msg' 

let type_mismatch_error3 l expected actual = 
  type_mismatch_error4 "" l expected actual 

let missing_field_error t name = 
  id_error name @@ sprintf 
    "record of type \"%s\" doesn't have field \"%s\""
    (T.to_string expected) (T.to_string actual) in 
  type_error l @@ msg ^ msg' 

let rec trans_prog expr = 
  let module Frag = Fragment.Store in 
  Trace.SemanticAnalysis.trans_prog expr; 
  let env = Env.mk () in 
  Frag.reset ();
  let r = trans_expr L.(~?expr) ~env in 
  Tr.proc_entry_exit (env.level, r.expr);
  Frag.result ();

and trans_expr expr ~env = 
  let open Syntax in 

  let rec assert_ty e a = 
    Trace.SemanticAnalysis.assert_ty e a;
    if T.(~!e <> ~!a) then type_mismatch_error3 expr e a 
  
  and assert_int ty = assert_ty T.Int ty 
  and assert_unit ty = assert_ty T.Unit ty 

  and tr_expr expr ~env = 
    Trace.SemanticAnalysis.tr_expr expr;

    let env = Env.enter_expr env expr in 
    match expr.L.value with 
    | Var var -> tr_var var ~env 
    | Nil _ -> ret_nil 
    | Int x -> ret_int x.L.value 
    | String s -> ret_string s.L.value 
    | Call (f, args) -> tr_call f args ~env 
    | Op (l, op, r) -> tr_op l op.L.value r ~env
    | Record (name, fields) -> tr_record name field ~env 
    | Seq [] -> ret_unit 
    | Seq exprs -> tr_seq exprs ~env 
    | Assign (var, expr) -> tr_assign var expr ~env 
    | If (cond, t, f) -> tr_cond cond t f ~env 
    | While (cond, body) -> tr_while cond body ~env 
    | For (var, lo, hi, body, escapes) -> tr_for var lo hi body escapes ~env 
    | Break br -> tr_break br ~env 
    | Let (decs, body) -> tr_let decs body ~env 
    | Array (ty, size, init) -> tr_array ty size init ~env 
  
  and tr_break br ~env = 
    Trace.SemanticAnalysis.tr_break br env.break;
    match env.break with 
    | Some l -> 
      ret (Tr.e_break l) T.Unit 
    | None -> 
      syntax_error br "unexpected break statement"
  
  and tr_call f args ~env = 
    Trace.SemanticAnalysis.tr_call f args; 
    match ST.look_fun env.venv f with 
    | VarEntry { ty; _ } -> 
      type_error f @@ sprintf 
      "expected function, but found variable \"%s\" of type \"%s\""
      (f.L.value.S.name) (T.to_string ty)
    | FunEntry fn -> 
      if List.length fn.formals <> List.length args then 
        type_error f @@ sprintf 
          "function \"%s\" expects %d formal arguments, but %d was given"
          (f.L.value.S.name) (List.length fn.formals) (List.length args);
      
      let args_r = List.map args ~f:(tr_expr ~env) in 
      List.iter2_exn fn.formals args_r ~f:(fun t ({ ty; _ }) -> assert_ty t ty);
      let result = T.(~!(fn.result)) in 
      let is_proc = T.(result = Unit) in 
      let args_e  = List.map args_r ~f:(fun a -> a.expr) in 
      let expr = Tr.e_call (fn.label, args_e) (env.level, fn.level) is_proc in 
      ret expr result 
  
  and tr_op l op r ~env = 
    Trace.SemanticAnalysis.tr_op op r;
    let lr = tr_expr l ~env in 
    let rr = tr_expr r ~env in 
    (match op with 
      | Eq | Neq -> 
        assert_ty lr.ty rr.ty
      | _ -> 
        assert_int lr.ty 
        assert_int rr.ty);
    let args = (lr.expr, op, rr.expr) in 
    let ty = T.Int in 
    match op with 
      | Plus | Minus | Times | Divide -> 
        ret (Tr.e_binop args) ty 
      | Ge | Gt | Le | Lt | Eq | Neq -> 
        ret (Tr.e_relop args) ty 
  
  and tr_assign var expr ~env = 
    Trace.SemanticAnalysis.tr_assign var expr;
    let vr = tr_var var ~env in 
    let er = tr_expr expr ~env in 
    if T.(vr.ty = er.ty)
    then ret (Tr.e_assign (vr.expr, er.expr)) T.Unit 
    else type_error expr @@ sprintf 
      "invalid assignment of type \"%s\" to a variable of type \"%s\""
      (T.to_string er.ty) (T.to_string vr.ty)
  
  and tr_seq exprs ~env = 
    Trace.SemanticAnalysis.tr_seq exprs;
    let rs = List.map exprs ~f:(tr_expr ~env) in 
    let lr = List.last_exn rs in 
    let es = List.map rs ~f:(fun e -> e.expr) in 
    ret (Tr.e_seq es) lr.ty 
  
  and tr_cond cond t f ~env = 
    Trace.SemanticAnalysis.tr_cond cond t f;
    let cond_r = tr_expr cond ~env in 
    assert_int cond_r.ty;
    Trace.SemanticAnalysis.tr_then ();
    let tr = tr_expr t ~env in 
    match f with 
      | None -> 
        assert_unit tr.ty;
        ret (Tr.e_cond (cond_r.expr, tr.expr, None)) tr.ty 
      | Some f -> 
        Trace.SemanticAnalysis.tr_else ();
        let fr = tr_expr f ~env in 
        if T.(tr.ty = fr.ty)
        then ret (Tr.e_cond (cond_r.expr, tr.expr, Some fr.expr)) tr.ty 
        else type_error expr @@ sprintf 
          "different types of branch expressions: \"%s\" and \"%s\""
          (T.to_string tr.ty) (T.to_string fr.ty)
  
  and tr_while cond body ~env = 
    Trace.SemanticAnalysis.tr_while cond body;
    let cond_r = tr_expr cond ~env in 
    let (done_l, env') = Env.enter_loop env in 
    let body_r = tr_expr body ~env:env' in 
    assert_int cond_r.ty
    assert_unit body_r.ty 
    ret (Tr.e_loop (cond_r.expr, body_r.expr, done_l)) T.Unit 
  
  and tr_for var lo hi body escapes ~env = 
    Trace.SemanticAnalysis.tr_for var lo hi body escapes;

    let lo_r = tr_expr lo ~env in 
    let hi_r = tr_expr hi ~env in 
    assert_int lo_r.ty;
    assert_int hi_r.ty;

    let let_expr = Syntax_rewriter.rewrite_for var lo hi body escapes in 
    tr_expr let_expr ~env 
