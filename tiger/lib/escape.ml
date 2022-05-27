open Core_kernel 

module S = Symbol
module ST = Symbol_table

type value = {
  depth: int;
  escapes: bool ref;
}

module Table = struct 
  include Map.Make (Symbol)
end 

type env = {
  table: value Table.t;
  depth: int
}

let traverse_prog expr = 
  let open Syntax in 

  let rec tr_expr expr ~env = 
    match expr with 
    | Var var -> tr_var var ~env 
    | Call (_, args) -> tr_exprs args ~env 
    | Op (l, _, r) -> tr_op l r ~env 
    | Record (_, fields) -> tr_record fields ~env 
    | Seq exprs -> tr_exprs exprs ~env 
    | Assign (var, expr) -> tr_assign var expr ~env 
    | If (cond, t, f) -> tr_cond cond t f ~env 
    | While (cond, body) -> tr_while cond body ~env 
    | For (var, lo, hi, body, escapes) -> tr_for var lo hi body escapes ~env 
    | Let (decs, body) -> tr_let decs body ~env
    | Array (_, size, body) -> tr_array size body ~env 
    | Nil _ | Int _ | String _ | Break _ -> ()
  
  and tr_exprs exprs ~env = 
    List.iter exprs ~f:(fun e -> tr_expr e.L.value ~env)

  and tr_op l r ~env = 
    tr_expr l.L.value ~env;
    tr_expr r.L.value ~env;
  
  and tr_record fields ~env = 
    fields 
    |> List.map ~f:snd 
    |> tr_exprs ~env 
  
  and tr_assign var expr ~env = 
    tr_var var ~env;
    tr_expr expr.L.value ~env
  
  and tr_cond cond t f ~env = 
    tr_expr cond.L.value ~env;
    tr_expr t.L.value ~env;
    ignore @@ Option.map f ~f:(fun e -> tr_expr e.L.value ~env)
  
  and tr_while cond body ~env = 
    tr_expr cond.L.value ~env;
    tr_expr body.L.value ~env 
  
  and tr_for var lo hi body escapes ~env = 
    tr_expr lo.L.value ~env;
    tr_expr hi.L.value ~env;

    let data = { depth = env.depth; escapes } in 
    let table' = Table.set env.table ~key:var.L.value ~data in 
    let env' = { env with table = table' } in 
    tr_expr body.L.value ~env:env'
  
  and tr_var var ~env = 
    match var.L.value with 
    | SimpleVar sym -> 
      (match Table.find env.table sym.L.value with
      | Some v -> 
        if env.depth > v.depth 
        then begin 
        v.escapes := true;
        Trace.Escaping.escapes sym env.depth
        end
      | None -> ())
    | FieldVar (var, _) -> 
      tr_var var ~env 
    | SubscriptVar (var, expr) -> 
      tr_var var ~env;
      tr_expr expr.L.value ~env 
  
  and tr_let decs body ~env =
    let env' = tr_decs decs ~env in 
    tr_expr body.L.value ~env:env'
  
  and tr_array size body ~env = 
    tr_expr size.L.value ~env;
    tr_expr body.L.value ~env 
  
  and tr_decs decs ~env = 
    List.fold_left decs 
      ~f:(fun env dec -> tr_dec dec ~env)
      ~init:env 
  
  and tr_dec ~env = function 
    | TypeDec _ -> env
    | VarDec var_dec -> tr_var_dec var_dec ~env 
    | FunDec fun_decs -> tr_fun_decs fun_decs ~env
  
  and tr_var_dec var ~env = 
    let data = {depth = env.depth; escapes = var.L.value.escapes} in 
    let key =  L.(var.L.value.var_name.value) in 
    let table' = Table.set env.table ~key ~data in
    { env with table = table'}
  
  and tr_fun_decs decs ~env = 
    List.fold_left decs 
      ~f:(fun env dec -> tr_fun_dec dec.L.value ~env)
      ~init:env 
  
  and tr_fun_dec dec ~env = 
    let depth = env.depth + 1 in 
    let add_arg table (arg : field) = 
      let data = (depth; escapes = arg.escapes) in
      Table.set table ~key:arg.name.L.value ~data 
    in 
    let table' = List.fold_left 
      dec.params ~f:add_arg ~init:env.table in 
    let env' = { table = table'; depth } in 
    tr_expr dec.body.L.value ~env:env'; 
    env 
  
  in 
  let env = {table = Table.empty; depth = 0} in 
  tr_expr expr ~env 