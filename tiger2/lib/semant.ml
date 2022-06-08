open Translate
open Llvm
 
module E = Env 
module T = Types 
module A = Absyn 
module S = Symbol 
module Err = Error.Error 
module U = Util 

type venv = E.enventry Symbol.table 
type tenv = T.ty Symbol.table 
type expty = {exp: Translate.exp; ty: T.ty}
type decty = { v_env : venv; t_env: tenv }

type name_type = { name: S.symbol; ty: T.ty}

let nested_loop_level = ref 0
let change_nested_loop_level oper = nester_loop_level := oper !nested_loop_level 1
let increase_nested_level () = change_nested_loop_level (+)
let decrease_nested_level () = change_nested_loop_level (-)
let get_nested_level () = !nested_loop_level


let trans_type ((t_env: tenv), (ty: A.ty)): T.ty = 
  let look_up_type ((s: S.symbol), (p: int)): T.ty = 
    match S.look(t_env, s) with 
      | Some t -> t 
      | None -> (Err.Error p ("Type" ^ S.name(s) ^ " has not been declared\n"); T.NIL) in 
  
  let map_field_to_record (A.Field {name; typ; pos; escape = _}): S.symbol * T.ty = 
    (name, look_up_type(typ, pos)) in 
  
  let check_record (fields: A.field list): T.ty = 
    T.RECORD (List.map map_field_to_record fields, Temp.newtemp()) in 
  
  let check_array_type e = T.ARRAY(look_up_type e, Temp.newtemp()) in 

  let rec trans_ty: A.ty -> T.ty = function 
    | A.NameTy (s, p) -> look_up_type (s, p)
    | A.RecordTy e -> check_record e 
    | A.ArrayTy (s, p) -> check_array_type (s, p)
    | A.FuncTy (args, ret, p) -> T.FUNC_CLOSURE(List.map trans_ty args, trans_ty ret)
  in 
  trans_ty ty 

let rec trans_dec (
      (v_env: venv), 
      (t_env: tenv),
      (level: Translate.level),
      (exps: Absyn.dec list),
      break
): decty = 
  
  let check_var_dec (
        (v_env: venv),
        (t_env: tenv),
        A.VarDec { name; typ; init; pos; escape; order}
  ) = 
    let {exp = initial_value; ty = rhs_type} = trans_exp (v_env, t_env, level, init, break)
    in 
    match typ with 
      Some (s, p) -> 
        (match S.look(t_env, s) with 
          | Some lhs_type -> 
            if T.eq(lhs_type, rhs_type)
            then 
              begin 
                let access = Translate.alloc_local level !order (S.name name) lhs_type in 
                let new_entry = E.VarEntry{ty = lhs_type; access = access} in 
                let new_v_env = S.enter(v_env, name, new_entry) in 

                let addr = Translate.simple_var_left access (S.name name) level in 
                let value = match rhs_type with 
                  | T.NIL -> Translate.nil_exp lhs_type 
                  | _ -> initial_value
                in 
                Translate.assign_stm addr value; 
                {
                  v_env = new_v_env;
                  t_env = t_env;
                }
              end
            else (
              let msg = Print.sprintf 
                  "Can't assign exp type '%s' to type '%s'"
                  (T.name(rhs_type)) (S.name(s)) in 
              Err.error p msg 
              { v_env = v_env; t_env = t_env }
            )
            | None -> (
              let msg = Printf.sprintf 
                  "Type '%s' has not been declared"
                  (S.name(s)) in 
              Err.error pos msg; 
              { v_env = v_env; t_env = t_env })
            )
          | None -> 
            begin 
              (match rhs_type with 
                | T.NIL -> Err.error pos "Can't assign Nil to non-record type variable"
                | _ -> ());

              let access = Translate.alloc_local level !order (S.name name) rhs_type in 
              let new_entry = E.VarEntry{ty = rhs_type; access = access} in 
              let addr = Translate.simple_var_left access "var_dec" level in 

              Translate.assign_stm addr initial_value; 
              { 
                v_env = S.enter(v_env, name, new_entry);
                t_env = t_env
              }
            end
    in 
    
    let check_type_dec ( 
          (v_env: venv),
          (t_env: tenv),
          (types: A.typeDec list)
       ) = 
      let add_dump_type t_env (A.Type {name; _}) = 
        S.enter(t_env, name, T.NAME(name, ref None)) in 
      
      let dump_t_env = List.fold_left add_dump_type t_env types in 

      let f { v_env; t_env } (A.Type { name; ty; pos}) = 
        { t_env = S.enter(t_env, name, trans_type(t_env, ty)); v_env = v_env}
      in 

      let resolve_dummy_type { v_env; t_env } (A.Type {name; ty; pos}) = 
        let actual_type = match S.look (t_env, name) with 
          | Some t -> U.actual_ty t_env t 
          | None -> 
            Error.Error.error pos ("Type " ^ S.name(name) ^ " can't be resolved!");
