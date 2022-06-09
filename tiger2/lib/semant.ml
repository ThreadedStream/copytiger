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
            T.NIL 
        in 
        { t_env = S.enter(t_env, name, actual_type); v_env = v_env }
    in 
    
    let check_func_dec (
        (v_env : venv), 
        (t_env : tenv),
        (fs: A.fundec list)) = 
      let look_type_up (s, p): T.ty = 
        match S.look(t_env, s) with 
          | Some t -> t 
          | None -> (Err.error p ("Type " ^ S.name(s) ^ "has not been declared"); T.NIL)
      in 
      
      let get_type_for_result = function 
        | Some (s, p) -> look_type_up (s, p)
        | None -> T.NIL 
      in 

      let get_type (A.Field {name; escape = _; typ; pos}): T.ty = 
        look_type_up (typ, pos)
      in 

      let get_name (A.Field {name}): S.symbol = name in 

      let get_name_type x = {name = get_name x; ty = get_type x} in 

      let check_result_type (expect_type, result_type) = 
        if T.eq(expect_type, T.NIL) then true 
        else T.eq(expect_type, result_type)
      in 

      let add_func_header acc (A.Func {name; params; result; body; pos}) = 
        let f param = get_type param |> (U.actual_ty t_env) in 
        let type_list = List.map f params in 
        let result_type = get_type_for_result result in 
        let label = Temp.newlabel in 
        Translate.add_func_header (S.name name) result_type type_list;
        S.enter(acc, name, E.FunEntry{
                          formals = type_list;
                          result = result_type;
                          label = name;
                          level = level
                      });
        in 

        let add_new_func_entry (cur_v_env: venv) (A.Func {name; params; result; body; pos}) = 
          let param_name_type = List.map get_name_type params in 
          let result_type = get_type_for_result result in
          let escapes = List.map (fun (A.Field {escape}) -> !escape) params in 
          let label = match S.look(cur_v_env, name) with 
            | Some(E.FunEntry {label; _}) -> label 
            | _ -> Temp.newlabel() in 
          let func_level = Translate.new_level level in 

          let body_type = ref T.NIL in 

          let get_dumb_venv_to_compute_esc () = 
            let f v_env {name; ty} = 
              S.enter (v_env, name, E.VarEntry{ ty = ty; access = Translate.dummy_access})
            in 
            List.fold_left f cur_v_env param_name_type 
          in 

          let add_arg_bindings (alloc_addrs: Translate.access list) = 
            let f v_env {name; ty} access = 
              S.enter (v_env, name, E.VarEntry{ ty = ty; access = access })
            in 
            let body_venv = List.fold_left2 f cur_v_env param_name_type alloc_addrs in 
            let translate_body () = 
              let { exp = bodyIr; ty = result_body_type } = trans_exp(body_venv, t_env, func_level, body, break) in 
              body_type := result_body_type;
              (result_body_type, bodyIr) 
            in
            translate_body
          in 

          let dumb_v_esc_venv = get_dumb_venv_to_compute_esc() in 
          let esc_vars = Link.extract_esc(dumb_v_esc_venv, t_env, body) in 
          let next_esc_order = 1 + List.length esc_vars in 

          let gen_arg_mappings
              (result, index)
              (A.Field {escape} as e) = 
            let esc_order = if !escape then index else -1 in 
            let new_index = if !escape then index + 1 else index in 
            let cur_map: arg_name_type_map = {name = get_name e; ty = get_type e; esc_order = esc_order} in 
            let new_result = result @ [cur_map] in 
            (new_result, new_index)
          in 
          let (args, _) = List.fold_left gen_arg_mappings ([], next_esc_order) params in 
          Translate.func_dec 
            func_level
            (S.name name)
            result_type
            esc_vars 
            args 
            add_arg_bindings; 
          (if check_result_type(result_type, !body_type) then ()
            else 
              let msg = Printf.sprintf "return type '%s' does not match with '%s'" 
                        (T.name !body_type) (T.name result_type) in 
              Err.error pos msg); 
            cur_v_env
        in 
        let base_env = List.fold_left add_func_header v_env fs in 
        let newv_env = List.fold_left add_new_func_entry base_env fs in 
        { v_env = newv_env; t_env = t_env }
      in 

      let tr_dec = function 
        | (v_env, t_env, (A.VarDec _ as e)) -> check_var_dec (v_env, t_env, e)
        | (v_env, t_env, A.TypeDec(e)) -> check_type_dec (v_env, t_env, e)
        | (v_env, t_env, A.FunctionDec(e)) -> check_func_dec (v_env, t_env, e)
      in 
      
      let helper = fun {v_env = v_env; t_env = t_env} dec -> tr_dec (v_env, t_env, dec) in 
      let result = List.fold_left helper { v_env = v_env; t_env = t_env } exps in 
      result 
