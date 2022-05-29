open Core_kernel 

module F = Frame 
module Frag = Fragment.Store 

type expr = 
  | Ex of Ir.expr 
  | Nx of Ir.stmt 
  | Cx of (Temp.label * Temp.label -> Ir.stmt)
[@@deriving show { with_path = false }]

let unEx expr = 
  let open Ir in 
  match expr with 
  | Ex e -> e 
  | Cx cond -> 
    let r = Temp.mk () in 
    let t = Temp.mk_label None in 
    let f = Temp.mk_label None in 
    let stmt = seq 
      [ ~*r <<< ~$1 
      ; cond(t, f) 
      ; ~|f
      ; ~*r <<< ~$0
      ; ~|t 
      ] in
    ESeq (stmt, ~*r)
  | Nx s -> ESeq (s, ~$0)

let unNx expr = 
  let open Ir in 
  match expr with
  | Ex e -> Expr e 
  | Nx s -> s 
  | Cx cond -> 
    let l = Temp.mk_label None in 
    let stmt = cond(l, l) in 
    Seq(stmt, ~|l)

let unCx expr = 
  let open Ir in 
  match expr with 
  | Ex (Const 0) -> fun (_, f) -> ~:f <|~ [f]
  | Ex (Const 1) -> fun (t, _) -> ~:t <|~ [t]
  | Ex e -> fun (f, t) -> 
    cjump e Eq ~$0 t f 
  | Cx cond -> cond 
  | Nx _ -> failwith "unCx cannot convert Nx (statement) to Cx (conditional)"

type level = {
  parent: level option;
  frame: F.t 
} [@@deriving show { with_path = false }]

type access = level * F.access 
[@@deriving show { with_path = false }]

let outermost = 
  let label = Temp.mk_label None in 
  let frame = F.mk ~label ~formals:[] in 
  { parent = None; frame }

let rec parent_frames level = 
  match level.parent with 
  | None -> [level.frame]
  | Some parent -> level.frame :: parent_frames parent 

let frames_path level =
  level |> parent_frames |> List.map ~f:F.id 

let nesting_depth level = 
  level |> parent_frames |> List.length 

let new_level ~parent ~label ~formals = 
  let static_link = true in 
  let formals = static_link :: formals in 
  let frame = F.mk ~label ~formals in 
  { parent; frame }

let formals level = 
  F.formals level.frame 
  |> List.tl_exn 
  |> List.map ~f:(fun access -> level, access)

let alloc_local ~level ~escapes = 
  let access = F.alloc_local level.frame ~escapes in 
  level, access 

module Sl = struct 
  let rec follow ~cur ~def = 
    if F.(cur.frame = def.frame) 
    then 
      Ir.(~*F.fp)
    else 
      match F.formals cur.frame with 
      | sl :: _ -> 
        (match cur.parent with 
        | None -> 
          failwith "Nested level without parent"
        | Some parent -> 
          let addr = follow ~cur:parent ~def in 
          F.access_expr sl ~addr
        )
      | [] -> 
        failwith "No static link in formals"
end

let cx_cjump l op r = 
  Cx (fun (t, f) -> Ir.cjump l op r t f)

let e_unit  = Ex Ir.(~$0)
let e_nil   = Ex Ir.(~$0)
let e_int n = Ex Ir.(~$n)

let e_string s = 
  Ex (Frag.push_string s)

let e_binop (l, op, r) = 
  Ex Ir.(BinOp (unEx l, binop_of_op op, unEx r))

let e_relop (l, op, r) = 
  cx_cjump (unEx l) op (unEx r)

let e_simple_var ((var_level, access), level) = 
  let addr = Sl.follow ~cur:level ~def:var_level in 
  Ex (F.access_expr access ~addr)

let e_subscript_var expr sub = 
  let e = unEx expr in 
  let i = unEx sub in 
  Ex Ir.(indexed e i F.word_size)

let e_field_var expr i =
  let open Ir in 
  let r = Temp.mk () in 
  let e = unEx expr in 
  let offset = i * F.word_size in 
  let stmt = (~*r <+> ~$offset) <<< e in 
  Ex (ESeq (stmt, ~*r))

let e_record_field i expr ~record = 
  Ir.((~*record <+> ~$(i * F.word_size)) <<< unEx expr)

let e_record fields = 
  let open Ir in
  let record = Temp.mk () in 
  let size = List.length fields * F.word_size in 
  let call = F.external_call Runtime.initRecord [~$size] in 
  let init_record = ~*record <<< call in
  let init_fields = List.mapi fields ~f:(e_record_field ~record) in 
  let stmt = seq @@ init_record :: init_fields in 
  Ex (ESeq (stmt, ~*record))

let e_array (size, init) = 
  let args = [unEx size; unEx init] in 
  Ex (F.external_call Runtime.initArray args)

let e_cond (cond_expr, then_expr, else_expr) = 
  let open Ir in 
  let t = Temp.mk_label None in 
  let f = Temp.mk_label None in 
  let cond = unCx cond_expr in 
  match then_expr, else_expr with 
  | Ex then_ex, Some else_e -> 
    let r = Temp.mk () in 
    let x = Temp.mk_label None in
    let stmt = seq 
      [ cond(t, f)
      ; ~|t 
      ; ~*r <<< then_ex 
      ; ~:x <|~ [x]
      ; ~|f 
      ; ~*r <<< unEx else_e 
      ; ~|x 
      ] in 
    Ex (ESeq(stmt, ~*r))
  | Nx then_nx, Some else_e -> 
    let x = Temp.mk_label None in 
    let stmt = seq 
      [ cond (t, f) 
       ; ~|t
       ; then_nx 
       ; ~:x <|~ [x]
       ; ~|f 
       ; unNx else_e 
       ; ~|x
      ] in 
    Nx stmt 
  | Nx then_nx, None -> 
    let stmt = seq 
      [ cond(t, f)
       ; ~|t 
       ; then_nx 
       ; ~|f 
       ] in 
    Nx stmt 
  | Cx _, Some (Nx _) -> 
    failwith "Translation phase has detected that \ 
              then-branch is a conditional expression, but else-branch is a statement. \
              Looks like semantic analysis is broken"
  | Cx then_cx, Some (Ex _ | Cx _ as else_e) -> 
    let else_cx = unCx else_e in 
    let stmt (t', f') = seq 
      [ cond (t, f)
        ; ~|t; then_cx(t', f')
        ; ~|f; else_cx(t', f')
      ] in 
    Cx stmt 
  | then_ex, None -> 
    let stmt = seq 
      [ cond(t, f) 
      ; ~|t 
      ; unNx then_ex 
      ; ~|f 
      ] in 
    Ex (ESeq (stmt, ~$0))

let e_loop (cond_expr, body_expr, done_l) = 
  let open Ir in 
  let cond_l = Temp.mk_label None in 
  let body_l = Temp.mk_label None in 
  let cond_e = unEx cond_expr in
  let cond_j = cjump cond_e Eq ~$0 done_l body_l in 
  let body_e = unNx body_expr in 
  let stmt = seq 
    [ ~|cond_l 
      ; cond_j 
      ; ~|body_l 
      ; body_e 
      ; ~:cond_l <|~ [cond_l]
      ; ~|done_l
    ] in 
  Nx stmt 

let e_break l = 
  Nx Ir.(~:l <|~ [l])

let e_call (label, args) (cur_level, def_level) is_proc = 
  let open Ir in 
  let args_e = List.map args ~f:unEx in 
  let args_e = 
    match def_level.parent with 
    | None -> args_e 
    | Some def -> 
      let sl = Sl.follow ~cur:cur_level ~def in 
      sl :: args_e 
  in 
  let call = Call (~:label, args_e) in 
  if is_proc then Nx (Expr call) else Ex call 

let e_assign (dst, src) = 
  Nx Ir.(unEx dst <<< unEx src)

let rec e_seq = function 
  | [] -> Ex Ir.(~$0)
  | e :: [] -> e 
  | e :: es -> Ex Ir.(ESeq (unNx e, unEx (e_seq es))) 

let e_let (dec_exprs, body_expr) = 
  let open Ir in 
  let dec_es = seq @@ List.map dec_exprs ~f:unNx in 
  let body_e = unEx body_expr in 
  Ex (ESeq (dec_es, body_e))


let e_dummy () = Ex (Ir.Const 1)

let proc_entry_exit ({ frame; _}, body) = 
  let open Ir in
  let stmt = ~*F.rv1 <<< unEx body in 
  let body = F.proc_entry_exit1 (frame, stmt) in 
  Frag.push_proc {frame; body}

module Printer = struct 
  let print_expr = function 
    | Ex expr -> 
      sprintf "ex:\n%s" (Ir_printer.print_expr expr)
    | Nx stmt -> 
      sprintf "nx:\n%s" (Ir_printer.print_stmt stmt)
    | Cx gen_stmt -> 
      let l = Temp.mk_label (Some "left") in
      let r = Temp.mk_label (Some "right") in 
      let stmt = gen_stmt (l, r) in 
      sprintf "cx:\n(%s, %s) ->\n%s"
        (Temp.print_label l)
        (Temp.print_label r)
        (Ir_printer.print_stmt stmt)
end 
