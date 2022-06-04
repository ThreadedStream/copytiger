open Core_kernel
open Ir 

module S = Symbol 
module ST = Symbol_table 
module L = List 

module BlockMap = Map.Make (Symbol)

type block = stmt list 

let commute s e = 
  match s, e with
    | Expr (Const _), _ -> true 
    | _, Name _ -> true 
    | _, Const _ -> true 
    | _, _ -> false 
  
let (<-->) s e = commute s e 

let join s1 s2 =
  match s1, s2 with 
    | s1, Expr (Const _) -> s1 
    | Expr (Const _), s2 -> s2 
    | s1, s2 -> Seq (s1, s2)
  
let (++) s1 s2 = join s1 s2 

let rec reorder = function 
  | [] ->   
    (nop, [])
  | (Call _ as call) :: es -> 
    let t = Temp.mk () in 

    reorder @@ ESeq (~*t <<< call, ~*t) :: es
  | e :: es -> 
    let (s1, e1) = pull_expr e in 
    let (s2, es') = reorder es in 
    if s1 <--> e1
    then 
      (s1 ++ s2, e1 :: es')
    else 
      let t = Temp.mk () in 
      (s1 ++ (~*t <<< e1) ++ s2, ~*t :: es')

and reorder_stmt exprs build = 
  let (s, l) = reorder exprs in 
  s ++ build l 

and reorder_expr exprs build = 
  let (s, l) = reorder exprs in 
  (s, build l)

and pull_stmt = function 
  | Seq (s1, s2) -> 
    pull_stmt s1 ++ pull_stmt s2 
  | Jump (e, l) -> 
    reorder_stmt [e] (fun es -> L.hd_exn es <|~ l)
  | CJump { op; left; right; t; f } -> 
    reorder_stmt [left; right]
      (fun es -> CJump { op; left = L.hd_exn es; right = L.(hd_exn (tl_exn es)); t; f})
  | Move (Temp t, Call (name, args)) -> 
    reorder_stmt (name :: args)
      (fun es -> ~*t <<< Call (L.hd_exn es, L.tl_exn es))
  | Move (Temp t, src) -> 
    reorder_stmt [src] (fun es -> ~*t <<< L.hd_exn es)
  | Move (Mem addr, e) -> 
    reorder_stmt [addr; e] 
      (fun es -> ~@(L.hd_exn es) <<< (L.(hd_exn (tl_exn es))))
  | Move (ESeq (s, e1), e2) -> 
    pull_stmt (Seq (s, e1 <<< e2))
  | Expr (Call (name, args)) -> 
    reorder_stmt (name :: args)
      (fun es -> Expr (Call (L.hd_exn es, L.tl_exn es)))
  | Expr e -> 
    reorder_stmt [e] (fun es -> Expr (L.hd_exn es))
  | e -> 
    reorder_stmt [] (fun _ -> e)

and pull_expr = function 
  | BinOp (l, op, r) -> 
    reorder_expr [l; r]
      (fun es -> BinOp (L.hd_exn es, op, L.(hd_exn (tl_exn es))))
  | Mem addr -> 
    reorder_expr [addr] (fun es -> ~@(L.hd_exn es))
  | ESeq (s, e) -> 
    let s1 = pull_stmt s in 
    let (s2, e') = pull_expr e in 
    (s1 ++ s2, e')
  | Call (name, args) -> 
    reorder_expr (name :: args)
      (fun es -> Call (L.hd_exn es, L.tl_exn es))
  | e -> 
    reorder_expr [] (fun _ -> e)
  

let%expect_test "reorder empty" = 
  let result = reorder [] in 
  print_string ([%show: stmt * expr list] result);
  [%expect {| ((Expr (Const 0)), []) |}]

let%expect_test "reorder call" = 
  let name = Temp.mk_label (Some "f") in 
  let stmts = [Call (~:name, [~$1])] in 
  let result = reorder stmts in 
  print_string ([%show: stmt * expr list] result);
  [%expect {|
    ((Move ((Temp (17, None)), 
        (Call ((Name { id = 5; name = "f" }), [(Const 1)])))),
        [(Temp (17, None))])  |}]

let rec linear stmt acc =  
  match stmt with 
    | Seq (s1, s2) -> linear s1 (linear s2 acc)
    | s -> s :: acc 

let%expect_test "linear" = 
  let l = Temp.mk_label (Some "a") in 
  let t = Temp.mk () in 
  let stmts = [~:l <|~ [l]; ~|l; ~*t <<< ~$1] in 
  let actual = linear (seq stmts) [] in 
  print_string ([%show: stmt list] actual);
  [%expect {|
    [(Jump ((Name { id = 6; name = "a" }), [{ id = 6; name = "a" }])); 
      (Label { id = 6; name = "a" }); (Move ((Temp (18, None)), (Const 1)))]  |}]

let linearize stmt = 
  linear (pull_stmt stmt) []

let rec mk_blocks stmts acc ~done_label = 
  match stmts with 
    | Label _ as label :: rest -> 
      let rec next stmts block = 
        match stmts with 
        | (Jump _ as s) :: ss | (CJump _ as s) :: ss -> 
          end_block ss (s :: block)
        | Label l :: _ -> 
          let jump = ~:l <|~ [l] in 

          next (jump :: stmts) block
        | s :: ss ->
          next ss (s :: block)
        | [] -> 
          let jump = ~:done_label <|~ [done_label] in 
          next [jump] block 
      
      and end_block stmts rest = 
        let block = List.rev rest in 
        mk_blocks stmts (block :: acc) ~done_label
      
      in 
      
      let new_block = [label] in 
      next rest new_block
    | [] -> 
      List.rev acc 
    | ss -> 
      let label = Temp.mk_label None in 
      mk_blocks (~|label :: ss) acc ~done_label 

      
  let basic_blocks stmts =
    let done_label = Temp.mk_label None in 
    let block_list = mk_blocks stmts [] ~done_label in 
    (done_label, block_list)
  
  let find_block map key = 
    Trace.Canon.find_block key;
    BlockMap.find map key 

  let add_block map key data = 
    Trace.Canon.set_block key;
    BlockMap.set map ~key ~data 
  
  let enter_block map = function 
    | (Label s :: _) as b -> add_block map s b
    | _ -> map 
  
  let rec trace ~map block label rest = 
    let map = add_block map label [] in 
    let most = L.drop_last_exn block in 
    match L.last_exn block with 
      | Jump (Name label', _) -> 
        (match find_block map label' with
          | Some ((_ :: _) as block') ->
            most @ trace ~map block' label rest
          | _ -> 
            block @ trace_next ~map rest)
      | CJump { op; left; right; t; f; } -> 
        let t_block = find_block map t in 
        let f_block = find_block map f in 
        (match t_block, f_block with 
          | _, Some ((_::_) as block') -> 
            block @ trace ~map block' label rest
          | Some ((_::_) as block'), _ -> 
            let inv_cjump = CJump { op = not_rel op; left; right; t; f} in 
            most @ [inv_cjump] @ trace ~map block' label rest
          | _, _ -> 
            let l = Temp.mk_label None in 
            let stmts = 
              [ CJump { left; op; right; f = t; t = f }
              ; ~|l 
              ; ~:l <|~ [l]
              ] in 
            most @ stmts @ trace_next ~map rest
          )
          | Jump _ -> 
            block @ trace_next ~map rest 
          | _ -> 
            failwith "Basic block does not end with conditional or unconditional jump statement"
  and trace_next ~map = function 
    | (Label label :: _) as block :: rest -> 
      (match find_block map label with 
        | Some _ -> trace ~map block label rest
        | None -> trace_next ~map rest)
    | [] -> [] 
    | _ -> failwith "Basic block does not start with a label"
  let trace_schedule (done_label, blocks) = 
    let init = BlockMap.empty in 
    let map = List.fold_left blocks ~init ~f:enter_block in 
    let result = trace_next ~map blocks in
    result @ [~|done_label] 
