open Core_kernel 

module S = Symbol 
module L = Location 
module T = Type 
module Tr = Translate

type target = 
  | Stdout 
  | File of string 
[@@deriving compare, equal, show]

type source = 
  | Symbol of target list 
  | Semant of target list 
[@@deriving compare, equal, show]


module SymbolTable = struct
  let src = Logs.Src.create "tig.symbol-table" ~doc:"Symbol table"

  let trace_loc op name sym = 
    Logs.debug ~src (fun m -> 
      m ~header:"symbol" "%s %s: %s" op name (S.to_string_loc sym))
    
  let bind name sym = trace_loc "<==" name sym
  let look name sym = trace_loc "==>" name sym
end 

module Canon = struct
  let src = Logs.Src.create "tig.symbol-table" ~doc:"Symbol table"

  let trace op sym = 
    Logs.debug ~src (fun m -> 
      m ~header:"canon" "%s: %s" op (S.to_string sym))

  let set_block name = trace "<<-" name 
  let find_block name = trace "->>" name 
end 

module SemanticAnalysis = struct 
  open Syntax
  open Syntax_printer 
  
  let src = Logs.Src.create "tig.semantic-analysis" ~doc:"Semantic analysis"
  let trace f = Logs.debug ~src (fun m -> f (m ~header: "semant"))
  let trace_tr name expr = trace @@ fun m -> m ">>> %s: %s" name expr 

  let trans_prog _ = 
    trace (fun m -> m "trans_prog")
  let trans_ty typ =
    trace_tr "trans_ty" (print_ty typ)
  let tr_expr expr = 
    trace_tr "tr_expr" (print_expr expr.L.value)
  let tr_var var = 
    trace_tr "tr_var" (print_var val.L.value)
  let tr_simple_var sym = 
    trace_tr "tr_simple_var" (print_simple_var sym)
  let tr_field_var var field = 
    trace_tr "tr_field_var" (print_field_var var field)
  let tr_subscript_var var sub = 
    trace_tr "tr_subscript_var" (print_subscript_var var sub)
  let tr_call f args = 
    trace_tr "tr_call" (print_call f args)
  let tr_op l op r = 
    trace_tr "tr_op" (print_op l op r)
  let tr_record ty_name vfields = 
    trace_tr "tr_record" (print_record ty_name vfields)
  