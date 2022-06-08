open Core_kernel 

module L = Location
module S = Symbol 
module T = Type 

type op = 
  | Plus
  | Minus 
  | Times 
  | Divide 
  | Ge 
  | Gt
  | Le
  | Lt 
  | Eq 
  | Neq
[@@deriving show { with_path = false }]

type field = {
  name: S.t L.t;
  typ: S.t L.t;
  escapes : bool ref;
} [@@deriving show { with_path = false }]

type ty = 
  | NameTy of S.t L.t
  | RecordTy of field list 
  | ArrayTy of S.t L.t 
[@@deriving show { with_path = false }]

type expr = 
  | Var of var L.t 
  | Nil of unit L.t 
  | Int of int L.t 
  | String of string L.t 
  | Call of S.t L.t * 
          expr L.t list 
  | Op of expr L.t * 
          op L.t * 
          expr L.t 
  | Record of S.t L.t *
              ( S.t L.t * expr L.t) list 
  | Seq of expr L.t list 
  | Assign of var L.t * 
              expr L.t 
  | If of expr L.t *
          expr L.t * 
          expr L.t option 
  | While of expr L.t * 
             expr L.t 
  | For of S.t L.t * 
            expr L.t *
            expr L.t *
            expr L.t *
            bool ref 
  | Break of unit L.t 
  | Let of dec list * 
           expr L.t 
  | Array of S.t L.t * 
             expr L.t * 
             expr L.t 
[@@deriving show { with_path = false }]

and var = 
  | SimpleVar of S.t L.t 
  | FieldVar of 
      var L.t *
      S.t L.t 
  | SubscriptVar of 
      var L.t * 
      expr L.t 
[@@deriving show { with_path = false }]

and dec = 
  | TypeDec of type_dec L.t list 
  | VarDec of var_dec L.t 
  | FunDec of fun_dec L.t list 
[@@deriving show { with_path = false }]

and var_dec = {
  var_name: S.t L.t;
  var_typ: S.t L.t option;
  init: expr L.t;
  escapes: bool ref;
} [@@deriving show { with_path = false }]

and type_dec = {
  type_name: S.t L.t;
  typ: ty;
} [@@deriving show { with_path = false }]

and fun_dec = {
  fun_name: S.t L.t;
  params: field list;
  body: expr L.t;
  result_typ: S.t L.t option;
} [@@deriving show { with_path = false }]