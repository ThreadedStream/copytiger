module ST = Symbol_table 

type expr_ty = {
  expr: Translate.expr;
  ty: Type.t;
}

val trans_prog : Syntax.expr -> Fragment.t list 

val trans_expr : Syntax.expr Location.t -> env:Env.t -> expr_ty 

val trans_ty : Env.tenv -> Syntax.ty -> Type.t 