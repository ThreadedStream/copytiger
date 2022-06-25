%token <int> INT 
%token <string> ID 
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN ASSIGN
%token EOL 

%start <Syntax.expr> main 
%{ open Syntax %}  

%% 
  let main :=   
    ~ = expr; EOL; <>
  
  let expr :=
        | t = term;    { Syntax.Term(t) }
        | e = expr; aop = additiveop; t = term;  { Syntax.AdditiveExpression(e, aop, t) }
        | id = ID; ASSIGN; i = INT; { Syntax.AssignExpression(Syntax.Id(id), i) }

  let term :=
        | f = factor;    { Syntax.Factor(f)}
        | t = term; mop = multiplicativeop; f = factor; { Syntax.MultiplicativeExpression(t, mop, f) }
  
  let factor := 
        | LPAREN; e = expr; RPAREN; { Syntax.Expression(e) }
        | i = INT; { Syntax.Literal(i) }
  
  let additiveop == 
      | PLUS; { OpPlus }
      | MINUS; { OpMinus }

  let multiplicativeop == 
      | DIV; { OpDiv }
      | TIMES; { OpTimes }
%%