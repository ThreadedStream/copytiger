%token <string> ID 
%token <string> EXPLICIT_STRING_CONST
%token <float> EXPLICIT_UNSIGNED_REAL_CONST
%token <int> EXPLICIT_UNSIGNED_INTEGER_CONST 
%token PLUS MINUS TIMES DIV 
%token MOD POWER 
%token VAR CONST TYPE
%token INT REAL BOOLEAN STRING ARRAY
%token LESS_THAN GREATER_THAN GREATER_THAN_OR_EQ LESS_THAN_OR_EQ
%token NOT_EQ NOT 
%token END RECORD 
%token AND OR 
%token OF 
%token ASSIGN 
%token COLON 
%token GET PUT SKIP OPEN CLOSE
%token COMMA DOT DOTDOT 
%token TRUE FALSE 
%token LPAREN RPAREN
%token EOF 

%start <Syntax.expr> program 
%{ open Syntax %}  

%%    
	let program := 	
		~ = expr; EOF; <>

	let expr := 	
		| e = expression; { e }
		| el = expression_list; { el }
		

	let type_declaration := 
		| TYPE; id = ID; COLON; ts = type_spec; { (*TODO*) }

	let type_spec := 
		| st = standard_type; { st }
		| srt = subrange_type; { srt }
		| at = array_type; { at }
		| rt = record_type; { rt }
		| nt = named_type; { nt }
	
	let standard_type := 
		| INT; { (*TODO*) }
		| REAL; { (*TODO*) }
		| BOOLEAN; { (*TODO*) }
		| STRING; cte = opt_comp_time_expression; { (*TODO*) }
	
	let subrange_type := 
		| cte = compile_time_expression; DOTDOT; exp = expression; { (* TODO *) }
	
	let array_type := 
		| ARRAY; itl = index_type_nonempty_list; OF; ts = type_spec; { (* TODO *) } 

	let record_type :=	
		| RECORD; idl = identifier_nonempty_list; COLON; ts = type_spec; END; RECORD; { (* TODO *) }

	let named_type := 
		| id = ID; { (* TODO *) }

	let subprogram_declaration :=	
		| sh = subprogram_header; sb = subprogram_body; { (* TODO *) }

	let subprogram_header := 	
		| PROCEDURE; id 	

	let type_spec_id_nonempty_list := 
		| ts = type_spec; { (* TODO *) }
		| hd = identifier_spec_component; tl = type_spec_id_nonempty_list; { (* TODO *) }

	let identifier_nonempty_list := 
		| id = ID; { (* TODO *) }
		| hd = ID; COMMA; tl = identifier_nonempty_list; { (* TODO *) }
	
	let identifier_spec_component := 
		| idl = identifier_nonempty_list; COLON; ts = type_spec; { (* TODO*) }

	let index_type_nonempty_list := 
		| it = index_type; { (* TODO *) }
		| it = index_type; COMMA; itl = index_type_nonempty_list; { (* TODO *) }

	let index_type := 
		| st = subrange_type; { (* TODO *) }
		| nt = named_type; 	{ (* TODO *) }

	let opt_comp_time_expression :=
		| (* empty *) { None }
		| LPAREN; cte = compile_time_expression; RPAREN; { (* TODO *) }
	
	(* expression types *)

	let open_statement := 
		| OPEN; COLON; fn = file_number; COMMA; s = string; COMMA; caplist = capability_list; { (* TODO *) }  

	let close_statement := 	
		| CLOSE; COLON; fn = file_number; { (* TODO *) }

	let get_statement := 
		| GET; osn = optional_stream_number; gil = get_item_list; { (*TODO*) } 

	let optional_stream_number := 
		| (*empty*) 		{ None }
		| COLON; sn = stream_number; COMMA; { (*TODO*) }
	
	let stream_number :=	
		| e = expression; { e }
	
	let width_expression :=	
		| e = expression; { e }
	
	let fraction_width := 	
		| e = expression; { e }

	let exponent_width := 
		| e = expression; { e }

	let file_number :=
		| e = expression; { e }

	(* end of expression aliases *)

	let variable_reference == 
		| reference 

	let reference := 	
		| ID; r2 = reference_2; { r2 }

	let reference_2 := 
		| (*no ref*)				{  }
		| hd = component_selector; tl = reference_2; {  }

	let compile_time_expression := 
		| e = expression; { e }

	let boolean_expression := 
		| e = expression; { e }

	let expression_list :=	
		| (*empty*)	{ ExpressionList([]) }
		|	explist = expression_nonempty_list; { explist }

	let put_item := 	
		| e = expression; { e } 

	let get_item := 
		| vr = variable_reference; { (* TODO *) }
		| SKIP; vr = variable_reference; COLON; TIMES; { (* TODO *) }
		| vr = variable_reference; COLON; wexpr = width_expression;  { (* TODO *) }

	let get_item_list := 
		| hd = get_item; { (* TODO *) }
		| hd = get_item; COMMA; tl = get_item_list; { (* TODO *) }

	let expression_nonempty_list :=	
		| e = expression; 			{ ExpressionList([e]) }
		| e = expression; COMMA; enl = expression_nonempty_list; { ExpressionList(e::[enl]) }

	let component_selector := 	
		| LPAREN; explist = expression_list; RPAREN; { explist }  
		| DOT; i = ID; { i } 

	let expression := 
		| expconst = explicit_constant; { ExplicitConstant(expconst) } 	

	let explicit_constant := 	
		| i = EXPLICIT_UNSIGNED_INTEGER_CONST; { ExplicitIntegerConst(i) } 
		| r = EXPLICIT_UNSIGNED_REAL_CONST; { ExplicitRealConst(r) }
		| s = EXPLICIT_STRING_CONST; { ExplicitStringConst(s) }
		| t = TRUE; { True }
		| f = FALSE; { False }

	let string ==	
		| EXPLICIT_STRING_CONST

	let substring := 
		| r = reference; LPAREN; spe = substring_position_expression;  { (* TODO *) }  

	let substring_position_expression := 
		| sp = substring_position;  { sp }
		| hd = substring_position; DOTDOT; tl = substring_position; { (* TODO *) } 

	let substring_position := 
		| fst = expression; TIMES; MINUS; scnd = expression; { fst }  
	
	let infix_operator == 
		| PLUS 
		| MINUS 
		| TIMES 
		| DIV 
		| MOD 
		| POWER 
		| LESS_THAN 
		| GREATER_THAN 
		| ASSIGN 
		| LESS_THAN_OR_EQ 
		| GREATER_THAN_OR_EQ 
		| NOT_EQ 
		| AND 
		| OR 
	
	let capability == 
		| GET 
		| PUT 

	let capability_list :=	
		| cap = capability; { (* TODO *)}
		| hd = capability; COMMA; tl = capability_list; { (* TODO *) }

	let prefix_operator := 
		| PLUS 
		| MINUS 
		| NOT

%%