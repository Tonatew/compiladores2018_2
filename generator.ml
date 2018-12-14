open Ast

let operator_code operator =
	match operator with
		| Negate -> "\tneg %%eax\r\n"
		| Complement -> "\tnot %%eax\r\n"
		| Not -> 	"\tcmpl $0, %%eax\r\n" ^
					"\tmovl $0, %%eax\r\n" ^
					"\tsete %%al\r\n"

let rec exp_code ast_exp =
	match ast_exp with
	| Constant(x) -> "\tmovl $" ^ string_of_int (x) ^ "0, %%eax\r\n"
	| UnOp(operator,exp) -> exp_code(exp) ^ operator_code operator


let statement_code ast_statement =
	match ast_statement with
		| Return(x) -> (exp_code x) ^ "\tret"

let function_code ast_fun_decl =
	match ast_fun_decl with
	| Function(id,statement) -> "\t.globl " ^ id ^ "\r\n" ^ 
								id ^ ":\r\n" ^ 
								(statement_code statement)

let gen ast = 
	match ast with
		| Program(x) -> function_code (x) 