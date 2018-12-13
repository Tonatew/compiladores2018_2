let exp_code ast_constant =
	let open Ast in
	match ast_constant with
	| Constant(x) -> string_of_int (x)

let statement_code ast_statement =
	let open Ast in
	match ast_statement with
	| Return(x) -> "\tmovl\t $" ^ (exp_code x) ^ ", %eax\r\n\tret"

let function_code ast_fun_decl =
	let open Ast in
	match ast_fun_decl with
	| Function(id,statement) -> "\t.globl " ^ id ^ "\r\n" ^ 
								id ^ ":\r\n" ^ 
								(statement_code statement)

let gen ast = 
	let open Ast in
	match ast with
	| Program(x) -> function_code (x) 