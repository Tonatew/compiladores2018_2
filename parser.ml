let rec parse_expression token_list=
	let open TokenTypes in
	let open Ast in
			match token_list with
			| Int value::rest -> Constant(value), rest
			| Negation::factor -> 
				let num, rest = parse_expression (factor) in
				UnOp (Negate,num), rest
			| BitwiseComp::factor -> 
				let num, rest = parse_expression (factor) in
				UnOp (Complement,num), rest
			| LogicNegation::factor -> 
				let num, rest = parse_expression (factor) in
				UnOp (Not,num), rest
			| _ -> failwith "Error, se esperaba entero"

let parse_statement token_list =
	let open TokenTypes in
	let open Ast in
		match token_list with
		| ReturnKeyword::rest -> 
			let exp, rest = parse_expression rest in
			begin
				match rest with
				| Semicolon::rest -> Return(exp), rest
				| _ -> failwith "Error, se esperaba <;>"
			end
		| _ -> failwith "Error se esperaba statement"

let parse_func_decl token_list=
	let open TokenTypes in
	let open Ast in
		match token_list with
		(* Int main () { *)
		| IntKeyword::Id id::OpenParen::CloseParen::OpenBrace::rest ->
			let statement, rest = parse_statement rest in
			begin
				match rest with
				| CloseBrace::rest -> Function(id, statement)
				| _ -> failwith "Error, se esperaba <}> "
			end
		| _ -> failwith "Error, se esperaba declaracion de funcion"


let parse token_list =
	let open Ast in
	Program (parse_func_decl token_list)