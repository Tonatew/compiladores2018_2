 type unuary_op =
 	|Negate
 	|Complement
 	|Not

 type exp = 
	|Constant of int
	|UnOp of unuary_op * exp

type statement =
	|Return of exp

type fun_decl=
	|Function of string * statement

type program = 
	|Program of fun_decl
