type exp = 
	|Constant of int

type statement =
	|Return of exp

type fun_decl=
	|Function of string*statement

type program = 
	|Program of fun_decl
