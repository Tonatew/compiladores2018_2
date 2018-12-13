
(*Funcion de mapeo*)
let rec map f l = 
  match l with 
      [] -> []
    |h::t -> f h :: map f t


let string_list_to_string l = String.trim (String.concat "" l)


(*obtiene las lineas de un archivo y devuelve un string list*)
(* channel -> string list *)
let file_to_string_list filename =
  let lines = ref [] in
  let chan = open_in filename in
    try
      while true; do
        lines := input_line chan :: !lines
      done; !lines
    with End_of_file ->
        close_in chan; 
        List.rev !lines

(*String list -> string*)
let read_file filename = 
  string_list_to_string (file_to_string_list filename)

let code_to_file output_name code=
  let chan = open_out output_name in
    output_string chan code;
    close_out chan

let filename = Array.get Sys.argv 1
let output_file = Array.get Sys.argv 2

let _ = 
  filename |> read_file |> Lexer.lex |> Parser.parse |> Generator.gen |> code_to_file output_file
