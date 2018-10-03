

pub fn parsear(tokens: Vec<&str>)-> Vec<&str>{
let mut ast=vec!();
if tokens.len()==0{
    println!("error, lexer vacio");
    unreachable!();

}else{
  ast=funcion(tokens);
}
ast
}



pub fn funcion(tokens: Vec<&str>)-> Vec<&str>{
let mut ast= vec!();
if tokens[0] != "IntKeyword"{
println!("eror, falta IntKeyword");
unreachable!();
}else if tokens[1] != "identifier" || tokens[2]!="main"{
println!("error en la declaración de funcion");
unreachable!();
}else if tokens[3] != "OpenParen"|| tokens[4]!="CloseParen"{
println!("faltan parentesis al main");
unreachable!();
}else if tokens[5] !="OpenBrace"{
println!(" no se abrio llave para el statement");
unreachable!();
}
else{
      let temp = &tokens;
     ast= vec!["programa","<","int","main","(",")","{",];
    for i in statement(temp.to_vec()){
        ast.push(i);
    }
}
if tokens[9] != "SemiColon" || tokens[10] != "CloseBrace"{
println!("error en cerrar funcion");
}else{
  ast.push(";");
  ast.push("}");
  ast.push(">");
}
ast
}


pub fn statement(tokens: Vec<&str>)->Vec<&str>{
  let mut temp = vec!();
  if tokens[6] != "ReturnKeyword"{
  println!("error, falta return");
  unreachable!();
  }else{
  let tempo= &tokens;
  temp= vec!["<","return","<",exp(tempo.to_vec()),">",">"];
  }
temp
}


pub fn exp(tokens: Vec<&str>)-> &str{
let mut temp= "temporal";
if tokens[7]!= "int"{
      println!("error en valor del return");
      unreachable!();
}else{
    temp=tokens[8];
}
&temp
}
