import java.util.*;

public class testClass
{

	
	public static List<String> Tokenizer(String cProgram)
	{

        List<String> tokens = new ArrayList<String>();

		cProgram=cProgram.trim();
		String separatedChars[] = cProgram.split(" ");

		for (String caracter : separatedChars)
				{

					tokens.add(caracter);

				}
		return tokens;
	}

	public static void main(String[] args)
	{
		String cProgram= "           int main () { return 0 ; }  ";
		List<String>  Tokens=Tokenizer(cProgram);
		for(String token : Tokens)
		{
			System.out.println(token);
		}
	}
	
}
class braceType
{
	private String openBrace;
	private String closeBrace;
	private String openCurlyBrace;
	private String closeCurlyBrace;
	

	public braceType(String brace)
	{
		//case para seleccionar el tipo
	}	
}

class keyWord
{
	private String cadena;

	public  keyWord(String cadena)
	{
		this.cadena= cadena;
	}
	public void printKeyWord()
	{
		System.out.println(this.cadena);
	}
}