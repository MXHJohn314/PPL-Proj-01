/*
 * CS3210 - Principles of Programming Languages - Fall 2021
 * Instructor: Thyago Mota
 * Student(s):Malcolm Johnson, Harrison Jones
 * Description: Prg 01 - LexicalAnalyzer (an iterable lexical analyzer)
 */

import scala.collection.mutable
import scala.collection.mutable._
import scala.io.Source
class LexicalAnalyzer(private var source: String, private var isFile: Boolean) extends Iterable[Lexeme] {
  // Some helpful regex strings, we will match every Lexeme to a Regex
  val COMMENT = """^'.*""".r // comments always start with a '
  val ID_REG = "^[A-Za-z][A-Za-z0-9]*".r // Identifiers always start with alpha followed by 0 or more numbers
  val LITERAL = "^[0|1-9][0-9]*".r       
  val LITERAL_BAD = "^[0-9][A-Za-z_]|0[1-9]".r
  val STRING = """^\".*?\"""".r
  val END_PROGRAM = """^\$\$""".r
  val MACRO_DEF = """(?s)^\$\s*[A-Za-z][A-Za-z0-9]*.*?@""".r
  val MACRO_CALL = "^#.*?;".r
  
  var macroMap: Map[String, String] = Map() //  Make a map where keys are macro names and values are the code for that macro
  
  val REGEX_MAP = Map( // map all regular expressiions to a token which represents them to make a Lexeme
    MACRO_CALL ->          Option(Token.MACRO_CALL),
    ID_REG ->              Option(Token.IDENTIFIER), 
    STRING ->              Option(Token.STRING), 
    LITERAL ->             Option(Token.LITERAL),
    END_PROGRAM ->         Option(Token.EO_PRG), 
    COMMENT ->             Option(Token.COMMENT),
//    (END_PROGRAM+"\\S") -> Option("Non-space after $$"),
//    LITERAL_BAD ->         Option("Number with leading zero or letter is invalid"),
    """^@""".r->            Option(Token.END_DEF),
    """^\?""".r ->           Option(Token.STD_IN),
    """^!""".r->            Option(Token.STD_OUT),
    """^\=""".r ->           Option(Token.EQUAL),
    """^\+""".r ->           Option(Token.ADD),
    """^\-""".r ->           Option(Token.SUBTRACT),
    """^\*""".r ->           Option(Token.MULTIPLY),
    """^\^""".r ->           Option(Token.BREAK),
    """^\.""".r ->           Option(Token.PUSH_VAL),
    """^\/""".r ->           Option(Token.DIVIDE),
    """^\\""".r ->           Option(Token.MODULO),
    """^\[""".r ->           Option(Token.BEGIN_IF),
    """^\]""".r ->           Option(Token.END_IF),
    """^\(""".r ->           Option(Token.BEGIN_WHILE),
    """^\)""".r ->           Option(Token.END_WHILE),  
  )
  val lexemes: Queue[Lexeme]  = {
    var input = if(isFile) Source.fromFile(source).getLines().mkString("\n").trim else source
    val q = Queue[Lexeme]()                                             //  Make a queue for all lexemes
    while(input.startsWith("$") && !input.startsWith("$$")) {           //  Loop until there are no more macro definitions
      val _m = MACRO_DEF.findFirstMatchIn(input)                        //  Check to see if there is a macro at the beginning of the string
      if(_m.isDefined) {                                                //  If so...
        val _match = _m.get                                             //  Get the match object
        if(_match.start == 0) {                                         //  Make sure the match happened at the beginning
          input = input.substring(1).trim                    //  Remove the macro from the string
          val macroName = ID_REG.findFirstIn(input).get                 //  Get the macro name from the input
          input = input.substring(macroName.length)                     //  Remove the macro name from the input
          val macroBody = input.substring(0, input.indexOf("@"))        //  THe macro body is everything else to the @ symbol
          input = input.substring(1)                         //  Remove the macro name from the input
//          q.enqueue(new Lexeme(macroBody, Token.MACRO_DEF))             //  Make a new lexeme with the macro body
//          q.enqueue(new Lexeme("@", Token.END_DEF))              //  
          input = input.substring(macroBody.length).trim                //  Remove the macro body form the input
          macroMap += macroName -> (macroBody + "\n$$" )                //  Put an entry in the macroMap
        } else {                                                        //  If the input starts with a bad macro definition...  
          throw new Exception("bad macro definition")                   //  Throw an exception
        }
      }  
    }
    while (input.nonEmpty) {                                   // Get all Lexemes and consume input
      input = input.trim                                       // Remove all leading spaces at beginning of loop
      for ((regex, tokenOrString: Option[Any]) <- REGEX_MAP) { // loop through pairs of Key/Value from REGEX_MAP
        val subStr = regex.findFirstIn(input)                  // find a match for each regex if it exists
        if (subStr.isDefined) {                                // If a match was found...
          tokenOrString match {                                // Figure out what kind of match it was
            case Some(token: Token.Value) =>                   // If the value is a token
              input = input.substring(subStr.get.length).trim  // That means we found a match, so you can safely call substr.get to retrieve the match
              val lexeme = new Lexeme(subStr.get.replaceAll("\\\\n", "\n").replaceAll("\"", ""), token)
              token match {                                    // Figure out what kind of token it was
                case Token.MACRO_CALL => ()                    // If it's a comment, ignore it
                  q.enqueue(lexeme)
                case Token.COMMENT => ()                       // If it's a comment, ignore it
                case _ =>                                      // otherwise, put the string into a lexeme and put that into the queue
                  q.enqueue(lexeme)
              }
//            case Some(err: String) => throw new Error(err + " -->" + input)
            case None => print("no good stuff founnd")
          }
        }
      }
//      if (lex.isEmpty) throw new Exception("Unknown symbol '" + input.length)
    }
    q.enqueue(new Lexeme("eof", Token.EOF))
    q // Return the Queue
  }
  def getMap: mutable.Map[String, String] = macroMap
  override def iterator: Iterator[Lexeme] = { lexemes.iterator }
}

object LexicalAnalyzer {
  def main(args: Array[String]): Unit = {
    val l = new LexicalAnalyzer(args(0), true)
      for(x <- l){
        println(x)
      }
  }
}
