import scala.collection.mutable

/*
 * CS3210 - Principles of Programming Languages - Fall 2021
 * Instructor: Thyago Mota
 * Student(s) Name(s): Malcolm Johnson, Harrison Jones
 * Description: Prg 01 - SyntaxAnalyzer (an iterable syntax analyzer)
 */
/*
mouse       = { definition } { statement } ´$$´
definition  = `$` defName { statement } ´@´
call        = `#`  defName { identifier } ´;´
defName     = letter { letter | literal }
statement   = ´?´ | ´!´ | call | string | identifier | ´=´ | literal | ´+´ | ´-´ | ´*´ | ´/´ | ´\´ | ´^´ | ´.´ | if | while
string      = ´"´ { character } ´"´
identifier  = letter
literal     = ´0´ | nonzero { digit }
nonzero     = ´1´ | ´2´ | ´3´ | ´4´ | ´5´ | ´6´ | ´7´ | ´8´ | ´9´
digit       = ´0´ | ´1´ | ´2´ | ´3´ | ´4´ | ´5´ | ´6´ | ´7´ | ´8´ | ´9´
if          = ´[´ { statement } ´]´
while       = ´(´ { statement } ´)´
letter      = ´a´ | ´b´ | ´c´ | ´d´ | ´e´ | ´f´ | ´g´ | ´h´ | ´i´ | ´j´ | ´k´ | ´l´ | ´m´ | ´n´ | ´o´ | ´p´ | ´q´ | ´r´ | ´s´ | ´t´ | ´u´ | ´v´ | ´x´ | ´y´ | ´w´ | ´z´ | ´A´ | ´B´ | ´C´ | ´D´ | ´E´ | ´F´ | ´G´ | ´H´ | ´I´ | ´J´ | ´K´ | ´L´ | ´M´ | ´N´ | ´O´ | ´P´ | ´Q´ | ´R´ | ´S´ | ´T´ | ´U´ | ´V´ | ´X´ | ´Y´ | ´W´ | ´Z´
punctuation = ´.´ | ´,´ | ´;´ | ´:´ | ´?´ | ´!´
special     = ´<´ | ´_´ | ´@´ | ´#´ | ´$´ | ´%´ | ´^´ | ´&´ | ´(´ | ´)´ | ´-´ | ´+´ | ´=´ | ´'´ | ´/´ | ´\´ | ´[´ | ´]´ | ´{´ | ´}´ | ´|´
blank       = ´ ´
character   = letter | digit | punctuation | special | blank
 */

class SyntaxAnalyzer(private var source: String, private var isFile: Boolean) {
  private val lexicalAnalyzer = new LexicalAnalyzer(source, isFile)
  private val it = lexicalAnalyzer.iterator
  private var current: Lexeme = null
  
   private val assigners = List(Token.IDENTIFIER, Token.LITERAL, Token.STRING, Token.MACRO_CALL)
  private val genericTokens = List(
     Token.PARAMETER, Token.STD_IN, Token.STD_OUT, Token.EQUAL, Token.ADD, Token.SUBTRACT, Token.MULTIPLY, Token.BREAK, Token.PUSH_VAL, Token.DIVIDE, Token.MODULO, Token.EO_PRG
  )
  private val specialTokens: Map[Token.Value, (String, String)] = Map (
    Token.BEGIN_WHILE -> ("while", ")"),
    Token.BEGIN_IF -> ("if", "]"),
//    Token.MACRO_CALL -> ("call", ";"),
//    Token.MACRO_DEF -> ("def", "@"),
  )
  // returns the current lexeme
  private def getLexeme: Lexeme = {
    if (current == null) {
      current = it.next
    }
    current
  }

  // advances the input one lexeme
  private def nextLexeme(): Unit = {
    current = it.next
  }

  def parse(labelAndToken:(String, String) = ("mouse", "eof")): Tree = {
    val branch = new Tree(labelAndToken._1) 
    if(labelAndToken._1!= "mouse"){
      val subTree = new Tree(getLexeme.getLabel())
      branch.add(subTree)
      if(false/*"call".contains(labelAndToken._1)*/){
        nextLexeme()
        subTree.setAttribute("value", getLexeme.getLabel())
      }
      nextLexeme()
    }
    val l = getLexeme.getLabel()
    while(getLexeme.getLabel() != labelAndToken._2) {
      branch.add(parseSyntaxRule())
    }
    branch
  }
  def getMap: mutable.Map[String, String] = lexicalAnalyzer.getMap
  // TODO: finish the recursive descent parser
  // parses the program, returning its corresponding parse tree
  private def parseSyntaxRule(): Tree  = {
    var node: Tree = null
    var token = getLexeme.getToken()
    var label = getLexeme.getLabel()
    while (token == Token.COMMENT) {
      nextLexeme()
      token = getLexeme.getToken()
      label = getLexeme.getLabel()
    }
    if(genericTokens.contains(token)) {
      node = new Tree(label)
    } else if (assigners.contains(token)) {
      node = new Tree(token.toString.toLowerCase)
      node.setAttribute("value", label)
    }
    else if(specialTokens.contains(token)) {
      node = parse(specialTokens(token))
      node.add(new Tree(specialTokens(token)._2))
    }
    val stmt = new Tree("statement")
    if(label == "$$") {
      nextLexeme()
      return node
    }
    if(node == null) throw new Exception("No matching token found for `"
      + label + "`. Are you missing one of `(`, `)`, `[`, `]`?")
    stmt.add(node)
    nextLexeme()
    stmt
  }
}

object SyntaxAnalyzer {
  def main(args: Array[String]): Unit = {

    // check if source file was passed through the command-line
    if (args.length != 1) {
      print("Missing source file!")
      System.exit(1)
    }

    val syntaxAnalyzer = new SyntaxAnalyzer(args(0), true)
    val parseTree = syntaxAnalyzer.parse()
    print(parseTree)
  }
}
