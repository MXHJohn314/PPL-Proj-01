/*
 * CS3210 - Principles of Programming Languages - Fall 2021
 * Instructor: Thyago Mota
 * Student(s):
 * Description: Prg 01 - SyntaxAnalyzer (an iterable syntax analyzer)
 */

/*
mouse       = { statement } ´$$´
statement   = ´?´ | ´!´ | string | identifier | ´=´ | literal | ´+´ | ´-´ | ´*´ | ´/´ | ´\´ | ´^´ | ´.´ | if | while
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

class SyntaxAnalyzer(private var source: String) {
  private var start = true;
  private val it = new LexicalAnalyzer(source).iterator
  private var current: Lexeme = null
   private val assigners = List(Token.IDENTIFIER, Token.LITERAL, Token.STRING)
  private val genericTokens = List(
     Token.QUESTION_MARK, Token.BANG, Token.EQUAL, Token.PLUS, Token.DASH, Token.ASTERISK, Token.CIRCUMFLEX, Token.PERIOD, Token.SLASH, Token.BACKSLASH, Token.EO_PRG
  )
  private val specialTokens: Map[Token.Value, (String, String)] = Map (
    Token.WHILE -> ("while", ")"),
    Token.BEGIN_IF -> ("if", "]"),
  )
  // returns the current lexeme
  private def getLexeme(): Lexeme = {
    if (current == null) {
      current = it.next
    }
    //    println(current)
    current
  }

  // advances the input one lexeme
  private def nextLexeme(): Unit = {
    current = it.next
  }

  def parse(labelAndToken:(String, String) = ("mouse", "eof")): TreeNode = {
    var branch = new TreeNode(labelAndToken._1) 
    if(labelAndToken._1!= "mouse"){
       branch.add(new TreeNode(getLexeme().getLabel()))
      nextLexeme()
    }
    while(getLexeme().getLabel() != labelAndToken._2) {
      branch.add(parseSyntaxRule())
    }
    branch
  }

  // TODO: finish the recursive descent parser
  // parses the program, returning its corresponding parse tree
  private def parseSyntaxRule(): TreeNode  = {
    var node: TreeNode = null
    var token = getLexeme().getToken()
    var label = getLexeme().getLabel()
    while (token == Token.COMMENT) {
      nextLexeme()
      token = getLexeme().getToken()
      label = getLexeme().getLabel()
    }
    if (genericTokens.contains(token)){
      node = new TreeNode(label)
    } else if (assigners.contains(token)){
      node = new TreeNode(token.toString.toLowerCase)
      node.setAttribute("value", label)
    }
    else if(specialTokens.contains(token)) {
      node = parse(specialTokens(token))
      node.add(new TreeNode(specialTokens(token)._2))
//      node.add(conditional)
    }
    val stmt = new TreeNode("statement")
    if(label == "$$"){
      nextLexeme()
      return node
    }
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

    val syntaxAnalyzer = new SyntaxAnalyzer(args(0))
    val parseTree = syntaxAnalyzer.parse()
    print(parseTree)
  }
}
