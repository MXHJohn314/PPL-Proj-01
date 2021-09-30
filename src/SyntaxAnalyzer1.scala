/*
 * CS3210 - Principles of Programming Languages - Fall 2021
 * Instructor: Thyago Mota
 * Student(s): Malcolm Johnson & Harrison Jones
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

import SyntaxAnalyzer1.{GRAMMAR_FILENAME, SLR_TABLE_FILENAME}
import Token.Value

import scala.collection.mutable.ArrayBuffer

class SyntaxAnalyzer1(private var source: String) {

  private val it = new LexicalAnalyzer(source).iterator
  private var current: Lexeme = null
  private val grammar = new Grammar(GRAMMAR_FILENAME)
  private val slrTable = new SLRTable(SLR_TABLE_FILENAME)

  // returns the current lexeme
  private def getLexeme(): Lexeme = {
    if (current == null) {
      current = it.next
    }
    //    println(current)
    current
  }

  // advances the input one lexeme
  private def nextLexeme() = {
    current = it.next
  }

  def parse(): TreeNode = {

    // create a stack of trees
    val trees: ArrayBuffer[TreeNode] = new ArrayBuffer[TreeNode]

    // initialize the parser's stack of (state, symbol) pairs
    val stack: ArrayBuffer[String] = new ArrayBuffer[String]
    stack.append("0")

    // main parser loop
    while (true) {

      if (SyntaxAnalyzer1.DEBUG)
        println("stack: " + stack.mkString(","))

      // get current lexeme
      val lexeme = getLexeme()

      // get current state
      var state = stack.last.strip().toInt
      if (SyntaxAnalyzer1.DEBUG)
        println("state: " + state)

      // get current token from lexeme
      val token = lexeme.getToken()

      // get action
      val action = slrTable.getAction(state, token.id)
      if (SyntaxAnalyzer1.DEBUG)
        println("action: " + action)

      if (action == "")
        throw new Exception("Syntax Error!")

      // implement the "shift" operation if the action's prefix is "s"
      if (action(0) == 's') {
        val nextState = action.substring(1)
        stack.append(nextState)

        // TODO: create a new tree with the lexeme (this probably isn't right)
        val newTree = new TreeNode(lexeme.getToken().toString)
        newTree.setAttribute("Value", lexeme.getLabel())
        trees.append(newTree)

        // acknowledge reading the input
        nextLexeme()
      }
      // implement the "reduce" operation if the action's prefix is "r"
      else if (action(0) == 'r') {
        // TODO: get the production to use
        val productionIndex = action.substring(1).toInt
        val productionLHS = grammar.getLHS(productionIndex)
        val productionRHS = grammar.getRHS(productionIndex)

        // TODO: update the parser's stack
        val rhsLen = productionRHS.length
        stack.dropRightInPlace(rhsLen)
        val goto = slrTable.getGoto(stack.last.toInt, productionLHS)
        stack.append(goto)

        // TODO: create a new tree with the "lhs" variable as its label
        val newTree = new TreeNode(productionLHS)

        // TODO: add "rhs.length" trees from the right-side of "trees" as children of "newTree"
        for (tree <- trees.slice(trees.length - rhsLen, trees.length)) {
          newTree.add(tree)
        }

        // TODO: drop "rhs.length" trees from the right-side of "trees"
        trees.dropRightInPlace(rhsLen)

        // TODO: append "newTree" to the list of "trees"
        trees.append(newTree)
      }
      // implement the "accept" operation
      else if (action.equals("acc")) {

        // create a new tree with the "lhs" of the first production ("start symbol")
        val newTree = new TreeNode(grammar.getLHS(0))

        // add all trees as children of "newTree"
        for (tree <- trees)
          newTree.add(tree)

        // return "newTree"
        return newTree
      }
      else
        throw new Exception("Syntax Analyzer Error!")
    }
    throw new Exception("Syntax Analyzer Error!")
  }
}

object SyntaxAnalyzer1 {

  val GRAMMAR_FILENAME   = "grammar.txt"
  val SLR_TABLE_FILENAME = "slr_table.csv"
  val DEBUG = true

  def main(args: Array[String]): Unit = {
    // check if source file was passed through the command-line
    if (args.length != 1) {
      print("Missing source file!")
      System.exit(1)
    }

    val syntaxAnalyzer = new SyntaxAnalyzer1(args(0))
    val parseTree = syntaxAnalyzer.parse()
    print(parseTree)
  }
}
