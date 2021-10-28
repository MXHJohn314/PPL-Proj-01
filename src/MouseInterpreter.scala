/*
 * CS3210 - Principles of Programming Languages - Fall 2021
 * Instructor: Thyago Mota
 * Description: Prg 01 - MouseInterpreter
 * Student(s) Name(s): Malcolm Johnson, Harrison Jones
 */

import scala.collection.mutable
import scala.io.StdIn

class MouseInterpreter(private var parseTreeNode: Tree, private var macroMap: mutable.Map[String, String]) {
  val stack: mutable.Stack[String] = mutable.Stack[String]()
  var variables: mutable.Map[String, String] = mutable.Map[String, String]()

  def getInt: Int = {
    val v = stack.pop()
    val regex = "[A-za-z_]".r
    regex.findFirstIn(v) match {
      case Some(varName) => 
        variables(varName).toInt
      case _ => 
        v.toInt
    }
  }
  
  def run(): Unit = {
    for(branch <- parseTreeNode.getBranches()){
      if(branch.getAttribute("label").get == "$$") return
      run(branch)
    }
  }
  
  def run(stmt: Tree): Unit = {
    var branch = stmt.getBranches()(0)
    var label = branch.getAttribute("label").get
    label match {
      case "identifier" | "literal" => stack.push(branch.getAttribute("value").get)
      case "string" => print(branch.getAttribute("value").get)
      case "?" => stack.push(StdIn.readInt + "")
      case "!" => print(stack.pop)
      case "=" => val b = getInt; val a = stack.pop(); variables += (a + "") -> (b + "")
      case "+" => val b = getInt; val a = getInt; stack.push((a + b) + "") 
      case "-" => val b = getInt; val a = getInt; stack.push((a - b) + "")
      case "*" => val b = getInt; val a = getInt; stack.push((a * b) + "")
      case "/" => val b = getInt; val a = getInt; stack.push((a / b) + "")
      case "/" => val b = getInt; val a = getInt; stack.push((a / b) + "")
      case "\\" => val b = getInt; val a = getInt; stack.push((a % b) + "")
      case "." => stack.push(variables(stack.pop))
      case "while" =>
        var loopDone = false
        while (!loopDone) {
          var iterationDone = false
          val it = branch.getBranches().iterator
          it.next // "consume" open parenthesis
          while (!iterationDone) {
            val subBranch = it.next()
            label = subBranch.getAttribute("label").get
            if (label.equals(")")) {
              iterationDone = true
            }
            else {
              val stmt = subBranch.getBranches()(0)
              if (stmt.getAttribute("label").get == "^" && getInt == 0) {
                iterationDone = true
                loopDone = true
              }
              else
                run(subBranch)
            }
          }
        }
        
      case "if" =>
        var done = false
        val it = branch.getBranches().iterator
        it.next // "consume" open bracket
        val condition = getInt > 0
        while (!done) {
          branch = it.next()
          label = branch.getAttribute("label").get
          if (label.equals("]"))
            done = true
          else if (condition)
            run(branch)
        }
      case "macro_call" =>
        var macroCall = branch.getAttribute("value").get
        macroCall = macroCall.substring(1, macroCall.length - 1).trim
        val macroName: String = """[A-Za-z_][A-Za-z_0-9]*""".r.findFirstIn(macroCall).get
        macroCall = macroCall.substring(macroName.length).trim
        val parameters = """([^\s]+)""".r.findAllMatchIn(macroCall).toArray.map{ _.subgroups.flatMap(Option(_)).fold("")(_ ++ _) }
        var program = ""
        macroMap.foreach(entry => program += "$" + entry._1 + "\n" + entry._2.replaceAll("\\$\\$", "@") + "\n")
        var macroBody: String = macroMap(macroName)
        var i = 1
        parameters.foreach(param => {
          program += param + " " + variables(param) + " =" + "\n"
          macroBody = macroBody.replaceAll(i + "%", param)
          i += 1
        })
        program += macroBody
        val syntaxAnalyzer = new SyntaxAnalyzer(program, false)
        val parseTreeNode = syntaxAnalyzer.parse()
        val interpreter = new MouseInterpreter(parseTreeNode, syntaxAnalyzer.getMap)
        interpreter.run()
        stack.push(interpreter.stack.pop())
      case _ => throw new Exception("invalid token.") 
    }
  }
}

object MouseInterpreter {

  // set this to true to see detailed debug info
  val DEBUG = false

  def main(args: Array[String]): Unit = {

    // check if source file was passed through the command-line
    if (args.length != 1) {
      print("Missing source file!")
      System.exit(1)
    }

    val syntaxAnalyzer = new SyntaxAnalyzer(args(0), true)
    val parseTreeNode = syntaxAnalyzer.parse()
    val interpreter = new MouseInterpreter(parseTreeNode, syntaxAnalyzer.getMap)
    interpreter.run()
  }
}
