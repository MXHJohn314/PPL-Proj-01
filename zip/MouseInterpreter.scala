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
              if (stmt.getAttribute("label").get.equals("^") && stack.pop().toInt == 0) {
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
        // Remove the # and ; from the macro call
        macroCall = macroCall.substring(1, macroCall.length - 1).trim
        // Identify which macro is being called
        val macroName: String = """[A-Za-z_][A-Za-z_0-9]*""".r.findFirstIn(macroCall).get
        // Remove macro name from call string
        macroCall = macroCall.substring(macroName.length).trim
        val parameters = """([^\s]+)""".r.findAllMatchIn(macroCall).toArray.map{ _.subgroups.flatMap(Option(_)).fold("")(_ ++ _) }
        var program = ""
        // Create a new sub-program starting with all the macro definitions so that macros can call other macros
        macroMap.foreach(entry => program += "$" + entry._1 + "\n" + entry._2.replaceAll("\\$\\$", "@") + "\n")
        // Get the function body from the map of macros
        var macroBody: String = macroMap(macroName)
        var i = 1
        parameters.foreach(param => {
          // Assign the variables passed as parameters so that they are instantiated at the top of the sub-program
          program += param + " " + variables(param) + " =" + "\n"
          // Replace each argument in the macro code (such as 1%, 2%...) with parameters from the macro call
          macroBody = macroBody.replaceAll(i + "%", param)
          i += 1
        })
        // Combine the program with the edited macro body
        program += macroBody
        // Run the sub-program
        val syntaxAnalyzer = new SyntaxAnalyzer(program, false)
        val parseTreeNode = syntaxAnalyzer.parse()
        val interpreter = new MouseInterpreter(parseTreeNode, syntaxAnalyzer.getMap)
        interpreter.run()
        
        // Get the return value from the macro, which is the last thing pushed on the stack before the sub-program exits
        stack.push(interpreter.stack.pop())
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
