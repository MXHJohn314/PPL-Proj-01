/*
 * CS3210 - Principles of Programming Languages - Fall 2021
 * Instructor: Thyago Mota
 * Description: Prg 01 - MouseInterpreter
 * Student(s) Name(s):
 */

import scala.collection.mutable
import scala.io.StdIn

class MouseInterpreter(private var parseTreeNode: Tree, private var macroMap: mutable.Map[String, String]) {
  val stack: mutable.Stack[String] = mutable.Stack[String]()
  var variables: mutable.Map[String, String] = mutable.Map("" -> "")
  
  def run(): Unit = {
    val it = parseTreeNode.getBranches().iterator
    var done = false
    while (!done && it.hasNext) {
      val branch = it.next()
      val label = branch.getAttribute("label").get
      if (label.equals("$$"))
        done = true
      else
        run(branch)
    }
  }

  def run(stmt: Tree): Unit = {
    var branch = stmt.getBranches()(0)

    var label = branch.getAttribute("label").get

    if (MouseInterpreter.DEBUG) {
      println("[DEBUG] stack: " + stack)
      println("[DEBUG] branch.label: " + label)
    }
    if  (label.equals("string")) {
      val value = branch.getAttribute("value").get
      print(value)

      if (MouseInterpreter.DEBUG)
        println("[DEBUG] branch.value: " + value)
    }
    else if (label.equals("identifier")) {
      val value: String = branch.getAttribute("value").get
      if (MouseInterpreter.DEBUG)
        println("[DEBUG] branch.value: " + value)
      stack.push(value + "")
    }
    else if (label.equals("literal")) {
      val value = branch.getAttribute("value").get
      if (MouseInterpreter.DEBUG)
        println("[DEBUG] branch.value: " + value)
      stack.push(value)
    }
    else if (label.equals("?")) {
      val anInt = StdIn.readInt()
      stack.push(anInt + "")
    }
    else if (label.equals("!")) {
      val anInt = stack.pop
      print(anInt)
    }
    else if (label.equals("=")) {
      val b = stack.pop
      val a = stack.pop
      variables += a -> b
      if (MouseInterpreter.DEBUG) {
        print("[DEBUG] variable: ")
        for ((k, v) <- variables)
          print("[" + k + "]=" + v + " ")
        println
      }
    }
    else if (label.equals("+")) {
      val b: Int = stack.pop.toInt
      val a: Int = stack.pop.toInt
      stack.push((a + b) + "")
    }
    else if (label.equals("-")) {
      val b: Int = stack.pop.toInt
      val a: Int = stack.pop.toInt
      stack.push((a - b) + "")
    }
    else if (label.equals("*")) {
      val b: Int = stack.pop.toInt
      val a: Int = stack.pop.toInt
      stack.push((a * b) + "")
    }
    else if (label.equals("/")) {
      val b: Int = stack.pop.toInt
      val a: Int = stack.pop.toInt
      stack.push((a / b) + "")
    }
    else if (label.equals("\\")) {
      val b: Int = stack.pop.toInt
      val a: Int = stack.pop.toInt
      stack.push((a % b) + "")
    }
    else if (label.equals(".")) {
        stack.push(variables(stack.pop))
    }
    else if (label.equals("if")) {
      var done = false
      val it = branch.getBranches().iterator
      it.next // "consume" open bracket
      val condition = stack.pop.toInt > 0
      while (!done) {
        branch = it.next()
        label = branch.getAttribute("label").get
        if (label.equals("]"))
          done = true
        else if (condition)
          run(branch)
      }
    }
    else if (label.equals("while")) {
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
    }
    else if (label == "macro_call") {
      var str = branch.getAttribute("value").get
      str = str.substring(1, str.length - 1) // Remove the # and ; from the macro call
      val r = """[A-Za-z_][A-Za-z_0-9]*\s*\.""".r // Regex to find the variables passed to the macro
      val matches = r.findAllMatchIn(str) // Match any variables
      val q = mutable.Queue[String]() // Put them all in a queue
      for(m <- matches){q.enqueue(str.substring(m.start, m.end))}
      var moreDefs = "" // A string to declare variables with values in the macro body
      while(q.nonEmpty) {
        val curr = q.dequeue()
        val v = curr.replace(".", "").trim // Remove the dot operator
        val str1 = variables(v) // Look up and get the value of the variable
        if("[0-9]+".r.findFirstMatchIn(curr).isDefined) { 
          str = str.replaceFirst(curr, str1) // Replace the variable with its value in the macro call if it's in there
        } else {
            moreDefs += v + " " + str1 + " =\n" // If not, assign the value to a variable at the top of the macro definition
        }
      }
      val findings = """([^\s]+)""".r.findAllMatchIn(str) // Get all the args into an array with this regex
      val args = findings.toArray.map{ _.subgroups.flatMap(Option(_)).fold("")(_ ++ _) }
      var macroBody = moreDefs + "\n" +  macroMap(args(0)) // This is the string representing the sub-program to run. Initially it has only the definitions to assign variables with values from the function call and the function name 
      var i = 0 // Keep track of the indexes
      var variablesSeen = 1 // Count the spaces we skip due to dot operators
      while(i < args.length - 1) {
        if(args(i + 1) != ".") { // Any we encounter a variable
          val argNumber = variablesSeen + "%" // the argument number is the number of dots, followed by a `%` symbol
          val currentArg = args(i + 1) // Offset by one because the first index is the macro name
          macroBody = macroBody.replaceAll(argNumber, currentArg) // Replace the argument variable with the alphanumeric variable
          variablesSeen += 1 // Increase the number of variables we've seen
        }
        i += 1
      }
      var defs = ""
      macroMap.foreach(entry => defs += "$" + entry._1 + "\n" + entry._2.replaceAll("\\$\\$", "@") + "\n") // add the macro definitions to the top of the file so that macros can call other macros
      macroBody = defs + macroBody
      val syntaxAnalyzer = new SyntaxAnalyzer(macroBody, false)
      val parseTreeNode = syntaxAnalyzer.parse()
      val interpreter = new MouseInterpreter(parseTreeNode, syntaxAnalyzer.getMap)
      interpreter.run()
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
