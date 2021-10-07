/*
 * CS3210 - Principles of Programming Languages - Fall 2021
 * Instructor: Thyago Mota
 * Description: Prg 01 - MouseInterpreter
 * Student(s) Name(s):
 */

import scala.collection.mutable
import scala.io.StdIn

class MouseInterpreter(private var parseTreeNode: TreeNode, private var macroMap: mutable.Map[String, String]) {
  val stack = mutable.Stack[Int]()
  val variable = new Array[Option[Int]](26 * 2)
  for (i <- 0 until variable.length)
    variable(i) = None

  private def nameToIndex(name: String) = {
    val c = name(0)
    if (c.isUpper)
      c - 'A'
    else
      c - 'a' + 26
  }

  private def indexToName(index: Int) = {
    if (index < 26)
      ('A' + index).toChar + ""
    else
      ('a' + index - 26).toChar + ""
  }

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

  def run(stmt: TreeNode): Unit = {
    var branch = stmt.getBranches()(0)

    var label = branch.getAttribute("label").get

    if (MouseInterpreter.DEBUG) {
      println("[DEBUG] stack: " + stack)
      println("[DEBUG] branch.label: " + label)
    }
    if      (label.equals("string")) {
      val value = branch.getAttribute("value").get
      print(value)

      if (MouseInterpreter.DEBUG)
        println("[DEBUG] branch.value: " + value)
    }
    else if (label.equals("identifier")) {
      val value = branch.getAttribute("value").get
      if (MouseInterpreter.DEBUG)
        println("[DEBUG] branch.value: " + value)
      stack.push(nameToIndex(value))
    }
    else if (label.equals("literal")) {
      val value = branch.getAttribute("value").get
      if (MouseInterpreter.DEBUG)
        println("[DEBUG] branch.value: " + value)
      stack.push(value.toInt)
    }
    else if (label.equals("?")) {
      val anInt = StdIn.readInt()
      stack.push(anInt)
    }
    else if (label.equals("!")) {
      val anInt = stack.pop
      print(anInt)
    }
    else if (label.equals("=")) {
      val b = stack.pop
      val a = stack.pop
      variable(a) = Some(b)
      if (MouseInterpreter.DEBUG) {
        print("[DEBUG] variable: ")
        for (i <- variable.indices)
          print("[" + i + "]=" + variable(i) + " ")
        println
      }
    }
    else if (label.equals("+")) {
      val b = stack.pop
      val a = stack.pop
      stack.push(a + b)
    }
    else if (label.equals("-")) {
      val b = stack.pop
      val a = stack.pop
      stack.push(a - b)
    }
    else if (label.equals("*")) {
      val b = stack.pop
      val a = stack.pop
      stack.push(a * b)
    }
    else if (label.equals("/")) {
      val b = stack.pop
      val a = stack.pop
      stack.push(a / b)
    }
    else if (label.equals("\\")) {
      val b = stack.pop
      val a = stack.pop
      stack.push(a % b)
    }
    else if (label.equals(".")) {
      val index = stack.pop
      variable(index) match {
        case Some(value) => stack.push(value)
        case None => throw new Exception("Runtime Error: variable " + indexToName(index) + " appears to be uninitialized!" )
      }
    }
    else if (label.equals("if")) {
      var done = false
      val it = branch.getBranches().iterator
      it.next // "consume" open bracket
      val condition = stack.pop > 0
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
            if (stmt.getAttribute("label").get.equals("^") && stack.pop() == 0) {
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
      str = str.substring(1, str.length - 1)
      val r = """[A-Za-z_][A-Za-z_0-9]*\s*\.""".r
      var matchesz = r.findAllMatchIn(str)
      var j = 0
      var q = mutable.Queue[String]()
      for(m <- matchesz){q.enqueue(str.substring(m.start, m.end))}
      var moreDefs = ""
      while(q.nonEmpty) {
        val curr = q.dequeue()
        val v = curr.replace(".", "").trim
        val str1 = variable((nameToIndex(v) + "").toInt).get + ""
        if("[0-9]+".r.findFirstMatchIn(curr).isDefined) {
          str = str.replaceFirst(curr, str1)
        } else {
            moreDefs += v + " " + str1 + " =\n"
        }
      }
      val findings = """([^\s]+)""".r.findAllMatchIn(str)
      val args = findings.toArray.map{ _.subgroups.flatMap(Option(_)).fold("")(_ ++ _) }
      var macroBody = moreDefs + "\n" +  macroMap(args(0)) 
      var i = 0
      var dots = 1
      while(i < args.length - 1) {
        if(args(i + 1) != ".") {
          val str1 = dots + "%"
          val arg = args(i + 1)
          macroBody = macroBody.replaceAll(str1, arg)
          dots += 1
        }
        i += 1
      }
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
