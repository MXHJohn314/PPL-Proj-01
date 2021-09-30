/*
 * CS3210 - Principles of Programming Languages - Fall 2021
 * Instructor: Thyago Mota
 * Student(s):
 * Description: Prg 01 - LexicalAnalyzer (an iterable lexical analyzer)
 */
//is comment ' or ~
import LexicalAnalyzer.{ARG_SEPARATORS, BLANKS, DIGITS, LETTERS, NEW_LINE, PUNCTUATIONS, SPACES, SPECIALS}

import scala.collection.mutable
import scala.io.Source
import scala.util.matching.Regex
class LexicalAnalyzer(private var source: String) extends Iterable[Lexeme]{
//   ´?´ | ´!´ | string | identifier | ´=´ | literal | ´+´ | ´-´ | ´*´ | ´/´ | ´\´ | ´^´ | ´.´ | if | while
  var macroMap/*:Map[String, (Int, String)]*/ = Map(("", (-1, "")))
  val tokenMap = Map(
   '?' -> Token.QUESTION_MARK,
   '!' -> Token.BANG,
   '=' -> Token.EQUAL,
   '+' -> Token.PLUS,
   '-' -> Token.DASH,
   '*' -> Token.ASTERISK,
   '^' -> Token.CIRCUMFLEX,
   '.' -> Token.PERIOD,
   '/' -> Token.SLASH,
   '\\' -> Token.BACKSLASH,
   '(' -> Token.WHILE,
   ')' -> Token.END_LOOP,
   '[' -> Token.BEGIN_IF,
   ']' -> Token.END_IF, 
   '$' -> Token.BEGIN_MACRO, 
   '@' -> Token.END_MACRO, 
   '#' -> Token.MACRO_DEF, 
//   '\n' -> Token.NEW_LINE
  ) 
  var input = Source.fromFile(source).getLines().mkString("\n").trim
/*
  getMacroDefs
  getMacroCalls
  def recurseGetMacroDefs(inpt: String): (String, String) = {
    var in = inpt
    var  _macroName = ""
    var  _macroBody = ""
    while(!in.isEmpty && in(0).isLetterOrDigit ) {
      _macroName += in(0)
      in = in.substring(1)
    }
    if(macroMap.contains(_macroName)){
      throw new Exception("Macros can only be defined onece!")
    }
    while(!in.isEmpty && in(0) != '@') {
      if(in(0) == '$' && in.length > 1 && in(1) != '$'){
        in = in.substring(1)
        var retVal = recurseGetMacroDefs(in)
        _macroBody += retVal._1
        in = retVal._2
      }
      _macroBody += in(0)
      in = in.substring(1)
    }
    var argsSet: Set[Int] = Set()
    var thing =  "\\d%".r.findAllMatchIn(_macroBody).foreach(t => {
      argsSet += (t + "").substring(0, (t + "").length - 1).toInt
    })
    
    val sorted = argsSet.toArray.sortInPlace()
    var allOk = true
    var i = 0
    while(i < sorted.length){
      allOk = if(!allOk) false else sorted(i) == i + 1
      i += 1
    }
    if(!allOk){
      throw new Exception("parameters are wrong")
    }
    macroMap += (_macroName -> (sorted.length , ""))
    (_macroBody, in)
  }
  def getMacroDefs(): Unit = {
    var in = Source.fromFile(source).getLines().mkString("\n").trim
    while(!in.isEmpty){
      if(in(0) == '$' && in.length > 1 && in(1) != '$'){
        in = in.substring(1)
        var retVal = recurseGetMacroDefs(in)
        in = retVal._2
      }
      in = in.substring(1)
    }
  }
  def getMacroCalls(): Unit = {
    var in = Source.fromFile(source).getLines().mkString("\n").trim
    val macroCallRegex: Regex = "#([A-Z])\\s*(,\\s*[A-Z]\\s*)*;".r
    var matches = macroCallRegex.findAllMatchIn(in)
    var replacements: Array[String] = new Array[String](matches.length)
    var j = 0
    while(matches.hasNext) {
      val m = matches.next()
      val s = in.substring(m.start, m.end)
      val arr = s.substring(1, s.length -1).split(",")
      val f = arr(0)
      val params = arr.slice(1, arr.length - 1)
      if(!macroMap.contains(f) || macroMap(f)._1 != params.length){
        throw new Exception("Macro '" + f + "' takes " + macroMap(f)._1 + " arguments, but " + params.length + " were provided.")
      }
      var newString = macroMap(f)._2
      var i = 0
      while(i < params.length) {
        i += 1
        newString = newString.replaceAll(i + "%", params(i))
      }
      replacements(j) = newString
      j += 1
    }
    j = 0
    matches = macroCallRegex.findAllMatchIn(in)
    while(matches.hasNext){
      matches.next()
      j += 1
    }
  }
*/
  
  // checks if reached eof
  private def eof: Boolean = {input.isEmpty}
  // returns the current char (requires checking for eof before call)
  private def getChar(): Char = {input(0)}
  // advances the input one character (requires checking for eof before call)
  private def nextChar() = {input = input.substring(1)}
  // checks if input has a blank character ahead
  private def hasBlank(): Boolean = {BLANKS.contains(getChar())}
  // checks if input has a letter ahead
  private def hasLetter(): Boolean = {LETTERS.contains(getChar())}
  private def hasDigit(): Boolean = {DIGITS.contains(getChar())}
  // checks if input has a special character ahead
  private def hasSpecial(): Boolean = {SPECIALS.contains(getChar())}
  private def hasPunctuation(): Boolean = {PUNCTUATIONS.contains(getChar())}
  private def hasChar(ch: Char): Boolean = {input.startsWith(ch + "")}
  // reads the input until a non-blank character is found, updating the input
  def readBlanks: Unit = {
    while (!eof && (hasBlank() || hasChar('\n'))) {
      nextChar()
    }
  }
  // returns an iterator for the lexical analyzer
  override def iterator: Iterator[Lexeme] = {
    new Iterator[Lexeme] {
      // returns true/false depending whether there is a lexeme to be read from the input
      override def hasNext: Boolean = {
        readBlanks
        !eof
      }
      // returns the next lexeme (or end of line if there isn't any lexeme left to be read)
      // TODO: finish this part of the code
      override def next(): Lexeme = {
        if (!hasNext)
          return new Lexeme("eof", Token.EOF)
        val c = getChar()
        readBlanks
        var str = ""
        if (hasChar('\'')) {
          nextChar()
           while (!eof && !hasChar('\n')){
             str += getChar() + ""
             nextChar()
           }
          return new Lexeme(str, Token.COMMENT)
        }
        /*if(hasChar('#')){
          advance
          if(!hasLetter()){
            throw new Exception("improper macro definition.")
          }
          val macroName = getChar()
          nextChar()
          readBlanks
          var params = Array()
          while(hasChar(',')){
            advance
            if(eof || !hasLetter()){
              throw new Exception("Blank parameter")
            }
            params += getChar
            nextChar
          }
        }
        else
        */
          if (hasLetter()) {
          str += getChar()
          nextChar()
          while (!eof && (hasLetter() || hasDigit())) {
            str = str + getChar()
            nextChar()
          }
          nextChar()
          return new Lexeme(str, Token.IDENTIFIER)
        } /*else if (hasChar('$') && input.length > 1 && input(0).isLetter){
            nextChar()
            str = input.substring(0, input.indexOf("@"))
            input = input.substring(str.length - 1)
            var macroName = input(0)
            if(!macroName.isUpper) throw new Exception("macro names must be an upper case letter.")
            nextChar()
            if(eof)throw new Exception("No macro defitition")
            while(str.endsWith("\\@")){
                str += input.substring(0, input.indexOf("@") - 1)
                input = input.substring(str.length - 1)
            }
            if(input(0) != '@') throw new Exception("Macros must end with '@'")
            nextChar()
            return new Lexeme(str, Token.Le)
          }*/
          else if (hasChar('"')) {
          nextChar()
          while(!eof && !hasChar('"')) {
            str = str + getChar()
            nextChar()
          }
          nextChar()
          return new Lexeme(str, Token.STRING)
        } else if(hasDigit()) {
          while(!hasBlank()) {
            str = str + getChar()
            nextChar()
          }
          if(str(0) == '0' && str.length > 1) {
            throw new Exception("Non-zero Integers cannot have leading zeros.")
          }
          return new Lexeme(str, Token.LITERAL)
        } else if (hasChar('$')) {
          if(!input.startsWith("$$")) {
            throw new Exception("$ must be followed by $")
          } else {
            nextChar()
            nextChar()
            return new Lexeme("$$", Token.EO_PRG)
          }
        } else {
          if(tokenMap.contains(c)) {
            val token = tokenMap.get(c).get
            nextChar()
            return new Lexeme(c + "", token)
          }
      }
      // throw an exception if an unrecognizable symbol is found
      throw new Exception("Lexical Analyzer Error: unrecognizable symbol '" + getChar() + "'")
    }
  }
}

  private def advance = {
    nextChar
    readBlanks
  }
}

object LexicalAnalyzer {
  val BLANKS         = " \t"
  val NEW_LINE       = '\n'
  val LETTERS        = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  val DIGITS         = "0123456789"
  val PUNCTUATIONS   = ".,;:?!"
  val SPECIALS       = "<_@#$%^&()-+=/\\\n[]{}|"
  val ARG_SEPARATORS = "\n \t,"
  val SPACES         = "\n\t " 

  def main(args: Array[String]): Unit = {
    // checks if source file was passed through the command-line
    if (args.length != 1) {
      print("Missing source file!")
      System.exit(1)
    }
      val lex = new LexicalAnalyzer(args(0))
      val it = lex.iterator
      while (it.hasNext) {
        try
          println(it.next())
      }
    println("\n\n")
  } // end main method
}
