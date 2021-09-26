/*
 * CS3210 - Principles of Programming Languages - Fall 2021
 * Instructor: Thyago Mota
 * Student(s):
 * Description: Prg 01 - LexicalAnalyzer (an iterable lexical analyzer)
 */

import LexicalAnalyzer.{BLANKS, DIGITS, LETTERS, NEW_LINE, PUNCTUATIONS, SPECIALS}

import scala.collection.mutable
import scala.io.Source
class LexicalAnalyzer(private var source: String) extends Iterable[Lexeme]{
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
   '(' -> Token.BEGIN_LOOP,
   ')' -> Token.END_LOOP,
   '[' -> Token.BEGIN_IF,
   ']' -> Token.END_IF, 
//   '\n' -> Token.NEW_LINE
  ) 
  var input = Source.fromFile(source).getLines().mkString("\n").trim  

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
           while (!eof && !hasChar(NEW_LINE)){
             str += getChar() + ""
             nextChar()
           }
          return new Lexeme(str, Token.COMMENT)
        }
        if (hasLetter()) {
          str += getChar()
          nextChar()
          while (!eof && (hasLetter() || hasDigit())) {
            str = str + getChar()
            nextChar()
          }
          nextChar()
          return new Lexeme(str, Token.IDENTIFIER)
        } else if (hasChar('"')) {
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
}

object LexicalAnalyzer {
  val BLANKS       = " \t"
  val NEW_LINE     = '\n'
  val LETTERS      = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  val DIGITS       = "0123456789"
  val PUNCTUATIONS = ".,;:?!"
  val SPECIALS     = "<_@#$%^&()-+=/\\\n[]{}|"

  def main(args: Array[String]): Unit = {
    // checks if source file was passed through the command-line
    if (args.length != 1) {
      print("Missing source file!")
      System.exit(1)
    }
    var counter = 0
    var max = args(0).toInt
    while(counter <= max) {     // iterates over the lexical analyzer, printing the lexemes found
      println("example" + counter + ".mouse:\n")
      val lex = new LexicalAnalyzer("example" + counter + ".mouse")
      val it = lex.iterator
      while (it.hasNext) {
        try
          println(it.next())
      }
      counter += 1
    }
    println("\n\n")
  } // end main method
}
