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

class LexicalAnalyzer(private var source: String) extends Iterable[Lexeme] {
  //   ´?´ | ´!´ | string | identifier | ´=´ | literal | ´+´ | ´-´ | ´*´ | ´/´ | ´\´ | ´^´ | ´.´ | if | while
  var macroMap /*:Map[String, (Int, String)]*/ = Map(("", (-1, "")))
  val tokenMap = Map(
    '?' -> Token.QUESTION_MARK,
    '!' -> Token.BANG,
    '=' -> Token.EQUAL,
    '+' -> Token.PLUS,
    '-' -> Token.HYPHEN,
    '*' -> Token.ASTERISK,
    '^' -> Token.CIRCUMFLEX,
    '.' -> Token.PERIOD,
    '/' -> Token.SLASH,
    '\\' -> Token.BACKSLASH,
    '(' -> Token.WHILE,
    ')' -> Token.END_LOOP,
    '[' -> Token.BEGIN_IF,
    ']' -> Token.END_IF,
    '#' -> Token.BEGIN_CALL,
    ';' -> Token.END_CALL,
    '$' -> Token.BEGIN_DEF,
    '@' -> Token.END_DEF,
    //   '\n' -> Token.NEW_LINE
  )
  var input = Source.fromFile(source).getLines().mkString("\n").trim

  // checks if reached eof
  private def eof: Boolean = {
    input.isEmpty
  }

  // returns the current char (requires checking for eof before call)
  private def getChar(): Char = {
    input(0)
  }

  // advances the input one character (requires checking for eof before call)
  private def nextChar(commentCheck: Boolean = false) = {
    input = input.substring(1)
    if(commentCheck && !eof && getChar == '\'') throw new Exception("Token fragment")
  }

  // checks if input has a blank character ahead
  private def hasBlank(): Boolean = {
    BLANKS.contains(getChar())
  }

  // checks if input has a letter ahead
  private def hasLetter(): Boolean = {
    LETTERS.contains(getChar())
  }

  private def hasDigit(): Boolean = {
    DIGITS.contains(getChar())
  }

  // checks if input has a special character ahead
  private def hasSpecial(): Boolean = {
    SPECIALS.contains(getChar())
  }

  private def hasPunctuation(): Boolean = {
    PUNCTUATIONS.contains(getChar())
  }

  private def hasChar(ch: Char): Boolean = {
    input.startsWith(ch + "")
  }

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
        readBlanks
        val c = getChar()
        var str = ""
        if (hasChar('\'')) {
          nextChar()
          while (!eof && !hasChar('\n')) {
            str += getChar() + ""
            nextChar()
          }
          return new Lexeme(str, Token.COMMENT)
        }
        /*if (hasChar('#')) {
          str += getChar
          nextChar()
          if (input.length < 2 || input(1).isLetter) {
            throw new Exception("improper macro definition.")
          }
          while(!eof && getChar != ';') {
            str += getChar
            nextChar(true)
          }
          if (getChar != ';') throw new Exception("Invalid macro call. Must end with ';'")
          nextChar()
         return new Lexeme(str, Token.BEGIN_CALL)
        }
        else*/ if (hasLetter()) {
          str += getChar()
          nextChar()
          while (!eof && (hasLetter() || hasDigit())) {
            str = str + getChar()
            nextChar(true)
          }
          if(getChar() != ';') {
            nextChar()
          }
          return new Lexeme(str, Token.IDENTIFIER)
        } 
        else if (hasChar('"')) {
          nextChar(false)
          while (!eof && !hasChar('"')) {
            if(input(0) == '\''){
                throw new Exception("Bad escape sequence at '" + input + "'")
            }
            if(getChar() == '\\') {
              if(input.length < 2 || !"nt\\'\"".contains(input(1))) {
                throw new Exception("Bad escape sequence at '" + input + "'")
              }
              str += input.substring(0, 2)
              nextChar()
              nextChar()
            } else{
              str += getChar()
              nextChar()
            }
          }
        nextChar()
          return new Lexeme(str, Token.STRING)
        } else if (hasDigit()) {
          while (!hasBlank()) {
            str = str + getChar()
            nextChar(true)
          }
          if (str(0) == '0' && str.length > 1) {
            throw new Exception("Non-zero Integers cannot have leading zeros.")
          } 
          if(str.endsWith("%")) {
//            if(getChar() != ';') {
//              nextChar()
//            }
            return new Lexeme(str, Token.ARG)
          }
//          if (getChar() != ';') {
//            nextChar()
//          }
          return new Lexeme(str, Token.LITERAL)
        }  else if (input.startsWith("$$")) {
            nextChar()
            nextChar()
            if(!eof || input == "\n") throw new Exception("Input encountered after end of file!")
            return new Lexeme("$$", Token.EO_PRG)
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
  val BLANKS = " \t"
  val NEW_LINE = '\n'
  val LETTERS = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  val DIGITS = "0123456789"
  val PUNCTUATIONS = ".,;:?!"
  val SPECIALS = "<_@#$%^&()-+=/\\\n[]{}|"
  val ARG_SEPARATORS = "\n \t,"
  val SPACES = "\n\t "

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
