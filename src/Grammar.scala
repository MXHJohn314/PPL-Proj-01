/*
 * CS3210 - Principles of Programming Languages - Fall 2021
 * Instructor: Thyago Mota
 * Description: Activity 09 - Grammar
 */

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class Grammar(private var source: String) {

  private var productions = new ArrayBuffer[String]
  for (line <- Source.fromFile(source).getLines)
    productions += line

  def getLHS(index: Integer): String = {
    val production = productions(index)
    production.split("->")(0).strip()
  }

  def getRHS(index: Integer) = {
    val production = productions(index)
    production.split("->")(1).strip().split(" ")
  }

  override def toString: String = {
    var out = ""
    for (i <- 0 until productions.length)
      out += i + ". " + getLHS(i) + " -> " + getRHS(i).mkString(" ") + "\n"
    out
  }
}
