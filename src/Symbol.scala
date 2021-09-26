/*
 * CS3210 - Principles of Programming Languages - Fall 2021
 * Instructor: Thyago Mota
 * Description: Prg 01 - Symbol (a String label)
 */

class Symbol(private var label: String) {

  def getLabel() = label

  def setLabel(label: String) = { this.label = label }

  override def toString: String = "(label:" + label + ")"
}
