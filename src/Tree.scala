import scala.collection.mutable.ArrayBuffer

/*
 * CS3210 - Principles of Programming Languages - Falll 2021
 * Instructor: Thyago Mota
 * Description: Prg 01 - Tree (each tree node has a label, a key-value map, and branches)
 */

class Tree(private var label: String) {

  private val attributes = scala.collection.mutable.Map[String, String]()
  attributes("label") = label
  private val branches = new ArrayBuffer[Tree]()

  def setAttribute(key: String, value: String): Unit = {
    attributes(key) = value
  }

  def getAttribute(key: String) = {
    attributes.get(key)
  }

  def add(branch: Tree): Unit = {
    branches += branch
  }

  def getBranches() = branches

  private def print(current: Tree, tabs: String): String = {
    var out = ""
    if (current == null)
      out
    else {
      out += tabs
      out += "{"
      for (key <- current.attributes.keys)
        out += key + ":" + current.attributes.get(key).get + ", "
      out = out.substring(0, out.length - 2)  + "}\n"
      for (branch <- current.branches)
        out += print(branch, tabs + "\t")
      out
    }
  }

  override def toString = print(this, "")

}

// example code
object Tree {
  def main(args: Array[String]): Unit = {
    val tree = new Tree("A")
    val ab1 = new Tree("ab1")
    val ab2 = new Tree("ab2")
    val ab3 = new Tree("ab3")
    val abc1 = new Tree("abc1")
    val abc2 = new Tree("abc2")
    tree.add(ab1)
    tree.add(ab2)
    tree.add(ab3)
    ab1.add(abc1)
    ab1.add(abc2)
    print(tree)
  }
}

