import scala.collection.mutable.ArrayBuffer

/*
 * CS3210 - Principles of Programming Languages - Falll 2021
 * Instructor: Thyago Mota
 * Description: Prg 01 - Tree (each tree node has a label, a key-value map, and branches)
 */

class ParesTreeNode(private var label: String) {

  private val attributes = scala.collection.mutable.Map[String, String]()
  attributes("label") = label
  private val branches = new ArrayBuffer[ParesTreeNode]()

  def setAttribute(key: String, value: String): Unit = {
    attributes(key) = value
  }

  def getAttribute(key: String) = {
    attributes.get(key)
  }

  def add(branch: ParesTreeNode): Unit = {
    branches += branch
  }

  def getBranches() = branches

  private def print(current: ParesTreeNode, tabs: String): String = {
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
object ParesTreeNode {
  def main(args: Array[String]): Unit = {
    val tree = new ParesTreeNode("A")
    val ab1 = new ParesTreeNode("ab1")
    val ab2 = new ParesTreeNode("ab2")
    val ab3 = new ParesTreeNode("ab3")
    val abc1 = new ParesTreeNode("abc1")
    val abc2 = new ParesTreeNode("abc2")
    tree.add(ab1)
    tree.add(ab2)
    tree.add(ab3)
    ab1.add(abc1)
    ab1.add(abc2)
    print(tree)
  }
}

