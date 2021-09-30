import scala.collection.mutable.ArrayBuffer

/*
 * CS3210 - Principles of Programming Languages - Falll 2021
 * Instructor: Thyago Mota
 * Description: Prg 01 - Tree (each tree node has a label, a key-value map, and branches)
 */

class TreeNode(private var label: String) {

  private val attributes = scala.collection.mutable.Map[String, String]()
  attributes("label") = label
  private val branches = new ArrayBuffer[TreeNode]()

  def setAttribute(key: String, value: String): Unit = {
    attributes(key) = value
  }

  def getAttribute(key: String) = {
    attributes.get(key)
  }

  def add(branch: TreeNode): Unit = {
    branches += branch
  }

  def getBranches() = branches

  private def print(current: TreeNode, tabs: String): String = {
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
object TreeNode {
  def main(args: Array[String]): Unit = {
    val tree = new TreeNode("A")
    val ab1 = new TreeNode("ab1")
    val ab2 = new TreeNode("ab2")
    val ab3 = new TreeNode("ab3")
    val abc1 = new TreeNode("abc1")
    val abc2 = new TreeNode("abc2")
    tree.add(ab1)
    tree.add(ab2)
    tree.add(ab3)
    ab1.add(abc1)
    ab1.add(abc2)
    print(tree)
  }
}

