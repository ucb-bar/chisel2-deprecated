package Chisel

class CSENode(val node: Node) {
  val isInObject = node.isInObject
  override def hashCode: Int = node.hashCodeForCSE
  override def equals(x: Any): Boolean = x match {
    case x: CSENode => node.equalsForCSE(x.node) && !x.isInObject
    case _ => false
  }
}

object CSE {
  def transform(mod: Module): Unit = {
    Module.components foreach doCSE
  }

  private def doCSE(mod: Module): Unit = while (doCSEOnce(mod)) {}

  private def doCSEOnce(mod: Module): Boolean = {
    val cseNodes = new collection.mutable.LinkedHashMap[CSENode, Node]
    val removedNodes = new collection.mutable.LinkedHashMap[Node, Node]
    for (n <- mod.nodes) {
      if (n.canCSE) {
        val cseNode = new CSENode(n)
        val cseTo = cseNodes.get(cseNode)
        if (cseTo.isEmpty)
          cseNodes += cseNode -> n
        else
          removedNodes += n -> cseTo.get
      }
    }

    var removed = false
    for (n <- mod.nodes) {
      for (i <- 0 until n.inputs.length) {
        val in = n.inputs(i)
        if (in.component == mod) {
          val cseTo = removedNodes.get(in)
          if (!cseTo.isEmpty) {
            n.inputs(i) = cseTo.get
            removed = true
          }
        }
      }
    }
    removed
  }

  def inputsEqual(x: Node, y: Node): Boolean = {
    if (x.width != y.width || x.inputs.length != y.inputs.length)
      return false
    for (i <- 0 until x.inputs.length)
      if (!(x.inputs(i) == y.inputs(i)))
        return false
    true
  }
}
