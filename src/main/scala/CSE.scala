package Chisel

class CSENode(val node: Node) {
  val outputs = collection.mutable.ArrayBuffer[(Node,Int)]()
  val isInObject = node.isInObject
  override def hashCode: Int = node.hashCodeForCSE
  override def equals(x: Any): Boolean = x match {
    case x: CSENode => node.equalsForCSE(x.node) && !isInObject && (node.component eq x.node.component)
    case _ => false
  }
}

object CSE {
  def transform(mod: Module): Unit = {
    val csenodes = new collection.mutable.HashMap[Node, CSENode]
    val canonical = new collection.mutable.HashMap[CSENode, Node]
    mod.bfs { n =>
      if (n.canCSE)
        csenodes += n -> new CSENode(n)
    }
    var done = false
    while (!done) {
      done = true
      mod.bfs { n =>
        var i = 0
        while (i < n.inputs.length) {
          val in = n.inputs(i)
          if (in.canCSE)
            csenodes(in).outputs += n -> i
          i = i+1
        }
      }
      for ((n, csen) <- csenodes) {
        val e = canonical.getOrElseUpdate(csen, n)
        if (!(e eq n)) {
          var i = 0
          while (i < csen.outputs.length) {
            val o = csen.outputs(i)
            if (!(o._1.inputs(o._2) eq e)) {
              o._1.inputs(o._2) = e
              done = false
            }
            i = i+1
          }
        }
      }
    }
  }
}
