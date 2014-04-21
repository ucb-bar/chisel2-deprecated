/*
 Copyright (c) 2011, 2012, 2013, 2014 The Regents of the University of
 California (Regents). All Rights Reserved.  Redistribution and use in
 source and binary forms, with or without modification, are permitted
 provided that the following conditions are met:

    * Redistributions of source code must retain the above
      copyright notice, this list of conditions and the following
      two paragraphs of disclaimer.
    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      two paragraphs of disclaimer in the documentation and/or other materials
      provided with the distribution.
    * Neither the name of the Regents nor the names of its contributors
      may be used to endorse or promote products derived from this
      software without specific prior written permission.

 IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
 SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS,
 ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
 REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

 REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT
 LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 A PARTICULAR PURPOSE. THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF
 ANY, PROVIDED HEREUNDER IS PROVIDED "AS IS". REGENTS HAS NO OBLIGATION
 TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
 MODIFICATIONS.
*/

package Chisel

class CSENode(val node: Node) {
  override def hashCode: Int = node.hashCodeForCSE
  override def equals(x: Any): Boolean = x match {
    case x: CSENode => node.equalsForCSE(x.node) && !x.node.isInObject
    case _ => false
  }
}

object CSE {
  def transform(mod: Module): Unit = {
    Driver.components foreach doCSE
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
