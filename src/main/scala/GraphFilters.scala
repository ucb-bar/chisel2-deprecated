/*
 Copyright (c) 2011, 2012, 2013 The Regents of the University of
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

import scala.collection.mutable.ArrayBuffer

/* This files contains Vertex and Edge filters to be used when walking
 a Chisel graph.
 */

/** Only traverse edges which connect to a ``MuxOp``.
*/
class OnlyMuxes extends EdgeFilter {

  override def apply( source: Node, target: Node ): Boolean = {
    target != null && target.isInstanceOf[MuxOp]
  }

}


/** Edges that flow between components.
  */
class InnerEdges extends EdgeFilter {

  override def apply(source: Node, target: Node): Boolean = {
    target != null && source.component == target.component
  }
}


/** Reachable Nodes in topological order excluding the roots
  */
class Reachable extends GraphVisitor {

  val nodes = new ArrayBuffer[Node]

  override def start( node: Node ): Unit = {
    nodes += node
  }

}


/** Sets the empty clause of ``MuxOp`` to *reg* while traversing a graph.
*/
class MuxDefault(val reg: Delay) extends GraphVisitor {

  override def start( node: Node ): Unit = {
    node match {
      case mux: MuxOp =>
        if( mux.inputs.length < 3 ) {
          mux.inputs.append(reg)
        }
      case _ => {}
    }
  }
}


class PrintNode extends GraphVisitor {

 private def isDottable (m: Node): Boolean = {
    if( m == null ) {
      false
    } else {
      m match {
        case x: Literal  => false;
        case _           => true;
      }
    }
  }

  override def start( node: Node ): Unit = {
    println(node + " inferWidth=" + node.inferWidth.toString)
  }

}
