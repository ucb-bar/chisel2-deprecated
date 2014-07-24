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

import scala.collection.mutable.{ArrayBuffer, HashSet, ListBuffer, Queue=>ScalaQueue, Stack}

object PartitionIslands {
  class NodeChain(theNodes: List[Node], theChainId: Int) {
    val nodes = theNodes
    val chainId = theChainId
  }

  /** Indicate whether a node is a root node or not.
   *  
   */
  def isRoot(node: Node): Boolean = {
    (node.asInstanceOf[Bits].dir == OUTPUT && node.consumers.length == 0)
  }

  def isSource(node: Node): Boolean = {
    (node.asInstanceOf[Bits].dir == INPUT && node.inputs.length == 0)
  }

  /** Flood fill the graph, marking all non-root nodes reachable from here.
   *
   */
  def flood(node: Node, islandId: Int, chain: ListBuffer[Node], inputs: ArrayBuffer[Node]) {
    // Mark this node with its islandId and add it to the island chain.
    node.islandId = islandId
    chain += node
    for (n <- (node.consumers ++ node.inputs)){
      // If we haven't visited this node before and it's not an input node
      //  (which we'll handle as part of its own chain), flood from it.
      if (n.islandId == 0 && ! inputs.contains(n)) {
        flood(n, islandId, chain, inputs)
      }
    }
  }
  /* Create separately compilable islands of combinational logic.
   *
   */
  def createIslands(module: Module): ArrayBuffer[NodeChain] = {
    val res = new ArrayBuffer[NodeChain]
    val roots = new ArrayBuffer[Node]
    val inputs = new ArrayBuffer[Node]
    // Generate an array of input and output nodes for this module, and initialize the island id for each node.
    for (node <- module.mods) {
      node.islandId = 0
      if (node.isIo) {
        if (isRoot(node)) {
          roots += node
        } else if (isSource(node)) {
          inputs += node
        }
      } else {
        if (node.isReg) {
          inputs += node
        }
      }
    }

    var chain: ListBuffer[Node] = new ListBuffer()
    // Flood fill, generating islands
    var islandId = 0
    for (inode <- inputs) {
      for (cnode <- inode.consumers) {
        islandId += 1
        // Do we need to save the current chain?
        if (! chain.isEmpty) {
          res += new NodeChain(chain.toList, islandId - 1)
          chain = new ListBuffer()
        }
        // Have we dealt with the input node?
        if (inode.islandId == 0) {
          inode.islandId = islandId
          chain += inode
        }
        // Have we encountered this consumer node before?
        if (cnode.islandId == 0) {
          flood(cnode, islandId, chain, inputs)
        }
      }
    }
    // Do we need to save the current chain?
    if (! chain.isEmpty) {
      res += new NodeChain(chain.toList, islandId)
      chain = new ListBuffer()
    }
    res
  }
}