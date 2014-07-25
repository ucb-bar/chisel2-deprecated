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

import scala.collection.mutable.{ArrayBuffer, Set, HashSet, HashMap, ListBuffer, Queue=>ScalaQueue, Stack}
import collection.mutable

object PartitionIslands {
  type IslandNodes = ArrayBuffer[Node]
  type MarkedNodes = HashMap[Node, Int]
  val debug: Boolean = false

  class Island(theIslandId: Int, theNodes: IslandNodes, theRoots: IslandNodes) {
    val nodes = theNodes
    val islandId = theIslandId
    val roots = theRoots
    def isEmpty: Boolean = nodes.isEmpty
  }
 
  /** Indicate whether a node is a root node or not.
   *
   */
  def isRoot(node: Node): Boolean = {
    (node.isIo && node.asInstanceOf[Bits].dir == OUTPUT && node.consumers.length == 0)
  }

  def isSource(node: Node): Boolean = {
    (node.isIo && node.asInstanceOf[Bits].dir == INPUT && node.inputs.length == 0)
  }

  /** Flood fill the graph, marking all non-root nodes reachable from here.
   * We visit inputs and consumers, stopping when we hit a register or memory.
   */
  def flood(sNode: Node, islandId: Int, marked: MarkedNodes, inputs: Set[Node]): Island = {
    val work = new Stack[Node]
    val islandNodes = new IslandNodes
    val islandRoots = new IslandNodes
    def processLinks(nodes: Seq[Node]) {
      // Process all unmarked nodes.
      for (n <- nodes) {
       if (debug) println(" considering " + n + " marked " + marked.contains(n) + " isInput " + inputs.contains(n))
       if (!inputs.contains(n)) {
         if (!marked.contains(n)) {
            work.push(n)
            marked += ((n, islandId))
         }
       }
      }
    }
    // We don't add the sNode to the island.
    // If it's a true input, we put it in island 0 (just to enable us to draw things with the dot backend).
    // It it's a register, it will be included in the island that provides its input.
    // Add all the unmarked consumers of this node to the work stack.
    processLinks(sNode.consumers)

    while (work.length > 0) {
      val node = work.pop()
      // We don't add the starting node to the island.
      // It's either a real input (which is added to island 0), or it's a register or memory
      // and will be added to the island that produces its input.
      if (debug) println("  adding " + node)
      islandNodes += node
      // Process un-marked inputs.
      processLinks(node.inputs)

      // If this is an "input" node (a register or memory), don't process its consumers.
      // They will be handled when we start flooding from this node.
      processLinks(node.consumers)

      // If we are a register or memory node, or we're a root node, add us to the root nodes for this island.
      node match {
        case d: Delay => islandRoots += d
        case _ => {
          if (isRoot(node)) {
            islandRoots += node
          }
        }
      }
    }
    new Island(islandId, islandNodes, islandRoots)
  }
  /* Create separately compilable islands of combinational logic.
   *
   */
  def createIslands(module: Module): ArrayBuffer[Island] = {
    val res = new ArrayBuffer[Island]
    val roots = new ArrayBuffer[Node]
    val inputs = new HashSet[Node]
    val delays = new HashSet[Node]
    val lits = new HashSet[Node]
    val barren = new HashSet[Node]
    val bogus = new HashSet[Node]
    val markedNodes = new MarkedNodes
    // Generate an array of input and output nodes for this module, and initialize the island id for each node.
    for (node <- module.omods) {
      if (node.isIo) {
        if (isRoot(node)) {
          roots += node
          // If this is a root node with no inputs, add it to the bogus list.
          bogus += node
        } else if (isSource(node)) {
          inputs += node
        } else if (node.inputs.length == 0 || node.consumers.length == 0) {
          bogus += node
        }
      } else {
        // If we're a delay node (Reg or Mem), our consumers become inputs
        node match {
          case d: Delay => {
            delays += d
          }
          case l: Literal => lits += l
          case r: MemRead => delays += r
          case _ => if (node.inputs.length == 0) barren += node
        }
      }
      if (node.inputs.length == 0) {
        if (!(inputs ++ delays ++ lits ++ barren ++ bogus).contains(node)) {
          println("createIslands: 0 inputs " + node)
        }
      }
    }
    for (n <- bogus) {
      println("createIslands: bogus " + n)
    }
    // Flood fill, generating islands
    var islandId = 1
    // Add all the real inputs to island 0 (just to give the dot backend a place to put them).
    val allLeaves = (inputs ++ delays ++ lits ++ barren ++ bogus)
    val allLeavesBuffer = allLeaves.to[mutable.ArrayBuffer]
    res += new Island(islandId, allLeavesBuffer, allLeavesBuffer)
    for (n <- allLeaves) {
      markedNodes += ((n, islandId))
    }
    for (inode <- (allLeaves)) {
      islandId += 1
      if (debug) println("creating island " + islandId)
      val islands = flood(inode, islandId, markedNodes, allLeaves)
      if (! islands.isEmpty) {
        res += islands
      }
    }
    // Output histogram info for the partitioning
    println("createIslands: " + res.length + " islands")
    for (island <- res) {
      println("  nodes: " + island.nodes.length)
    }
    res
  }
  
}