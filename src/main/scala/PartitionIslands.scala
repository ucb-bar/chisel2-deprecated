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

import scala.collection.mutable.{ArrayBuffer, Set, HashSet, HashMap, ListBuffer, Map, Queue=>ScalaQueue, Stack}
import collection.mutable

object PartitionIslands {
  type IslandNodes = ArrayBuffer[Node]
  type MarkedNodes = HashMap[Node, Int]
  val debug: Boolean = true
  val moveDelays = false

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
       if (debug) println(" considering " + n.component.name + "/" + n + " marked " + marked.contains(n) + " isInput " + inputs.contains(n))
       if (!inputs.contains(n)) {
         if (!marked.contains(n)) {
            work.push(n)
            marked += ((n, islandId))
         }
       }
      }
    }

    work.push(sNode)
    while (work.length > 0) {
      val node = work.pop()
      if (debug) println("  adding " + node.component.name + "/" + node)
      islandNodes += node
      // Process un-marked inputs and outputs (consumers).
      processLinks(node.inputs ++ node.consumers)

      // If we are a register or memory node, or we're a root node, add us to the root nodes for this island.
      node match {
        case r: Reg => {
          islandRoots += node
        }
        case m: MemRead => {
          islandRoots += node
        }
        case _ => {
          if (isRoot(node)) {
            islandRoots += node
          }
        }
      }
      
      // Are we're moving delay (Reg or Mem) nodes to the island that provides their input?
      if (moveDelays) {
        // Iterate through our consumers, moving any delay nodes (register or memory read)
        // into this island - the supplier of their input.
        for (n <- node.consumers) n match {
          case r: Reg => {
            marked(n) = islandId
            islandNodes += n
          }
          case m: MemRead => {
            marked(n) = islandId
            islandNodes += n
          }
          case _ => ;
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
          if (node.inputs.length == 0) {
            bogus += node
          }
        } else if (isSource(node)) {
          inputs += node
        } else if (node.inputs.length == 0 || node.consumers.length == 0) {
          bogus += node
        }
      } else {
        // If we're a delay node (Reg or Mem), our consumers become inputs
        node match {
          case r: Reg => {
            delays += r
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
    var islandId = 1
    // Add all the real inputs to island 1 (just to give the dot backend a place to put them).
    val allNonDelayLeaves = (inputs ++ lits ++ barren ++ bogus)
    val allNonDelayLeavesBuffer = allNonDelayLeaves.to[mutable.ArrayBuffer]
    val allLeaves = allNonDelayLeaves ++ delays
    for (n <- allLeaves) {
      markedNodes += ((n, islandId))
      // We'll add delay nodes to the island that supplies their inputs,
      //  so don't make this an island if it's a delay (Reg or Memory) node.
      if (!moveDelays || !delays.contains(n)) {
        val an = new ArrayBuffer[Node]()
        an += n
        println("creating island leaf: " + islandId + " on " + n.component.name + "/" + n)
        res += new Island(islandId, an, an)
      }
      islandId += 1
    }
    // Flood fill, generating islands
    for (inode <- (allLeaves)) {
      // Flood from all the unmarked consumers of this node.
      // We don't add the inode to the island.
      // If it's a true input, it's already been put in its own island.
      // It it's a Reg or Mem, it will be included in the island that provides its input.
      for (sNode <- inode.consumers) {
        islandId += 1
        if (!markedNodes.contains(sNode)) {
          if (debug) {
            println("creating island " + islandId + " on " + sNode.component.name + "/" + sNode)
          }
          val island = flood(sNode, islandId, markedNodes, allLeaves)
          if (! island.isEmpty) {
            res += island
          }
        }
      }
    }
    // Output histogram info for the partitioning
    println("createIslands: " + res.length + " islands")
    val islandHistogram = Map[Int, Int]()
    for (island <- res) {
      val n = island.nodes.length
      islandHistogram(n) = islandHistogram.getOrElse(n, 0) + 1
    }
    for ((k,v) <- islandHistogram) {
      println("createIslands: islands " + v + ", nodes " + k)
    }
    res
  }
}
