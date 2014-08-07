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
  type IslandNodes = HashSet[Node]
  type MarkedNodes = HashMap[Node, Int]
  type IslandCollection = HashMap[Int, Island]
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
  def flood(sNode: Node, islandId: Int, marked: MarkedNodes, inputs: Set[Node], prefixNodes: Seq[Node]): Island = {
    val work = new Stack[Node]
    val islandNodes = new IslandNodes
    islandNodes ++= prefixNodes
    val islandRoots = new IslandNodes
    def processLinks(nodes: Seq[Node]) {
      // Process all unmarked nodes.
      for (n <- nodes) {
       if (debug) println(" considering " + n.component.name + "/" + n + " marked " + marked.contains(n) + " isInput " + inputs.contains(n))
       if (!inputs.contains(n)) {
         if (!marked.contains(n)) {
            if (debug) println("  queuing " + n.component.name + "/" + n)
            work.push(n)
            marked += ((n, islandId))
         }
       }
      }
    }

    work.push(sNode)
    marked += ((sNode, islandId))
    while (work.length > 0) {
      val node = work.pop()
      if (debug) println("  adding " + node.component.name + "/" + node)
      islandNodes += node

      // Process un-marked inputs and outputs (consumers) for non-register nodes.
      if (debug) println("processLinks: inputs for " + node.component.name + "/" + node)
      processLinks(node.inputs)
      if (debug) println("processLinks: consumers for " + node.component.name + "/" + node)
      processLinks(node.consumers)
      
    }
    new Island(islandId, islandNodes, islandRoots)
  }

  /* Create separately compilable islands of combinational logic.
   *
   */
  def createIslands(module: Module): Array[Island] = {
    val res = new IslandCollection
    val roots = new ArrayBuffer[Node]
    val inputs = new HashSet[Node]
    val delays = new HashSet[Node]
    val lits = new HashSet[Node]
    val barren = new HashSet[Node]
    val bogus = new HashSet[Node]
    val markedNodes = new MarkedNodes
    var islandId = 1

    // Mark all the Bits nodes directly reachable from an initial node.
    def markBitsNodes(iNode: Node, marked: MarkedNodes, islandId: Int) {
      // Follow consumers of the initial (input) node,
      // until we find a non-Bits node from which we will later flood.
      // We need to mark these nodes now so we don't flood into them during the flooding stage.
      // The islandId isn't critical here.
      if (iNode.isInstanceOf[Bits]) {
        marked += ((iNode, islandId))
        for (p <- iNode.consumers) {
          markBitsNodes(p, marked, islandId)
        }
      }
    }

    def islandsFromNonBitsNode(iNode: Node, res: IslandCollection, marked: MarkedNodes, inputs: Set[Node], work: Stack[Node]): Int = {
      // Follow consumers of the initial (input) node, until we find a non-Bits node from which to flood.
      // Keep track of the chain to that node and insert the nodes on that path into the same island
      // we create.
      // This will create multiple islands, one for each non-Bits node origin.
      // we return the number of islands we created so client code can keep track
      // of the next available free islandId.
      var nIslands = 0
      if (debug) {
        println("islandsFromNonBitsNode: pushing " + iNode.component.name + "/" + iNode)
      }
      work.push(iNode)
      if (iNode.isInstanceOf[Bits]) {
        for (p <- iNode.consumers) {
          nIslands += islandsFromNonBitsNode(p, res, marked, inputs, work)
        }
      } else {
        if (!marked.contains(iNode)) {
          if (debug) {
            println("creating island " + islandId + " on " + iNode.component.name + "/" + iNode)
          }
          val island = flood(iNode, islandId, marked, inputs, work)
          if (! island.isEmpty) {
            res += ((islandId, island))
            nIslands += 1
            islandId += 1
          }
        } else {
          // This node is marked, so we don't want to flood from it,
          //  but we do want to add ourselves to its island.
          val floodedIslandId = marked(iNode)
          val island = res(floodedIslandId)
          island.nodes ++= work
        }
        work.pop()
        if (debug) {
          println("islandsFromNonBitsNode: popped " + iNode.component.name + "/" + iNode)
        }
      }
      nIslands
    }
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
          case m: Mem[_] => delays += m
          /*
          case r: MemRead => delays += r
          case w: MemWrite => delays += w
          */
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
    // Add all the real inputs to island 1 (just to give the dot backend a place to put them).
    val allNonDelayLeaves = (inputs ++ lits ++ barren ++ bogus)
    val allNonDelayLeavesBuffer = allNonDelayLeaves.to[mutable.ArrayBuffer]
    val allLeaves = allNonDelayLeaves ++ delays
    // First we mark all the "input" nodes.
    // This includes all the "true" input nodes, plus any Reg or Mem nodes,
    //  plus all Bits nodes directly reachable from an input node.
    for (inode <- allLeaves) {
      markedNodes += ((inode, islandId))
      // Put all input, delay, bogus, and barren nodes in their own island.
      val an = new IslandNodes
      an += inode
      println("creating island leaf: " + islandId + " on " + inode.component.name + "/" + inode)
      res += ((islandId, new Island(islandId, an, an)))
      // Now mark all the directly-reachable Bits nodes.
      for ( pNode <-inode.consumers) {
        markBitsNodes(pNode, markedNodes, islandId)
      }
      islandId += 1
    }
    // Flood fill, generating islands
    // We build a work stack until we hit a node that is not an instance of Bits,
    // then we flood from the non-Bits node including all that node's parents in the island.
    for (inode <- allLeaves) {
      // Flood from all the unmarked consumers of this node.
      // We don't add the inode to the island.
      // It's already been put in its own island.
      for ( pNode <-inode.consumers) {
        val work = new Stack[Node]
        islandsFromNonBitsNode(pNode, res, markedNodes, allLeaves, work)
      }
    }
    // Output histogram info for the partitioning
    println("createIslands: " + res.size + " islands")
    val islandHistogram = Map[Int, Int]()
    for (island <- res.values) {
      val n = island.nodes.size
      islandHistogram(n) = islandHistogram.getOrElse(n, 0) + 1
    }
    for ((k,v) <- islandHistogram) {
      println("createIslands: islands " + v + ", nodes " + k)
    }
    res.values.toArray
  }
}
