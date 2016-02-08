/*
 Copyright (c) 2011 - 2016 The Regents of the University of
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

import scala.collection.mutable.{ArrayBuffer, Set, HashSet, Map, HashMap, Stack}

object PartitionIslands {
  type IslandNodes = scala.collection.immutable.HashSet[Node]
  type NodeIdIslands = scala.collection.immutable.HashSet[Island]
  val debug: Boolean = false
  val printHistogram = true

  // The external interface - immutable sets.
  class Island(theIslandId: Int, theNodes: scala.collection.immutable.Set[Node], theRoots: scala.collection.immutable.Set[Node]) {
    val islandId = theIslandId
    val nodes = theNodes
    val roots = theRoots
    def isEmpty: Boolean = nodes.isEmpty
  }

  // We define this as a class (as opposed to a simple type) in order to specify the size.
  private class MarkedNodes(isize: Int = 16) extends HashMap[Node, Int] {
    override def initialSize: Int = isize
  }

  private type mutableIslandNodes = scala.collection.mutable.HashSet[Node]
  private class mutableIsland(theIslandId: Int, theNodes: mutableIslandNodes, theRoots: mutableIslandNodes) {
    val islandId = theIslandId
    val nodes = theNodes
    val roots = theRoots
    def isEmpty: Boolean = nodes.isEmpty
  }

  private type IslandCollection = HashMap[Int, mutableIsland]

  /** Indicate whether a node is a root node or not.
   *
   */
  def isRoot(node: Node): Boolean = {
    (node.isIo && node.asInstanceOf[Bits].dir == OUTPUT && node.consumers.size == 0)
  }

  def isSource(node: Node): Boolean = {
    (node.isIo && node.asInstanceOf[Bits].dir == INPUT && node.inputs.size == 0)
  }

  /** Flood fill the graph, marking all non-root nodes reachable from here.
   * We visit inputs and consumers, stopping when we hit a register or memory.
   * We process unmarked inputs of marked nodes.
   * We do not process any consumers of marked nodes.
   */
  private def flood(sNode: Node, islandId: Int, marked: MarkedNodes, inputs: Set[Node], prefixNodes: Seq[Node]): mutableIsland = {
    val work = new Stack[Node]
    val islandNodes = new mutableIslandNodes
    islandNodes ++= prefixNodes
    val islandRoots = new mutableIslandNodes
    def processLinks(nodes: Seq[Node]) {
      // Process all unmarked nodes.
      for (n <- nodes) {
        if (n.isInstanceOf[RegReset]) {
          if (debug) println(" ignoring " + n.component.name + "/" + n + " marked " + marked.contains(n))
        } else {
          if (debug) println(" considering " + n.component.name + "/" + n + " marked " + marked.contains(n))
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
      processLinks(node.consumers.toSeq)
      // Special case - we want to process any unmarked inputs for consumer nodes which are island boundaries, marked or not.
      for (c <- node.consumers; if inputs.contains(c) && marked.contains(c)) {
        processLinks(c.inputs)
      }

    }
    new mutableIsland(islandId, islandNodes, islandRoots)
  }

  /* Create separately compilable islands of combinational logic.
   *
   */
  def createIslands(): Array[Island] = {
    type NodeSet = HashSet[Node]
    val res = new IslandCollection
    val roots = new ArrayBuffer[Node]
    val inputs = new NodeSet
    val registers = new NodeSet
    val memories = new NodeSet
    val lits = new NodeSet
    val barren = new NodeSet
    val bogus = new NodeSet
    val markedNodes = new MarkedNodes(Driver.orderedNodes.size)
    var islandId = 1
    val doMoveMemories = true
    val mergeSingleNodeIslands = true

    // Mark all the Bits nodes directly reachable from an initial node.
    def markBitsNodes(iNode: Node, marked: MarkedNodes, islandId: Int) {
      // Follow consumers of the initial (input) node,
      // until we find a non-Bits node from which we will later flood.
      // We need to mark these nodes now so we don't flood into them during the flooding stage.
      // The islandId isn't critical here.
      // If we run out of consumers before hitting a non-bits node,
      //  don't mark the last non-bits node. Otherwise, we won't account for this
      //  collection and we'll generate an exception when we try to determine the island
      //  for the intermediate nodes.
      if (iNode.isInstanceOf[Bits] && iNode.consumers.size > 0) {
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
      if (iNode.isInstanceOf[Bits] && iNode.consumers.size > 0) {
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
      }
      work.pop()
      if (debug) {
        println("islandsFromNonBitsNode: popped " + iNode.component.name + "/" + iNode)
      }
      nIslands
    }

    ChiselError.info("creating combinatorial islands")
    var nodeCount = 0
    var maxNodeId = 0
    // Generate an array of input and output nodes for this module, and initialize the island id for each node.
    for (node <- Driver.orderedNodes) {
      if (node.isIo) {
        if (isRoot(node)) {
          roots += node
          // If this is a root node with no inputs, add it to the bogus list.
          if (node.inputs.size == 0) {
            bogus += node
          }
        } else if (isSource(node)) {
          inputs += node
        } else if (node.inputs.size == 0 || node.consumers.size == 0) {
          bogus += node
        }
      } else {
        // If we're a delay node (Reg or Mem), our consumers become inputs
        node match {
          case r: Reg => {
            registers += r
          }
          case l: Literal => lits += l
          case m: Mem[_] => memories += m
          /*
          case r: MemRead => memories += r
          case w: MemWrite => memories += w
          */
          case _ => if (false && node.inputs.size == 0) barren += node
        }
      }
      nodeCount += 1
      if (node._id > maxNodeId) {
        maxNodeId = node._id
      }
      if (debug && (nodeCount % 1000) == 0) {
        println("createIslands: classified " + nodeCount + " nodes.")
      }
    }

    val delays = registers ++ memories

    for (n <- bogus) {
      println("createIslands: bogus " + n)
    }
    // Add all the real inputs to island 1 (just to give the dot backend a place to put them).
    val allNonDelayLeaves = (inputs ++ lits ++ barren ++ bogus)
    val allNonDelayLeavesBuffer = allNonDelayLeaves.to[ArrayBuffer]
    val allLeaves = allNonDelayLeaves ++ delays
    // First we mark all the "input" nodes.
    // This includes all the "true" input nodes, plus any Reg or Mem nodes,
    //  plus all Bits nodes directly reachable from an input node.
    for (inode <- allLeaves) {
      markedNodes += ((inode, islandId))
      // Put all input, delay, bogus, and barren nodes in their own island.
      val an = new mutableIslandNodes
      an += inode
      if (debug) {
        println("creating island leaf: " + islandId + " on " + inode.component.name + "/" + inode)
      }
      // Now mark all the directly-reachable Bits nodes.
      for ( pNode <-inode.consumers) {
        markBitsNodes(pNode, markedNodes, islandId)
      }
      islandId += 1
    }

    // Put all the nodes we've marked into the results.
    for ((node, island) <- markedNodes) {
      if (res.contains(island)) {
        res(island).nodes += node
      } else {
        val an = new mutableIslandNodes
        an += node
        res += ((island, new mutableIsland(island, an, an)))
      }
    }

    // Flood fill, generating islands
    // We build a work stack until we hit a node that is not an instance of Bits,
    // then we flood from the non-Bits node including all that node's parents in the island.
    for (inode <- allLeaves) {
      // Flood from all the unmarked consumers of this node.
      // We don't add the inode to the island.
      // It's already been put in its own island.
      // When we're all done, we may move this node into the island containing all its inputs.
      for ( pNode <-inode.consumers) {
        val work = new Stack[Node]
        islandsFromNonBitsNode(pNode, res, markedNodes, allLeaves, work)
      }
    }

    // Move from our current island to that of our inputs.
    // Do nothing if our inputs aren't all in the same island.
    // Ignore literals and registers.
    // In the case of literals, their island of residence shouldn't matter.
    def islandHop(s: Node) {
      val inputsIslandSet = Set[Int]()
      s.inputs.filter(i => !(i.isInstanceOf[Literal] || i.isInstanceOf[Reg])).foreach(inputsIslandSet += markedNodes(_))
      if (inputsIslandSet.size != 1) {
//        ChiselError.info("islandHop: node "+ s + " - non-unique input")
        return
      }
      val srcIsland = markedNodes(s)
      val dstIsland = markedNodes(s.inputs(0))
      // Ensure srcIsland and dstIsland exist.
      if (!res.contains(srcIsland)) {
        ChiselError.error("islandHop: node " + s + " - source island " + srcIsland + " not in results", s.line)
        return
      }
      if (!res.contains(dstIsland)) {
        ChiselError.error("islandHop: node " + s.inputs(0) + " - destination island " + dstIsland + " not in results", s.inputs(0).line)
        return
      }
      // Remove us from collection of nodes associated with this island.
      res(srcIsland).nodes -= s
      // If this island is now empty, remove it from the island set.
      if (res(srcIsland).nodes.size == 0) {
        res -= srcIsland
      }
      // Add us to the destination island node collection.
      res(dstIsland).nodes += s
      // Update our entry in the markedNodes collection to reflect the new island location.
      // This is not strictly necessary since we're done with the markedNodes collection.
      markedNodes(s) = dstIsland
    }
    // Move memory nodes from their independent island, to the island containing
    // their single input.
    // We need to do this so the clock_hi generated code updates the memory state correctly.
    if (doMoveMemories) {
      for (m <- registers ++ memories) {
        // Move from our current island to that of our single input.
        // islandHop will complain if we don't have a unique input.
        islandHop(m)
      }
    }
    // Delete any empty islands, merge single node islands (if configured to do so),
    //  and find the one with the most nodes.
    var islandMaxNodes = (0, 0)
    var singleNodeIslandId = 0
    var singleNodeIslandNodes = 0
    val countSingleIslandMerges = false
    val maxNodesPerIsland = 30000
    for ((k, v) <- res) {
      var nnode = k
      var nnodes = v.nodes.size
      if (nnodes == 0) {
        res -= k
      } else {
        if (mergeSingleNodeIslands && nnodes == 1) {
          // This is a single node island. Combine it with all the other single node islands.
          singleNodeIslandNodes += 1
          // Is it time for a new single node island?
          if ((singleNodeIslandNodes + nnodes >= maxNodesPerIsland) || singleNodeIslandId == 0) {
            singleNodeIslandId = k
            singleNodeIslandNodes = 1
          } else {
            if (countSingleIslandMerges) {
              nnodes = singleNodeIslandNodes
              nnode = singleNodeIslandId
            }
            res(singleNodeIslandId).nodes ++= v.nodes
            res(singleNodeIslandId).roots ++= v.roots
            res -= k
          }
        }
        // Is this a new maximum?
        if (nnodes > islandMaxNodes._2) {
          islandMaxNodes = (nnode, nnodes)
        }
      }
    }

    // Construct the delivered data structure, with correctly sized Sets
    val resArrayBuffer = new ArrayBuffer[Island](res.size)
    for (islandId <- res.keySet.toList.sorted) {
      val island = res(islandId)
      val theNodes = scala.collection.immutable.HashSet.empty ++ island.nodes
      val theRoots = scala.collection.immutable.HashSet.empty ++ island.roots
      resArrayBuffer += new Island(islandId, theNodes, theRoots)
      island.nodes.clear()
      island.roots.clear()
    }
    res.clear()

    if (printHistogram) {
      // Output histogram info for the partitioning
      println("createIslands: " + resArrayBuffer.size + " islands, containing " + nodeCount + " nodes")
      val islandHistogram = Map[Int, Int]()
      for (island <- resArrayBuffer) {
        val n = island.nodes.size
        islandHistogram(n) = islandHistogram.getOrElse(n, 0) + 1
      }
      for ((k,v) <- islandHistogram) {
        println("createIslands: islands " + v + ", nodes " + k)
      }
      println("createIslands: island " + islandMaxNodes._1 + " has " + islandMaxNodes._2 + " nodes")
    }
    resArrayBuffer.toArray
  }

  // Generate a mapping from node to islands, given an array of islands.
  def generateNodeToIslandArray(islands: Array[Island]): Array[NodeIdIslands] = {
    if (islands == null || islands.size == 0) {
      return null
    } else if (! islands.exists(_.nodes.size > 0)) {
      // No islands with nodes.
      // Construct an array with a single entry containing an empty set.
      return Array[NodeIdIslands](new NodeIdIslands())
    }
    // Find the maximum node _id
    val maxIslandId = islands.map(_.nodes.map(_._id).max).max
    // Generate an array to hold the set of islands per node.
    val nodeToIslands: Array[NodeIdIslands] = new Array(maxIslandId + 1)
    for (island <- islands) {
      for (node <- island.nodes) {
        if (nodeToIslands(node._id) == null) {
          nodeToIslands(node._id) = new NodeIdIslands
        }
        nodeToIslands(node._id) += island
      }
    }
    nodeToIslands
  }
}
