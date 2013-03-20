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
import Node._
import Reg._
import ChiselError._
import Component._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack
import scala.collection.mutable.HashSet

object Backend {
}

abstract class Backend {
  def depthString(depth: Int): String = {
    var res = "";
    for (i <- 0 until depth)
      res += "  ";
    res
  }

 def emitTmp(node: Node): String

  def emitRef(node: Node): String = {
    node match {
      case r: Reg =>
        if (r.name == "") "R" + r.emitIndex else r.name
      case _ =>
        if(node.name == "" || !node.named) {
          "T" + node.emitIndex
        } else if(!node.named) {
          node.name + "_" + node.emitIndex
        } else {
          node.name
        }
    }
  }

  def emitRef(c: Component): String =
    c.name

  def emitDec(node: Node): String = ""

  val transforms = ArrayBuffer[(Component) => Unit]()

  // DFS walk of graph to collect nodes of every component
  def collectNodesIntoComp(c: Component) = {
    val dfsStack = new Stack[(Node, Component)]()
    val walked = new HashSet[Node]()

    def walk = {
      var nextComp = c

      while(!dfsStack.isEmpty) {
        val (node, curComp) = dfsStack.pop

        if(!walked.contains(node)) {
          walked += node
          // push and pop components as necessary
          node match {
            case io: Bits => {
              if (io.dir == OUTPUT) { // push
                nextComp = io.component
              } else if (io.dir == INPUT) { // pop
                nextComp = io.component.parent
              } else { // do nothing
                nextComp = curComp
              }
            }
            case any => // do nothing
              nextComp = curComp
          }

          // collect inputs into component
          for (input <- node.inputs) {
            if(!walked.contains(input)) {
              nextComp.nodes += input
              if(input.component == null) input.component = nextComp
              dfsStack.push((input, nextComp))
            }
          }
        }
      }

    }

    println("resolving nodes to the components")
    // start DFS from top level inputs
    // dequeing from dfsStack => walked
    for ((name, io) <- c.io.flatten) {
      assert(io.isInstanceOf[Bits])
      if(io.asInstanceOf[Bits].dir == OUTPUT) {
        c.nodes += io
        io.component = c
        dfsStack.push((io, c))
      }
    }

    walk;
    assert(dfsStack.isEmpty)
    println("finished resolving")
  }

  def transform(c: Component, transforms: ArrayBuffer[(Component) => Unit]): Unit = {
    for (t <- transforms)
      t(c)
  }


  def emitDef(node: Node): String = ""

  def elaborate(c: Component): Unit = { }

  def compile(c: Component, flags: String = null): Unit = { }

  def checkPorts(topC: Component) = {

    def prettyPrint(n: Node, c: Component) = {
      val dir = if (n.asInstanceOf[Bits].dir == INPUT) "Input" else "Output"
      val portName = n.name
      val compName = c.name
      val compInstName = c.instanceName
      println("Warning: " + dir + " port " + portName
        + " is unconnected in module " + compInstName + " " + compName)
    }

    for (c <- components) {
      if (c != topC) {
        for ((n,i) <- c.io.flatten) {
          if (i.inputs.length == 0) {
            prettyPrint(i, c)
          }
        }
      }
    }

  }

}


