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
import scala.collection.mutable.HashMap

object Backend {
  var moduleNamePrefix = ""
}

abstract class Backend {

  /* Set of keywords which cannot be used as node and component names. */
  val keywords: HashSet[String];

  def depthString(depth: Int): String = {
    var res = "";
    for (i <- 0 until depth)
      res += "  ";
    res
  }

  def extractClassName(comp: Component): String = {
    val cname  = comp.getClass().getName().replace("$", "_")
    val dotPos = cname.lastIndexOf('.');
    Backend.moduleNamePrefix + (
      if (dotPos >= 0) cname.substring(dotPos + 1) else cname);
  }

  def nameChildren(root: Component) {
    // Name all nodes at this level
    root.io.nameIt("io");
    // We are going through all declarations, which can return Nodes,
    // ArrayBuffer[Node], Cell, BlackBox and Components.
    val nameSpace = new HashSet[String];
    for (m <- root.getClass().getDeclaredMethods) {
      val name = m.getName();
      val types = m.getParameterTypes();
      if (types.length == 0 && name != "test") {
        val o = m.invoke(root);
        o match {
         case node: Node => {
           /* XXX It seems to always be true. How can name be empty? */
           if ((node.isTypeNode || name != ""
             || node.name == null || (node.name == "" && !node.named))) {
             node.nameIt(asValidName(name));
           }
           nameSpace += node.name;
         }
         case buf: ArrayBuffer[_] => {
           /* We would prefer to match for ArrayBuffer[Node] but that's
            impossible because of JVM constraints which lead to type erasure.
            XXX We might want to match Seq[_] instead of ArrayBuffer[_]. */
           if(!buf.isEmpty && buf(0).isInstanceOf[Node]){
             val nodebuf = buf.asInstanceOf[ArrayBuffer[Node]];
             var i = 0;
             for(elm <- nodebuf){
               if( elm.isTypeNode || elm.name == null || elm.name.isEmpty ) {
                 /* XXX This code is sensitive to when Bundle.nameIt is called.
                  Whether it is called late (elm.name is empty) or we override
                  any previous name that could have been infered,
                  this has for side-effect to create modules with the exact
                  same logic but textually different in input/output
                  parameters, hence generating unnecessary modules. */
                 elm.nameIt(asValidName(name + "_" + i));
               }
               nameSpace += elm.name;
               i += 1;
             }
           }
         }
         case cell: Cell => {
           cell.name = asValidName(name);
           cell.named = true;
           nameSpace += cell.name;
         }
         case bb: BlackBox => {
           if(!bb.named) {
             bb.name = name;
             bb.named = true
           };
           nameSpace += bb.name;
         }
         case comp: Component => {
           if(!comp.named) {
             comp.name = asValidName(name);
             comp.named = true
           };
           nameSpace += comp.name;
         }
         case any => {
           /* We have no idea what to do with class members which are
            neither of the previous types. Let's discard them. */
         }
        }
      }
    }
    /* Recursively name the nodes and components inside this root.
     This code must be executed between the root-level naming and the naming
     of bindings otherwise some identifiers will leak into the input/output
     of a module. */
    val byNames = new HashMap[String, ArrayBuffer[Component] ];
    for (c <- root.children) {
      nameChildren(c);
      if( c.name.isEmpty ) {
        /* We don't have a name because we are not dealing with
         a class member. */
        val className = extractClassName(c);
        if( byNames contains className ) {
          byNames(className).append(c);
        } else {
          byNames += (className -> ArrayBuffer[Component](c));
        }
      }
    }

    for( (className, comps) <- byNames ) {
        if( comps.length > 1 ) {
          for( (c, index) <- comps.zipWithIndex ) {
            c.name = className + "_" + index.toString
          }
        } else {
          comps(0).name = className;
        }
    }

    for (bind <- root.bindings) {
      var genName = if (bind.targetNode.name == null || bind.targetNode.name.length() == 0) "" else bind.targetComponent.name + "_" + bind.targetNode.name;
      if(nameSpace.contains(genName)) genName += ("_" + bind.emitIndex);
      bind.name = asValidName(genName); // Not using nameIt to avoid override
      bind.named = true;
    }
  }

  /* Returns a string derived from _name_ that can be used as a valid
   identifier for the targeted backend. */
  def asValidName( name: String ): String = {
    if (keywords.contains(name)) name + "_" else name;
  }

  def nameAll(root: Component) = {
    root.name = extractClassName(root);
    nameChildren(root);
    for( node <- nodes ) {
      if( (node.nameHolder != null && !node.nameHolder.name.isEmpty)
        && !node.named && !node.isInstanceOf[Literal] ){
        node.name = node.nameHolder.name; // Not using nameIt to avoid override
        node.named = node.nameHolder.named;
        node.nameHolder.name = "";
      }
    }
  }

 def emitTmp(node: Node): String

  def emitRef(node: Node): String = {
    node match {
      case r: Reg =>
        if (r.name == "") "R" + r.emitIndex else r.name
      case _ =>
        if(node.name == "") {
          "T" + node.emitIndex
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

  def elaborate(c: Component): Unit = {}

  def compile(c: Component, flags: String = null): Unit = { }

  def checkPorts(topC: Component) = {

    def prettyPrint(n: Node, c: Component) = {
      val dir = if (n.asInstanceOf[Bits].dir == INPUT) "Input" else "Output"
      val portName = n.name
      val compName = c.name
      val compInstName = c.moduleName
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


