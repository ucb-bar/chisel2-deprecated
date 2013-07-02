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
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import java.lang.reflect.Modifier._
import java.io.File
import java.io.InputStream
import java.io.OutputStream
import java.io.PrintStream

object Backend {
  var moduleNamePrefix = ""
}

abstract class Backend {

  /* Set of keywords which cannot be used as node and component names. */
  val keywords: HashSet[String];

  def createOutputFile(name: String): java.io.FileWriter = {
    val baseDir = ensureDir(Mod.targetDir)
    new java.io.FileWriter(baseDir + name)
  }

  def depthString(depth: Int): String = {
    var res = "";
    for (i <- 0 until depth)
      res += "  ";
    res
  }

  /** Ensures a directory *dir* exists on the filesystem. */
  def ensureDir(dir: String): String = {
    val d = dir + (if (dir == "" || dir(dir.length-1) == '/') "" else "/")
    new File(d).mkdirs()
    d
  }

  def extractClassName(comp: Mod): String = {
    val cname  = comp.getClass().getName().replace("$", "_")
    val dotPos = cname.lastIndexOf('.');
    Backend.moduleNamePrefix + (
      if (dotPos >= 0) cname.substring(dotPos + 1) else cname);
  }

  protected def genIndent(x: Int): String = {
    if(x == 0) "" else "    " + genIndent(x-1);
  }

  def nameChildren(root: Mod) {
    // Name all nodes at this level
    root.io.nameIt("io");
    val nameSpace = new HashSet[String];
    /* We are going through all declarations, which can return Nodes,
     ArrayBuffer[Node], Cell, BlackBox and Mods.
     Since we call invoke() to get a proper instance of the correct type,
     we have to insure the method is accessible, thus all fields
     that will generate C++ or Verilog code must be made public. */
    for (m <- root.getClass().getDeclaredMethods) {
      val name = m.getName();
      val types = m.getParameterTypes();
      if (types.length == 0
        && isPublic(m.getModifiers()) && !(Mod.keywords contains name)) {
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
            XXX Using Seq instead of ArrayBuffer will pick up members defined
            in Mod that are solely there for implementation purposes. */
           if(!buf.isEmpty && buf.head.isInstanceOf[Node]){
             val nodebuf = buf.asInstanceOf[Seq[Node]];
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
         case comp: Mod => {
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
    val byNames = new HashMap[String, ArrayBuffer[Mod] ];
    for (c <- root.children) {
      nameChildren(c);
      if( c.name.isEmpty ) {
        /* We don't have a name because we are not dealing with
         a class member. */
        val className = extractClassName(c);
        if( byNames contains className ) {
          byNames(className).append(c);
        } else {
          byNames += (className -> ArrayBuffer[Mod](c));
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

  def nameAll(root: Mod) {
    root.name = extractClassName(root);
    nameChildren(root);
    for( node <- Mod.nodes ) {
      if( (node.nameHolder != null && !node.nameHolder.name.isEmpty)
        && !node.named && !node.isInstanceOf[Literal] ){
        node.name = node.nameHolder.name; // Not using nameIt to avoid override
        node.named = node.nameHolder.named;
        node.nameHolder.name = "";
      }
    }
  }

  def fullyQualifiedName( m: Node ): String = {
    m match {
      case l: Literal => l.toString;
      case any       =>
        if (m.name != ""
          && m != Mod.topComponent.reset && m.component != null) {
          /* Only modify name if it is not the reset signal
           or not in top component */
          if(m.name != "reset" && m.component != Mod.topComponent) {
            m.component.getPathName + "__" + m.name;
          } else {
            m.name
          }
        } else {
          m.name
        }
    }
  }

 def emitTmp(node: Node): String =
    emitRef(node)

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

  def emitRef(c: Mod): String =
    c.name

  def emitDec(node: Node): String = ""

  val transforms = ArrayBuffer[(Mod) => Unit]()

  // DFS walk of graph to collect nodes of every component
  def collectNodesIntoComp(c: Mod) {
    val dfsStack = c.initializeDFS
    val walked = new HashSet[Node]()

    def walk {
      var nextComp = c

      while(!dfsStack.isEmpty) {
        val node = dfsStack.pop

        if(!walked.contains(node)) {
          walked += node
          // collect unassigned nodes into component
          for (input <- node.inputs) {
            if(!walked.contains(input)) {
              nextComp.nodes += input
              if( input.component == null ) input.component = node.component
              dfsStack.push(input)
            }
          }
        }
      }

    }

    ChiselError.info("resolving nodes to the components")
    // start DFS from top level inputs
    // dequeing from dfsStack => walked
    for (io <- dfsStack) {
      // XXX Not needed anymore?
      assert(io.isInstanceOf[Bits])
      if(io.asInstanceOf[Bits].dir == OUTPUT) {
        c.nodes += io
        assert( io.component == c )
      }
    }

    walk;
    assert(dfsStack.isEmpty)
    ChiselError.info("finished resolving")
  }

  def transform(c: Mod, transforms: ArrayBuffer[(Mod) => Unit]): Unit = {
    for (t <- transforms)
      t(c)
  }


  def emitDef(node: Node): String = ""

  def elaborate(c: Mod): Unit = {
    Mod.topComponent = c;
    /* XXX If we call nameAll here and again further down, we end-up with
     duplicate names in the generated C++.
     nameAll(c) */

    Mod.components.foreach(_.elaborate(0));

    /* XXX We should name all signals before error messages are generated
     so as to give a clue where problems are showing up but that interfers
     with the *bindings* (see later comment). */
    for (c <- Mod.components)
      c.markComponent();
    // XXX This will create nodes after the tree is traversed!
    c.genAllMuxes;
    Mod.components.foreach(_.postMarkNet(0));
    ChiselError.info("// COMPILING " + c + "(" + c.children.length + ")");
    Mod.assignResets()

    /* XXX Temporary debugging info. */
//XXX    c.findConsumers();

    ChiselError.info("started inference")
    val nbOuterLoops = c.inferAll();
    ChiselError.info("finished inference (" + nbOuterLoops + ")")
    ChiselError.info("start width checking")
    c.forceMatchingWidths;
    ChiselError.info("finished width checking")
    ChiselError.info("started flattenning")
    val nbNodes = c.removeTypeNodes()
    ChiselError.info("finished flattening (" + nbNodes + ")")
    ChiselError.checkpoint()

    /* The code in this function seems wrong. Yet we still need to call
     it to associate components to nodes that were created after the call
     tree has been executed (ie. in genMuxes). More nodes are created
     in transforms. I don't know why collect with be executed before then.
     */
    collectNodesIntoComp(c)
    // two transforms added in Mem.scala (referenced and computePorts)
    transform(c, transforms)
    c.traceNodes();
    ChiselError.checkpoint()

    /* We execute nameAll after traceNodes because bindings would not have been
       created yet otherwise. */
    nameAll(c)
    ChiselError.checkpoint()

    if(!Mod.dontFindCombLoop) {
      ChiselError.info("checking for combinational loops")
      c.findCombLoop();
      ChiselError.checkpoint()
      ChiselError.info("NO COMBINATIONAL LOOP FOUND")
    }
    if(Mod.saveComponentTrace) {
      printStack
    }
  }

  def compile(c: Mod, flags: String = null): Unit = { }

  def checkPorts(topC: Mod) {

    def prettyPrint(n: Node, c: Mod) {
      val dir = if (n.asInstanceOf[Bits].dir == INPUT) "Input" else "Output"
      val portName = n.name
      val compName = c.name
      val compInstName = c.moduleName
      ChiselError.warning(dir + " port " + portName
        + " is unconnected in module " + compInstName + " " + compName)
    }

    for (c <- Mod.components) {
      if (c != topC) {
        for ((n,i) <- c.io.flatten) {
          if (i.inputs.length == 0) {
            prettyPrint(i, c)
          }
        }
      }
    }

  }

  /** Prints the call stack of Component as seen by the push/pop runtime. */
  protected def printStack {
    var res = ""
    for((i, c) <- Mod.printStackStruct){
      res += (genIndent(i) + c.moduleName + " " + c.name + "\n")
    }
    ChiselError.info(res)
  }

}


