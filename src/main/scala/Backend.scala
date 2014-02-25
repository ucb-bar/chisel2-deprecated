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
import scala.collection.mutable.{Queue=>ScalaQueue}
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
    val baseDir = ensureDir(Module.targetDir)
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

  def extractClassName(comp: Module): String = {
    val cname  = comp.getClass().getName().replace("$", "_")
    val dotPos = cname.lastIndexOf('.');
    Backend.moduleNamePrefix + (
      if (dotPos >= 0) cname.substring(dotPos + 1) else cname);
  }

  protected def genIndent(x: Int): String = {
    if(x == 0) "" else "    " + genIndent(x-1);
  }

  def setNames(top: Module) {
    // First, check whether there are multiple instances of a module
    val classNames = new HashMap[String, ArrayBuffer[Module]]

    for (c <- Module.components) {
      // c.io nameIt ("io", true) // naming ios
      
      if (c.name == "") {
        val className = extractClassName(c)
        if (!(classNames contains className)) {
          classNames(className) = new ArrayBuffer[Module]
        }
        classNames(className) += c
      }
    }

    // Give names to components according to their instrance numbers
    for ((name, comps) <- classNames) {
      // A class has more than one instance
      if (comps.size > 1) {
        for ((c, i) <- comps.zipWithIndex) {
          c.name = asValidName(name + "_" + i)
        }
      }
      // only one instance
      else {
        comps.head.name = asValidName(name)
      }
    }

    top dfs { node =>
      if( node.nameHolder != null && node.nameHolder.name != "" &&
          !node.named && !node.isLit ){
        node.name = node.nameHolder.name // Not using nameIt to avoid override
        node.named = node.nameHolder.named
        node.nameHolder.name = ""
      } else if (!node.isTypeNode && node.name == "") {
        node.emitIndex
        // emitRef(node)
      }
    }
  }

  /* Returns a string derived from _name_ that can be used as a valid
   identifier for the targeted backend. */
  def asValidName( name: String ): String = {
    if (keywords.contains(name)) name + "_" else name;
  }

  def fullyQualifiedName( m: Node ): String = {
    m match {
      case l: Literal => l.toString;
      case any       =>
        if (m.name != ""
          && m != Module.topComponent.defaultResetPin && m.component != null) {
          /* Only modify name if it is not the reset signal
           or not in top component */
          if(m.name != "reset" && m.component != Module.topComponent) {
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
        if (node.name == "") "T" + node.emitIndex else node.name
    }
  }

  def emitRef(c: Module): String =
    c.name

  def emitDec(node: Node): String = ""

  val preElaborateTransforms = ArrayBuffer[(Module) => Unit]()
  val transforms = ArrayBuffer[(Module) => Unit]()
  if (Module.isCSE) transforms += CSE.transform
  val analyses = ArrayBuffer[(Module) => Unit]()

  def initializeDFS: Stack[Node] = {
    val res = new Stack[Node]

    /* XXX Make sure roots are consistent between initializeBFS, initializeDFS
     and findRoots.
     */
    for( clock <- Module.clocks ; if clock.isEnabled) {
      res.push(clock)
    }
    for( c <- Module.components ) {
      for( a <- c.debugs ) {
        res.push(a)
      }
      for((n, flat) <- c.io.flatten) {
        res.push(flat)
      }
    }

    res
  }

  /** Nodes which are created outside the execution trace from the toplevel
    component constructor (i.e. through the () => Module(new Top()) ChiselMain
    argument) will have a component field set to null. For example, genMuxes,
    forceMatchWidths and transforms (all called from Backend.elaborate) create
    such nodes.

    This method walks all nodes from all component roots (outputs, debugs).
    and reassociates the component to the node both ways (i.e. in Module.nodes
    and Node.component).

    We assume here that all nodes at the components boundaries (io) have
    a non-null and correct node/component association. We further assume
    that nodes generated in elaborate are inputs to a node whose component
    field is set.

    Implementation Node:
    At first we did implement *collectNodesIntoComp* to handle a single
    component at a time but that did not catch the cases where Regs are
    passed as input to sub-module without being tied to an output
    of *this.component*.
    */
  def isBitsIo(node: Node, dir: IODirection): Boolean = node match {
    case b: Bits => b.isIo && b.dir == dir
    case _ => false
  }

  def collectNodesIntoComp(dfsStack: Stack[Node]) {
    val walked = new HashSet[Node]()
    walked ++= dfsStack
    // invariant is everything in the stack is walked and has a non-null component
    while(!dfsStack.isEmpty) {
      val node = dfsStack.pop
      /*
      we're tracing from outputs -> inputs, so if node is an input, then its
      inputs belong to the outside component. Otherwise, its inputs are the same
      as node's inputs.
      */
      val curComp = 
        if ( node.isIo && node.asInstanceOf[Bits].dir == INPUT ) {
          node.component.parent
        } else {
          node.component
        }
      if (node.component == null) {
        println("NULL NODE COMPONENT " + node)
      }
      if (!node.component.nodes.contains(node))
        node.component.nodes += node
      for (input <- node.inputs) {
        if (input.component != null && input.component != node.component) {
          if (!input.isLit && !isBitsIo(node, INPUT) && !isBitsIo(input, OUTPUT))
            ChiselErrors += new ChiselError(() => { "Illegal cross module reference between " + node + " and " + input}, node.line)
        }
        if(!walked.contains(input)) {
          if( input.component == null ) {
            input.component = curComp
          }
          walked += input
          dfsStack.push(input)
        }
      }
    }

    assert(dfsStack.isEmpty)
  }

  def execute(c: Module, walks: ArrayBuffer[(Module) => Unit]): Unit = {
    for (w <- walks)
      w(c)
  }

  def pruneUnconnectedIOs(m: Module) {
    m.checkIo // make sure all module ios are ports

    val inputs = m.io.flatten.filter(_._2.dir == INPUT)
    val outputs = m.io.flatten.filter(_._2.dir == OUTPUT)

    for ((name, i) <- inputs) {
      if (i.inputs.length == 0 && m != Module.topComponent)
        if (i.consumers.length > 0) {
          if (Module.warnInputs)
            ChiselError.warning({"UNCONNECTED INPUT " + emitRef(i) + " in COMPONENT " + i.component +
                                 " has consumers"})
          i.driveRand = true
        } else {
          if (Module.warnInputs)
            ChiselError.warning({"FLOATING INPUT " + emitRef(i) + " in COMPONENT " + i.component})
          i.prune = true
        }
    }

    for ((name, o) <- outputs) {
      if (o.inputs.length == 0) {
        if (o.consumers.length > 0) {
          if (Module.warnOutputs)
            ChiselError.warning({"UNCONNECTED OUTPUT " + emitRef(o) + " in component " + o.component + 
                                 " has consumers on line " + o.consumers(0).line})
          o.driveRand = true
        } else {
          if (Module.warnOutputs)
            ChiselError.warning({"FLOATING OUTPUT " + emitRef(o) + " in component " + o.component})
          o.prune = true
        }
      }
    }
  }

  def pruneNodes {
    val walked = new HashSet[Node]
    val bfsQueue = new ScalaQueue[Node]
    for (node <- Module.randInitIOs) bfsQueue.enqueue(node)
    var pruneCount = 0

    // conduct bfs to find all reachable nodes
    while(!bfsQueue.isEmpty){
      val top = bfsQueue.dequeue
      walked += top
      val prune = top.inputs.map(_.prune).foldLeft(true)(_ && _)
      pruneCount+= (if (prune) 1 else 0)
      top.prune = prune
      for(i <- top.consumers) {
        if(!(i == null)) {
          if(!walked.contains(i)) {
            bfsQueue.enqueue(i)
            walked += i
          }
        }
      }
    }
    ChiselError.warning("Pruned " + pruneCount + " nodes due to unconnected inputs")
  }

  def emitDef(node: Node): String = ""

  def levelChildren(root: Module) {
    root.level = 0;
    root.traversal = VerilogBackend.traversalIndex;
    VerilogBackend.traversalIndex = VerilogBackend.traversalIndex + 1;
    for(child <- root.children) {
      levelChildren(child)
      root.level = math.max(root.level, child.level + 1);
    }
  }

  def gatherChildren(root: Module): ArrayBuffer[Module] = {
    var result = new ArrayBuffer[Module]();
    for (child <- root.children)
      result = result ++ gatherChildren(child);
    result ++ ArrayBuffer[Module](root);
  }

  // go through every Module and set its clock and reset field
  def assignClockAndResetToModules {
    for (module <- Module.sortedComps.reverse) {
      if (module.clock == null)
        module.clock = module.parent.clock
      if (!module.hasExplicitReset)
        module.reset_=
    }
  }

  // go through every Module, add all clocks+resets used in it's tree to it's list of clocks+resets
  def gatherClocksAndResets {
    for (parent <- Module.sortedComps) {
      for (child <- parent.children) {
        for (clock <- child.clocks ; if !clock.isEnabled) {
          parent.addClock(clock)
        }
        for (reset <- child.resets.keys) {
          // create a reset pin in parent if reset does not originate in parent and 
          // if reset is not an output from one of parent's children
          if (reset.component != parent && !parent.children.contains(reset.component))
            parent.addResetPin(reset)

          // special case for implicit reset
          if (reset == Module.implicitReset && parent == Module.topComponent)
            if (!parent.resets.contains(reset))
              parent.resets += (reset -> reset)
        }
      }
    }
  }

  def connectResets {
    for (parent <- Module.sortedComps) {
      for (child <- parent.children) {
        for (reset <- child.resets.keys) {
          if (child.resets(reset).inputs.length == 0)
            if (parent.resets.contains(reset))
              child.resets(reset).inputs += parent.resets(reset)
            else 
              child.resets(reset).inputs += reset
        }
      }
    }
  }

  def nameRsts {
    for (comp <- Module.sortedComps) {
      for (rst <- comp.resets.keys) {
        if (!comp.resets(rst).named)
            comp.resets(rst).setName(rst.name)
      }
    }
  }

  def nameBindings(nameSpace: HashSet[String]) {
    for (c <- Module.sortedComps ; bind <- c.bindings) {
      var genName = ""
      if (bind.targetNode.name != null || bind.targetNode.name != "")
        genName = bind.targetComponent.name + "_" + bind.targetNode.name

      if(nameSpace contains genName) 
        genName += ("_" + bind.emitIndex)

      // Not using nameIt to avoid override
      bind.name = asValidName(genName)
      bind.named = true;
    }
  }

  // walk forward from root register assigning consumer clk = root.clock
  def createClkDomain(root: Node, walked: ArrayBuffer[Node]) = {
    val dfsStack = new Stack[Node]
    walked += root; dfsStack.push(root)
    val clock = root.clock
    while(!dfsStack.isEmpty) {
      val node = dfsStack.pop
      for (consumer <- node.consumers) {
        if (!consumer.isInstanceOf[Delay] && !walked.contains(consumer)) {
          val c1 = consumer.clock
          val c2 = clock
          if(!(consumer.clock == null || consumer.clock == clock)) {
            ChiselError.warning({consumer.getClass + " " + emitRef(consumer) + " " + emitDef(consumer) + "in module" +
                                 consumer.component + " resolves to clock domain " + 
                                 emitRef(c1) + " and " + emitRef(c2) + " traced from " + root.name})
          } else { consumer.clock = clock }
          walked += consumer
          dfsStack.push(consumer)
        }
      }
    }
  }

  val nameSpace = new HashSet[String]
  def elaborate(c: Module): Unit = {
    println("backend elaborate")
    Module.setAsTopComponent(c)

    Module.components.foreach(_.elaborate(0));

    /* XXX We should name all signals before error messages are generated
     so as to give a clue where problems are showing up but that interfers
     with the *bindings* (see later comment). */
    for (c <- Module.components)
      c markComponent nameSpace

    // Set signal signals except bindings because they are not generated.
    // Bindings are named separately after generated
    setNames(c)

    // XXX This will create nodes after the tree is traversed!
    c.genAllMuxes

    // obtain sorted modules before preElaborateTransforms
    levelChildren(c)
    Module.sortedComps = gatherChildren(c).sortWith(
      (x, y) => (x.level < y.level || (x.level == y.level && x.traversal < y.traversal)));

    // We have high-leve IRs here, 
    // so we can include optimizations
    // like backannotation, automatic tools
    // in preElaborateTransforms
    execute(c, preElaborateTransforms)

    // preElaborateTransforms can change the top component
    // e.g. Counter generation
    val top = Module.topComponent

    Module.components.foreach(_.postMarkNet(0));
    ChiselError.info("// COMPILING " + c + "(" + c.children.length + ")");
    // Module.assignResets()

    assignClockAndResetToModules
    Module.sortedComps.map(_.addDefaultReset)
    top.addClockAndReset
    gatherClocksAndResets
    connectResets
    nameRsts // name resets here

    ChiselError.info("started inference")
    val nbOuterLoops = top.inferAll();
    ChiselError.info("finished inference (" + nbOuterLoops + ")")
    ChiselError.info("start width checking")
    top.forceMatchingWidths;
    ChiselError.info("finished width checking")
    ChiselError.info("started flattenning")
    val nbNodes = top.removeTypeNodes()
    ChiselError.info("finished flattening (" + nbNodes + ")")
    ChiselError.checkpoint()

    /* *collectNodesIntoComp* associates components to nodes that were
     created after the call tree has been executed (ie. in genMuxes
     and forceMatchWidths).

     The purpose of *collectNodesIntoComp* is to insure user-defined
     transforms will be able to query a component for all its nodes
     and a node for its component.

     Technically all user-defined transforms are responsible to update
     nodes and component correctly or call collectNodesIntoComp on return.
     */
    ChiselError.info("resolving nodes to the components")
    collectNodesIntoComp(initializeDFS)
    ChiselError.info("finished resolving")

    // two transforms added in Mem.scala (referenced and computePorts)
    // then, we have medium-level IRs
    // include optimizations into transforms
    ChiselError.info("started transforms")
    execute(top, transforms)
    ChiselError.info("finished transforms")

    Module.sortedComps.map(_.nodes.map(_.addConsumers))
    top.traceNodes
    nameBindings(nameSpace)
    val clkDomainWalkedNodes = new ArrayBuffer[Node]
    for (comp <- Module.sortedComps)
      for (node <- comp.nodes)
        if (node.isInstanceOf[Reg])
          createClkDomain(node, clkDomainWalkedNodes)
    ChiselError.checkpoint()

    for (comp <- Module.sortedComps ) {
      // remove unconnected outputs
      pruneUnconnectedIOs(comp)
    }

    // We have low-level IRs 
    execute(top, analyses)

    ChiselError.checkpoint()

    if(!Module.dontFindCombLoop) {
      ChiselError.info("checking for combinational loops")
      top.findCombLoop();
      ChiselError.checkpoint()
      ChiselError.info("NO COMBINATIONAL LOOP FOUND")
    }

    if(Module.saveComponentTrace) {
      printStack
    }
  }

  def compile(c: Module, flags: String = null): Unit = { }

  def checkPorts(topC: Module) {

    def prettyPrint(n: Node, c: Module) {
      val dir = if (n.asInstanceOf[Bits].dir == INPUT) "Input" else "Output"
      val portName = n.name
      val compName = c.name
      val compInstName = c.moduleName
      ChiselError.warning(dir + " port " + portName
        + " is unconnected in module " + compInstName + " " + compName)
    }

    for (c <- Module.components) {
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
    for((i, c) <- Module.printStackStruct){
      res += (genIndent(i) + c.moduleName + " " + c.name + "\n")
    }
    ChiselError.info(res)
  }
}


