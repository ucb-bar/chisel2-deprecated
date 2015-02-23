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
import Node._
import Reg._
import ChiselError._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{Queue=>ScalaQueue}
import scala.collection.mutable.Stack
import scala.collection.mutable.{HashSet, HashMap, LinkedHashMap}
import java.lang.reflect.Modifier._
import java.io.File
import java.io.InputStream
import java.io.OutputStream
import java.io.PrintStream

object Backend {
  var moduleNamePrefix = ""
}

trait FileSystemUtilities {
  /** Ensures a directory *dir* exists on the filesystem. */
  def ensureDir(dir: String): String = {
    val d = dir + (if (dir == "" || dir(dir.length-1) == '/') "" else "/")
    new File(d).mkdirs()
    d
  }

  def createOutputFile(name: String): java.io.FileWriter = {
    val baseDir = ensureDir(Driver.targetDir)
    new java.io.FileWriter(baseDir + name)
  }
}

abstract class Backend extends FileSystemUtilities{
  /* Set of keywords which cannot be used as node and component names. */
  val keywords: Set[String]
  val nameSpace = HashSet[String]()
  /* Set of Ops that this backend doesn't natively support and thus must be
     lowered to simpler Ops. */
  val needsLowering = Set[String]()

  /* Whether or not this backend decomposes along Module boundaries. */
  def isEmittingComponents: Boolean = false

  def depthString(depth: Int): String = {
    var res = "";
    for (i <- 0 until depth)
      res += "  ";
    res
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

  def sortComponents {
    def levelChildren(root: Module, traversal: Int) {
      root.level = 0
      root.traversal = traversal
      for (child <- root.children) {
        levelChildren(child, traversal+1)
        root.level = math.max(root.level, child.level+1)
      }
    }

    def gatherChildren(root: Module): ArrayBuffer[Module] = {
      val result = ArrayBuffer[Module]()
      for (child <- root.children)
        result ++= gatherChildren(child)
      result += root
      result
    }

    levelChildren(Driver.topComponent, 0)
    Driver.sortedComps.clear
    Driver.sortedComps ++= gatherChildren(Driver.topComponent).sortWith(
      (x, y) => (x.level < y.level || (x.level == y.level && x.traversal < y.traversal)))
  }

  def markComponents {
    Driver.components foreach (_.markComponent)
  }

  def verifyAllMuxes {
    Driver.bfs { _ match {
      case p: proc => p.verifyMuxes
      case _ =>
    } }
  }

  /* Returns a string derived from _name_ that can be used as a valid
   identifier for the targeted backend. */
  def asValidName( name: String ): String = {
    if (keywords.contains(name)) name + "_" else name;
  }

  def nameAll(mod: Module) {
    // module naming
    val byNames = LinkedHashMap[String, ArrayBuffer[Module]]();
    for (c <- Driver.sortedComps) {
      if( c.name.isEmpty ) {
        /* We don't have a name because we are not dealing with
         a class member. */
        val className = extractClassName(c);
        if( byNames contains className ) {
          byNames(className) += c
        } else {
          byNames(className) = ArrayBuffer[Module](c)
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
    // temporary node naming:
    // these names are for the Verilog Backend
    // and used in custom transforms such as backannotation
    for (comp <- Driver.sortedComps) {
      comp dfs { _ match {
        case reg: Reg if reg.name == "" =>
          reg setName "R" + reg.component.nextIndex
        case node: Node if !node.isTypeNode && node.name == "" =>
          node.name = "T" + node.component.nextIndex
        case _ =>
      } }
    }
  }

  def fullyQualifiedName( m: Node ): String = {
    m match {
      case l: Literal => l.toString;
      case any       =>
        if (m.name != ""
          && m != Driver.topComponent.defaultResetPin && m.component != null) {
          /* Only modify name if it is not the reset signal
           or not in top component */
          if(m.name != "reset" && m.component != Driver.topComponent) {
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
      case _: Literal =>
        node.name
      case _: Reg =>
        if (node.named) node.name else "R" + node.emitIndex
      case _ =>
        if (node.named) node.name else "T" + node.emitIndex
    }
  }

  def emitRef(c: Module): String = c.name
  def emitDec(node: Node): String = ""

  val transforms = ArrayBuffer[(Module) => Unit]()
  if (Driver.isCSE) transforms += CSE.transform
  val analyses = ArrayBuffer[(Module) => Unit]()

  /** Nodes which are created outside the execution trace from the toplevel
    component constructor (i.e. through the () => Module(new Top()) ChiselMain
    argument) will have a component field set to null. For example, genMuxes,
    forceMatchWidths and transforms (all called from Backend.elaborate) create
    such nodes.

    This method walks all nodes from all component roots (outputs, debugs).
    and reassociates the component to the node both ways (i.e. in Driver.nodes
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

  def collectNodesIntoComp(mod: Module) {
    Driver.sortedComps foreach (_.nodes.clear)
    Driver.dfs { node =>
      val curComp = node match {
        case io: Bits if io.isIo && io.dir == INPUT =>
          node.component.parent
        case _ =>
          node.component
      }
      assert(node.component != null, "NULL NODE COMPONENT " + node.name)
      if (!node.isTypeNode) node.component.nodes += node
      for (input <- node.inputs ; if !input.isTypeNode) {
        if (!(input.component == null || input.component == node.component) &&
            !input.isLit && !isBitsIo(input, OUTPUT) && !isBitsIo(node, INPUT) &&
            // ok if parent referring to any child nodes
            // not symmetric and only applies to direct children
            // READ BACK INPUT -- TODO: TIGHTEN THIS UP
            !isBitsIo(input, INPUT)) {
          ChiselErrors += new ChiselError(() => {
            "Illegal cross module reference between " + node + " and " + input }, node.line)
        }
        if (input.component == null) input.component = curComp
      }
    }
  }

  def checkModuleResolution {
    // check module resolution
    val comps = HashMap[Node, Module]()
    Driver.dfs { node =>
      val nextComp = node match {
        case io: Bits if io.isIo && io.dir == OUTPUT => io.component
        case io: Bits if io.isIo && io.dir == INPUT  => io.component.parent
        case _ => comps getOrElse (node, null)
      }
      for (input <- node.inputs ; if !(input == null)) {
        input match {
          case _: Literal => // Skip the below check for Literals, which can safely be static
          //tmp fix, what happens if multiple componenets reference static nodes?
          case _ if input.component == null || !(Driver.components contains input.component) =>
            /* If Backend.collectNodesIntoComp does not resolve the component
               field for all components, we will most likely end-up here. */
            assert(input.component == nextComp,
              (if (input.name != null && !input.name.isEmpty) input.name else "?") +
              "[" + input.getClass.getName + "] has no match between component " +
              (if (input.component == null ) "(null)" else input.component) +
              " and '" + nextComp + "' input of " +
              (if (node.name != null && !node.name.isEmpty) node.name else "?"))
          case _ =>
        }
        comps(input) = nextComp
      }
    }
  }

  def execute(c: Module, walks: ArrayBuffer[(Module) => Unit]): Unit = {
    for (w <- walks) {
      w(c)
    }
    if (Driver.modAdded) {
      sortComponents
      markComponents
      Driver.modAdded = false
    }
  }

  def pruneUnconnectedIOs(m: Module) {
    val inputs = m.wires.filter(_._2.dir == INPUT)
    val outputs = m.wires.filter(_._2.dir == OUTPUT)

    for ((name, i) <- inputs) {
      if (i.inputs.length == 0 && m != Driver.topComponent)
        if (i.consumers.size > 0) {
          if (Driver.warnInputs)
            ChiselError.warning({"UNCONNECTED INPUT " + emitRef(i) + " in COMPONENT " + i.component +
                                 " has consumers"})
          i.driveRand = true
        } else {
          if (Driver.warnInputs)
            ChiselError.warning({"FLOATING INPUT " + emitRef(i) + " in COMPONENT " + i.component})
          i.prune = true
        }
    }

    for ((name, o) <- outputs) {
      if (o.inputs.length == 0 && !o.component.isInstanceOf[BlackBox]) {
        if (o.consumers.size > 0) {
          if (Driver.warnOutputs)
            ChiselError.warning({"UNCONNECTED OUTPUT " + emitRef(o) + " in component " + o.component +
                                 " has consumers on line " + o.consumers.head.line})
          o.driveRand = true
        } else {
          if (Driver.warnOutputs)
            ChiselError.warning({"FLOATING OUTPUT " + emitRef(o) + " in component " + o.component})
          o.prune = true
        }
      }
    }
  }

  def checkPorts {
    def prettyPrint(n: Node, c: Module) {
      val dir = if (n.asInstanceOf[Bits].dir == INPUT) "Input" else "Output"
      val portName = n.name
      val compName = c.name
      val compInstName = c.moduleName
      ChiselError.warning(dir + " port " + portName
        + " is unconnected in module " + compInstName + " " + compName)
    }

    for (c <- Driver.components ; if c != Driver.topComponent) {
      for ((n,i) <- c.wires) {
        if (i.inputs.length == 0) {
          prettyPrint(i, c)
        }
      }
    }
  }

  def emitDef(node: Node): String = ""

  // go through every Module and set its clock and reset field
  def assignClockAndResetToModules {
    for (module <- Driver.sortedComps.reverse) {
      if (module.clock == null)
        module.clock = module.parent.clock
      if (!module.hasExplicitReset)
        module.reset_=
    }
  }

  // for every reachable delay element
  // assign it a clock and reset where
  // clock is chosen to be the component's clock if delay does not specify a clock
  // reset is chosen to be
  //          component's explicit reset
  //          delay's explicit clock's reset
  //          component's clock's reset
  def addClocksAndResets {
    Driver.bfs {
      _ match {
        case x: Delay =>
          val clock = if (x.clock == null) x.component.clock else x.clock
          val reset =
            if (x.component.hasExplicitReset) x.component._reset
            else if (x.clock != null) x.clock.getReset
            else if (x.component.hasExplicitClock) x.component.clock.getReset
            else x.component._reset
          x.assignReset(x.component.addResetPin(reset))
          x.assignClock(clock)
          x.component.addClock(clock)
         case x: Printf =>
          val clock = if (x.clock == null) x.component.clock else x.clock
          x.assignClock(clock)
          x.component.addClock(clock)
       case _ =>
      }
    }
  }

  def addDefaultResets {
    Driver.components foreach (_.addDefaultReset)
  }

  // go through every Module, add all clocks+resets used in it's tree to it's list of clocks+resets
  def gatherClocksAndResets {
    for (parent <- Driver.sortedComps) {
      for (child <- parent.children) {
        for (clock <- child.clocks) {
          parent.addClock(clock)
        }
        for (reset <- child.resets.keys) {
          // create a reset pin in parent if reset does not originate in parent and
          // if reset is not an output from one of parent's children
          if (reset.component != parent && !parent.children.contains(reset.component))
            parent.addResetPin(reset)

          // special case for implicit reset
          if (reset == Driver.implicitReset && parent == Driver.topComponent)
            if (!parent.resets.contains(reset))
              parent.resets += (reset -> reset)
        }
      }
    }
  }

  def connectResets {
    for (parent <- Driver.sortedComps) {
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
    for (comp <- Driver.sortedComps) {
      for (rst <- comp.resets.keys) {
        if (!comp.resets(rst).named)
            comp.resets(rst).setName(rst.name)
      }
    }
  }

  // walk forward from root register assigning consumer clk = root.clock
  private def createClkDomain(root: Node, walked: HashSet[Node]) = {
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

  def inferAll(mod: Module): Int = {
    // Verify all the muxes just before width inference
    verifyAllMuxes

    val nodesList = ArrayBuffer[Node]()
    Driver.idfs { nodesList += _ }

    def verify {
      var hasError = false
      for (elm <- nodesList) {
        if (elm.infer || elm.width == -1) {
          ChiselError.error("Could not infer the width on: " + elm)
          hasError = true
        }
      }
      if (hasError) throw new Exception("Could not elaborate code due to uninferred width(s)")
    }

    var count = 0
    // Infer all node widths by propagating known widths
    // in a bellman-ford fashion.
    for(i <- 0 until nodesList.length) {
      var nbUpdates = 0
      var done = true;
      for(elm <- nodesList){
        val updated = elm.infer
        if( updated ) { nbUpdates = nbUpdates + 1  }
        done = done && !updated
      }

      count += 1

      if(done){
        verify
        return count;
      }
    }
    verify
    count
  }

  def findConsumers(mod: Module) {
    Driver.bfs (_.consumers.clear)
    Driver.bfs (_.addConsumers)
  }

  def forceMatchingWidths {
    Driver.idfs (_.forceMatchingWidths)
  }

  def computeMemPorts(mod: Module) {
    if (Driver.hasMem) {
      Driver.bfs { _ match {
        case memacc: MemAccess => memacc.referenced = true
        case _ =>
      } }
      Driver.bfs { _ match {
        case mem: Mem[_] => mem.computePorts
        case _ =>
      } }
    }
  }

  /** All classes inherited from Data are used to add type information
   and do not represent logic itself. */
  def removeTypeNodes(mod: Module) = {
    var count = 0
    Driver.bfs {x =>
      // If this a UInt literal, generate a Chisel error.
      // Issue #168 - lit as port breaks chisel
      if (x.isTypeNode) {
        ChiselError.error("Real node required here, but 'type' node found - did you neglect to insert a node with a direction?", x.line)
      }
      count += 1
      for (i <- 0 until x.inputs.length)
        if (x.inputs(i) != null && x.inputs(i).isTypeNode) {
          x.inputs(i) = x.inputs(i).getNode
        }
    }
    count
  }

  def lowerNodes(mod: Module) {
    if (!needsLowering.isEmpty) {
      val lowerTo = new HashMap[Node, Node]
      Driver.bfs { x =>
        for (i <- 0 until x.inputs.length) x.inputs(i) match {
          case op: Op =>
            if (needsLowering contains op.op)
            x.inputs(i) = lowerTo.getOrElseUpdate(op, op.lower)
          case _ =>
        }
      }
      if (!lowerTo.isEmpty)
        inferAll(mod)
    }
  }

  def addBindings {
    for (comp <- Driver.sortedComps) {
      /* This code finds an output binding for a node.
         We search for a binding only if the io is an output
         and the logic's grandfather component is not the same
         as the io's component and the logic's component is not
         same as output's component unless the logic is an input */
      for ((n, io) <- comp.wires) {
        if (io.dir == OUTPUT) {
          for (node <- io.consumers; if !(node == null) && io.component != node.component.parent) {
            for ((input, i) <- node.inputs.zipWithIndex ; if input eq io) {
              node match {
                case bits: Bits if bits.dir == INPUT => {
                  node.inputs(i) = Binding(io, io.component.parent, io.component)
                  node.inputs(i).consumers += node
                }
                case _ if io.component != node.component => {
                  node.inputs(i) = Binding(io, io.component.parent, io.component)
                  node.inputs(i).consumers += node
                }
                case _ =>
              }
            }
          }
        }
        // In this case, we are trying to use the input of a submodule
        // as part of the logic outside of the submodule.
        // If the logic is outside the submodule, we do not use
        // the input name. Instead, we use whatever is driving
        // the input. In other words, we do not use the Input name,
        // if the component of the logic is the part of Input's
        // component. We also do the same when assigning
        // to the output if the output is the parent
        // of the subcomponent.
        else if (io.dir == INPUT && !io.inputs.isEmpty) {
          for (node <- io.consumers; if !(node == null) && io.component.parent == node.component) {
            for ((input, i) <- node.inputs.zipWithIndex ; if input eq io) {
              node match {
                case bits: Bits if bits.dir == OUTPUT => {
                  node.inputs(i) = io.inputs(0)
                  node.inputs(i).consumers -= io
                  node.inputs(i).consumers += node
                }
                case _ if !node.isIo => {
                  node.inputs(i) = io.inputs(0)
                  node.inputs(i).consumers -= io
                  node.inputs(i).consumers += node
                }
                case _ =>
              }
            }
          }
        }
      }
    }
  }

  def nameBindings {
    for (comp <- Driver.sortedComps) {
      for (bind <- comp.bindings) {
        var genName = bind.targetComponent.name + "_" + bind.targetNode.name
        if(nameSpace contains genName) genName += ("_" + bind.emitIndex)
        bind.name = asValidName(genName) // Not using nameIt to avoid override
        bind.named = true
      }
    }
  }

  def findCombLoop {
    // Tarjan's strongly connected components algorithm to find loops
    var sccIndex = 0
    val stack = new Stack[Node]
    val sccList = new ArrayBuffer[ArrayBuffer[Node]]

    def tarjanSCC(n: Node): Unit = {
      if(n.isInstanceOf[Delay]) throw new Exception("trying to DFS on a register")

      n.sccIndex = sccIndex
      n.sccLowlink = sccIndex
      sccIndex += 1
      stack.push(n)

      for(i <- n.inputs) {
        if(!(i == null) && !i.isInstanceOf[Delay] && !i.isReg) {
          if(i.sccIndex == -1) {
            tarjanSCC(i)
            n.sccLowlink = math.min(n.sccLowlink, i.sccLowlink)
          } else if(stack.contains(i)) {
            n.sccLowlink = math.min(n.sccLowlink, i.sccIndex)
          }
        }
      }

      if(n.sccLowlink == n.sccIndex) {
        val scc = new ArrayBuffer[Node]

        var top: Node = null
        do {
          top = stack.pop()
          scc += top
        } while (!(n == top))
        sccList += scc
      }
    }

    Driver.bfs { node =>
      if(node.sccIndex == -1 && !node.isInstanceOf[Delay] && !(node.isReg)) {
        tarjanSCC(node)
      }
    }

    // check for combinational loops
    var containsCombPath = false
    for (nodelist <- sccList) {
      if(nodelist.length > 1) {
        containsCombPath = true
        ChiselError.error("FOUND COMBINATIONAL PATH!")
        for((node, ind) <- nodelist zip nodelist.indices) {
          ChiselError.error("  (" + ind +  ")", node.line)
        }
      }
    }
  }

  /** Prints the call stack of Component as seen by the push/pop runtime. */
  protected def printStack {
    var res = ""
    for((i, c) <- Driver.printStackStruct){
      res += (genIndent(i) + c.moduleName + " " + c.name + "\n")
    }
    ChiselError.info(res)
  }

  def flattenAll {
    // Reset indices for temporary nodes
    Driver.components foreach (_.nindex = -1)

    // Flatten all signals
    val comp = Driver.topComponent
    for (c <- Driver.components ; if c != comp) {
      comp.debugs ++= c.debugs
      comp.nodes ++= c.nodes
    }

    // Find roots
    val roots = ArrayBuffer[Node]()
    for (c <- Driver.components)
      roots ++= c.debugs
    for ((n, io) <- comp.wires)
      roots += io
    for (b <- Driver.blackboxes)
      roots += b.io
    for (node <- comp.nodes) {
      node match {
        case io: Bits if io.dir == OUTPUT && io.consumers.isEmpty =>
          roots += node
        case _: Delay =>
          roots += node
        case _ =>
    } }

    // visit nodes and find ordering
    val stack = Stack[(Int, Node)]()
    val walked = HashSet[Node]()
    for (root <- roots) stack push ((0, root))
    while (!stack.isEmpty) {
      val (depth, node) = stack.pop
      if (depth == -1) Driver.orderedNodes += node
      else {
        node.depth = math.max(node.depth, depth)
        if (!(walked contains node)) {
          walked += node
          stack push ((-1, node))
          for (i <- node.inputs; if !(i == null)) {
            i match {
              case _: Delay =>
              case _ => stack push ((depth + 1, i))
            }
          }
        }
      }
    }
  }

  def findGraphDims: (Int, Int, Int) = {
    val nodes = Driver.orderedNodes.filter(!_.isInstanceOf[Literal])
    val mhist = new HashMap[String, Int]
    val whist = new HashMap[Int, Int]
    val hist = new HashMap[String, Int]
    for (m <- nodes) {
      mhist(m.component.toString) = 1 + mhist.getOrElse(m.component.toString, 0)
      val w = m.needWidth()
      whist(w) = 1 + whist.getOrElse(w, 0)
      val name = m match {
        case op: Op => op.op
        case o      => {
          val name = m.getClass.getName
          name.substring(name.indexOf('.') + 1)
        }
      }
      hist(name) = 1 + hist.getOrElse(name, 0)
    }
    ChiselError.info("%60s %7s".format("module", "node count"));
    for (n <- mhist.keys.toList.sortWith((a, b) => mhist(a) > mhist(b)))
      ChiselError.info("%60s %7d".format(n, mhist(n)))
    ChiselError.info("%12s %7s".format("name", "count"));
    for (n <- hist.keys.toList.sortWith((a, b) => hist(a) > hist(b)))
      ChiselError.info("%12s %7d".format(n, hist(n)))
    ChiselError.info("%5s %s".format("width", "count"));
    for (w <- whist.keys.toList.sortWith((a, b) => a < b))
      ChiselError.info("%5d %7d".format(w, whist(w)))
    val maxDepth = nodes.map(_.depth).foldLeft(0)(_ max _)
    val widths = new Array[Int](maxDepth + 1)
    for (m <- nodes)
      widths(m.depth) += 1
    val maxWidth = widths.foldLeft(0)(_ max _)
    (nodes.length, maxWidth, maxDepth)
  }

  /* Perform a depth first search of the tree for zero-width nodes,
   *  eliminating them if possible.
   */
  def W0Wtransform(): Unit = {
    val nodesList = ArrayBuffer[Node]()
    /* Construct the depth-first list of nodes, set them all to unmodified,
     *  and construct their parent list.
     */
    Driver.idfs { n => { n.modified = false; nodesList += n ; n.inputs.foreach(_.parents += n)} }
    for ( n <- nodesList) {
      // If this node has any zero-width children, have it deal with them.
      if (n.inputs exists {  c => c.inferWidth(c).needWidth == 0 }) {
        n.W0Wtransform()
      }
      // If this node or any of its children have been modified, visit it.
      if (n.modified || (n.inputs exists {  _.modified })) {
        n.review()
      }
    }
  }

  def elaborate(c: Module): Unit = {
    ChiselError.info("// COMPILING " + c + "(" + c.children.length + ")");
    markComponents
    sortComponents

    ChiselError.info("giving names")
    nameAll(c)
    ChiselError.checkpoint()

    ChiselError.info("executing custom transforms")
    execute(c, transforms)
    ChiselError.checkpoint()

    ChiselError.info("adding clocks and resets")
    assignClockAndResetToModules
    addClocksAndResets
    addDefaultResets
    gatherClocksAndResets
    connectResets

    ChiselError.info("inferring widths")
    inferAll(c)
    if (Driver.isSupportW0W) {
      ChiselError.info("eliminating W0W")
      W0Wtransform
    }
    ChiselError.info("checking widths")
    forceMatchingWidths
    ChiselError.info("lowering complex nodes to primitives")
    lowerNodes(c)
    ChiselError.info("removing type nodes")
    val nbNodes = removeTypeNodes(c)
    ChiselError.info("compiling %d nodes".format(nbNodes))
    ChiselError.checkpoint()

    ChiselError.info("computing memory ports")
    computeMemPorts(c)

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
    collectNodesIntoComp(c)
    checkModuleResolution
    ChiselError.checkpoint()

    findConsumers(c)
    // name temporary nodes generated by transforms
    nameAll(c)
    nameRsts

    ChiselError.info("creating clock domains")
    val clkDomainWalkedNodes = new HashSet[Node]
    for (comp <- Driver.sortedComps)
      for (node <- comp.nodes)
        if (node.isInstanceOf[Reg])
          createClkDomain(node, clkDomainWalkedNodes)

    ChiselError.info("pruning unconnected IOs")
    for (comp <- Driver.sortedComps) {
      // remove unconnected outputs
      pruneUnconnectedIOs(comp)
    }

    if (Driver.isCheckingPorts) {
      ChiselError.info("checking for unconnected ports")
      checkPorts
    }

    ChiselError.checkpoint()

    if (!Driver.dontFindCombLoop) {
      ChiselError.info("checking for combinational loops")
      findCombLoop
      ChiselError.checkpoint()
      ChiselError.info("NO COMBINATIONAL LOOP FOUND")
    }

    if (Driver.saveComponentTrace) {
      printStack
    }

    execute(c, analyses)
  }

  def compile(c: Module, flags: String = null): Unit = { }

  // Allow the backend to decide if this node should be recorded in the "object".
  def isInObject(n: Node): Boolean = false
}


