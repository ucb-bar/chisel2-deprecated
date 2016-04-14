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
import scala.collection.mutable.{ArrayBuffer, HashSet, HashMap, LinkedHashMap, Stack, Queue=>ScalaQueue}
import sys.process.stringSeqToProcess

trait FileSystemUtilities {
  /** Ensures a directory *dir* exists on the filesystem. */
  def ensureDir(dir: String) = {
    val file = new java.io.File(dir)
    if (!file.exists) file.mkdirs
    dir
  }

  def createOutputFile(name: String) = {
    val baseDir = ensureDir(Driver.targetDir)
    new java.io.FileWriter(s"${baseDir}/${name}")
  }

  def copyToTarget(filename: String) = {
    val resourceStream = getClass().getResourceAsStream(s"/${filename}")
    if( resourceStream != null ) {
      val classFile = createOutputFile(filename)
      while(resourceStream.available > 0) {
        classFile.write(resourceStream.read())
      }
      classFile.close()
      resourceStream.close()
    } else {
      println(s"WARNING: Unable to copy '$filename'" )
    }
  }

  import scala.util.Properties.envOrElse
  protected val CC = envOrElse("CC", "g++" )
  protected val CXX = envOrElse("CXX", "g++" )
  protected val CCFLAGS = envOrElse("CCFLAGS", "")
  protected val CXXFLAGS = envOrElse("CXXFLAGS", "")
  protected val CPPFLAGS = envOrElse("CPPFLAGS", "")
  protected val LDFLAGS = envOrElse("LDFLAGS", "")
  protected val chiselENV = envOrElse("CHISEL", "")

  def run(cmd: String) = {
    val bashCmd = Seq("bash", "-c", cmd)
    val c = bashCmd.!
    ChiselError.info(cmd + " RET " + c)
    c == 0
  }

  def cc(dir: String, name: String, flags: String = "", isCC: Boolean = false) {
    val compiler = if (isCC) CC else CXX
    val cmd = List(compiler, "-c", "-o", s"${dir}/${name}.o", flags, s"${dir}/${name}.cpp").mkString(" ")
    if (!run(cmd)) throwException("failed to compile " + name + ".cpp")
  }

  def link(dir: String, target: String, objects: Seq[String], isCC: Boolean = false, isLib: Boolean = false) {
    val compiler = if (isCC) CC else CXX
    val shared = if (isLib) "-shared" else ""
    val ac = (List(compiler, LDFLAGS, shared, "-o", s"${dir}/${target}") ++
      (objects map (obj => s"${dir}/${obj}"))).mkString(" ")
    if (!run(ac)) throwException("failed to link " + objects.mkString(", "))
  }
}

class Backend extends FileSystemUtilities{
  /* Set of keywords which cannot be used as node and component names. */
  val keywords = VerilogBackend.keywords
  val nameSpace = HashSet[String]()
  /* Set of Ops that this backend doesn't natively support and thus must be
     lowered to simpler Ops. */
  val needsLowering = Set[String]()

  def topMod = Driver.topComponent getOrElse (throwException("no top component"))

  /* Whether or not this backend decomposes along Module boundaries. */
  def isEmittingComponents: Boolean = false

  private val uniqueSet = HashSet[String]()

  /** Assert that 'uniqueStr' is unique for this backend */
  private[Chisel] def assertUnique(uniqueStr : String, msg : String) {
    // Multiple empty strings are allowed
    if (uniqueStr != "") {
      if (uniqueSet(uniqueStr))
        ChiselError.warning("[BUG] Internal error: " + msg)
      else
        uniqueSet += uniqueStr
    }
  }

  def extractClassName(comp: Module): String = {
    val cname  = comp.getClass().getName().replace("$", "_")
    val dotPos = cname.lastIndexOf('.');
    Driver.moduleNamePrefix + (
      if (dotPos >= 0) cname.substring(dotPos + 1) else cname);
  }

  def sortComponents {
    def levelChildren(root: Module, traversal: Int) {
      root.level = 0
      root.traversal = traversal
      for (child <- root.children) {
        levelChildren(child, traversal + 1)
        root.level = math.max(root.level, child.level + 1)
      }
    }

    def gatherChildren(root: Module): ArrayBuffer[Module] = {
      val result = ArrayBuffer[Module]()
      for (child <- root.children)
        result ++= gatherChildren(child)
      result += root
      result
    }

    levelChildren(topMod, 0)
    Driver.sortedComps.clear
    Driver.sortedComps ++= gatherChildren(topMod).sortWith(
      (x, y) => (x.level < y.level || (x.level == y.level && x.traversal < y.traversal)))
  }

  def markComponents { Driver.sortedComps foreach (_.markComponent) }

  def verifyComponents { Driver.sortedComps foreach (_.verify) }

  def verifyAllMuxes {
    Driver.bfs {
      case p: proc => p.verifyMuxes
      case _ =>
    }
  }

  /* Returns a string derived from _name_ that can be used as a valid
   identifier for the targeted backend. */
  def asValidName( name: String ): String = if (keywords(name)) name + "_" else name

  def nameAll() {
    // Helper classes to get unique names for everything
    class NameSpace {
      private[this] val namespace = scala.collection.mutable.HashSet[String]() // shouldn't need to be a LinkedHashSet
      private[this] def ensureUnique(candidate: String): String = {
        if(namespace(candidate.toLowerCase)) {
          // Use a for comprehension to return all available names (lazily)
          (for{
            idx <- Stream.from(1)
            new_cand = s"${candidate}_${idx}"
            if !namespace(new_cand.toLowerCase)
          } yield new_cand).head // only use the first one
        } else candidate
      }
      // Ignore attempts to reserve an empty ("") name - pr499, issue 459
      def reserveName(name: String): Unit = if (name != "") assert(name == getUniqueName(name), "name " + name + " cannot be reserved")
      def getUniqueName(candidate: String): String = {
        val unique_name = ensureUnique(candidate)
        namespace += unique_name.toLowerCase
        unique_name
      }
    }
    object NameSpace {
      private[this] val namespaces_cache = LinkedHashMap[Module,NameSpace]()
      def apply(target: Module): NameSpace = namespaces_cache.getOrElseUpdate(target, new NameSpace)
    }

    val childrenOfParent = Driver.sortedComps.groupBy(_.parent)
    // Now, go through each module and assign unique names
    for(comp <- Driver.sortedComps) {
      val namespace = NameSpace(comp)

      if(comp.name.isEmpty) comp.name = extractClassName(comp)
        // ensure this component has a name
      val children = childrenOfParent.getOrElse(comp, Seq.empty)
      assert(children.filter(_.name.isEmpty).isEmpty, ChiselError.error("Internal Error: Unnamed Children"))
        // since sortedComps, all children should have names due to check above

      // ensure all nodes in the design has SOME name
      comp dfs {
        case reg: Reg if reg.name.isEmpty =>
          reg setName "R" + reg.component.nextIndex
        case mem: Mem[_] if mem.name.isEmpty =>
          mem setName "T" + mem.component.nextIndex
        case node: Node if !node.isTypeNode && node.name.isEmpty && node.compOpt != None =>
          node.name = "T" + node.component.nextIndex
        case _ =>
      }

      // Now, ensure everything has a UNIQUE name
      // First, reserve all the IO names
      comp.io.flatten map(_._2) foreach(n => namespace.reserveName(n.name))
      // TODO: Reserve other things like clock, reset, etc.
      // Second, give module instances high priority for names
      children.foreach(c => c.name = namespace.getUniqueName(c.name))
      // Then, check all other nodes in the design
      comp dfs {
        case reg: Reg =>
          reg setName namespace.getUniqueName(reg.name)
        case mem: Mem[_] =>
          mem setName namespace.getUniqueName(mem.name)
        case node: Node if !node.isTypeNode && !node.isLit && !node.isIo => {
          // the isLit check should not be necessary
          // the isIo check is also strange and happens because parents see child io in the DFS
          node.name = namespace.getUniqueName(node.name)
        }
        case _ =>
      }
    }
  }

  /** Ensures each node such that it has a unique name across the whole
    hierarchy by prefixing its name by a component path (except for "reset"
    and all nodes in *c*). */
  def renameNodes(nodes: Seq[Node], sep: String = "_") = nodes foreach {
    case _: Literal =>
    case m if m.named => m.compOpt match {
      case Some(p) if (p != topMod || m.name != "reset" || m.name != Driver.implicitReset.name) =>
        m.name = m.component.getPathName(sep) + sep + sep + m.name
      case _ =>
    }
    case _ =>
  }

  def fullyQualifiedName( m: Node ): String = m match {
    case l: Literal => l.toString;
    case _ if m.name != "" && m.name != "reset" => m.compOpt match {
      case Some(p) if p != topMod =>
        /* Only modify name if it is not the reset signal or not in top component */
        m.component.getPathName + "__" + m.name
      case _ => m.name
    }
    case _ => m.name
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
    Driver.dfs { node => node.compOpt match {
      case None => throwException("NULL NODE COMPONENT " + node.name)
      case Some(p) =>
        val curComp = node match {
          case io: Bits if io.isIo && io.dir == INPUT => p.parent
          case _ => p
        }
        if (!node.isTypeNode) p.nodes += node
        node.inputs filterNot (_.isTypeNode) foreach (input => input.compOpt match {
          case None => input.compOpt = Some(curComp)
          case Some(q) => if (p != q && !input.isLit &&
            !isBitsIo(input, OUTPUT) && !isBitsIo(node, INPUT) &&
            // ok if parent referring to any child nodes
            // not symmetric and only applies to direct children
            // READ BACK INPUT -- TODO: TIGHTEN THIS UP
            !isBitsIo(input, INPUT)) {
            ChiselError.error(new ChiselError(() => {
             "Illegal cross module reference between " + node + " and " + input }, node.line))
          }
        })
      }
    }
  }

  def checkModuleResolution {
    // check module resolution
    val comps = HashMap[Node, Module]()
    val compSet = Driver.components.toSet
    Driver.dfs { node =>
      val nextComp = node match {
        case io: Bits if io.isIo && io.dir == OUTPUT => io.component
        case io: Bits if io.isIo && io.dir == INPUT  => io.component.parent
        case _ => comps getOrElse (node, null)
      }
      node.inputs foreach {
        case _: Literal => // Skip the below check for Literals, which can safely be static
        //tmp fix, what happens if multiple componenets reference static nodes?
        case input: Node => input.compOpt match {
          case Some(p) if compSet(p) =>
          case _ => assert(input.component == nextComp,
            /* If Backend.collectNodesIntoComp does not resolve the component
               field for all components, we will most likely end-up here. */
            ChiselError.error("Internal Error: " + (if (!input.name.isEmpty) input.name else "?") +
            "[" + input.getClass.getName + "] has no match between component " +
            ((input.compOpt map (_.toString)) getOrElse "(null)") +
            " and '" + nextComp + "' input of " + (if (!node.name.isEmpty) node.name else "?")))
        }
      }
      comps ++= node.inputs map (_ -> nextComp)
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

  def pruneUnconnectedIOs = for (m <- Driver.sortedComps) {
    val (inputs, outputs) = m.wires.unzip._2 partition (_.dir == INPUT)
    if (m != topMod) {
      for (i <- inputs if i.inputs.isEmpty) {
        if (i.consumers.isEmpty) {
          if (Driver.warnInputs)
            ChiselError.warning("FLOATING INPUT " + emitRef(i) + " in COMPONENT " + i.component)
          i.prune = true
        } else {
          if (Driver.warnInputs)
            ChiselError.warning("UNCONNECTED INPUT " + emitRef(i) + " in COMPONENT " +
                                i.component + " has consumers")
          i.driveRand = true
        }
      }
    }
    m match {
      case _: BlackBox =>
      case _ => for (o <- outputs if o.inputs.isEmpty) {
        if (o.consumers.isEmpty) {
          if (Driver.warnOutputs)
            ChiselError.warning("FLOATING OUTPUT " + emitRef(o) + " in component " + o.component)
          o.prune = true
        } else {
          if (Driver.warnOutputs)
            ChiselError.warning("UNCONNECTED OUTPUT " + emitRef(o) + " in component " +
                                o.component + " has consumers")
          o.driveRand = true
        }
      }
    }
  }

  def checkPorts {
    for {
      c <- Driver.sortedComps
      if c != topMod
      n <- c.wires.unzip._2
      if n.inputs.isEmpty
    } {
      val portName = n.name
      val compName = c.name
      val compInstName = c.moduleName
      ChiselError.warning(n.dir + " port " + portName
        + " is unconnected in module " + compInstName + " " + compName)
    }
  }

  def emitDef(node: Node): String = ""

  // go through every Module and set its clock and reset field
  def assignClockAndResetToModules {
    for (module <- Driver.sortedComps.reverse) {
      if (module._clock == None)
        module._clock = module.parent._clock
      if (!module.hasExplicitReset)
        module._reset = module.parent._reset
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
      case x: Delay =>
        val clock = x.clock getOrElse x.component._clock.get
        val reset =
          if (x.clock != None) x.clock.get.getReset
          else if (x.component.hasExplicitReset) x.component._reset.get
          else if (x.component.hasExplicitClock) x.component._clock.get.getReset
          else x.component._reset.get
        x.assignReset(x.component addResetPin reset)
        x.assignClock(clock)
        x.component.addClock(clock)
      case _ =>
    }
  }

  def addDefaultResets {
    Driver.sortedComps foreach (_.addDefaultReset)
  }

  // go through every Module, add all clocks+resets used in it's tree to it's list of clocks+resets
  def gatherClocksAndResets {
    for (parent <- Driver.sortedComps ; childSet = parent.children.toSet ; child <- parent.children) {
      child.clocks foreach (parent addClock _)
      for (reset <- child.resets.keys) {
        // create a reset pin in parent if reset does not originate in parent and
        // if reset is not an output from one of parent's children
        if (reset.component != parent && !childSet(reset.component))
          parent addResetPin reset
        // special case for implicit reset
        if (reset == Driver.implicitReset && parent == topMod)
          parent.resets getOrElseUpdate (reset, reset)
      }
    }
  }

  def connectResets {
    for {
      parent <- Driver.sortedComps
      child <- parent.children
      reset <- child.resets.keys
      pin = child.resets(reset) if pin.inputs.isEmpty
    } {
      pin := (parent.resets getOrElse (reset, reset))
    }
  }

  def nameRsts {
    for (comp <- Driver.sortedComps ; rst <- comp.resets.keys ; if !comp.resets(rst).named) {
      comp.resets(rst) setName rst.name
    }
  }

  // walk forward from root register assigning consumer clk = root.clock
  private def createClkDomain(root: Reg, walked: HashSet[Node]) = {
    val dfsStack = Stack[Node]()
    walked += root
    dfsStack.push(root)
    val clock = root.clock getOrElse (throwException(s"Reg(${root.name} in ${extractClassName(root.component)}) should have its own clock"))
    while(!dfsStack.isEmpty) {
      val node = dfsStack.pop
      node.consumers filterNot walked foreach {
        case _: Delay =>
        case consumer => consumer.clock match {
          case Some(clk) if clk != clock =>
            ChiselError.warning(consumer.getClass + " " + emitRef(consumer) + " " + emitDef(consumer) +
                                "in module" + consumer.component + " resolves to clock domain " +
                                emitRef(clk) + " and " + emitRef(clock) + " traced from " + root.name)
          case _ => consumer.clock = Some(clock)
        }
        walked += consumer
        dfsStack.push(consumer)
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
      if (hasError) throwException("Could not elaborate code due to uninferred width(s)")
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
      if(done){ verify ; return count }
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

  def convertMaskedWrites(mod: Module) {
    Driver.bfs {
      case mem: Mem[_] => mem.convertMaskedWrites
      case _ =>
    }
  }

  def computeMemPorts(mod: Module) {
    Driver.bfs {
      case memacc: MemAccess => memacc.referenced = true
      case _ =>
    }
    Driver.bfs {
      case mem: Mem[_] => mem.computePorts
      case _ =>
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
      for ((input, i) <- x.inputs.zipWithIndex if input.isTypeNode) {
        x.inputs(i) = input.getNode
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
            if (needsLowering(op.op))
            x.inputs(i) = lowerTo.getOrElseUpdate(op, op.lower)
          case _ =>
        }
      }
      if (!lowerTo.isEmpty) inferAll(mod)
    }
  }

  def addBindings {
    // IMPORTANT INVARIANT FOR THIS FUNCTION
    //   After every graph manipulation, consumers for each node MUST be kept consistent!
    for (comp <- Driver.sortedComps) {
      // For a module input or output, logic needs to be adjusted if
      //   the consumer is NOT in a direct submodule
      //     (likely as an input since as output/internal wire is illegal cross-module reference)
      //  AND
      //   consumer is either in a different module (including submodule) OR is an input
      //     (since, in the input case, the connection is really being done in the parent)
      for ((n, io) <- comp.wires) {
        // OUTPUT logic is adjusted by creating a Binding node in parent
        //   and then having all subsequent logic use that
        if (io.dir == OUTPUT) {
          // IMPORTANT: the toSet call makes an immutable copy, allows for manipulation of io.consumers
          for (node <- io.consumers.toSet; if io.component != node.component.parent) {
            if(node match {
              case _ if io.component != node.component => true
              case b: Bits if (b.dir == INPUT) && io.component == node.component => true
              case _ => false
            }) {
              for ((input, i) <- node.inputs.zipWithIndex ; if input eq io) {
                // note: after this redirection: node.inputs(i) == io
                node.inputs(i).consumers -= node
                node.inputs(i) = Binding(io, io.component.parent, io.component)
                node.inputs(i).consumers += node
              }
            }
          }
        }
        // INPUT logic is adjusted by having consumers use that input's producer
        else if (io.dir == INPUT && !io.inputs.isEmpty) {
          // IMPORTANT: the toSet call makes an immutable copy, allows for manipulation of io.consumers
          for (node <- io.consumers.toSet; if io.component != node.component.parent) {
            if(node match {
              case _ if io.component != node.component => true
              case b: Bits if (b.dir == INPUT) && io.component == node.component => true
              case _ => false
            }) {
              for ((input, i) <- node.inputs.zipWithIndex ; if input eq io) {
                // note: after this redirection: node.inputs(i) == io
                node.inputs(i).consumers -= node
                node.inputs(i) = io.inputs(0)
                node.inputs(i).consumers += node
              }
            }
          }
        }
      }
    }
  }

  def nameBindings {
    for (comp <- Driver.sortedComps ; bind <- comp.bindings) {
      var genName = bind.targetComponent.name + "_" + bind.targetNode.name
      if (nameSpace(genName)) genName += ("_" + bind.emitIndex)
      bind.name = asValidName(genName) // Not using nameIt to avoid override
      bind.named = true
    }
  }

  def findCombLoop {
    // Tarjan's strongly connected components algorithm to find loops
    var index = 0
    val stack = Stack[Node]()
    val onStack = HashSet[Node]()
    val sccs = ArrayBuffer[List[Node]]()

    def tarjanSCC(node: Node): Unit = node match {
      case _: Delay => throwException("trying to DFS on a register")
      case _ =>

      node.sccIndex = Some(index)
      node.sccLowlink = Some(index)
      index += 1
      stack.push(node)
      onStack += node

      node.inputs foreach {
        case _: Delay =>
        case input if input.sccIndex == None =>
          tarjanSCC(input)
          node.sccLowlink = Some(math.min(node.sccLowlink getOrElse -1, input.sccLowlink getOrElse -1))
        case input if onStack(input) =>
          node.sccLowlink = Some(math.min(node.sccLowlink getOrElse -1, input.sccIndex getOrElse -1))
        case _ =>
      }

      if (node.sccLowlink == node.sccIndex) {
        val scc = ArrayBuffer[Node]()
        do {
          scc += stack.pop
          onStack -= scc.last
        } while (node ne scc.last)
        sccs += scc.toList
      }
    }

    Driver.bfs {
      case _: Delay =>
      case node if node.sccIndex == None => tarjanSCC(node)
      case _ =>
    }

    // check for combinational loops
    sccs filter (_.size > 1) foreach {scc =>
      ChiselError.error("FOUND COMBINATIONAL PATH!")
      scc.zipWithIndex foreach { case (node, i) =>
        ChiselError.error(s"  (${i}: ${node.component.getPathName(".")}.${node.name})", node.line) }}
  }

  /** Prints the call stack of Component as seen by the push/pop runtime. */
  protected def genIndent(x: Int): String = (for (i <- 0 until x) yield "    ").mkString
  protected def printStack {
    ChiselError.info(Driver.printStackStruct map {
      case (i, c) => "%s%s %s\n".format(genIndent(i), c.moduleName, c.name)
    } mkString "")
  }

  def flattenAll {
    // Reset indices for temporary nodes
    Driver.components foreach (_.nindex = None)

    // Flatten all signals
    val nodes = Driver.components flatMap (_.nodes)
    val debugs = Driver.components flatMap (_.debugs)

    // Find roots
    val roots = debugs ++
      (topMod.wires map (_._2)) ++
      (Driver.blackboxes map (_.io)) ++
      (nodes filter {
        case io: Bits => io.dir == OUTPUT && io.consumers.isEmpty
        case _: Delay => true
        case _ => false
      })

    // visit nodes and find ordering
    val stack = Stack[(Node, Option[Int])]((roots.reverse map ((_, Some(0)))):_*)
    val walked = HashSet[Node]()
    while (!stack.isEmpty) {
      stack.pop match {
        case (node, None) => Driver.orderedNodes += node
        case (node, Some(depth)) => {
          node.depth = math.max(node.depth, depth)
          if (!walked(node)) {
            walked += node
            stack push ((node, None))
            node.inputs foreach {
              case _: Delay =>
              case i => stack push ((i, Some(depth + 1)))
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
    Driver.idfs { n => { n.modified = false; nodesList += n ; n.inputs.foreach(_.consumers += n)} }
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
    ChiselError.checkpoint()
    ChiselError.info("// COMPILING " + c + "(" + c.children.size + ")");
    sortComponents
    markComponents

    verifyComponents
    ChiselError.checkpoint()

    // Ensure all conditional assignments have defauts.
    verifyAllMuxes
    ChiselError.checkpoint()

    ChiselError.info("giving names")
    nameAll
    ChiselError.checkpoint()

    ChiselError.info("executing custom transforms")
    execute(c, transforms)
    ChiselError.checkpoint()

    ChiselError.info("convert masked writes of inline mems")
    convertMaskedWrites(c)

    ChiselError.info("adding clocks and resets")
    assignClockAndResetToModules
    addClocksAndResets
    addDefaultResets
    gatherClocksAndResets
    connectResets

    ChiselError.info("inferring widths")
    inferAll(c)
    if (Driver.isSupportW0W) {
      ChiselError.info("eliminating W0W (pre width check)")
      W0Wtransform
    }
    ChiselError.info("checking widths")
    forceMatchingWidths
    if (Driver.isSupportW0W) {
      ChiselError.info("eliminating W0W (post width check)")
      W0Wtransform
    }
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
    nameAll
    nameRsts

    ChiselError.info("creating clock domains")
    val clkDomainWalkedNodes = HashSet[Node]()
    for (comp <- Driver.sortedComps ; node <- comp.nodes) {
      node match {
        case r: Reg => createClkDomain(r, clkDomainWalkedNodes)
        case _ =>
      }
    }

    ChiselError.info("pruning unconnected IOs")
    pruneUnconnectedIOs

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

    // generate chisel names
    Driver.dfs { _.chiselName }
    execute(c, analyses)
  }

  def compile(c: Module, flags: Option[String] = None): Unit = { }

  // Allow the backend to decide if this node should be recorded in the "object".
  def isInObject(n: Node): Boolean = false
}


