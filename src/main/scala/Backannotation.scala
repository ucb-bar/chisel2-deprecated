package Chisel

import Chisel._
import Module._
import Node._
import Backend._
import ChiselError._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack
import scala.io.Source
import scala.sys.process._

trait Backannotation extends Backend {
  preElaborateTransforms += ((c: Module) => levelChildren(c))
  preElaborateTransforms += ((c: Module) => 
    Module.sortedComps = gatherChildren(c).sortWith(
      (x, y) => (x.level < y.level || 
          (x.level == y.level && x.traversal < y.traversal) ) ) )
  preElaborateTransforms += ((c: Module) => c.inferAll)
  preElaborateTransforms += ((c: Module) => c.forceMatchingWidths)
  preElaborateTransforms += ((c: Module) => collectNodesIntoComp(initializeDFS))
  preElaborateTransforms += ((c: Module) => nameAll(c))
  preElaborateTransforms += ((c: Module) => getNodeIndices(c))

  protected def getParentNames(m: Module, delim: String = "/"): String = {
    if (m == Module.topComponent) ""
    else getParentNames(m.parent) + emitRef(m.parent) + delim
  }

  protected def getSignalName(n: Node, delim: String = "/"): String = {
    if (n == null) "null" else getParentNames(n.componentOf, delim) + emitTmp(n)
  }
}

trait SignalBackannotation extends Backannotation {
  // For signal backannotation
  preElaborateTransforms += ((c: Module) => annotateCrosses(c))
  preElaborateTransforms += ((c: Module) => printCrosses(Module.crosses, c.name + "_crosses.rpt"))

  private def readCrosses(filename: String): ArrayBuffer[(Double, Array[String])] = {
    val lines: Iterator[String] = Source.fromFile(filename).getLines
    val TermRegex = """\s*([\w\._\:]+)\s+([\d\.\+-e]+)\s+[\d\.\+-e]+\s+[\d\.\+-e]+\s+[\d\.\+-e]+""".r
   
    (lines foldLeft (ArrayBuffer[(Double, Array[String])]())) {
      case (res: ArrayBuffer[(Double, Array[String])], TermRegex(exp, coeff)) => {
        val vars = exp split ':'
        // ChiselError.info(coeff + ", " + (vars.head /: vars.tail) {_ + ", " + _})
        if (coeff.toDouble != 0.0) res += ((coeff.toDouble, vars))
        res
      }
      case (res: ArrayBuffer[(Double, Array[String])], _) => res
    }
  }

  private def annotateCrosses(m: Module): Unit = {
    ChiselError.info("Donggyu: annotate signal crosses")
    
    val crosses = readCrosses(Module.crossFilename)

    def getNode(variable: Array[String]): Node = {
      val parentNames = variable.init
      val nodeName = variable.last
 
      def checkParents(names: Array[String], module: Module): Boolean = {
        if (names.length == 1 && module.parent == Module.topComponent) true
        else if (emitRef(module) == names.last) checkParents(names.init, module.parent)
        else false
      }

      val comps = for {
        m <- Module.components
        if checkParents(parentNames, m)
      } yield m

      val nodes = for {
        m <- comps; n <- m.nodes
        if emitTmp(n) == nodeName  
      } yield n

      // TODO: Are clocks included in the future?
      /*
      val clks = for {
        m <- comps
        if m.clock != null && emitTmp(m.clock) == nodeName
      } yield m.clock
      */

      val resets = for {
        m <- comps
        if emitTmp(m.reset) == nodeName
      } yield m.reset

      if (!nodes.isEmpty) nodes.head
      // else if (!clks.isEmpty) clks.head
      else if (!resets.isEmpty) resets.head
      else null
    }

    def getTerm(coeff: Double, exp: Array[String])  = {
      val term = for { v <- exp } yield getNode( v split '.' )
      val (busses, signals) = term partition (x => x != null && x.width > 1)

      // val termToString = (printSignal(term.head) /: term.tail) { _ + " * " +  printSignal(_) }
      // ChiselError.info("term: " + coeff.toString + " * " +  termToString)

      (coeff, signals, busses)
    }

    Module.crosses ++= crosses map { 
      case (coeff: Double, exp: Array[String]) => {
        getTerm(coeff, exp)
      }
    }
  }
t
t

  private def printCrosses(crosses: ArrayBuffer[(Double, Array[Node], Array[Node])], filename: String = "crosses.rpt"): Unit = {
    val basedir = ensureDir(Module.targetDir)
    val rptdir  = ensureDir(basedir+"report")
    val rptfile = new java.io.FileWriter(rptdir+filename) 
    val report = new StringBuilder();

    def printSignal(node: Node): String = {
      if (node == null) "null"
      else if (node.width > 1) "Bus(" + getSignalName(node, ".") + ")"
      else "Signal(" + getSignalName(node, ".") + ")"
    }

    ChiselError.info("Donggyu: print out signal crosses")

    report.append("\t\t+--------------------------------+\n")
    report.append("\t\t|        Signal Crosses          |\n")
    report.append("\t\t|                   by Donggyu   |\n")
    report.append("\t\t+--------------------------------+\n\n")

    for ((coeff, signals, busses) <- crosses) {
      val signalsToString = {
        if (signals.isEmpty) ""
        else " * " + (printSignal(signals.head) /: signals.tail) { _ + " * " + printSignal(_) }
      }

      val bussesToString = {
        if (busses.isEmpty) ""
        else " * " + (printSignal(busses.head) /: busses.tail) { _ + " * " + printSignal(_) }
      }

      report.append(coeff.toString +  signalsToString + bussesToString + "\n")
    }

    // ChiselError.info(report)
    // Write the signals into the file
    try {
      rptfile.write(report.toString)
    } finally {
      rptfile.close()
    }
  }
}

trait DelayBackannotation extends Backannotation {
  preElaborateTransforms += ((c: Module) => c.removeTypeNodes)
  preElaborateTransforms += ((c: Module) => generateTcl(c))
  preElaborateTransforms += ((c: Module) => executeDC())
  preElaborateTransforms += ((c: Module) => annotateDelay(c, c.name + "_timing.rpt"))
  preElaborateTransforms += ((c: Module) => calcCriticalPathDelay())

  private def getTimingPaths(stack: Stack[Node]): ArrayBuffer[List[Node]] = {
    def insertToMultimap(multimap: HashMap[Node, HashSet[List[Node]]], node: Node, list: List[Node]) {
      if (!(multimap contains node)) multimap(node) = new HashSet[List[Node]]
      multimap(node) += list
    }

    def printTimingPaths(paths: ArrayBuffer[List[Node]]) {
      for (path <- paths) {
        ChiselError.info(
          (getSignalName(path.head) /: path.tail) ( _ + " -> " + getSignalName(_))
        )
      }
    }

    val paths = new ArrayBuffer[List[Node]]() // Timing paths
    val tailpaths = new HashMap[Node, HashSet[List[Node]]]() // Multimap for tail paths
    val walked = new HashSet[Node]() // Set of walked nodes (DFS)
    walked ++= stack

    // do BFS
    while (!stack.isEmpty) {
      val node = stack.pop

      node match {
        // OUTPUT PORT: the end point of a timing path
        // => initialize timing paths
        case bits: Bits if bits.dir == OUTPUT => 
          insertToMultimap(tailpaths, node, Nil)
        // REGISTER: the end point of a timing path
        // => initialize timing paths
        case _: Reg => 
          insertToMultimap(tailpaths, node, Nil)
        case _ =>
      }

      val tails = tailpaths getOrElse (node, new HashSet[List[Node]]())

      for (input <- node.inputs) {
        input match {
          // INPUT PORT: the start point of a timing path
          // => return timing paths 
          case bits: Bits if bits.dir == INPUT =>
            for (tail <- tails) {
              paths += (input::node::tail)
            }
          // REGISTER: the start point of a timing path
          // => return timing paths
          case _: Reg => 
            for (tail <- tails) {
              paths += (input::node::tail)
            }
          // Other nodes => construct timing paths though this node
          case _ => {
            for (tail <- tails) {
              insertToMultimap(tailpaths, input, node::tail)
            } 
          }
        }

        // Not walked => should be visited later
        if (!(walked contains input)) {
          walked += input
          stack push input
        }
        // When all the input's coumsumers are not visited, the input node should be visited later
        else if ((input.consumers foldLeft false) { (x, y) =>  x || !(walked contains y) }) {
          stack push input
        }
      }
    }

    // printTimingPaths(paths) // for debug
    paths
  }

  private def generateTcl(m: Module, filename: String = "gentcl.tcl") {
    val basedir = ensureDir(Module.targetDir)
    val dcsyndir  = ensureDir(basedir+"dc-syn")
    val tcl = new StringBuilder();
    val tclfile = new java.io.FileWriter(dcsyndir+filename)
    val rpt_filename = m.name + "_timing.rpt"

    ChiselError.info("Donggyu: generate a tcl file")
    tcl.append("""set ucb_vlsi_home [getenv UCB_VLSI_HOME]"""+"\n")
    tcl.append("""set stdcells_home $ucb_vlsi_home/stdcells/synopsys-32nm/typical_rvt"""+"\n")
    tcl.append("""set search_path "$stdcells_home/db $ucb_vlsi_home/install/vclib ../" """+"\n")
    tcl.append("""set target_library "cells.db" """+"\n")
    // tcl.append("""set synthetic_library "dw_foundation.sldb" """+"\n")
    tcl.append("""set synthetic_library "standard.sldb" """+"\n")
    tcl.append("""set link_library "* $target_library $synthetic_library" """+"\n")
    tcl.append("""set alib_library_analysis_path "alib" """+"\n\n")

    tcl.append("analyze -format verilog " + m.name + ".v\n")
    tcl.append("elaborate " + m.name + "\n")
    tcl.append("link\n")
    tcl.append("check_design\n")
    tcl.append("create_clock clk -name ideal_clock1 -period 1\n")
    // tcl.append("define_name_rules backannotation -dont_change_bus_members -dont_change_ports\n")
    // tcl.append("change_names -rules backannotation\n")

    // set_dont_touch for signal preserving
    /*
    m bfs { node => 
      node match {
        case _: Op => {
          tcl.append("set_dont_touch " + getSignalName(node) + "\n")
        }
        case _ =>
      }
    }
    */

    tcl.append("set hdlin_infer_mux all\n")
    tcl.append("set hdlin_keep_signal_name all\n")
    // tcl.append("compile -no_design_rule\n\n")
    // tcl.append("compile -only_design_rule\n\n")
    tcl.append("compile -only_hold_time\n\n")

   
    var first_report = true

    def cmdhead = 
      if (first_report) { 
        first_report = false 
        "redirect " + rpt_filename + " { " 
      } else { 
        "redirect -append " + rpt_filename + " { " 
      }
    val cmdopts = " -significant_digits 5 -path only"
    val cmdtail = " }\n"

    tcl.append("report_timing %s > %s_critical.rpt\n".format(cmdopts, m.name))

    val paths = getTimingPaths(initializeDFS)
 
    def genReports(from: String, to: String, via: List[Node]) {
      def powerset (list: List[Node]): Set[Set[Node]] = {
        list match {
          case Nil => Set(Set[Node]())
          case head::tail => (powerset(tail) map { Set(head) ++ _ }) ++ powerset(tail)
        }
      }

      def reports (sets: Set[Set[Node]]) {
        def condcmds(via: Set[Node]) = {
          def cond(node: Node) = "[get_nets " + getSignalName(node) + " ] != \"\""
          def conds = (cond(via.head) /: via.tail) (_ + " && " + cond(_))
          def throughs = ("" /: via) (_ + " -through " + getSignalName(_))

          if (via.isEmpty) 
            "{\n\t" + cmdhead + "report_timing" + from + to + cmdopts + cmdtail + "}\n"
          else
            "{ " + conds + " } " +
            "{\n\t" + cmdhead + "report_timing" + from + throughs + to + cmdopts + cmdtail + "}"
        }

        if (sets.size <= 1) {
          tcl.append(cmdhead + "report_timing" + from + to + cmdopts + cmdtail)
        } else {
          val head = sets.head
          val mid = sets.tail.init
          val last = sets.last
          tcl.append(
            "if " + condcmds(head) +
            (("" /: mid) (_ + " elseif " + condcmds(_))) +
            " else " + condcmds(last)
          )
        }
      }
  
      val throughSets = powerset(via).sortWith(_.size >= _.size)
      /*
      for (throughs <- throughSets) {
        if (throughs.isEmpty) {
          ChiselError.info("{}")
        }
        else {
          ChiselError.info(
            "{ " + ((getSignalName(throughs.head) /: throughs.tail) (_ + ", " + getSignalName(_))) + " }"
          )
        }
      }
      */

      reports(throughSets)      
    }
 
    for (path <- paths) {
      val pathToString = (getSignalName(path.head) /: path.tail) ( _ + " -> " + getSignalName(_))
      tcl.append(cmdhead + "echo \"\\n" + pathToString + "\"" + cmdtail)
      // ChiselError.info(pathToString)

      val start = path.head
      val via = path.tail.init
      val end = path.last

      // val thoughs = ("" /: via) (_ + " -through " + getSignalName(_))
      
      (start, end) match {
        case (_: Reg, _: Reg) => {
          val from = " -from [get_pins " + getSignalName(start) + "_reg*/CLK]"
          val to = " -to [get_pins " + getSignalName(end) + "_reg*/D]"
          genReports(from, to, via)
        }
        case (_: Reg, bits: Bits) if bits.dir == OUTPUT => {
          val from = " -from [get_pins " + getSignalName(start) + "_reg*/CLK]"
          val to = " -to " + getSignalName(end)
          genReports(from, to, via)
        }
        case (bits: Bits, _: Reg) if bits.dir == INPUT => {
          val from = " -from " + getSignalName(start)
          val to = " -to [get_pins " + getSignalName(end) + "_reg*/D]"
          genReports(from, to, via)
        }
        case (input: Bits, output: Bits) if input.dir == INPUT && output.dir == OUTPUT => {
          val from = " -from " + getSignalName(start)
          val to = " -to " + getSignalName(end)
          genReports(from, to, via)
        }
        case (_, _) =>
      }
    }

    tcl.append("\nwrite -f verilog -hierarchy -output " + m.name + ".mapped.v\n\n")
    tcl.append("exit\n")

    // ChiselError.info(tcl.toString) 

    try {
      tclfile.write(tcl.toString)
    } finally {
      tclfile.close()
    }
  }

  private def executeDC(filename: String = "gentcl.tcl") = {
    val basedir = ensureDir(Module.targetDir)
    val dcsyndir  = ensureDir(basedir+"dc-syn")
    ChiselError.info("Donggyu: start Design Compiler")
    val cmd = "make dc"
    // val cmd = "dc_shell -64bit -f " + dcsyndir + filename
    val c = Process(cmd).!
    ChiselError.info("Donggyu: finish Design Compiler")
  }

  private def annotateDelay(m: Module, filename: String = "timing.rpt") {
    ChiselError.info("Donggyu: annotate delays")
    val basedir = ensureDir(Module.targetDir)
    val dcsyndir  = ensureDir(basedir+"dc-syn")
    val lines: Iterator[String] = Source.fromFile(dcsyndir+filename).getLines
    val PathRegex = """(\w+(?: -> \w+)+).*""".r
    val RegclkRegex = """\s*([\w/_]+)_reg(?:\[\d+\])?/CLK \(([\w_]+)\)(?: <-)?\s+(\d+\.\d+)\s+(\d+\.\d+).*""".r
    val ReginRegex = """\s*([\w/_]+)_reg(?:\[\d+\])?/D \(([\w_]+)\)(?: <-)?\s+(\d+\.\d+)\s+(\d+\.\d+).*""".r
    val RegoutRegex = """\s*([\w/_]+)_reg(?:\[\d+\])?/Q \(([\w_]+)\)(?: <-)?\s+(\d+\.\d+)\s+(\d+\.\d+).*""".r
    val PinRegex = """\s*([\w/_]+) \(([\w_]+)\)(?: <-)?\s+(\d+\.\d+)\s+(\d+\.\d+).*""".r
    val NetRegex = """\s*([\w/_]+)(?:\[\d+\])? \(net\)\s+(\d+\.\d+)\s+(\d+\.\d+).*""".r
    val PortinRegex = """\s*([\w/_]+)(?:\[\d+\])? \(in\)\s+(\d+\.\d+)\s+(\d+\.\d+).*""".r
    val PortoutRegex = """\s*([\w/_]+)(?:\[\d+\])? \(out\)\s+(\d+\.\d+)\s+(\d+\.\d+).*""".r
    val arrivalmap = new HashMap[String, Double]
    val delaymap = new HashMap[String, Double]
    var points: Array[String] = null
    var pointIndex = 0
 
    for (line <- lines) {
      line match {
        case PathRegex(path) => {
          // ChiselError.info("\npath: %s".format(path))
          points = path.split(" -> ")
          pointIndex = 0
        }
        case PortinRegex(in, incr, path) => {
          // ChiselError.info("in: %s %s %s".format(in, incr, path))
          assert(in == points.head)
          val arrival = arrivalmap getOrElse (in, 0.0)
          if (arrival < path.toDouble) 
            arrivalmap(in) = path.toDouble
        }
        case PortoutRegex(out, incr, path) => {
          // ChiselError.info("out: %s %s %s".format(out, incr, path))
          assert(out == points.last)
          val arrival = arrivalmap getOrElse (out, 0.0)
          if (arrival < path.toDouble) 
            arrivalmap(out) = path.toDouble
        }
        case ReginRegex(reg, ref, incr, path) => {
          // ChiselError.info("reg/D: %s %s %s %s".format(reg, ref, incr, path))
          assert(reg == points.last)
          val arrival = arrivalmap getOrElse (reg, 0.0)
          if (arrival < path.toDouble) 
            arrivalmap(reg) = path.toDouble
        }
        case RegoutRegex(reg, ref, incr, path) => {
          // ChiselError.info("reg/Q: %s %s %s %s".format(reg, ref, incr, path))
          assert(reg == points.head)
          val delay = delaymap getOrElse (reg, 0.0)
          if (delay < incr.toDouble)
            delaymap(reg) = incr.toDouble
        }
        case RegclkRegex(reg, ref, incr, path) => {
          // ChiselError.info("reg/CLK: %s %s %s %s".format(reg, ref, incr, path))
        }
        case NetRegex(net, incr, path) => {
          // ChiselError.info("net: %s %s %s".format(net, incr, path))

          // We have disapearing signals,
          // so guess the arrival time and delay for them

          var newIndex = pointIndex
          while (points(newIndex) != net && newIndex < points.length) {
            newIndex += 1
          }

          val startpoint = points(pointIndex)
          val startarrival: Double = arrivalmap getOrElse (startpoint, 0.0)
          val netarrival: Double = arrivalmap getOrElse (net, 0.0) 
          val startTime: Double = startarrival
          val finishTime: Double = if (netarrival > path.toDouble) netarrival else path.toDouble
          val interval: Int = newIndex - pointIndex
          val delay: Double = (finishTime - startTime) / interval.toDouble
          var arrival = finishTime
          while (newIndex > pointIndex) {
            val point = points(newIndex)
            val oldarrival = arrivalmap getOrElse (point, 0.0)

            if (arrival > oldarrival)
              arrivalmap(point) = arrival
            else
              arrival = oldarrival
            
            arrival -= delay            
            newIndex -= 1
          }

          pointIndex += interval
        }
        case PinRegex(pin, ref, incr, path) => { 
          // ChiselError.info("pin: %s %s %s %s".format(pin, ref, incr, path))
        }
        case _ =>
      }
    }

    // annotate arrival times and delays for REGs
    m bfs {node => 
      val arrival = arrivalmap getOrElse (getSignalName(node), 0.0)
      val delay = delaymap getOrElse (getSignalName(node), 0.0)

      node match {
        // In this case, its input disappear, and it is higly likely that 
        // the delay is attributed to its input.
        case bits: Bits if bits.dir == OUTPUT && bits.inputs.length == 1 => {
          node.arrival = arrival
          // node.delay = 0.0
          node.inputs(0).arrival = arrival
          // node.inputs(0).delay = delay
        }
        case _ => {
          if (node.arrival < arrival) node.arrival = arrival
          if (node.delay < delay) node.delay = delay
        }
      }
    }

    // Delay adjustment
    m bfs {node =>
      var arrival = 0.0
      for (input <- node.inputs) {
        if (input.isReg) {
          if (arrival < input.delay) arrival = input.delay
        } else {
          if (arrival < input.arrival) arrival = input.arrival 
        }
      }
      val delay = node.arrival - arrival
      if (!node.isReg) {
        if (node.delay < delay) node.delay = delay 
      }
    }
  }

  private def calcCriticalPathDelay() {
    val paths = getTimingPaths(initializeDFS)
    var maxdelay = 0.0

    for (path <- paths) {
      val delay = (0.0 /: path) (_ + _.delay)
      if (maxdelay < delay) maxdelay = delay
    }
  
    // ChiselError.info("Critical path delay = %.5f".format(maxdelay))  
    Module.criticalPathDelay = maxdelay
  }

/*
  private def printDelay(m: Module, filename: String = "delay.rpt"): Unit = {
    val basedir = ensureDir(Module.targetDir)
    val rptdir  = ensureDir(basedir+"report")
    val rptfile = new java.io.FileWriter(rptdir+filename) 
    val report = new StringBuilder();

    ChiselError.info("Donggyu: print out delays")

    report.append("\t\t+--------------------------------+\n")
    report.append("\t\t|         Nodes' delay           |\n")
    report.append("\t\t|                   by Donggyu   |\n")
    report.append("\t\t+--------------------------------+\n\n")

    m bfs { node =>
      report.append(getSignalName(node) + ":" + nodeToString(node) + 
                    " ==> indelay: %.4f, outdelay: %.4f\n".format(node.indelay, node.outdelay))
    }
   
    report.append("\nCritical path delay: %.4f".format(calcCPDelay(m)))
   
    // ChiselError.info(report)
    // Write the signals into the file
    try {
      rptfile.write(report.toString)
    } finally {
      rptfile.close()
    }
  }
  */
  /*
  override def elaborate(c: Module) {
    super.elaborate(c)
  
    generateTcl(c)
    executeDC()
    annotateDelay(c, c.name + "_timing.rpt")
    calcCriticalPathDelay()
  }
  */
}
