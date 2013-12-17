package Chisel

import Chisel._
import Module._
import Node._
// import GraphTrace._
import ChiselError._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack
import scala.io.Source
import scala.sys.process._
import scala.util.Random

trait Backannotation extends GraphTrace {
  preElaborateTransforms += ((c: Module) => levelChildren(c))
  preElaborateTransforms += ((c: Module) => 
    Module.sortedComps = gatherChildren(c).sortWith(
      (x, y) => (x.level < y.level || 
          (x.level == y.level && x.traversal < y.traversal) ) ) )
  // TODO: No side effiects?
  preElaborateTransforms += ((c: Module) => assignClockAndResetToModules)
  preElaborateTransforms += ((c: Module) => Module.sortedComps.map(_.addDefaultReset))
  preElaborateTransforms += ((c: Module) => c.addClockAndReset)
  preElaborateTransforms += ((c: Module) => gatherClocksAndResets)
  preElaborateTransforms += ((c: Module) => connectResets)
  /*************************/
  preElaborateTransforms += ((c: Module) => c.inferAll)
  preElaborateTransforms += ((c: Module) => c.forceMatchingWidths)
  preElaborateTransforms += ((c: Module) => collectNodesIntoComp(initializeDFS))
  preElaborateTransforms += ((c: Module) => nameAll(c))
  preElaborateTransforms += ((c: Module) => getNodeIndices(c))
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
        if emitRef(n) == nodeName  
      } yield n

      // TODO: Are clocks included in the future?
      /*
      val clks = for {
        m <- comps
        if m.clock != null && emitRef(m.clock) == nodeName
      } yield m.clock
      */

      val resets = for {
        m <- comps
        if emitRef(m.reset) == nodeName
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
  preElaborateTransforms += ((c: Module) => calcCriticalPathDelay(c))
  preElaborateTransforms += ((c: Module) => printGraph(c, c.name + "_graph.rpt"))

  private def getTimingPaths(m: Module): HashSet[List[Node]] = {
    def insertToMultimap(multimap: HashMap[Node, HashSet[List[Node]]], node: Node, list: List[Node]) {
      if (!(multimap contains node)) multimap(node) = new HashSet[List[Node]]
      multimap(node) += list
    }

    val paths = new HashSet[List[Node]]() // Timing paths
    val tailpaths = new HashMap[Node, HashSet[List[Node]]]() // Multimap for tail paths
    val walked = new HashSet[Node]() // Set of walked nodes (DFS)
    val stack = m.initializeDFS

    walked ++= stack
    walked ++= m.resets.values

    // do DFS
    while (!stack.isEmpty) {
      val node = stack.pop

      node match {
        // OUTPUT PORT: the end point of a timing path
        // => initialize timing paths
        case bits: Bits if bits.dir == OUTPUT && node.componentOf == Module.topComponent => {
          insertToMultimap(tailpaths, node, Nil)
        }
        // REGISTER: the end point of a timing path
        // => initialize timing paths
        case _: Reg => {
          insertToMultimap(tailpaths, node, Nil)
        }
        // MEMORY: the end point of a timing path
        // => initialize timing paths
        case mem: Mem[_] if mem.isInline => {
          insertToMultimap(tailpaths, node, Nil)
        }
        case _ =>
      }

      val tails = tailpaths getOrElse (node, new HashSet[List[Node]]())
      val newtailAdded = new HashMap[Node, Boolean]()

      for (input <- node.inputs) {
        input match {
          // INPUT PORT: the start point of a timing path
          // => return timing paths 
          case bits: Bits if bits.dir == INPUT && input.componentOf == Module.topComponent =>
            for (tail <- tails) {
              paths += (input::node::tail)
            }
          // REGISTER: the start point of a timing path
          // => return timing paths
          case _: Reg => 
            for (tail <- tails) {
              paths += (input::node::tail)
            }
          // MEMORY: the start point of a timing path
          // => return timing paths
          case mem: Mem[_] if mem.isInline => 
            for (tail <- tails) {
              paths += (input::node::tail)
            }
          // Other nodes => construct timing paths though this node
          case _ => {
            val oldtails = (tailpaths getOrElse (input, new HashSet[List[Node]]())).clone
            for (tail <- tails) {
              insertToMultimap(tailpaths, input, node::tail)
            } 
            val newtails = tailpaths getOrElse (input, new HashSet[List[Node]]())
            newtailAdded(input) = (oldtails != newtails)
          }
        }

        // Not walked => should be visited later
        if (!(input == null) && !(walked contains input)) {
          walked += input
          stack push input
        }
        // When all the input's coumsumers are not visited, the input node should be visited later
        /*
        else if ((input.consumers foldLeft false) { (x, y) =>  x || !(walked contains y) }) {
          stack push input
        }
        */
        // An input node should be visited again when its new tails are added
        else if (newtailAdded getOrElse (input, false)) {
          stack push input
        }
      }

      newtailAdded.clear
    }

    /*
    for (path <- paths) {
      ChiselError.info(
        (getSignalName(path.head) /: path.tail) ( _ + " -> " + getSignalName(_))
      )
    }
    */

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
    val cmdopts = " -significant_digits 5 -path only -nosplit"
    val cmdtail = " }\n"

    tcl.append("report_timing %s -nets > %s_critical.rpt\n".format(cmdopts, m.name))
    tcl.append("report_timing -from [all_inputs] %s -nets >> %s_critical.rpt\n".format(cmdopts, m.name))
    tcl.append("report_timing -to [all_outputs] %s -nets >> %s_critical.rpt\n".format(cmdopts, m.name))
    tcl.append("report_timing -from [all_inputs] -to [all_outputs] %s -nets >> %s_critical.rpt\n".format(cmdopts, m.name))
  
    val paths = getTimingPaths(m)

    // Remove superpaths
    // e.g. Path1: x -> T1 -> y, Path2: x -> T1 -> T2 -> y ==> remove Path1
    for (path <- paths) {
      var flag = false
      val pathhead = path.head
      val pathmid = path.tail.init
      val pathlast = path.last
      for (p <- paths) {
        val head = p.head
        val mid = p.tail.init
        val last = p.last
        flag |= ( (path != p) && (pathhead == head) && (pathlast == last) &&
          (true /: pathmid) ((x, y) => x && (mid contains y)) )
      }
      if (flag) paths -= path
    }

 
    def genReports(from: String, to: String, via: List[Node], end: Node) {
      // When a node has only one input,
      // prune its input.
      // Otherwise, you will se a giant powerset.
      def truncation (list: List[Node]):List[Node] = {
        def pruneOneInput(list: List[Node]): ListBuffer[Node] = 
          list match {
            case Nil => new ListBuffer[Node]()
            case _::Nil => new ListBuffer[Node]()
            case input::consumer::tail =>
              if (consumer.inputs.length > 1){
                val list = pruneOneInput(consumer::tail)
                list.prepend(input)
                list
              } else 
                pruneOneInput(consumer::tail)
          }

        val prunedArray = pruneOneInput(list)

        // Randomly deleting points
        val random = new Random
        val sizeLimit = 8 // emperically best number
        var arrayLength = prunedArray.length
        while (arrayLength > sizeLimit) {
          val rmIndex = random.nextInt(arrayLength)
          prunedArray.remove(rmIndex)
          arrayLength = arrayLength - 1
        }

        prunedArray.toList
      }

      def powerset (list: List[Node]): Array[List[Node]] = {
        list match {
          case Nil => Array(Nil)
          case head::tail => (powerset(tail) map { head::_ }) ++ powerset(tail)
        }
      }

      def reports (sets: Array[List[Node]]) {
        def condcmds(via: List[Node]) = {
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
          tcl.append("if " + condcmds(head))
          for (via <- mid) {
            tcl.append(" elseif " + condcmds(via))
          }
          tcl.append(" else " + condcmds(last))
        }
      }
  
      val throughSets = powerset(truncation(via:::List(end))).sortWith(_.size > _.size)

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
      
      (start, end) match {
        case (_: Delay, _: Delay) => {
          val from = " -from [get_pins " + getSignalName(start) + "_reg*/CLK]"
          val to = " -to [get_pins " + getSignalName(end) + "_reg*/D]"
          genReports(from, to, via, end)
        }
        case (_: Delay, bits: Bits) if bits.dir == OUTPUT => {
          val from = " -from [get_pins " + getSignalName(start) + "_reg*/CLK]"
          val to = " -to " + getSignalName(end)
          genReports(from, to, via, end)
        }
        case (bits: Bits, _: Delay) if bits.dir == INPUT => {
          val from = " -from " + getSignalName(start)
          val to = " -to [get_pins " + getSignalName(end) + "_reg*/D]"
          genReports(from, to, via, end)
        }
        case (input: Bits, output: Bits) if input.dir == INPUT && output.dir == OUTPUT => {
          val from = " -from " + getSignalName(start)
          val to = " -to " + getSignalName(end)
          genReports(from, to, via, end)
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
    val PathRegex = """([\w/_]+(?: -> [\w/_]+)+).*""".r
    val RegclkRegex = """\s*([\w/_]+)_reg(?:\[\d+\])*/CLK \(([\w_]+)\)(?: <-)?\s+(\d+\.\d+)\s+(\d+\.\d+).*""".r
    val ReginRegex = """\s*([\w/_]+)_reg(?:\[\d+\])*/D \(([\w_]+)\)(?: <-)?\s+(\d+\.\d+)\s+(\d+\.\d+).*""".r
    val RegoutRegex = """\s*([\w/_]+)_reg(?:\[\d+\])*/Q \(([\w_]+)\)(?: <-)?\s+(\d+\.\d+)\s+(\d+\.\d+).*""".r
    val PinRegex = """\s*([\w/_]+) \(([\w_]+)\)(?: <-)?\s+(\d+\.\d+)\s+(\d+\.\d+).*""".r
    val NetRegex = """\s*([\w/_]+)(?:\[\d+\])* \(net\)\s+(\d+\.\d+)\s+(\d+\.\d+).*""".r
    val PortinRegex = """\s*([\w/_]+)(?:\[\d+\])* \(in\)\s+(\d+\.\d+)\s+(\d+\.\d+).*""".r
    val PortoutRegex = """\s*([\w/_]+)(?:\[\d+\])* \(out\)\s+(\d+\.\d+)\s+(\d+\.\d+).*""".r
    val StartpointRegex = """\s*Startpoint: ([\w/_]+)(?:\[\d+\])*.*""".r
    val EndpointRegex = """\s*Endpoint: ([\w/_]+)(?:\[\d+\])*.*""".r
    val delaymap = new HashMap[String, Double]
    val seldelaymap = new HashMap[String, Double]
    val regdelaymap = new HashMap[String, Double]
    var points: Array[String] = null
    var pointIndex = 0
    var startPoint = ""
    var endPoint = ""
    var prevArrivalTime = 0.0

    // We have disapearing signals,
    // so guess the arrival time and delay for them
    def inferDelay(node: String, totalDelay: Double, isRegin: Boolean = false) {
      var newIndex = pointIndex

      while (newIndex < points.length - 1 && points(newIndex) != node) {
        newIndex += 1
      }

      val interval: Int = newIndex - pointIndex + 1
      val delay: Double = totalDelay / interval.toDouble

      while (pointIndex <= newIndex) {
        val curPoint = points(pointIndex)
        val prevPoint = if (pointIndex == 0) null else points(pointIndex - 1)

        // This explains the delays of conditional statements 
        // for registers (reset, enable)
        if (pointIndex == newIndex && isRegin && prevPoint != null) {
          if ((seldelaymap getOrElse (prevPoint, 0.0)) < delay)
            seldelaymap(prevPoint) = delay
        } else {
          if ((delaymap getOrElse (curPoint, 0.0)) < delay) 
            delaymap(curPoint) = delay
        }

        pointIndex += 1
      }
    }
 
    for (line <- lines) {
      line match {
        case PathRegex(path) => {
          // ChiselError.info("\npath: %s".format(path))
          points = path.split(" -> ")
          pointIndex = 0
          prevArrivalTime = 0.0
        }
        case StartpointRegex(start) => {
          var startPoint = start
          if ((start drop (start.length - 4)) == "_reg")
            startPoint = start take (start.length - 4)
          // ChiselError.info("start: " + startPoint)
          if (points.head != startPoint) { 
            points = Array(startPoint) ++ points
          }
        }
        case EndpointRegex(end) => {
          var endPoint = end
          if ((end drop (end.length - 4)) == "_reg")
            endPoint = end take (end.length - 4)
          // ChiselError.info("end: " + endPoint)
          if (points.last != endPoint) 
            points = points ++ Array(endPoint)
        }
        case PortinRegex(in, incr, arr) => {
          assert(in == points.head)
          // ChiselError.info("in: %s %s %s".format(in, incr, arr))
          val arrivalTime = arr.toDouble
          val delay = arrivalTime - prevArrivalTime
          if ((delaymap getOrElse (in, 0.0)) < delay) 
            delaymap(in) = delay
          pointIndex += 1
          prevArrivalTime = arrivalTime
        }
        case PortoutRegex(out, incr, arr) => {
          assert(out == points.last)
          // ChiselError.info("out: %s %s %s".format(out, incr, arr))
          val arrivalTime = arr.toDouble
          val delay = arrivalTime - prevArrivalTime
          inferDelay(out, delay)
          prevArrivalTime = arrivalTime
        }
        case ReginRegex(reg, ref, incr, arr) => {
          assert(reg == points.last)
          // ChiselError.info("reg/D: %s %s %s %s".format(reg, ref, incr, arr))
          val arrivalTime = arr.toDouble
          val delay = arrivalTime - prevArrivalTime
          inferDelay(reg, delay, true)
          prevArrivalTime = arrivalTime
        }
        case RegoutRegex(reg, ref, incr, arr) => {
          // ChiselError.info("reg/Q: %s %s %s %s".format(reg, ref, incr, arr))
          assert(reg == points.head)
          val delay = incr.toDouble
          if ((regdelaymap getOrElse (reg, 0.0)) < delay)
            delaymap(reg) = delay
          pointIndex += 1
          prevArrivalTime = arr.toDouble
        }
        case RegclkRegex(reg, ref, incr, arr) => {
          // ChiselError.info("reg/CLK: %s %s %s %s".format(reg, ref, incr, arr))
        }
        case NetRegex(net, incr, arr) => {
          // ChiselError.info("net: %s %s %s".format(net, incr, arr))
          val arrivalTime = arr.toDouble
          val delay = arrivalTime - prevArrivalTime
          inferDelay (net, delay)
          prevArrivalTime = arrivalTime
        }
        case PinRegex(pin, ref, incr, arr) => { 
          // ChiselError.info("pin: %s %s %s %s".format(pin, ref, incr, arr))
        }
        case _ =>
      }
    }

    // annotate arrival times and delays for REGs
    m bfs {node => 
      val nodeName = getSignalName(node)
      val delay = delaymap getOrElse (nodeName, 0.0)

      node match {
        // In this case, its input disappear, and it is higly likely that 
        // the delay is attributed to its input.
        case bits: Bits if bits.dir == OUTPUT && bits.inputs.length == 1 => {
          val inputName = getSignalName(node.inputs(0))
          val olddelay = delaymap getOrElse (inputName, 0.0)
          delaymap(inputName) = olddelay + delay
          node.delay = 0.0
        }
        case _ => {
          /*
          if (node.isReg) {
            val regdelay = regdelaymap getOrElse (nodeName, 0.0)
            if (node.delay < regdelay)
              node.delay = regdelay
            if (node.seldelay < delay)
              node.seldelay = delay
          } else {
            if (node.delay < delay) 
              node.delay = delay
          }
          */
          val seldelay = seldelaymap getOrElse (nodeName, 0.0)

          if (node.delay < delay)
            node.delay = delay
          if (node.seldelay < seldelay)
            node.seldelay = seldelay
        }
      }
    }

    // Delay adjustment
    /*
    m bfs {node =>
      node match {
        case reg: Reg => {
          if (reg.isEnable && (reg.enableSignal.litOf == null || reg.enableSignal.litOf.value != 1)) {
            val enableDelay = reg.arrival - reg.enableSignal.arrival
            if (reg.enableDelay < enableDelay)
              reg.enableDelay = enableDelay
          }
          if (reg.isReset) {
            val resetDelay = reg.arrival - reg.inputs.last.arrival
            if (reg.resetDelay < resetDelay)
              reg.resetDelay = resetDelay
          }
        }
        case _ =>
      }

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
    */
  }

  private def calcCriticalPathDelay(m: Module) {
    val paths = getTimingPaths(m)
    var maxpath = ""
    var maxdelay = 0.0

    for (path <- paths) {
      val head = path.head
      val init = path.init
      val last = path.last
      var delay = (0.0 /: init) (_ + _.delay)

      delay += ( if (last.isReg) init.last.seldelay else last.delay )
/*
( last match {
        case reg: Reg => {
          val input = init.last
          if (reg.inputs.last == input)
            reg.seldelay 
          else
            0.0
        }
        case _: Mem[_] => 0.0
        case _ => last.delay
      } )
*/

      if (maxdelay < delay) { 
        maxdelay = delay
        maxpath = (getSignalName(path.head) /: path.tail) ( _ + " -> " + getSignalName(_))
      }
    }
  
    // ChiselError.info("Critical path delay = %.5f".format(maxdelay))  
    Module.criticalPath = maxpath
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
}
