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
  // Timing paths used for generating a tcl file & calculating a critical path
  private val paths = new HashMap[String, ListBuffer[Node]]() 
  protected def pathToString(path: ListBuffer[Node]) = { 
    (getSignalName(path.head) /: path.tail) ( _ + " -> " + getSignalName(_))
  }

  val basedir = ensureDir(Module.targetDir)
  val dcsyndir = ensureDir(basedir+"dc-syn")
  protected var dctclfilename: String = ""
  protected var rptfilename: String = ""

  preElaborateTransforms += ((c: Module) => c.removeTypeNodes)
  preElaborateTransforms += ((c: Module) => getTimingPaths(c))
  preElaborateTransforms += ((c: Module) => generateTcl(c))
  preElaborateTransforms += ((c: Module) => executeDC())
  preElaborateTransforms += ((c: Module) => annotateDelay(c, c.name + "_timing.rpt"))
  preElaborateTransforms += ((c: Module) => calcCriticalPathDelay(c))
  preElaborateTransforms += ((c: Module) => printGraph(c, c.name + "_graph.rpt"))

  private def getTimingPaths(m: Module) {
    val newpaths = new HashSet[ListBuffer[Node]]() 
    val tailpaths = new HashMap[Node, HashSet[ListBuffer[Node]]]() // Multimap for tail paths
    val newtailAdded = new HashMap[Node, Boolean]()
    def insertTails(multimap: HashMap[Node, HashSet[ListBuffer[Node]]], node: Node, listbuffer: ListBuffer[Node]): Boolean {
      if (!(multimap contains node)) multimap(node) = new HashSet[ListBuffer[Node]]
      listbuffer prepend node
      multimap(node) add list
    }
    def getTails(multimap: HashMap[Node, HashSet[List[Node]]], node: Node) = 
      multimap getOrElse (node, new HashSet[List[Node]]())

    val walked = new HashSet[Node]() // Set of walked nodes (DFS)
    val stack = m.initializeDFS

    walked ++= stack
    walked ++= m.resets.values

    // do DFS
    while (!stack.isEmpty) {
      val node = stack.pop

      val tailAdded = node match {
        // OUTPUT PORT: the end point of a timing path
        // => initialize timing paths
        case bits: Bits if bits.dir == OUTPUT && node.componentOf == Module.topComponent => 
          insertTails(tailpaths, node, new ListBuffer())
        // REGISTER: the end point of a timing path
        // => initialize timing paths
        case _: Reg =>
          insertTails(tailpaths, node, new ListBuffer())
        // MEMORY: the end point of a timing path
        // => initialize timing paths
        case mem: Mem[_] =>
          insertTails(tailpaths, node, new ListBuffer())
        case _ => false
      }

      val tails = getTails(tailpaths, node)
      
      for (input <- node.inputs) {
        val inputTailAdded = input match {
          // INPUT PORT: the start point of a timing path
          // => return timing paths 
          case bits: Bits if bits.dir == INPUT && input.componentOf == Module.topComponent => {
            for (tail <- tails) {
              tail prepend input
              newpaths += tail
            }
            false
          }  
          // REGISTER: the start point of a timing path
          // => return timing paths
          case _: Reg => {
            for (tail <- tails) {
              tail prepend input
              newpaths += tail
            }
            false
          }
          // MEMORY: the start point of a timing path
          // => return timing paths
          case mem: Mem[_] => {
            for (tail <- tails) {
              tail prepend input
              newpaths += tail
            }
            false
          }
          // Other nodes => construct timing paths though this node
          case _ => {
            val inputTails = getTails(tailpaths, input)
            var added = false
            for (tail <- inputTails) {
              added |= insertTails(tailpaths, input, tail)
            }
            added
          }
        }

        // Not walked => should be visited later
        if (!(input == null) && !(walked contains input)) {
          walked += input
          stack push input
        }
        // An input node should be visited again when its new tails are added
        else if (inputTailAdded) {
          stack push input
        }
      }
    }

    // Prune superpaths
    // e.g. Path1: x -> T1 -> y, Path2: x -> T1 -> T2 -> y ==> remove Path1
    for (i <- 0 until newpaths.size) {
      val path = newpaths(i)
      val pathhead = path.head
      val pathmid = path.tail.init
      val pathlast = path.last
      var isSuperpath = false
      for (j <- i + 1 until newpaths.size) {
        val p = newpaths(j)
        val head = p.head
        val mid = p.tail.init
        val last = p.last
        isSuperpath |= ( (path != p) && (pathhead == head) && (pathlast == last) &&
          (true /: pathmid) ((x, y) => x && (mid contains y)) )
      }
      // Build the path map
      if (!isSuperpath) {
        val pathString = pathToString(path)
        paths(pathString) = path
        // ChiselError.info(pathString)
      }
    }
  }

  private def generateTcls(m: Module) {
    dctclfilename = m.name + "_gendctcl.tcl"
    val tcl = new StringBuilder
    val tclfile = new java.io.FileWriter(dcsyndir+filename)
    val tclthreshold = (1 << 19) // write tcl to the tcl file when it's capacity exceeds this value

    ChiselError.info("Donggyu: generate a tcl file")
    // Read designs
    tcl append "analyze -format verilog %s.v\n".format(m.name)
    tcl append "elaborate %s\n".format(m.name)
    tcl append "link\n"
    tcl append "check_design\n"
    tcl append "create_clock clk -name ideal_clock1 -period 1\n"

    // tcl.append("\nwrite -f verilog -hierarchy -output " + m.name + ".unmapped.v\n\n")

    // Compile designs
    tcl append "compile -only_hold_time\n\n"

    // Critical path reports
    tcl append "report_timing %s -nets > %s_critical.rpt\n".format(cmdopts, m.name)
    tcl append "report_timing -from [all_inputs] %s -nets >> %s_critical.rpt\n".format(cmdopts, m.name)
    tcl append "report_timing -to [all_outputs] %s -nets >> %s_critical.rpt\n".format(cmdopts, m.name)
    tcl append "report_timing -from [all_inputs] -to [all_outputs] %s -nets >> %s_critical.rpt\n\n".format(cmdopts, m.name)

    tclfile write tcl.result
    tclfile.flush
    tcl.clear

    // Timing path reports  
    rpt_filename = m.name + "_timing.rpt"
    var first_report = true

    def cmdhead = 
      if (first_report) { 
        first_report = false 
        "redirect %s { ".format(rpt_filename) 
      } else { 
        "redirect -append %s { ".format(rpt_filename)
      }
    val cmdopts = "-significant_digits 5 -path only -nosplit "
    val cmdtail = " }\n"

    // data structures used to generate reports
    val mid = new ListBuffer[Node]()
    val powerset = new ListBuffer[ListBuffer[Node]]()
    val stack = new Stack[Node]()
    val collections = new HashMap[Node, String]

    for ((pathString, path) <- paths) {
      tcl append (cmdhead + "echo \"\\n" + pathString + "\"" + cmdtail)

      // First, prune nodes the number of whose inputs is 1
      // Otherwise, we will see a tremendous conditions
      val head = path.head
      val last = path.last
      for (i <- 2 until path.length) {
        val node = path(i)
        if (node.inputs.length != 1) {
          mid += path(i-1)
        } else {
          // we don't have to include the node's input
          // since all the timing paths though the node 
          // also  go though its input
        }
      }
    
      //Next, generate a power set of the pruned path
      stack ++= mid
      powerset prepend (new ListBuffer[Node]())
      while (!stack.isEmpty) {
        val node = stack.pop
        for (i <- powerset.size - 1 to 0) {
          val list = poweset(i).clone
          list prepend node
          powerset prepend list
        }
      }

      // Net collections for each node in the pruend path
      var index = 0
      for (node <- mid) {
        val nodeName = getSignalName(node)
        val collection = "set nets%d %s\n".format(index, nodeName)
        tcl append collection
        collections(node) = collection
      }

      // Report command with conditions
      var isFirst = true
      val headname = getSignalName(head)
      val lastname = getSignalName(last)
      val (from, to) = (head, last) match {
        case (_: Delay, _: Delay) => {
          ( "-from [get_pins %s_reg*/CP %s_reg*/CLK] ".format(headname, headname),
            "-to [get_pins %s_reg*/D] ".forname(lastname) )
        }
        case (_: Delay, bits: Bits) if bits.dir == OUTPUT => {
          ( "-from [get_pins %s_reg*/CP %s_reg*/CLK] ".format(headname, headname),
            "-to %s ".forname(lastname) )
        }
        case (bits: Bits, _: Delay) if bits.dir == INPUT => {
          ( "-from %s ".format(headname), "-to [get_pins %s_reg*/D] ".forname(lastname) )
        }
        case (input: Bits, output: Bits) if input.dir == INPUT && output.dir == OUTPUT => {
          ( "-from %s ".format(headname), "-to %s ".forname(lastname) )
        }
        // It shouldn't happen
        // Todo: handle it as an exception
        case (_, _) => ("", "")
      }

      if (powerset.size <= 1) {
        tcl append (cmdhead + "report_timing " + from + to + cmdopt + cmdtail)
      } else {
        val report = new StringBuilder
        for (list <- powerset) {
          if (!(list isEmpty)) {
            // Conditions
            if (isFirst) {
              report append "\tif { "
              isFirst = false
            } else {
              report append "\telseif { "
            }
 
            val listhaed = list.head
            val listtail = list.tail
          
            val headnets = collections(listhead)
            report append " $%s != {}".format(headnets)
            for (node <- listtail) {
              val nets = collections(node)
              report append " && $%s != {}".format(nets)
            }
            report append "} {\n\t"

            // Reports
            val throughs = ("" /: list) (_ + "-through %s ".format(getSignalName(_)))
            report append (cmdhead + "report_timing " + from + throughs + to + cmdopt + cmdtail)
            report append "}\n"
          }

          tcl append report.result
          report.clear
        }
      }
      
      mid.clear
      powerset.clear
      stack.clear      
      collections.clear
      if (tcl.capacity > tclthreshold) {
        tclfile write tcl.result
        tclfile.flush
        tcl.clear
      }
    } 

    // Save the result
    tcl append "\nwrite -f verilog -hierarchy -output %s.mapped.v\n\n".format(m.name)
  
    try {
      tclfile write tcl.result
    } finally {
      tclfile.close
    }
  }

  private def executeDC() = {
    // Copy the dcsetup.tcl file into the targetDir
    val resourceStream = getClass getResouceAsStream "../resources/dcsetup.tcl"
    if (resourceStream != null) {
      val dcsetupFile =  new java.io.FileWriter(dcsyndir+"dcsetup.tcl")
      while(resourceStream.available > 0) {
        dcsetupFile write (resouceStream read)
      }
      dcsetupFile.close
      resourceStream.close 
    }

    // Execute Design Compiler
    ChiselError.info("Donggyu: start Design Compiler")
    val cmd = "cd %s; dc_shell -64bit -f dcsetup.tcl".format(dcsyndir)
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
    val missdelaymap = new HashMap[String, (Int, Double)]
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
        } else if (interval == 1) {
          if ((delaymap getOrElse (curPoint, 0.0)) < delay) 
            delaymap(curPoint) = delay
        } else {
          val (cnt, sum) = missdelaymap getOrElse (curPoint, (0, 0.0))
          missdelaymap(curPoint) = (cnt + 1, sum + delay)
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
          if ((delaymap getOrElse (reg, 0.0)) < delay)
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
      val (cnt, missdelay) = missdelaymap getOrElse (nodeName, (0, 0.0))

      node match {
        // In this case, its input disappear, and it is higly likely that 
        // the delay is attributed to its input.
        case bits: Bits if bits.dir == OUTPUT && bits.inputs.length == 1 => {
          // val inputName = getSignalName(node.inputs(0))
          // val inputdelay = delaymap getOrElse (inputName, 0.0)
          if (delay > 0) {
            if (node.inputs(0).delay < delay) {
              node.inputs(0).delay = delay
            }
          } else if (cnt != 0) {
            if (node.inputs(0).delay < (missdelay / cnt.toDouble)) {
              node.inputs(0).delay = (missdelay / cnt.toDouble)
            }
          }

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

          if (delay > 0.0) {
            node.delay = node.delay + delay
          } else if (cnt != 0) {
            ChiselError.info("hi! " + nodeName + ": " +  node.delay.toString)
            node.delay = node.delay + (missdelay / cnt.toDouble)
            ChiselError.info("hi! " + nodeName + ": " +  node.delay.toString)
          }

          /*
          if (cnt != 0)
            node.delay = delay + (missdelay / cnt.toDouble)
          else if (node.delay < delay)
            node.delay = delay
          */

          val seldelay = seldelaymap getOrElse (nodeName, 0.0)

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
