package Chisel

import Chisel._
import Module._
import Node._
import Backend._
import ChiselError._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.io.Source
import scala.sys.process._

trait SignalBackannotation extends Backend {
  // For signal backannotation
  preElaborateTransforms += ((c: Module) => levelChildren(c))
  preElaborateTransforms += ((c: Module) => 
    Module.sortedComps = gatherChildren(c).sortWith(
      (x, y) => (x.level < y.level || 
          (x.level == y.level && x.traversal < y.traversal) ) ) )
  preElaborateTransforms += ((c: Module) => c.inferAll)
  preElaborateTransforms += ((c: Module) => c.forceMatchingWidths)
  // preElaborateTransforms += ((c: Module) => c.removeTypeNodes)
  preElaborateTransforms += ((c: Module) => collectNodesIntoComp(initializeDFS))
  preElaborateTransforms += ((c: Module) => c.traceNodes)
  preElaborateTransforms += ((c: Module) => nameAll(c))
  preElaborateTransforms += ((c: Module) => nameNodes)
  preElaborateTransforms += ((c: Module) => annotateCrosses(c))
  preElaborateTransforms += ((c: Module) => printCrosses(Module.crosses, c.name + "_crosses.rpt"))

  // For consistent naming
  private def nameNodes: Unit = {
    for(comp <- Module.sortedComps) {
      for(node <- comp.mods if node != null) {
        if (node.isTypeNode)
          emitTmp(node.getNode)
        else
          emitTmp(node)
      }
    }
  }

  private def readCrosses(filename: String): ArrayBuffer[(Double, Array[String])] = {
    val lines: Iterator[String] = Source.fromFile(filename).getLines
    val TermRegex = """([\w\._]+)\s+(-?\d\.\d+e[\+-]\d+)""".r
   
    (lines foldLeft (ArrayBuffer[(Double, Array[String])]())) {
      case (res: ArrayBuffer[(Double, Array[String])], TermRegex(exp, coeff)) => {
        val vars = exp split ".__cross__."
        ChiselError.info(coeff + ", " + (vars.head /: vars.tail) {_ + ", " + _})
        res += ((coeff.toDouble, vars))
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
        if (names.isEmpty) true
        else if (emitRef(module) == names.last) checkParents(names.init, module.parent)
        else false
      }

      val comps = for {
        m <- Module.components
        if checkParents(parentNames, m)
      } yield m

      val nodes = for {
        m <- comps; n <- m.mods
        if emitTmp(n) == nodeName  
      } yield n

      val clks = for {
        m <- comps
        if m.clock != null && emitTmp(m.clock) == nodeName
      } yield m.clock

      val resets = for {
        m <- comps
        if emitTmp(m.reset) == nodeName
      } yield m.reset

      if (!nodes.isEmpty) nodes.head
      else if (!clks.isEmpty) clks.head
      else if (!resets.isEmpty) resets.head
      else null
    }

    def getTerm(coeff: Double, exp: Array[String])  = {
      val term = for { v <- exp } yield getNode( v split '.' )
      val (busses, signals) = term partition (x => x != null && x.width > 1)

      val termToString = (printSignal(term.head) /: term.tail) { _ + " * " +  printSignal(_) }
      ChiselError.info("term: " + coeff.toString + " * " +  termToString)

      (coeff, signals, busses)
    }

    Module.crosses ++= crosses map { 
      case (coeff: Double, exp: Array[String]) => {
        getTerm(coeff, exp)
      }
    }
  }
  
  private def printParents(module: Module): String = {
    if (module == Module.topComponent) emitRef(module)
    else printParents(module.parent) + "." + emitRef(module)
  }

  private def printSignal(node: Node): String = {
    if (node == null) "null"
    else if (node.width > 1) "Bus(" + printParents(node.componentOf) + "." + emitTmp(node) + ")"
    else "Signal(" + printParents(node.componentOf) + "." + emitTmp(node) + ")"
  }

  private def printCrosses(crosses: ArrayBuffer[(Double, Array[Node], Array[Node])], filename: String = "crosses.rpt"): Unit = {
    val basedir = ensureDir(Module.targetDir)
    val rptdir  = ensureDir(basedir+"report")
    val rptfile = new java.io.FileWriter(rptdir+filename) 
    val report = new StringBuilder();

    ChiselError.info("Donggyu: print out signal crosses")

    report.append("\t\t+--------------------------------+\n")
    report.append("\t\t|        Signal Crosses          |\n")
    report.append("\t\t|                   by Donggyu   |\n")
    report.append("\t\t+--------------------------------+\n\n")

    for ((coeff, signals, busses) <- crosses) {
      val signalsToString = {
        if (signals.isEmpty) ""
        else (printSignal(signals.head) /: signals.tail) { _ + " * " + printSignal(_) }
      }

      val bussesToString = {
        if (busses.isEmpty) ""
        else (printSignal(busses.head) /: busses.tail) { _ + " * " + printSignal(_) }
      }

      report.append(coeff.toString + " * " + signalsToString + " * " + bussesToString + "\n")
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

trait DelayBackannotation extends Backend {
/*
  analyses += ((c: Module) => generateTcl(c))
  analyses += ((c: Module) => executeDC())
  analyses += ((c: Module) => annotateDelay(c))
  analyses += ((c: Module) => printDelay(c))
*/

  private def getParents(m: Module): String = {
    if (m == Module.topComponent) ""
    else getParents(m.parent) + emitRef(m.parent) + "/"
  }

  private def getNodeName(n: Node): String = {
    getParents(n.componentOf) + emitTmp(n)
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
    tcl.append("""set synthetic_library "dw_foundation.sldb" """+"\n")
    tcl.append("""set link_library "* $target_library $synthetic_library" """+"\n")
    tcl.append("""set alib_library_analysis_path "alib" """+"\n\n")

    tcl.append("analyze -format verilog " + m.name + ".v\n")
    tcl.append("elaborate " + m.name + "\n")
    tcl.append("link\n")
    tcl.append("check_design\n")
    tcl.append("create_clock clk -name ideal_clock1 -period 1\n")
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

    val cmdopts = " -input_pins -significant_digits 5 -path only"
    val cmdtail = " }\n"

    val delset = new HashSet[Node]()
    val signalmap = new HashMap[Node, Node]()

    // Construct a map from deleted signals to their corresponding signals
    m.bfs { node => 
      for (c <- node.consumers) { 
        c match {
          case bits: Chisel.Bits if bits.inputs.length == 1 && bits.dir == OUTPUT => {
            signalmap += ((node, bits))
          }
          case _ =>
        }
      }
    }

    /*
    for ((key, value) <- signalmap) {
      ChiselError.info(nodeToString(key) + " => " + nodeToString(value))
    }
    */

    tcl.append(cmdhead + "echo \"\\n*** Critical Path Timing Information ***\\n\"" + cmdtail)
    tcl.append(cmdhead + "report_timing" + cmdopts + cmdtail)

    m.bfs { from =>
      for (to <- from.consumers) {
        (from, to) match {
          case (_: Reg, to: Reg) => {
            tcl.append(cmdhead + "echo \"\\n" + getNodeName(from) + " ---> " + getNodeName(to) + "\"" + cmdtail)
            tcl.append(cmdhead + "report_timing -from [get_pins " + getNodeName(from) + "_reg*/Q] -to [get_pins " + 
                       getNodeName(to) + "_reg*/D]" + cmdopts + cmdtail)
          }
          case (_: Reg, _) => {
            (signalmap get to) match {
              case None    => {
                tcl.append(cmdhead + "echo \"\\n" + getNodeName(from) + " ---> " + getNodeName(to) + "\"" + cmdtail)
                tcl.append(cmdhead + "report_timing -from [get_pins " + getNodeName(from) + "_reg*/Q] -to " + 
                           getNodeName(to) + cmdopts + cmdtail)
              }
              case Some(to) => { 
                tcl.append(cmdhead + "echo \"\\n" + getNodeName(from) + " ---> " + getNodeName(to) + "\"" + cmdtail)
                tcl.append(cmdhead + "report_timing -from [get_pins " + getNodeName(from) + "_reg*/Q] -to " + 
                           getNodeName(to) + cmdopts + cmdtail)
              }
            }
          }
          case (_: Literal, _) =>
          case (_, _: Reg) => {
            (signalmap get from) match {
              case None    => {
                tcl.append(cmdhead + "echo \"\\n" + getNodeName(from) + " ---> " + getNodeName(to) + "\"" + cmdtail)
                tcl.append(cmdhead + "report_timing -from " + getNodeName(from) + " -to [get_pins " + 
                           getNodeName(to) + "_reg*/D]" + cmdopts + cmdtail)
              }
              case Some(_) =>
            }
          }
          case (_, _) => {
            (signalmap get from, signalmap get to) match {
              case (None, None)    => 
                tcl.append(cmdhead + "echo \"\\n" + getNodeName(from) + " ---> " + getNodeName(to) + "\"" + cmdtail)
                tcl.append(cmdhead + "report_timing -from " + getNodeName(from) + " -to " + 
                           getNodeName(to) + cmdopts + cmdtail)
              case (None, Some(to)) =>
                tcl.append(cmdhead + "echo \"\\n" + getNodeName(from) + " ---> " + getNodeName(to) + "\"" + cmdtail)
                tcl.append(cmdhead + "report_timing -from " + getNodeName(from) + " -to " + 
                           getNodeName(to) + cmdopts + cmdtail)
              case (Some(_), _)    =>
            }
          }
        }
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

  private def readDelay(filename: String): HashMap[(String, String), (Double, Double, Double)] = {
    val lines: Iterator[String] = Source.fromFile(filename).getLines
    val NodeRegex = """(\w+) ---> (\w+)""".r
    val DelayRegex = """\s*([\w/_\*\[\]]+) \([\w/_\*\[\]]*\)\s+([\d\.]+)\s+([\d\.]+).*""".r
    val FlagRegex = """\s*([\w/_\*\[\]]+) \([\w/_\*\[\]]*\) <- \s+([\d\.]+)\s+([\d\.]+).*""".r
    val RetRegex = """(\d)""".r
   
    var nodepair = ("", "")
    var start = false
    var firstflag = false
    var secondflag = false
    var delay1 = 0.0
    var delay2 = 0.0
    var delay3 = 0.0
    val hashmap = new HashMap[(String, String), (Double, Double, Double)]

    // for (line <- lines) { ChiselError.info(line) }

    for (line <- lines) {
      line match {
        case RetRegex(ret) => {
          // ChiselError.info ("RET " + ret.toString)
          hashmap += ((nodepair, (delay1, delay2, delay3)))
          start = false
          firstflag = false
          secondflag = false
          delay1 = 0.0
          delay2 = 0.0
          delay3 = 0.0
        }
        case NodeRegex(from, to) => {
          // ChiselError.info((from + " ---> " + to))
          start = true
          nodepair = (from, to)
        }
        case DelayRegex(cell, incr, path) => {
          if (secondflag) { 
            delay3 += incr.toDouble 
          }
          else if (firstflag) { 
            delay2 += incr.toDouble 
          }
          else if (start) {
            delay1 += incr.toDouble 
          }
          else { }
          // ChiselError.info("cell : " + cell + "\tincr : " + incr.toDouble + "\tpath : " + path + "\t( " + 
          //   delay1 + " , " + delay2 + " , " + delay3 + " )")
        }
        case FlagRegex(cell, incr, path) => {
          if (firstflag) {
            secondflag = true
            delay2 += incr.toDouble
          } else if (start) {
            firstflag = true
            delay1 += incr.toDouble
          } else {
          }
          // ChiselError.info("cell : " + cell + " <- \tincr : " + incr + "\tpath : " + path + "\t( " + 
          //   delay1 + " , " + delay2 + " , " + delay3 + " )")
        }
        case _ =>
      }
    }

    hashmap  
  }

  private def annotateDelay(m: Module, filename: String = "timing.rpt") {
    ChiselError.info("Donggyu: annotate delays")
    val basedir = ensureDir(Module.targetDir)
    val dcsyndir  = ensureDir(basedir+"dc-syn")
    val delaymap = readDelay(dcsyndir+filename)

    m bfs { from => 
      val fromname = getNodeName(from)
      for (to <- from.consumers) {
        val toname = getNodeName(to)
        val (delay1, delay2, delay3) =  
            delaymap getOrElse ((fromname, toname), (0.0, 0.0, 0.0))
        (from, to) match {
          case (from: Bits, _) if from.dir == INPUT => {
            if (from.outdelay < delay1) from.outdelay = delay1
          }
          case (_: Reg, _) => {
            if (from.outdelay < delay1) from.outdelay = delay1
            if (to.indelay < delay2) to.indelay = delay2
          }
          case (_, _: Reg) => {
            if (from.outdelay < delay2) from.outdelay = delay2
          }
          case _ => {
            if (from.outdelay < delay2) from.outdelay = delay2
          }
        }
      }
    }
  }

  private def calcCPDelay(m: Module) : Double = {
    ChiselError.info("Donggyu: calculate critical path delays")

    // Calculate early start & early finish times
    // in the boundary of registers
    m bfs { from =>
      from match {
        case _: Reg => from.earlyFinish = from.outdelay
        case _ => from.earlyFinish = from.earlyStart + from.outdelay
      }

      for (to <- from.consumers) {
        if (to.earlyStart < from.earlyFinish + to.indelay) {
          to.earlyStart = from.earlyFinish + to.indelay
        }
      }
    }

    // Critical path delay = the latest early start time among nodes 
    var cp_delay = 0.0

    m bfs { node =>
      if (cp_delay < node.earlyStart)
        cp_delay = node.earlyStart
    }

    return cp_delay
  }

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
      report.append(getNodeName(node) + ":" + nodeToString(node) + 
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

  override def elaborate(c: Module) {
    super.elaborate(c)
  
    generateTcl(c)
    executeDC()
    annotateDelay(c, c.name + "_timing.rpt")
    printDelay(c, c.name + "_delay.rpt")
  }
}
