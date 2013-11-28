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
  preElaborateTransforms += ((c: Module) => printCrosses(Module.crosses))

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

  private def printParents(module: Module): String = {
    if (module == Module.topComponent) emitRef(module)
    else printParents(module.parent) + "." + emitRef(module)
  }

  private def printSignal(node: Node): String = {
    if (node == null) "null"
    else if (node.width > 1) "Bus(" + printParents(node.componentOf) + "." + emitTmp(node) + ")"
    else "Signal(" + printParents(node.componentOf) + "." + emitTmp(node) + ")"
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

  protected def generateTcl(m: Module, filename: String = "gentcl.tcl") {
    val basedir = ensureDir(Module.targetDir)
    val dcsyndir  = ensureDir(basedir+"dc-syn")
    val tcl = new StringBuilder();
    val tclfile = new java.io.FileWriter(dcsyndir+filename)

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
    tcl.append("create_clock clk -name ideal_clock1 -period 1\n\n")
   
    var first_report = true

    def cmdhead = 
      if (first_report) { 
        first_report = false 
        "redirect timing.rpt { " 
      } else { 
        "redirect -append timing.rpt { " 
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

    m.bfs { node =>
      for (i <- node.inputs) {
        (i, node) match {
          case (_: Chisel.Reg, r: Chisel.Reg) => {
            tcl.append(cmdhead + "echo \"\\n" + emitTmp(i) + " ---> " + emitTmp(r) + "\"" + cmdtail)
            tcl.append(cmdhead + "report_timing -from [get_clock] -to [get_pins " + emitTmp(r) + 
                       "_reg*/next_state]" + cmdopts + cmdtail)
          }
          case (r: Chisel.Reg, _) => {
            (signalmap get node) match {
              case None    => {
                tcl.append(cmdhead + "echo \"\\n" + emitTmp(r) + " ---> " + emitTmp(node) + "\"" + cmdtail)
                tcl.append(cmdhead + "report_timing -from [get_pins " + emitTmp(r) + "_reg*/Q] -to " + 
                           emitTmp(node) + cmdopts + cmdtail)
              }
              case Some(c) => { 
                tcl.append(cmdhead + "echo \"\\n" +emitTmp(r) + " ---> " + emitTmp(c) + "\"" + cmdtail)
                tcl.append(cmdhead + "report_timing -from [get_pins " + emitTmp(r) + "_reg*/Q] -to " + 
                           emitTmp(c) + cmdopts + cmdtail)
              }
            }
          }
          case (_: Chisel.Literal, _) =>
          case (_, r: Chisel.Reg) => {
            (signalmap get i) match {
              case None    => {
                tcl.append(cmdhead + "echo \"\\n" + emitTmp(i) + " ---> " + emitTmp(r) + "\"" + cmdtail)
                tcl.append(cmdhead + "report_timing -from [get_clock] -to [get_pins " + emitTmp(r) + 
                           "_reg*/Q]" + cmdopts + cmdtail)
              }
              case Some(_) =>
            }
          }
          case (_, _) => {
            (signalmap get i, signalmap get node) match {
              case (None, None)    => 
                tcl.append(cmdhead + "echo \"\\n" + emitTmp(i) + " ---> " + emitTmp(node) + "\"" + cmdtail)
                tcl.append(cmdhead + "report_timing -from " + emitTmp(i) + " -to " + emitTmp(node) + cmdopts + cmdtail)
              case (None, Some(c)) =>
                tcl.append(cmdhead + "echo \"\\n" + emitTmp(i) + " ---> " + emitTmp(c) + "\"" + cmdtail)
                tcl.append(cmdhead + "report_timing -from " + emitTmp(i) + " -to " + emitTmp(c) + cmdopts + cmdtail)
              case (Some(_), _)    =>
            }
          }
        }
      }
    }

    tcl.append("\nwrite -f verilog -hierarchy -output " + m.name + ".mapped.v\n\n")
    tcl.append("exit")

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
    var earlydelay = 0.0
    var middledelay = 0.0
    var lastdelay = 0.0
    val hashmap = new HashMap[(String, String), (Double, Double, Double)]

    // for (line <- lines) { ChiselError.info(line) }

    for (line <- lines) {
      line match {
        case RetRegex(ret) => {
          // ChiselError.info ("RET " + ret.toString)
          hashmap += ((nodepair, (earlydelay, middledelay, lastdelay)))
          start = false
          firstflag = false
          secondflag = false
          earlydelay = 0.0
          middledelay = 0.0
          lastdelay = 0.0
        }
        case NodeRegex(i, node) => {
          // ChiselError.info((i + " ---> " + node))
          start = true
          nodepair = (i, node)
        }
        case DelayRegex(cell, incr, path) => {
          if (secondflag) { 
            lastdelay += incr.toDouble 
          }
          else if (firstflag) { 
            middledelay += incr.toDouble 
          }
          else if (start) {
            earlydelay += incr.toDouble 
          }
          else { }
          // ChiselError.info("cell : " + cell + "\tincr : " + incr.toDouble + "\tpath : " + path + "\t( " + 
          //   earlydelay + " , " + middledelay + " , " + lastdelay + " )")
        }
        case FlagRegex(cell, incr, path) => {
          if (firstflag) {
            secondflag = true
            lastdelay += incr.toDouble
          } else if (start) {
            firstflag = true
            middledelay += incr.toDouble
          } else {
          }
          // ChiselError.info("cell : " + cell + " <- \tincr : " + incr + "\tpath : " + path + "\t( " + 
          //   earlydelay + " , " + middledelay + " , " + lastdelay + " )")
        }
        case _ =>
      }
    }

    hashmap  
  }

  protected def printDelay(m: Module, filename: String = "delay.rpt"): Unit = {
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
      report.append(emitTmp(node) + ":" + nodeToString(node) + " ==> delay: " + node.delay.toString + "\n")
    }

    // ChiselError.info(report)
    // Write the signals into the file
    try {
      rptfile.write(report.toString)
    } finally {
      rptfile.close()
    }
  }

  protected def annotateDelay(m: Module, filename: String = "timing.rpt") {
    ChiselError.info("Donggyu: annotate delays")
    val basedir = ensureDir(Module.targetDir)
    val dcsyndir  = ensureDir(basedir+"dc-syn")
    val delaymap = readDelay(dcsyndir+filename)

    m bfs { node => 
      val nodename = emitTmp(node)
      for (i <- node.inputs) {
        val inputname = emitTmp(i)
        val (earlydelay, middledelay, lastdelay) = 
            delaymap getOrElse ((inputname, nodename), (0.0, 0.0, 0.0))
        (i, node) match {
          case (_: Chisel.Reg, _: Chisel.Reg) => {
            node.delay = middledelay
          }
          case (_: Chisel.Reg, _) => {
            node.delay = middledelay
          }
          case (_, _: Chisel.Reg) => {
            node.delay = earlydelay
          }
          case _ => {
            node.delay = middledelay
          }
        }
      }
    }
  }
}
