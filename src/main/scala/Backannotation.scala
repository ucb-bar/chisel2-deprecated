package Chisel

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

object nodeToString {
  def apply(node: Node): String = { 
    node match {
      case bits  : Bits      => 
        if (!bits.isTypeNode || bits.inputs.length == 0) {
          if (bits.dir == OUTPUT) "OUTPUT(" + bits.name + ")"
          else if (bits.dir == INPUT) "INPUT(" +bits.name+ ")"
          else if (bits.name != null && !bits.inputs.isEmpty) 
            "Bits(%s)".format(nodeToString(bits.inputs(0)))
          else "Bits(?)"
        }
        else nodeToString(bits.inputs(0).getNode)
      case bundle: Bundle    => "Bundle(" + 
        { for { (n, i) <- bundle.elements } yield n + " => " + nodeToString(i) + ", " } + ")"
      case vec   : Vec[_]    => "Vec(" + vec.name + ")"
      case reg   : Reg       => "Reg(%s)".format(reg.name)
      case lit   : Literal   => "Lit(%s)".format(lit.name)
      case op    : Op        => 
        if (op.inputs.length == 1) op.op + "(" + nodeToString(op.inputs(0)) + ")"
        else if (op.op == "Mux") "[ " + nodeToString(op.inputs(0)) + " ] ? [ " + nodeToString(op.inputs(1)) + " ] : [ " + nodeToString(op.inputs(2)) + " ]"
        else "[ " + nodeToString(op.inputs(0)) + " ] " + op.op + " [ " + nodeToString(op.inputs(1)) + " ]"
      case cat   : Cat       => 
        { "{ " + ((nodeToString(cat.inputs.head) /: cat.inputs.tail) (_ + ", " + nodeToString(_))) + " }" }
      case fill  : Fill      =>
        { "Fill(" + nodeToString(fill.inputs(0)) + ", " + nodeToString(fill.n) + ")" }
      case ext   : Extract   => 
        val hi: String = nodeToString(ext.hi)
        val lo: String = nodeToString(ext.lo) 
        nodeToString(ext.inputs(0)) + "[" + { if (hi == lo) hi else hi + ":" + lo } + "]"  
      case bind  : Binding   => "Binding(" + nodeToString(bind.inputs(0)) + ")"
      case mem   : Mem[_]    => "Mem(%s)".format(mem.name)
      case memacc: MemAccess => nodeToString(memacc.mem) + "[" + nodeToString(memacc.addr) + "]"
      case rom   : ROM[_]    => "ROM(%s)".format(rom.name) 
      case romread: ROMRead[_] => nodeToString(romread.rom) + "[" + nodeToString(romread.addr) + "]"
      case clk   : Clock     => "Clock(%s)".format(clk.name)
      case _ => if (node == null) "" else node.toString
    }      
  }
}

trait Backannotation extends Backend {
  val targetdir = ensureDir(Module.targetDir)

  preElaborateTransforms += ((c: Module) => c.genAllMuxes)
  preElaborateTransforms += ((c: Module) => nameAll(c))
  preElaborateTransforms += ((c: Module) => c bfs (x => if(!x.isTypeNode) emitRef(x)))
 
  // TODO: remove them 
  /*
  preElaborateTransforms += ((c: Module) => levelChildren(c))
  preElaborateTransforms += ((c: Module) => 
    Module.sortedComps = gatherChildren(c).sortWith(
      (x, y) => (x.level < y.level || 
          (x.level == y.level && x.traversal < y.traversal) ) ) )
  preElaborateTransforms += ((c: Module) => Module.sortedComps map (_.addDefaultReset))
  preElaborateTransforms += ((c: Module) => connectResets)
  preElaborateTransforms += ((c: Module) => c.inferAll)
  preElaborateTransforms += ((c: Module) => c.forceMatchingWidths)
  preElaborateTransforms += ((c: Module) => c.removeTypeNodes)
  preElaborateTransforms += ((c: Module) => collectNodesIntoComp(initializeDFS))
  preElaborateTransforms += ((c: Module) => nameAll(c))
  */

  // Todo: no recursion
  protected def getParentNames(m: Module, delim: String = "/"): String = {
    if (m == Module.topComponent) emitRef(m) + delim
    else getParentNames(m.parent) + emitRef(m) + delim
  }

  protected def getSignalName(n: Node, delim: String = "/"): String = {
    if (n == null) "null" else getParentNames(n.componentOf, delim) + emitRef(n)
  }

  protected def copyResource(filename: String, toDir: String) {
    val resourceStream = getClass getResourceAsStream "/" + filename //Todo: understand it (Java?)
    if (resourceStream != null) {
      val file =  new java.io.FileWriter(toDir+filename)
      while(resourceStream.available > 0) {
        file write (resourceStream read)
      }
      file.close
      resourceStream.close 
    } else {
      ChiselError.info("Critical Error: we should be able to access resource/" + filename)
    }
  }
}

trait SignalBackannotation extends Backannotation {
  val signals = new HashSet[Node]

  preElaborateTransforms += ((c: Module) => annotateSignals(c))
  // analyses += ((c: Module) => reportSignals(c))

  private def annotateSignals(m: Module) {
    ChiselError.info("Backannotation: annotate signals")

    // Read the signal list file
    // TODO: generalize the signal file format
    val lines = Source.fromFile(Module.signalFilename).getLines
    val TermRegex = """\s*([\w\._\:]+)\s+([\d\.\+-e]+)\s+([\d\.\+-e]+)\s+([\d\.\+-e]+)\s+([\d\.\+-e]+)""".r
    val signalNames = new HashSet[String]

    for (line <- lines) {
      line match {
        case TermRegex(exp, coeff, se, tstat, pvalue) => {
          val vars = exp split ":"
          if (tstat != "NaN" && pvalue != "NaN") {
            signalNames ++= vars
          }
        }
        case _ =>
      }
    }

    // Find correspoinding nodes
    m bfs { node =>
      if (!node.isTypeNode) {
        val signalName = getSignalName(node, ".")
        if (signalNames contains signalName) {
          signals += node
        }
      }
    }

    // For resets
    for (m <- Module.components) {
      val reset = m.reset
      val resetName = getSignalName(reset, ".")
      if (signalNames contains resetName) {
        signals += reset
      }
    }
  }

  // TODO: elaborate it
  private def reportSignals(m: Module) {
    val rptdir  = ensureDir(targetdir+"report")
    val rptfile = new java.io.FileWriter(rptdir+"%s_signal.rpt".format(m.name))
    val report = new StringBuilder();

    ChiselError.info("Backannotation: report annotated signals")

    report append "\t\t+-------------------------------------+\n"
    report append "\t\t|     Signal and Conter Report        |\n"
    report append "\t\t|                     by Donggyu Kim  |\n"
    report append "\t\t+-------------------------------------+\n\n"

    Module.sortedComps map { module =>
      report append "Module: %s\n".format(module.getPathName)
      module.nodes map { node =>
       if (signals contains node) {
          val counter = node.counter
          report append "  %s: %s => %s ==> %s\n".format(
            getSignalName(node), 
            nodeToString(node), 
            getSignalName(counter), 
            nodeToString(counter.inputs(0))
          )
       }
      }
    } 

    ChiselError.info(report.result)

    try {
      rptfile.write(report.result)
    } finally {
      rptfile.close()
    }
  }
}

trait DelayBackannotation extends Backannotation {
  // Timing paths used for generating a tcl file & calculating a critical path
  protected val paths = new HashMap[String, ListBuffer[Node]]
  protected val pathdelays = new HashMap[String, Double]
  protected var criticalpath = ""
  protected var criticaldelay = 0.0
  protected def pathToString(path: ListBuffer[Node]) = { 
    (getSignalName(path.head) /: path.tail) ( _ + " -> " + getSignalName(_))
  }

  val dcsyndir = ensureDir(targetdir+"dc-syn")

  // preElaborateTransforms += ((c: Module) => c.removeTypeNodes)
  preElaborateTransforms += ((c: Module) => getTimingPaths(c))
  preElaborateTransforms += ((c: Module) => generateTcl(c))
  preElaborateTransforms += ((c: Module) => executeDC(c))
  preElaborateTransforms += ((c: Module) => annotateDelay(c))
  preElaborateTransforms += ((c: Module) => calculateCriticalPath)
  analyses += ((c: Module) => reportDelay(c))
  // preElaborateTransforms += ((c: Module) => printGraph(c, c.name + "_graph.rpt"))

  private def getTimingPaths(m: Module) {
    val newpaths = new HashSet[ListBuffer[Node]] 
    val tailpaths = new HashMap[Node, HashSet[ListBuffer[Node]]] // Multimap for tail paths
    def insertTails(multimap: HashMap[Node, HashSet[ListBuffer[Node]]], 
                    node: Node, listbuffer: ListBuffer[Node]): Boolean = {
      if (!(multimap contains node)) multimap(node) = new HashSet[ListBuffer[Node]]
      listbuffer prepend node
      multimap(node) add listbuffer
    }
    def getTails(multimap: HashMap[Node, HashSet[ListBuffer[Node]]], node: Node) = 
      multimap getOrElse (node, new HashSet[ListBuffer[Node]])

    val walked = new HashSet[Node] // Set of walked nodes (DFS)
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
          insertTails(tailpaths, node, new ListBuffer)
        // REGISTER: the end point of a timing path
        // => initialize timing paths
        case _: Reg =>
          insertTails(tailpaths, node, new ListBuffer)
        // MEMORY: the end point of a timing path
        // => initialize timing paths
        case mem: Mem[_] =>
          insertTails(tailpaths, node, new ListBuffer)
        case _ => false
      }

      val tails = getTails(tailpaths, node)
      
      for (input <- node.inputs) {
        val inputTailAdded = input match {
          // INPUT PORT: the start point of a timing path
          // => return timing paths 
          case bits: Bits if bits.dir == INPUT && input.componentOf == Module.topComponent => {
            for (tail <- tails) {
              val path = tail.clone
              path prepend input
              newpaths += path
            }
            false
          }  
          // REGISTER: the start point of a timing path
          // => return timing paths
          case _: Reg => {
            for (tail <- tails) {
              val path = tail.clone
              path prepend input
              newpaths += path
            }
            false
          }
          // MEMORY: the start point of a timing path
          // => return timing paths
          case mem: Mem[_] => {
            for (tail <- tails) {
              val path = tail.clone
              path prepend input
              newpaths += path
            }
            false
          }
          // Other nodes => construct timing paths though this node
          case _ => {
            var added = false
            for (tail <- tails) {
              val newtail = tail.clone
              added |= insertTails(tailpaths, input, newtail)
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
    val newpathArray = newpaths.toArray
    for (i <- 0 until newpathArray.size) {
      val path = newpathArray(i)
      val pathhead = path.head
      val pathmid = path.tail.init
      val pathlast = path.last
      var isSuperpath = false
      for (j <- i + 1 until newpathArray.size) {
        val p = newpathArray(j)
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
      }
    }
  }

  private def generateTcl(m: Module) {
    val tcl = new StringBuilder
    val tclfile = new java.io.FileWriter(dcsyndir+"%s_gendctcl.tcl".format(m.name))
    val tclthreshold = (1 << 19) // write tcl to the tcl file when it's capacity exceeds this value

    ChiselError.info("Backannotation: generate a tcl file")

    // Critical path reports
    val rptopts = "-nets -significant_digits 5 -path only -nosplit"

    tcl append "report_timing %s -nets > %s_critical.rpt\n".format(rptopts, m.name)
    tcl append "report_timing -from [all_inputs] %s -nets >> %s_critical.rpt\n".format(rptopts, m.name)
    tcl append "report_timing -to [all_outputs] %s -nets >> %s_critical.rpt\n".format(rptopts, m.name)
    tcl append "report_timing -from [all_inputs] -to [all_outputs] %s -nets >> %s_critical.rpt\n\n".format(rptopts, m.name)

    // Timing path reports  
    var isFirstWrite = true
    def redir = if (isFirstWrite) { isFirstWrite = false ; ">" } else ">>"

    // data structures used to generate reports
    val stack = new Stack[Node]
    val mids = new ArrayBuffer[Node]
    val powerset = new ListBuffer[ListBuffer[Node]]
    val random = new Random

    for ((pathString, path) <- paths) {
      tcl append "echo \"\\n%s\" %s %s_timing.rpt\n".format(pathString, redir, m.name) 

      // First, prune nodes the number of whose inputs is 1
      // Otherwise, we will see a tremendous conditions
      for (i <- 2 until path.length) {
        val node = path(i)
        if (node.inputs.length != 1) {
          mids += path(i-1)
        } else {
          // we don't have to include the node's input
          // since all the timing paths though the node 
          // also  go though its input
        }
      }
    
      // Next, remove nodes randomly 
      // to reduce the number of conditions
      val sizelimit = 8
      var midsLength = mids.length
      while (midsLength > sizelimit) {
        mids remove (random nextInt midsLength)
        midsLength -= 1
      }  

      // construct stack
      for (mid <- mids) {
        stack push mid
      }

      // Generate a power set of the pruned path
      // Todo: more optimizations?
      powerset prepend (new ListBuffer[Node])
      while (!stack.isEmpty) {
        val node = stack.pop
        val powersetSize = powerset.size
        for (i <- 0 until powersetSize) {
          val list = powerset(i).clone
          list prepend node
          powerset += list
        }
      }

      // Net collections for each node in the pruend path
      for (node <- mids) {
        val setName = getSignalName(node, "_")
        val nodeName = getSignalName(node)
        tcl append "set %s_nets [get_nets %s]\n".format(setName, nodeName)
      }

      // Report command with conditions
      var isFirst = true
      val head = path.head
      val last = path.last
      val headname = getSignalName(head, "_")
      val lastname = getSignalName(last, "_")
      val (from, to) = (head, last) match {
        case (_: Delay, _: Delay) => {
          ( "-from [get_pins { %s_reg*/CP %s_reg*/CLK }]".format(headname, headname),
            "-to [get_pins %s_reg*/D]".format(lastname) )
        }
        case (_: Delay, bits: Bits) if bits.dir == OUTPUT => {
          ( "-from [get_pins { %s_reg*/CP %s_reg*/CLK} ]".format(headname, headname),
            "-to %s".format(lastname) )
        }
        case (bits: Bits, _: Delay) if bits.dir == INPUT => {
          ( "-from %s".format(headname), "-to [get_pins %s_reg*/D]".format(lastname) )
        }
        case (input: Bits, output: Bits) if input.dir == INPUT && output.dir == OUTPUT => {
          ( "-from %s".format(headname), "-to %s".format(lastname) )
        }
        // It shouldn't happen
        // Todo: handle it as an exception
        case (_, _) => ("", "")
      }

      if (powerset.size <= 1) {
        tcl append "report_timing %s %s %s >> %s_timing.rpt\n\n".format(from, to, rptopts, m.name)
      } else {
        for (list <- powerset.reverse) {
          if (!(list isEmpty)) {
            val conds = ("$%s_nets != {} ".format(getSignalName(list.head, "_")) /: list.tail) (
              (x, y) => x + "&& $%s_nets != {} ".format(getSignalName(y, "_"))
            )
            val throughs = ("" /: list) ((x, y) => x + "-through %s ".format(getSignalName(y)))

            if (isFirst) {
              isFirst = false
              tcl append "if { %s } {\n  report_timing %s %s %s %s >> %s_timing.rpt \n} ".format(
                conds, from, throughs, to, rptopts, m.name
              )
            } else {
              tcl append "elseif { %s } {\n  report_timing %s %s %s %s >> %s_timing.rpt \n} ".format(
                conds, from, throughs, to, rptopts, m.name
              )
            }
          } else {
            // This produce unexpected wrong numbers
            // tcl append "else {\n  report_timing %s %s %s >> %s_timing.rpt \n}".format(from, to, rptopts, m.name)
            tcl append "else {\n  echo \"\\n1\\n\" >> %s_timing.rpt \n}".format(m.name)
          }
        }
        tcl append "\n"
      }
      
      mids.clear
      powerset.clear
      stack.clear      

      /*
      if (tcl.capacity > tclthreshold) {
        tclfile write tcl.result
        // tclfile.flush
        tcl.clear
      }
      */
    } 

    // Save the result
    tcl append "\nwrite -f verilog -hierarchy -output %s.mapped.v\n\n".format(m.name)
  
    try {
      tclfile write tcl.result
    } finally {
      tclfile.close
    }
  }

  private def executeDC(m: Module) = {
    if (!(new java.io.File(dcsyndir+"dcsetup.tcl").exists)) {
      copyResource("dcsetup.tcl", dcsyndir)
      // Copy the dcsetup.tcl file into the targetDir
      /*
      val resourceStream = getClass getResourceAsStream "/dcsetup.tcl" //Todo: understand it (Java?)
      if (resourceStream != null) {
        val dcsetupFile =  new java.io.FileWriter(dcsyndir+"dcsetup.tcl")
        while(resourceStream.available > 0) {
          dcsetupFile write (resourceStream read)
        }
        dcsetupFile.close
        resourceStream.close 
      } else {
        ChiselError.info("Critical Error: we should be able to access chisel/src/main/resources/dcsetup.tcl")
      }
      */
    }

    if (!(new java.io.File(dcsyndir+"%s_timing.rpt".format(m.name)).exists)) {
      // Execute Design Compiler
      ChiselError.info("Backannotation: start Design Compiler")
      val dccmd = "dc_shell -64bit -f dcsetup.tcl"
      val c = Process(dccmd, 
                      new java.io.File(dcsyndir), 
                      "DESIGN_NAME" -> m.name,
                      "DCTCL_NAME" -> "%s_gendctcl.tcl".format(m.name)).!
      ChiselError.info("Backannotation: finish Design Compiler")
    }
  }

  private def annotateDelay(m: Module) {
    val lines = Source.fromFile(dcsyndir+"%s_timing.rpt".format(m.name)).getLines
    val PathRegex = """([\w/_]+(?: -> [\w/_]+)+).*""".r
    val StartRegex = """\s*Startpoint: ([\w/_]+)(?:\[\d+\])*.*""".r
    val EndRegex = """\s*Endpoint: ([\w/_]+)(?:\[\d+\])*.*""".r
    val PortinRegex = """\s*([\w/_]+)(?:\[\d+\])* \(in\)\s+(\d+\.\d+)\s+(\d+\.\d+).*""".r
    val PortoutRegex = """\s*([\w/_]+)(?:\[\d+\])* \(out\)\s+(\d+\.\d+)\s+(\d+\.\d+).*""".r
    val RegStartRegex = """\s*([\w/_]+)_reg(?:\[\d+\])*/Q \(([\w_]+)\)\s+(\d+\.\d+)\s+(\d+\.\d+).*""".r
    val RegEndRegex = """\s*([\w/_]+)_reg(?:\[\d+\])*/D \(([\w_]+)\)\s+(\d+\.\d+)\s+(\d+\.\d+).*""".r
    val NetRegex = """\s*([\w/_]+)(?:\[\d+\])* \(net\)\s+\d*\s+(\d+\.\d+)\s+(\d+\.\d+).*""".r
    // Maybe useful in the future
    // val PinRegex = """\s*([\w/_]+) \(([\w_]+)\)(?: <-)?\s+(\d+\.\d+)\s+(\d+\.\d+).*""".r
    // val RegclkRegex = """\s*([\w/_]+)_reg(?:\[\d+\])*/(?: CLK | CP) \(([\w_]+)\)\s+(\d+\.\d+)\s+(\d+\.\d+).*""".r

    ChiselError.info("Backannotation: annotate delays")

    val arrtimes = new HashMap[String, Double]
    val nets = new scala.collection.mutable.Queue[String]
    var pathString = ""
    var startpoint = ""
    var endpoint = ""
    var startArrtime = 0.0
    var endArrtime = 0.0

    for (line <- lines) {
      line match {
        // The end of the timing report
        // => Process timing information
        case "1" => {
          val path = paths getOrElse (pathString, new ListBuffer[Node])
          val missingNodes = new ArrayBuffer[Node]
          // val missingNodes = new scala.collection.mutable.Queue[Node] // for random signal mapping
          val random = new Random

          var prevSignalName = ""
          var prevArrtime = 0.0
          var isFirst = true
          for (node <- path) {
            val signalName = getSignalName(node)
            val arrtime = 
              if (signalName == startpoint) startArrtime 
              else if (signalName == endpoint) endArrtime 
              else arrtimes getOrElse (signalName, -1.0)
            if (arrtime >= 0.0) {
              // Missing signal exist ==> Delay estimation
              val missingNodeNum = missingNodes.length
              if (missingNodeNum > 0) {
                val delay = (arrtime - prevArrtime) / (missingNodeNum + 1)
                // Current Method: average delays for missing signals
                for (missingNode <- missingNodes) {
                  //if (delay >= (missingNode.delays getOrElse (prevSignalName, 0.0)))
                  missingNode.delays(prevSignalName) = delay
                  prevSignalName = getSignalName(missingNode)
                  prevArrtime += delay
                }                

                missingNodes.clear

                // Todo: Unfortunaltely the following method does not work well!
                //       How do we get a more elegant way?
                /*
                // Obtain nets between two non missing signals
                val missingNets = new ArrayBuffer[String]
                var dequeue = ""
                while (dequeue != signalName && !nets.isEmpty) {
                  dequeue = nets.dequeue
                  if (dequeue != startpoint && dequeue != endpoint) {
                    missingNets += dequeue
                  }
                }
 
                // Mapping: { missing signals } -> { nets between two non missing signals }
                while (!missingNodes.isEmpty) {
                  // Net to be mapped to the first missing signal 
                  // => One of the first 'sizediff' nets of the missing net queue
                  // -> Randomly choose it 'iterNum' times
                  // Todo: elaborate estimation methods
                  val sizediff = missingNets.length - missingNodes.length
                  var delay = 0.0
                  var mapIdx = 0
                  if (sizediff > 0) {
                    val iterNum = 25
                    var totalDelay = 0.0
                    var idxTotal = 0.0
                    for (i <- 1 to iterNum) {
                      val rndIdx = random nextInt sizediff
                      val missingNet = missingNets(rndIdx)
                      val missingArrtime = arrtimes getOrElse (missingNet, 0.0)
                      val delay = missingArrtime - prevArrtime
                      totalDelay += delay
                      idxTotal += rndIdx
                    }
                    delay = totalDelay / iterNum
                    prevArrtime = arrtimes getOrElse (missingNets(mapIdx), 0.0)
                    mapIdx = (idxTotal / iterNum).toInt
                    missingNets drop (mapIdx + 1)
                  } else if (!missingNets.isEmpty) {
                    val missingNet = missingNets.head
                    val missingArrtime = arrtimes getOrElse (missingNet, 0.0)
                    delay = missingArrtime - prevArrtime
                    prevArrtime = arrtimes getOrElse (missingNet, 0.0)
                    missingNets drop 1
                  } else {
                    delay = 0.0
                  }

                  val missingNode = missingNodes.dequeue
                  if (delay >= (missingNode.delays getOrElse (prevSignalName, 0.0))) 
                    missingNode.delays(prevSignalName) = delay
                  
                  prevSignalName = getSignalName(missingNode)
                }

                missingNets.clear
                */
              }

              // Assign delays with the signal name of its prev node in the path
              node match {
                case _: Delay if isFirst => {
                  if (arrtime >= (node.delays getOrElse (signalName, 0.0)))
                    node.delays(signalName) = arrtime
                }
                case _ => {
                  if (!isFirst) {
                    val delay = arrtime - prevArrtime
                    if (delay >= (node.delays getOrElse (prevSignalName, 0.0)))
                      node.delays(prevSignalName) = delay 
                  } 
                }
              }

              // Throw away nets not used to calculate or estimate delays
              if (signalName != startpoint && signalName != endpoint) {
                var dequeue = ""
                while (dequeue != signalName && !nets.isEmpty) {
                  dequeue = nets.dequeue
                }
              }

              prevSignalName = signalName
              prevArrtime = arrtime
              
              if (isFirst) isFirst = false
            } 
            // delay < 0.0 => this means missing signals
            else {
              missingNodes += node
              // missingNodes enqueue node // for random signal mapping
            }
          }

          nets.clear
          arrtimes.clear

          pathString = ""
          startpoint = ""
          endpoint = ""
          startArrtime = 0.0
          endArrtime = 0.0
        }
        // Read timing reports
        case PathRegex(path) => {
          pathString = path
        }
        case StartRegex(point) => {
          if (point contains "_reg") {
            startpoint = point take (point.length - 4)
          } else {
            startpoint = point
          }
        }
        case EndRegex(point) => {
          if (point contains "_reg") {
            endpoint = point take (point.length - 4)
          } else {
            endpoint = point
          }
        }
        case PortinRegex(port, inc, arrtime) => {
          startArrtime = arrtime.toDouble
        }
        case PortoutRegex(port, inc, arrtime) => {
          endArrtime = arrtime.toDouble
        }
        case RegStartRegex(reg, ref, inc, arrtime) => {
          startArrtime = arrtime.toDouble
        }
        case RegEndRegex(reg, ref, inc, arrtime) => {
          endArrtime = arrtime.toDouble
        }
        case NetRegex(net, inc, arrtime) => {
          if (net != startpoint && net != endpoint) {
            arrtimes(net) = arrtime.toDouble
            nets enqueue net
          }
        }
        case _ =>
      }
    }
  }

  private def calculateCriticalPath() {
    var maxpath = ""
    var maxdelay = 0.0

    ChiselError.info("Backannotation: calculate the critcal path")

    for ((pathString, path) <- paths) {
      var prevSignalName = ""
      var delay = 0.0
      var isFirst = true
      for (node <- path) {
        val signalName = getSignalName(node)
        if (isFirst) {
          node match {
            case _: Delay => delay += (node.delays getOrElse (signalName, 0.0))
            case _ =>
          }
          isFirst = false
        } else {
          delay += (node.delays getOrElse (prevSignalName, 0.0))
        }
        prevSignalName = signalName
      }

      pathdelays(pathString) = delay

      if (delay > maxdelay) {
        maxpath = pathString
        maxdelay = delay
      }
    }

    criticalpath = maxpath
    criticaldelay = maxdelay
  }

  protected def reportDelay(m: Module): Unit = {
    val rptdir  = ensureDir(targetdir+"report")
    val rptfile = new java.io.FileWriter(rptdir+"%s_timing.rpt".format(m.name)) 
    val report = new StringBuilder();

    ChiselError.info("Backannotation: report delays")

    report append "\t\t+--------------------------------+\n"
    report append "\t\t|         Delay Report           |\n"
    report append "\t\t|                by Donggyu Kim  |\n"
    report append "\t\t+--------------------------------+\n\n"

    val walked = new HashSet[Module]
    val stack = new Stack[Module]
    stack push m

    while (!stack.isEmpty) {
      val module = stack.pop
      
      report append "Module: %s\n".format(module.getPathName)
      for(mod <- module.mods) {
        report append "  %s (%s): ".format(getSignalName(mod), nodeToString(mod))
        for ((signal, delay) <- mod.delays) {
          report append " %f (%s) ".format(delay, signal)
        }
        report append "\n"
      }
      report append "\n\n"
      walked += module

      for(child <- module.children) {
        if (!(walked contains child)) {
          stack push child
        }
      }
    }

    report append "\nTiming paths:\n"
    for ((pathString, path) <- paths) {
      report append "  %s: %f\n".format(pathString, pathdelays getOrElse (pathString, 0.0))
    }

    report append "\nCritical path:\n"
    report append "  %s: %f\n".format(criticalpath, criticaldelay)
   
    try {
      rptfile.write(report.result)
    } finally {
      rptfile.close()
    }
  }
}
