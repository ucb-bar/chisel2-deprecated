// author: jonathan bachrach
package Chisel {

import scala.math._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue
import scala.collection.mutable.Stack
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.BitSet
import java.lang.reflect.Modifier._;
import java.io.File;
import java.io.InputStream
import java.io.OutputStream
import java.io.PrintStream
import scala.sys.process._

import scala.math.max;
import Node._;
import Literal._;
import Component._;
import Bundle._;
import ChiselError._;

object Component {
  var saveWidthWarnings = false
  var saveConnectionWarnings = false
  var saveComponentTrace = false
  var saveDot = false
  var dontFindCombLoop = false
  var widthWriter: java.io.FileWriter = null
  var connWriter: java.io.FileWriter = null
  var isDebug = false;
  var isIoDebug = true;
  var isClockGatingUpdates = false;
  var isClockGatingUpdatesInline = false;
  var isVCD = false;
  var isFolding = true;
  var isGenHarness = false;
  var isReportDims = false;
  var scanFormat = "";
  var scanArgs: ArrayBuffer[Node] = null;
  var printFormat = "";
  var printArgs: ArrayBuffer[Node] = null;
  var testNodes: Array[Node] = null;
  var includeArgs: List[String] = Nil;
  var targetDir: String = null
  var configStr: String = null;
  var compIndex = -1;
  val compIndices = HashMap.empty[String,Int];
  val compDefs = new HashMap[StringBuilder, String];
  var isEmittingComponents = false;
  var isCompilingEmittedC = false;
  var isTestingC = false;
  var backendName = "c";
  var topComponent: Component = null;
  val components = ArrayBuffer[Component]();
  val procs = ArrayBuffer[proc]();
  val resetList = ArrayBuffer[Node]();
  val muxes = ArrayBuffer[Node]();
  var ioMap = new HashMap[Node, Int];
  var chiselOneHotMap = new HashMap[(UFix, Int), Bits]
  var chiselOneHotBitMap = new HashMap[(Bits, Int), Bool]
  var chiselAndMap = new HashMap[(Node, Node), Bool]
  var searchAndMap = true
  var ioCount = 0;
  val compStack = new Stack[Component]();
  var stackIndent = 0;
  var printStackStruct = ArrayBuffer[(Int, Component)]();
  var firstComp = true;
  def genCompName(name: String): String = {
    if (compIndices contains name) {
      val count = (compIndices(name) + 1)
      compIndices += (name -> count)
      name + "_" + count
    } else {
      compIndices += (name -> 1)
      name + "_" + 1
    }
  }
  def nextCompIndex : Int = { compIndex = compIndex + 1; compIndex }
  def splitArg (s: String) = s.split(' ').toList;

  // TODO: MAYBE CHANGE NAME TO INITCOMPONENT??
  // TODO: ADD INIT OF TOP LEVEL NODE STATE
  // TODO: BETTER YET MOVE ALL TOP LEVEL STATE FROM NODE TO COMPONENT
  def initChisel () = {
    saveWidthWarnings = false
    saveConnectionWarnings = false
    saveComponentTrace = false
    saveDot = false
    dontFindCombLoop = false
    widthWriter = null
    connWriter = null
    isGenHarness = false;
    isDebug = false;
    isIoDebug = true;
    isClockGatingUpdates = false;
    isClockGatingUpdatesInline = false;
    isFolding = true;
    isReportDims = false;
    scanFormat = "";
    scanArgs = new ArrayBuffer[Node]();
    printFormat = "";
    printArgs = new ArrayBuffer[Node]();
    testNodes = null;
    isCoercingArgs = true;
    targetDir = "."
    configStr = "";
    compIndex = -1;
    compIndices.clear();
    components.clear();
    compStack.clear();
    stackIndent = 0;
    firstComp = true;
    printStackStruct.clear();
    procs.clear();
    resetList.clear()
    muxes.clear();
    ioMap.clear()
    chiselOneHotMap.clear()
    chiselOneHotBitMap.clear()
    chiselAndMap.clear()
    searchAndMap = false
    ioCount = 0;
    isEmittingComponents = false;
    isCompilingEmittedC = false;
    isTestingC = false;
    backendName = "c";
    topComponent = null;

    conds.clear()
    conds.push(Bool(true))
  }

  def ensure_dir(dir: String) = {
    val d = dir + (if (dir == "" || dir(dir.length-1) == '/') "" else "/");
    new File(d).mkdirs();
    d
  }

  //component stack handling stuff
  
  def isSubclassOfComponent(x: java.lang.Class[ _ ]): Boolean = {
    val classString = x.toString;
    if(classString == "class java.lang.Object")
      return false;
    else if(classString == "class Chisel.Component")
      return true;
    else
      isSubclassOfComponent(x.getSuperclass)
  }

  def printStack = {
    var res = ""
    for((i, c) <- printStackStruct){
      val dispName = if(c.moduleName == "") c.className else c.moduleName
      res += (genIndent(i) + dispName + " " + c.instanceName + "\n")
    }
    println(res)
  }

  def genIndent(x: Int): String = {
    if(x == 0)
      return ""
    else 
      return "    " + genIndent(x-1);
  }

  def nameChildren(root: Component) = {
    val walked = new HashSet[Component] // this is overkill, but just to be safe
    
    //initialize bfs queue of Components
    val bfsQueue = new Queue[Component]()
    bfsQueue.enqueue(root)

    // if it popped off the queue, then it already has an instance name
    while(!bfsQueue.isEmpty) {
      val top = bfsQueue.dequeue
      walked += top
      for(child <- top.children){
        top.nameChild(child)
        if(!walked.contains(child)) bfsQueue.enqueue(child)
      }
    }
  }

  def push(c: Component){
    if(firstComp){
      compStack.push(c);
      firstComp = false;
      printStackStruct += ((stackIndent, c));
    } else {
      val st = Thread.currentThread.getStackTrace;
      //for(elm <- st)
      //println(elm.getClassName + " " + elm.getMethodName + " " + elm.getLineNumber);
      var skip = 3;
      for(elm <- st){
	if(skip > 0) {
	  skip -= 1;
	} else {
	  if(elm.getMethodName == "<init>") {

	    val className = elm.getClassName;

	    if(isSubclassOfComponent(Class.forName(className)) && !c.isSubclassOf(Class.forName(className))) {
              if(saveComponentTrace)
	        println("marking " +className+ " as parent of " + c.getClass);
	      while(compStack.top.getClass != Class.forName(className)){
		pop;
	      }

              val dad = compStack.top;
	      c.parent = dad;
              dad.children += c;

	      compStack.push(c);
	      stackIndent += 1;
	      printStackStruct += ((stackIndent, c));
	      return;
	    }
	  }
	}
      }
    }
  }

  def pop(){
    compStack.pop;
    stackIndent -= 1;
  }

  def getComponent(): Component = if(compStack.length != 0) compStack.top else { 
    // val st = Thread.currentThread.getStackTrace;
    // println("UNKNOWN COMPONENT "); 
    // for(frame <- st)
    //   println("  " + frame);
    null 
  };
  
  def assignResets() {
    for(c <- components) {
      if(c.reset.inputs.length == 0 && c.parent != null)
	c.reset.inputs += c.parent.reset
    }
  }
}


abstract class Component(resetSignal: Bool = null) {
  var ioVal: Data = null;
  var name: String = "";
  val bindings = new ArrayBuffer[Binding];
  var wiresCache: Array[(String, Bits)] = null;
  var parent: Component = null;
  var containsReg = false;
  val children = new ArrayBuffer[Component];
  var inputs = new ArrayBuffer[Node];
  var outputs = new ArrayBuffer[Node];
  val asserts = ArrayBuffer[Assert]();
  val blackboxes = ArrayBuffer[BlackBox]();
  val debugs = HashSet[Node]();
  
  val mods  = new ArrayBuffer[Node];
  val omods = new ArrayBuffer[Node];
  // val gmods = new ArrayBuffer[Node];
  val regs  = new ArrayBuffer[Reg];
  val nexts = new Queue[Node];
  var nindex = -1;
  var defaultWidth = 32;
  var moduleName: String = "";
  var className:  String = "";
  var instanceName: String = "";
  var pathName: String = "";
  var pathParent: Component = null;
  val childNames = new HashMap[String, Int];
  var named = false;
  var verilog_parameters = "";
  components += this;

  push(this);

  def nameChild(child: Component) = {
    if(!child.named){
      Predef.assert(child.className != "")
      if(childNames contains child.className){
	childNames(child.className)+=1;
	child.instanceName = child.className + "_" + childNames(child.className);
      } else {
	childNames += (child.className -> 0);
	child.instanceName = child.className;
      }
      child.named = true;
    }
  }

  //true if this is a subclass of x
  def isSubclassOf(x: java.lang.Class[ _ ]): Boolean = {
    var className: java.lang.Class[ _ ] = this.getClass;
    while(className.toString != x.toString){
      if(className.toString == "class Chisel.Component") return false;
      className = className.getSuperclass;
    }
    return true;
  }

  def depthString(depth: Int): String = {
    var res = "";
    for (i <- 0 until depth)
      res += "  ";
    res
  }
  def ownIo() = {
    // println("COMPONENT " + name + " IO " + io);
    val wires = io.flatten;
    for ((n, w) <- wires) {
      // println(">>> " + w + " IN " + this);
      w.component = this;
    }
  }
  def name_it() = {
    val cname  = getClass().getName(); 
    val dotPos = cname.lastIndexOf('.');
    name = if (dotPos >= 0) cname.substring(dotPos+1) else cname;
    className = name;
    if(!isEmittingComponents) {
      if (compIndices contains name) {
        val compIndex = (compIndices(name) + 1);
        compIndices += (name -> compIndex);
        name = name + "_" + compIndex;
      } else {
        compIndices += (name -> 0);
      }
    }
  }
  def findBinding(m: Node): Binding = {
    // println("FINDING BINDING " + m + " OUT OF " + bindings.length + " IN " + this);
    for (b <- bindings) {
      // println("LOOKING AT " + b + " INPUT " + b.inputs(0));
      if (b.inputs(0) == m)
        return b
    }
    // println("UNABLE TO FIND BINDING FOR " + m);
    return null
  }
  //def io: Data = ioVal;
  def io: Data
  def nextIndex : Int = { nindex = nindex + 1; nindex }
  val nameSpace = new HashSet[String];
  def genName (name: String): String = 
    if (name == null || name.length() == 0) "" else this.instanceName + "_" + name;
  var isWalking = new HashSet[Node];
  var isWalked = new HashSet[Node];
  override def toString: String = name
  def wires: Array[(String, Bits)] = {
    if (wiresCache == null)
      wiresCache = io.flatten;
    wiresCache
  }
  def assert(cond: Bool, message: String) = 
    asserts += Assert(cond, message);
  def debug(x: Node) = 
    debugs += x;
  def <>(src: Component) = io <> src.io;
  def apply(name: String): Data = io(name);
  // COMPILATION OF REFERENCE
  def emitDec: String = {
    var res = "";
    val wires = io.flatten;
    for ((n, w) <- wires) 
      res += w.emitDec;
    res
  }

  val reset = Bool(INPUT)
  resetList += reset
  reset.component = this
  reset.setName("reset")
  if(!(resetSignal == null)) reset := resetSignal

  def emitRef: String = if (backendName == "c") emitRefC else emitRefV;
  def emitRefC: String = emitRefV;
  def emitRefV: String = name;
  def emitDef: String = {
    val spacing = (if(verilog_parameters != "") " " else "");
    var res = "  " + moduleName + " " +verilog_parameters+ spacing + instanceName + "(";
    val hasReg = containsReg || childrenContainsReg;
    res = res + (if(hasReg) ".clk(clk), .reset(" + (if(reset.inputs.length==0) "reset" else reset.inputs(0).emitRef) + ")" else "");
    var isFirst = true;
    var nl = ""
    for ((n, w) <- wires) {
      if(n != "reset") {
	if (isFirst && !hasReg) {isFirst = false; nl = "\n"} else nl = ",\n";
	res += nl + "       ." + n + "( ";
	//if(w.isInstanceOf[IO]) println("WALKED TO " + w + ": " + w.walked);
	//if(w.isInstanceOf[IO])
	//println("COMP WALKED " + w + " is " + this.isWalked.contains(w));
	w match {
          case io: Bits  => 
            if (io.dir == INPUT) {
              if (io.inputs.length == 0) { 
                  if(saveConnectionWarnings)
		    connWriter.write("// " + io + " UNCONNECTED IN " + io.component + "\n"); 
              } else if (io.inputs.length > 1) {
                  if(saveConnectionWarnings)
		    connWriter.write("// " + io + " CONNECTED TOO MUCH " + io.inputs.length + "\n"); 
	      } else if (!this.isWalked.contains(w)){ 
                  if(saveConnectionWarnings)
		    connWriter.write("// UNUSED INPUT " +io+ " OF " + this + " IS REMOVED" + "\n");
              } else {
		res += io.inputs(0).emitRef;
              }
            } else if(io.dir == OUTPUT) {
              if (io.consumers.length == 0) {
                  if(saveConnectionWarnings)
		    connWriter.write("// " + io + " UNCONNECTED IN " + io.component + " BINDING " + findBinding(io) + "\n"); 
              } else {
		var consumer: Node = parent.findBinding(io);
		if (consumer == null) {
                  if(saveConnectionWarnings)
                    connWriter.write("// " + io + "(" + io.component + ") OUTPUT UNCONNECTED (" + io.consumers.length + ") IN " + parent + "\n"); 
		} else {
                  res += consumer.emitRef; // TODO: FIX THIS?
                }
              }
            }
	};
	res += " )";
      }
    }
    res += ");\n";
    res
  }
  def emitDefLoC: String = {
    var res = "";
    for ((n, w) <- wires) {
      w match {
        case io: Bits  => 
          if (io.dir == INPUT)
            res += "  " + emitRef + "->" + n + " = " + io.inputs(0).emitRef + ";\n";
      };
    }
    res += emitRef + "->clock_lo(reset);\n";
    for ((n, w) <- wires) {
      w match {
        case io: Bits => 
          if (io.dir == OUTPUT)
            res += "  " + io.consumers(0).emitRef + " = " + emitRef + "->" + n + ";\n";
      };
    }
    res
  }
  def emitDefHiC: String = {
    var res = emitRef + "->clock_hi(reset);\n";
    res
  }
  // COMPILATION OF BODY
  def emitDefs: StringBuilder = {
    val res = new StringBuilder()
    for (m <- mods) {
      res.append(m.emitDef)
    }
    for (c <- children) {
      res.append(c.emitDef)
    }
    res
  }
  def emitRegs: StringBuilder = {
    val res = new StringBuilder();
    res.append("  always @(posedge clk) begin\n");
    for (m <- mods) {
      res.append(m.emitReg)
    }
    res.append("  end\n");
    res
  }
  def emitDecs: StringBuilder = {
    val res = new StringBuilder();
    for (m <- mods) {
      res.append(m.emitDec)
    }
    res
  }
  def isInferenceTerminal(m: Node): Boolean = {
    m.isFixedWidth || (
      m match { 
        case io: Bits => io.dir != null; 
        case b: Binding => true; 
        case _ => false }
    )
    /*
    var isAllKnown = true;
    for (i <- m.inputs) {
      if (i.width == -1)
        isAllKnown = false;
    }
    isAllKnown
    */
  }


  def initializeBFS: Queue[Node] = {
    val res = new Queue[Node]

    // initialize bfsQueue
    for((n, elm) <- io.flatten) 
      if(elm.isInstanceOf[Bits] && elm.asInstanceOf[Bits].dir == OUTPUT)
  	res.enqueue(elm)
    for(a <- asserts) 
      res.enqueue(a)
    for(b <- blackboxes) 
      res.enqueue(b.io)
    
    for(r <- resetList)
      res.enqueue(r)

    res
  }

  def inferAll(): Unit = {
    println("started inference")
    var nodesList = ArrayBuffer[Node]()
    val walked = new HashSet[Node]
    val bfsQueue = initializeBFS

    // conduct bfs to find all reachable nodes
    while(!bfsQueue.isEmpty){
      val top = bfsQueue.dequeue
      walked += top
      nodesList += top
      for(i <- top.inputs) 
        if(!(i == null)) {
  	  if(!walked.contains(i)) {
  	    bfsQueue.enqueue(i) 
            walked += i
  	  }
        }
    }
    var count = 0

    // bellman-ford to infer all widths
    for(i <- 0 until nodesList.length) {

      var done = true;
      for(elm <- nodesList){
	val updated = elm.infer
  	done = done && !updated
	//done = done && !(elm.infer) TODO: why is this line not the same as previous two?
      }

      count += 1

      if(done){
  	for(elm <- nodesList)
  	  if (elm.infer || elm.width == -1) println("Error");
  	println(count)
        println("finished inference")
  	return;
      }
    }
    for(elm <- nodesList)
      if (elm.infer || elm.width == -1) println("Error");

    println(count)
    println("finished inference")
  }

  def removeTypeNodes() {
    println("started flattenning")
    val walked = new HashSet[Node]
    val bfsQueue = initializeBFS

    def getNode(x: Node): Node = {
      var res = x
      while(res.isTypeNode && res.inputs.length != 0){
	res = res.inputs(0)
      }
      res
    }

    var count = 0

    while(!bfsQueue.isEmpty) {
      val top = bfsQueue.dequeue
      top.fixName()
      walked += top
      count += 1

      for(i <- 0 until top.inputs.length) {
        if(!(top.inputs(i) == null)) {
          if(top.inputs(i).isTypeNode) top.inputs(i) = getNode(top.inputs(i))
          if(!walked.contains(top.inputs(i))) {
            bfsQueue.enqueue(top.inputs(i))
            walked += top.inputs(i)
          }
        }
      }

    }
    
    println(count)
    println("finished flattening")
  }

  def forceMatchingWidths = {
    println("start width checking")

    var nodesList = ArrayBuffer[Node]()
    val walked = new HashSet[Node]
    val bfsQueue = initializeBFS

    // conduct bfs to find all reachable nodes
    while(!bfsQueue.isEmpty){
      val top = bfsQueue.dequeue
      walked += top
      nodesList += top
      for(i <- top.inputs) 
        if(!(i == null)) {
  	  if(!walked.contains(i)) {
  	    bfsQueue.enqueue(i) 
            walked += i
  	  }
        }
    }


    for(node <- nodesList) {

      if(node.inputs.length == 1 && node.isInstanceOf[Bits]) {

	if (node.width > node.inputs(0).width){

	  if(node.inputs(0).isInstanceOf[Fix]){
	    val topBit = NodeExtract(node.inputs(0), node.inputs(0).width-1); topBit.infer
	    val fill = NodeFill(node.width - node.inputs(0).width, topBit); fill.infer
	    val res = Concatenate(fill, node.inputs(0)); res.infer
	    node.inputs(0) = res
	  } else {
	    val topBit = Literal(0,1)
	    val fill = NodeFill(node.width - node.inputs(0).width, topBit); fill.infer
	    val res = Concatenate(fill, node.inputs(0)); res.infer
	    node.inputs(0) = res
	  }

	} else if (node.width < node.inputs(0).width) {
	  val res = NodeExtract(node.inputs(0), node.width-1, 0); res.infer
	  node.inputs(0) = res
	}

      }

    }

    println("finished width checking")
  }

  def findConsumers() = {
    for (m <- mods) {
      m.addConsumers;
    }
  }
  def findRoots(): ArrayBuffer[Node] = {
    val roots = new ArrayBuffer[Node];
    for (a <- asserts) 
      roots += a.cond;
    for (b <- blackboxes) 
      roots += b.io;
    for (m <- mods) {
      m match {
        case io: Bits          => if (io.dir == OUTPUT) { if (io.consumers.length == 0) roots += m; }
        case d: Delay        => roots += m;
	case mr: MemRef[ _ ] => if(mr.isReg) roots += m;
        case any             =>
      }
    }
    roots
  }
  def visitNodes(roots: Array[Node]) = {
    val stack = new Stack[(Int, Node)]();
    for (root <- roots)
      stack.push((0, root));
    isWalked.clear();
    while (stack.length > 0) {
      val (newDepth, node) = stack.pop();
      val comp = node.componentOf;
      if (newDepth == -1) 
        comp.omods += node;
      else {
        node.depth = max(node.depth, newDepth);
        if (!comp.isWalked.contains(node)) {
          comp.isWalked += node;
          node.walked = true;
          stack.push((-1, node));
          for (i <- node.inputs) {
            if (i != null) {
              i match {
                case m: MemRef[ _ ] => if(!m.isReg) stack.push((newDepth+1, i));
                case d: Delay       => ;
                case o              => stack.push((newDepth+1, o)); 
              }
            }
          }
        }
      }
    }
  }
  def findOrdering() = visitNodes(findRoots().toArray);

  def findGraphDims(): (Int, Int, Int) = {
    var maxDepth = 0;
    val imods = new ArrayBuffer[Node]();
    for (m <- mods) {
      m match {
        case l: Literal =>
        case i      => imods += m;
      }
    }
    val whist = new HashMap[Int, Int]();
    for (m <- imods) {
      val w = m.width;
      if (whist.contains(w))
        whist(w) = whist(w) + 1;
      else
        whist(w) = 1;
    }
    val hist = new HashMap[String, Int]();
    for (m <- imods) {
      var name = m.getClass().getName();
      m match {
        case m: Mux => name = "Mux";
        case op: Op => name = op.op;
        case o      => name = name.substring(name.indexOf('.')+1);
      }
      if (hist.contains(name))
        hist(name) = hist(name) + 1;
      else
        hist(name) = 1;
    }
    for (m <- imods) 
      maxDepth = max(m.depth, maxDepth);
    // for ((n, c) <- hist) 
    println("%6s: %s".format("name", "count"));
    for (n <- hist.keys.toList.sortWith((a, b) => a < b)) 
      println("%6s: %4d".format(n, hist(n)));
    println("%6s: %s".format("width", "count"));
    for (w <- whist.keys.toList.sortWith((a, b) => a < b)) 
      println("%3d: %4d".format(w, whist(w)));
    var widths = new Array[Int](maxDepth+1);
    for (i <- 0 until maxDepth+1)
      widths(i) = 0;
    for (m <- imods) 
      widths(m.depth) = widths(m.depth) + 1;
    var numNodes = 0;
    for (m <- imods) 
      numNodes += 1;
    var maxWidth = 0;
    for (i <- 0 until maxDepth+1)
      maxWidth = max(maxWidth, widths(i));
    (numNodes, maxWidth, maxDepth)
  }
  def collectNodes(c: Component) = {
    for (m <- c.mods) {
      // println("M " + m.name);
      m match {
        case io: Bits  => 
          if (io.dir == INPUT) 
            inputs += m;
          else if (io.dir == OUTPUT)
            outputs += m;
        case r: Reg    => regs += r;
        case other     =>
      }
    }
  }
  def traceableNodes = io.traceableNodes;
  def childrenContainsReg: Boolean = {
    var res = containsReg;
    if(children.isEmpty) return res; 
    for(child <- children){
      res = res || child.containsReg || child.childrenContainsReg;
      if(res) return res;
    }
    res
  }
  def markComponent() = {
    name_it();
    ownIo();
    io.name_it("io", true);
    // println("COMPONENT " + name);
    val c = getClass();
    for (m <- c.getDeclaredMethods) {
      val name = m.getName();
      // println("LOOKING FOR " + name);
      val types = m.getParameterTypes();
      if (types.length == 0 && name != "test") {
        val o = m.invoke(this);
        o match { 
	  //case comp: Component => { comp.component = this;}
          case node: Node => { if ((node.isTypeNode || (node.name == "" && !node.named) || node.name == null || name != "")) node.name_it(name, true);
			       if (node.isReg || node.isRegOut || node.isClkInput) containsReg = true;
			      nameSpace += name;
			    }
	  case buf: ArrayBuffer[Node] => {
	    var i = 0;
	    if(!buf.isEmpty && buf(0).isInstanceOf[Node]){
	      for(elm <- buf){
		if ((elm.isTypeNode || (elm.name == "" && !elm.named) || elm.name == null)) 
		  elm.name_it(name + "_" + i, true);
		if (elm.isReg || elm.isRegOut || elm.isClkInput) 
		  containsReg = true;
		nameSpace += name + "_" + i;
		i += 1;
	      }
	    }
	  }
          // TODO: THIS CASE MAY NEVER MATCH
	  case bufbuf: ArrayBuffer[ArrayBuffer[ _ ]] => {
	    var i = 0;
	    println(name);
	    for(buf <- bufbuf){
	      var j = 0;
	      for(elm <- buf){
		elm match {
		  case node: Node => {
		    if ((node.isTypeNode || (node.name == "" && !node.named) || node.name == null)) 
		      node.name_it(name + "_" + i + "_" + j, true);
		    if (node.isReg || node.isRegOut || node.isClkInput) 
		      containsReg = true;
		    nameSpace += name + "_" + i + "_" + j;
		    j += 1;
		  }
		  case any =>
		}
	      }
	      i += 1;
	    }
	  }
	  case cell: Cell => { cell.name = name;
			       cell.named = true;
			      if(cell.isReg) containsReg = true;
			      nameSpace += name;
			    }
	  case bb: BlackBox => {
            if(!bb.named) {bb.instanceName = name; bb.named = true};
            bb.pathParent = this;
            //bb.name = name;
            //bb.named = true;
            for((n, elm) <- io.flatten) {
              if (elm.isClkInput) containsReg = true
            }
	    nameSpace += name;
          }
	  case comp: Component => {
            if(!comp.named) {comp.instanceName = name; comp.named = true};
            comp.pathParent = this;
	    nameSpace += name;
          }
          case any =>
        }
      }
    }
  }

  def doCompileV(out: java.io.FileWriter, depth: Int): Unit = {
    // println("COMPILING COMP " + name);
    println("// " + depthString(depth) + "COMPILING " + this + " " + children.length + " CHILDREN");
    for (top <- children)
      top.doCompileV(out, depth+1);
    findConsumers();
    if(!ChiselErrors.isEmpty){
      for(err <- ChiselErrors) err.printError;
      throw new IllegalStateException("CODE HAS " + ChiselErrors.length +" ERRORS");
    }
    collectNodes(this);
    val hasReg = containsReg || childrenContainsReg;
    val res = new StringBuilder()
    res.append((if (hasReg) "input clk, input reset" else ""));
    var first = true;
    var nl = "";
    for ((n, w) <- wires) {
      if(first && !hasReg) {first = false; nl = "\n"} else nl = ",\n";
      w match {
        case io: Bits => {
          if (io.dir == INPUT) {
	    res.append(nl + "    input " + io.emitWidth + " " + io.emitRef);
          } else if(io.dir == OUTPUT) {
	    res.append(nl + "    output" + io.emitWidth + " " + io.emitRef);
          }
        }
      };
    }
    res.append(");\n\n");
    // TODO: NOT SURE EXACTLY WHY I NEED TO PRECOMPUTE TMPS HERE
    for (m <- mods)
      m.emitTmp;
    res.append(emitDecs);
    res.append("\n");
    res.append(emitDefs);
    if (regs.size > 0) {
      res.append("\n");
      res.append(emitRegs);
    }
    res.append("endmodule\n\n");
    if(compDefs contains res){
      moduleName = compDefs(res);
    }else{
      if(compDefs.values.toList contains name) {
	moduleName = genCompName(name);
      } else {
	moduleName = name;
      }
      compDefs += (res -> moduleName);
      res.insert(0, "module " + moduleName + "(");
      out.append(res);
    }
  }
  def compileV(): Unit = {
    topComponent = this;
    components.foreach(_.elaborate(0));
    for (c <- components)
      c.markComponent();
    genAllMuxes;
    components.foreach(_.postMarkNet(0));
    assignResets()
    removeTypeNodes()
    if(!ChiselErrors.isEmpty){
      for(err <- ChiselErrors) err.printError;
      throw new IllegalStateException("CODE HAS " + ChiselErrors.length +" ERRORS");
    }
    inferAll();
    val base_name = ensure_dir(targetDir)
    if(saveWidthWarnings)
      widthWriter = new java.io.FileWriter(base_name + name + ".width.warnings")
    forceMatchingWidths;
    nameChildren(topComponent)
    traceNodes();
    if(!ChiselErrors.isEmpty){
      for(err <- ChiselErrors) err.printError;
      throw new IllegalStateException("CODE HAS " + ChiselErrors.length +" ERRORS");
    }
    if(!dontFindCombLoop) findCombLoop();
    val out = new java.io.FileWriter(base_name + name + ".v");
    if(saveConnectionWarnings)
      connWriter = new java.io.FileWriter(base_name + name + ".connection.warnings")
    doCompileV(out, 0);
    verifyAllMuxes;
    if(saveConnectionWarnings)
      connWriter.close()
    if(ChiselErrors isEmpty)
      out.close();
    else {
      for(err <- ChiselErrors)	err.printError;
      throw new IllegalStateException("CODE HAS " + ChiselErrors.length +" ERRORS");
    }
    if (configStr.length > 0) {
      val out_conf = new java.io.FileWriter(base_name+Component.topComponent.name+".conf");
      out_conf.write(configStr);
      out_conf.close();
    }
    if(saveComponentTrace)
      printStack
    compDefs.clear;
  }

  def nameAllIO(): Unit = {
    io.name_it("");
    for (child <- children) 
      child.nameAllIO();
  }
  def genAllMuxes = {
    for (p <- procs) {
      p match {
        case b: Bits  => if(b.updates.length > 0) b.genMuxes(b.default);
        case r: Reg  => r.genMuxes(r);
        case m: Mem[_] => m.genMuxes(m);
        case mr: MemRef[_] =>
        case e: Extract =>
        case v: VecProc =>
      }
    }
  }
  def verifyAllMuxes = {
    for(m <- muxes) {
      if(m.inputs(0).width != 1 && m.component != null && (!isEmittingComponents || !m.component.isInstanceOf[BlackBox]))
	ChiselErrors += ChiselError({"Mux " + m.name + " has " + m.inputs(0).width +"-bit selector " + m.inputs(0).name}, m);
    }
  }
  def elaborate(fake: Int = 0) = {}
  def postMarkNet(fake: Int = 0) = {}
  def splitFlattenNodes(args: Seq[Node]): (Seq[Node], Seq[Node]) = {
    if (args.length == 0) {
      (Array[Node](), Array[Node]())
    } else {
      val testNodes = args.map(maybeFlatten).reduceLeft(_ ++ _).map(x => x.getNode);
      (keepInputs(testNodes), removeInputs(testNodes))
    }
  }
  def genHarness(base_name: String, name: String) = {
    val makefile = new java.io.FileWriter(base_name + name + "-makefile");
    makefile.write("CPPFLAGS = -O2 -I../ -I${CHISEL}/csrc\n\n");
    makefile.write(name + ": " + name + ".o" + " " + name + "-emulator.o\n");
    makefile.write("\tg++ -o " + name + " " + name + ".o " + name + "-emulator.o\n\n");
    makefile.write(name + ".o: " + name + ".cpp " + name + ".h\n");
    makefile.write("\tg++ -c ${CPPFLAGS} " + name + ".cpp\n\n");
    makefile.write(name + "emulator.o: " + name + "-emulator.cpp " + name + ".h\n");
    makefile.write("\tg++ -c ${CPPFLAGS} " + name + "-emulator.cpp\n\n");
    makefile.close();
    val harness  = new java.io.FileWriter(base_name + name + "-emulator.cpp");
    harness.write("#include \"" + name + ".h\"\n");
    harness.write("int main (int argc, char* argv[]) {\n");
    harness.write("  " + name + "_t* c = new " + name + "_t();\n");
    harness.write("  int lim = (argc > 1) ? atoi(argv[1]) : -1;\n");
    harness.write("  c->init();\n");
    if (isVCD)
      harness.write("  FILE *f = fopen(\"" + name + ".vcd\", \"w\");\n");
    harness.write("  for (int t = 0; lim < 0 || t < lim; t++) {\n");
    harness.write("    dat_t<1> reset = LIT<1>(t == 0);\n");
    harness.write("    if (!c->scan(stdin)) break;\n");
    harness.write("    c->clock_lo(reset);\n");
    harness.write("    c->print(stdout);\n");
    harness.write("    c->clock_hi(reset);\n");
    if (isVCD)
      harness.write("    c->dump(f, t);\n");
    harness.write("  }\n");
    harness.write("}\n");
    harness.close();
  }
  def dumpVCDScope(file: java.io.FileWriter, top: Component, names: HashMap[Node, String]): Unit = {
    file.write("    fprintf(f, \"" + "$scope module " + name + " $end" + "\\n\");\n");
    for (mod <- top.omods) {
      if (mod.component == this && mod.isInVCD) {
        file.write("    fprintf(f, \"$var wire " + mod.width + " " + names(mod) + " " + stripComponent(mod.emitRefVCD) + " $end\\n\");\n");
      
      }
    }
    for (child <- children) {
      child.dumpVCDScope(file, top, names);
    }
    file.write("    fprintf(f, \"$upscope $end\\n\");\n");
  }
  def stripComponent(s: String) = s.split("__").last
  def dumpVCD(file: java.io.FileWriter): Unit = {
    var num = 0;
    val names = new HashMap[Node, String];
    for (mod <- omods) {
      if (mod.isInVCD) {
        names(mod) = "N" + num;
        num += 1;
      }
    }
    file.write("void " + name + "_t::dump(FILE *f, int t) {\n");
    if (isVCD) {
    file.write("  if (t == 0) {\n");
    file.write("    fprintf(f, \"$timescale 1ps $end\\n\");\n");
    dumpVCDScope(file, this, names);
    file.write("    fprintf(f, \"$enddefinitions $end\\n\");\n");
    file.write("    fprintf(f, \"$dumpvars\\n\");\n");
    file.write("    fprintf(f, \"$end\\n\");\n");
    file.write("  }\n");
    file.write("  fprintf(f, \"#%d\\n\", t);\n");
    for (mod <- omods) {
      if (mod.isInVCD && !(mod.name == "reset" && mod.component == this))
        file.write(mod.emitDefVCD(names(mod)));
    }
    }
    file.write("}\n");
  }

  def getPathName: String = {
    val res = (if(instanceName != "") instanceName else name);
    if(parent == null)
      return res;
    else
      parent.getPathName + "_" + res;
  }

  def traceNodes() = {
    val queue = Stack[() => Any]();
    queue.push(() => io.traceNode(this, queue));
    for (a <- asserts)
      queue.push(() => a.traceNode(this, queue));
    for (b <- blackboxes)
      queue.push(() => b.io.traceNode(this, queue));
    while (queue.length > 0) {
      val work = queue.pop();
      work();
    }
  }

  def findCombLoop() = {
    println("BEGINNING COMBINATIONAL LOOP CHECKING")

    var nodesList = ArrayBuffer[Node]()
    val walked = new HashSet[Node]
    val bfsQueue = initializeBFS

    // initialize bfsQueue
    // search for all reachable nodes, then pass this graph into tarjanSCC

    while(!bfsQueue.isEmpty){
      val top = bfsQueue.dequeue
      walked += top
      nodesList += top

      for(i <- 0 until top.inputs.length) {
        if(!(top.inputs(i) == null)) {
          
          if(!walked.contains(top.inputs(i))) {
            bfsQueue.enqueue(top.inputs(i))
            walked += top.inputs(i)
          }
          
        }
      }
    }

    // Tarjan's strongly connected components algorithm to find loops
    println("BEGINNING SEARCHING CIRCUIT FOR COMBINATIONAL LOOP")
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
            n.sccLowlink = min(n.sccLowlink, i.sccLowlink)
          } else if(stack.contains(i)) {
            n.sccLowlink = min(n.sccLowlink, i.sccIndex)
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

    for (node <- nodesList) {
      if(node.sccIndex == -1 && !node.isInstanceOf[Delay] && !(node.isReg))
        tarjanSCC(node)
    }

 
    // check for combinational loops
    println("FINISHED ANALYZING CIRCUIT")
    var containsCombPath = false
    for (nodelist <- sccList) {
      if(nodelist.length > 1) {
        containsCombPath = true
        println("FOUND COMBINATIONAL PATH!")
        for((node, ind) <- nodelist zip nodelist.indices) {
          val ste = findFirstUserLine(node.line)
          println("  (" + ind +  ") on line " + ste.getLineNumber + 
                                  " in class " + ste.getClassName +
                                  " in file " + ste.getFileName + 
                                  ", " + node.name)
        }
      }
    }
    if(containsCombPath) throw new Exception("CIRCUIT CONTAINS COMBINATIONAL PATH")
    println("NO COMBINATIONAL LOOP FOUND")
  }
  def maybeFlatten(node: Node): Seq[Node] = {
    node match {
      case b:Bundle => 
        val buf = ArrayBuffer[Node]();
        for ((n, e) <- b.flatten) buf += e.getNode;
        buf
      case o        => 
        Array[Node](node.getNode);
    }
  }
  def isInput(node: Node) = 
    node match { case b:Bits => b.dir == INPUT; case o => false }
  def keepInputs(nodes: Seq[Node]): Seq[Node] = 
    nodes.filter(isInput)
  def removeInputs(nodes: Seq[Node]): Seq[Node] = 
    nodes.filter(n => !isInput(n))
  def findCombinationalBlock(reg: Reg): AbstractFunction = {
    val traced = new HashSet[Node];
    val outsTraced = new HashMap[Node, BitSet];
    val toVisit = new HashSet[Node];
    toVisit += reg.updateVal
    outsTraced(reg.updateVal) = BitSet(0);
    // println("FINDING UPDATE " + reg.name);

    def dequeue(): Node = {
      for (n <- toVisit)
        if(isAllOutsTraced(n)){
          toVisit.remove(n)
          return n
        }
      return null
    }

    def isAllOutsTraced(n: Node): Boolean = {
      outsTraced(n).size == n.consumers.size
    }

    var break = false
    while (!toVisit.isEmpty && !break) { 
      var node = dequeue()
      // println("VISITING " + node.name + " " + node + " " + node.getClass.getName);
      if (null == node) {
        break = true
      } else if (traced.contains(node)) {
        // println("  ALREADY TRACED");
      } else if (node.isReg || node.isInstanceOf[ListLookupRef[ _ ]]) {
        // println("  STOPPING ON REG");
      } else {
        // println("  ALL OUTS TRACED");
        Predef.assert(isAllOutsTraced(node))
        traced += node;
        for (c <- node.inputs)  {
          outsTraced(c) = outsTraced.getOrElse(c, BitSet.empty) + c.consumers.indexOf(node);
          // println("    ENQUEUEING " + c);
          toVisit += c
        }
      }
    }
    // if (traced.size > 40 && reg.name == "exe_reg_op2_data") 
    if (traced.size > 40) {
      //println("+++ FOUND COMBINATIONAL BLOCK SIZE " + traced.size + " FOR " + reg.name);
      val block = Function(reg, reg.updateVal, reg.enableSignal, traced);
      Predef.assert(!reg.enableSignal.isInstanceOf[Literal])
      reg.updateVal.consumers -= reg;
      reg.inputs(0) = block;
      reg.updateVal.consumers += reg;
      for(elm <- traced)
        while(mods.contains(elm)){
          mods -= elm
        }
      block.component = reg.component;
      block
    } else
      null
  }
  def renameNodesC(nodes: Seq[Node]) = {
    for (m <- nodes) {
      m match {
        case l: Literal => ;
        case any        => 
          if (m.name != "" && !(m == reset) && !(m.component == null)) {
	    // only modify name if it is not the reset signal or not in top component
	    if(m.name != "reset" || !(m.component == this)) 
	      m.name = m.component.getPathName + "__" + m.name;
	  }
      }
    }
  }
  def gcc(flags: String = "-O2"): Unit = {
    val allFlags = flags + " -I../ -I${CHISEL}/csrc"
    val dir = targetDir + "/"
    def run(cmd: String) = {
      val c = Process(cmd).!
      println(cmd + " RET " + c)
    }
    def link(name: String) = {
      val ac = "g++ -o " + dir + name + " " + dir + name + ".o " + dir + name + "-emulator.o"
      run(ac)
    }
    def cc(name: String) = {
      val cmd = "g++ -c -o " + dir + name + ".o " + allFlags + " " + dir + name + ".cpp"
      run(cmd)
    }
    cc(name + "-emulator")
    cc(name)
    link(name)
  }
  var testIn: InputStream = null
  var testOut: OutputStream = null
  var testInputNodes: Array[Node] = null
  var testNonInputNodes: Array[Node] = null 
  def test(vars: HashMap[Node, Node]): Boolean = {
    // println("TESTONE " + testInputNodes.length + " INPUTS " + testNonInputNodes.length + " OUTPUTS")
    for (n <- testInputNodes) {
      val v = vars.getOrElse(n, null)
      val i = if (v == null) BigInt(-1) else v.litValue() // TODO: WARN
      val s = i.toString(16)
      println(n + " = " + i)
      testOut.write(' ')
      for (c <- s)
        testOut.write(c)
      testOut.write('\n')
      testOut.flush
    }
    var isSame = true
    var c = testIn.read
    val sb = new StringBuilder()
    for (o <- testNonInputNodes) {
      sb.clear()
      def isSpace(c: Int) = c == 0x20 || c == 0x9 || c == 0xD || c == 0xA
      while (isSpace(c)) {
        // println("SKIPPING CHAR '" + c.toChar + "'")
        c = testIn.read
      }
      while (!isSpace(c)) {
        // println("READ CHAR '" + c.toChar + "'")
        sb += c.toChar
        c   = testIn.read
      }
      val s = sb.toString
      val rv = toLitVal(s)
      println("READ " + o + " = " + rv)
      if (!vars.contains(o)) {
        vars(o) = Literal(rv)
      } else {
        val tv = vars(o).litValue()
        println(o + " = " + tv)
        if (tv != rv) {
          isSame = false
          println("FAILURE")
        }
      }
    }
    isSame
  }
  def startTest: Process = {
    val cmd = targetDir + "/" + name
    val process = Process(cmd)
    val pio = new ProcessIO(in => testOut = in, out => testIn = out, err => err.close())
    val p = process.run(pio) 
    println("STARTING " + cmd)
    p
  }
  def endTest(p: Process) = {
    testOut.close()
    testIn.close()
    p.destroy()
  }
  def withTesting(body: => Boolean): Boolean = {
    var res = false
    var p: Process = null
    try {
      p = startTest
      res = body
    } finally {
      if (p != null) endTest(p)
    }
    println("TESTING = " + res)
    res
  }
  var tests: () => Boolean = () => { println("DEFAULT TESTS"); true }
  var testVars: Array[Node] = null
  def defTests(nodes: Node*)(body: => Boolean) = {
    testNodes = nodes.toArray
    val (ins, outs) = splitFlattenNodes(testNodes)
    testInputNodes = ins.toArray; testNonInputNodes = outs.toArray
    tests = () => withTesting { body }
  }
  def compileC(): Unit = {
    components.foreach(_.elaborate(0));
    for (c <- components)
      c.markComponent();
    genAllMuxes;
    components.foreach(_.postMarkNet(0));
    val base_name = ensure_dir(targetDir)
    val out_h = new java.io.FileWriter(base_name + name + ".h");
    val out_c = new java.io.FileWriter(base_name + name + ".cpp");
    println("// COMPILING " + this + "(" + children.length + ")");
    topComponent = this;
    assignResets()
    removeTypeNodes()
    if(!ChiselErrors.isEmpty){
      for(err <- ChiselErrors)	err.printError;
      throw new IllegalStateException("CODE HAS " + ChiselErrors.length + " ERRORS");
      return
    }
    inferAll();
    if(saveWidthWarnings)
      widthWriter = new java.io.FileWriter(base_name + name + ".width.warnings")
    forceMatchingWidths;
    traceNodes();
    if(!ChiselErrors.isEmpty){
      for(err <- ChiselErrors)	err.printError;
      throw new IllegalStateException("CODE HAS " + ChiselErrors.length + " ERRORS");
      return
    }
    if(!dontFindCombLoop) findCombLoop();
    for (c <- components) {
      if (!(c == this)) {
        mods       ++= c.mods;
        asserts    ++= c.asserts;
        blackboxes ++= c.blackboxes;
        debugs     ++= c.debugs;
      }
    }
    findConsumers();
    verifyAllMuxes;
    if(!ChiselErrors.isEmpty){
      for(err <- ChiselErrors)	err.printError;
      throw new IllegalStateException("CODE HAS " + ChiselErrors.length + " ERRORS");
      return
    }
    collectNodes(this);
    val funs = new ArrayBuffer[AbstractFunction];
    if (isClockGatingUpdates || isClockGatingUpdatesInline) {
    for (r <- regs) {
      if(r.isEnable) {
        if(r.enableSignal.isInstanceOf[Literal] && r.enableSignal.asInstanceOf[Literal].value == 1){
          r.isEnable = false
        } else {
          val res = findCombinationalBlock(r);
          if (!(res == null)) 
            funs += res;
        }
      }
    }
    }
    findOrdering(); // search from roots  -- create omods
    renameNodesC(omods);
    if (isReportDims) {
    val (numNodes, maxWidth, maxDepth) = findGraphDims();
    println("NUM " + numNodes + " MAX-WIDTH " + maxWidth + " MAX-DEPTH " + maxDepth);
    }
    

    if (isGenHarness)
      genHarness(base_name, name);
    out_h.write("#include \"emulator.h\"\n\n");
    out_h.write("class " + name + "_t : public mod_t {\n");
    out_h.write(" public:\n");
    val funNodes = new ArrayBuffer[Node];
    for (fun <- funs)
      funNodes ++= fun.nodes;
    renameNodesC(funNodes);
    // renameNodesC(funs);
    for (m <- (omods ++ funNodes)) {
      if(m.name != "reset" || !(m.component == this)) {
        if (m.isInObject)
          out_h.write(m.emitDecC);
        if (m.isInVCD)
          out_h.write(m.emitDecVCD);
      }
    }
    out_h.write("\n");
    out_h.write("  void init ( bool random_initialization = false );\n");
    out_h.write("  void clock_lo ( dat_t<1> reset );\n");
    out_h.write("  void clock_hi ( dat_t<1> reset );\n");
    out_h.write("  void print ( FILE* f );\n");
    out_h.write("  bool scan ( FILE* f );\n");
    out_h.write("  void dump ( FILE* f, int t );\n");
    if(isClockGatingUpdates){
      for (fun <- funs) 
        out_h.write(fun.decString);
    }
    out_h.write("};\n");
    out_h.close();

    out_c.write("#include \"" + name + ".h\"\n");
    for(str <- includeArgs) out_c.write("#include \"" + str + "\"\n"); 
    out_c.write("\n");
    out_c.write("void " + name + "_t::init ( bool random_initialization ) {\n");
    for (m <- omods) {
      out_c.write(m.emitInitC);
    }
    out_c.write("}\n");

    if(isClockGatingUpdates){
      for (fun <- funs) {
        val fun_out_c = new java.io.FileWriter(base_name + fun.name + ".cpp")
        fun_out_c.write("#include \"" + name + ".h\"\n")
        fun_out_c.write("\n")
        fun_out_c.write(fun.defString(this));
        fun_out_c.close()
      }
    }

    out_c.write("void " + name + "_t::clock_lo ( dat_t<1> reset ) {\n");
    for (m <- omods) {
      out_c.write(m.emitDefLoC);
    }
    for (a <- asserts) {
      out_c.write("  ASSERT(" + a.cond.emitRefC + ", \"" + a.message + "\");\n");
    }
    out_c.write("}\n");
    out_c.write("void " + name + "_t::clock_hi ( dat_t<1> reset ) {\n");
    for (r <- omods) 
      out_c.write(r.emitInitHiC);
    for (m <- omods) 
      out_c.write(m.emitDefHiC);
    out_c.write("}\n");
    def splitFormat(s: String) = {
      var off = 0;
      var res: List[String] = Nil;
      for (i <- 0 until s.length) {
        if (s(i) == '%') {
          if (off < i) 
            res = s.substring(off, i) :: res;
          res = "%" :: res;
          if (i == (s.length-1)) {
            println("Badly formed format argument kind: %");
          } else if (s(i+1) != 'x') {
            println("Unsupported format argument kind: %" + s(i+1));
          } 
          off = i + 2;
        }
      }
      if (off < (s.length-1))
        res = s.substring(off, s.length) :: res;
      res.reverse
    }
    out_c.write("void " + name + "_t::print ( FILE* f ) {\n");
    if (testNodes.length > 0) {
      scanArgs.clear();  scanArgs  ++= testInputNodes;    scanFormat  = ""
      printArgs.clear(); printArgs ++= testNonInputNodes; printFormat = ""
    } 
    if (printArgs.length > 0) {
      val format =
        if (printFormat == "") printArgs.map(a => "%x").reduceLeft((y,z) => z + " " + y) 
        else printFormat;
      val toks = splitFormat(format);
      var i = 0;
      for (tok <- toks) {
        if (tok(0) == '%') {
          val nodes = maybeFlatten(printArgs(i))
          for (j <- 0 until nodes.length) 
            out_c.write("  fprintf(f, \"" + (if (j > 0) " " else "") + 
                        "%s\", TO_CSTR(" + nodes(j).emitRef + "));\n");
          i += 1;
        } else {
          out_c.write("  fprintf(f, \"%s\", \"" + tok + "\");\n");
        }
      }
      out_c.write("  fprintf(f, \"\\n\");\n");
      out_c.write("  fflush(f);\n");
    }
    out_c.write("}\n");
    def constantArgSplit(arg: String) = arg.split('=');
    def isConstantArg(arg: String) = constantArgSplit(arg).length == 2;
    out_c.write("bool " + name + "_t::scan ( FILE* f ) {\n");
    if (scanArgs.length > 0) {
      val format =
        if (scanFormat == "") scanArgs.map(a => "%x").reduceLeft((y,z) => z + y) 
        else scanFormat;
      val toks = splitFormat(format);
      var i = 0;
      for (tok <- toks) {
        if (tok(0) == '%') {
          val nodes = keepInputs(maybeFlatten(scanArgs(i)))
          for (j <- 0 until nodes.length) 
            out_c.write("  str_to_dat(read_tok(f), " + nodes(j).emitRef + ");\n");
          i += 1;
        } else {
          out_c.write("  fscanf(f, \"%s\", \"" + tok + "\");\n");
        }
      }
      // out_c.write("  getc(f);\n");
    }
    out_c.write("  return(!feof(f));\n");
    out_c.write("}\n");
    dumpVCD(out_c);
    out_c.close();
    if(saveComponentTrace)
      printStack
    def isDottable (m: Node) = {
      if (m == reset) {
        false
      } else {
        m match {
          case x: Literal  => false;
          case x: MapNode  => false;
          case x: ListNode => false;
          case _           => true;
        }
      }
    }
    if(saveDot) {
      var gn = -1;
      val out_cd = new java.io.FileWriter(base_name + name + "_c.dot");
      out_cd.write("digraph TopTop {\n");
      out_cd.write("rankdir = LR;\n");
      def genNum = { gn += 1; gn };
      def dumpComponent (c: Component): Unit = {
        out_cd.write("subgraph cluster" + c.name + "{\n");
        out_cd.write("label = \"" + c.name + "\";\n");
        def dumpIo (n: String, d: Data): Unit = {
          d match {
            case b: Bundle => 
              out_cd.write("subgraph cluster" + n + "__" + genNum + "{\n");
              out_cd.write("node [shape=box];\n");
              out_cd.write("label = \"" + n + "\";\n");
              for ((cn, cd) <- b.elements)
                dumpIo(cn, cd);
              out_cd.write("}\n");
            case o => 
              out_cd.write(d.emitRefDot + "[label=\"" + n + "\"];\n");
              for (in <- d.inputs) 
                if (isDottable(in))
                  out_cd.write(in.emitRefDot + " -> " + d.emitRefDot + "[label=\"" + in.getWidth + "\"];\n");
          }
        }
        dumpIo("io", c.io);
        for (cc <- c.children) 
          dumpComponent(cc);
        out_cd.write("}\n");
      }
      dumpComponent(this);
      out_cd.write("}");
      out_cd.close();
      val out_d = new java.io.FileWriter(base_name + name + ".dot");
      out_d.write("digraph " + name + "{\n");
      out_d.write("rankdir = LR;\n");
      for (m <- mods) {
        if (isDottable(m)) {
          out_d.write(m.emitRefDot);
          var label  = m.dotName;
          val anyLit = m.inputs.find(x => !isDottable(x));
          if (!anyLit.isEmpty) {
            var i = 0;
            label += "(";
            for (in <- m.inputs) {
              if (i != 0) label += ", ";
              label += (if (in.isLit) in.emitRefDot else "_");
              i += 1;
            }
            label += ")";
          }
          out_d.write("[label=\"" + label + "\"];\n");
        }
      }
      for (m <- mods) {
        for (in <- m.inputs) {
          if (isDottable(m) && isDottable(in)) 
            out_d.write("  " + in.emitRefDot + " -> " + m.emitRefDot + "[label=\"" + in.getWidth + "\"];\n");
        }
      }
      out_d.write("}");
      out_d.close();
    }
  }

};

}
