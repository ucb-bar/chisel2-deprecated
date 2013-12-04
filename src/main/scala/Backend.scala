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

/** Base class for C++, verilog, etc. backends.

  Each ``Backend`` is responsible to emit source text that can simulate
  the Chisel design (Graph) as specified by a top level ``Module``.
  */
abstract class Backend {

  /* Set of keywords which cannot be used as node and component names. */
  val keywords: HashSet[String];

  var sortedComps: ArrayBuffer[Module] = null

  var traversalIndex = 0

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

  var nindex = 0
  def emitIndex : Int = { val res = nindex; nindex = nindex + 1; res }


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

  def findRoots(root: Module): Seq[Node] = {
    var res = root.outputs()
    for (comp <- root.children) {
      res ++= findRoots(comp)
    }
    res
  }

  /** Use Scala class introspection to set names of various nodes
    in the generated graph.
    */
  def nameChildren(root: Module) {
    // Name all nodes at this level
    root.io.nameIt("io");
    val nameSpace = new HashSet[String];
    /* We are going through all declarations, which can return Datas,
     ArrayBuffer[Data], BlackBox and Modules.
     Since we call invoke() to get a proper instance of the correct type,
     we have to insure the method is accessible, thus all fields
     that will generate C++ or Verilog code must be made public. */
    for (m <- root.getClass().getDeclaredMethods) {
      val name = m.getName();
      val types = m.getParameterTypes();
      if (types.length == 0
        && isPublic(m.getModifiers()) && !(Module.keywords contains name)) {
        val o = m.invoke(root);
        o match {
          case data: Data => {
            nameSpace += data.nameIt(asValidName(name)).name
          }
          case mem: Mem[_] => {
            nameSpace += mem.nameIt(asValidName(name)).name
          }
          case buf: ArrayBuffer[_] => {
           /* We would prefer to match for ArrayBuffer[Data] but that's
            impossible because of JVM constraints which lead to type erasure.
            XXX Using Seq instead of ArrayBuffer will pick up members defined
            in Module that are solely there for implementation purposes. */
           if(!buf.isEmpty && buf.head.isInstanceOf[Data]){
             val nodebuf = buf.asInstanceOf[Seq[Data]];
             var i = 0;
             for(elm <- nodebuf){
               if( elm.name == null || elm.name.isEmpty ) {
                 /* XXX This code is sensitive to when Bundle.nameIt is called.
                  Whether it is called late (elm.name is empty) or we override
                  any previous name that could have been infered,
                  this has for side-effect to create modules with the exact
                  same logic but textually different in input/output
                  parameters, hence generating unnecessary modules. */
                 elm.nameIt(asValidName(name + "_" + i));
               }
               nameSpace += elm.name;
               i += 1;
             }
           }
         }
         case bb: BlackBox => {
           if(!bb.named) {
             bb.name = name;
             bb.named = true
           };
           nameSpace += bb.name;
           /* XXX It is not just naming here. We also update the path
            to the parent (which might not be necessary anymore). */
           bb.pathParent = root;
         }
         case comp: Module => {
           if(!comp.named) {
             comp.name = asValidName(name);
             comp.named = true
           };
           nameSpace += comp.name;
           /* XXX It is not just naming here. We also update the path
            to the parent (which might not be necessary anymore). */
           comp.pathParent = root;
         }
         case any => {
           /* We have no idea what to do with class members which are
            neither of the previous types. Let's discard them. */
         }
        }
      }
    }
    /* Recursively name the nodes and components inside this root.
     This code must be executed between the root-level naming and the naming
     of bindings otherwise some identifiers will leak into the input/output
     of a module. */
    val byNames = new HashMap[String, ArrayBuffer[Module] ];
    for (c <- root.children) {
      nameChildren(c);
      if( c.name.isEmpty ) {
        /* We don't have a name because we are not dealing with
         a class member. */
        val className = extractClassName(c);
        if( byNames contains className ) {
          byNames(className).append(c);
        } else {
          byNames += (className -> ArrayBuffer[Module](c));
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


    for (bind <- root.bindings) {
      if( bind.target != null ) {
        /* the IOBound is connected. */
        var genName = if(bind.target.name.isEmpty) "" else bind.target.component.name + "_" + bind.target.name;
        if(nameSpace.contains(genName)) genName += ("_" + emitIndex);
        bind.name = asValidName(genName); // Not using nameIt to avoid override
        bind.named = true;
      }
    }

  }

  /* Returns a string derived from _name_ that can be used as a valid
   identifier for the targeted backend. */
  def asValidName( name: String ): String = {
    if (keywords.contains(name)) name + "_" else name;
  }

  def nameAll(root: Module) {
    nindex = 0
    root.name = extractClassName(root);
    nameChildren(root);
  }

  def fullyQualifiedName( m: Node ): String = {
    m match {
      case l: Literal => l.toString;
      case any       =>
        if (m.name != ""
          && m != Module.scope.resets.head && m.component != null) {
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
      case r: RegDelay =>
        if (r.name == "") r.name = "R" + emitIndex
        r.name
      case _ =>
        if(node.name == "") node.name = "T" + emitIndex
        node.name
    }
  }

  def emitRef(c: Module): String =
    c.name

  def emitDec(node: Node): String = ""

  val preElaborateTransforms = ArrayBuffer[(Module) => Unit]()
  val transforms = ArrayBuffer[(Module) => Unit]()

  def initializeDFS: Stack[Node] = {
    val res = new Stack[Node]

    /* XXX Make sure roots are consistent between initializeBFS, initializeDFS
     and findRoots.
     */
    for( c <- Module.components ) {
      for( a <- c.debugs ) {
        res.push(a)
      }
      for((n, flat) <- c.io.flatten) {
        res.push(flat.node)
      }
    }
    res
  }


  def transform(c: Module, transforms: ArrayBuffer[(Module) => Unit]): Unit = {
    for (t <- transforms)
      t(c)
  }

  def pruneUnconnectedIOs(m: Module) {
    val inputs = m.io.flatten.filter(x => x._2.node.isInstanceOf[IOBound] && x._2.node.asInstanceOf[IOBound].isDirected(INPUT))
    val outputs = m.io.flatten.filter(x => x._2.node.isInstanceOf[IOBound] && x._2.node.asInstanceOf[IOBound].isDirected(OUTPUT))

    for ((name, i) <- inputs) {
      val node = i.node
      if (node.inputs.length == 0 && m != Module.topComponent) {
        if (node.consumers.length > 0) {
          if (Module.warnInputs)
            ChiselError.warning({"UNCONNECTED INPUT " + emitRef(node) + " in COMPONENT " + node.component +
                                 " has consumers"})
          node.driveRand = true
        } else {
          if (Module.warnInputs)
            ChiselError.warning({"FLOATING INPUT " + emitRef(node) + " in COMPONENT " + node.component})
          node.prune = true
        }
      }
    }

    for ((name, o) <- outputs) {
      val node = o.node
      if (node.inputs.length == 0) {
        if (Module.warnOutputs)
          ChiselError.warning({"UNCONNETED OUTPUT " + emitRef(node) + " in component " + node.component})
        node.driveRand = true
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
    root.traversal = traversalIndex;
    traversalIndex = traversalIndex + 1;
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


  def elaborate(c: Module): Unit = {
    Module.setAsTopComponent(c)

    /* XXX If we call nameAll here and again further down, we end-up with
     duplicate names in the generated C++.
    nameAll(c) */

    /* XXX We should name all signals before error messages are generated
     so as to give a clue where problems are showing up but that interfers
     with the *bindings* (see later comment). */
    transform(c, preElaborateTransforms)
    ChiselError.info("// COMPILING " + c + "(" + c.children.length + ")");

    levelChildren(c)
    sortedComps = gatherChildren(c).sortWith(
      (x, y) => (x.level < y.level || (x.level == y.level && x.traversal < y.traversal)));

    /* compute necessary clocks and resets */
    for (comp <- sortedComps ) {
      val clks = new ByClassVisitor[Update]()
      GraphWalker.depthFirst(findRoots(comp), clks)
      comp.clocks = clks.items
      val rsts = new ByClassVisitor[Delay]()
      GraphWalker.depthFirst(findRoots(comp), rsts)
      comp.resets.clear()
      for( rst <- rsts.items.map(x => { x.reset }).filter(_ != null) ) {
        rst.isIo = true
        if( !comp.resets.contains(rst) ) comp.resets += rst
      }
    }

    ChiselError.info("started width inference")
    var updated = true
    while( updated ) {
      val inferWF = new InferWidthForward
      GraphWalker.tarjan(findRoots(c), inferWF)
      updated = inferWF.updated
    }
    val inferBW = new InferWidthBackward
    GraphWalker.tarjan(findRoots(c), inferBW)
    ChiselError.info("finished width inference")
    ChiselError.checkpoint()

    // two transforms added in Mem.scala (referenced and computePorts)
    ChiselError.info("started transforms")
    transform(c, transforms)
    ChiselError.info("finished transforms")
    ChiselError.checkpoint()

    GraphWalker.depthFirst(findRoots(c), new AddConsumersVisitor)

    /* We execute nameAll after traceNodes because bindings would not have been
       created yet otherwise. */
    nameAll(c)
    for (comp <- sortedComps ) {
      // remove unconnected outputs
      pruneUnconnectedIOs(comp)
    }
    ChiselError.checkpoint()

    if(!Module.dontFindCombLoop) {
      ChiselError.info("checking for combinational loops")
      c.findCombLoop();
      ChiselError.checkpoint()
      ChiselError.info("NO COMBINATIONAL LOOP FOUND")
    }
    if(Module.saveComponentTrace) {
      printStack
    }
    verifyAllMuxes(c)
    ChiselError.checkpoint()
  }

  def compile(c: Module, flags: String = null): Unit = { }

  def checkPorts(topC: Module) {

    def prettyPrint(n: Node, c: Module) {
      val dir = n.asInstanceOf[IOBound].dir.toString()
      val portName = n.name
      val compName = c.name
      val compInstName = c.moduleName
      ChiselError.warning(dir + " port " + portName
        + " is unconnected in module " + compInstName + " " + compName)
    }

    for (c <- Module.components) {
      if (c != topC) {
        for ((n,i) <- c.io.flatten) {
          val node = i.node
          if (node.inputs.length == 0) {
            prettyPrint(node, c)
          }
        }
      }
    }

  }

  /** Prints the call stack of Component as seen by the push/pop runtime. */
  protected def printStack {
    var res = ""
    for((i, c) <- Module.scope.printStackStruct){
      res += (genIndent(i) + c.moduleName + " " + c.name + "\n")
    }
    ChiselError.info(res)
  }

  def verifyAllMuxes(c: Module) {
    val stateVars = new ByClassVisitor[Delay]()
    GraphWalker.depthFirst(findRoots(c), stateVars)
    for( state <- stateVars.items ) {
      GraphWalker.depthFirst(state :: Nil,
        new MuxDefault(state), new OnlyMuxes)
    }
    GraphWalker.depthFirst(findRoots(c), new VerifyMuxes())
  }

}

