// author: jonathan bachrach
package Chisel {

import scala.collection.mutable.ArrayBuffer
import java.lang.reflect.Modifier._;

import Node._;
import Component._;
import IOdir._;

object bundle_t {
  def nullbundle_t = bundle_t(Map[String, dat_t]());
  def apply (elts: Map[String, dat_t]): bundle_t = {
    val res = new bundle_t();
    // println("NEW BUNDLE");
    res.elementsCache = elts; // TODO: REMOVE REDUNDANT CREATION
    for ((n, i) <- elts) {
      i.name = n;
      // println("  ELT " + n + " " + i);
    }
    res
  }
}

class bundle_t(view_arg: Seq[String] = null) extends dat_t{
  var view = view_arg;
  var elementsCache: Map[String, dat_t] = null;
  var bundledElm: Node = null;
  def calcElements(view: Seq[String]): Map[String, dat_t] = {
    val c      = getClass();
    var elts   = Map[String, dat_t]();
    var isCollecting = true;
    // println("COLLECTING " + c + " IN VIEW " + view);
    for (m <- c.getMethods) {
      val name = m.getName();
      if (isCollecting) {
        val modifiers = m.getModifiers();
        // println("  CHECKING " + name + " MODS " + modifiers);
        val types = m.getParameterTypes();
        val rtype = m.getReturnType();
        var isFound = false;
        var isInterface = false;
        var c = rtype;
        val sc = Class.forName("Chisel.dat_t");
        do {
          if (c == sc) {
            isFound = true; isInterface = true;
          } else if (c == null || c == Class.forName("java.lang.Object")) {
            isFound = true; isInterface = false;
          } else 
            c = c.getSuperclass();
        } while (!isFound);
        if (types.length == 0 && !isStatic(modifiers) && isInterface &&
            name != "elements" && name != "flip" && name != "toString" && name != "flatten" && name != "binding" && name != "asInput" && name != "asOutput" && name != "unary_$tilde" && name != "unary_$bang" && name != "unary_$minus" && name != "clone" &&
            (view == null || view.contains(name))) {
          val o = m.invoke(this);
          o match { 
	    case bv: BundleVec[dat_t] => elts += ((name + bv.name, bv));
            case i: dat_t => elts += ((name, i)); i.name = name; 
              // println("    ADDING " + name + " -> " + o);
            case any =>
              // println("    FOUND " + o);
          }
        }
      } else if (name == "elementsCache") 
        // println("IS-COLLECTING");
        isCollecting = true;
    }
    // println("END ->>>>");
    elts
  }
  def elements: Map[String, dat_t] = {
    if (elementsCache == null) {
      elementsCache = calcElements(view);
    }
    elementsCache
  }
  override def toString: String = {
    val init = "BUNDLE(";
    var res = init;
    for ((n, i) <- elements) {
      if (res.length() > init.length()) res += ", ";
      res += n + " => " + i;
    }
    res += ")";
    res
  }
  def view (elts: Map[String, dat_t]): bundle_t = { 
    elementsCache = elts; this 
  }
  override def name_it (path: String, named: Boolean = true) = {
    if(path.length > 0 && name == "") name = path;
    for ((n, i) <- elements) {
      i.name = (if (path.length > 0) path + "_" else "") + n;
      i.name_it(i.name, named);
      // println("  ELT " + n + " " + i);
    }
  }

  def +(other: bundle_t): bundle_t = {
    var elts = Map[String, dat_t]();
    for ((n, i) <- elements) 
      elts += ((n, i));
    for ((n, i) <- other.elements) 
      elts += ((n, i));
    bundle_t(elts)
  }
  def +=(other: int_t) = {
    elements;
    elementsCache += ((other.name, other));
  }
  override def flip(): this.type = {
    for ((n, i) <- elements) {
      i.flip()
    }
    this
  }
  override def removeCellIOs() = {
    for ((n, elt) <- elements)
      elt.removeCellIOs
  }
  override def findNodes(depth: Int, c: Component): Unit = {
    for ((n, elt) <- elements){
      elt.removeCellIOs;
      elt.findNodes(depth, c);
    }
    /*
    val elts = flatten;
    println(depthString(depth) + "BUNDLE " + this + " => " + elts);
    for ((n, elt) <- elts) {
      println(depthString(depth+1) + "I: " + elt);
      elt match { case i: Input => i.findNodes(depth); case o => ; }
    }
    for ((n, elt) <- elts) {
      println(depthString(depth+1) + "O: " + elt);
      elt match { case o: Output => o.findNodes(depth); case i => ; }
    }
    */
  }
  override def apply(name: String): dat_t = elements(name)
  override def <>(src: Node) = { 
    // println("B <>'ing " + this + " & " + src);
    src match {
      case other: bundle_t => {
        for ((n, i) <- elements) {
          if (other.elements.contains(n))
            i <> other(n);
          else{
            println("// UNABLE TO FIND " + n + " IN " + other.component);
	  }
        }
      }
      case default =>
        println("// TRYING TO CONNECT BUNDLE TO NON BUNDLE " + default);
    }
  }
  override def ^^(src: Node) = { 
    // println("B <>'ing " + this + " & " + src);
    src match {
      case other: bundle_t =>
        for ((n, i) <- elements) {
          if(other.elements.contains(n)) {
            // println(" := ELT " + i + " & " + other(n));
            i ^^ other(n);
          }
        }
    }
  }
  override def flatten: Array[(String, IO)] = {
    var res = ArrayBuffer[(String, IO)]();
    for ((n, i) <- elements)
      res = res ++ i.flatten
    res.toArray
  }

  override def toBits: Node = {
    if(bundledElm == null) {
      val nodes = flatten.map{case (n, i) => i};
      bundledElm = Concatanate(nodes.head, nodes.tail.toList: _*)
    }
    bundledElm
  }

  override def fromBits(n: Node): this.type = {
    val res = this.getClass.newInstance.asInstanceOf[this.type];
    var ind = 0;
    for((name, io) <- res.flatten.toList.reverse) {
      io.asOutput();
      if(io.width > 1) io := Bits(n, ind + io.width-1, ind) else io := Bits(n, ind);
      ind += io.width;
    }
    res
  }

  override def asInput(): this.type = {
    for ((n, i) <- elements)
      i.asInput();
    this
  }

  override def asOutput(): this.type = {
    for ((n, i) <- elements)
      i.asOutput();
    this
  }
  override def setIsCellIO() = {
    isCellIO = true;
    for ((n, i) <- elements)
      i.setIsCellIO
  }
}
}
