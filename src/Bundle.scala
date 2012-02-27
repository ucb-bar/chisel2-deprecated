// author: jonathan bachrach
package Chisel {

import scala.collection.mutable.ArrayBuffer
import java.lang.reflect.Modifier._;

import Node._;
import Component._;
import IOdir._;
import ChiselError._
import sort._

object Bundle {
  def nullbundle_t = Bundle(ArrayBuffer[(String, Data)]());
  def apply (elts: ArrayBuffer[(String, Data)]): Bundle = {
    val res = new Bundle();
    // println("NEW BUNDLE");
    res.elementsCache = elts; // TODO: REMOVE REDUNDANT CREATION
    for ((n, i) <- elts) {
      i.name = n;
      // println("  ELT " + n + " " + i);
    }
    res
  }

}

object sort {
  def apply(a: Array[(String, IO)]): Array[(String, IO)] = {
    var i = 0
    for (j <- 1 until a.length) {
      val keyElm = a(j);
      val key = ioMap(keyElm._2)
      i = j - 1

      while (i >= 0 && ioMap(a(i)._2) > key) {
        a(i + 1) = a(i)
        i = i - 1
      }
      a(i + 1) = keyElm
    }
    a
  }

  def checkPorts(x: Data, y: Data) = {

    if(x.isInstanceOf[IO] && !y.isInstanceOf[IO]) {

      val xIO = x.asInstanceOf[IO];
      val yIO = y.asInstanceOf[IO];

      if(xIO.dir == null || xIO.dir != INPUT) {
	ChiselErrors += IllegalState("left hand side of bulk := must be OUTPUT", 3);
      } else if (!yIO.isCellIO || yIO.dir != OUTPUT)
	ChiselErrors += IllegalState("right hand side of bulk := must be INPUT", 3);
    }
  }

}

class Bundle(view_arg: Seq[String] = null) extends Data{
  var dir = "";
  var view = view_arg;
  var elementsCache: ArrayBuffer[(String, Data)] = null;
  var bundledElm: Node = null;
  def calcElements(view: Seq[String]): ArrayBuffer[(String, Data)] = {
    val c      = getClass();
    var elts   = ArrayBuffer[(String, Data)]();
    val seen   = ArrayBuffer[Object]();
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
        val sc = Class.forName("Chisel.Data");
        do {
          if (c == sc) {
            isFound = true; isInterface = true;
          } else if (c == null || c == Class.forName("java.lang.Object")) {
            isFound = true; isInterface = false;
          } else 
            c = c.getSuperclass();
        } while (!isFound);
        // TODO: SPLIT THIS OUT TO TOP LEVEL LIST
        if (types.length == 0 && !isStatic(modifiers) && isInterface &&
            name != "elements" && name != "flip" && name != "toString" && name != "flatten" && name != "binding" && name != "asInput" && name != "asOutput" && name != "unary_$tilde" && name != "unary_$bang" && name != "unary_$minus" && name != "clone" && name != "toUFix" && name != "toBits" && name != "toBool" && name != "toFix" &&
            (view == null || view.contains(name)) && !seen.contains(m.invoke(this))) {
          val o = m.invoke(this);
          o match { 
	    case bv: Vec[Data] => elts += ((name + bv.name, bv));
            case i: Data => elts += ((name, i)); i.name = name; 
              // if (view != null)
              //   println("    ADDING " + name + " -> " + o + " COMP " + i.component + " DONE " + (i.component == null));
           case any =>
              // println("    FOUND " + o);
          }
          seen += o;
        }
      } else if (name == "elementsCache") 
        // println("IS-COLLECTING");
        isCollecting = true;
    }
    // println("END ->>>>");
    elts
  }
  def elements: ArrayBuffer[(String, Data)] = {
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
  def view (elts: ArrayBuffer[(String, Data)]): Bundle = { 
    elementsCache = elts; this 
  }
  override def name_it (path: String, named: Boolean = true) = {
    if(path.length > 0 && !this.named) {name = path; this.named = named};
    for ((n, i) <- elements) {
      i.name = (if (path.length > 0) path + "_" else "") + n;
      i.name_it(i.name, named);
      // println("  ELT " + n + " " + i);
    }
  }

  def +(other: Bundle): Bundle = {
    var elts = ArrayBuffer[(String, Data)]();
    for ((n, i) <- elements) 
      elts += ((n, i));
    for ((n, i) <- other.elements) 
      elts += ((n, i));
    Bundle(elts)
  }
  def +=[T <: Data](other: T) = {
    elements;
    elementsCache += ((other.name, other));
    if(isCellIO) other.setIsCellIO;
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
  override def traceableNodes = elements.map(tup => tup._2).toArray;
  override def apply(name: String): Data = {
    for((n,i) <- elements)
      if(name == n) return i;
    throw new NoSuchElementException();
    return null;
  }
  override def <>(src: Node) = { 
    // println("B <>'ing " + this + " & " + src);
    if(comp == null || (dir == "output" && 
			src.isInstanceOf[Bundle] && 
			src.asInstanceOf[Bundle].dir == "output")){
      src match {
	case other: Bundle => {
          for ((n, i) <- elements) {
            if (other.contains(n)){
              i <> other(n);
	    }
            else{
              println("// UNABLE TO FIND " + n + " IN " + other.component);
	    }
          }
	}
	case default =>
          println("// TRYING TO CONNECT BUNDLE TO NON BUNDLE " + default);
      }
    } else {
      src match { 
	case other: Bundle => {
	  comp assign other.toNode
	}
	case default =>
	  println("CONNECTING INCORRECT TYPES INTO WIRE OR REG")
      }
    }
  }
  override def ^^(src: Node) = { 
    // println("B <>'ing " + this + " & " + src);
    src match {
      case other: Bundle =>
        for ((n, i) <- elements) {
          if(other.contains(n)) {
            // println(" := ELT " + i + " & " + other(n));
            i ^^ other(n);
          }
        }
    }
  }

  def contains(name: String): Boolean = {
    for((n,i) <- elements)
      if(n == name) return true;
    return false;
  }

  def :=(src: Bundle) = {
    src match {
      case other: Bundle => {

	if (this.comp != null) {
	  this.comp procAssign other.toNode;
	} else {
	  for ((n, i) <- elements) {
	    if(other.contains(n)){
	      checkPorts(i, other(n));
	      i := other(n)
	    }
	    else {
	      println("// UNABLE TO FIND " + n + " IN " + other.component);
	    }
	  }
	}
	
      }
      case default =>
	println("// TRYING TO := BUNDLE TO NON BUNDLE " + default);
    }
  }

  override def flatten: Array[(String, IO)] = {
    var res = ArrayBuffer[(String, IO)]();
    for ((n, i) <- elements){
      res = res ++ i.flatten
    }
    sort(res.toArray)
  }

  override def getWidth(): Int = {
    var w = 0
    for((name, io) <- res.flatten)
      w += io.getWidth
    w
  }

  override def toNode: Node = {
    if(bundledElm == null) {
      val nodes = flatten.map{case (n, i) => i};
      bundledElm = Concatanate(nodes.head, nodes.tail.toList: _*)
    }
    bundledElm
  }
  
  override def fromNode(n: Node): this.type = {
    val res = this.clone()
    var ind = 0;
    for((name, io) <- res.flatten.toList.reverse) {
      io.asOutput();
      if(io.width > 1) io assign NodeExtract(n, ind + io.width-1, ind) else io assign NodeExtract(n, ind);
      ind += io.width;
    }
    res
  }

  override def asInput(): this.type = {
    for ((n, i) <- elements)
      i.asInput();
    this.dir = "input";
    this
  }

  override def asOutput(): this.type = {
    for ((n, i) <- elements)
      i.asOutput();
    this.dir = "output"
    this
  }
  override def setIsCellIO() = {
    isCellIO = true;
    for ((n, i) <- elements)
      i.setIsCellIO
  }
}
}
