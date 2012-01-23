// author: jonathan bachrach
package Chisel {

import scala.collection.mutable.ArrayBuffer
import java.lang.reflect.Modifier._;

import Node._;
import Component._;
import IOdir._;

object Bundle {
  def nullbundle_t = Bundle(Map[String, Data]());
  def apply (elts: Map[String, Data]): Bundle = {
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

class Bundle(view_arg: Seq[String] = null) extends Data{
  var dir = "";
  var view = view_arg;
  var elementsCache: Map[String, Data] = null;
  var bundledElm: Node = null;
  def calcElements(view: Seq[String]): Map[String, Data] = {
    val c      = getClass();
    var elts   = Map[String, Data]();
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
        if (types.length == 0 && !isStatic(modifiers) && isInterface &&
            name != "elements" && name != "flip" && name != "toString" && name != "flatten" && name != "binding" && name != "asInput" && name != "asOutput" && name != "unary_$tilde" && name != "unary_$bang" && name != "unary_$minus" && name != "clone" && name != "toUFix" && name != "toBits" && name != "toBool" && name != "toFix" &&
            (view == null || view.contains(name))) {
          val o = m.invoke(this);
          o match { 
	    case bv: Vec[Data] => elts += ((name + bv.name, bv));
            case i: Data => elts += ((name, i)); i.name = name; 
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
  def elements: Map[String, Data] = {
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
  def view (elts: Map[String, Data]): Bundle = { 
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
    var elts = Map[String, Data]();
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
  override def apply(name: String): Data = elements(name)
  override def <>(src: Node) = { 
    // println("B <>'ing " + this + " & " + src);
    if(comp == null || (dir == "output" && 
			src.isInstanceOf[Bundle] && 
			src.asInstanceOf[Bundle].dir == "output")){
      src match {
	case other: Bundle => {
          for ((n, i) <- elements) {
            if (other.elements.contains(n)){
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
          if(other.elements.contains(n)) {
            // println(" := ELT " + i + " & " + other(n));
            i ^^ other(n);
          }
        }
    }
  }
  def <==(src: Bundle) = {
    src match {
      case other: Bundle => {
	if (this.comp != null) {
	  this.comp procAssign other.toNode;
	} else {
	  println("INCORRECT USE OF <== ON BUNDLES");
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
