// author: jonathan bachrach
package Chisel {

import scala.collection.mutable.ArrayBuffer
import java.lang.reflect.Modifier._;

import Node._;
import Component._;

object Bundle {
  def nullBundle = Bundle(Map[String, Interface]());
  def apply (elts: Map[String, Interface]): Bundle = {
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

class Bundle(view_arg: Seq[String] = null) extends Interface {
  var view = view_arg;
  var elementsCache: Map[String, Interface] = null;
  def calcElements(view: Seq[String]): Map[String, Interface] = {
    val c      = getClass();
    var elts   = Map[String, Interface]();
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
        val sc = Class.forName("Chisel.Interface");
        do {
          if (c == sc) {
            isFound = true; isInterface = true;
          } else if (c == null || c == Class.forName("java.lang.Object")) {
            isFound = true; isInterface = false;
          } else 
            c = c.getSuperclass();
        } while (!isFound);
        if (types.length == 0 && !isStatic(modifiers) && isInterface &&
            name != "elements" && name != "flip" && name != "toString" && name != "flatten" && name != "binding" &&
            (view == null || view.contains(name))) {
          val o = m.invoke(this);
          o match { 
	    case bv: BundleVec[Interface] => elts += ((name, bv)); for(elm <- bv.bundleVector) elm.name = name + elm.name;
            case i: Interface => elts += ((name, i)); i.name = name; 
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
  def elements: Map[String, Interface] = {
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
  def view (elts: Map[String, Interface]): Bundle = { 
    elementsCache = elts; this 
  }
  override def name_it (path: String) = {
    for ((n, i) <- elements) {
      i.name = (if (path.length > 0) path + "_" else "") + n;
      i.name_it(i.name);
      // println("  ELT " + n + " " + i);
    }
  }
  def +(other: Bundle): Bundle = {
    var elts = Map[String, Interface]();
    for ((n, i) <- elements) 
      elts += ((n, i));
    for ((n, i) <- other.elements) 
      elts += ((n, i));
    Bundle(elts)
  }
  override def flip(): this.type = {
    for ((n, i) <- elements) 
      i.flip()
    this
  }
  override def findNodes(depth: Int, c: Component): Unit = {
    for ((n, elt) <- elements) 
      elt.findNodes(depth, c);
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
  override def apply(name: String): Interface = elements(name)
  override def <>(src: Node) = { 
    // println("B <>'ing " + this + " & " + src);
    src match {
      case other: Bundle => {
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
      case other: Bundle =>
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
}

}
