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
import scala.collection.immutable.HashSet
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack
import java.lang.reflect.Modifier._
import Node._;
import ChiselError._
import sort._

object Bundle {
  val keywords = HashSet[String]("elements", "flip", "toString",
    "flatten", "binding", "asInput", "asOutput", "unary_$tilde",
    "unary_$bang", "unary_$minus", "clone", "toUFix", "toBits",
    "toBool", "toFix", "asDirectionless")

  def apply (elts: ArrayBuffer[(String, Data)]): Bundle = {
    val res = new Bundle();
    res.elementsCache = elts; // TODO: REMOVE REDUNDANT CREATION
    res
  }

}

object sort {
  def apply(a: Array[(String, Bits)]): Array[(String, Bits)] = {
    var i = 0
    for (j <- 1 until a.length) {
      val keyElm = a(j);
      val key = Mod.ioMap(keyElm._2)
      i = j - 1

      while (i >= 0 && Mod.ioMap(a(i)._2) > key) {
        a(i + 1) = a(i)
        i = i - 1
      }
      a(i + 1) = keyElm
    }
    a
  }
}

/** Defines a collection of datum of different types into a single coherent
  whole.
  */
class Bundle(view_arg: Seq[String] = null) extends CompositeData {
  var dir = "";
  var view = view_arg;
  private var elementsCache: ArrayBuffer[(String, Data)] = null;
  var bundledElm: Node = null;

  /** Populates the cache of elements declared in the Bundle. */
  private def calcElements(view: Seq[String]): ArrayBuffer[(String, Data)] = {
    val c      = getClass();
    var elts   = ArrayBuffer[(String, Data)]();
    val seen   = ArrayBuffer[Object]();
    for (m <- c.getMethods) {
      val name = m.getName();
      val modifiers = m.getModifiers();
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
        } else {
          c = c.getSuperclass();
        }
      } while (!isFound);
      // TODO: SPLIT THIS OUT TO TOP LEVEL LIST
      if( types.length == 0 && !isStatic(modifiers) && isInterface
        && !(Bundle.keywords contains name)
        && (view == null || view.contains(name)) ) {
        val o = m.invoke(this);
        if( !seen.contains(o)) {
          o match {
            case bv: Vec[_] => {
              /* We would prefer to match for Vec[Data] but that's impossible
               because of JVM constraints which lead to type erasure.*/
              val datavec = bv.asInstanceOf[Vec[Data]];
              elts += ((name + datavec.name, datavec))
            }
            case i: Data => {
              elts += ((name, i));
            }
            case any =>
          }
          seen += o;
        }
      }
    }
    elts
  }

  /* XXX This method is not private since it is used in Dot.scala. */
  def elements: ArrayBuffer[(String, Data)] = {
    if (elementsCache == null) {
      elementsCache = calcElements(view);
    }
    elementsCache
  }

  override def toString: String = {
    var res = "BUNDLE(";
    var sep = "";
    for ((n, i) <- elements) {
      res += sep + n + " => " + i;
      sep = ", ";
    }
    res += ")";
    res
  }

  override def terminate(): Unit = {
    for ((n, i) <- elements)
      i.terminate();
  }

  def view (elts: ArrayBuffer[(String, Data)]): Bundle = {
    elementsCache = elts; this
  }

  override def nameIt (path: String) {
    if( !named
      && (name.isEmpty
        || (!path.isEmpty && name != path)) ) {
      name = path
      val prefix = if (name.length > 0) name + "_" else ""
      for ((n, i) <- elements) {
        i.nameIt(prefix + n)
      }
    } else {
      /* We are trying to rename a Bundle that has a fixed name. */
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

  def +=[T <: Data](other: T) {
    elements;
    elementsCache += ((other.name, other));
    if(isTypeNode) other.setIsTypeNode;
  }

  override def flip(): this.type = {
    for ((n, i) <- elements) {
      i.flip()
    }
    this
  }

  override def removeTypeNodes() {
    for ((n, elt) <- elements)
      elt.removeTypeNodes
  }

  override def traceableNodes: Array[Node] = elements.map(tup => tup._2).toArray;

  override def traceNode(c: Mod, stack: Stack[() => Any]) {
    for((n, i) <- flatten) {
      stack.push(() => i.traceNode(c, stack))
    }
  }

  override def apply(name: String): Data = {
    for((n,i) <- elements)
      if(name == n) return i;
    throw new NoSuchElementException();
    return null;
  }

  override def <>(src: Node) {
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
              ChiselError.warning("UNABLE TO FIND " + n + " IN " + other.component);
            }
          }
        }
        case default =>
          ChiselError.warning("TRYING TO CONNECT BUNDLE TO NON BUNDLE " + default);
      }
    } else {
      src match {
        case other: Bundle => {
          comp assign other
        }
        case default =>
          ChiselError.warning("CONNECTING INCORRECT TYPES INTO WIRE OR REG")
      }
    }
  }

  override def ^^(src: Node) {
    src match {
      case other: Bundle =>
        for ((n, i) <- elements) {
          if(other.contains(n)) {
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

  override def :=[T <: Data](src: T): Unit = {
    src match {
      case other: Bundle => {
        this := other
      }
      case any =>
        this.asInstanceOf[Data] := src
    }
  }

  def :=(src: Bundle): Unit = {
    if(this.isTypeNode && comp != null) {
      this.comp.procAssign(src.toNode)
      return
    }
    for((n, i) <- elements) {
      i match {
        case bundle: Bundle => {
          if (src.contains(n)) bundle := src(n).asInstanceOf[Bundle]
        }
        case bits: Bits => {
          if (src.contains(n)) bits := src(n).asInstanceOf[Bits]
        }
        case vec: Vec[_] => {
           /* We would prefer to match for Vec[Data] but that's impossible
            because of JVM constraints which lead to type erasure. */
          val vecdata = vec.asInstanceOf[Vec[Data]]
          if (src.contains(n)) vecdata := src(n).asInstanceOf[Vec[Data]]
        }
      }
    }
  }

  override def flatten: Array[(String, Bits)] = {
    var res = ArrayBuffer[(String, Bits)]();
    for ((n, i) <- elements){
      res = res ++ i.flatten
    }
    sort(res.toArray)
  }

  override def getWidth(): Int = {
    var w = 0
    for((name, io) <- elements)
      w += io.getWidth
    w
  }

  override def toNode: Node = {
    if(bundledElm == null) {
      val nodes = flatten.map{case (n, i) => i};
      bundledElm = Concatenate(nodes.head, nodes.tail.toList: _*)
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

  override def asDirectionless(): this.type = {
    elements.foreach(_._2.asDirectionless)
    this.dir = ""
    this
  }

  override def asInput(): this.type = {
    elements.foreach(_._2.asInput)
    this.dir = "input"
    this
  }

  override def asOutput(): this.type = {
    elements.foreach(_._2.asOutput)
    this.dir = "output"
    this
  }

  override def isDirectionless: Boolean = {
    (dir == "") && elements.map{case (n,i) => i.isDirectionless}.reduce(_&&_)
  }

  override def setIsTypeNode() {
    isTypeNode = true;
    for ((n, i) <- elements)
      i.setIsTypeNode
  }
}
