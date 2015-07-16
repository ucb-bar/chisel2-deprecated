/*
 Copyright (c) 2011, 2012, 2013, 2014 The Regents of the University of
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
import scala.collection.mutable.HashSet
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.Stack
import java.lang.reflect.Modifier._
import Node._;
import ChiselError._

object Bundle {
  val keywords = Set("elements", "flip", "toString",
    "flatten", "binding", "asInput", "asOutput", "unary_$tilde",
    "unary_$bang", "unary_$minus", "clone", "cloneType", "toUInt", "toBits",
    "toBool", "toSInt", "asDirectionless")

  def apply[T <: Bundle](b: => T)(implicit p: Parameters): T = {
    Driver.parStack.push(p.push)
    val res = b
    Driver.parStack.pop
    res
  }
  def apply[T <: Bundle](b: => T,  f: PartialFunction[Any,Any]): T = {
    val q = params.alterPartial(f)
    apply(b)(q)
  }
  private def params = if(Driver.parStack.isEmpty) Parameters.empty else Driver.parStack.top
}

/** Defines a collection of datum of different types into a single coherent
  whole.
  */
class Bundle(val view: Seq[String] = null) extends Aggregate {
  /** Populates the cache of elements declared in the Bundle. */
  private def calcElements(view: Seq[String]) = {
    val c      = getClass
    var elts   = LinkedHashMap[String, Data]() 
    val seen   = HashSet[Object]()
    for (m <- c.getMethods.sortWith(
      (x, y) => (x.getName < y.getName)
    )) {
      val name = m.getName
      val modifiers = m.getModifiers
      val types = m.getParameterTypes

      val rtype = m.getReturnType
      val isInterface = classOf[Data].isAssignableFrom(rtype)

      // TODO: SPLIT THIS OUT TO TOP LEVEL LIST
      if( types.length == 0 && !isStatic(modifiers) && isInterface
        && !(name contains '$')
        && !(Bundle.keywords contains name)
        && (view == null || (view contains name))
        && checkPort(m, name)) {
        // Fetch the actual object
        val obj = m invoke this
        if(!(seen contains obj)) {
          obj match {
            case d: Data => elts(name) = d
            case any =>
          }
          seen += obj
        }
      }
    }
    // Chisel3 - compatibility - use cloneType instead of clone
    if (Driver.minimumCompatibility > "2") {
      val methodNames = c.getDeclaredMethods.map(_.getName())
      if (methodNames.contains("clone") && !methodNames.contains("cloneType")) {
        // Use the line number for the bunde definition (if we have it).
        val errorLine = if (line != null) {
          line
        } else {
          val stack = Thread.currentThread().getStackTrace
          findFirstUserLine(stack) getOrElse stack(0)
        }
        ChiselError.warning("method \"clone\" is deprecated. Please use \"cloneType\"", errorLine)
      }
    }
    elts
  }

  protected def checkPort(obj : Any, name : String) : Boolean = true

  lazy val elements = calcElements(view)

  def fromMap(elemmap: Map[String, Data]): this.type = {
    // only well defined for 'flat' bundles, for now
    val result = this.cloneType
    elemmap.foreach({case (subfield: String, source: Data) => {
      result.elements.get(subfield) match {
        case Some(sink: Data) => sink := source
        case None => ChiselError.error(s"In fromMap, Map attempts to index subfield ${subfield}, which does not exist in ${result.getClass.getName}")
      }
    }})
    result.asInstanceOf[this.type]
  }

  override def toString: String = {
    var res = "BUNDLE("
    var sep = ""
    for ((n, i) <- elements) {
      res += sep + n + " => " + i
      sep = ", "
    }
    res += ")"
    res
  }

  override def nameIt (path: String, isNamingIo: Boolean) {
    if( !named
      && (name.isEmpty
        || (!path.isEmpty && name != path)) ) {
      name = path
      val prefix = if (name.length > 0) name + "$" else ""
      for ((n, i) <- elements) {
        i.nameIt(prefix + n, isNamingIo)
      }
    } else {
      /* We are trying to rename a Bundle that has a fixed name. */
    }
  }

  def +(other: Bundle): Bundle = {
    val res = new Bundle
    res.elements ++= elements
    res.elements ++= other.elements
    res
  }

  override def flip(): this.type = {
    elements foreach (_._2.flip)
    this
  }

  override def removeTypeNodes {
    elements foreach (_._2.removeTypeNodes)
  }

  def contains(name: String): Boolean = elements contains name 

  override def apply(name: String): Data = elements(name)

  override def <>(src: Node): Unit = {
    if (comp == null) {
      src match {
        case other: Bundle => {
          for ((n, i) <- elements) {
            if (other contains n){
              i <> other(n)
            } else {
              ChiselError.warning("UNABLE TO FIND " + n + " IN " + other.component)
            }
          }
        }
        case default =>
          ChiselError.warning("TRYING TO CONNECT BUNDLE TO NON BUNDLE " + default)
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

  override protected def colonEquals(src: Bundle): Unit = {
    if (isTypeNode && comp != null) {
      comp procAssign src.toNode
    } else {
      for ((n, i) <- elements ; if src contains n) i := src(n)
    }
  }

  override def flatten: Array[(String, Bits)] = {
    val sortedElems = elements.toArray sortWith (_._2._id < _._2._id) 
    (sortedElems foldLeft Array[(String, Bits)]()){(res, x) => 
      val (n, i) = x
      res ++ (if (i.name != "") i.flatten else i match {
        case b: Bits => Array((n, b))
        case _ => i.flatten map (x => (n + "_" + x._1, x._2))
      })
    }
  }

  override def getWidth: Int = (elements foldLeft 0)(_ + _._2.getWidth)

  override def asDirectionless(): this.type = {
    elements.foreach(_._2.asDirectionless)
    this
  }

  override def asInput(): this.type = {
    elements.foreach(_._2.asInput)
    this
  }

  override def asOutput(): this.type = {
    elements.foreach(_._2.asOutput)
    this
  }

  override def isDirectionless: Boolean = elements.forall(_._2.isDirectionless)

  override def setIsTypeNode {
    isTypeNode = true
    elements foreach (_._2.setIsTypeNode)
  }

  // Chisel3 - type-only nodes (no data - initialization or assignment) - used for verifying Wire() wrapping
  override def isTypeOnly: Boolean = {
    elements.forall(_._2.isTypeOnly)
  }
}
