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
import ChiselError._

object ChiselError {
  val ChiselErrors = new ArrayBuffer[ChiselError];

  def apply(m: String, n: Node): ChiselError =
    new ChiselError(() => m, n.line)

  def apply(mf: => String, n: Node): ChiselError =
    new ChiselError(() => mf, n.line)

  def apply(m: String, stack: Array[StackTraceElement]): ChiselError = {
    new ChiselError(() => m, findFirstUserLine(stack))
  }

  def apply(mf: => String, stack: Array[StackTraceElement]): ChiselError =
    new ChiselError(() => mf, findFirstUserLine(stack))

  def findFirstUserLine(stack: Array[StackTraceElement]): StackTraceElement = {
    val index = findFirstUserInd(stack)
    if( index > 0 ) stack(index) else stack(0)
  }

  def findFirstUserInd(stack: Array[StackTraceElement]): Int = {
    /* Starts at one because item 0 is java.lang.Thread.getStackTrace */
    for(i <- 1 until stack.length) {
      val ste = stack(i)
      val className = ste.getClassName()
      try {
        val cls = Class.forName(className)
        val supercls = cls.getSuperclass()
        if( supercls == classOf[Component] ) {
          return i
        }
      } catch {
        case e: java.lang.ClassNotFoundException => {}
      }
    }
    /* XXX Do it the old way until we figure if it is safe
           to remove from Node.scala
       var line: StackTraceElement = findFirstUserLine(Thread.currentThread().getStackTrace)
     */
    for(i <- 1 until stack.length) {
      val ste = stack(i)
      val classname = ste.getClassName
      val dotPos = classname.lastIndexOf('.')
      if( dotPos > 0 ) {
        val pkg = classname.subSequence(0, dotPos)
        if (pkg != "Chisel" && !classname.contains("scala")) {
          return i
        }
      }
    }
    println("COULDN'T FIND LINE NUMBER (" + stack(1) + ")")
    return 0
  }

  def printError(msgFun: () => String, line: StackTraceElement) {
    /* Following conventions for error formatting */
    println(line.getFileName + ":" + line.getLineNumber
      + ": error: " + msgFun() +
      " in class " + line.getClassName)
  }
}

class ChiselError(val msgFun: () => String, val line: StackTraceElement) {
  def printError: Unit = ChiselError.printError(msgFun, line)

}
