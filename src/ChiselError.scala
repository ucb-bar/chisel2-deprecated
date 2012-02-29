package Chisel {

import scala.collection.mutable.ArrayBuffer;
import ChiselError._

object ChiselError {
  val ChiselErrors = new ArrayBuffer[ChiselError];

  def apply(m: String, n: Node): ChiselError = 
    new ChiselError(m, n.line)

  def apply(m: String, stack: Array[StackTraceElement]): ChiselError =
    new ChiselError(m, stack)

  def findFirstUserLine(stack: Array[StackTraceElement]): StackTraceElement = {
    for(i <- 1 until stack.length) {
      val ste = stack(i)
      val classname = ste.getClassName
      val dotPos = classname.lastIndexOf('.')
      val pkg = classname.subSequence(0, dotPos)
      if (pkg != "Chisel" && !classname.contains("scala"))
        return ste
    }
    println("COULDN'T FIND LINE NUMBER")
    return stack(0)
  }
}

class ChiselError(val msg: String, val stack: Array[StackTraceElement]) {
  def printError = {
    val ste = findFirstUserLine(stack)
    println(msg + " on line " + ste.getLineNumber + 
            " in class " + ste.getClassName + 
            " in file " + ste.getFileName)
  }
}

}
