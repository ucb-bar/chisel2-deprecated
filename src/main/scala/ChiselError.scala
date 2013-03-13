package Chisel
import scala.collection.mutable.ArrayBuffer
import ChiselError._

object ChiselError {
  val ChiselErrors = new ArrayBuffer[ChiselError];

  def apply(m: String, n: Node): ChiselError = 
    new ChiselError(() => m, n.line)

  def apply(mf: => String, n: Node): ChiselError = 
    new ChiselError(() => mf, n.line)

  def apply(m: String, stack: Array[StackTraceElement]): ChiselError =
    new ChiselError(() => m, findFirstUserLine(stack))

  def apply(mf: => String, stack: Array[StackTraceElement]): ChiselError =
    new ChiselError(() => mf, findFirstUserLine(stack))

  def findFirstUserLine(stack: Array[StackTraceElement]): StackTraceElement = {
    for(i <- 1 until stack.length) {
      val ste = stack(i)
      val classname = ste.getClassName
      val dotPos = classname.lastIndexOf('.')
      if( dotPos > 0 ) {
        val pkg = classname.subSequence(0, dotPos)
        if (pkg != "Chisel" && !classname.contains("scala"))
          return ste
      }
    }
    println("COULDN'T FIND LINE NUMBER")
    return stack(0)
  }

  def findFirstUserInd(stack: Array[StackTraceElement]): Int = {
    for(i <- 1 until stack.length) {
      val ste = stack(i)
      val classname = ste.getClassName
      val dotPos = classname.lastIndexOf('.')
      val pkg = classname.subSequence(0, dotPos)
      if (pkg != "Chisel" && !classname.contains("scala"))
        return i
    }
    println("COULDN'T FIND LINE NUMBER")
    return 0
  }

  def printError(msgFun: () => String, line: StackTraceElement) = {
    println(msgFun() + " on line " + line.getLineNumber + 
            " in class " + line.getClassName + 
            " in file " + line.getFileName)
  }
}

class ChiselError(val msgFun: () => String, val line: StackTraceElement) {
  def printError: Unit = ChiselError.printError(msgFun, line)

}
