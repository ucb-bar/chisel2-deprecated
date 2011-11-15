package Chisel {

import scala.collection.mutable.ArrayBuffer;

object ChiselError {
  val ChiselErrors = new ArrayBuffer[ChiselError];
}

class ChiselError(val err: String, val m: String, val st: Array[StackTraceElement]) {
  var index: Int = 0;
  def printError = {
    println("  " + err + ": " + m);  
    if(index != 0)
      for(i <- 0 until 10)
	println("     " + (if(index != 0) st(i).toString else ""));
    println();
  }
  override def equals(x: Any): Boolean = {
      if (!x.isInstanceOf[ChiselError])
          return false
      val xError = x.asInstanceOf[ChiselError]
      xError.err == err && xError.m == m && xError.index == index && ((index > 0) && xError.st == st || index == 0)
  }
}

object IllegalArgument {
  def apply(m: String, index: Int): ChiselError = {
    val res = new ChiselError("Illegal Argument", m, Thread.currentThread.getStackTrace);
    res.index = index;
    res
  }
}

object IllegalState {
  def apply(m: String, index: Int): ChiselError = {
    val res = new ChiselError("Illegal State", m, Thread.currentThread.getStackTrace);
    res.index = index;
    res
  }
  def apply(m: String, st: Array[StackTraceElement]) = {
    val res = new ChiselError("Illegal State", m, st);
    res.index = 1;
    res
  }
}

object IllegalConnection {
  def apply(m: String, index: Int): ChiselError = {
    val res = new ChiselError("IllegalConnection", m, Thread.currentThread.getStackTrace);
    res.index = index;
    res
  }
}

object IllegalName {
  def apply(m: String): ChiselError = {
    new ChiselError("Illegal Name", m, Thread.currentThread.getStackTrace);
  }
}

object TypeError {
  def apply(op: String, a: String, b: String): ChiselError = {
    val res = new ChiselError("Type Error", op + " is not defined on " + a + " and " + b, Thread.currentThread.getStackTrace);
    res.index = 4;
    res
  }
}

}
