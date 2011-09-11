package Chisel {

import scala.collection.mutable.ArrayBuffer;

object ChiselError {
  val ChiselErrors = new ArrayBuffer[ChiselError];
}

class ChiselError(err: String, m: String, st: Array[StackTraceElement]) {
  var index: Int = 0;
  def printError = {
    println("  " + err + ": " + m);  
    if(index != 0)
      for(i <- 0 until 10)
	println("     " + (if(index != 0) st(i).toString else ""));
    println();
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

}
