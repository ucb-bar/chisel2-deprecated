// author: jonathan bachrach
package Chisel {

import Node._;

object Mem {
  val noResetVal = Lit(0);
  def apply (n: Int, isEnable: Node = Lit(0), wrAddr: Node = Lit(0), wrData: Node = Lit(0), resetVal: Node = noResetVal): Mem = {
    val res = new Mem(n);
    res.init("", widthOf(2), isEnable, wrAddr, wrData, resetVal);
    res.isResetVal = resetVal != noResetVal;
    res
  }
}
class Mem(n_val: Int) extends Delay {
  val n          = n_val;
  var isResetVal = false;
  def wrEnable   = inputs(0);
  def wrEnable_= (x: Node) = inputs(0) = x;
  def wrAddr     = inputs(1);
  def wrAddr_= (x: Node) = inputs(1) = x;
  def wrData     = inputs(2);
  def wrData_= (x: Node) = inputs(2) = x;
  def resetVal   = inputs(3);
  def resetVal_= (x: Node) = { isResetVal = true; inputs(3) = x; }
  override def isRamWriteInput(i: Node) = 
    i == wrEnable || i == wrAddr || i == wrData;
  override def toString: String = "MEM(" + wrEnable + " " + wrAddr + " " + wrData + ")";
  override def emitDef: String = {
    var res = 
      "  always @(posedge clk) begin\n" +
      "    if (" + wrEnable.emitRef + ")\n" +
      "      " + emitRef + "[" + wrAddr.emitRef + "] <= " + wrData.emitRef + ";\n";
    if (isResetVal) {
      res += "    else if (reset) begin\n";
      for (i <- 0 until n) 
        res += "      " + emitRef + "[" + i + "] <= " + resetVal.emitRef + ";\n";
      res += "    end\n";
    }
    res += "  end\n";
    res
  }
  override def emitDec: String = 
    "  reg[" + (width-1) + ":0] " + emitRef + "[" + (n-1) + ":0];\n";
  override def emitDefHiC: String = {
    var res = 
      "  if (" + wrEnable.emitRef + ".to_bool())\n" +
      "    " + emitRef + ".put(" + wrAddr.emitRef + ", " + wrData.emitRef + ");\n";
    if (isResetVal) {
      res += "  if (reset.to_bool()) {\n";
      res += "    for (int i = 0; i < " + n + "; i++) \n";
      res += "      "  + emitRef + ".put(i, " + resetVal.emitRef + ");\n";
      res += "  }\n";
    }
    // println("RESET MEM" + res);
    res
  }
  override def emitDecC: String = 
    "  mem_t<" + width + "," + n + "> " + emitRef + ";\n";
  override def apply(addr: Node): Node = MemRef(this, addr);
}

}
