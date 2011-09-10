// author: jonathan bachrach
package Chisel {

import Node._;

object Mem {
  val noResetVal = Literal(0);


  def apply[T <: dat_t](n: Int, isEnable: Fix, wrAddr: Fix, wrData: T, resetVal: T): MemCell[T] ={
    val memcell = new MemCell(n, wrData, resetVal != null);
    memcell.io.wrData <> wrData;
    memcell.io.wrAddr := wrAddr;
    memcell.io.isEnable := isEnable;
    if(resetVal != null) memcell.io.resetVal <> resetVal.asInstanceOf[T];
    memcell
  }

}



class MemCell[T <: dat_t](n: Int, data: T, isReset: Boolean) extends Cell {
  val io = new bundle_t(){val wrData = data.clone.asInput;
			  val wrAddr = Input();
			  val isEnable = Input();
			  val resetVal = data.clone.asInput;
			}
  io.setIsCellIO;
  isReg = true;
  val primitiveNode = new Mem(n);
  if(isReset)
    primitiveNode.init("primitiveNode", widthOf(2), io.isEnable, io.wrAddr, io.wrData.toNode, io.resetVal.toNode);
  else
    primitiveNode.init("primitiveNode", widthOf(2), io.isEnable, io.wrAddr, io.wrData.toNode);
  primitiveNode.asInstanceOf[Mem].isResetVal = isReset;
  def apply(addr: Node): T = {
    val res = data.fromNode(MemRef(primitiveNode, addr)).asInstanceOf[T];
    res.setIsCellIO;
    res
  }
  primitiveNode.nameHolder = this;
}

class Mem(n_val: Int) extends Delay {
  val n          = n_val;
  var isResetVal = false;
  def wrEnable   = inputs(0).getNode;
  def wrEnable_= (x: Node) = inputs(0) = x
  def wrAddr     = inputs(1);
  def wrAddr_= (x: Node) = inputs(1) = x;
  def wrData     = inputs(2)
  def wrData_= (x: Node) = inputs(2) = x;
  def resetVal   = inputs(3).getNode;
  def resetVal_= (x: Node) = { isResetVal = true; inputs(3) = x; }
  override def getNode() = {
    fixName();
    removeCellIOs();
    this
  }
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
