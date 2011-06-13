// author: jonathan bachrach
package Chisel {

import scala.math.max;

object Rom {
  def romWidth(data: Array[Lit]) = { 
    (m: Node) => { 
      var res = 0; 
      for (d <- data) 
        res = max(d.width, res); 
      res  }
  }
  def apply (data: Array[Lit]): Rom = {
    val res = new Rom(data);
    res.init("", romWidth(data));
    res
  }
}
class Rom(data_vals: Array[Lit]) extends Delay {
  val data = data_vals;
  override def toString: String = "ROM(" + data + ")";
  override def emitDef: String = {
    var res = "  initial begin\n";
    for (i <- 0 until data.length) 
      res += "    " + emitRef + "[" + i + "] = " + data(i).emitRef + ";\n";
    res += "  end\n";
    res
  }
  override def emitDec: String = 
    "  reg[" + (width-1) + ":0] " + emitRef + "[" + (data.length-1) + ":0];\n";
  override def emitInitC: String = {
    var res = "";
    for (i <- 0 until data.length) 
      res += "  " + emitRef + ".put(" + i + ", " + data(i).emitRef + ");\n";
    res
  }
  override def emitDecC: String = 
    "  mem_t<" + width + "," + data.length + "> " + emitRef + ";\n";
  override def apply(addr: Node): Node = MemRef(this, addr);
}

}
