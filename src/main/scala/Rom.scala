// author: jonathan bachrach
package Chisel {

import scala.math.max;
import Rom._;
import IOdir._;

object Rom {
  def romWidth(data: Array[Node]) = { 
    (m: Node) => { 
      var res = 0; 
      for (d <- data) 
        res = max(d.width, res); 
      res  }
  }
  def apply (data: Array[Node]): Rom = {
    val res = new Rom(data);
    res.init("", romWidth(data));
    res
  }

  def apply[T <: dat_t]( data: Array[T], addr: Node): RomCell[T] = {
    new RomCell(data, addr);
  }

}


class RomCell[T <: dat_t](data: Array[T], addr: Node) extends Cell {
  val io = new bundle_t{val addr = Fix('input);
			val out = data(0).clone.asOutput;
  }
  io.setIsCellIO;
  val dataBits = data.map(x => x.toNode);
  val primitiveNode = new Rom(dataBits);
  primitiveNode.init("primitiveNode", romWidth(dataBits), dataBits: _*);
  val fb = io.out.fromNode(primitiveNode(addr)).asInstanceOf[T];
  fb.setIsCellIO;
  fb ^^ io.out;
  primitiveNode.nameHolder = this;
}

class Rom(data_vals: Array[Node]) extends Delay {
  val data = data_vals.toArray;

  override def removeCellIOs() = {
    for(i <- 0 until data.length)
      if(data(i).isCellIO)
	data(i) = data(i).getNode()
  }

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
