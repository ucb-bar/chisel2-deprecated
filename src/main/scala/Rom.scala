// author: jonathan bachrach
package Chisel {

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue
import scala.collection.mutable.Stack
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import java.lang.reflect.Modifier._;
import java.io.File;

import scala.math.log;
import scala.math.abs;
import scala.math.ceil;
import scala.math.max;
import scala.math.min;
import Node._;
import Wire._;
import Lit._;
import Op._;
import Reg._;
import Component._;
import Bundle._;
import IOdir._;

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
