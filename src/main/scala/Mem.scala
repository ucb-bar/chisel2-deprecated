// Mem3.scala -- Memory abstraction for Chisel
// Author: Brian Richards - 9/9/2011
// based on Mem2.scala -- Memory abstraction for Chisel
// Author: Brian Richards -- 8/2011
// based on Mem.scala:
// author: jonathan bachrach

package Chisel {

import scala.collection.mutable.ListBuffer;
import Node._;

object Mem {
  val noResetVal = Literal(0);

  def apply[T <: Data](depth: Int, isEnable: Fix, wrAddr: Num, wrData: T, wrMask: T = null, reset: T): MemCell[T] = {
    val memcell = new MemCell(depth, wrData);
    memcell.write(isEnable, wrAddr, wrData, wrMask);
    if (reset != null) memcell.reset_val(reset);
    memcell
  }

  def apply[T <: Data](depth: Int, isEnable: Fix, wrAddr: Num, wrData: T): MemCell[T] = {
    val memcell = new MemCell(depth, wrData);
    memcell.write(isEnable, wrAddr, wrData, null.asInstanceOf[T]);
    memcell
  }

  def apply[T <: Data](n: Int, wrDataProto: T): MemCell[T] = {
    val memcell = new MemCell(n, wrDataProto);
    memcell
  }
}

class MemCell[T <: Data](n: Int, data: T) extends Cell {
  val io = new Bundle();

  io.setIsCellIO;
  isReg = true;
  val primitiveNode = new Mem[T](n, data);

  primitiveNode.init("primitiveNode", data.getWidth);

  def apply(addr: Node): T = read(addr);
  def read(addr: Node): T = {
    val res = data.fromNode(MemRef(primitiveNode, addr)).asInstanceOf[T];
    res.setIsCellIO;
    res
  }

  def write(wen: Node, addr: Node, w_data: T, w_mask: T = null.asInstanceOf[T]) = {
    primitiveNode.AddWritePort(io, addr, w_data, wen, w_mask);
  }

  def reset_val(r_val: T) = {
    primitiveNode.AddResetVal(io, r_val);
  }

  primitiveNode.nameHolder = this;
}

class MemWPort[T <: Data](mem: Mem[T], io: Bundle, addr: Node, data: T, wen: Node, wbm: T = null.asInstanceOf[T]) {
  val port_offset = mem.inputs.length;
  val m = mem;

  //mem.inputs += addr;
  //mem.inputs += data;
  //mem.inputs += wen;

  val addr_port = Fix('input);
  addr_port.setName("addr_" + port_offset);
  io += addr_port;
  mem.inputs += addr_port;
  addr_port := addr;

  val data_port = data.clone.asInput;
  data_port.setName("data_" + (port_offset + 1));
  io += data_port;
  mem.inputs += data_port;
  data_port <> data;

  val wen_port  = Fix(1, 'input);
  wen_port.setName("wen_" + (port_offset + 2));
  io += wen_port;
  mem.inputs += wen_port;
  wen_port := wen;

  if (wbm != null) {
    val wbm_port = wbm.clone.asInput;
    wbm_port.setName("wbm_" + (port_offset + 3));
    io += wbm_port;
    mem.inputs += wbm_port;
    wbm_port <> wbm;
  }

  def wrAddr = mem.inputs(port_offset);
  def wrData = mem.inputs(port_offset + 1);
  def wrEnable = mem.inputs(port_offset + 2);
  def wrBitMask = mem.inputs(port_offset + 3);

  def isRamWriteInput(i: Node) = {
    i == wrData || i == wrAddr || i == wrEnable;
  }
  def emitDef: String = {
    var res = 
      "  always @(posedge clk) begin\n"
    if (wbm == null) {
      res +=
      "    if (" + wrEnable.emitRef + ")\n" +
      "      " + m.emitRef + "[" + wrAddr.emitRef + "] <= " + wrData.emitRef + ";\n"
    } else {
      res +=
      "    if (" + wrEnable.emitRef + ")\n" +
      "      " + m.emitRef + "[" + wrAddr.emitRef + "] <= " +
        wrData.emitRef + " & " + wrBitMask.emitRef + " | " +
        m.emitRef + "[" + wrAddr.emitRef + "] & ~" + wrBitMask.emitRef + ";\n"
    }
    res += "  end\n";
    res
  }
  def emitDefHiC: String = {
    var res = 
      "  if (" + wrEnable.emitRef + ".to_bool()) {\n"
    if (wbm == null) {
      res +=
      "    " + m.emitRef + ".put(" + wrAddr.emitRef + ", " +
        wrData.emitRef + ");\n" +
      "  }\n";
    } else {
      res +=
      "    " + m.emitRef + ".put(" + wrAddr.emitRef + ", " +
        wrData.emitRef + " & " + wrBitMask.emitRef + " | " +
        m.emitRef + ".get(" + wrAddr.emitRef + ") & ~" + wrBitMask.emitRef + ");\n"
      "  }\n";
    }
    res
  }
}

class MemResetPort[T <: Data](mem: Mem[T], io: Bundle, reset_val: T) {
  val port_offset = mem.inputs.length;
  val m = mem;

  //mem.inputs += reset_val;

  val reset_val_port = reset_val.clone.asInput;
  reset_val_port.setName("reset_val_" + (port_offset + 1));
  io += reset_val_port;
  mem.inputs += reset_val_port;
  reset_val_port <> reset_val;

  def resetVal = mem.inputs(port_offset);

  def emitDef: String = {
    var res = 
      "  always @(posedge clk) begin\n" +
      "    if (reset) begin\n"
    for (i <- 0 until m.n) {
      res += "      " + m.emitRef + "[" + i + "] <= " + resetVal.emitRef + ";\n";
    }
    res += "    end\n";
    res += "  end\n";
    res
  }
  def emitDefHiC: String = {
    val res =
      "  if (reset.to_bool()) {\n" +
      "    for (int i = 0; i < " + m.n + "; i++) \n" +
      "      "  + m.emitRef + ".put(i, " + resetVal.emitRef + ");\n" +
      "  }\n"
    res
  }
}

class Mem[T <: Data](n_val: Int, w_data: T) extends Delay {
  val n                     = n_val;
  var reset_port_opt: Option[MemResetPort[T]] = None;
  val write_ports           = ListBuffer[MemWPort[T]]();

  def AddWritePort(io: Bundle, addr: Node, data: T, wen: Node, wbm: T) = {
    val write_port = new MemWPort[T](this, io, addr, data, wen, wbm);
    write_ports += write_port;
  }

  def AddResetVal(io: Bundle, r_val: T) = {
    val reset_port = new MemResetPort[T](this, io, r_val);
    reset_port_opt = Some(reset_port);
  }

  override def getNode() = {
    fixName();
    removeCellIOs();
    this
  }

  override def isRamWriteInput(i: Node) = {
    ! write_ports.forall {p => !p.isRamWriteInput(i)}
  }

  override def toString: String = "MEM(" + ")";
  override def emitDef: String = {
    var res = ("" /: write_ports) { (s, p) => s + p.emitDef };
    if (reset_port_opt != None) {
      res += reset_port_opt.get.emitDef;
    }
    res
  }
  override def emitDec: String = 
    "  reg[" + (width-1) + ":0] " + emitRef + "[" + (n-1) + ":0];\n";
  override def emitDefHiC: String = {
    // Fold the code emitted for each write port.
    var res = ("" /: write_ports) { (s, p) => s + p.emitDefHiC };
    if (reset_port_opt != None) {
      res += reset_port_opt.get.emitDefHiC;
    }
    res
  }
  override def emitDecC: String = 
    "  mem_t<" + width + "," + n + "> " + emitRef + ";\n";
  override def apply(addr: Node): Node = MemRef(this, addr);
}

}
