// Mem.scala -- Memory abstraction for Chisel
// Author: Brian Richards - 9/9/2011
// based on Mem2.scala -- Memory abstraction for Chisel
// Author: Brian Richards -- 8/2011
// based on Mem.scala:
// author: jonathan bachrach

package Chisel {

import scala.collection.mutable.ListBuffer;
import Node._;

object Mem4 {
  val noResetVal = Literal(0);

  def apply[T <: Data](depth: Int,
                       wrData: T, // If no address, this defines the data prototype.
                       wrEnable: Bool = null.asInstanceOf[Bool],
                       wrAddr: Num = null.asInstanceOf[Num],
                       wrMask: Bits = null.asInstanceOf[Bits],
                       resetVal: T = null.asInstanceOf[T]): Mem4Cell[T] = {
    val memcell = new Mem4Cell(depth, wrData);
    if (!(wrAddr == null) && !(wrEnable == null)) {
      memcell.write(wrEnable, wrAddr, wrData, wrMask);
    }
    if (!(resetVal == null)) memcell.reset_val(resetVal);
    memcell;
  }
  def apply[T <: Data](depth: Int, isEnable: Bool, wrAddr: Num, wrData: T): Mem4Cell[T] = {
    val memcell = new Mem4Cell(depth, wrData);
    memcell.write(isEnable, wrAddr, wrData, null);
    memcell
  }
}

class Mem4Cell[T <: Data](n: Int, data: T) extends Cell {
  val io = new Bundle();

  io.setIsCellIO;
  isReg = true;
  val primitiveNode = new Mem4[T](n, data);

  primitiveNode.init("primitiveNode", data.toNode.getWidth);

  def apply(addr: Node, oe: Bool = null.asInstanceOf[Bool]): T = read(addr, oe);
  def read(addr: Node, oe: Bool = null.asInstanceOf[Bool]): T = {
    val memRef = Mem4Ref(primitiveNode, addr, oe);
    val res = data.fromNode(memRef).asInstanceOf[T];
    res.setIsCellIO;
    res
  }
  def w(addr: Node): Mem4Ref = {
    val res = new Mem4Ref();
    primitiveNode.mem_refs += res;
    res.init("", widthOf(0), primitiveNode, addr);
    res;
  }

  def write(wen: Node, addr: Node, w_data: T, w_mask: Bits = null) = {
    primitiveNode.AddWritePort(io, addr, w_data, wen, w_mask);
  }

  def reset_val(r_val: T) = {
    primitiveNode.AddResetVal(io, r_val);
  }

  primitiveNode.nameHolder = this;
}

class Mem4WPort[T <: Data](mem: Mem4[T], io: Bundle, addr: Node, data: T, wen: Node, wbm: Bits = null) {
  val port_offset = mem.inputs.length;
  val port_index = mem.port_count;
  val m = mem;
  var indent = "";

  //mem.inputs += addr;
  //mem.inputs += data;
  //mem.inputs += wen;

  val addr_port = Fix('input);
  addr_port.setName("addr_" + port_offset);
  io += addr_port;
  mem.inputs += addr_port;
  addr_port assign addr;

  val data_port = data.clone.asInput;
  data_port.setName("data_" + (port_offset + 1));
  io += data_port;
  mem.inputs += data_port.toNode;
  data_port <> data;

  val wen_port  = Bool('input);
  wen_port.setName("wen_" + (port_offset + 2));
  io += wen_port;
  mem.inputs += wen_port;
  wen_port assign wen;

  if (!(wbm == null)) {
    val wbm_port = wbm.clone.asInput;
    wbm_port.setName("wbm_" + (port_offset + 3));
    io += wbm_port;
    mem.inputs += wbm_port.toNode;
    wbm_port <> wbm;
  }

  def hasWrBitMask = !(wbm == null);
  def wrAddr = mem.inputs(port_offset);
  def wrData = mem.inputs(port_offset + 1);
  def wrEnable = mem.inputs(port_offset + 2);
  def wrBitMask = mem.inputs(port_offset + 3);

  def isRamWriteInput(i: Node) = {
    i == wrData || i == wrAddr || i == wrEnable;
  }
  def emitDef: String = {
    var res = "";
      //"  always @(posedge clk) begin\n"
    if (wbm == null) {
      res +=
      indent + "    if (" + wrEnable.emitRef + ")\n" +
      indent + "      " + m.emitRef + "[" + wrAddr.emitRef + "] <= " + wrData.emitRef + ";\n"
    } else {
      val gen_i = m.emitRef+"__i";
      val pre_read_buf = m.emitRef+"__next"+port_index;
      res += 
      indent+"    "+pre_read_buf+" = "+m.emitRef+"["+wrAddr.emitRef+"];\n"+
      indent+"    if ("+wrEnable.emitRef+") begin\n"+
      indent+"      generate for ("+gen_i+" = 0; "+gen_i+" < "+m.width+"; "+gen_i+" = "+gen_i+" + 1) begin:"+
                      m.emitRef+"__W"+port_index+"\n"+
      indent+"        if("+wrBitMask.emitRef+"["+gen_i+"]) "+pre_read_buf+"["+gen_i+"] = "+
                      wrData.emitRef+"["+gen_i+"];\n"+
      indent+"      end\n"+
      indent+"    end\n"+
      indent+"    "+m.emitRef+"["+wrAddr.emitRef+"] = "+pre_read_buf+";\n"
    }
    //res += indent + "  end\n";
    res;
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
        m.emitRef + ".get(" + wrAddr.emitRef + ") & ~" + wrBitMask.emitRef + ");\n" +
      "  }\n";
    }
    res
  }
}

class Mem4ResetPort[T <: Data](mem: Mem4[T], io: Bundle, reset_val: T) {
  val port_offset = mem.inputs.length;
  val m = mem;

  //mem.inputs += reset_val;

  val reset_val_port = reset_val.clone.asInput;
  reset_val_port.setName("reset_val_" + (port_offset + 1));
  io += reset_val_port;
  mem.inputs += reset_val_port.toNode;
  reset_val_port <> reset_val;

  def resetVal = mem.inputs(port_offset);

  def emitDef: String = {
    var res = 
      //"  always @(posedge clk) begin\n" +
      "    if (reset) begin\n"
    for (i <- 0 until m.n) {
      res += "      " + m.emitRef + "[" + i + "] <= " + resetVal.emitRef + ";\n";
    }
    //res += "    end\n";
    res += "    end else begin\n"; 
    //res += "  end\n";
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

class Mem4[T <: Data](n_val: Int, w_data: T) extends Delay {
  val n                     = n_val;
  var reset_port_opt: Option[Mem4ResetPort[T]] = None;
  val write_ports           = ListBuffer[Mem4WPort[T]]();
  var port_count            = 0;
  var mem_refs              = ListBuffer[Mem4Ref]();

  def AddWritePort(io: Bundle, addr: Node, data: T, wen: Node, wbm: Bits) = {
    val write_port = new Mem4WPort[T](this, io, addr, data, wen, wbm);
    port_count += 1;
    write_ports += write_port;
  }

  def AddResetVal(io: Bundle, r_val: T) = {
    val reset_port = new Mem4ResetPort[T](this, io, r_val);
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
    val hasReset = reset_port_opt != None;
    var res = "  always @(posedge clk) begin\n"
    if(hasReset){
      res += reset_port_opt.get.emitDef
    }
    res += ("" /: write_ports) { (s, p) => {if(hasReset) p.indent = "  "; s + p.emitDef} };
    if(hasReset){
      res += "    end\n"
    }
    res += "  end\n"
    res += ("" /: mem_refs)    { (s, r) => {s + r.emitDefLocal;} };
    //if (reset_port_opt != None) {
    //  res += reset_port_opt.get.emitDef;
    //}
    res
  }
  override def emitDec: String = {
    var hasWBM = false;
    var res = "  reg[" + (width-1) + ":0] " + emitRef + "[" + (n-1) + ":0];\n";
    for (p <- write_ports) {
      if (p.hasWrBitMask) {
        hasWBM = true;
        res += "  reg[" + (width-1) + ":0] " + emitRef + "__next"+p.port_index+";\n";
      }
    }
    if (hasWBM) {
      res += "  genvar "+emitRef+"__i;\n";
    }
    res;
  }
  override def emitDefHiC: String = {
    // Fold the code emitted for each write port.
    var res = ("" /: write_ports) { (s, p) => s + p.emitDefHiC };
    if (reset_port_opt != None) {
      res += reset_port_opt.get.emitDefHiC;
    }
    res
  }
  override def emitDefLoC: String = {
    val res = ("" /: mem_refs) { (s, r) => s + r.emitDefLoCLocal };
    res
  }
  override def emitDecC: String = 
    "  mem_t<" + width + "," + n + "> " + emitRef + ";\n";
  def apply(addr: Node, oe: Bool = null.asInstanceOf[Bool]): Node = Mem4Ref(this, addr, oe);
}

object Mem4Ref {
  def apply[T <: Data](mem:  Mem4[T],
                       addr: Node,
                       oe:   Bool = null.asInstanceOf[Bool]): Node = {
    val memRef = new Mem4Ref();
    if (oe == null) {
      memRef.init("", widthOf(0), mem, addr);
    } else {
      memRef.init("", widthOf(0), mem, addr, oe);
    }
    mem.mem_refs += memRef;
    memRef;
  }
  def r[T <: Data](mem: Mem4[T], addr: Node, data: T): T = {
    val memRef = new Mem4Ref();
    memRef.init("", widthOf(0), mem, addr);
    mem.mem_refs += memRef
    val res = data.fromNode(memRef).asInstanceOf[T];
    res
  }
}
class Mem4Ref extends Node {
  def colonEqual(src: Bits) = {
    println("[info] Using MemRef colonEqual");
    // generateError(src);
    // Assign src as the write value.
    inputs += src;
  }
  def := (src: Bits) = colonEqual(src);
  def := (src: Bool) = colonEqual(src);
  def := (src: Fix)  = colonEqual(src);
  def := (src: UFix) = colonEqual(src);

  override def toString: String = inputs(0) + "[" + inputs(1) + "]";
  def emitDefLocal: String = {
    var res = "";
    if (inputs.length > 2) {
      res += "  assign "+emitTmp+" = "+inputs(2).emitRef+" ? "+inputs(0).emitRef+"["+inputs(1).emitRef+"] : "+
              inputs(0).getWidth+"'bx;\n";
    } else {
      res += "  assign " + emitTmp + " = " + inputs(0).emitRef + "[" + inputs(1).emitRef + "];\n";
    }
    res;
  }
  def emitDefLoCLocal: String = 
    "  " + emitTmp + " = " + inputs(0).emitRef + ".get(" + inputs(1).emitRef + ");\n"
}

}
