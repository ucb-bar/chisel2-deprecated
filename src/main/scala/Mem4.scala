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

  def apply[T <: Data](depth:    Int,
                       wrData:   T, // If no address, this defines the data prototype.
                       wrEnable: Bool = null.asInstanceOf[Bool],
                       wrAddr:   Num  = null.asInstanceOf[Num],
                       wrMask:   Bits = null.asInstanceOf[Bits],
                       cs:       Bool = null.asInstanceOf[Bool],
                       resetVal: T    = null.asInstanceOf[T],
                       readLatency: Int = 1
                     ): Mem4Cell[T] = {
    val memcell = new Mem4Cell(depth, wrData, readLatency);
    if (!(wrAddr == null) && !(wrEnable == null)) {
      memcell.write(wrAddr, wrData, wrEnable, wrMask, cs);
    }
    if (!(resetVal == null)) memcell.reset_val(resetVal);
    memcell;
  }
  def apply[T <: Data](depth: Int, isEnable: Bool, wrAddr: Num, wrData: T): Mem4Cell[T] = {
    val memcell = new Mem4Cell(depth, wrData, 1);
    memcell.write(wrAddr, wrData, isEnable, null);
    memcell
  }
}

class Mem4Cell[T <: Data](n: Int, data: T, readLatency: Int) extends Cell {
  val io = new Bundle();

  io.setIsCellIO;
  isReg = true;
  val primitiveNode = new Mem4[T](n, data, readLatency);

  primitiveNode.init("primitiveNode", data.toNode.getWidth);
  primitiveNode.nameHolder = this;

  def apply(addr: Node, oe: Bool = null.asInstanceOf[Bool], cs: Bool = null): T = read(addr, oe, cs);
  def read(addr: Node, oe: Bool = null.asInstanceOf[Bool], cs: Bool = null): T = {
    val read_port = primitiveNode.addReadPort(io, addr, cs, oe);
    val res = data.fromNode(read_port.memRef).asInstanceOf[T];
    res.setIsCellIO;
    //res.setName("a_read_port");
    res;
  }
  def w(addr: Node, we: Node = null, w_mask: Bits = null, cs: Bool = null.asInstanceOf[Bool]): Mem4Port[T] = {
    primitiveNode.addWritePort(io, addr, null.asInstanceOf[T], we, w_mask, cs);
  }
  def rw(addr: Node, we: Node = null, w_mask: Bits = null, cs: Bool = null.asInstanceOf[Bool], oe: Bool = null): Mem4Port[T] = {
    primitiveNode.addRWPort(io, addr, null.asInstanceOf[T], we, w_mask, cs, oe);
  }

  def write(addr: Node, w_data: T, we: Node = null, w_mask: Bits = null, cs: Bool = null.asInstanceOf[Bool]) = {
    primitiveNode.addWritePort(io, addr, w_data, we, w_mask, cs);
  }

  def getReadLatency = readLatency;
  def reset_val(r_val: T) = { primitiveNode.addResetVal(io, r_val); }
  def setTarget(s: Symbol) = { primitiveNode.target = s; }
}

class Mem4Port[T <: Data](mem:        Mem4[T],
                          io:         Bundle,
                          port_type:  Symbol,
                          addr:       Node,
                          data:       T,
                          we:         Node = null,
                          wbm:        Bits = null,
                          cs:         Bool = null,
                          oe:         Bool = null,
                          val memRef: Mem4Ref = null.asInstanceOf[Mem4Ref]
                        ) {
  var port_offset = mem.inputs.length;
  val port_index = mem.port_count;
  val m = mem;
  var indent = "";

  val addr_port = Fix('input);
  val addr_offset = port_offset;
  port_offset += 1;
  io += addr_port;
  mem.inputs += addr_port;
  addr_port assign addr;

  var data_offset = -1;
  assign_data(data);

  var we_offset = -1;
  if (!(we == null)) {
    val we_port  = Bool('input);
    we_offset = port_offset;
    port_offset += 1;
    io += we_port;
    mem.inputs += we_port;
    we_port assign we;
  }
  var wbm_offset = -1;
  if (!(wbm == null)) {
    val wbm_port = wbm.clone.asInput;
    wbm_offset = port_offset;
    port_offset += 1;
    io += wbm_port;
    mem.inputs += wbm_port.toNode;
    wbm_port <> wbm;
  }
  var cs_offset = -1;
  if (!(cs == null)) {
    val cs_port = Bool('input);
    cs_offset = port_offset;
    port_offset += 1;
    io += cs_port;
    mem.inputs += cs_port;
    cs_port assign cs;
  }
  var oe_offset = -1;
  if (!(oe == null)) {
    val oe_port = Bool('input);
    oe_offset = port_offset;
    port_offset += 1;
    io += oe_port;
    mem.inputs += oe_port;
    oe_port assign oe;
  }

  def getPortType = port_type;
  def getReadLatency = m.getReadLatency;
  def assign_data(data: T) = {
    if (data_offset != -1) {
      println("[warning] Memory data input is already assigned");
    } else if (!(data == null)) {
      val data_port = data.clone.asInput;
      data_offset = port_offset;
      port_offset += 1;
      io += data_port;
      mem.inputs += data_port.toNode;
      data_port <> data;
    }
  }

  def wrAddr = mem.inputs(addr_offset);
  def wrData = mem.inputs(data_offset);
  def wrEnable = mem.inputs(we_offset);
  def wrBitMask = mem.inputs(wbm_offset);
  def hasWrBitMask = !(wbm == null);
  def chipSel = mem.inputs(cs_offset);
  def hasCS = !(cs == null);
  def outEn = mem.inputs(oe_offset);

  // Procedural assignment to memory:
  def colonEqual(src: Bits) = {
    println("[info] Using Mem4Port colonEqual");
    // generateError(src);
    assign_data(src.asInstanceOf[T]);
  }
  def := (src: Bits) = colonEqual(src);
  def := (src: Bool) = colonEqual(src);
  def := (src: Fix)  = colonEqual(src);
  def := (src: UFix) = colonEqual(src);

  def emitInstanceDef: String = {
    var res = "";
    if (port_type == 'read || port_type == 'rw) {
      res +=
        "  .O"+port_index+"("+memRef.emitTmp+"),\n" +
        "  .OE"+port_index+"("+(if (oe == null) "1'b1" else outEn.emitRef)+"),\n";
    }
    if (port_type == 'write || port_type == 'rw) {
      res +=
        "  .I"+port_index+"("+wrData.emitRef+"),\n" +
        "  .A"+port_index+"("+wrAddr.emitRef+"),\n" +
        "  .WE"+port_index+"("+(if (we == null) "1'b1" else wrEnable.emitRef)+"),\n";
      if (!(wbm == null)) {
        res += indent + "  .WBM"+port_index+"("+wrBitMask.emitRef+"),\n";
      }
    }
    res += "  .CS"+port_index+"("+(if(hasCS) chipSel.emitRef else "1'b1")+")";
    res
  }
  def emitDefWrite: String = {
    if (data_offset == -1) {
      println("[error] Memory write operation has no assigned value.");
      return "<no write data>";
    }
    var res = "";
    if (port_type == 'write || port_type == 'rw) {
      if (wbm == null) {
        res +=
        indent + "    if (" + wrEnable.emitRef + ")\n" +
        indent + "      " + m.emitRef + "[" + wrAddr.emitRef + "] <= " + wrData.emitRef + ";\n"
      } else {
        val gen_i = m.emitRef+"__i";
        val pre_read_buf = m.emitRef+"__next"+port_index;
        res += 
        indent+"    "+pre_read_buf+" = "+m.emitRef+"["+wrAddr.emitRef+"];\n"+
        indent+"    if ("+wrEnable.emitRef+(if(hasCS) " & "+chipSel.emitRef else "")+") begin\n"+
        indent+"      generate for ("+gen_i+" = 0; "+gen_i+" < "+m.width+"; "+gen_i+" = "+gen_i+" + 1) begin:"+
                            m.emitRef+"__W"+port_index+"\n"+
        indent+"        if("+wrBitMask.emitRef+"["+gen_i+"]) "+pre_read_buf+"["+gen_i+"] = "+
                            wrData.emitRef+"["+gen_i+"];\n"+
        indent+"      end\n"+
        indent+"    end\n"+
        indent+"    "+m.emitRef+"["+wrAddr.emitRef+"] = "+pre_read_buf+";\n"
      }
    }
    res;
  }
  def emitDefRead: String = {
    var res = "";
    val read_buf = m.emitRef+"__read"+port_index;
    def read_out = memRef.emitTmp;

    if (port_type == 'read || port_type == 'rw) {
      if (m.getReadLatency > 0) {
        res += "  always @(posedge clk) begin\n";
        res += "    "+read_buf+" <= ";
      } else {
        res += "  assign "+read_out+" = ";
      }
      if (oe == null) {
        res += m.emitRef+"["+wrAddr.emitRef+"]\n";
      } else {
        res += "("+outEn.emitRef+(if (hasCS) " & "+chipSel.emitRef else "")+") ? "+
            m.emitRef+"["+wrAddr.emitRef+"] : "+
            m.getWidth+"'bz;\n";
      }
      if (m.getReadLatency > 0) {
        res += "  end\n";
        res += "  assign "+read_out+" = "+read_buf+";\n";
      }
    }
    res;
  }
  def emitDefHiC: String = {
    var res = "";
    if (port_type == 'write || port_type == 'rw) {
      res +=  "  if (" + wrEnable.emitRef + ".to_bool()) {\n"
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
    }
    if (port_type == 'read || port_type == 'rw) {
      res += "  " + memRef.emitTmp + " = " + m.emitRef + ".get(" + wrAddr.emitRef + ");\n"
    }
    res
  }
}

class Mem4ResetPort[T <: Data](mem: Mem4[T], io: Bundle, reset_val: T) {
  val port_offset = mem.inputs.length;
  val m = mem;

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

class Mem4[T <: Data](n_val: Int, w_data: T, readLatency: Int) extends Delay {
  val n                     = n_val;
  var reset_port_opt: Option[Mem4ResetPort[T]] = None;
  val port_list             = ListBuffer[Mem4Port[T]]();
  var port_count            = 0;
  var mem_refs              = ListBuffer[Mem4Ref]();
  var target                = 'rtl;

  def addWritePort(io: Bundle, addr: Node, data: T, we: Node, wbm: Bits, cs: Bool) = {
    val we_opt = (if (we == null) Bool(true) else we);
    val write_port = new Mem4Port[T](this, io, 'write, addr, data, we_opt, wbm, cs, null);
    port_count += 1;
    port_list += write_port;
    write_port;
  }
  def addReadPort(io: Bundle, addr: Node, cs: Bool = null, oe: Bool = null) = {
    val memRef = Mem4Ref(this);
    val read_port = new Mem4Port[T](this, io, 'read, addr, w_data, null, null, cs, oe, memRef);
    port_count += 1;
    port_list += read_port;
    read_port;
  }
  def addRWPort(io: Bundle, addr: Node, data: T, we: Node, wbm: Bits, cs: Bool, oe: Bool = null) = {
    val memRef = Mem4Ref(this);
    val we_opt = (if (we == null) Bool(true) else we);
    val rw_port = new Mem4Port[T](this, io, 'rw, addr, data, we_opt, wbm, cs, oe, memRef);
    port_count += 1;
    port_list += rw_port;
    rw_port;
  }

  def addResetVal(io: Bundle, r_val: T) = {
    val reset_port = new Mem4ResetPort[T](this, io, r_val);
    reset_port_opt = Some(reset_port);
  }

  override def getNode() = {
    fixName();
    removeCellIOs();
    this
  }

  override def isRamWriteInput(n: Node) = {
    ! inputs.forall {in => !(n == in)}
  }

  def getReadLatency = readLatency;
  override def toString: String = "MEM(" + emitRef + ")";
  def toXML =
    <memory module={getPathName} depth={""+n} width={""+w_data.getWidth}>
      {port_list.map(p =>
        <port>{p.getPortType.toString}</port>
      )}
    </memory>

  override def emitDef: String = {
    println(""+toXML);
    if (target == 'rtl) {
      emitRTLDef;
    } else if (target == 'inst) {
      emitInstanceDef;
    } else {
      "// target = "+target+" is undefined.";
    }
  }
  def emitRTLDef: String = {
    val hasReset = reset_port_opt != None;
    var res = "  always @(posedge clk) begin\n";
    if(hasReset){
      res +=
      "    if (reset) begin\n" +
              reset_port_opt.get.emitDef +
      "    end else begin\n" +
              ("" /: port_list) { (s, p) => {p.indent = "  "; s + p.emitDefWrite} } +
      "    end\n";
    } else {
      res += ("" /: port_list) { (s, p) => {s + p.emitDefWrite} };
    }
    res += "  end\n"
    res += ("" /: port_list) { (s, p) => {s + p.emitDefRead} };
    res
  }
  def getPathName = { component.getPathName + "_" + emitRef; }
  def emitInstanceDef: String = {
    var res = getPathName + " #(.depth("+n+"), .width("+w_data.getWidth+")) " + emitRef + "(.CLK(clk), .RST(reset)";
    res +=
      ("" /: port_list) { (s, p) => {s + ",\n" + p.emitInstanceDef} }
    res += ");\n";
    res;
  }
  override def emitDec: String = {
    if (target == 'rtl) {
      emitRTLDec
    } else {
      ""
    }
  }
  def emitRTLDec: String = {
    var hasWBM = false;
    var res = "  reg[" + (width-1) + ":0] " + emitRef + "[" + (n-1) + ":0];\n";
    for (p <- port_list) {
      if (p.hasWrBitMask) {
        hasWBM = true;
        res += "  reg[" + (width-1) + ":0] " + emitRef + "__next"+p.port_index+";\n";
      }
      if (readLatency > 0 && (p.getPortType == 'read || p.getPortType == 'rw)) {
        res += "  reg[" + (width-1) + ":0] " + emitRef + "__read"+p.port_index+";\n";
      }
    }
    if (hasWBM) {
      res += "  genvar "+emitRef+"__i;\n";
    }
    res;
  }
  override def emitDefHiC: String = {
    // Fold the code emitted for each write port.
    var res = ("" /: port_list) { (s, p) => s + p.emitDefHiC };
    if (reset_port_opt != None) {
      res += reset_port_opt.get.emitDefHiC;
    }
    res
  }
  override def emitDefLoC: String = {
    val res = "";
    //val res = ("" /: mem_refs) { (s, r) => s + r.emitDefLoCLocal };
    res
  }
  override def emitDecC: String = 
    "  mem_t<" + width + "," + n + "> " + emitRef + ";\n";
/*
  def apply(addr: Node, oe: Bool = null.asInstanceOf[Bool], cs: Bool = null): Node = {
    Mem4Ref(this, addr, oe, cs);
  }
*/
}

object Mem4Ref {
  def apply[T <: Data](mem: Mem4[T]) = {
    val memRef = new Mem4Ref();
    memRef.init("", widthOf(0), mem);
    mem.mem_refs += memRef; /// Still needed?
    memRef;
  }
  /// Still needed?
  def apply[T <: Data](mem:  Mem4[T],
                       addr: Node,
                       oe:   Bool = null.asInstanceOf[Bool],
                       cs:   Bool = null.asInstanceOf[Bool]): Node = {
    val memRef = new Mem4Ref();
    memRef.port_index = mem.port_count;
    mem.port_count += 1;
    if (oe == null && cs == null) {
      memRef.init("", widthOf(0), mem, addr);
    } else if (cs == null) {
      memRef.init("", widthOf(0), mem, addr, oe);
    } else {
      memRef.init("", widthOf(0), mem, addr, (if (oe == null) Bool(true) else oe), cs);
    }
    mem.mem_refs += memRef;
    memRef;
  }
}
class Mem4Ref extends Node {
  var port_index: Int = 0;
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
  def emitInstanceDef: String = {
    var res =
    "  .O"+port_index+"("+emitTmp+"),\n"+
    "  .A"+port_index+"("+inputs(1).emitRef+"),\n" +
    "  .OE"+port_index+"("+(if(inputs.length > 2) inputs(2).emitRef else "1'b1")+"),\n" +
    "  .CS"+port_index+"("+(if(inputs.length > 3) inputs(3).emitRef else "1'b1")+"),\n";
    res
  }
}

}
