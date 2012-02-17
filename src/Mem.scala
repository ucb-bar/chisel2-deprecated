// Mem.scala -- Memory abstraction for Chisel
// Author: Brian Richards - 9/9/2011
// based on Mem2.scala -- Memory abstraction for Chisel
// Author: Brian Richards -- 8/2011
// based on Mem.scala:
// author: jonathan bachrach

package Chisel {

import scala.collection.mutable.ListBuffer;
import java.io.File;
import Component._;
import Node._;

object Mem {
  val noResetVal = Literal(0);

  def apply[T <: Data](depth:    Int,
                       gen:      T): MemCell[T] = {
    val memcell = new MemCell(depth, gen);
    memcell;
  }
  def apply[T <: Data](depth:    Int,
                       wrEnable: Bool,
                       wrAddr:   Num,
                       wrData:   T,
                       wrMask:   Bits = null.asInstanceOf[Bits],
                       cs:       Bool = null.asInstanceOf[Bool],
                       resetVal: T    = null.asInstanceOf[T],
                       readLatency: Int = 0,
                       hexInitFile: String = ""
                     ): MemCell[T] = {
    val memcell = new MemCell(depth, wrData);
    memcell.setReadLatency(readLatency);
    if (!(wrAddr == null) && !(wrEnable == null) && !(wrData == null)) {
      memcell.write(wrAddr, wrData, wrEnable, wrMask, cs);
    }
    if (!(resetVal == null)) memcell.reset_val(resetVal);
    if (hexInitFile != "") memcell.setHexInitFile(hexInitFile);
    memcell;
  }
  def apply[T <: Data](depth: Int, isEnable: Bool, wrAddr: Num, wrData: T): MemCell[T] = {
    val memcell = new MemCell(depth, wrData);
    memcell.write(wrAddr, wrData, isEnable, null);
    memcell
  }
  def ensure_dir(dir: String) = {
    val d = dir + (if (dir == "" || dir(dir.length-1) == '/') "" else "/");
    new File(d).mkdirs();
    d
  }
  var defReadLatency = 0;
  def setDefaultReadLatency(latency: Int) = { defReadLatency = latency; }
  def getDefaultReadLatency = defReadLatency;

  var defMemoryImplementation = 'rtl;
  def setDefaultMemoryImplementation(impl: Symbol) = { defMemoryImplementation = impl; }
  def getDefaultMemoryImplementation = defMemoryImplementation;
}

class MemCell[T <: Data](n: Int, data: T) extends Cell {
  val io = new Bundle();
  val port_list = ListBuffer[MemPort[T]]();
  var port_count = 0;
  io.setIsCellIO;
  isReg = true;
  val primitiveNode = new Mem[T](n, this);

  primitiveNode.init("primitiveNode", data.toNode.getWidth);
  primitiveNode.nameHolder = this;

  def getWidth = data.getWidth;
  def getDataType = data;
  def apply(addr: Node, oe: Bool = null.asInstanceOf[Bool], cs: Bool = null): T = read(addr, oe, cs);
  def read(addr: Node, oe: Bool = null.asInstanceOf[Bool], cs: Bool = null): T = {
    //val read_port = primitiveNode.addReadPort(this, addr, cs, oe);
    val read_port = new MemPort[T](this, 'read, addr, data, null, null, cs, oe);
    port_count += 1;
    port_list += read_port;
    read_port.read;
  }
  def port: MemPort[T] = {
    println("MemCell.port");
    val virtual_port = new MemPort[T](this, 'physical, null, null.asInstanceOf[T]);
    // w(null);
    port_count += 1;
    port_list += virtual_port;
    virtual_port;
  }
  def w(addr: Node, we: Node = null, w_mask: Bits = null, cs: Bool = null.asInstanceOf[Bool]): MemPort[T] = {
    val we_opt = (if (we == null) Bool(true) else we);
    val write_port = new MemPort[T](this, 'write, addr, null.asInstanceOf[T], we_opt, w_mask, cs);
    port_count += 1;
    port_list += write_port;
    write_port;
  }
  def rw(addr: Node, w_data: T, we: Node = null, w_mask: Bits = null, cs: Bool = null.asInstanceOf[Bool], oe: Bool = null): T = {
    val we_opt = (if (we == null) Bool(true) else we);
    val rw_port = new MemPort[T](this, 'rw, addr, w_data, we_opt, w_mask, cs, oe);
    port_count += 1;
    port_list += rw_port;
    rw_port.read;
  }
  def write(addr: Node, w_data: T, we: Node = null, w_mask: Bits = null, cs: Bool = null.asInstanceOf[Bool]) = {
    // primitiveNode.addWritePort(this, addr, w_data, we, w_mask, cs);
    val we_opt = (if (we == null) Bool(true) else we);
    val write_port = new MemPort[T](this, 'write, addr, w_data, we_opt, w_mask, cs, null);
    port_count += 1;
    port_list += write_port;
    write_port;
}
  def setHexInitFile(hexInitFileName: String) = {
    primitiveNode.setHexInitFile(hexInitFileName);
  }

  def getReadLatency = primitiveNode.getReadLatency;
  def setReadLatency(latency: Int) = {
    primitiveNode.setReadLatency(latency);
    primitiveNode.setCppReadLatency(latency);

    if(!isEmittingComponents)
      for(port <- port_list){
        if(port.port_type == 'read || port.port_type == 'rw)
	  port.genRegChain(latency)
      }
  }
  def reset_val(r_val: T) = { primitiveNode.addResetVal(this, r_val); }
  def setTarget(s: Symbol) = { primitiveNode.target = s; }
}

class MemPort[T <: Data](cell:       MemCell[T],
                          prt_type:   Symbol,
                          addr:       Node,
                          data:       T,
                          we:         Node = null,
                          wbm:        Bits = null,
                          cs:         Bool = null,
                          oe:         Bool = null
                        ) {
  val mem = cell.primitiveNode;
  val memRef = MemRef(this, addr);
  var holder: Bits = null;
  memRef.port_type = prt_type;
  var port_type = prt_type;
  def next_input_index = mem.inputs.length;
  var port_index = cell.port_count;
  var indent = "";
  // The virtual_ports are implemented by this port.
  val virtual_ports = ListBuffer[MemPort[T]]();
  var physical_port: MemPort[T] = null;
  var when_expr = Bool(true);

  var addr_offset = -1;
  var data_offset = -1;
  var we_offset = -1;
  var wbm_offset = -1;
  var cs_offset = -1;
  var oe_offset = -1;

  if (!(addr == null)) {
    connectToCell(0);
  } else {
    // println("[info] Found an unmapped virtual port (no address assigned). Type was: "+getPortType);
    port_type = 'physical;
  }

  def connectToCell(arg_to_prevent_reflection: Int) = {
    assign_data(data.toNode);
    assign_addr(addr);
    assign_we(we);
    assign_wbm(wbm);
    assign_cs(cs);
    assign_oe(oe);
  }
  def assign_addr(addr: Node) = {
    if (addr_offset != -1) {
      println("[warning] Memory addr input is already assigned");
    } else if (!(addr == null)) {
      val addr_port = Fix(INPUT);
      addr_offset = next_input_index;
      cell.io += addr_port;
      mem.inputs += addr_port;
      addr_port assign addr;
      if (mem.addr_width > 0) {
        if (mem.addr_width != addr.getWidth) {
          println("[error] Memory address width differs from other memory ports");
        }
      } else {
        mem.addr_width = addr.getWidth;
      }
    }
  }
  def assign_data(data: Node) = {
    if (data_offset != -1) {
      println("[warning] Memory data input is already assigned");
    } else if (!(data == null)) {
      val data_port = Bits(data.getWidth, INPUT);
      data_offset = next_input_index;
      cell.io += data_port;
      mem.inputs += data_port.toNode;
      data_port assign data;
    }
  }
  def assign_we(we: Node) = {
    if (we_offset != -1) {
      println("[warning] Memory we input is already assigned");
    } else if (!(we == null)) {
      val we_port  = Bool(INPUT);
      we_offset = next_input_index;
      cell.io += we_port;
      mem.inputs += we_port;
      we_port assign we;
    } else {
      // println("[error] assign_we: null signal");
    }
  }
  def assign_wbm(wbm: Bits) = {
    if (!(wbm == null)) {
      val wbm_port = wbm.clone.asInput;
      wbm_offset = next_input_index;
      cell.io += wbm_port;
      mem.inputs += wbm_port.toNode;
      wbm_port <> wbm;
    }
  }
  def assign_cs(cs: Bool) = {
    if (!(cs == null)) {
      val cs_port = Bool(INPUT);
      cs_offset = next_input_index;
      cell.io += cs_port;
      mem.inputs += cs_port;
      cs_port assign cs;
    }
  }
  def assign_oe(oe: Bool) = {
     if (!(oe == null)) {
      val oe_port = Bool(INPUT);
      oe_offset = next_input_index;
      cell.io += oe_port;
      mem.inputs += oe_port;
      oe_port assign oe;
    }
  }

  def getWE = (if (hasWrEnable) wrEnable else Bool(true))
  def getData = (if (hasWrData) wrData else Bits(1));
  def getAddr = (if (hasWrAddr) wrAddr else Bits(0));

  def setPortType(p_type: Symbol) = { port_type = p_type; }
  def getPortType = port_type;
  def isReadable = (port_type == 'read || port_type == 'rw);
  def isWritable = (port_type == 'write || port_type == 'rw);
  def isMapped   = (List('read, 'write, 'rw) contains port_type);
   
  def getReadLatency = mem.getReadLatency;

  def genRegChain(x: Int) = {
    println("called " + x)
    for(i <- 0 until x){
      val reg = new Reg();
      reg.init("", widthOf(0), holder.inputs(0));
      holder.inputs(0) = reg;
    }
  }

  def wrAddr =       { if (addr_offset < 0) println("[error] Using bad wrAddr"); mem.inputs(addr_offset); }
  def hasWrAddr =    { addr_offset >= 0; }
  def wrData =       { if (data_offset < 0) println("[error] Using bad wrData"); mem.inputs(data_offset); }
  def hasWrData =      { data_offset >= 0; }
  def wrEnable =     { if (we_offset < 0) println("[error] Using bad wrEnable"); mem.inputs(we_offset); }
  def hasWrEnable =  { we_offset >= 0; }
  def wrBitMask =    { if (wbm_offset < 0) println("[error] Using bad wrBitMask"); mem.inputs(wbm_offset); }
  def hasWrBitMask = { wbm_offset >= 0; }
  def chipSel =      { if (cs_offset < 0) println("[error] Using bad chipSel"); mem.inputs(cs_offset); }
  def hasChipSel =   { cs_offset >= 0; }
  def outEn =        { if (oe_offset < 0) println("[error] Using bad outEn"); mem.inputs(oe_offset); }
  def hasOutEn =     { oe_offset >= 0; }

  def r: T = read;
  def read: T = {
    var res: T = null.asInstanceOf[T];
    if (cell.getDataType == null) {
      println("[error] read has no data prototype");
    } else {
      //res = cell.getDataType.fromNode(memRef).asInstanceOf[T];

      if(isEmittingComponents) {
        res = cell.getDataType.fromNode(memRef).asInstanceOf[T]
        res.comp = memRef
      } else {
        holder = Bits(data.toNode.getWidth, OUTPUT)
        holder.inputs += memRef;
        res = cell.getDataType.fromNode(holder);
        res.comp = holder
        genRegChain(mem.cppReadLatency)
      }
      res.setIsCellIO;
    }
    res;
  }

  def apply(addr: Node, we: Bool = null, wbm: Bits = null.asInstanceOf[Bits], cs: Bool = null, oe: Bool = null): T = {
    val we_opt = (if (we == null) Bool(true) else we);
    val virtual_port = new MemPort[T](cell, 'virtual, addr, null.asInstanceOf[T], we_opt, wbm, cs, oe);
    cell.port_count += 1;
    cell.port_list += virtual_port;
    if (virtual_port.physical_port == null) {
      virtual_port.physical_port = this;
      virtual_ports += virtual_port;
    } else {
      // This shouldn't happen...
      println("[error] Virtual port already has a physical port");
    }
    virtual_port.read;
  }

  def colonEqual(src: Bits) = {
    if (getPortType == 'virtual) {
      // Update the port type if needed.
      val phy_port_type = physical_port.getPortType
      if (phy_port_type == 'read || phy_port_type == 'rw) {
        physical_port.setPortType('rw);
      } else if (phy_port_type == 'physical || phy_port_type == 'write) {
        physical_port.setPortType('write);
      }
      // Procedural assignment: Assign the src data to this virtual port.
      assign_data(src.asInstanceOf[T]);
      if (when_expr == null) {
        when_expr = Node.conds.top;
      }
    } else {
      // This operator only applies to a virtual port.
      println("[error] Assignment to a non-virtual port is not supported.");
    }
  }

  def := (src: Bits) = colonEqual(src);
  def := (src: Bool) = colonEqual(src);
  def := (src: Fix) =  colonEqual(src);
  def := (src: UFix) = colonEqual(src);

  def genMux: Unit = {
    if (virtual_ports.length == 0) {
      // println("[warning] No memory port assignments specified.");
      return;
    }
    var we_mux: Node = Lit(false); // No memory write unless otherwise specified.
    var addr_mux: Node = Bits(0,virtual_ports(0).getAddr.getWidth);
    var data_mux: Node = Bits(0,mem.getDataWidth);
    // Loop through the memory ports from last to first, and chain multiplexers.
    for (p <- virtual_ports.reverse) {
      we_mux = Multiplex(p.when_expr, p.getWE, we_mux);
      addr_mux = Multiplex(p.when_expr, p.getAddr, addr_mux);
      data_mux = Multiplex(p.when_expr, p.getData, data_mux);
    }
    // println("Inferred port type: "+port_type);
    assign_we(we_mux);
    assign_addr(addr_mux);
    //assign_data(cell.getDataType.fromNode(data_mux));
    assign_data(data_mux);
  }
  def emitInstanceDef: String = {
    var res = "";
    if (isMapped) {
      res += ",\n"+
        "  .A"+port_index+"("+wrAddr.emitRef+"),\n" +
        "  .CS"+port_index+"("+(if(hasChipSel) chipSel.emitRef else "1'b1")+")";
    }
    if (isReadable) {
      res += ",\n"+
        "  .O"+port_index+"("+memRef.emitTmp+"),\n" +
        "  .OE"+port_index+"("+(if (!hasOutEn) "1'b1" else outEn.emitRef)+")";
    }
    if (isWritable) {
      res += ",\n"+
        "  .I"+port_index+"("+wrData.emitRef+"),\n" +
        "  .WE"+port_index+"("+(if (!hasWrEnable) "1'b1" else wrEnable.emitRef)+")";
      if (mem.hasWBM) {
        if (hasWrBitMask) {
          res += ",\n" + indent + "  .WBM"+port_index+"("+wrBitMask.emitRef+")";
        } else {
          res += ",\n" + indent + "  .WBM"+port_index+"({"+mem.getDataWidth+"{1'b1}})";
        }
      }
    }
    res
  }
  def emitDefWrite: String = {
    if (data_offset == -1) {
      println("[error] Memory write operation has no assigned value.");
      return "<no write data>";
    }
    var res = "";
    if (isWritable) {
      if (!hasWrBitMask) {
        res +=
        indent + "    if (" + wrEnable.emitRef + ")\n" +
        indent + "      " + mem.emitRef + "[" + wrAddr.emitRef + "] <= " + wrData.emitRef + ";\n"
      } else {
        val gen_i = mem.emitRef+"__i";
        val pre_read_buf = mem.emitRef+"__next"+port_index;
        res += 
        indent+"    "+pre_read_buf+" = "+mem.emitRef+"["+wrAddr.emitRef+"];\n"+
        indent+"    if ("+wrEnable.emitRef+(if(hasChipSel) " & "+chipSel.emitRef else "")+") begin\n"+
        indent+"      for ("+gen_i+" = 0; "+gen_i+" < "+mem.width+"; "+gen_i+" = "+gen_i+" + 1) begin:"+
                            mem.emitRef+"__W"+port_index+"\n"+
        indent+"        if("+wrBitMask.emitRef+"["+gen_i+"]) "+pre_read_buf+"["+gen_i+"] = "+
                            wrData.emitRef+"["+gen_i+"];\n"+
        indent+"      end\n"+
        indent+"    end\n"+
        indent+"    "+mem.emitRef+"["+wrAddr.emitRef+"] = "+pre_read_buf+";\n"
      }
    }
    res;
  }
  def emitDefRead: String = {
    var res = "";
    val read_buf = mem.emitRef+"__read"+port_index+"_";
    def read_out = memRef.emitTmp;

    if (isReadable) {
      if (mem.getReadLatency > 0) {
        res += "  always @(posedge clk) begin\n";
        res += "    "+read_buf+"0 <= ";
      } else {
        res += "  assign "+read_out+" = ";
      }
      if (!hasOutEn) {
        res += mem.emitRef+"["+wrAddr.emitRef+"];\n";
      } else {
        res += "("+outEn.emitRef+(if (hasChipSel) " & "+chipSel.emitRef else "")+") ? "+
            mem.emitRef+"["+wrAddr.emitRef+"] : "+
            mem.getWidth+"'bz;\n";
      }
      if (mem.getReadLatency > 0) {
        for (lat <- 1 until mem.getReadLatency) {
          res += "    "+read_buf+lat+" <= "+read_buf+(lat-1)+";\n";
        } 
        res += "  end\n";
        res += "  assign "+read_out+" = "+read_buf+(mem.getReadLatency-1)+";\n";
      }
    }
    res;
  }

  def emitDefLoC: String = {
    var res = "";
    val read_buf = mem.emitRef+"__read"+port_index+"_";
    if (isReadable) {
      val read_mem = mem.emitRef + ".get(" + wrAddr.emitRef + ")";
      val read_lat = mem.getReadLatency;
      if (read_lat > 0) {
        res += "  "+memRef.emitRef+" = "+read_buf+(read_lat-1)+";\n";
        for (lat <- 1 until read_lat) {
          res += "  "+read_buf+(read_lat-lat)+"_shadow = "+read_buf+(read_lat-lat-1)+";\n";
        }
      }
      res += "  " + (if (read_lat > 0) read_buf+"0_shadow" else memRef.emitRef) + " = ";
      if (!hasOutEn) {
        res += read_mem+";\n";
      } else if (hasChipSel) {
        res += "("+outEn.emitRef+".to_bool() & "+chipSel.emitRef+".to_bool()) ? "+read_mem+" : LIT<"+mem.getWidth+">(0L);\n";
      } else {
        res += "("+outEn.emitRef+".to_bool()) ? "+read_mem+" : LIT<"+mem.getWidth+">(0L);\n";
      }
    }
    res;
  }


  def emitDefHiC: String = {
    isHiC = true;
    var res = "";
    val read_buf = mem.emitRef+"__read"+port_index+"_";
    if (isWritable) {
      res += " if (" + wrEnable.emitRef + ".to_bool()) {\n"
      if (!hasWrBitMask) {
        res +=
        " " + mem.emitRef + ".put(" + wrAddr.emitRef + ", " +
        wrData.emitRef + ");\n" +
        " }\n";
      } else {
        res +=
        " " + mem.emitRef + ".put(" + wrAddr.emitRef + ", " +
        wrData.emitRef + " & " + wrBitMask.emitRef + " | " +
        mem.emitRef + ".get(" + wrAddr.emitRef + ") & ~" + wrBitMask.emitRef + ");\n" +
        " }\n";
      }
    }
    if (isReadable) {
      val read_buf = mem.emitRef+"__read"+port_index+"_";
      val read_lat = mem.getReadLatency;
      if (read_lat > 0) {
        // res += " "+memRef.emitRef+" = "+memRef.emitRef+"_shadow;\n";
        for (lat <- 0 until read_lat) {
          res += " "+read_buf+(read_lat-lat-1)+" = "+read_buf+(read_lat-lat-1)+"_shadow;\n";
        }
      }
    }
    isHiC = false;
    res
  }
  
}

class MemResetPort[T <: Data](mem: Mem[T], cell: MemCell[T], reset_val: T) {
  val reset_port_index = mem.inputs.length;
  val m = mem;

  val reset_val_port = reset_val.clone.asInput;
  reset_val_port.setName("reset_val");
  cell.io += reset_val_port;
  mem.inputs += reset_val_port.toNode;
  reset_val_port <> reset_val;

  def resetVal = mem.inputs(reset_port_index);

  def emitDef: String = {
    var res = ""
      //"  always @(posedge clk) begin\n" +
      //"    if (reset) begin\n"
    for (i <- 0 until mem.getDepth) {
      res += "      " + mem.emitRef + "[" + i + "] <= " + resetVal.emitRef + ";\n";
    }
    //res += "    end\n";
    //res += "    end else begin\n"; 
    //res += "  end\n";
    res
  }
  def emitDefHiC: String = {
    val res =
      "  if (reset.to_bool()) {\n" +
      "    for (int i = 0; i < " + mem.getDepth + "; i++) \n" +
      "      "  + mem.emitRef + ".put(i, " + resetVal.emitRef + ");\n" +
      "  }\n"
    res
  }
}

class Mem[T <: Data](depth: Int, val cell: MemCell[T]) extends Delay with proc {
  var reset_port_opt: Option[MemResetPort[T]] = None;
  var target                = Mem.getDefaultMemoryImplementation;
  var hexInitFile           = "";
  var readLatency           = Mem.getDefaultReadLatency;
  var hasWBM                = false;
  var addr_width            = 0;

  // proc trait methods.
  def procAssign(src: Node) = {
    println("Mem.procAssign");
  }
  override def genMuxes(default: Node) = {
    for (p <- cell.port_list) {
      p.genMux;
    }
  }

  def getDepth = depth;
  def getAddrWidth = addr_width;
  def getDataWidth = cell.getWidth;
  def addResetVal(cell: MemCell[T], r_val: T) = {
    val reset_port = new MemResetPort[T](this, cell, r_val);
    reset_port_opt = Some(reset_port);
  }
  def setHexInitFile(hexInitFileName: String) = {
    hexInitFile = hexInitFileName;
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
  def setReadLatency(latency: Int) = { 
    if(isEmittingComponents) 
      readLatency = latency 
  }

  var cppReadLatency = 0

  def setCppReadLatency(latency: Int) = cppReadLatency = latency

  override def toString: String = "MEM( + emitRef + )";

  def toCMD = {
    var read_port_count = 0;
    var write_port_count = 0;
    var rw_port_count = 0;
    var delim = "";
    var res = "gen_chisel_mem -name \""+getPathName+"\" -depth "+depth+" -width "+cell.getWidth;
    res += " -addr_width "+getAddrWidth+" -read_latency "+getReadLatency+" -port_types \"";
    for (p <- cell.port_list) {
      if (p.hasWrBitMask) hasWBM = true;
      if (p.getPortType == 'read)       { read_port_count += 1;  res += delim+"read";  delim = " "; }
      else if (p.getPortType == 'write) { write_port_count += 1; res += delim+"write"; delim = " "; }
      else if (p.getPortType == 'rw)    { rw_port_count += 1;    res += delim+"rw";    delim = " "; }
    }
    res += "\" -has_write_mask "+(if(hasWBM) "true" else "false")+"\n";
    res;
  }
  def toJSON(indent: String = "") = {
    var read_port_count = 0;
    var write_port_count = 0;
    var rw_port_count = 0;
    var comma = "";
    var res =
      indent+"{\"Mem\" : {\n"+
      indent+"  \"name\":         \""+getPathName+"\",\n"+
      indent+"  \"depth\":        "+depth+",\n"+
      indent+"  \"width\":        "+cell.getWidth+",\n"+
      indent+"  \"read_latency\": "+getReadLatency+",\n"+
      indent+"  \"port_types\":   [";
    for (p <- cell.port_list) {
      if (p.hasWrBitMask) hasWBM = true;
      if (p.getPortType == 'read)       { read_port_count += 1;  res += comma+"\"read\""; comma = ", "; }
      else if (p.getPortType == 'write) { write_port_count += 1; res += comma+"\"write\""; comma = ", "; }
      else if (p.getPortType == 'rw)    { rw_port_count += 1;    res += comma+"\"rw\""; comma = ", ";}
    }
    res += "],\n"+
      indent+"  \"read_ports\":   "+read_port_count+",\n"+
      indent+"  \"write_ports\":  "+write_port_count+",\n"+
      indent+"  \"rw_ports\":     "+rw_port_count+",\n"+
      indent+"  \"hasWriteMask\": "+(if(hasWBM) "true" else "false")+"\n";
    res += indent+"}}\n";
    res;    
  }
  override def emitDef: String = {
    if (target == 'rtl) {
      emitRTLDef;
    } else if (target == 'inst) {
      val res = emitInstanceDef;
      Component.configStr += toCMD;
      res;
    } else {
      "// target = "+target+" is undefined.";
    }
  }
  def setPortIndices = {
    var port_index = 0;
    cell.port_list.filter(_.isMapped).foreach(p => {p.port_index = port_index; port_index += 1;})
  }
  def emitRTLDef: String = {
    val hasReset = reset_port_opt != None;
    var res = "  always @(posedge clk) begin\n";
    if(hasReset){
      res +=
      "    if (reset) begin\n" +
              reset_port_opt.get.emitDef +
      "    end else begin\n" +
              ("" /: cell.port_list) { (s, p) => {p.indent = "  "; s + p.emitDefWrite} } +
      "    end\n";
    } else {
      res += ("" /: cell.port_list) { (s, p) => {s + p.emitDefWrite} };
    }
    res += "  end\n"
    res += ("" /: cell.port_list) { (s, p) => {s + p.emitDefRead} };
    res
  }
  def getPathName = { component.getPathName + "_" + emitRef; }
  def emitInstanceDef: String = {
    hasWBM = false;
    var res = getPathName + " " + emitRef + "(.CLK(clk), .RST(reset)";
    val mapped_ports = cell.port_list.filter(_.isMapped);
    for (p <- mapped_ports) { if (p.hasWrBitMask) hasWBM = true; }
    res +=
      ("" /: mapped_ports) { (s, p) => {s + p.emitInstanceDef}};
    res += ");\n";
    res;
  }
  override def emitDec: String = {
    setPortIndices;
    if (target == 'rtl) {
      emitRTLDec
    } else {
      ""
    }
  }
  def emitRTLDec: String = {
    var res = "  reg[" + (width-1) + ":0] " + emitRef + "[" + (depth-1) + ":0];\n";
    if (hexInitFile != "") {
      // println("hexInitFile: "+hexInitFile);
      res += "  initial $readmemh(\""+hexInitFile+"\", "+emitRef+");\n";
    }
    for (p <- cell.port_list) {
      if (p.hasWrBitMask) {
        hasWBM = true;
        res += "  reg[" + (width-1) + ":0] " + emitRef + "__next"+p.port_index+";\n";
      }
      if (p.isReadable) {
        for (lat <- 0 until readLatency) {
          res += "  reg[" + (width-1) + ":0] " + emitRef + "__read"+p.port_index+"_"+lat+";\n";
        }
      }
    }
    if (hasWBM) {
      res += "  integer "+emitRef+"__i;\n";
    }
    res;
  }
  override def emitDefHiC: String = {
    // Fold the code emitted for each write port.
    var res = ("" /: cell.port_list) { (s, p) => s + p.emitDefHiC };
    if (reset_port_opt != None) {
      res += reset_port_opt.get.emitDefHiC;
    }
    res
  }
  // Move call to p.emitDefLoC to the MemRef Class to properly schedule reads
  //override def emitDefLoC: String = {
  //  var res = ("" /: cell.port_list) { (s, p) => s + p.emitDefLoC };
  //  res
  //}
  override def emitInitC: String = {
    if (hexInitFile != "") {
      "  "+emitRef+".read_hex(\""+hexInitFile+"\");\n"
    } else {
      ""
    }
  }
  override def emitDecC: String = {
    var res = "  mem_t<"+width+","+depth+"> "+emitRef+";\n";
    setPortIndices;
    for (p <- cell.port_list) {
      if (p.hasWrBitMask) {
        hasWBM = true;
        res += "  dat_t<" + width + "> " + emitRef + "__next"+p.port_index+";\n";
      }
      if (p.isReadable) {
        for (lat <- 0 until readLatency) {
          res += "  dat_t<" + width + "> " + emitRef + "__read"+p.port_index+"_"+lat+";\n";
          res += "  dat_t<" + width + "> " + emitRef + "__read"+p.port_index+"_"+lat+"_shadow;\n";
        }
      }
    }
    res;
  }
/*
  def apply(addr: Node, oe: Bool = null.asInstanceOf[Bool], cs: Bool = null): Node = {
    MemRef(this, addr, oe, cs);
  }
*/
}

object MemRef {
  def apply[T <: Data](mem_port: MemPort[T]) = {
    val memRef = new MemRef(mem_port);
    val mem = mem_port.mem;
    memRef.init("", widthOf(0), mem);
    memRef;
  }
  /// Still needed?
  def apply[T <: Data](mem_port:  MemPort[T],
                       addr: Node,
                       oe:   Bool = null.asInstanceOf[Bool],
                       cs:   Bool = null.asInstanceOf[Bool]) = {
    val memRef = new MemRef(mem_port);
    val mem = mem_port.mem;
    memRef.port_index = mem.cell.port_count;
    mem.cell.port_count += 1;
    if (oe == null && cs == null) {
      memRef.init("", widthOf(0), mem, addr);
    } else if (cs == null) {
      memRef.init("", widthOf(0), mem, addr, oe);
    } else {
      memRef.init("", widthOf(0), mem, addr, (if (oe == null) Bool(true) else oe), cs);
    }
    memRef;
  }
}
class MemRef[T <: Data](mem_port: MemPort[T]) extends Node with proc {
  var port_index: Int = 0;

  var port_type = 'read
  override def isInObject = true; 
  override def isReg = inputs(0).asInstanceOf[Mem[ _ ]].getReadLatency > 0 && port_type == 'rw

  // proc trait methods. Note that the proc trait is expected so that
  // this node can be assigned to Node.comp to support assignment.
  def procAssign(src: Node) = {
    src match {
      case data: Data => {
        // Add an input port if none has been assigned
        mem_port.colonEqual(data.toBits);
      }
      case any => {
        println("[warning] Procedural assignment to Mem through MemRef: Unrecognized node type: "+src.getClass);
      }
    }
  }
  override def genMuxes(default: Node) = {}

  override def toString: String = inputs(0) + "[" + inputs(1) + "]";
  def emitInstanceDef: String = {
    var res =
    "  .O"+port_index+"("+emitTmp+"),\n"+
    "  .A"+port_index+"("+inputs(1).emitRef+"),\n" +
    "  .OE"+port_index+"("+(if(inputs.length > 2) inputs(2).emitRef else "1'b1")+"),\n" +
    "  .CS"+port_index+"("+(if(inputs.length > 3) inputs(3).emitRef else "1'b1")+")";
    res
  }

  // override def assign(src: Node) = {
  //   src match {
  //     case data: Data => {
  //       // Add an input port if none has been assigned
  //       mem_port.colonEqualEqual(data.toBits);
  //     }
  //     case any => {
  //       println("[warning] Assigning to Mem through MemRef: Unrecognized node type: "+src.getClass);
  //     }
  //   }
  // }

  override def emitDefLoC: String = {
    // Emit port read code (may be combinational).
    mem_port.emitDefLoC;
  }
}

}
