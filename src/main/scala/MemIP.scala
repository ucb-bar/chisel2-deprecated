package Mem {

import scala.collection.mutable.ListBuffer;
import java.io.File;
import Chisel._;
import Component._;

object MemIP {
  def apply[T <: Data](memGen: MemGen[T], depth: Int, wrDataProto: T): MemIP[T] = {
    new MemIP(memGen, depth, wrDataProto);
  }
  def apply[T <: Data](memGen: MemGen[T]): MemIP[T] = {
    val memIP = new MemIP(memGen, memGen.numWords, memGen.wrData);
    for (p <- memGen.port_list) {
      val mip_port = new MemIPPort(memGen.memSpec, memIP.mem, p.portType,
                                   p.addr_port, p.data_in, p.we_port,
                                   p.oe_port, p.cs_port, p.wr_mask_port);
      if (p.portType == 'read) {
        // Connect the read data to the MemGen output port
        mip_port.data_out ^^ p.data_out;
        memIP.read_ports += mip_port;
      } else if (p.portType == 'write) {
        memIP.write_ports += mip_port;
      } else if (p.portType == 'rw) {
        mip_port.data_out ^^ p.data_out;
        memIP.rw_ports += mip_port;
      } else {
        println("[error] MemIP constructor called with invalid port type" + p.portType);
      }
    }
    memIP;
  }
}

class MemIPPort[T <: Data](val memSpec: MemorySpec,
                           val mem: MemCell[T],
                           val port_type: Symbol,
                           val addr: Num,
                           val data: T,
                           val we: Bool,
                           val oe: Bool,
                           val cs: Bool = null,
                           val wrMask: Bits = null) {
  var size = 0;//data.toNode.getWidth;

  val addr_port = addr.clone.asInput;
  val ce_port   = Bool('input);
  val cs_port   = Bool('input);
  val data_out  = data.clone.asOutput;
  val oe_port   = Bool('input);
  val data_in   = data.clone.asInput;
  val we_port   = Bool('input);
  val wr_mask_port = if (wrMask == null) Bits(1,'input) else wrMask.clone.asInput;
  var wr_mask_bits = 0;
  var data_in_bits = 0;

  //implement(0);
  def implement(fake: Int = 0) = {
    // println("[info] MemIPPort implement");
    if (!(wrMask == null)) {
      wr_mask_port := wrMask;
      wr_mask_bits = wrMask.getWidth;
      data_in_bits = data.getWidth;
      memSpec.data_bits_per_mask_bit = data_in_bits / wr_mask_bits;
      memSpec.no_bit_mask = false;
      val extra_bits = data_in_bits % wr_mask_bits;
      if (extra_bits != 0) {
        println("[warning] Write data width "+data.getWidth+
                " is not divisible by write mask width of "+
                wrMask.getWidth+".");
      }
    }
    addr_port ^^ addr;

    cs_port := (if (cs == null) Bool(false) else !cs);

    ce_port.setIsClkInput;

    if (port_type.equals('write) || port_type.equals('rw)) {
      data_in ^^ data;
      we_port := !we;

      val expanded_wbm = expand_wbm_to_bits(wrMask, memSpec.data_bits_per_mask_bit, data_in_bits);
      mem.write(!we_port && !cs_port, addr_port, data_in, expanded_wbm);
    }

    if (port_type.equals('read) || port_type.equals('rw)) {
      val read_data = mem.read(addr_port);
      val zero_data = Fix(0);
      val data_mux = Fix(dir = 'output);//zero_data.clone;

      data_mux assign (Multiplex(!oe_port && !cs_port, read_data, zero_data));

      oe_port := !oe;
      val data_mux_reg = Reg(data_mux, zero_data);
      data_mux_reg ^^ data_out;
    }
  }

  // expand_wbm_to_bits -- Duplicate each input bit in wbm dup times  
  def expand_wbm_to_bits(wbm: Bits, dup: Int, out_width: Int): Bits = {
    val mask_bits = if (wbm == null) 0 else wbm.getWidth;
    if (wbm == null) {
      wbm;
    } else if (out_width < dup) {
      Fill(out_width, wbm(0));
    } else {
      val next_bit_mask = if(mask_bits == 1) wbm(0) else wbm(mask_bits-1,1);
      Cat(expand_wbm_to_bits(next_bit_mask, dup, out_width - dup),
          Fill(dup, wbm(0)));
    }
  }

  def add_io(io: Bundle, port_index: Int) = {
    ce_port.setName(memSpec.emitClkEn(port_index));
    io += ce_port;
    addr_port.setName(memSpec.emitAddr(port_index));
    io += addr_port;
    cs_port.setName(memSpec.emitCS(port_index));
    io += cs_port;
    if (port_type.equals('write) || port_type.equals('rw)) {
      data_in.setName(memSpec.emitDataIn(port_index));
      io += data_in;
      we_port.setName(memSpec.emitWE(port_index));
      io += we_port;
      if (!(wrMask == null)) {
        wr_mask_port.setName(memSpec.emitWBM(port_index));
        io += wr_mask_port;
      }
    }
    if (port_type.equals('read) || port_type.equals('rw)) {
      data_out.setName(memSpec.emitDataOut(port_index));
      io += data_out;
      oe_port.setName(memSpec.emitOE(port_index));
      io += oe_port;
    }
  }
}

class MemIP[T <: Data](memGen: MemGen[T], numWords: Int, wrDataProto: T) extends BlackBox {
  val memSpec = memGen.memSpec;
  val io = new Bundle();
  val mem = Mem(numWords, wrDataProto);
  var size = wrDataProto.getWidth;
  var id = 0;
  var read_ports  = ListBuffer[MemIPPort[T]]();
  var write_ports = ListBuffer[MemIPPort[T]]();
  var rw_ports    = ListBuffer[MemIPPort[T]]();

  def setMaster(memGen: MemGen[T]) = {
    setName(emitGeneratedMasterName(memGen));
  }
  def setMaster(n: String) = {
    setName(n);
  }
  override def genName(n: String): String = {
    if (n == null || n.length() == 0) "" else "memIP_" + n;
  }
  override def elaborate(fake: Int = 0) = {
    var port_index = 1;
    //println("[info] MemIP Elaborate");
    // The ports must be implemented and named before markComponent is run.
    memSpec.set_dimensions(numWords, size, 1);
    memSpec.set_port_count(read_ports.length, write_ports.length, rw_ports.length);
    
    io.elementsCache = null; // Detach elements from the io bundle.
    read_ports.foreach(p  => {p.implement(0); p.add_io(io, port_index); port_index += 1; });
    write_ports.foreach(p => {p.implement(0); p.add_io(io, port_index); port_index += 1; });
    rw_ports.foreach(p    => {p.implement(0); p.add_io(io, port_index); port_index += 1; });
  }
  override def postMarkNet(fake: Int = 0) = {
    //println("[info] Parent" + getPathName +"|"+name);
    // The component names are not known until after markComponent is run.
    setMaster(memGen);
  }

  def read(addr: Num, oe: Bool, cs: Bool = null): T = {
    val read_port = new MemIPPort(memSpec, mem, 'read, addr, wrDataProto, null, oe, cs);
    read_ports += read_port;
    read_port.data_out;
  }
  def write(we: Bool, addr: Num, wrData: T, cs: Bool = null, wrMask: Bits = null) = {
    val write_port = new MemIPPort(memSpec, mem, 'write, addr, wrData, we, null, cs, wrMask);
    write_ports += write_port;
  }
  def rw(we: Bool, addr: Num, wrData: T, oe: Bool, cs: Bool = null, wrMask: Bits = null): T = {
    var rw_port = new MemIPPort(memSpec, mem, 'rw, addr, wrData, we, oe, cs, wrMask);
    rw_ports += rw_port;
    rw_port.data_out;
  }
  def setSize(s: Int) = {
    size = s;
  }
  def emitGeneratedMasterName(memGen: MemGen[T]) = {
    //println("[info] eGMN: "+memGen.name+"|"+memGen.getPathName);
    var res = memGen.getPathName + "_" + memGen.size + "x" + memGen.numWords;
    val port_pat: String = read_ports.length+"_"+write_ports.length+"_"+rw_ports.length;
    if (port_pat == "1_1_0" || port_pat == "0_0_2") {
      res += "_2P";
    } else if (port_pat == "0_0_1") {
      res += "_1P";
    } else {
      res += "_not_supported";
    }
    res;
  }
  def emitBuild(memGen: MemGen[T]) = {
    val res =
      "conf:\n" +
      "  baseName:   " + memGen.getPathName + "\n" +
      "  wordLength: " + memGen.size + "\n" +
      "  numWords:   " + memGen.numWords + "\n" +
      "  numRWPorts: " + rw_ports.length + "\n" +
      "  numRPorts:  " + read_ports.length + "\n" +
      "  numWPorts:  " + write_ports.length + "\n" +
      "  technology: 65\n" +
      "  opCond:     Typical\n" +
      "  debug:      False\n" +
      "  noBM:       " + (if (memGen.hasWrMask) "True" else "False") + "\n";
    val base_name = ensure_dir(targetVerilogRootDir + "/" + targetDir);
    val conf = new java.io.FileWriter(base_name + memGen.getPathName + ".conf");
    conf.write(res);
    conf.close;
    res;
  }
  // Disable implicit clock insertion.
  override def childrenContainsReg = {
    if (isEmittingC) {
      super.childrenContainsReg;
    } else {
      false;
    }
  }

  override def doCompileV(out: java.io.FileWriter, depth: Int): Unit = {
    containsReg = false; // Turn off implicit clock generation.
    super.doCompileV(out, depth);
    // The moduleName is set above.
    emitBuild(memGen);
  }
}

}
