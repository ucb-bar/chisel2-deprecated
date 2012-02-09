package Mem {

import scala.collection.mutable.ListBuffer;
import java.io.File;
import Chisel._;
import Component._;

object MemRTL {
  def apply[T <: Data](memGen: MemGen[T], depth: Int, wrDataProto: T): MemRTL[T] = {
    new MemRTL(memGen, depth, wrDataProto);
  }
  def apply[T <: Data](memGen: MemGen[T]): MemRTL[T] = {
    val memIP = new MemRTL(memGen, memGen.numWords, memGen.wrData);
    for (p <- memGen.port_list) {
      val mip_port = new MemRTLPort(memGen.memSpec, memIP.mem, p.portType,
                                   p.addr_port, p.data_in, p.we_port,
                                   p.oe_port, p.cs_port, p.wr_mask_port);
      if (p.portType == 'read) {
        // Connect the read data to the MemGen output port
        mip_port.data_mux_reg ^^ p.data_out;
        memIP.read_ports += mip_port;
      } else if (p.portType == 'write) {
        memIP.write_ports += mip_port;
      } else if (p.portType == 'rw) {
        mip_port.data_mux_reg ^^ p.data_out;
        memIP.rw_ports += mip_port;
      } else {
        println("[error] MemRTL constructor called with invalid port type" + p.portType);
      }
    }
    memIP;
  }
}

class MemRTLPort[T <: Data](val memSpec: MemorySpec,
                           val mem: MemCell[T],
                           val port_type: Symbol,
                           val addr: Num,
                           val data: T,
                           val we: Bool,
                           val oe: Bool,
                           val cs: Bool = null,
                           val wrMask: Bits = null) {
  var size = 0;//data.toNode.getWidth;

  val cs_wire = Wire(){Bool()};
  var data_mux_reg = Wire(){data.clone};
  var wr_mask_bits = 0;
  var data_in_bits = 0;

  //implement(0);
  def implement(fake: Int = 0) = {
    // println("[info] MemRTLPort implement");
    if (!(wrMask == null)) {
      ///wr_mask_port := wrMask;
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

    cs_wire := (if (cs == null) Bool(true) else !cs);
    if (port_type.equals('write) || port_type.equals('rw)) {
      if (wrMask == null) {
        mem.write(we && cs_wire, addr, data)
      } else {
        val expanded_wbm = expand_wbm_to_bits(wrMask, memSpec.data_bits_per_mask_bit, data_in_bits);
        mem.write(we && cs_wire, addr, data, expanded_wbm);
      }
    }

    val zero_data: T = data.fromNode(Fix(0, data.getWidth)).asInstanceOf[T];
    val data_mux = data.clone.asInstanceOf[T];
    
    if (port_type.equals('read) || port_type.equals('rw)) {
      data_mux assign Mux(oe && cs, mem.read(addr), zero_data);
    } else {
      data_mux assign zero_data;
    }
    data_mux_reg = Reg(data_mux);
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
}

class MemRTL[T <: Data](memGen: MemGen[T], numWords: Int, wrDataProto: T) {
  val memSpec = memGen.memSpec;
  val io = new Bundle();
  val mem = Mem(numWords, wrDataProto);
  var size = wrDataProto.getWidth;
  var id = 0;
  var read_ports  = ListBuffer[MemRTLPort[T]]();
  var write_ports = ListBuffer[MemRTLPort[T]]();
  var rw_ports    = ListBuffer[MemRTLPort[T]]();

  def elaborate(fake: Int = 0) = {
    var port_index = 1;
    //println("[info] MemRTL Elaborate");
    // The ports must be implemented and named before markComponent is run.
    memSpec.set_dimensions(numWords, size, 1);
    memSpec.set_port_count(read_ports.length, write_ports.length, rw_ports.length);
  }

  def read(addr: Num, oe: Bool, cs: Bool = null): T = {
    val read_port = new MemRTLPort(memSpec, mem, 'read, addr, wrDataProto, null, oe, cs);
    read_ports += read_port;
    read_port.data_mux_reg;
  }
  def write(we: Bool, addr: Num, wrData: T, cs: Bool = null, wrMask: Bits = null) = {
    val write_port = new MemRTLPort(memSpec, mem, 'write, addr, wrData, we, null, cs, wrMask);
    write_ports += write_port;
  }
  def rw(we: Bool, addr: Num, wrData: T, oe: Bool, cs: Bool = null, wrMask: Bits = null): T = {
    var rw_port = new MemRTLPort(memSpec, mem, 'rw, addr, wrData, we, oe, cs, wrMask);
    rw_ports += rw_port;
    rw_port.data_mux_reg;
  }
  def setSize(s: Int) = {
    size = s;
  }
  def ensure_dir(dir: String) = {
    val d = dir + (if (dir == "" || dir(dir.length-1) == '/') "" else "/");
    new File(d).mkdirs();
    d
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
}

}
