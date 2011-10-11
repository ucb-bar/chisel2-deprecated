package Mem {

import scala.collection.mutable.ListBuffer;
import Chisel._;
import Component._;

object MemGL {
  def apply[T <: Data](depth: Int, wrDataProto: T, memSpec: MemorySpec): MemGL[T] = {
    new MemGL(memSpec, depth, wrDataProto);
  }
}

class MemGLPort[T <: Data](val memSpec: MemorySpec,
                           val mem: MemCell[T],
                           val port_type: Symbol,
                           val addr: Num,
                           val data: T,
                           val we: Bool,
                           val oe: Bool,
                           val cs: Bool = null,
                           val wrMask: Bits = null) {
  //println("MemGLPort");
  var size = 0;

  // Declare a forward reference to port read data, for access from the port object.
  val data_out_pre = Wire(){Bits(width=data.getWidth)};
  val data_out_post = Wire(){Bits(width=data.getWidth)};
  val we_post = Wire(){Bits(width=1)};
  val activeHi = memSpec.active_high_ctrl != 0;

  def wire_port(io: Bundle, port_index: Int) = {
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

    val addr_width = addr.getWidth;

    if (!(wrMask == null)) {
      wrMask ^^ wr_mask_port;
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
    
    cs_port := (if (cs == null) Bool(true) else cs);

    ce_port.setIsClkInput;

    if (port_type.equals('write) || port_type.equals('rw)) {
      //data_in <> data;
      data_in ^^ data;
      we_port := we;
      
      val expanded_wbm = expand_wbm_to_bits(wrMask, memSpec.data_bits_per_mask_bit, data_in_bits);
      if (memSpec.active_high_ctrl != 0) {
        mem.write(we_port && cs_port, addr_port, data_in, expanded_wbm);
      } else {
        mem.write(!we_port && !cs_port, addr_port, data_in, expanded_wbm);
      }
    }

    if (port_type.equals('read) || port_type.equals('rw)) {
      val read_data = mem.read(addr_port);
      val zero_data = Fix(0, read_data.getWidth);
      val data_mux = Fix(dir = 'output);//zero_data.clone;
      //val data_out_reg = Reg(resetVal = zero_data);
      
      if (memSpec.active_high_ctrl != 0) {
        data_mux assign (Mux(oe_port && cs_port, read_data, zero_data));
      } else {
        data_mux assign (Mux(!oe_port && !cs_port, read_data, zero_data));
      }

      oe_port := oe;
      val data_mux_reg = Reg(data_mux, zero_data);
      // data_out assign data_mux;
      //data_out <> data_mux_reg;
      data_mux_reg ^^ data_out;
    }

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
  def catAddr(prev: Bits) = {
    if (prev == null) addr.toBits else Cat(addr, prev);
  }
  def catDataIn(prev: Bits) = {
    if (port_type == 'write || port_type == 'rw) {
      if (prev == null) data.toBits else Cat(data, prev);
    } else {
      if (prev == null) Fill(data.getWidth, Bits(0,1)) else Cat(Fill(data.getWidth, Bits(0,1)), prev);
    }      
  }

  def weToBits: Bits = { if (we == null) Bits(0,1) else if (activeHi) we.toBits else (!we).toBits }
  def csToBits: Bits = { if (cs == null) Bits(1,1) else if (activeHi) cs.toBits else (!cs).toBits }
  def oeToBits: Bits = { if (oe == null) Bits(1,1) else if (activeHi) oe.toBits else (!oe).toBits }
  def andCsWeToBits: Bits = { weToBits & csToBits }
  def andCsOeToBits: Bits = { oeToBits & csToBits }

  def getDataOut(data_out_vec: Bits, ind: Int) = {
    val w = data.getWidth;
    val offset = w * ind;
    data_out_vec(offset + w - 1, offset);
  }
  def add_io(io: Bundle, port_index: Int) = {
  }
}

class MemGL[T <: Data](memSpec: MemorySpec, numWords: Int, wrDataProto: T) extends BlackBox {
  val io = new Bundle();
  val mem = Mem(numWords, wrDataProto);
  var size = wrDataProto.getWidth;
  var addr_bit_width: Int = 0;
  var id = 0;
  var port_list  = ListBuffer[MemGLPort[T]]();
  var port_count = 0;
  val activeHi = memSpec.active_high_ctrl != 0;

  def setMaster(m_name: String) = {
    memSpec.set_master_name(m_name);
    setName(memSpec.emitGeneratedMasterName);
  }

  override def genName(n: String): String = {
    if (n == null || n.length() == 0) "" else "memGL_" + n;
  }
  def check_config(dummy_arg_to_prevent_visit_from_markComponent: Int = 0) = {
    port_count = port_list.length;
    io.elementsCache = null; // Detach elements from the io bundle.
    wireMem(0);
    setVerilogParameters(genVerilogParameters(0));
    memSpec.set_dimensions(numWords, size, 1);
    //memSpec.set_port_count(read_ports.length, write_ports.length, rw_ports.length);
  }

  def read(addr: Num, oe: Bool, cs: Bool = null): T = {
    val read_port = new MemGLPort(memSpec, mem, 'read, addr, wrDataProto, null, oe, cs);
    port_list += read_port;
    read_port.data_out_post.toFix.asInstanceOf[T];
  }
  def write(we: Bool, addr: Num, wrData: T, cs: Bool = null, wrMask: Bits = null) = {
    val write_port = new MemGLPort(memSpec, mem, 'write, addr, wrData, we, null, cs, wrMask);
    port_list += write_port;
  }
  def rw(we: Bool, addr: Num, wrData: T, oe: Bool, cs: Bool = null, wrMask: Bits = null): T = {
    var rw_port = new MemGLPort(memSpec, mem, 'rw, addr, wrData, we, oe, cs, wrMask);
    port_list += rw_port;
    rw_port.data_out_post.toFix.asInstanceOf[T];
  }
  def setSize(s: Int) = {
    size = s;
  }

  // Disable implicit clock insertion.
  override def childrenContainsReg = {
    if (isEmittingC) {
      super.childrenContainsReg;
    } else {
      false;
    }
  }

  def getAddrWidth = {
    val port_sizes = port_list.map(port => port.addr.getWidth);
    port_sizes.max;
  }
  def wireMem(dummy_arg_to_prevent_visit_from_markComponent: Int = 0) = {
    println("wireMem");
    val addr_bit_width = getAddrWidth;

    val addr_port = Bits(width = addr_bit_width * port_count, dir = 'input);
    val ce_port   = Bits(width = port_count, dir = 'input);
    //val cs_port   = Bits(width = port_count, dir = 'input);
    val data_out  = Bits(width = size * port_count, dir = 'output);
    val oe_port   = Bits(width = port_count, dir = 'input);
    val data_in   = Bits(width = size * port_count, dir = 'input);
    val we_port   = Bits(width = port_count, dir = 'input);
    val rst_port  = Bits(width = port_count, dir = 'input);

    // Connect Clock inputs on each port.
    val concatClock = Wire(){Bits(width=port_count)};
    val fake_clk: Bits = Input("clk", 1);
    concatClock := Fill(port_count, fake_clk);
    concatClock ^^ ce_port;
    ce_port.isClkInput = true;

    //for (ind <- 0 until port_count) Fill{ ce_port(ind).setIsClkInput }

    // Concatenate all address ports into a single address vector.
    val concatAddr = Wire(){Bits(width=addr_bit_width * port_count)};
    concatAddr := port_list.reverse.map(_.addr.toBits).reduceLeft(Cat(_,_));
    concatAddr ^^ addr_port;

    // Concatenate all data write ports into a single data vector.
    val concatDataIn = Wire(){Bits(width=size * port_count)};
    concatDataIn := port_list.reverse.map(_.data.toBits).reduceLeft(Cat(_,_));
    concatDataIn ^^ data_in;

    // Concatenate all write enables
    val concatWriteEn = Wire(){Bits(width=port_count)};
    concatWriteEn := port_list.reverse.map(_.andCsWeToBits).reduceLeft(Cat(_,_));
    concatWriteEn ^^ we_port;

    // Concatenate all output enables
    val concatOutEn = Wire(){Bits(width=port_count)};
    concatOutEn := port_list.reverse.map(_.andCsOeToBits).reduceLeft(Cat(_,_));
    concatOutEn ^^ oe_port;

    // Create all reset inputs tied low
    val concatReset = Bits("h0", port_count);
    concatReset ^^ rst_port;

    val concatDataOut = Wire(){Bits(width = size * port_count)};

    for ((p, ind) <- (port_list zip (0 until port_list.length))) {
      val addr_offset = ind*addr_bit_width;
      val data_offset = ind*size;

      // Add a memory write port for emulation.
      if (p.port_type == 'write || p.port_type == 'rw) {
        p.we_post := we_port(ind);
        mem.write(p.we_post, addr_port(addr_offset + addr_bit_width - 1, addr_offset),
                  data_out(data_offset + size - 1, data_offset).toFix.asInstanceOf[T]);
      }

      // Add emulation read ports or zero fill if none for the port.
      if (p.port_type == 'read || p.port_type == 'rw) {
        val mem_read =  mem.read(addr_port(addr_offset + addr_bit_width - 1, addr_offset)).toBits
        val out_en = oe_port(ind);
        p.data_out_pre := Reg(Mux(out_en.toBool, mem_read, Bits(0, size)));
      } else {
        p.data_out_pre := Fill(size, Bits(0,1));
      }
      p.data_out_post := data_out(data_offset + size - 1, data_offset);
    }

    // Reduce the memory read data to a single port:
    concatDataOut := port_list.reverse.map(_.data_out_pre).reduceLeft(Cat(_,_));
    concatDataOut ^^ data_out;

    addr_port.setName("Address");
    io += addr_port;
    data_in.setName("DIn");
    io += data_in;
    we_port.setName("Write");
    io += we_port;
    //cs_port.setName("CS");
    //io += cs_port;
    oe_port.setName("Enable");
    io += oe_port;
    ce_port.setName("Clock");
    io += ce_port;
    data_out.setName("DOut");
    io += data_out;
    rst_port.setName("Reset");
    io += rst_port;
  }

  def genVerilogParameters(fake: Int = 0): String = {
    var res = "#(\n\t.DWidth = " + size + ",\n\t.AWidth = " + getAddrWidth + ",\n";
    res += "\t.NPorts = " + port_count + ",\n";
    res += "\t.WriteMask = " + port_count + "'" + genWriteMask + ",\n";
    res += "\t.ReadMask = " + port_count + "'" + genReadMask + ")";
    res;
  }

  def genWriteMask = {
    "b" + port_list.reverse.map(p => if (p.port_type == 'write || p.port_type == 'rw) "1" else "0").reduceLeft(_+_)
  }
  def genReadMask = {
    "b" + port_list.reverse.map(p => if (p.port_type == 'read || p.port_type == 'rw) "1" else "0").reduceLeft(_+_);
  }

  override def doCompileV(out: java.io.FileWriter, depth: Int): Unit = {
    containsReg = false; // Turn off implicit clock generation.
    super.doCompileV(out, depth);
    // The moduleName is set above.
    memSpec.emitBuild;
  }
}

}
