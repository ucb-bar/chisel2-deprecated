package Mem {

import scala.collection.mutable.ListBuffer;
import Chisel._;
import Component._;

object MemGen {
  def apply[T <: Data](depth: Int, wrDataProto: T, memSpec: MemorySpec): MemGen[T] = {
    new MemGen(memSpec, depth, wrDataProto);
  }

  def apply[T <: Data](
      wen:         Bool,
      wrAddr:      UFix,
      wrData:      T,
      oen:         Bool       = null,
      rdAddr:      UFix       = null,
      rdData:      T          = null,
      cs:          Bool       = null,
      depth:       Int        = 0,
      rinLatency:  Int        = 1,
      routLatency: Int        = 1,
      wLatency:    Int        = 1,
      moduleName:  String     = "MemGen",
      memSpec:     MemorySpec = null): MemGen[T] = {
    val memorySpec = if (memSpec == null) new MemorySpecUCBSC else memSpec;
      
    val bit_width = wrData.getWidth;
    val address_width = wrAddr.getWidth;
    val depth_from_addr = 1 << address_width;
    val depth_gen = if (depth > 0) depth else depth_from_addr;
    if (depth > 0 && depth != depth_from_addr) {
      println("[warning} The specified depth "+depth+" does not correspond to the address width "+address_width);
    }
    def oen_node = if (!(oen == null)) oen else Bool(true);
    def cs_node = if (!(cs == null)) cs else Bool(true);
    //println("bit_width: "+bit_width+" address_width = "+address_width+", depth = "+depth_gen);

    val mem = new MemGen(memorySpec, depth_gen, wrData);
    if (!(rdAddr == null) && !(rdData == null)) {
      // Separate read and write ports.
      mem.write(wen, wrAddr, wrData, cs_node)
      def mem_read = mem.read(rdAddr);
      if (rdData.comp != null) {
        rdData.comp assign mem_read.toNode;
      } else {
        mem_read ^^ rdData;
      }
    } else if (rdAddr == null && !(rdData == null)) {
      // Define a single RW port, using the address wrAddr.
      def mem_read = mem.rw(wen, wrAddr, wrData, oen_node, cs_node);
      if (rdData.comp != null) {
        rdData.comp assign mem_read.toNode;
      } else {
        mem_read ^^ rdData;
      }
    } else {
      // Define only the write port; read ports can be added later.
      mem.write(wen, wrAddr, wrData, cs_node);
    }

    mem.check_config;
    mem.setMaster(moduleName);

    mem
  }

  def delay[T <: Data](expr: T, latency: Int = 1): T = {
    if (latency == 0) {
      expr;
    } else if (latency == 1) {
      Reg(expr);
    } else {
      delay(expr, latency - 1);
    }
  }
}

class MemGenPort[T <: Data](val memSpec: MemorySpec,
                            val memGen: MemGen[T],
                            // val memIP: MemIP[T],
                            val portType: Symbol,
                            val addr: Num,
                            val wrData: T,
                            val we: Bool,
                            val oe: Bool,
                            val cs: Bool = null,
                            val wrMask: Bits = null) {
  var size = wrData.getWidth;
  val addr_port = addr.clone.asInput;
  val ce_port   = Bool('input);
  val cs_port   = Bool('input);
  val data_out  = wrData.clone.asOutput;
  val oe_port   = Bool('input);
  val data_in   = wrData.clone.asInput;
  val we_port   = Bool('input);
  val wr_mask_port = if (wrMask == null) null else wrMask.clone.asInput;

  def implement(fake: Int = 0) = {
    addr_port assign addr;
    cs_port := (if (cs == null) Bool(true) else cs);
    ce_port.setIsClkInput;

    if (portType.equals('read)) {
      oe_port := oe;
      //memIP.read(addr_port, !oe_port, !cs_port) ^^ data_out;
    } else if (portType.equals('write)) {
      data_in ^^ wrData;
      we_port := we;
      // memIP.write(!we_port, addr_port, data_in, !cs_port, wrMask = wr_mask_port);
    } else if (portType.equals('rw)) {
      data_in ^^ wrData;
      oe_port := oe;
      we_port := we;
      // memIP.rw(!we_port, addr_port, data_in, !oe_port, !cs_port,
      //         wrMask = wr_mask_port) ^^ data_out;
    }

    if (!(wrMask == null)) {
      wr_mask_port ^^ wrMask;
      memGen.hasWrMask = true;
    }
  }

  def add_io(io: Bundle, port_index: Int) = {
    ce_port.setName("clk" + port_index);
    io += ce_port;
    addr_port.setName("addr" + port_index);
    io += addr_port;
    cs_port.setName("cs" + port_index);
    // cs_port.component = memGen;
    io += cs_port;
    if (portType.equals('write) || portType.equals('rw)) {
      data_in.setName("in" + port_index);
      io += data_in;
      we_port.setName("we" + port_index);
      io += we_port;
      if (!(wrMask == null)) {
        wr_mask_port.setName("wbm" + port_index);
        io += wr_mask_port;
      }
    }
    if (portType.equals('read) || portType.equals('rw)) {
      data_out.setName("out" + port_index);
      io += data_out;
      oe_port.setName("oe" + port_index);
      io += oe_port;
    }
  }
}

class MemGen[T <: Data](val memSpec: MemorySpec,
                        val numWords: Int,
                        val wrData: T) extends Component {
  val io = new Bundle();
  //val memIP = MemIP(numWords, wrData, memSpec);
  var memIP: Component = null;
  var masterName = "MemGen";
  var size = wrData.getWidth;
  var id = 0;
  val port_list  = ListBuffer[MemGenPort[T]]();
  var hasWrMask = false;
  def setMaster(m_name: String) = {
    masterName = m_name;
  }

  override def elaborate(fake: Int = 0) = {
    // println("[info] Elaborating memory "+ name + " of " + masterName);
    // Erase and reattach port elements to the io bundle, allowing the
    // names to change if needed.
    io.elementsCache = null;
    var port_index = 1;
    for (p <- port_list) {
      p.implement(0);
      p.add_io(io, port_index);
      port_index += 1;
    }
    memIP = MemIP(this);
    //memIP.setMaster(getPathName);
    //memIP.setMaster(masterName);
  }
  def check_config = {
    //memIP.check_config;
  }

  def read(addr: Num, oe: Bool = Bool(true), cs: Bool = Bool(true),
           rdData: T = null.asInstanceOf[T]): T = {
    val read_port = new MemGenPort(memSpec, this, 'read, addr, wrData, null, oe, cs);
    port_list += read_port;
    check_config;
    if (!(rdData == null)) {
      // Optional read output: Connect to an IO or wire.
      if (rdData.comp != null) {
        rdData.comp assign read_port.data_out;
      } else {
        read_port.data_out ^^ rdData;
      }
    }
    read_port.data_out;
  }
  def read(rp: ReadMemoryPort[T]) = {
    val read_port = new MemGenPort(memSpec, this, 'read, rp.addr, rp.readData, null, rp.oen, rp.cs);
    port_list += read_port;
    check_config;
    rp.readData <> read_port.data_out;
  }
  def write(we: Bool, addr: Num, write_data: T, cs: Bool = Bool(true),
            wrMask: Bits = null) = {
    var write_port = new MemGenPort(memSpec, this, 'write, addr, write_data,
                                    we, null, cs, wrMask);
    port_list += write_port;
    check_config;
  }
  def rw(we: Bool, addr: Num, wrData: T, oe: Bool = Bool(true), 
         cs: Bool = Bool(true), rdData: T = null.asInstanceOf[T],
         wrMask: Bits = null): T = {
    var rw_port = new MemGenPort(memSpec, this, 'rw, addr, wrData, we, oe, cs,
                                 wrMask);
    port_list += rw_port;
    check_config;
    if (!(rdData == null)) {
      if (rdData.comp != null) {
        rdData.comp assign rw_port.data_out;
      } else {
        rw_port.data_out ^^ rdData;
      }
    }
    rw_port.data_out;
  }
  def setSize(s: Int) = {
    size = s;
  }
  override def doCompileV(out: java.io.FileWriter, depth: Int): Unit = {
    val rval = super.doCompileV(out, depth);
    rval;
  }
}

}
