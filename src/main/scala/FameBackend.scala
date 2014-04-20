package Chisel
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.{Queue=>ScalaQueue}

object FameDecoupledIO
{
  def connect[T <: Bundle](flattened: FameDecoupledIO[Bits], connectTo: FameDecoupledIO[T], tgt_bits_type: Bundle): Unit = {
    val is_flip = (flattened.host_ready.dir == OUTPUT)
    if(is_flip){
      flattened.host_valid := connectTo.host_valid
      connectTo.host_ready := flattened.host_ready
      flattened.target.valid := connectTo.target.valid
      connectTo.target.ready := flattened.target.ready
      flattened.target.bits := connectTo.target.bits.toBits
    } else {
      connectTo.host_valid := flattened.host_valid
      flattened.host_ready := connectTo.host_ready
      connectTo.target.valid := flattened.target.valid
      flattened.target.ready := connectTo.target.ready
      connectTo.target.bits := tgt_bits_type.fromBits(flattened.target.bits)
    }
  }
}

class FameDecoupledIO[T <: Data](data: T) extends Bundle
{
  val host_valid = Bool(OUTPUT)
  val host_ready = Bool(INPUT)
  val target = new DecoupledIO(data)
  override def clone: this.type = { new FameDecoupledIO(data).asInstanceOf[this.type]}
}

class FameQueue[T <: Data] (val entries: Int)(data: => T) extends Module
{
  val io = new Bundle{
    val deq = new FameDecoupledIO(data)
    val enq = new FameDecoupledIO(data).flip()
  }
  
  val target_queue = Module(new Queue(data, entries))
  val tracker = Module(new FameQueueTracker(entries, entries))
  
  target_queue.io.enq.valid := io.enq.host_valid && io.enq.target.valid
  target_queue.io.enq.bits := io.enq.target.bits
  io.enq.target.ready := target_queue.io.enq.ready
  
  io.deq.target.valid := tracker.io.entry_avail && target_queue.io.deq.valid
  io.deq.target.bits := target_queue.io.deq.bits
  target_queue.io.deq.ready := io.deq.host_ready && io.deq.target.ready && tracker.io.entry_avail
  
  tracker.io.tgt_queue_count := target_queue.io.count
  tracker.io.produce := io.enq.host_valid && io.enq.host_ready
  tracker.io.consume := io.deq.host_valid && io.deq.host_ready
  tracker.io.tgt_enq := target_queue.io.enq.valid && target_queue.io.enq.ready
  tracker.io.tgt_deq := io.deq.target.valid && target_queue.io.deq.ready

  io.enq.host_ready := !tracker.io.full && target_queue.io.enq.ready 
  io.deq.host_valid := !tracker.io.empty
  
}

class FameQueueTrackerIO() extends Bundle{
  val tgt_queue_count = UInt(INPUT)
  val produce = Bool(INPUT)
  val consume = Bool(INPUT)
  val tgt_enq = Bool(INPUT)
  val tgt_deq = Bool(INPUT)
  val empty = Bool(OUTPUT)
  val full = Bool(OUTPUT)
  val entry_avail = Bool(OUTPUT)
}

class FameQueueTracker(num_tgt_entries: Int, num_tgt_cycles: Int) extends Module{
  val io = new FameQueueTrackerIO()
  val aregs = Vec.fill(num_tgt_cycles){ Reg(init = UInt(0, width = log2Up(num_tgt_entries))) }
  val tail_pointer = Reg(init = UInt(1, width = log2Up(num_tgt_cycles)))
  
  val next_tail_pointer = UInt()
  tail_pointer := next_tail_pointer
  next_tail_pointer := tail_pointer
  when(io.produce && !io.consume){
    next_tail_pointer := tail_pointer + UInt(1)
  }.elsewhen(!io.produce && io.consume){
    next_tail_pointer := tail_pointer - UInt(1)
  }
  for (i <- 1 until num_tgt_cycles - 1){
    val next_reg_val = UInt()
    aregs(i) := next_reg_val
    next_reg_val := aregs(i)
    when(UInt(i) === tail_pointer){
      when(io.produce && io.tgt_enq && !io.consume){
        next_reg_val := aregs(i - 1) + UInt(1)
      }.elsewhen(io.produce && !io.tgt_enq && !io.consume){
        next_reg_val := aregs(i - 1)
      }
    }.elsewhen(UInt(i) === tail_pointer - UInt(1)){
      when(io.produce && io.tgt_enq && io.consume && io.tgt_deq){
      }.elsewhen(io.produce && io.tgt_enq && io.consume && !io.tgt_deq){
        next_reg_val := aregs(i) + UInt(1)
      }.elsewhen(io.produce && !io.tgt_enq && io.consume && io.tgt_deq){
        next_reg_val := aregs(i) - UInt(1)
      }
    }.otherwise{
      when(io.produce && io.tgt_enq && io.consume && io.tgt_deq){
        next_reg_val := aregs(i + 1) - UInt(1)
      }.elsewhen(io.produce && io.tgt_enq && io.consume && !io.tgt_deq){
        next_reg_val := aregs(i + 1)
      }.elsewhen(io.produce && !io.tgt_enq && io.consume && io.tgt_deq){
        next_reg_val := aregs(i + 1) - UInt(1)
      }.elsewhen(io.produce && !io.tgt_enq && io.consume && !io.tgt_deq){
        next_reg_val := aregs(i + 1)
      }.elsewhen(!io.produce && io.consume && io.tgt_deq){
        next_reg_val := aregs(i + 1) - UInt(1)
      }.elsewhen(!io.produce && io.consume && !io.tgt_deq){
        next_reg_val := aregs(i + 1)
      }
    }
  }
  val next_reg_val0 = UInt()
  aregs(0) := next_reg_val0
  next_reg_val0 := aregs(0)
  when(UInt(0) === tail_pointer){
    when(io.produce && io.tgt_enq && !io.consume){
      next_reg_val0 := io.tgt_queue_count + UInt(1)
    }.elsewhen(io.produce && !io.tgt_enq && io.consume && io.tgt_deq){
    }.elsewhen(io.produce && !io.tgt_enq && io.consume && !io.tgt_deq){
    }.elsewhen(io.produce && !io.tgt_enq && !io.consume){
      next_reg_val0 := io.tgt_queue_count
    }
  }.elsewhen(UInt(0) === tail_pointer - UInt(1)){
    when(io.produce && io.tgt_enq && io.consume && !io.tgt_deq){
      next_reg_val0 := aregs(0) + UInt(1)
    }.elsewhen(io.produce && !io.tgt_enq && io.consume && io.tgt_deq){
      next_reg_val0 := aregs(0) - UInt(1)
    }.elsewhen(io.produce && !io.tgt_enq && io.consume && !io.tgt_deq){
    }
  }.otherwise{
    when(io.produce && io.tgt_enq && io.consume && io.tgt_deq){
      next_reg_val0 := aregs(1) - UInt(1)
    }.elsewhen(io.produce && io.tgt_enq && io.consume && !io.tgt_deq){
      next_reg_val0 := aregs(1)
    }.elsewhen(io.produce && !io.tgt_enq && io.consume && io.tgt_deq){
      next_reg_val0 := aregs(1) - UInt(1)
    }.elsewhen(io.produce && !io.tgt_enq && io.consume && !io.tgt_deq){
      next_reg_val0 := aregs(1)
    }.elsewhen(!io.produce && io.consume && io.tgt_deq){
      next_reg_val0 := aregs(1) - UInt(1)
    }.elsewhen(!io.produce && io.consume && !io.tgt_deq){
      next_reg_val0 := aregs(1)
    }
  }
  val next_reg_val_last = UInt()
  aregs(num_tgt_cycles - 1) := next_reg_val_last
  next_reg_val_last := aregs(num_tgt_cycles - 1)
  when(UInt(num_tgt_cycles - 1) === tail_pointer){
    when(io.produce && io.tgt_enq && io.consume && !io.tgt_deq){
    }.elsewhen(io.produce && io.tgt_enq && !io.consume){
      next_reg_val_last := aregs(num_tgt_cycles - 1 - 1) + UInt(1)
    }.elsewhen(io.produce && !io.tgt_enq && !io.consume){
      next_reg_val_last := aregs(num_tgt_cycles - 1 - 1)
    }
  }.elsewhen(UInt(num_tgt_cycles - 1) === tail_pointer - UInt(1)){
    when(io.produce && io.tgt_enq && io.consume && !io.tgt_deq){
      next_reg_val_last := aregs(num_tgt_cycles - 1) + UInt(1)
    }.elsewhen(io.produce && !io.tgt_enq && io.consume && io.tgt_deq){
      next_reg_val_last := aregs(num_tgt_cycles - 1) - UInt(1)
    }.elsewhen(io.produce && !io.tgt_enq && io.consume && !io.tgt_deq){
    }
  }
  io.full := tail_pointer === UInt(num_tgt_cycles)
  io.empty := tail_pointer === UInt(0)
  io.entry_avail := aregs(0) != UInt(0)
}

class RegIO[T <: Data](data: T) extends Bundle
{
  val bits = data.clone.asOutput
}

class FameReg[T <: Data] (entries: Int)(data: T, resetVal: T = null) extends Module
{
  val io = new Bundle{
    val deq = new DecoupledIO(data)
    val enq = new DecoupledIO(data).flip()
  }
  
  val shiftRegs = new ArrayBuffer[T]
  for(i <- 0 until entries){
    if(i == 0){
      shiftRegs += RegInit(resetVal)
    } else {
      shiftRegs += Reg(data.clone)
    }
  }
  
  val tailPointer = Reg(init = UInt(1, width = log2Up(entries)))
  val enqueue = io.enq.valid && io.enq.ready
  val dequeue = io.deq.valid && io.deq.ready
  when(enqueue && !dequeue){
    tailPointer := tailPointer + UInt(1)
  }.elsewhen(!enqueue && dequeue){
    tailPointer := tailPointer - UInt(1)
  }
  val empty = tailPointer === UInt(0)
  val full = (tailPointer === UInt(entries))

  for(i <- 0 until (entries - 1)){
    when(dequeue){
      shiftRegs(i) := shiftRegs(i + 1)
    }
  }
  
  for(i <- 0 until entries){
    when(UInt(i) === tailPointer){
      when(enqueue){
        when(!dequeue){
          shiftRegs(i) := io.enq.bits
        }
      }
    }.elsewhen(UInt(i) === (tailPointer - UInt(1))){
      when(enqueue){
        when(dequeue){
          shiftRegs(i) := io.enq.bits
        }
      }
    }
  }
  io.deq.valid := !empty
  io.deq.bits := shiftRegs(0)
  io.enq.ready := !full

}

class Fame1WrapperIO(num_queues: Int, num_regs: Int, num_debug: Int) extends Bundle {
  var queues:Vec[FameDecoupledIO[Bits]] = null
  if(num_queues > 0) {
    queues = Vec.fill(num_queues){ new FameDecoupledIO(Bits())}
  }
  var regs:Vec[DecoupledIO[Bits]] = null
  if(num_regs > 0) {
    regs = Vec.fill(num_regs){ new DecoupledIO(Bits())}
  }
  var debug:Vec[Bits] = null
  if(num_debug > 0) {
    debug = Vec.fill(num_debug){Bits()}
  }
}

class Fame1Wrapper(f: => Module) extends Module {
  def transform(isTop: Boolean, module: Module, parent: Module): Unit = {
    Fame1Transform.fame1Modules += module
    val isFire = Bool(INPUT)
    isFire.isIo = true
    isFire.setName("is_fire")
    isFire.component = module
    Fame1Transform.fireSignals(module) = isFire
    if(!isTop){
      Predef.assert(Fame1Transform.fireSignals(parent) != null)
      isFire := Fame1Transform.fireSignals(parent)
    }
    module.io.asInstanceOf[Bundle] += isFire
    for(submodule <- module.children){
      transform(false, submodule, module)
    }
  }
  
  val originalModule = Module(f)
  transform(true, originalModule, null)

  //counter number of RegIO and Decoupled IO in original module
  var num_decoupled_io = 0
  var num_reg_io = 0
  var num_debug_io = 0
  for ((name, io) <- originalModule.io.asInstanceOf[Bundle].elements){ 
    io match { 
      case q : DecoupledIO[_] => num_decoupled_io += 1; 
      case r : RegIO[_] => num_reg_io += 1;
      case _ => {
        if (name != "is_fire") {
          num_debug_io += 1
        }
      }
    }
  }

  val io = new Fame1WrapperIO(num_decoupled_io, num_reg_io, num_debug_io)
  
  val RegIOs = new HashMap[String, DecoupledIO[Bits]]()
  val DecoupledIOs  = new HashMap[String, FameDecoupledIO[Bits]]()
  val DebugIOs = new HashMap[String, Data]()

  var decoupled_counter = 0
  var reg_counter = 0
  var debug_counter = 0 
  //populate fame1RegIO and fame1DecoupledIO bundles with the elements from the original RegIO and DecoupleIOs
  for ((name, ioNode) <- originalModule.io.asInstanceOf[Bundle].elements) {
    ioNode match {
      case decoupled : DecoupledIO[_] => {
        val is_flip = (decoupled.ready.dir == OUTPUT)
        val fame1Decoupled      = io.queues(decoupled_counter)
        if (is_flip) {
          fame1Decoupled.flip()
          fame1Decoupled.target.ready := decoupled.ready
          decoupled.valid := fame1Decoupled.target.valid
          val decoupledBitsClone = decoupled.bits.clone()
          decoupled.bits := decoupledBitsClone.fromBits(fame1Decoupled.target.bits)
        } else {
          decoupled.ready := fame1Decoupled.target.ready
          fame1Decoupled.target.bits := decoupled.bits.toBits
          fame1Decoupled.target.valid := decoupled.valid 
        }
        DecoupledIOs(name) = fame1Decoupled
        decoupled_counter += 1
      }
      case reg : RegIO[_] => {
        val is_flip = (reg.bits.flatten(0)._2.dir == INPUT)
        val fame1RegIO = io.regs(reg_counter)
        if (is_flip) {
          fame1RegIO.flip()
          val regBitsClone = reg.bits.clone()
          reg.bits := regBitsClone.fromBits(fame1RegIO.bits)
        } else {
          fame1RegIO.bits := reg.bits.toBits
        }
        RegIOs(name) = fame1RegIO
        reg_counter += 1
      }
      case _ => {
        if (name != "is_fire") {
          Predef.assert(ioNode.isInstanceOf[Bits])
          if(ioNode.toBits.dir == INPUT){
            io.debug(debug_counter).asInput
            ioNode := io.debug(debug_counter)
          } else {
            io.debug(debug_counter).asOutput
            io.debug(debug_counter) := ioNode.toBits
          }
          DebugIOs(name) = io.debug(debug_counter)
          debug_counter += 1
        }
      }
    }
  }
  //generate fire_tgt_clk signal
  var fire_tgt_clk = Bool(true)
  if (io.queues != null){
    for (q <- io.queues)
      fire_tgt_clk = fire_tgt_clk && 
        (if (q.host_valid.dir == OUTPUT) q.host_ready else q.host_valid)
  }
  if (io.regs != null){
    for (r <- io.regs) {
      fire_tgt_clk = fire_tgt_clk && 
        (if (r.valid.dir == OUTPUT) r.ready else r.valid)
    }
  }
  
  //generate host read and host valid signals
  Fame1Transform.fireSignals(originalModule) := fire_tgt_clk
  if (io.queues != null){
    for (q <- io.queues) {
      if (q.host_valid.dir == OUTPUT) 
        q.host_valid := fire_tgt_clk
      else
        q.host_ready := fire_tgt_clk
    }
  }
  if (io.regs != null){
    for (r <- io.regs) {
      if (r.valid.dir == OUTPUT) 
        r.valid := fire_tgt_clk
      else
        r.ready := fire_tgt_clk
    }
  }
}

object Fame1Transform {
  val fame1Modules = new HashSet[Module]
  val fireSignals = new HashMap[Module, Bool]
}

trait Fame1Transform extends Backend {
  private def collectMems(module: Module): ArrayBuffer[(Module, Mem[Data])] = {
    val mems = new ArrayBuffer[(Module, Mem[Data])]
    //find all the mems in FAME1 modules
    def findMems(module: Module): Unit = {
      if(Fame1Transform.fame1Modules.contains(module)){
        for(mem <- module.nodes.filter(_.isInstanceOf[Mem[Data]])){
          mems += ((module, mem.asInstanceOf[Mem[Data]]))
        }
      }
      for(childModule <- module.children){
        findMems(childModule)
      }
    }
    findMems(module)
    return mems
  }
  
  private def appendFireToRegWriteEnables(top: Module) = {
    //find regs that are part of sequential mem read ports
    val mems = collectMems(top)
    val seqMemReadRegs = new HashSet[Reg]
    for((module, mem) <- mems){
      val memSeqReads = mem.seqreads ++ mem.readwrites.map(_.read)
      /*if(mem.seqRead){
        for(memRead <- mem.reads){
          seqMemReadRegs += memRead.addr.inputs(0).asInstanceOf[Reg]
        }
      }*/
      for(memSeqRead <- memSeqReads){
        seqMemReadRegs += memSeqRead.addrReg
      }
    }

    //find all the registers in FAME1 modules
    val regs = new ArrayBuffer[(Module, Reg)]
    def findRegs(module: Module): Unit = {
      if(Fame1Transform.fame1Modules.contains(module)){
        for(reg <- module.nodes.filter(_.isInstanceOf[Reg])){
          if(!seqMemReadRegs.contains(reg.asInstanceOf[Reg])){
            regs += ((module, reg.asInstanceOf[Reg]))
          }
        }
      }
      for(childModule <- module.children){
        findRegs(childModule)
      }
    }
    findRegs(top)
    
    
    for((module, reg) <- regs){
      reg.enable = reg.enable && Fame1Transform.fireSignals(module)
      if(reg.updates.length == 0){
        val regOutput = Bits()
        regOutput.inputs += reg
        val regMux = Bits()
        regMux.inputs += reg.inputs(0)
        reg.inputs(0) = Mux(Fame1Transform.fireSignals(module), regMux, regOutput)
      } else {
        for(i <- 0 until reg.updates.length){
          val wEn = reg.updates(i)._1
          val wData = reg.updates(i)._2
          reg.updates(i) = ((wEn && Fame1Transform.fireSignals(module), wData))
        }
      }
    }
  }
 
  private def appendFireToMemEnables(top: Module) = {
    val mems = collectMems(top)

    for((module, mem) <- mems){
      val memWrites = mem.writes ++ mem.readwrites.map(_.write)
      val memSeqReads = mem.seqreads ++ mem.readwrites.map(_.read)
      for(memWrite <- memWrites){
        if(mem.seqRead){
          if(Module.backend.isInstanceOf[CppBackend]){
            if(memWrite.inputs(0).asInstanceOf[Data].comp != null && memWrite.inputs(1).asInstanceOf[Data].comp != null){//huge hack for extra MemWrite generated for seqread mems in CPP backed; if both the cond and enable both happen to be directly from registers, this will fail horribly
              memWrite.inputs(1) = memWrite.inputs(1).asInstanceOf[Bool] && Fame1Transform.fireSignals(module)
            } else {
              memWrite.inputs(1) = Bool(false)
            }
          } else {
            memWrite.inputs(1) = memWrite.inputs(1).asInstanceOf[Bool] && Fame1Transform.fireSignals(module)
          }
        } else {
          memWrite.inputs(1) = memWrite.inputs(1).asInstanceOf[Bool] && Fame1Transform.fireSignals(module)
        }
      }
      for(memSeqRead <- memSeqReads){
        Predef.assert(memSeqRead.addrReg.updates.length == 1)
        val oldReadAddr = Bits()
        oldReadAddr.inputs += memSeqRead.addrReg.updates(0)._2
        val oldReadAddrReg = Reg(Bits())
        oldReadAddrReg.comp.component = module
        oldReadAddrReg.comp.asInstanceOf[Reg].enable = if (oldReadAddrReg.comp.asInstanceOf[Reg].isEnable) oldReadAddrReg.comp.asInstanceOf[Reg].enable || Fame1Transform.fireSignals(module) else Fame1Transform.fireSignals(module)
        oldReadAddrReg.comp.asInstanceOf[Reg].isEnable = true
        oldReadAddrReg.comp.asInstanceOf[Reg].updates += ((Fame1Transform.fireSignals(module), oldReadAddr))
        
        val newReadAddr = Mux(Fame1Transform.fireSignals(module), oldReadAddr, oldReadAddrReg)
        
        val oldReadEn = Bool()
        oldReadEn.inputs += memSeqRead.addrReg.updates(0)._1
        val renReg = Reg(init=Bool(false))
        renReg.comp.component = module
        
        renReg.comp.asInstanceOf[Reg].enable = if(renReg.comp.asInstanceOf[Reg].isEnable) renReg.comp.asInstanceOf[Reg].enable || Fame1Transform.fireSignals(module) else Fame1Transform.fireSignals(module)
        renReg.comp.asInstanceOf[Reg].isEnable = true
        renReg.comp.asInstanceOf[Reg].updates += ((Fame1Transform.fireSignals(module), oldReadEn))
        val newRen = Mux(Fame1Transform.fireSignals(module), oldReadEn, renReg)
        
        memSeqRead.addrReg.enable = newRen
        memSeqRead.addrReg.updates.clear
        memSeqRead.addrReg.updates += ((newRen, newReadAddr))
      }
    }
  }
  
  
  preElaborateTransforms += ((top: Module) => collectNodesIntoComp(initializeDFS))
  preElaborateTransforms += ((top: Module) => appendFireToRegWriteEnables(top))
  preElaborateTransforms += ((top: Module) => top.genAllMuxes)
  preElaborateTransforms += ((top: Module) => appendFireToMemEnables(top))
  preElaborateTransforms += ((top: Module) => collectNodesIntoComp(initializeDFS))
  preElaborateTransforms += ((top: Module) => top.genAllMuxes)
}

class Fame1CppBackend extends CppBackend with Fame1Transform
class Fame1VerilogBackend extends VerilogBackend with Fame1Transform
class Fame1FPGABackend extends FPGABackend with Fame1Transform

/*
class Fame5WrapperIO(num_copies: Int, num_queues: Int, num_regs: Int, num_debug: Int) extends Bundle {
  var queues:Vec[FameDecoupledIO[Bits]] = null
  if(num_queues > 0) {
    queues = Vec.fill(num_copies*num_queues){ new FameDecoupledIO(Bits())}
  }
  var regs:Vec[DecoupledIO[Bits]] = null
  if(num_regs > 0) {
    regs = Vec.fill(num_copies*num_regs){ new DecoupledIO(Bits())}
  }
  var debug:Vec[Bits] = null
  if(num_debug > 0) {
    debug = Vec.fill(num_copies*num_debug){Bits()}
  }
}

class Fame5Wrapper(num_copies: Int, f: => Module) extends Module {
  def addFireToIO(isTop: Boolean, module: Module, parent: Module): Unit = {
    Fame5Transform.fame1Modules += module
    val isFire = Bool(INPUT)
    isFire.isIo = true
    isFire.setName("is_fire")
    isFire.component = module
    Fame5Transform.fireSignals(module) = isFire
    if(!isTop){
      Predef.assert(Fame5Transform.fireSignals(parent) != null)
      isFire := Fame5Transform.fireSignals(parent)
    }
    module.io.asInstanceOf[Bundle] += isFire
    for(submodule <- module.children){
      addFireToIO(false, submodule, module)
    }
  }
  
  def replicateIO(module: Module): Unit = {
  }

  val originalModules = new ArrayBuffer[Module]
  for(i <- 0 until num_copies){
    originalModules += Module(f)
  }
  for(originalModule <- originalModules){
    addFireToIO(true, originalModule, null)
  }
  
  //counter number of RegIO and Decoupled IO in original module
  var num_decoupled_io = 0
  var num_reg_io = 0
  var num_debug_io = 0
  for ((name, io) <- originalModules(0).io.asInstanceOf[Bundle].elements){ 
    io match { 
      case q : DecoupledIO[_] => num_decoupled_io += 1; 
      case r : RegIO[_] => num_reg_io += 1;
      case _ => {
        if (name != "is_fire") {
          num_debug_io += 1
        }
      }
    }
  }

  val io = new Fame5WrapperIO(num_copies, num_decoupled_io, num_reg_io, num_debug_io)
  
  val RegIOs = new ArrayBuffer[HashMap[String, DecoupledIO[Bits]]]()
  val DecoupledIOs  = new ArrayBuffer[HashMap[String, FameDecoupledIO[Bits]]]()
  val DebugIOs = new ArrayBuffer[HashMap[String, Data]]()

  for(i <- 0 until num_copies){
    RegIOs += new HashMap[String, DecoupledIO[Bits]]()
    DecoupledIOs  += new HashMap[String, FameDecoupledIO[Bits]]()
    DebugIOs += new HashMap[String, Data]()
  }

  for(i <- 0 until num_copies){
    val originalModule = originalModules(i)
    var decoupled_counter = 0
    var reg_counter = 0
    var debug_counter = 0 
    //populate fame1RegIO and fame1DecoupledIO bundles with the elements from the original RegIO and DecoupleIOs
    for ((name, ioNode) <- originalModule.io.asInstanceOf[Bundle].elements) {
      ioNode match {
        case decoupled : DecoupledIO[_] => {
          val is_flip = (decoupled.ready.dir == OUTPUT)
          val fame1Decoupled = io.queues(num_decoupled_io*i + decoupled_counter)
          if (is_flip) {
            fame1Decoupled.flip()
            fame1Decoupled.target.ready := decoupled.ready
            decoupled.valid := fame1Decoupled.target.valid
            val decoupledBitsClone = decoupled.bits.clone()
            decoupled.bits := decoupledBitsClone.fromBits(fame1Decoupled.target.bits)
          } else {
            decoupled.ready := fame1Decoupled.target.ready
            fame1Decoupled.target.bits := decoupled.bits.toBits
            fame1Decoupled.target.valid := decoupled.valid 
          }
          DecoupledIOs(i)(name) = fame1Decoupled
          decoupled_counter += 1
        }
        case reg : RegIO[_] => {
          val is_flip = (reg.bits.flatten(0)._2.dir == INPUT)
          val fame1RegIO = io.regs(num_reg_io*i + reg_counter)
          if (is_flip) {
            fame1RegIO.flip()
            val regBitsClone = reg.bits.clone()
            reg.bits := regBitsClone.fromBits(fame1RegIO.bits)
          } else {
            fame1RegIO.bits := reg.bits.toBits
          }
          RegIOs(i)(name) = fame1RegIO
          reg_counter += 1
        }
        case _ => {
          if (name != "is_fire") {
            Predef.assert(ioNode.isInstanceOf[Bits])
            if(ioNode.toBits.dir == INPUT){
              io.debug(debug_counter).asInput
              ioNode := io.debug(debug_counter)
            } else {
              io.debug(debug_counter).asOutput
              io.debug(debug_counter) := ioNode.toBits
            }
            DebugIOs(i)(name) = io.debug(num_debug_io*i + debug_counter)
            debug_counter += 1
          }
        }
      }
    }

    //generate fire_tgt_clk signal
    var fire_tgt_clk = Bool(true)
    for (queue <- DecoupledIOs(i).values){
      fire_tgt_clk = fire_tgt_clk && (if (queue.host_valid.dir == OUTPUT) queue.host_ready else queue.host_valid)
    }
    for (reg <- RegIOs(i).values) {
      fire_tgt_clk = fire_tgt_clk && (if (reg.valid.dir == OUTPUT) reg.ready else reg.valid)
    }
    
    //generate host read and host valid signals
    Fame5Transform.fireSignals(originalModule) := fire_tgt_clk
    for (queue <- DecoupledIOs(i).values) {
      if (queue.host_valid.dir == OUTPUT){ 
        queue.host_valid := fire_tgt_clk
      } else {
        queue.host_ready := fire_tgt_clk
      }
    }
    for (reg <- RegIOs(i).values) {
      if (reg.valid.dir == OUTPUT) {
        reg.valid := fire_tgt_clk
      } else {
        reg.ready := fire_tgt_clk
      }
    }
  }
}

object Fame5Transform {
  val fame1Modules = new HashSet[Module]
  val fireSignals = new HashMap[Module, Bool]
}

trait Fame5Transform extends Backend {
  private def collectMems(module: Module): ArrayBuffer[(Module, Mem[Data])] = {
    val mems = new ArrayBuffer[(Module, Mem[Data])]
    //find all the mems in FAME1 modules
    def findMems(module: Module): Unit = {
      if(Fame5Transform.fame1Modules.contains(module)){
        for(mem <- module.nodes.filter(_.isInstanceOf[Mem[Data]])){
          mems += ((module, mem.asInstanceOf[Mem[Data]]))
        }
      }
      for(childModule <- module.children){
        findMems(childModule)
      }
    }
    findMems(module)
    return mems
  }
  
  private def appendFireToRegWriteEnables(top: Module) = {
    //find regs that are part of sequential mem read ports
    val mems = collectMems(top)
    val seqMemReadRegs = new HashSet[Reg]
    for((module, mem) <- mems){
      val memSeqReads = mem.seqreads ++ mem.readwrites.map(_.read)
      for(memSeqRead <- memSeqReads){
        seqMemReadRegs += memSeqRead.addrReg
      }
    }

    //find all the registers in FAME1 modules
    val regs = new ArrayBuffer[(Module, Reg)]
    def findRegs(module: Module): Unit = {
      if(Fame5Transform.fame1Modules.contains(module)){
        for(reg <- module.nodes.filter(_.isInstanceOf[Reg])){
          if(!seqMemReadRegs.contains(reg.asInstanceOf[Reg])){
            regs += ((module, reg.asInstanceOf[Reg]))
          }
        }
      }
      for(childModule <- module.children){
        findRegs(childModule)
      }
    }
    findRegs(top)
    
    
    for((module, reg) <- regs){
      reg.enable = reg.enable && Fame5Transform.fireSignals(module)
      if(reg.updates.length == 0){
        val regOutput = Bits()
        regOutput.inputs += reg
        val regMux = Bits()
        regMux.inputs += reg.inputs(0)
        reg.inputs(0) = Mux(Fame5Transform.fireSignals(module), regMux, regOutput)
      } else {
        for(i <- 0 until reg.updates.length){
          val wEn = reg.updates(i)._1
          val wData = reg.updates(i)._2
          reg.updates(i) = ((wEn && Fame5Transform.fireSignals(module), wData))
        }
      }
    }
  }
 
  private def appendFireToMemEnables(top: Module) = {
    val mems = collectMems(top)

    for((module, mem) <- mems){
      val memWrites = mem.writes ++ mem.readwrites.map(_.write)
      val memSeqReads = mem.seqreads ++ mem.readwrites.map(_.read)
      for(memWrite <- memWrites){
        if(mem.seqRead){
          if(Module.backend.isInstanceOf[CppBackend]){
            if(memWrite.inputs(0).asInstanceOf[Data].comp != null && memWrite.inputs(1).asInstanceOf[Data].comp != null){//huge hack for extra MemWrite generated for seqread mems in CPP backed; if both the cond and enable both happen to be directly from registers, this will fail horribly
              memWrite.inputs(1) = memWrite.inputs(1).asInstanceOf[Bool] && Fame5Transform.fireSignals(module)
            } else {
              memWrite.inputs(1) = Bool(false)
            }
          } else {
            memWrite.inputs(1) = memWrite.inputs(1).asInstanceOf[Bool] && Fame5Transform.fireSignals(module)
          }
        } else {
          memWrite.inputs(1) = memWrite.inputs(1).asInstanceOf[Bool] && Fame5Transform.fireSignals(module)
        }
      }
      for(memSeqRead <- memSeqReads){
        Predef.assert(memSeqRead.addrReg.updates.length == 1)
        val oldReadAddr = Bits()
        oldReadAddr.inputs += memSeqRead.addrReg.updates(0)._2
        val oldReadAddrReg = Reg(Bits())
        oldReadAddrReg.comp.component = module
        oldReadAddrReg.comp.asInstanceOf[Reg].enable = if (oldReadAddrReg.comp.asInstanceOf[Reg].isEnable) oldReadAddrReg.comp.asInstanceOf[Reg].enable || Fame5Transform.fireSignals(module) else Fame5Transform.fireSignals(module)
        oldReadAddrReg.comp.asInstanceOf[Reg].isEnable = true
        oldReadAddrReg.comp.asInstanceOf[Reg].updates += ((Fame5Transform.fireSignals(module), oldReadAddr))
        
        val newReadAddr = Mux(Fame5Transform.fireSignals(module), oldReadAddr, oldReadAddrReg)
        
        val oldReadEn = Bool()
        oldReadEn.inputs += memSeqRead.addrReg.updates(0)._1
        val renReg = Reg(init=Bool(false))
        renReg.comp.component = module
        
        renReg.comp.asInstanceOf[Reg].enable = if(renReg.comp.asInstanceOf[Reg].isEnable) renReg.comp.asInstanceOf[Reg].enable || Fame5Transform.fireSignals(module) else Fame5Transform.fireSignals(module)
        renReg.comp.asInstanceOf[Reg].isEnable = true
        renReg.comp.asInstanceOf[Reg].updates += ((Fame5Transform.fireSignals(module), oldReadEn))
        val newRen = Mux(Fame5Transform.fireSignals(module), oldReadEn, renReg)
        
        memSeqRead.addrReg.enable = newRen
        memSeqRead.addrReg.updates.clear
        memSeqRead.addrReg.updates += ((newRen, newReadAddr))
      }
    }
  }
  
  
  preElaborateTransforms += ((top: Module) => collectNodesIntoComp(initializeDFS))
  preElaborateTransforms += ((top: Module) => appendFireToRegWriteEnables(top))
  preElaborateTransforms += ((top: Module) => top.genAllMuxes)
  preElaborateTransforms += ((top: Module) => appendFireToMemEnables(top))
  preElaborateTransforms += ((top: Module) => collectNodesIntoComp(initializeDFS))
  preElaborateTransforms += ((top: Module) => top.genAllMuxes)
}

class Fame5CppBackend extends CppBackend with Fame5Transform
class Fame5VerilogBackend extends VerilogBackend with Fame5Transform
class Fame5FPGABackend extends FPGABackend with Fame5Transform
*/

class Fame5WrapperIO(num_copies: Int, num_queues: Int, num_regs: Int, num_debug: Int) extends Bundle {
  var queues:Vec[FameDecoupledIO[Bits]] = null
  if(num_queues > 0) {
    queues = Vec.fill(num_copies*num_queues){ new FameDecoupledIO(Bits())}
  }
  var regs:Vec[DecoupledIO[Bits]] = null
  if(num_regs > 0) {
    regs = Vec.fill(num_copies*num_regs){ new DecoupledIO(Bits())}
  }
  var debug:Vec[Bits] = null
  if(num_debug > 0) {
    debug = Vec.fill(num_copies*num_debug){Bits(OUTPUT)}
  }
}

class Fame5Wrapper(num_copies: Int, f: => Module) extends Module {
  def markFame5Modules(module: Module): Unit = {
    if(!module.isInstanceOf[TransactionMem[_]]){
      Fame5Transform.fame5Modules += module
      for(submodule <- module.children){
        markFame5Modules(submodule)
      }
    }
  }
  
  def replicateIO(): Unit = {
    for ((name, io) <- originalModule.io.asInstanceOf[Bundle].elements){ 
      io match { 
        case queue : DecoupledIO[_] => {
          val queueAsBits = queue.asInstanceOf[DecoupledIO[Bits]]
          Fame5Transform.DecoupledIOs(name) = new ArrayBuffer[DecoupledIO[Bits]]
          Fame5Transform.DecoupledIOs(name) += queueAsBits
          for(i <- 1 until num_copies){
            val ioCopy = new DecoupledIO(queueAsBits.bits.asInstanceOf[Bits])
            if(queueAsBits.ready.dir == OUTPUT){
              ioCopy.flip
            }
            ioCopy.valid.isIo = true
            ioCopy.valid.setName("io_" + name + "_valid_" + i)
            ioCopy.valid.component = originalModule
            ioCopy.ready.isIo = true
            ioCopy.ready.setName("io_" + name + "_ready_" + i)
            ioCopy.ready.component = originalModule
            ioCopy.bits.isIo = true
            ioCopy.bits.setName("io_" + name + "_bits_" + i)
            ioCopy.bits.component = originalModule
            ioCopy.isIo = true
            ioCopy.setName(name + "_" + i)
            ioCopy.component = originalModule
            originalModule.io.asInstanceOf[Bundle] += ioCopy
            Fame5Transform.DecoupledIOs(name) += ioCopy
          }
        }
        case reg : RegIO[_] => {
          val regAsBits = reg.asInstanceOf[RegIO[Bits]]
          Fame5Transform.RegIOs(name) = new ArrayBuffer[RegIO[Bits]]
          Fame5Transform.RegIOs(name) += regAsBits
          for(i <- 1 until num_copies){
            val ioCopy = new RegIO(regAsBits.bits.asInstanceOf[Bits])
            if(regAsBits.bits.dir == INPUT){
              ioCopy.flip
            }
            ioCopy.bits.isIo = true
            ioCopy.bits.setName("io_" + name + "_bits_" + i)
            ioCopy.bits.component = originalModule
            ioCopy.isIo = true
            ioCopy.setName(name + "_" + i)
            ioCopy.component = originalModule
            originalModule.io.asInstanceOf[Bundle] += ioCopy
            Fame5Transform.RegIOs(name) += ioCopy
          }
        }
        case _ => {
          val ioAsBits = io.asInstanceOf[Bits]
          Fame5Transform.DebugIOs(name) = new ArrayBuffer[Bits]
          Fame5Transform.DebugIOs(name) += ioAsBits
          for(i <- 1 until num_copies){
            val ioCopy = ioAsBits.clone
            Predef.assert(ioCopy.inputs.size == 0)
            Predef.assert(ioCopy.updates.size == 0)
            ioCopy.dir = ioAsBits.dir
            ioCopy.isIo = true
            ioCopy.setName(name + "_" + i)
            ioCopy.component = originalModule
            originalModule.io.asInstanceOf[Bundle] += ioCopy
            Fame5Transform.DebugIOs(name) += ioCopy
          }
        }
      }
    } 
  }
  
  def addTestToIO(isTop: Boolean, module: Module, parent: Module): Unit = {
    val threadSelID = UInt(INPUT, width = log2Up(num_copies))
    threadSelID.isIo = true
    threadSelID.setName("test")
    threadSelID.component = module
    Fame5Transform.testSignals(module) = threadSelID
    if(!isTop){
      threadSelID := Fame5Transform.testSignals(parent)
    }
    module.io.asInstanceOf[Bundle] += threadSelID
    for(submodule <- module.children){
      addTestToIO(false, submodule, module)
    }
  }

  def addThreadReadyToIO(isTop: Boolean, module: Module, parent: Module): Unit = {
    Fame5Transform.threadReadySignals(module) = new ArrayBuffer[Bool]
    for(i <- 0 until num_copies){
      val threadReady = Bool(INPUT)
      threadReady.isIo = true
      threadReady.setName("thread_ready" + "_" + i)
      threadReady.component = module
      Fame5Transform.threadReadySignals(module) += threadReady
      if(!isTop){
        Predef.assert(Fame5Transform.threadReadySignals(parent)(i) != null)
        threadReady := Fame5Transform.threadReadySignals(parent)(i)
      }
      module.io.asInstanceOf[Bundle] += threadReady
    }
    for(submodule <- module.children){
      addThreadReadyToIO(false, submodule, module)
    }
  }
  
  def addThreadSelIDToIO(isTop: Boolean, module: Module, parent: Module): Unit = {
    val threadSelID = UInt(INPUT, width = log2Up(num_copies))
    threadSelID.isIo = true
    threadSelID.setName("thread_sel_id")
    threadSelID.component = module
    Fame5Transform.threadSelIDSignals(module) = threadSelID
    if(!isTop){
      Predef.assert(Fame5Transform.threadSelIDSignals(parent) != null)
      threadSelID := Fame5Transform.threadSelIDSignals(parent)
    }
    module.io.asInstanceOf[Bundle] += threadSelID
    for(submodule <- module.children){
      addThreadSelIDToIO(false, submodule, module)
    }
  }
  
  def connectWrapperTargetIOs(): Unit = {
    var decoupled_counter = 0
    var reg_counter = 0
    var debug_counter = 0
    for((name, decoupledIOs) <- Fame5Transform.DecoupledIOs){
      Predef.assert(decoupledIOs.length == num_copies)
      for(i <- 0 until decoupledIOs.length){
        val decoupled = decoupledIOs(i)
        val is_flip = (decoupled.ready.dir == OUTPUT)
        val fame1Decoupled = io.queues(decoupled_counter)
        if (is_flip) {
          fame1Decoupled.flip()
          fame1Decoupled.target.ready := decoupled.ready
          decoupled.valid.inputs += fame1Decoupled.target.valid
          decoupled.bits.asInstanceOf[Bits].inputs += fame1Decoupled.target.bits
        } else {
          decoupled.ready.inputs += fame1Decoupled.target.ready
          fame1Decoupled.target.bits := decoupled.bits.toBits
          fame1Decoupled.target.valid := decoupled.valid 
        }
        DecoupledIOs(i)(name) = fame1Decoupled
        decoupled_counter += 1
      }
    }
    for((name, regIOs) <- Fame5Transform.RegIOs){
      Predef.assert(regIOs.length == num_copies)
      for(i <- 0 until regIOs.length){
        val reg = regIOs(i)
        val is_flip = (reg.bits.flatten(0)._2.dir == INPUT)
        val fame1RegIO = io.regs(reg_counter)
        if (is_flip) {
          fame1RegIO.flip()
          reg.bits.asInstanceOf[Bits].inputs += fame1RegIO.bits
        } else {
          fame1RegIO.bits := reg.bits.asInstanceOf[Bits]
        }
        RegIOs(i)(name) = fame1RegIO
        reg_counter += 1
      }
    }
    for((name, debugIOs) <- Fame5Transform.DebugIOs){
      Predef.assert(debugIOs.length == num_copies)
      for(i <- 0 until debugIOs.length){
        val debug = debugIOs(i)
        Predef.assert(debug.isInstanceOf[Bits])
        if(debug.toBits.dir == INPUT){
          io.debug(debug_counter).asInput
          debug := io.debug(debug_counter)
        } else {
          io.debug(debug_counter).asOutput
          io.debug(debug_counter) := debug.toBits
        }
        DebugIOs(i)(name) = io.debug(debug_counter)
        debug_counter += 1
      }
    }
  }

  def generateThreadReadySignals() = {
    for(i <- 0 until num_copies){
      var threadReadySignal = Bool(true)
      for (queue <- DecoupledIOs(i).values){
        threadReadySignal = threadReadySignal && (if (queue.host_valid.dir == OUTPUT) queue.host_ready else queue.host_valid)
      }
      for (reg <- RegIOs(i).values) {
        threadReadySignal = threadReadySignal && (if (reg.valid.dir == OUTPUT) reg.ready else reg.valid)
      }
      Fame5Transform.threadReadySignals(originalModule)(i) := threadReadySignal
    }
  }

  def generateThreadSelIDSignal() = {
    val counter = Reg(init = UInt(0, width = log2Up(num_copies)))
    counter := counter + UInt(1)
    when(counter === UInt(num_copies - 1)){
      counter := UInt(0)
    }
    val threadSelID = UInt()
    threadSelID := counter
    Fame5Transform.threadSelIDSignals(originalModule) := threadSelID
    Fame5Transform.testSignals(originalModule) := UInt(0, width = 4)
  }

  def connectWrapperHostIOs() = {
    for(i <- 0 until num_copies){
      for (queue <- DecoupledIOs(i).values) {
        if (queue.host_valid.dir == OUTPUT){ 
          queue.host_valid := Fame5Transform.threadReadySignals(originalModule)(i) && (UInt(i) === Fame5Transform.threadSelIDSignals(originalModule))
        } else {
          queue.host_ready := Fame5Transform.threadReadySignals(originalModule)(i) && (UInt(i) === Fame5Transform.threadSelIDSignals(originalModule))
        }
      }
      for (reg <- RegIOs(i).values) {
        if (reg.valid.dir == OUTPUT) {
          reg.valid := Fame5Transform.threadReadySignals(originalModule)(i) && (UInt(i) === Fame5Transform.threadSelIDSignals(originalModule))
        } else {
          reg.ready := Fame5Transform.threadReadySignals(originalModule)(i) && (UInt(i) === Fame5Transform.threadSelIDSignals(originalModule))
        }
      }
    }
  }

  val originalModule = Module(f)
  Fame5Transform.topModule = originalModule
  Fame5Transform.numCopies = num_copies
  //count number of RegIO and DecoupledIO in original module
  var num_decoupled_io = 0
  var num_reg_io = 0
  var num_debug_io = 0
  for ((name, io) <- originalModule.io.asInstanceOf[Bundle].elements){ 
    io match { 
      case q : DecoupledIO[_] => num_decoupled_io += 1; 
      case r : RegIO[_] => num_reg_io += 1;
      case _ => num_debug_io += 1;
    }
  }

  val io = new Fame5WrapperIO(num_copies, num_decoupled_io, num_reg_io, num_debug_io)
  
  val RegIOs = new ArrayBuffer[HashMap[String, DecoupledIO[Bits]]]()
  val DecoupledIOs  = new ArrayBuffer[HashMap[String, FameDecoupledIO[Bits]]]()
  val DebugIOs = new ArrayBuffer[HashMap[String, Data]]()

  for(i <- 0 until num_copies){
    RegIOs += new HashMap[String, DecoupledIO[Bits]]()
    DecoupledIOs  += new HashMap[String, FameDecoupledIO[Bits]]()
    DebugIOs += new HashMap[String, Data]()
  }
  
  markFame5Modules(originalModule)
  replicateIO()//replicateIO must be called before addThreadReadyToIO and addThreadSelToIO because we don't want the added threadReady and threadSelID signals to be replecated
  addTestToIO(true, originalModule, null)
  connectWrapperTargetIOs()
  addThreadReadyToIO(true, originalModule, null)
  addThreadSelIDToIO(true, originalModule, null)
  generateThreadReadySignals()
  generateThreadSelIDSignal()
  connectWrapperHostIOs()
}

object Fame5Transform {
  val fame5Modules = new HashSet[Module]
  var numCopies: Int = 0
  var topModule: Module = null
  val testSignals = new HashMap[Module, UInt]
  val threadReadySignals = new HashMap[Module, ArrayBuffer[Bool]]
  val threadSelIDSignals = new HashMap[Module, UInt]
  val DecoupledIOs = new HashMap[String, ArrayBuffer[DecoupledIO[Bits]]]
  val RegIOs = new HashMap[String, ArrayBuffer[RegIO[Bits]]]
  val DebugIOs = new HashMap[String, ArrayBuffer[Bits]]
  var consumerMap = new HashMap[Node, ArrayBuffer[(Node, Int)]]
  val regCopiesMap = new HashMap[Reg, ArrayBuffer[Reg]]//hash map of original fame0 register to a list of all of its copies,including itself
  val regTypeNodesMap = new HashMap[Reg, Bits]
  val memCopiesMap = new HashMap[TransactionMem[Data], ArrayBuffer[TransactionMem[Data]]]
}

trait Fame5Transform extends Backend {
  private def findConsumerMap(module: Module) = {
    Fame5Transform.consumerMap = new HashMap[Node, ArrayBuffer[(Node, Int)]]
    val allNodes = new ArrayBuffer[Node]
    def findAllNodes(module: Module):Unit = {
      for(node <- module.nodes){
        allNodes += node
      }
      for(child <- module.children){
        findAllNodes(child)
      }
    }
    findAllNodes(module)

    for(node <- allNodes){
      Fame5Transform.consumerMap(node) = new ArrayBuffer[(Node, Int)]
    }
    for(node <- allNodes){
      if(node.isInstanceOf[Data]){//huge hack should move this out to its own method
        if(node.asInstanceOf[Data].comp != null){
          if(node.asInstanceOf[Data].comp.isInstanceOf[Reg]){
            Fame5Transform.regTypeNodesMap(node.asInstanceOf[Data].comp.asInstanceOf[Reg]) = node.asInstanceOf[Bits]
          }
        }
      }
      for(i <- 0 until node.inputs.length){
        val nodeInput = node.inputs(i)
        if(Fame5Transform.consumerMap.contains(nodeInput)){
          Fame5Transform.consumerMap(nodeInput) += ((node, i))
        }
      }
    }
  }

  private def driveOutputs(): Unit = {
    for((name, decoupledIOs) <- Fame5Transform.DecoupledIOs){
      for(i <- 1 until decoupledIOs.length){
        val decoupled = decoupledIOs(i)
        val is_flip = (decoupled.ready.dir == OUTPUT)
        if (is_flip) {
          Predef.assert(decoupledIOs(0).ready.inputs.length == 1)
          decoupled.ready.inputs += decoupledIOs(0).ready.inputs(0)
        } else {
          Predef.assert(decoupledIOs(0).valid.inputs.length == 1)
          decoupled.valid.inputs += decoupledIOs(0).valid.inputs(0)
          Predef.assert(decoupledIOs(0).bits.isInstanceOf[Bits])
          Predef.assert(decoupledIOs(0).bits.asInstanceOf[Bits].inputs.length == 1)
          decoupled.bits.asInstanceOf[Bits].inputs += decoupledIOs(0).bits.asInstanceOf[Bits].inputs(0)
        }
      }
    }
    for((name, regIOs) <- Fame5Transform.RegIOs){
      for(i <- 1 until regIOs.length){
        val reg = regIOs(i)
        if(reg.bits.flatten(0)._2.dir == OUTPUT){
          Predef.assert(regIOs(0).bits.isInstanceOf[Bits])
          Predef.assert(regIOs(0).bits.asInstanceOf[Bits].inputs.length == 1)
          reg.bits.asInstanceOf[Bits].inputs += regIOs(0).bits.asInstanceOf[Bits].inputs(0)
        }
      }
    }
    for((name, debugIOs) <- Fame5Transform.DebugIOs){
      for(i <- 1 until debugIOs.length){
        val debug = debugIOs(i)
        if(debug.toBits.dir == OUTPUT){
          Predef.assert(debugIOs(0).inputs.length == 1)
          debug.inputs += debugIOs(0).inputs(0)
        }
      }
    }
  }

  private def muxInputs(): Unit = {
    for((name, decoupledIOs) <- Fame5Transform.DecoupledIOs){
      val originalDecoupledIO = decoupledIOs(0)
      if(originalDecoupledIO.valid.dir == INPUT){
        val validCopies = new ArrayBuffer[Bits]
        for(decoupledIO <- decoupledIOs){
          validCopies += decoupledIO.valid
        }
        insertMuxOnConsumers(originalDecoupledIO.valid, validCopies, Fame5Transform.threadSelIDSignals(Fame5Transform.topModule))
        val dataCopies = new ArrayBuffer[Bits]
        for(decoupledIO <- decoupledIOs){
          dataCopies += decoupledIO.bits.asInstanceOf[Bits]
        }
        insertMuxOnConsumers(originalDecoupledIO.bits.asInstanceOf[Bits], dataCopies, Fame5Transform.threadSelIDSignals(Fame5Transform.topModule))
      } else {
        val readyCopies = new ArrayBuffer[Bits]
        for(decoupledIO <- decoupledIOs){
          readyCopies += decoupledIO.ready
        }
        insertMuxOnConsumers(originalDecoupledIO.ready, readyCopies, Fame5Transform.threadSelIDSignals(Fame5Transform.topModule))
      }
    }
    for((name, regIOs) <- Fame5Transform.RegIOs){
      val originalRegIO = regIOs(0)
      if(originalRegIO.bits.asInstanceOf[Bits].dir == INPUT){
        val copies = new ArrayBuffer[Bits]
        for (regIO <- regIOs){
          copies += regIO.bits.asInstanceOf[Bits]
        }
        insertMuxOnConsumers(originalRegIO.bits.asInstanceOf[Bits], copies, Fame5Transform.threadSelIDSignals(Fame5Transform.topModule) + Fame5Transform.testSignals(Fame5Transform.topModule))
      }
    }
    for((name, debugIOs) <- Fame5Transform.DebugIOs){
      val originalDebug = debugIOs(0)
      if(originalDebug.toBits.dir == INPUT){
        val copies = new ArrayBuffer[Bits]
        for(debugIO <- debugIOs){
          copies += debugIO.asInstanceOf[Bits]
        }
        insertMuxOnConsumers(originalDebug, copies, Fame5Transform.threadSelIDSignals(Fame5Transform.topModule))
      }
    }
  }

  private def insertMuxOnConsumers(node: Bits, copies: ArrayBuffer[Bits], threadSelId: UInt): Unit = {
    val muxMapping = new ArrayBuffer[(Bool, Bits)]
    for(i <- 0 until copies.length){
      muxMapping += ((threadSelId === UInt(i), copies(i)))
    }
    //using MuxCase here is a hack, it is much more effiient to directly use the threadSelId as the signal to a large n-way mux
    val mux = MuxCase(node, muxMapping)
    for((consumer, inputNum) <- Fame5Transform.consumerMap(node)){
      consumer.inputs(inputNum) = mux
    }
  }
 
  private def collectMems(module: Module): ArrayBuffer[(Module, Mem[Data])] = {
    val mems = new ArrayBuffer[(Module, Mem[Data])]
    //find all the mems in FAME1 modules
    def findMems(module: Module): Unit = {
      if(Fame5Transform.fame5Modules.contains(module)){
        for(mem <- module.nodes.filter(_.isInstanceOf[Mem[Data]])){
          mems += ((module, mem.asInstanceOf[Mem[Data]]))
        }
      }
      for(childModule <- module.children){
        findMems(childModule)
      }
    }
    findMems(module)
    return mems
  }
  
  private def replicateRegisters(): Unit = {
    /*val mems = collectMems(Fame5Transform.topModule)  
    val seqMemReadRegs = new HashSet[Reg]
    val seqMemCPPWriteRegs = new HashSet[Reg]
    //find extra regs associated with mems that we do not want to replicate
    for((module, mem) <- mems){
      val memSeqReads = mem.seqreads ++ mem.readwrites.map(_.read)
      for(memSeqRead <- memSeqReads){
        seqMemReadRegs += memSeqRead.addrReg
      }
      if(mem.seqRead){
        if(Module.backend.isInstanceOf[CppBackend]){
          val memWrites = mem.writeAccesses
          for(memWrite <- memWrites){
            if(memWrite.inputs(0).asInstanceOf[Data].comp != null && memWrite.inputs(1).asInstanceOf[Data].comp != null){//huge hack for extra registers generated infront f mem write addr, write enable and write data ports for seq read mems in the CPP backend; if both the cond and enable both happen to be directly from registers, this will fail horribly
              seqMemCPPWriteRegs += memWrite.inputs(0).asInstanceOf[Data].comp.asInstanceOf[Reg]
              seqMemCPPWriteRegs += memWrite.inputs(1).asInstanceOf[Data].comp.asInstanceOf[Reg]
              Predef.assert(memWrite.inputs(2).inputs.length == 1)
              seqMemCPPWriteRegs += memWrite.inputs(2).inputs(0).asInstanceOf[Reg]
            }
          }
        }
      }
    }*/

    //find all the registers in FAME5 modules
    val regs = new ArrayBuffer[(Module, Reg)]
    def findRegs(module: Module): Unit = {
      if(Fame5Transform.fame5Modules.contains(module)){
        for(reg <- module.nodes.filter(_.isInstanceOf[Reg])){
          //if(!seqMemReadRegs.contains(reg.asInstanceOf[Reg])){
            regs += ((module, reg.asInstanceOf[Reg]))
          //}
        }
      }
      for(childModule <- module.children){
        findRegs(childModule)
      }
    }
    findRegs(Fame5Transform.topModule)
    
    //replicate registers
    for((module, reg) <- regs){
      Fame5Transform.regCopiesMap(reg) = new ArrayBuffer[Reg]
      Fame5Transform.regCopiesMap(reg) += reg
      for(i <- 1 until Fame5Transform.numCopies){
        val regCopy = copyReg(reg)
        regCopy.setName(reg.name + "_" + i) 
        Fame5Transform.regCopiesMap(reg) += regCopy
      }
    }
  }
  
  private def copyReg(reg: Reg): Reg = {
    val regCopy = new Reg()
    //regCopy.inputs += reg.inputs(0)
    regCopy.inputs += reg.inputs(1)
    regCopy.enableIndex = reg.enableIndex
    regCopy.isReset = reg.isReset
    regCopy.isEnable = reg.isEnable
    regCopy.assigned = reg.assigned
    regCopy.enable = reg.enable
    regCopy.inputs += regCopy.enable
    for((en, data) <- reg.updates){
      regCopy.updates += ((en,data))
    }
    regCopy.genMuxes(regCopy)
    return regCopy
  }

  private def muxRegOutputs():Unit = {
    for(reg <- Fame5Transform.regCopiesMap.keys){
      val regsAsBits = new ArrayBuffer[Bits]
      for(register <- Fame5Transform.regCopiesMap(reg)){
        val regOutput = Bits()
        regOutput.inputs += register
        regsAsBits += regOutput
      }
      insertMuxOnConsumers(Fame5Transform.regTypeNodesMap(reg), regsAsBits, Fame5Transform.threadSelIDSignals(reg.component)) 
    }
  }

  private def appendFireToRegWriteEnables() = {
    for(reg <- Fame5Transform.regCopiesMap.keys){
      for(i <- 0 until Fame5Transform.regCopiesMap(reg).length){
        val register = Fame5Transform.regCopiesMap(reg)(i)
        val fameEnable = Fame5Transform.threadReadySignals(reg.component)(i) && (UInt(i) === Fame5Transform.threadSelIDSignals(reg.component))
        register.enable = register.enable && fameEnable
        val registerOutput = Bits()
        registerOutput.inputs += register
        val registerMux = Bits()
        registerMux.inputs += register.inputs(0)
        register.inputs(0) = Mux(fameEnable, registerMux, registerOutput)
        
        for(i <- 0 until register.updates.length){
          val wEn = register.updates(i)._1
          val wData = register.updates(i)._2
          register.updates(i) = ((wEn && fameEnable, wData))
        }
      }
    }
  }

  private def replicateTransactionMems(): Unit = {
    val mems = new ArrayBuffer[TransactionMem[Data]]
    for(module <- Fame5Transform.fame5Modules){
      for(child <- module.children){
        if(child.isInstanceOf[TransactionMem[_]]){
          mems += child.asInstanceOf[TransactionMem[Data]]
        }
      }
    }
    for(mem <- mems){
      Fame5Transform.memCopiesMap(mem) = new ArrayBuffer[TransactionMem[Data]]
      Fame5Transform.memCopiesMap(mem) += mem.asInstanceOf[TransactionMem[Data]]
      for(i <- 1 until Fame5Transform.numCopies){
        val memCopy = copyMem(mem)
        memCopy.name = mem.name + "_" + i
        Fame5Transform.memCopiesMap(mem) += memCopy
      }
    }
  }
  
  private def copyMem(mem: TransactionMem[Data]): TransactionMem[Data] = {
    /*Module.trigger = true
    val memCopy = new TransactionMem(mem.numMemLines, mem.readPortNum, mem.virtWritePortNum, mem.phyWritePortNum, mem.writeMap, mem.isSeqRead)(mem.dataType.clone)
    memCopy.parent = mem.parent
    mem.parent.children += memCopy
    memCopy.level = mem.level
    for((name, ioWire) <- memCopy.wires){
      ioWire.isIo = true
    }*/
    Module.compStack.clear()
    Module.compStack.push(mem.parent)
    val memCopy = Module(new TransactionMem(mem.numMemLines, mem.readPortNum, mem.virtWritePortNum, mem.phyWritePortNum, mem.writeMap, mem.isSeqRead)(mem.dataType.clone))

    memCopy.genAllMuxes
    /*for(i <- 0 until mem.readPortNum){
      Predef.assert(mem.io.reads(i).is.inputs.length == 1)
      memCopy.io.reads(i).is.inputs += mem.io.reads(i).is.inputs(0)
      Predef.assert(mem.io.reads(i).adr.inputs.length == 1)
      memCopy.io.reads(i).adr.inputs += mem.io.reads(i).adr.inputs(0)
    }
    for(i <- 0 until mem.virtWritePortNum){
      Predef.assert(mem.io.writes(i).is.inputs.length == 1)
      memCopy.io.writes(i).is.inputs += mem.io.writes(i).is.inputs(0)
      Predef.assert(mem.io.writes(i).adr.inputs.length == 1)
      memCopy.io.writes(i).adr.inputs += mem.io.writes(i).adr.inputs(0)
      Predef.assert(mem.io.writes(i).dat.inputs.length == 1)
      memCopy.io.writes(i).dat.inputs += mem.io.writes(i).dat.inputs(0)
    }*/
    return memCopy.asInstanceOf[TransactionMem[Data]]
  }

  private def muxTransactionMemOutputs(): Unit = {
    for(originalMem <- Fame5Transform.memCopiesMap.keys){
      for(i <- 0 until originalMem.readPortNum){
        val readDatas = new ArrayBuffer[Bits]
        for(mem <- Fame5Transform.memCopiesMap(originalMem)){
          readDatas += mem.io.reads(i).dat.asInstanceOf[Bits]
        }
        insertMuxOnConsumers(originalMem.io.reads(i).dat.asInstanceOf[Bits], readDatas, Fame5Transform.threadSelIDSignals(originalMem.parent))
      }
    }
  }

  private def appendFireToTransactionMemEnables(): Unit = {
    for(originalMem <- Fame5Transform.memCopiesMap.keys){
      for(i <- 0 until Fame5Transform.memCopiesMap(originalMem).length){
        val mem = Fame5Transform.memCopiesMap(originalMem)(i)
        val fameEnable = Fame5Transform.threadReadySignals(originalMem.parent)(i) && (UInt(i) === Fame5Transform.threadSelIDSignals(originalMem.parent))
        for(j <- 0 until mem.virtWritePortNum){
          Predef.assert(mem.io.writes(j).is.inputs.length == 1)
          mem.io.writes(j).is.inputs(0) = mem.io.writes(j).is.inputs(0).asInstanceOf[Bool] && fameEnable
        }
        if(originalMem.isSeqRead){ 
          for(j <- 0 until mem.readPortNum){
            
          }
        }
      }
    }
  }
  /*private def appendFireToMemEnables(top: Module) = {
    val mems = collectMems(top)

    for((module, mem) <- mems){
      val memWrites = mem.writes ++ mem.readwrites.map(_.write)
      val memSeqReads = mem.seqreads ++ mem.readwrites.map(_.read)
      for(memWrite <- memWrites){
        if(mem.seqRead){
          if(Module.backend.isInstanceOf[CppBackend]){
            if(memWrite.inputs(0).asInstanceOf[Data].comp != null && memWrite.inputs(1).asInstanceOf[Data].comp != null){//huge hack for extra MemWrite generated to simulate bogus data if read and write is performed on the same cycle for seqread mems in CPP backed; if both the cond and enable both happen to be directly from registers, this will fail horribly
              memWrite.inputs(1) = memWrite.inputs(1).asInstanceOf[Bool] && Fame5Transform.fireSignals(module)
            } else {
              memWrite.inputs(1) = Bool(false)
            }
          } else {
            memWrite.inputs(1) = memWrite.inputs(1).asInstanceOf[Bool] && Fame5Transform.fireSignals(module)
          }
        } else {
          memWrite.inputs(1) = memWrite.inputs(1).asInstanceOf[Bool] && Fame5Transform.fireSignals(module)
        }
      }
      for(memSeqRead <- memSeqReads){
        Predef.assert(memSeqRead.addrReg.updates.length == 1)
        val oldReadAddr = Bits()
        oldReadAddr.inputs += memSeqRead.addrReg.updates(0)._2
        val oldReadAddrReg = Reg(Bits())
        oldReadAddrReg.comp.component = module
        oldReadAddrReg.comp.asInstanceOf[Reg].enable = if (oldReadAddrReg.comp.asInstanceOf[Reg].isEnable) oldReadAddrReg.comp.asInstanceOf[Reg].enable || Fame5Transform.fireSignals(module) else Fame5Transform.fireSignals(module)
        oldReadAddrReg.comp.asInstanceOf[Reg].isEnable = true
        oldReadAddrReg.comp.asInstanceOf[Reg].updates += ((Fame5Transform.fireSignals(module), oldReadAddr))
        
        val newReadAddr = Mux(Fame5Transform.fireSignals(module), oldReadAddr, oldReadAddrReg)
        
        val oldReadEn = Bool()
        oldReadEn.inputs += memSeqRead.addrReg.updates(0)._1
        val renReg = Reg(init=Bool(false))
        renReg.comp.component = module
        
        renReg.comp.asInstanceOf[Reg].enable = if(renReg.comp.asInstanceOf[Reg].isEnable) renReg.comp.asInstanceOf[Reg].enable || Fame5Transform.fireSignals(module) else Fame5Transform.fireSignals(module)
        renReg.comp.asInstanceOf[Reg].isEnable = true
        renReg.comp.asInstanceOf[Reg].updates += ((Fame5Transform.fireSignals(module), oldReadEn))
        val newRen = Mux(Fame5Transform.fireSignals(module), oldReadEn, renReg)
        
        memSeqRead.addrReg.enable = newRen
        memSeqRead.addrReg.updates.clear
        memSeqRead.addrReg.updates += ((newRen, newReadAddr))
      }
    }
  }*/
  
  def visualizeGraph(nodes: ArrayBuffer[Node], fileName: String) = {
    val outFile = new java.io.FileWriter("/home/eecs/wenyu/fame5-transform/fame5-testbench/" + fileName)
    outFile.write("digraph G {\n")
    outFile.write("graph [rankdir=LR];\n")
    var nameEnum = 0
    val nodeNames = new HashMap[Node, String]
    for(node <- nodes){
      var fillerStatus = ""
      outFile.write("n" + nameEnum + " [label=\"" + node.name + " " + fillerStatus + """\n""" + "\"" + ", style = filled, fillcolor = red" + "];\n")
      nodeNames(node) = "n" + nameEnum
      nameEnum = nameEnum + 1
    }
    for(node <- nodes){
      for(input <- node.inputs){
        if(nodeNames.contains(input)){
          outFile.write(nodeNames(input) + " -> " + nodeNames(node) + ";\n")
        }
      }
    }
    outFile.write("}\n")
    outFile.close
  }
  
  preElaborateTransforms += ((top: Module) => levelChildren(top))
  preElaborateTransforms += ((top: Module) => {Module.sortedComps = gatherChildren(top).sortWith((x,y) => (x.level < y.level || (x.level == y.level && x.traversal < y.traversal)) )})
  preElaborateTransforms += ((top: Module) => collectNodesIntoComp(initializeDFS))
  preElaborateTransforms += ((top: Module) => findConsumerMap(top)) 
  preElaborateTransforms += ((top: Module) => driveOutputs())
  preElaborateTransforms += ((top: Module) => collectNodesIntoComp(initializeDFS))
  preElaborateTransforms += ((top: Module) => findConsumerMap(top)) 
  preElaborateTransforms += ((top: Module) => muxInputs())
  preElaborateTransforms += ((top: Module) => replicateRegisters())
  preElaborateTransforms += ((top: Module) => muxRegOutputs())
  preElaborateTransforms += ((top: Module) => appendFireToRegWriteEnables())
  preElaborateTransforms += ((top: Module) => collectNodesIntoComp(initializeDFS))
  preElaborateTransforms += ((top: Module) => findConsumerMap(top)) 
  preElaborateTransforms += ((top: Module) => replicateTransactionMems())
  //preElaborateTransforms += ((top: Module) => muxTransactionMemOutputs())
  //preElaborateTransforms += ((top: Module) => appendFireToTransactionMemEnables())
  //preElaborateTransforms += ((top: Module) => collectNodesIntoComp(initializeDFS))
  //preElaborateTransforms += ((top: Module) => visualizeGraph(Fame5Transform.topModule.nodes, "nodeGraph.gv"))
  /*preElaborateTransforms += ((top: Module) => top.genAllMuxes)
  preElaborateTransforms += ((top: Module) => appendFireToMemEnables(top))
  preElaborateTransforms += ((top: Module) => top.genAllMuxes)*/

}

class Fame5CppBackend extends CppBackend with Fame5Transform
class Fame5VerilogBackend extends VerilogBackend with Fame5Transform
class Fame5FPGABackend extends FPGABackend with Fame5Transform
