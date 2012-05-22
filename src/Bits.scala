package Chisel {

import Node._;
import Bits._;
import ChiselError._
import Component._

object Bits {
  def conv(x: Bits): Bool = {
    if(x.getWidth > 1)
      throw new Exception("multi bit signal " + x + " converted to Bool");
    if(x.getWidth == -1)
      throw new Exception("unable to automatically convert " + x + " to Bool, convert manually instead")
    x.toBool
  }

  def apply(x: Int): Bits = Lit(x){Bits()};
  def apply(x: Int, width: Int): Bits = Lit(x, width){Bits()};
  def apply(x: String): Bits = Lit(x){Bits()};
  def apply(x: String, width: Int): Bits = Lit(x, width){Bits()};
  
  def apply(width: Int = -1, dir: IODirection = null): Bits = {
    val res = new Bits();
    res.dir = dir;
    if(width > 0)
      res.init("", width);
    else 
      res.init("", widthOf(0))
    res
  }
  
  def apply(dir: IODirection): Bits = Bits(-1, dir) 
  
  def apply(): Bits = Bits(-1, null);
}

class Bits extends Data with proc {
  ioMap += ((this, ioCount));
  ioCount += 1;

  var dir: IODirection = null;
  var unnamed = false;

  override def isIo = dir != null;

  override def toNode = this;
  override def fromNode(n: Node) = {
    val res = Bits(OUTPUT).asInstanceOf[this.type];
    res assign n;
    res
  }
  def default: Node = if (inputs.length < 1 || inputs(0) == null) null else inputs(0);

  // internal, non user exposed connectors
  var assigned = false;

  def procAssign(src: Node) = {
    if (assigned) {
      ChiselErrors += ChiselError("reassignment to Node", Thread.currentThread().getStackTrace);
    } else {
      updates.enqueue((genCond(), src));
    }
  }

  override def assign(src: Node) = {
    if(assigned || inputs.length > 0) {
      ChiselErrors += ChiselError("reassignment to Wire", Thread.currentThread().getStackTrace);
    } else { 
      assigned = true; super.assign(src)
    }
  }

  //code generation stuff
  override def emitRefC: String = if (!isInObject && inputs.length == 1) inputs(0).emitRefC else super.emitRefC

  override def emitDefLoC: String = 
    if (isInObject && inputs.length == 1)
      "  " + emitTmp + " = " + inputs(0).emitRef + ";\n"
    else if (inputs.length == 0 && !isInObject) {
      "  " + emitTmp + ";\n"
    } else
      ""

  override def emitDef: String = {
    if (dir == INPUT)
      ""
    else {
      if (inputs.length == 0) {
        println("// UNCONNECTED " + this + " IN " + component); ""
      } else if (inputs(0) == null) {
        println("// UNCONNECTED WIRE " + this + " IN " + component); ""
      } else
        "  assign " + emitTmp + " = " + inputs(0).emitRef + ";\n" 
    }
  }

  override def emitDec: String = 
    if(dir == null)
      super.emitDec
    else
      ""

  override def emitDecC: String = 
    if(dir == null) {
      super.emitDecC
    } else {
      if (isEmittingComponents) ""; else super.emitDecC
    }

  override def apply(name: String): Data = this

  override def flatten = Array((name, this));

  override def toString: String = {
    if (dir == INPUT)
      "INPUT(" + name + (if (component == null) "" else ("." + component)) + ")";
    else if (dir == OUTPUT)
      "OUTPUT(" + name + (if (component == null) "" else ("." + component)) + ")";
    else
      "BITS(" + name + ", " + super.toString + ")"      
  }


  override def flip(): this.type = {
    assert(dir != null, println("Can't flip something that doesn't have a direction"))
    if (dir == INPUT) {
      dir = OUTPUT
    } else if(dir == OUTPUT) {
      dir = INPUT
    }
    this
  }

  override def asInput(): this.type = {
    dir = INPUT
    this
  }

  override def asOutput(): this.type = {
    dir = OUTPUT
    this
  }

  override def <>(src: Node) = { 
    if (dir == INPUT) {
      src match { 
      case other: Bits => 
	if (other.dir == OUTPUT) { // input - output connections
          if(this.staticComp == other.staticComp && !isTypeNode) //passthrough
	    other assign this
          else if (this.staticComp.parent == other.staticComp.parent || isTypeNode) //producer - consumer
            this assign other
          else
            ChiselErrors += ChiselError({"Undefined connections between " + this + " and " + other}, Thread.currentThread().getStackTrace)
        } else if (other.dir == INPUT) { // input <> input conections
	  if(this.staticComp == other.staticComp.parent) // parent <> child
	    other assign this
	  else if(this.staticComp.parent == other.staticComp) //child <> parent
	    this assign other
	  else
	    ChiselErrors += ChiselError({"Can't connect Input " + this + " Input " + other}, Thread.currentThread().getStackTrace)
	} else { // io <> wire
          if(this.staticComp == other.staticComp) //internal wire
            other assign this
          else if(this.staticComp.parent == other.staticComp) //external wire
            this assign other
          else 
            ChiselErrors += ChiselError({"Connecting Input " + this + " to " + other}, Thread.currentThread().getStackTrace)
        }
      case default => 
        ChiselErrors += ChiselError({"Connecting Input " + this + " to IO without direction " + default}, Thread.currentThread().getStackTrace)
      }
    } else if (dir == OUTPUT) {
      src match { 
        case other: Bits  => 
	  if (other.dir == INPUT) { // input - output connections
            if (this.staticComp == other.staticComp && !isTypeNode) //passthrough
	      this assign other;
	    else if (this.staticComp.parent == other.staticComp.parent || isTypeNode) //producer - consumer
              other assign this;
            else
              ChiselErrors += ChiselError({"Undefined connection between " + this + " and " + other}, Thread.currentThread().getStackTrace)
          } else if (other.dir == OUTPUT) { // output <> output connections
	    if(this.staticComp == other.staticComp.parent) // parent <> child
	      this assign other
	    else if (this.staticComp.parent == other.staticComp) // child <> parent
	      other assign this
	    else if (this.isTypeNode && other.isTypeNode) //connecting two type nodes together
	      ChiselErrors += ChiselError("Ambiguous Connection of Two Nodes", Thread.currentThread().getStackTrace)
	    else if (this.isTypeNode){ // type <> output
	      other assign this; }
	    else if (other.isTypeNode){ // output <> type
	      this assign other; }
	    else
	      ChiselErrors += ChiselError({"Connecting Output " + this + " to Output " + other}, Thread.currentThread().getStackTrace)
	  } else { // io <> wire
            if(this.staticComp == other.staticComp) //output <> wire
              this assign other
            else if(this.staticComp.parent == other.staticComp)
              ChiselErrors += ChiselError({"Connecting Ouptut " + this + " to an external wire " + other}, Thread.currentThread().getStackTrace)
            else
              ChiselErrors += ChiselError({"Connecting Output " + this + " to IO without direction " + other}, Thread.currentThread().getStackTrace)
          }
        case default => 
          ChiselErrors += ChiselError({"Connecting Output " + this + " to an IO withouth direction " + default}, Thread.currentThread().getStackTrace)
      }
    }
    else {
      src match {
        case other: Bits =>
          if (other.dir == INPUT) { // wire <> input
            if(this.staticComp == other.staticComp)
              this assign other
            else if(this.staticComp == other.staticComp.parent)
              other assign this
            else 
              ChiselErrors += ChiselError({"Undefined connection between wire " + this + " and input " + other}, Thread.currentThread().getStackTrace)
          } else if (other.dir == OUTPUT) { //wire <> output
            if(this.staticComp == other.staticComp) // internal wire
              other assign this
            else if(this.staticComp == other.staticComp.parent) // external wire
              this assign other
            else
              ChiselErrors += ChiselError({"Undefined connection between wire " + this + " and output " + other}, Thread.currentThread().getStackTrace)
          } else {
            this assign other
          }
        case default =>
          ChiselErrors += ChiselError({"Undefined connection between " + this + " and " + default}, Thread.currentThread().getStackTrace)
      }
    }
  }

  override def emitRefV = 
    if(!isInObject || name == "") 
      super.emitRefV 
    else if(!named) 
      name + "_" + emitIndex 
    else 
      name

  override def setIsTypeNode = isTypeNode = true;

  override def setIsClkInput = {isClkInput = true; this assign clk;}

  override def clone = {
    val res = this.getClass.newInstance.asInstanceOf[this.type];
    res.inferWidth = this.inferWidth
    res.width_ = this.width_;
    res.dir = this.dir;
    res.name = this.name;
    res
  }

  override def maxNum = {
    if(inputs.length == 0) 
      width;
    else if(inputs(0).isLit)
      inputs(0).value
    else if (inputs.length == 1 && isTypeNode)
      inputs(0).maxNum
    else
      super.maxNum
  }

  def generateError(src: Bits) = {
    val myClass = this.getClass;
    val srcClass = src.getClass;
    if(myClass != classOf[Bits] && myClass == srcClass)
      ChiselErrors += ChiselError(":= not defined on " + myClass.toString + " " + classOf[Bits].toString, Thread.currentThread().getStackTrace)
    else if(myClass != classOf[Bits])
      ChiselErrors += ChiselError(":= not defined on " + myClass.toString + " " + srcClass.toString, Thread.currentThread().getStackTrace)
  }

  private def colonEqual(src: Bits) = {
    generateError(src);
    if(comp != null){
      comp procAssign src.toNode;
    } else {
      this procAssign src.toNode;
    }
  }

  def := (src: Bool) = colonEqual(src);
  def := (src: Fix)  = colonEqual(src);
  def := (src: UFix) = colonEqual(src);

  override def := [T <: Data](src: T): Unit = {
    src match {
      case bool: Bool => {
	this := bool;
      }
      case fix: Fix => {
	this := fix;
      }
      case ufix: UFix => {
	this := ufix
      }
      case bits: Bits => {
	this colonEqual(bits);
      }
      case any =>
	ChiselErrors += ChiselError(":= not defined on " + this.getClass + " and " + src.getClass, Thread.currentThread().getStackTrace);
    }
  }

  def apply(bit: Int): Bits = { Extract(this, bit){Bits()}};
  def apply(hi: Int, lo: Int): Bits = {Extract(this, hi, lo){Bits()}};
  def apply(bit: UFix): Bits = Extract(this, bit){Bits()};
  def apply(hi: UFix, lo: UFix): Bits = Extract(this, hi, lo, -1){Bits()};
  def apply(range: (Int, Int)): Bits = this(range._1, range._2);
  
  def unary_-(): Bits = UnaryOp(this, "-"){Bits()};
  def unary_~(): Bits = UnaryOp(this, "~"){Bits()};
  def andR(): Bool    = ReductionOp(this, "&"){Bits()};
  def orR():  Bool    = ReductionOp(this, "|"){Bits()};
  def ===(b: Bits): Bool = LogicalOp(this, b, "==="){Bits()};
  def != (b: Bits): Bool = LogicalOp(this, b, "!="){Bits()};
  def << (b: UFix): Bits = BinaryOp(this, b.toBits, "<<"){Bits()};
  def >> (b: UFix): Bits = BinaryOp(this, b.toBits, ">>"){Bits()};
  def &  (b: Bits): Bits = BinaryOp(this, b, "&"){Bits()};
  def |  (b: Bits): Bits = BinaryOp(this, b, "|"){Bits()};
  def ^  (b: Bits): Bits = BinaryOp(this, b, "^"){Bits()};
  def ## (b: Bits): Bits = BinaryOp(this, b, "##"){Bits()};

  def && (b: Bool): Bool = conv(this) && b;
  def || (b: Bool): Bool = conv(this) || b;
  
}

}
