package Chisel {

import IOdir._;
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

  def apply(x: BigInt): Bits = Lit(x){Bits()};
  def apply(x: BigInt, width: Int): Bits = Lit(x, width){Bits()};
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

class Bits extends IO {
  override def toNode = this;
  ioMap += ((this, ioCount));
  ioCount += 1;
  override def fromNode(n: Node) = {
    val res = Bits(OUTPUT).asInstanceOf[this.type];
    res assign n;
    res
  }

  override def toString: String = {
    "BITS(" + name + ", " + super.toString + ")"
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
      // println("COMP " + comp + " SRC " + src + " SRC.TONODE " + src.toNode);
      comp procAssign src.toNode;
    } else {
      this.asInstanceOf[Wire] procAssign src.toNode;
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
