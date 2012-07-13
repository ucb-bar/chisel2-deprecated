package Chisel
import Node._
import ChiselError._

object UFix {
  // def apply(x: BigInt): UFix = Lit(x){UFix()};
  // def apply(x: BigInt, width: Int): UFix = Lit(x, width){UFix()};
  def apply(x: Int): UFix = Lit(x){UFix()};
  def apply(x: Int, width: Int): UFix = Lit(x, width){UFix()};
  
  def apply(dir: IODirection = null, width: Int = -1): UFix = {
    val res = new UFix();
    res.dir = dir;
    if(width > 0)
      res.init("", width);
    else 
      res.init("", widthOf(0))
    res
  }
}

class UFix extends Num {
  type T = UFix;
  override def fromNode(n: Node) = {
    val res = UFix(OUTPUT).asInstanceOf[this.type];
    res assign n;
    res
  }

  override def :=(src: UFix) = {
    if(comp != null)
      comp procAssign src.toNode;
    else
      this procAssign src.toNode;
  }

  override def :=[T <: Data](src: T): Unit = {
    src match {
      case ufix: UFix => {
	this := ufix;
      }
      case any => 
	ChiselErrors += ChiselError(":= not defined on " + this.getClass + " and " + src.getClass, Thread.currentThread().getStackTrace)
    }
  }
  
  override def apply(bit: Int): UFix = { Extract(this, bit){UFix()}};
  override def apply(hi: Int, lo: Int): UFix = {Extract(this, hi, lo){UFix()}};
  override def apply(bit: UFix): UFix = {Extract(this, bit){UFix()}};
  override def apply(hi: UFix, lo: UFix): UFix = {Extract(this, hi, lo, -1){UFix()}};
  override def apply(range: (Int, Int)): UFix = this(range._1, range._2);

  override def unary_-(): UFix = UnaryOp(this, "-"){UFix()};
  override def unary_~(): UFix = UnaryOp(this, "~"){UFix()};
  override def andR(): Bool    = ReductionOp(this, "&"){UFix()};
  override def orR():  Bool    = ReductionOp(this, "|"){UFix()};
  override def << (b: UFix): UFix = BinaryOp(this, b, "<<"){UFix()};
  override def >> (b: UFix): UFix = BinaryOp(this, b, ">>"){UFix()};
  def +  (b: UFix): UFix = BinaryOp(this, b, "+"){UFix()};
  def *  (b: UFix): UFix = BinaryOp(this, b, "*"){UFix()};
  def ^  (b: UFix): UFix = BinaryOp(this, b, "^"){UFix()};
  def ?  (b: UFix): UFix = BinaryOp(this, b, "?"){UFix()};
  def -  (b: UFix): UFix = BinaryOp(this, b, "-"){UFix()};
  def ## (b: UFix): UFix = BinaryOp(this, b, "##"){UFix()};
  def ===(b: UFix): Bool = LogicalOp(this, b, "==="){UFix()};
  def != (b: UFix): Bool = LogicalOp(this, b, "!="){UFix()};
  def >  (b: UFix): Bool = LogicalOp(this, b, ">"){UFix()};
  def <  (b: UFix): Bool = LogicalOp(this, b, "<"){UFix()};
  def <= (b: UFix): Bool = LogicalOp(this, b, "<="){UFix()};
  def >= (b: UFix): Bool = LogicalOp(this, b, ">="){UFix()};
  def &  (b: UFix): UFix = BinaryOp(this, b, "&"){UFix()};
  def |  (b: UFix): UFix = BinaryOp(this, b, "|"){UFix()};

  //UFix op Fix arithmetic
  def *   (b: Fix): Fix = BinaryOp(this, b, "u*s"){Fix()}.toFix;
}

class Eyum extends UFix { };
object Eyum {
  def apply[T <: Eyum](x: Int, w: Int)(gen: => T): T = { Lit(x, w){ gen } }
  def apply[T <: Eyum](w: Int)(gen: => T): Int => T = { (x: Int) => Lit(x, w){ gen } }
}
