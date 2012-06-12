package Chisel
import Node._
import ChiselError._

object Bool {
  def apply(x: Boolean) = Lit(x);
  
  def apply(dir: IODirection = null): Bool = {
    val res = new Bool();
    res.dir = dir;
    res.init("", 1)
    res
  }
  
  def apply(): Bool = Bool(null);
}

class Bool extends Bits {
  override def fromNode(n: Node) = {
    val res = Bool(OUTPUT).asInstanceOf[this.type];
    res assign n;
    res
  }

  override def :=(src: Bool) = {
    if(comp != null)
      comp procAssign src.toNode;
    else
      this procAssign src.toNode;
  }

  def := (src: Bits): Unit = {
    val res = src.toBool;
    if(src.getWidth > 1)
      throw new Exception("multi bit signal " + src + " converted to Bool");
    if(src.getWidth == -1)
      throw new Exception("unable to automatically convert " + src + " to Bool, convert manually instead");
    this := res;
  }
  
  override def :=[T <: Data](src: T): Unit = {
    src match {
      case bool: Bool => {
	this := bool;
      }
      case bits: Bits => {
	this := bits;
      }
      case any => 
	ChiselErrors += ChiselError(":= not defined on " + this.getClass + " and " + src.getClass, Thread.currentThread().getStackTrace)
    }
  }

  def generateError = {
    ChiselErrors += ChiselError("Cannot perform extraction on a Bool", Thread.currentThread().getStackTrace);
  }

  override def apply(bit: Int): Bool = { generateError; this};
  override def apply(hi: Int, lo: Int): Bool = {generateError; this};
  override def apply(bit: UFix): Bool = {generateError; this};
  override def apply(hi: UFix, lo: UFix): Bool = {generateError; this};

  override def unary_-(): Bool = UnaryBoolOp(this, "-");
  override def unary_~(): Bool = UnaryBoolOp(this, "~");
  def unary_!(): Bool = UnaryBoolOp(this, "!");
  def ^  (b: Bool): Bool = BinaryBoolOp(this, b, "^");
  def ===(b: Bool): Bool = BinaryBoolOp(this, b, "===");
  def != (b: Bool): Bool = BinaryBoolOp(this, b, "!=");
  override def && (b: Bool): Bool = if (b.isTrue) this else BinaryBoolOp(this, b, "&&");
  override def || (b: Bool): Bool = BinaryBoolOp(this, b, "||");
  def &  (b: Bool): Bool = BinaryBoolOp(this, b, "&");
  def |  (b: Bool): Bool = BinaryBoolOp(this, b, "|");

  def isTrue: Boolean = {
    if(inputs.length == 0) return false
    inputs(0) match {
      case l: Literal => {l.isLit && l.value == 1};
      case any        => false;
    }
  }
}
