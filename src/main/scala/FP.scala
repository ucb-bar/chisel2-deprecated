package Chisel
import Node._
import ChiselError._

object FloLit {
  def apply(x: Float) = {
    (new FloLiteral(x)).setTypeNode(new Flo())
  }
}
class FloLiteral(val floValue: Float) extends Node {
  override val value: BigInt = floValue.toInt
  override def genSubNodes = subnodes += new FloLiteral(floValue)
}

object Flo {

  def apply(x: Float): Flo = FloLit(x);
  def apply(x: Double): Flo = FloLit(x.toFloat);
  
  def apply(dir: IODirection = null): Flo = {
    val res = new Flo();
    res.dir = dir;
    res.init("", fixWidth(32))
    res
  }
}

class Flo extends Num {
  setIsSigned

  override def setIsTypeNode = {inputs(0).setIsSigned; super.setIsTypeNode}

  type T = Flo;
  override def fromNode(n: Node) = {
    val res = Flo(OUTPUT).asInstanceOf[this.type]; 
    res assign n; 
    res};

  private def colonEqual(src: Flo) = {
    if(comp != null)
      comp procAssign src.toNode;
    else
      this procAssign src.toNode;
  }

  override def :=[T <: Data](src: T): Unit = {
    src match {
      case flo: Flo => 
        this := flo;
      case any => 
	ChiselErrors += ChiselError(":= not defined on " + this.getClass + " and " + src.getClass, Thread.currentThread().getStackTrace)
    }
  }

  def :=(src: Flo)  = colonEqual(src);

  def gen[T <: Num](): T = Flo().asInstanceOf[T];

  override def unary_-(): Flo = UnaryOp(this, "f-"){Flo()};
  def +  (b: Flo): Flo = BinaryOp(this, b, "f+"){Flo()};
  def -  (b: Flo): Flo = BinaryOp(this, b, "f-"){Flo()};
  def *  (b: Flo): Flo = BinaryOp(this, b, "f*"){Flo()};
  def /  (b: Flo): Flo = BinaryOp(this, b, "f/"){Flo()};
  def ===(b: Flo): Bool = LogicalOp(this, b, "f=="){Flo()};
  def != (b: Flo): Bool = LogicalOp(this, b, "f!="){Flo()};
  def >  (b: Flo): Bool = LogicalOp(this, b, "f>"){Flo()};
  def <  (b: Flo): Bool = LogicalOp(this, b, "f<"){Flo()};
  def <= (b: Flo): Bool = LogicalOp(this, b, "f<="){Flo()};
  def >= (b: Flo): Bool = LogicalOp(this, b, "f>="){Flo()};
}

object Sin {
  def apply (x: Flo) = UnaryOp(x, "fsin"){Flo()};
}
