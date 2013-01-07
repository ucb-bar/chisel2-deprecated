package Chisel
import Node._
import ChiselError._

/// FLO

object FloLit {
  def apply(x: Float) = {
    (new FloLiteral(x)).setTypeNode(new Flo())
  }
}
class FloLiteral(val floValue: Float) extends Node {
  override val value: BigInt = floValue.toInt
  override def genSubNodes = setSubNode(0, new FloLiteral(floValue))
  inferWidth = fixWidth(32)
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

/// DBL

object DblLit {
  def apply(x: Double) = {
    (new DblLiteral(x)).setTypeNode(new Dbl())
  }
}
class DblLiteral(val dblValue: Double) extends Node {
  override val value: BigInt = dblValue.toInt
  override def genSubNodes = setSubNode(0, new DblLiteral(dblValue))
  inferWidth = fixWidth(64)
}

object Dbl {

  def apply(x: Float): Dbl = DblLit(x.toDouble);
  def apply(x: Double): Dbl = DblLit(x);
  
  def apply(dir: IODirection = null): Dbl = {
    val res = new Dbl();
    res.dir = dir;
    res.init("", fixWidth(64))
    res
  }
}

class Dbl extends Num {
  setIsSigned

  override def setIsTypeNode = {inputs(0).setIsSigned; super.setIsTypeNode}

  type T = Dbl;
  override def fromNode(n: Node) = {
    val res = Dbl(OUTPUT).asInstanceOf[this.type]; 
    res assign n; 
    res};

  private def colonEqual(src: Dbl) = {
    if(comp != null)
      comp procAssign src.toNode;
    else
      this procAssign src.toNode;
  }

  override def :=[T <: Data](src: T): Unit = {
    src match {
      case dbl: Dbl => 
        this := dbl;
      case any => 
	ChiselErrors += ChiselError(":= not defined on " + this.getClass + " and " + src.getClass, Thread.currentThread().getStackTrace)
    }
  }

  def :=(src: Dbl)  = colonEqual(src);

  def gen[T <: Num](): T = Dbl().asInstanceOf[T];

  override def unary_-(): Dbl = UnaryOp(this, "d-"){Dbl()};
  def +  (b: Dbl): Dbl = BinaryOp(this, b, "d+"){Dbl()};
  def -  (b: Dbl): Dbl = BinaryOp(this, b, "d-"){Dbl()};
  def *  (b: Dbl): Dbl = { BinaryOp(this, b, "d*"){Dbl()}; }
  def /  (b: Dbl): Dbl = BinaryOp(this, b, "d/"){Dbl()};
  def ===(b: Dbl): Bool = LogicalOp(this, b, "d=="){Dbl()};
  def != (b: Dbl): Bool = LogicalOp(this, b, "d!="){Dbl()};
  def >  (b: Dbl): Bool = LogicalOp(this, b, "d>"){Dbl()};
  def <  (b: Dbl): Bool = LogicalOp(this, b, "d<"){Dbl()};
  def <= (b: Dbl): Bool = LogicalOp(this, b, "d<="){Dbl()};
  def >= (b: Dbl): Bool = LogicalOp(this, b, "d>="){Dbl()};
}

object Sin {
  def apply (x: Flo) = UnaryOp(x, "fsin"){Flo()};
  def apply (x: Dbl) = UnaryOp(x, "dsin"){Dbl()};
}

object Log {
  def apply (x: Flo): Flo = UnaryOp(x, "flog"){Flo()};
  def apply (x: Dbl): Dbl  = UnaryOp(x, "dlog"){Dbl()};
  def apply (x: Flo, p: Flo): Flo = Log(x)/Log(p)
  def apply (x: Dbl, p: Dbl): Dbl = Log(x)/Log(p)
}

object Pow {
  def apply (x: Flo, y: Flo) = BinaryOp(x, y, "fpow"){Flo()};
  def apply (x: Dbl, y: Dbl) = BinaryOp(x, y, "dpow"){Dbl()};
}
