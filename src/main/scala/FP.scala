package Chisel
import Node._
import ChiselError._

/// FLO

object FloLit {
  def apply(x: Float) = {
    (new Flo()).fromNode(new FloLiteral(x))
  }
}
class FloLiteral(val floValue: Float) extends Node {
  override val value: BigInt = floValue.toInt
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

class Flo extends Bits {
  // setIsSigned

  // override def setIsTypeNode = {inputs(0).setIsSigned; super.setIsTypeNode}

  type T = Flo;
  override def fromNode(n: Node): this.type = {
    val res = Flo(OUTPUT).asInstanceOf[this.type]; 
    res assign n; 
    res};

  override def fromInt(x: Int): this.type = {
    Flo(x.toFloat).asInstanceOf[this.type]
  }

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
	ChiselErrors += new ChiselError(() => { ":= not defined on " + this.getClass + " and " + src.getClass }, this.line)
    }
  }

  def floLitOf: FloLiteral = 
    if (inputs.length == 1) {
      if (inputs(0).isInstanceOf[FloLiteral])
        inputs(0).asInstanceOf[FloLiteral] 
      else if (inputs(0).isInstanceOf[Flo])
        inputs(0).asInstanceOf[Flo].floLitOf
      else 
        null;
    } else 
      null;

  def :=(src: Flo)  = colonEqual(src);

  def gen[T <: Bits](): T = Flo().asInstanceOf[T];

  def unary_-(): Flo = newUnaryOp("f-")
  def +  (b: Flo): Flo = newBinaryOp(b, "f+")
  def -  (b: Flo): Flo = newBinaryOp(b, "f-")
  def *  (b: Flo): Flo = newBinaryOp(b, "f*")
  def /  (b: Flo): Flo = newBinaryOp(b, "f/")
  def %  (b: Flo): Flo = newBinaryOp(b, "f%")
  def ===(b: Flo): Bool = newLogicalOp(b, "f==")
  def != (b: Flo): Bool = newLogicalOp(b, "f!=")
  def >  (b: Flo): Bool = newLogicalOp(b, "f>")
  def <  (b: Flo): Bool = newLogicalOp(b, "f<")
  def <= (b: Flo): Bool = newLogicalOp(b, "f<=")
  def >= (b: Flo): Bool = newLogicalOp(b, "f>=")
  def pow (b: Flo): Flo = newBinaryOp(b, "fpow") 
  def sin: Flo = newUnaryOp("fsin")
  def cos: Flo = newUnaryOp("fcos")
  def tan: Flo = newUnaryOp("ftan")
  def sqrt: Flo = newUnaryOp("fsqrt")
  def floor: Flo = newUnaryOp("ffloor")
  def ceil: Flo = newUnaryOp("fceil")
  def round: Flo = newUnaryOp("fround")
  def log: Flo = newUnaryOp("flog")
  override def toSInt (): SInt = SInt(OUTPUT).fromNode(Op("fToSInt", 1, fixWidth(32), this))
  override def toUInt (): UInt = UInt(OUTPUT).fromNode(Op("fToUInt", 1, fixWidth(32), this))
}

/// DBL

object DblLit {
  def apply(x: Double) = {
    (new Dbl()).fromNode(new DblLiteral(x))
  }
}
class DblLiteral(val dblValue: Double) extends Node {
  override val value: BigInt = dblValue.toInt
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

class Dbl extends Bits {
  // setIsSigned

  // override def setIsTypeNode = {inputs(0).setIsSigned; super.setIsTypeNode}

  type T = Dbl;
  override def fromNode(n: Node) = {
    val res = Dbl(OUTPUT).asInstanceOf[this.type]; 
    res assign n; 
    res};

  override def fromInt(x: Int): this.type = {
    Dbl(x.toDouble).asInstanceOf[this.type]
  }

  def dblLitOf: DblLiteral =
    if (inputs.length == 1) {
      if (inputs(0).isInstanceOf[DblLiteral])
        inputs(0).asInstanceOf[DblLiteral] 
      else if (inputs(0).isInstanceOf[Dbl])
        inputs(0).asInstanceOf[Dbl].dblLitOf
      else 
        null;
    } else 
      null;

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
	ChiselErrors += new ChiselError(() => { ":= not defined on " + this.getClass + " and " + src.getClass }, this.line)
    }
  }

  def :=(src: Dbl)  = colonEqual(src);

  def gen[T <: Bits](): T = Dbl().asInstanceOf[T];

  def unary_-(): Dbl = newUnaryOp("d-")
  def +  (b: Dbl): Dbl = newBinaryOp(b, "d+")
  def -  (b: Dbl): Dbl = newBinaryOp(b, "d-")
  def *  (b: Dbl): Dbl = newBinaryOp(b, "d*")
  def /  (b: Dbl): Dbl = newBinaryOp(b, "d/")
  def %  (b: Dbl): Dbl = newBinaryOp(b, "d%")
  def ===(b: Dbl): Bool = newLogicalOp(b, "d==")
  def != (b: Dbl): Bool = newLogicalOp(b, "d!=")
  def >  (b: Dbl): Bool = newLogicalOp(b, "d>")
  def <  (b: Dbl): Bool = newLogicalOp(b, "d<")
  def <= (b: Dbl): Bool = newLogicalOp(b, "d<=")
  def >= (b: Dbl): Bool = newLogicalOp(b, "d>=")
  def pow (b: Dbl): Dbl = newBinaryOp(b, "dpow") 
  def sin: Dbl = newUnaryOp("dsin")
  def cos: Dbl = newUnaryOp("dcos")
  def tan: Dbl = newUnaryOp("dtan")
  def sqrt: Dbl = newUnaryOp("dsqrt")
  def floor: Dbl = newUnaryOp("dfloor")
  def ceil: Dbl = newUnaryOp("dceil")
  def round: Dbl = newUnaryOp("dround")
  def log: Dbl = newUnaryOp("dlog")
  override def toSInt (): SInt = SInt(OUTPUT).fromNode(Op("dToSInt", 1, fixWidth(64), this))
  override def toUInt (): UInt = UInt(OUTPUT).fromNode(Op("dToUInt", 1, fixWidth(64), this))
}

object Sin {
  def apply (x: Flo): Flo = x.sin
  def apply (x: Dbl): Dbl = x.sin
}

object Cos {
  def apply (x: Flo): Flo = x.cos
  def apply (x: Dbl): Dbl = x.cos
}

object Tan {
  def apply (x: Flo): Flo = x.tan
  def apply (x: Dbl): Dbl = x.tan
}

object Sqrt {
  def apply (x: Flo): Flo = x.sqrt
  def apply (x: Dbl): Dbl = x.sqrt
}

object Floor {
  def apply (x: Flo): Flo = x.floor
  def apply (x: Dbl): Dbl = x.floor
}

object Ceil {
  def apply (x: Flo): Flo = x.ceil
  def apply (x: Dbl): Dbl = x.ceil
}

object Round {
  def apply (x: Flo): Flo = x.round
  def apply (x: Dbl): Dbl = x.round
}

object Log {
  def apply (x: Flo): Flo = x.log
  def apply (x: Dbl): Dbl  = x.log
  def apply (x: Flo, p: Flo): Flo = Log(x)/Log(p)
  def apply (x: Dbl, p: Dbl): Dbl = Log(x)/Log(p)
}

object Pow {
  def apply (x: Flo, y: Flo): Flo = x.pow(y)
  def apply (x: Dbl, y: Dbl): Dbl = x.pow(y)
}
