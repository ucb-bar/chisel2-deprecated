package Chisel {

import IOdir._;
import Node._;

object Bool {
  def apply(x: Boolean) = Lit(x);
  
  def apply(dir: Symbol = null): Bool = {
    val res = new Bool();
    if(dir == null)
      res.dir = null;
    else if(dir.equals('input))
      res.dir = INPUT;
    else if(dir.equals('output))
      res.dir = OUTPUT;
    else
      println("INVALID DIRECTION");
      res.init("", 1)
    res
  }
  
  def apply(): Bool = Bool(null);
}

class Bool extends Bits {
  override def fromNode(n: Node) = {
    val res = Bool('output).asInstanceOf[this.type];
    res assign n;
    res
  }

  
  def := (src: Bool) = {
    if (comp == null)
      this.asInstanceOf[IO] assign src
    else
      comp assign src.toNode;
  }

  def <==(src: Bool) = {
    comp procAssign src.toNode;
  }

  override def unary_-(): Bool = UnaryBoolCell(this, "-");
  override def unary_~(): Bool = UnaryBoolCell(this, "~");
  def unary_!(): Bool = UnaryBoolCell(this, "!");
  def ^  (b: Bool): Bool = BinaryBoolCell(this, b, "^");
  def ===(b: Bool): Bool = BinaryBoolCell(this, b, "===");
  def != (b: Bool): Bool = BinaryBoolCell(this, b, "!=");
  def >  (b: Bool): Bool = BinaryBoolCell(this, b, ">");
  def <  (b: Bool): Bool = BinaryBoolCell(this, b, "<");
  def <= (b: Bool): Bool = BinaryBoolCell(this, b, "<=");
  def >= (b: Bool): Bool = BinaryBoolCell(this, b, ">=");
  def && (b: Bool): Bool = if (b.isTrue) this else BinaryBoolCell(this, b, "&&");
  def || (b: Bool): Bool = BinaryBoolCell(this, b, "||");
  def &  (b: Bool): Bool = BinaryBoolCell(this, b, "&");
  def |  (b: Bool): Bool = BinaryBoolCell(this, b, "|");

  def isTrue: Boolean = {
    if(inputs.length == 0) return false
    inputs(0) match {
      case l: Literal => {l.isLit && l.value == 1};
      case any        => false;
    }
  }
}
}

