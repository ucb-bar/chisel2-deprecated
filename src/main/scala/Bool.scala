package Chisel {

import IOdir._;
import Node._;

object Bool {
  def apply(x: Boolean) = Lit(x);
  
  def apply(width: Int = -1, dir: Symbol = null): Bool = {
    val res = new Bool();
    if(dir == null)
      res.dir = null;
    else if(dir.equals('input))
      res.dir = INPUT;
    else if(dir.equals('output))
      res.dir = OUTPUT;
    else
      println("INVALID DIRECTION");
    if(width > 0)
      res.init("", width);
    else 
      res.init("", widthOf(0))
    res
  }
  
  def apply(dir: Symbol): Bool = Bool(-1, dir) 
  
  def apply(): Bool = Bool(-1, null);
}

class Bool extends Fix {
  override def fromNode(n: Node) = {
    val res = Bool('output).asInstanceOf[this.type];
    res := n;
    res
  }

  override def unary_-(): Bool = UnaryBoolCell(this, "-");
  override def unary_~(): Bool = UnaryBoolCell(this, "~");
  override def unary_!(): Bool = UnaryBoolCell(this, "!");
  def ^  (b: Bool): Bool = BinaryBoolCell(this, b, "^");
  def ===(b: Bool): Bool = BinaryBoolCell(this, b, "===");
  def != (b: Bool): Bool = BinaryBoolCell(this, b, "!=");
  def >  (b: Bool): Bool = BinaryBoolCell(this, b, ">");
  def <  (b: Bool): Bool = BinaryBoolCell(this, b, "<");
  def <= (b: Bool): Bool = BinaryBoolCell(this, b, "<=");
  def >= (b: Bool): Bool = BinaryBoolCell(this, b, ">=");
  def && (b: Bool): Bool = BinaryBoolCell(this, b, "&&");
  def || (b: Bool): Bool = BinaryBoolCell(this, b, "||");
  def &  (b: Bool): Bool = BinaryBoolCell(this, b, "&");
  def |  (b: Bool): Bool = BinaryBoolCell(this, b, "|");

  def isTrue = {
    inputs(0) match {
      case l: Literal => l.value == 1;
      case any        => false;
    }
  }
}
}

