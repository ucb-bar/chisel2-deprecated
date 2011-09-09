package Chisel {
import IOdir._;

object Bool {
  def apply(dir: IOdir): Bool = {
    val res = new Bool();
    res.dir = dir;
    res.init("", 1);
    res
  }
  def apply(dir: Symbol = null): Bool = {
    if (dir == null){
      val res = new Bool()
      res.init("", 1)
      res
    }
    else if(dir.equals('input))
      Bool(INPUT)
    else if(dir.equals('output))
      Bool(OUTPUT)
    else {
      println("INVALID DIRECTION")
      val res = new Bool()
      res.init("", 1)
      res
    }
  }
}

class Bool extends Fix {
  override def toBits = this;
  override def fromBits(n: Node) = {
    val res = Bool(OUTPUT).asInstanceOf[this.type];
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
}
}

