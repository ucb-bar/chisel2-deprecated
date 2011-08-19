package Chisel {
import IOdir._;

object bool_t {
  def apply(dir: IOdir): bool_t = {
    val res = new bool_t();
    res.dir = dir;
    res.init("", 1);
    res
  }
  def apply(dir: Symbol): bool_t = {
    if (dir == null){
      val res = new bool_t()
      res.init("", 1)
      res
    }
    else if(dir.equals('input))
      bool_t(INPUT)
    else if(dir.equals('output))
      bool_t(OUTPUT)
    else {
      println("INVALID DIRECTION")
      val res = new bool_t()
      res.init("", 1)
      res
    }
  }
}

class bool_t extends int_t {
  override def toBits = this;
  override def fromBits(n: Node) = {
    val res = bool_t(OUTPUT).asInstanceOf[this.type];
    res := n;
    res
  }
  override def unary_-(): bool_t = UnaryBoolCell(this, "-");
  override def unary_~(): bool_t = UnaryBoolCell(this, "~");
  override def unary_!(): bool_t = UnaryBoolCell(this, "!");
  def ===(b: bool_t): bool_t = BinaryBoolCell(this, b, "===");
  def != (b: bool_t): bool_t = BinaryBoolCell(this, b, "!=");
  def >  (b: bool_t): bool_t = BinaryBoolCell(this, b, ">");
  def <  (b: bool_t): bool_t = BinaryBoolCell(this, b, "<");
  def <= (b: bool_t): bool_t = BinaryBoolCell(this, b, "<=");
  def >= (b: bool_t): bool_t = BinaryBoolCell(this, b, ">=");
  def && (b: bool_t): bool_t = BinaryBoolCell(this, b, "&&");
  def || (b: bool_t): bool_t = BinaryBoolCell(this, b, "||");
  def &  (b: bool_t): bool_t = BinaryBoolCell(this, b, "&");
  def |  (b: bool_t): bool_t = BinaryBoolCell(this, b, "|");
}
}

