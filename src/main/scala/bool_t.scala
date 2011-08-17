package Chisel {
import IOdir._;

object bool_t {
  def apply(dir: IOdir): bool_t = {
    val res = new bool_t();
    res.dir = dir;
    res.init("", 1);
    res
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

