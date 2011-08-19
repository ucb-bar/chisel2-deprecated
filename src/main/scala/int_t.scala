package Chisel {

import IOdir._;
import Node._;

object int_t {
  def apply(n: Node): int_t = {
    val res = new int_t();
    res.init("", widthOf(0), n);
    res
  }
  def apply(dir: IOdir, width: Int): int_t = {
    val res = new int_t();
    res.dir = dir
    if(width > 0)
      res.init("", width);
    else
      res.init("", widthOf(0));
    res
  }
  def apply(dir: IOdir): int_t = {
    int_t(dir, -1);
  }
  def apply(dir: Symbol): int_t = {
    if(dir.equals('input))
      int_t(INPUT)
    else
      int_t(OUTPUT)
  }
  def apply(width: Int, dir: Symbol = null): int_t = {
    if(dir == null){
      val res = new int_t()
      res.init("", widthOf(0))
      res
    }
    else if(dir.equals('input))
      int_t(INPUT, width)
    else if(dir.equals('output))
      int_t(OUTPUT, width)
    else{
      println("INVALID DIRECTION");
      val res = new int_t();
      res.init("", widthOf(0))
      res
    }
  }
}

class int_t extends IO {
  override def toBits = this;
  override def fromBits(n: Node) = {
    val res = int_t(OUTPUT).asInstanceOf[this.type]; 
    res := n; 
    res};
  def <==(src: int_t) = {
    comp <== src.toBits;
  }
  def := (src: int_t) = {
    if (comp == null)
      this.asInstanceOf[IO] := src
    else
      comp := src.toBits;
  }
  override def apply(bit: Int): int_t = { Bits(this, bit)};
  override def apply(hi: Int, lo: Int): int_t = {Bits(this, hi, lo)};
  def apply(hi: int_t, lo: int_t): int_t = {Bits(this, hi, lo)};
  def apply(bit: int_t): int_t = {Bits(this, bit)};
  override def unary_-(): int_t = UnaryNodeCell(this, "-");
  override def unary_~(): int_t = UnaryNodeCell(this, "~");
  override def unary_!(): int_t = UnaryNodeCell(this, "!");
  def << (b: int_t): int_t = BinaryNodeCell(this, b, "<<");
  def >> (b: int_t): int_t = BinaryNodeCell(this, b, ">>");
  def >>>(b: int_t): int_t = BinaryNodeCell(this, b, ">>>");
  def +  (b: int_t): int_t = BinaryNodeCell(this, b, "+");
  def *  (b: int_t): int_t = BinaryNodeCell(this, b, "*");
  def ^  (b: int_t): int_t = BinaryNodeCell(this, b, "^");
  def ?  (b: int_t): int_t = BinaryNodeCell(this, b, "?");
  def -  (b: int_t): int_t = BinaryNodeCell(this, b, "-");
  def ## (b: int_t): int_t = BinaryNodeCell(this, b, "##");
  def ===(b: int_t): bool_t = LogicalNodeCell(this, b, "===");
  def != (b: int_t): bool_t = LogicalNodeCell(this, b, "!=");
  def >  (b: int_t): bool_t = LogicalNodeCell(this, b, ">");
  def <  (b: int_t): bool_t = LogicalNodeCell(this, b, "<");
  def <= (b: int_t): bool_t = LogicalNodeCell(this, b, "<=");
  def >= (b: int_t): bool_t = LogicalNodeCell(this, b, ">=");
  def && (b: int_t): bool_t = LogicalNodeCell(this, b, "&&");
  def || (b: int_t): bool_t = LogicalNodeCell(this, b, "||");
  def &  (b: int_t): int_t = BinaryNodeCell(this, b, "&");
  def |  (b: int_t): int_t = BinaryNodeCell(this, b, "|");
}
}
