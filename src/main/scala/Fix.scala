package Chisel {

import IOdir._;
import Node._;

object Fix {
  def apply(width: Int, dir: Symbol): Fix = {
    val res = new Fix();
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
  
  def apply(dir: Symbol = null): Fix = Fix(-1, dir) 

}

class Fix extends Num {
  override def fromBits(n: Node) = {
    val res = Fix('output).asInstanceOf[this.type]; 
    res := n; 
    res};
  def <==(src: Fix) = {
    comp <== src.toBits;
  }
  def := (src: Fix) = {
    if (comp == null)
      this.asInstanceOf[IO] := src
    else
      comp := src.toBits;
  }
  override def apply(bit: Int): Fix = { Extract(this, bit){Fix()}};
  override def apply(hi: Int, lo: Int): Fix = {Extract(this, hi, lo){Fix()}};
  def apply(hi: Fix, lo: Fix): Fix = Extract(this, hi, lo, -1){Fix()};
  def apply(bit: Fix): Fix = {Extract(this, bit){Fix()}};
  override def unary_-(): Fix = UnaryNodeCell(this, "-"){Fix()};
  override def unary_~(): Fix = UnaryNodeCell(this, "~"){Fix()};
  override def unary_!(): Fix = UnaryNodeCell(this, "!"){Fix()};
  override def and(): Bool    = ReductionNodeCell(this, "&"){Fix()};
  override def or():  Bool    = ReductionNodeCell(this, "|"){Fix()};
  def << (b: Fix): Fix = BinaryNodeCell(this, b, "<<"){Fix()};
  def >> (b: Fix): Fix = BinaryNodeCell(this, b, ">>"){Fix()};
  def >>> (b: Fix): Fix = BinaryNodeCell(this, b, ">>>"){Fix()};
  def +  (b: Fix): Fix = BinaryNodeCell(this, b, "+"){Fix()};
  def *  (b: Fix): Fix = BinaryNodeCell(this, b, "*"){Fix()};
  def ^  (b: Fix): Fix = BinaryNodeCell(this, b, "^"){Fix()};
  def ?  (b: Fix): Fix = BinaryNodeCell(this, b, "?"){Fix()};
  def -  (b: Fix): Fix = BinaryNodeCell(this, b, "-"){Fix()};
  def ## (b: Fix): Fix = BinaryNodeCell(this, b, "##"){Fix()};
  def ===(b: Fix): Bool = LogicalNodeCell(this, b, "==="){Fix()};
  def != (b: Fix): Bool = LogicalNodeCell(this, b, "!="){Fix()};
  def >  (b: Fix): Bool = LogicalNodeCell(this, b, ">"){Fix()};
  def <  (b: Fix): Bool = LogicalNodeCell(this, b, "<"){Fix()};
  def <= (b: Fix): Bool = LogicalNodeCell(this, b, "<="){Fix()};
  def >= (b: Fix): Bool = LogicalNodeCell(this, b, ">="){Fix()};
  def && (b: Fix): Bool = LogicalNodeCell(this, b, "&&"){Fix()};
  def || (b: Fix): Bool = LogicalNodeCell(this, b, "||"){Fix()};
  def &  (b: Fix): Fix = BinaryNodeCell(this, b, "&"){Fix()};
  def |  (b: Fix): Fix = BinaryNodeCell(this, b, "|"){Fix()};
}
}
