package Chisel {

import IOdir._;
import Node._;

object Fix {

  def apply(x: Int): Fix = Lit(x){Fix()};
  def apply(x: Int, width: Int): Fix = Lit(x, width){Fix()};
  
  def apply(width: Int = -1, dir: Symbol = null): Fix = {
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
  
  def apply(dir: Symbol): Fix = Fix(-1, dir) 
  
  def apply(): Fix = Fix(-1, null);

}

class Fix extends Num {
  override def fromNode(n: Node) = {
    val res = Fix('output).asInstanceOf[this.type]; 
    res := n; 
    res};
  def <==(src: Fix) = {
    comp <== src.toNode;
  }
  def := (src: Fix) = {
    if (comp == null)
      this.asInstanceOf[IO] := src
    else
      comp := src.toNode;
  }
  
  def gen[T <: Num](): T = Fix().asInstanceOf[T];

  override def apply(bit: Int): Fix = { Extract(this, bit){Fix()}};
  override def apply(hi: Int, lo: Int): Fix = {Extract(this, hi, lo){Fix()}};
  def apply(hi: Fix, lo: Fix): Fix = Extract(this, hi, lo, -1){Fix()};
  def apply(bit: Fix): Fix = {Extract(this, bit){Fix()}};
  override def unary_-(): Fix = UnaryNodeCell(this, "-"){Fix()};
  override def unary_~(): Fix = UnaryNodeCell(this, "~"){Fix()};
  def unary_!(): Fix = UnaryNodeCell(this, "!"){Fix()};
  def andR(): Bool    = ReductionNodeCell(this, "&"){Fix()};
  def orR():  Bool    = ReductionNodeCell(this, "|"){Fix()};
  def xorR(): Bool   = ReductionNodeCell(this, "^"){Fix()};
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
