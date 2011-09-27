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
      println("INVALID DIRECTION" + dir);
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
  type T = Fix;
  override def fromNode(n: Node) = {
    val res = Fix('output).asInstanceOf[this.type]; 
    res assign n; 
    res};
  
  private def lessEqEq(src: Num) = {
    comp procAssign src.toNode;
  }

  private def colonEqual(src: Num) = {
    if (comp == null)
      this.asInstanceOf[IO] assign src
    else
      comp assign src.toNode
  }

  override def :=(src: Fix)  = colonEqual(src);
  override def :=(src: UFix) = colonEqual(Cat(Bits(0), src).toUFix);

  override def <==(src: Fix)  = lessEqEq(src);
  override def <==(src: UFix) = lessEqEq(Cat(Bits(0), src).toUFix);

  def gen[T <: Num](): T = Fix().asInstanceOf[T];

  override def apply(bit: Int): Fix = { Extract(this, bit){Fix()}};
  override def apply(hi: Int, lo: Int): Fix = {Extract(this, hi, lo){Fix()}};
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
