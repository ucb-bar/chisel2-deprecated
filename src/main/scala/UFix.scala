package Chisel {

import IOdir._;
import Node._;

object UFix {
  def apply(width: Int, dir: Symbol): UFix = {
    val res = new UFix();
    if(dir.equals('input))
      res.dir = INPUT;
    else if(dir.equals('output))
      res.dir = OUTPUT;
    else if(dir == null)
      res.dir == null;
    else
      println("INVALID DIRECTION");
    if(width > 0)
      res.init("", width);
    else 
      res.init("", widthOf(0))
    res
  }

  def apply(dir: Symbol = null): UFix = UFix(-1, dir)
}

class UFix extends Num {
  override def fromBits(n: Node) = {
    val res = UFix('output).asInstanceOf[this.type];
    res := n;
    res
  }

  def <==(src: UFix) = {
    comp <== src.toBits;
  }

  def := (src: UFix) = {
    if (comp == null)
      this.asInstanceOf[IO] := src
    else
      comp := src.toBits
  }
  
  override def apply(bit: Int): UFix = { Extract(this, bit){UFix()}};
  override def apply(hi: Int, lo: Int): UFix = {Extract(this, hi, lo){UFix()}};
  override def unary_-(): UFix = UnaryNodeCell(this, "-"){UFix()};
  override def unary_~(): UFix = UnaryNodeCell(this, "~"){UFix()};
  override def unary_!(): UFix = UnaryNodeCell(this, "!"){UFix()};
  override def and(): Bool    = ReductionNodeCell(this, "&"){UFix()};
  override def or():  Bool    = ReductionNodeCell(this, "|"){UFix()};
  def << (b: UFix): UFix = BinaryNodeCell(this, b, "<<"){UFix()};
  def >> (b: UFix): UFix = BinaryNodeCell(this, b, ">>"){UFix()};
  def +  (b: UFix): UFix = BinaryNodeCell(this, b, "+"){UFix()};
  def *  (b: UFix): UFix = BinaryNodeCell(this, b, "*"){UFix()};
  def ^  (b: UFix): UFix = BinaryNodeCell(this, b, "^"){UFix()};
  def ?  (b: UFix): UFix = BinaryNodeCell(this, b, "?"){UFix()};
  def -  (b: UFix): UFix = BinaryNodeCell(this, b, "-"){UFix()};
  def ## (b: UFix): UFix = BinaryNodeCell(this, b, "##"){UFix()};
  def ===(b: UFix): Bool = LogicalNodeCell(this, b, "==="){UFix()};
  def != (b: UFix): Bool = LogicalNodeCell(this, b, "!="){UFix()};
  def >  (b: UFix): Bool = LogicalNodeCell(this, b, ">"){UFix()};
  def <  (b: UFix): Bool = LogicalNodeCell(this, b, "<"){UFix()};
  def <= (b: UFix): Bool = LogicalNodeCell(this, b, "<="){UFix()};
  def >= (b: UFix): Bool = LogicalNodeCell(this, b, ">="){UFix()};
  def && (b: UFix): Bool = LogicalNodeCell(this, b, "&&"){UFix()};
  def || (b: UFix): Bool = LogicalNodeCell(this, b, "||"){UFix()};
  def &  (b: UFix): UFix = BinaryNodeCell(this, b, "&"){UFix()};
  def |  (b: UFix): UFix = BinaryNodeCell(this, b, "|"){UFix()};

}

}
