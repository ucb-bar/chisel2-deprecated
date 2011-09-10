package Chisel {

import IOdir._;
import Node._;

object UFix {
  
  def apply(x: Int): UFix = Lit(x){UFix()};
  def apply(x: Int, width: Int): UFix = Lit(x, width){UFix()};
  
  def apply(width: Int = -1, dir: Symbol = null): UFix = {
    val res = new UFix();
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
  
  def apply(dir: Symbol): UFix = UFix(-1, dir) 
  
  def apply(): UFix = UFix(-1, null);
}

class UFix extends Num {
  override def fromNode(n: Node) = {
    val res = UFix('output).asInstanceOf[this.type];
    res := n;
    res
  }

  def <==(src: UFix) = {
    comp <== src.toNode;
  }

  def := (src: UFix) = {
    if (comp == null)
      this.asInstanceOf[IO] := src
    else
      comp := src.toNode
  }


  
  override def apply(bit: Int): UFix = { Extract(this, bit){UFix()}};
  override def apply(hi: Int, lo: Int): UFix = {Extract(this, hi, lo){UFix()}};
  override def unary_-(): UFix = UnaryNodeCell(this, "-"){UFix()};
  override def unary_~(): UFix = UnaryNodeCell(this, "~"){UFix()};
  override def unary_!(): UFix = UnaryNodeCell(this, "!"){UFix()};
  override def andR(): Bool    = ReductionNodeCell(this, "&"){UFix()};
  override def orR():  Bool    = ReductionNodeCell(this, "|"){UFix()};
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
