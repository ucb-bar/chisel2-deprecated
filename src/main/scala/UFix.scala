package Chisel {

import IOdir._;
import Node._;
import ChiselError._;

object UFix {
  
  def apply(x: Int): UFix = Lit(x){UFix()};
  def apply(x: Int, width: Int): UFix = Lit(x, width){UFix()};
  
  def apply(width: Int = -1, dir: IODirection = null): UFix = {
    val res = new UFix();
    res.dir = dir;
    if(width > 0)
      res.init("", width);
    else 
      res.init("", widthOf(0))
    res
  }
  
  def apply(dir: IODirection): UFix = UFix(-1, dir) 
  
  def apply(): UFix = UFix(-1, null);
}

class UFix extends Num {
  type T = UFix;
  override def fromNode(n: Node) = {
    val res = UFix(OUTPUT).asInstanceOf[this.type];
    res assign n;
    res
  }

  override def :=(src: UFix) = {
    if(comp != null)
      comp procAssign src.toNode;
    else
      this.asInstanceOf[Wire] procAssign src.toNode;
  }

  // override def :== (src: UFix) = {
  //   if (comp == null)
  //     this.asInstanceOf[IO] assign src
  //   else
  //     comp assign src.toNode
  // }

  // override def :==[T <: Data](src: T): Unit = {
  //   src match {
  //     case ufix: UFix => {
  //       this :== ufix;
  //     }
  //     case any => 
  //       ChiselErrors += IllegalState(":== not defined on " + this.getClass + " and " + src.getClass, 1)
  //   }
  // }

  override def :=[T <: Data](src: T): Unit = {
    src match {
      case ufix: UFix => {
	this := ufix;
      }
      case any => 
	ChiselErrors += IllegalState(":= not defined on " + this.getClass + " and " + src.getClass, 1)
    }
  }
  
  override def apply(bit: Int): UFix = { Extract(this, bit){UFix()}};
  override def apply(hi: Int, lo: Int): UFix = {Extract(this, hi, lo){UFix()}};
  override def apply(bit: UFix): UFix = {Extract(this, bit){UFix()}};
  override def apply(hi: UFix, lo: UFix): UFix = {Extract(this, hi, lo, -1){UFix()}};
  override def apply(range: (Int, Int)): UFix = this(range._1, range._2);

  override def unary_-(): UFix = UnaryNodeCell(this, "-"){UFix()};
  override def unary_~(): UFix = UnaryNodeCell(this, "~"){UFix()};
  def unary_!(): UFix = UnaryNodeCell(this, "!"){UFix()};
  override def andR(): Bool    = ReductionNodeCell(this, "&"){UFix()};
  override def orR():  Bool    = ReductionNodeCell(this, "|"){UFix()};
  override def << (b: UFix): UFix = BinaryNodeCell(this, b, "<<"){UFix()};
  override def >> (b: UFix): UFix = BinaryNodeCell(this, b, ">>"){UFix()};
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
  def &  (b: UFix): UFix = BinaryNodeCell(this, b, "&"){UFix()};
  def |  (b: UFix): UFix = BinaryNodeCell(this, b, "|"){UFix()};

  //UFix op Fix arithmetic
  def *   (b: Fix): Fix = BinaryNodeCell(this, b, "u*s"){Fix()}.toFix;
}

}
