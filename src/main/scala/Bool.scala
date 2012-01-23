package Chisel {

import IOdir._;
import Node._;
import ChiselError._;

object Bool {
  def apply(x: Boolean) = Lit(x);
  
  def apply(dir: IODirection = null): Bool = {
    val res = new Bool();
    res.dir = dir;
    res.init("", 1)
    res
  }
  
  def apply(): Bool = Bool(null);
}

class Bool extends Bits {
  override def fromNode(n: Node) = {
    val res = Bool(OUTPUT).asInstanceOf[this.type];
    res assign n;
    res
  }

  
  override def := (src: Bool) = {
    if (comp == null)
      this.asInstanceOf[IO] assign src
    else
      comp assign src.toNode;
  }

  override def := (src: Bits) = {
    val res = src.toBool;
    if(src.getWidth > 1)
      ChiselErrors += IllegalState("multi bit signal " + src + " converted to Bool",1);
    if(src.getWidth == -1)
      ChiselErrors += IllegalState("unable to automatically convert " + src + " to Bool, convert manually instead",1);
    this := res;
  }

  override def <==(src: Bool) = {
    if(comp != null)
      comp procAssign src.toNode;
    else
      this.asInstanceOf[Wire] procAssign src.toNode;
  }

  override def <== (src: Bits) = {
    val res = src.toBool;
    if(src.getWidth > 1)
      ChiselErrors += IllegalState("multi bit signal " + src + " converted to Bool",1);
    if(src.getWidth == -1)
      ChiselErrors += IllegalState("unable to automatically convert " + src + " to Bool, convert manually instead",1);
    this <== res;
  }

  def generateError = {
    ChiselErrors += IllegalState("Cannot perform extraction on a Bool", 4);
  }

  override def apply(bit: Int): Bool = { generateError; this};
  override def apply(hi: Int, lo: Int): Bool = {generateError; this};
  override def apply(bit: UFix): Bool = {generateError; this};
  override def apply(hi: UFix, lo: UFix): Bool = {generateError; this};

  override def unary_-(): Bool = UnaryBoolCell(this, "-");
  override def unary_~(): Bool = UnaryBoolCell(this, "~");
  def unary_!(): Bool = UnaryBoolCell(this, "!");
  def ^  (b: Bool): Bool = BinaryBoolCell(this, b, "^");
  def ===(b: Bool): Bool = BinaryBoolCell(this, b, "===");
  def != (b: Bool): Bool = BinaryBoolCell(this, b, "!=");
  def && (b: Bool): Bool = if (b.isTrue) this else BinaryBoolCell(this, b, "&&");
  def || (b: Bool): Bool = BinaryBoolCell(this, b, "||");
  def &  (b: Bool): Bool = BinaryBoolCell(this, b, "&");
  def |  (b: Bool): Bool = BinaryBoolCell(this, b, "|");

  def isTrue: Boolean = {
    if(inputs.length == 0) return false
    inputs(0) match {
      case l: Literal => {l.isLit && l.value == 1};
      case any        => false;
    }
  }
}
}

