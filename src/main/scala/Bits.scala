/*
 Copyright (c) 2011, 2012, 2013 The Regents of the University of
 California (Regents). All Rights Reserved.  Redistribution and use in
 source and binary forms, with or without modification, are permitted
 provided that the following conditions are met:

    * Redistributions of source code must retain the above
      copyright notice, this list of conditions and the following
      two paragraphs of disclaimer.
    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      two paragraphs of disclaimer in the documentation and/or other materials
      provided with the distribution.
    * Neither the name of the Regents nor the names of its contributors
      may be used to endorse or promote products derived from this
      software without specific prior written permission.

 IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
 SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS,
 ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
 REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

 REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT
 LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 A PARTICULAR PURPOSE. THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF
 ANY, PROVIDED HEREUNDER IS PROVIDED "AS IS". REGENTS HAS NO OBLIGATION
 TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
 MODIFICATIONS.
*/

package Chisel
import Node._
import Bits._
import ChiselError._
import Component._

object Bits {
  def conv(x: Bits): Bool = {
    if(x.getWidth > 1) {
      throw new Exception("multi bit signal " + x + " converted to Bool");
    }
    if(x.getWidth == -1) {
      throw new Exception("unable to automatically convert " + x + " to Bool, convert manually instead")
    }
    x.toBool
  }

  def apply(x: Int): Bits = Lit(x){Bits()};
  def apply(x: Int, width: Int): Bits = Lit(x, width){Bits()};
  def apply(x: String): Bits = Lit(x){Bits()};
  def apply(x: String, width: Int): Bits = Lit(x, width){Bits()};

  def apply(dir: IODirection = null, width: Int = -1): Bits = {
    val res = new Bits();
    res.dir = dir;
    if(width > 0) {
      res.init("", width);
    } else {
      res.init("", widthOf(0))
    }
    res
  }
}

class Bits extends Data with proc {
  ioMap += ((this, ioCount));
  ioCount += 1;

  var dir: IODirection = null;

  override def isIo = dir != null;

  override def toNode = this;
  override def fromNode(n: Node) = {
    val res = Bits(OUTPUT).asInstanceOf[this.type];
    res assign n;
    res
  }
  def default: Node = if (inputs.length < 1 || inputs(0) == null) null else inputs(0);

  override def litOf: Literal = {
    if(inputs.length == 1 && inputs(0) != null) {
      inputs(0).litOf
    } else {
      null
    }
  }

  // internal, non user exposed connectors
  var assigned = false;


  override def assign(src: Node) = {
    if(assigned || inputs.length > 0) {
      ChiselError.error({"reassignment to Wire " + this + " with inputs " + this.inputs(0) + " RHS: " + src});
    } else {
      assigned = true; super.assign(src)
    }
  }

  def procAssign(src: Node) = {
    if (assigned) {
      ChiselError.error("reassignment to Node");
    } else {
      updates += ((genCond(), src))
    }
  }

  //code generation stuff

  override def apply(name: String): Data = this

  override def flatten = Array((name, this));

  override def toString: String = {
    if (dir == INPUT) {
      "INPUT(" + name + (if (component == null) "" else ("." + component)) + ")";
    } else if (dir == OUTPUT) {
      "OUTPUT(" + name + (if (component == null) "" else ("." + component)) + ")";
    } else {
      "BITS(" + name + ", " + super.toString + ")"
    }
  }


  override def flip(): this.type = {
    assert(dir != null, println("Can't flip something that doesn't have a direction"))
    if (dir == INPUT) {
      dir = OUTPUT
    } else if(dir == OUTPUT) {
      dir = INPUT
    }
    this
  }

  override def asInput(): this.type = {
    dir = INPUT
    this
  }

  override def asOutput(): this.type = {
    dir = OUTPUT
    this
  }

  override def isDirectionless: Boolean = {
    return dir == null
  }

  override def <>(src: Node) = {
    if (dir == INPUT) {
      src match {
      case other: Bits =>
          if (other.dir == OUTPUT) { // input - output connections
          if(this.staticComp == other.staticComp && !isTypeNode) {//passthrough
            other assign this
          } else if (this.staticComp.parent == other.staticComp.parent || isTypeNode) { //producer - consumer
            if(other.inputs.length > 0 || other.updates.length > 0 ) {
              this assign other // only do assignment if output has stuff connected to it
            }
          } else {
            ChiselError.error({"Undefined connections between " + this + " and " + other})
          }
        } else if (other.dir == INPUT) { // input <> input conections
            if(this.staticComp == other.staticComp.parent) { // parent <> child
              other assign this
            } else if(this.staticComp.parent == other.staticComp) { //child <> parent
              this assign other
            } else {
              ChiselError.error({"Can't connect Input " + this + " Input " + other})
            }
          } else { // io <> wire
            if(this.staticComp == other.staticComp) { //internal wire
              other assign this
            } else if(this.staticComp.parent == other.staticComp) { //external wire
              this assign other
            } else {
            ChiselError.error({"Connecting Input " + this + " to " + other})
            }
        }
      case default =>
        ChiselError.error({"Connecting Input " + this + " to IO without direction " + default})
      }
    } else if (dir == OUTPUT) {
      src match {
        case other: Bits  =>
          if (other.dir == INPUT) { // input - output connections
            if (this.staticComp == other.staticComp && !isTypeNode) { //passthrough
              this assign other;
            } else if (this.staticComp.parent == other.staticComp.parent || isTypeNode) { //producer - consumer
              if(this.inputs.length > 0 || this.updates.length > 0) {
                other assign this; // only do connection if I have stuff connected to me
              }
            } else {
              ChiselError.error({"Undefined connection between " + this + " and " + other})
            }
          } else if (other.dir == OUTPUT) { // output <> output connections
            if(this.staticComp == other.staticComp.parent) { // parent <> child
              if(other.inputs.length > 0 || other.updates.length > 0) {
                this assign other // only do connection if child is assigning to that output
              }
            } else if (this.staticComp.parent == other.staticComp) { // child <> parent
              if(this.inputs.length > 0 || this.updates.length > 0) {
                other assign this // only do connection if child (me) is assinging that output
              }
            } else if (this.isTypeNode && other.isTypeNode) { //connecting two type nodes together
              ChiselError.error("Ambiguous Connection of Two Nodes")
            } else if (this.isTypeNode){ // type <> output
              other assign this;
            } else if (other.isTypeNode){ // output <> type
              this assign other;
            } else {
              ChiselError.error({"Connecting Output " + this + " to Output " + other})
            }
          } else { // io <> wire
            if(this.staticComp == other.staticComp) { //output <> wire
              this assign other
            } else if(this.staticComp.parent == other.staticComp) {
              ChiselError.error({"Connecting Ouptut " + this + " to an external wire " + other})
            } else {
              ChiselError.error({"Connecting Output " + this + " to IO without direction " + other})
            }
          }
        case default =>
          ChiselError.error({"Connecting Output " + this + " to an IO withouth direction " + default})
      }
    }
    else {
      src match {
        case other: Bits =>
          if (other.dir == INPUT) { // wire <> input
            if(this.staticComp == other.staticComp) {
              this assign other
            } else if(this.staticComp == other.staticComp.parent) {
              other assign this
            } else {
              ChiselError.error({"Undefined connection between wire " + this + " and input " + other})
            }
          } else if (other.dir == OUTPUT) { //wire <> output
            if(this.staticComp == other.staticComp) { // internal wire
              other assign this
            } else if(this.staticComp == other.staticComp.parent) { // external wire
              this assign other
            } else {
              ChiselError.error({"Undefined connection between wire " + this + " and output " + other})
            }
          } else {
            this assign other
          }
        case default =>
          ChiselError.error({"Undefined connection between " + this + " and " + default})
      }
    }
  }

  override def setIsClkInput = {isClkInput = true; this assign clk;}

  override def clone = {
    val res = this.getClass.newInstance.asInstanceOf[this.type];
    res.inferWidth = this.inferWidth
    res.width_ = this.width_;
    res.dir = this.dir;
    res
  }

  override def maxNum: BigInt = {
    if (inputs.length == 0) {
      width;
    } else if (inputs(0).isLit) {
      inputs(0).value
    } else if (inputs(0).litOf != null) {
      inputs(0).litOf.value
    } else if (inputs.length == 1 && inputs(0) != null) {
      inputs(0).maxNum
    } else {
      super.maxNum
    }
  }

  override def forceMatchingWidths {
    if(inputs.length == 1 && inputs(0).width != width) {
      inputs(0) = inputs(0).matchWidth(width)
    }
  }

  def generateError(src: Bits) {
    val myClass = this.getClass;
    val srcClass = src.getClass;
    if(myClass != classOf[Bits] && myClass == srcClass) {
      ChiselError.error(":= not defined on " + myClass.toString + " " + classOf[Bits].toString)
    } else if(myClass != classOf[Bits]) {
      ChiselError.error(":= not defined on " + myClass.toString + " " + srcClass.toString)
    }
  }

  private def colonEqual(src: Bits) = {
    generateError(src);
    if(comp != null){
      comp procAssign src.toNode;
    } else {
      this procAssign src.toNode;
    }
  }

  def := (src: Bool) = colonEqual(src);
  def := (src: Fix)  = colonEqual(src);
  def := (src: UFix) = colonEqual(src);

  override def :=[T <: Data](src: T): Unit = {
    src match {
      case bool: Bool => {
        this := bool;
      }
      case fix: Fix => {
        this := fix;
      }
      case ufix: UFix => {
        this := ufix
      }
      case bits: Bits => {
        this colonEqual(bits);
      }
      case any =>
        ChiselError.error(":= not defined on " + this.getClass + " and " + src.getClass);
    }
  }

  def apply(bit: Int): Bits = { Extract(this, bit){Bits()}};
  def apply(hi: Int, lo: Int): Bits = {Extract(this, hi, lo){Bits()}};
  def apply(bit: UFix): Bits = Extract(this, bit){Bits()};
  def apply(hi: UFix, lo: UFix): Bits = Extract(this, hi, lo, -1){Bits()};
  def apply(range: (Int, Int)): Bits = this(range._1, range._2);

  def unary_-(): Bits = UnaryOp(this, "-"){Bits()};
  def unary_~(): Bits = UnaryOp(this, "~"){Bits()};
  def andR(): Bool    = ReductionOp(this, "&"){Bits()};
  def orR():  Bool    = ReductionOp(this, "|"){Bits()};
  def xorR():  Bool   = ReductionOp(this, "^"){Bits()};
  def ===(b: Bits): Bool = LogicalOp(this, b, "==="){Bits()};
  def != (b: Bits): Bool = LogicalOp(this, b, "!="){Bits()};
  def << (b: UFix): Bits = BinaryOp(this, b.toBits, "<<"){Bits()};
  def >> (b: UFix): Bits = BinaryOp(this, b.toBits, ">>"){Bits()};
  def &  (b: Bits): Bits = BinaryOp(this, b, "&"){Bits()};
  def |  (b: Bits): Bits = BinaryOp(this, b, "|"){Bits()};
  def ^  (b: Bits): Bits = BinaryOp(this, b, "^"){Bits()};
  def ## (b: Bits): Bits = BinaryOp(this, b, "##"){Bits()};

  def && (b: Bool): Bool = conv(this) && b;
  def || (b: Bool): Bool = conv(this) || b;

}
