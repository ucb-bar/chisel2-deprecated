/*
 Copyright (c) 2011, 2012, 2013, 2014 The Regents of the University of
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

/* backward compatibility */
object Bits {
  def apply(x: Int): UInt = UInt(x)
  def apply(x: Int, width: Int): UInt = UInt(x, width)
  def apply(x: BigInt): UInt = UInt(x)
  def apply(x: BigInt, width: Int): UInt = UInt(x, width)
  def apply(x: String): UInt = UInt(x)
  def apply(x: String, width: Int): UInt = UInt(x, width)
  def apply(dir: IODirection = NODIR, width: Int = -1): UInt = UInt(dir, width)
  def DC(width: Int): BitPat = BitPat.DC(width)
}


/** Base class for built-in Chisel types Bits and SInt. */
abstract class Bits extends Data with proc {
  var dir: IODirection = NODIR

  def create(dir: IODirection, width: Int) {
    this.dir = dir
    if(width > -1) {
      init("", width)
    } else {
      init("", Node.widthOf(0))
    }
  }

  /** assigns this instance as the data type for *node*.
    */
  protected[Chisel] final def asTypeFor(node: Node): this.type = {
    this assign node ; setIsTypeNode ; this
  }

  def fromInt(x: Int): this.type
  def toSInt(): SInt = chiselCast(this){SInt()}
  def toUInt(): UInt = chiselCast(this){UInt()}
  override def getNode: Node = if (procAssigned) this else super.getNode

  // internal, non user exposed connectors
  private var assigned = false
  private def checkAssign(src: Node) = {
    dir match {
      case INPUT if component == Module.current && (component.wires.unzip._2 contains this) =>
        ChiselError.error("assigning to your own input port " + this + " RHS: " + src)
      case OUTPUT if component != Module.current && src.component != null && src.component.parent != component =>
        ChiselError.error("assigning to a non parent module's output port: " + this + " RHS: " + src)
      case _ if assigned =>
        ChiselError.error("reassignment to Wire " + this + " with inputs " + inputs(0) + " RHS: " + src)
      case _ =>
    }
    !assigned
  }

  override def assign(src: Node): Unit = {
    if (Driver.topComponent != None || checkAssign(src)) {
      assigned = true
      if (!procAssigned) inputs += src
      else if (defaultMissing) setDefault(src)
    }
  }

  override def procAssign(src: Node): Unit = {
    if (Driver.topComponent != None || checkAssign(src)) {
      if (defaultMissing && Module.current.whenCond.canBeUsedAsDefault)
        setDefault(src)
      else
        super.procAssign(src)
    }
  }

  override def defaultRequired: Boolean = true

  //code generation stuff

  override def apply(name: String): Data = this

  override def flatten: Array[(String, Bits)] = Array((name, this))

  override def toString: String = {
    // XXX We cannot print the width here as it would computed the infered
    // width, hence change the computations. It might be possible to print
    // width_ but it seems to also have some underlying computations associated
    // to it.
    var str = (
      "/*" + (if (name != "") name else "?")
        + (if (component != null) (" in " + component) else "") + "*/ "
        + getClass.getName + "(" + dir + ", " + "width=" + width_
        + ", connect to " + inputs.length + " inputs: (")
    var sep = ""
    for( i <- inputs ) {
      str = (str + sep + (if (i.name != "") i.name else "?")
        + "[" + i.getClass.getName + "]"
        + " in " + (if (i.component != null) i.component.getClass.getName else "?"))
      sep = ", "
    }
    str = str + "))"
    str
  }

  override def flip: this.type = {
    dir match {
      case INPUT => dir = OUTPUT
      case OUTPUT => dir = INPUT
      case NODIR => throwException("Can't flip something that doesn't have a direction")
    }
    this
  }

  override def asDirectionless: this.type = { dir = NODIR ; this }
  override def asInput: this.type = { dir = INPUT ; this }
  override def asOutput: this.type = { dir = OUTPUT ; this }
  override def isDirectionless: Boolean = { dir == NODIR }

  override def <>(src: Node) {
    checkCompatibility(src)
    val p = component
    val q = src.component
    src match {
      case other: Bits => (dir, other.dir) match {
        // input <> output connections
        case (INPUT, OUTPUT) if p == q && !isTypeNode => other assign this //passthrough
        case (INPUT, OUTPUT) if p.parent == q.parent || isTypeNode => q match { //producer - consumer
          // includes when a child is a blackbox
          case _: BlackBox => this assign other 
          // only do assignment if output has stuff connected to it
          case _ if !other.inputs.isEmpty => this assign other 
          case _ => 
        }
        case (INPUT, OUTPUT) =>
          ChiselError.error("Undefined connections between " + this + " and " + other)

        // output <> input connections
        case (OUTPUT, INPUT) if p == q && !isTypeNode => this assign other //passthrough
        case (OUTPUT, INPUT) if p.parent == q.parent || isTypeNode => q match { //producer - consumer
          // includes when a child is a black box
          case _: BlackBox => other assign this
          // only do connection if I have stuff connected to me
          case _ if !inputs.isEmpty => other assign this
          case _ =>
        }
        case (OUTPUT, INPUT) => 
            ChiselError.error("Undefined connection between " + this + " and " + other)

        // input <> input conections
        case (INPUT, INPUT) if p == q.parent => other assign this // parent <> child
        case (INPUT, INPUT) if p.parent == q => this assign other //child <> parent
        case (INPUT, INPUT) => 
          ChiselError.error("Can't connect Input " + this + " Input " + other)

        // output <> output connections
        case (OUTPUT, OUTPUT) if p == q.parent => q match { // parent <> child
          // includes when a child is a black box
          case _: BlackBox => this assign other
          // only do connection if child is assigning to that output
          case _ if !other.inputs.isEmpty => this assign other 
          case _ =>
        }
        case (OUTPUT, OUTPUT) if p.parent == q => p match { // child <> parent
          // includes when a child is a black box
          case _: BlackBox => other assign this
          // only do connection if child (me) is assinging that output
          case _ if !inputs.isEmpty => other assign this 
          case _ =>
        }
        case (OUTPUT, OUTPUT) if isTypeNode && other.isTypeNode => //connecting two type nodes together
          ChiselError.error("Ambiguous Connection of Two Nodes")
        case (OUTPUT, OUTPUT) if isTypeNode => other assign this // type <> output
        case (OUTPUT, OUTPUT) if other.isTypeNode => this assign other // output <> type
        case (OUTPUT, OUTPUT) =>
          ChiselError.error("Connecting Output " + this + " to Output " + other)

        // input <> wire
        case (INPUT, NODIR) if p == q => other assign this //internal wire
        case (INPUT, NODIR) if p.parent == q => this assign other //external wire
        case (INPUT, NODIR) =>
          ChiselError.error("Connecting Input " + this + " to " + other)

        // wire <> input
        case (NODIR, INPUT) if p == q => this assign other
        case (NODIR, INPUT) if p == q.parent => other assign this
        case (NODIR, INPUT) =>
          ChiselError.error("Undefined connection between wire " + this + " and input " + other)

        // io <> wire
        case (OUTPUT, NODIR) if p == q => this assign other
        case (OUTPUT, NODIR) if p.parent == q =>
          ChiselError.error("Connecting Ouptut " + this + " to an external wire " + other)
        case (OUTPUT, NODIR) =>
          ChiselError.error("Connecting Output " + this + " to IO without direction " + other)

        // wire <> output
        case (NODIR, OUTPUT) if p == q => other assign this // internal wire
        case (NODIR, OUTPUT) if p == q.parent => this assign other // external wire
        case (NODIR, OUTPUT) =>
          ChiselError.error("Undefined connection between wire " + this + " and output " + other)

        // wire <> wire
        case (NODIR, NODIR) => this assign other

        case _ => // shouldn't reach here
      }
      case _ => dir match {
        case INPUT =>
          ChiselError.error("Connecting Input " + this + " to IO without direction " + src)
        case OUTPUT =>
          ChiselError.error("Connecting Output " + this + " to an IO withouth direction " + src)
        case NODIR =>        
          ChiselError.error("Undefined connection between " + this + " and " + src)
      }
    }
  }

  override def cloneType: this.type = {
    val res = getClass.newInstance.asInstanceOf[this.type];
    res.inferWidth = inferWidth
    res.width_ = width_.clone()
    res.dir = dir;
    res
  }

  override def forceMatchingWidths {
    if(inputs.length == 1 && inputs(0).widthW != widthW) {
      // Our single child differs.
      val w = widthW
      val w0 = inputs(0).widthW
      if (w.isKnown) {
        inputs(0) = inputs(0).matchWidth(w)
      } else if (w0.isKnown) {
        matchWidth(w0)
      }
    }
  }

  override def matchWidth(w: Width): Node = {
    if (w.isKnown && isLit && !litOf.isZ) {
      val wi = w.needWidth()   // TODO 0WW
      Literal(litOf.value & ((BigInt(1) << wi)-1), wi)
    }
    else super.matchWidth(w)
  }

  // Operators
  protected final def newUnaryOp(opName: String): this.type = {
    fromNode(UnaryOp(this, opName))
  }

  protected final def newBinaryOp(right: Bits, opName: String): this.type = {
    fromNode(BinaryOp(this, right, opName))
  }

  protected final def newLogicalOp(right: Bits, opName: String): Bool = {
    Bool(OUTPUT).asTypeFor(LogicalOp(this, right, opName))
  }

  protected final def newReductionOp(opName: String): Bool = {
    Bool(OUTPUT).asTypeFor(ReductionOp(this, opName))
  }

  /** Assignment operator */
  override protected def colonEquals(that: Bits) {
    checkCompatibility(that)
    (comp match { case None => this case Some(p) => p}) procAssign that
  }

  // bitwise operators
  // =================

  /** Extract a single Bool at index *bit*.
    */
  final def apply(bit: Int): Bool = apply(UInt(bit))
  final def apply(bit: UInt): Bool = {
    val res = Extract(this, bit){Bool()}
    res.comp = Some(new Insert(this, bit, 1))
    res
  }

  /** Extract a range of bits */
  final def apply(hi: Int, lo: Int): UInt = {
    val res = Extract(this, hi, lo){UInt()}
    res.comp = Some(new Insert(this, UInt(lo), hi - lo + 1))
    res
  }
  final def apply(hi: UInt, lo: UInt): UInt = {Extract(this, hi, lo, -1){UInt()}};
  final def apply(range: (Int, Int)): UInt = this(range._1, range._2);

  def unary_~(): this.type   = newUnaryOp("~");
  def andR(): Bool           = newLogicalOp(SInt(-1), "===")
  def orR(): Bool            = newLogicalOp(UInt(0), "!=")
  def xorR(): Bool           = newReductionOp("^");
  def != (b: Bits): Bool     = newLogicalOp(b, "!=");
  def & (b: Bits): this.type = newBinaryOp(b, "&");
  def | (b: Bits): this.type = newBinaryOp(b, "|");
  def ^ (b: Bits): this.type = newBinaryOp(b, "^");
  def <<(b: UInt): this.type = newBinaryOp(b, "<<")

  def bitSet(off: UInt, dat: UInt): this.type = {
    val bit = UInt(1, 1) << off
    this & ~bit | dat.toSInt & bit
  }

  def toBools: Vec[Bool] = Vec.tabulate(this.getWidth)(i => this(i))

  def error(b: Bits): Bits = {
    throw new Exception("+ not defined on " + this.getClass + " and " + b.getClass)
    this
  }

  def + (b: Bits): Bits = {
    this match {
      case u0: UInt => {
        b match {
          case u1: UInt => u0 + u1
          case f1: SInt => u0 + f1
          case _ => error(b)
        }
      }
      case f0: SInt => {
        b match {
          case u1: UInt => f0 + u1
          case f1: SInt => f0 + f1
          case _ => error(b)
        }
      }
      case _ => error(b)
    }
  }
  def - (b: Bits): Bits = {
    this match {
      case u0: UInt => {
        b match {
          case u1: UInt => u0 - u1
          case f1: SInt => u0 - f1
          case _ => error(b)
        }
      }
      case f0: SInt => {
        b match {
          case u1: UInt => f0 - u1
          case f1: SInt => f0 - f1
          case _ => error(b)
        }
      }
      case _ => error(b)
    }
  }
  def * (b: Bits): Bits = {
    this match {
      case u0: UInt => {
        b match {
          case u1: UInt => u0 * u1
          case f1: SInt => u0 * f1
          case _ => error(b)
        }
      }
      case f0: SInt => {
        b match {
          case u1: UInt => f0 * u1
          case f1: SInt => f0 * f1
          case _ => error(b)
        }
      }
      case _ => error(b)
    }
  }
  def % (b: Bits): Bits = {
    this match {
      case u0: UInt => {
        b match {
          case u1: UInt => u0 % u1
          case f1: SInt => u0 % f1
          case _ => error(b)
        }
      }
      case f0: SInt => {
        b match {
          case u1: UInt => f0 % u1
          case f1: SInt => f0 % f1
          case _ => error(b)
        }
      }
      case _ => error(b)
    }
  }
  def / (b: Bits): Bits = {
    this match {
      case u0: UInt => {
        b match {
          case u1: UInt => u0 / u1
          case f1: SInt => u0 / f1
          case _ => error(b)
        }
      }
      case f0: SInt => {
        b match {
          case u1: UInt => f0 / u1
          case f1: SInt => f0 / f1
          case _ => error(b)
        }
      }
      case _ => error(b)
    }
  }

  override def ===[T <: Data](right: T): Bool = {
    right match {
      case b: Bits => newLogicalOp(b, "===");
      case _ =>
        this.asInstanceOf[Data] === right
    }
  }

  override def ##[T <: Data](right: T): this.type = {
    right match {
      case b: Bits => newBinaryOp(b, "##");
      case _ =>
        (this.asInstanceOf[Data] ## right).asInstanceOf[this.type];
    }
  }

  // Chisel3 - Check version compatibility (assignments requiring Wire() wrappers)
  private def checkCompatibility(src: Node) {
    if (Driver.minimumCompatibility > "2") {
      component addAssignment this
    }
  }
}


object andR {
    def apply(x: Bits): Bool = x.andR
}

object orR {
    def apply(x: Bits): Bool = x.orR
}

object xorR {
    def apply(x: Bits): Bool = x.xorR
}
