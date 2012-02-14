package Chisel {

import IOdir._;
import Node._;
import ChiselError._

abstract class Num extends Bits {
  type T <: Num;
  def <<(b: Num): Num = {ChiselErrors += TypeError("<<", this.getClass.toString, b.getClass.toString); this}
  def >>(b: Num): Num = {ChiselErrors += TypeError(">>", this.getClass.toString, b.getClass.toString); this}
  // def >>>(b: Num): Num = {ChiselErrors += TypeError(">>>", this.getClass.toString, b.getClass.toString); this}
  def +(b: Num): Num = {ChiselErrors += TypeError("+", this.getClass.toString, b.getClass.toString); this}
  def *(b: Num): Num = {ChiselErrors += TypeError("*", this.getClass.toString, b.getClass.toString); this}
  def ^(b: Num): Num = {ChiselErrors += TypeError("^", this.getClass.toString, b.getClass.toString); this}
  def ?(b: Num): Num = {ChiselErrors += TypeError("?", this.getClass.toString, b.getClass.toString); this}
  def -(b: Num): Num = {ChiselErrors += TypeError("-", this.getClass.toString, b.getClass.toString); this}
  def ##(b: Num): Num = {ChiselErrors += TypeError("##", this.getClass.toString, b.getClass.toString); this}
  def ===(b: Num): Bool = {ChiselErrors += TypeError("===", this.getClass.toString, b.getClass.toString); Bool(false)};
  def !=(b: Num): Bool = {ChiselErrors += TypeError("!=", this.getClass.toString, b.getClass.toString); Bool(false)};
  def >(b: Num): Bool = {ChiselErrors += TypeError(">", this.getClass.toString, b.getClass.toString); Bool(false)};
  def <=(b: Num): Bool = {ChiselErrors += TypeError("<=", this.getClass.toString, b.getClass.toString); Bool(false)};
  def >=(b: Num): Bool = {ChiselErrors += TypeError(">=", this.getClass.toString, b.getClass.toString); Bool(false)};
  def &&(b: Num): Bool = {ChiselErrors += TypeError("&&", this.getClass.toString, b.getClass.toString); Bool(false)};
  def ||(b: Num): Bool = {ChiselErrors += TypeError("||", this.getClass.toString, b.getClass.toString); Bool(false)};
  def &(b: Num): Num = {ChiselErrors += TypeError("&", this.getClass.toString, b.getClass.toString); this}
  def |(b: Num): Num = {ChiselErrors += TypeError("|", this.getClass.toString, b.getClass.toString); this}
}
}

