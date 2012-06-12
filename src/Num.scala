package Chisel
import Node._
import ChiselError._

abstract class Num extends Bits {
  type T <: Num;
  def <<(b: Num): Num = {ChiselErrors += ChiselError("<< not defined on " + this.getClass.toString + " " + b.getClass.toString, Thread.currentThread().getStackTrace); this}
  def >>(b: Num): Num = {ChiselErrors += ChiselError(">> not defined on " + this.getClass.toString + " " + b.getClass.toString, Thread.currentThread().getStackTrace); this}
  // def >>>(b: Num): Num = {ChiselErrors += ChiselError(">>> not defined on " + this.getClass.toString + " " + b.getClass.toString, Thread.currentThread().getStackTrace); this}
  def +(b: Num): Num = {ChiselErrors += ChiselError("+ not defined on " + this.getClass.toString + " " + b.getClass.toString, Thread.currentThread().getStackTrace); this}
  def *(b: Num): Num = {ChiselErrors += ChiselError("* not defined on " + this.getClass.toString + " " + b.getClass.toString, Thread.currentThread().getStackTrace); this}
  def ^(b: Num): Num = {ChiselErrors += ChiselError("^ not defined on " + this.getClass.toString + " " + b.getClass.toString, Thread.currentThread().getStackTrace); this}
  def ?(b: Num): Num = {ChiselErrors += ChiselError("? not defined on " + this.getClass.toString + " " + b.getClass.toString, Thread.currentThread().getStackTrace); this}
  def -(b: Num): Num = {ChiselErrors += ChiselError("- not defined on " + this.getClass.toString + " " + b.getClass.toString, Thread.currentThread().getStackTrace); this}
  def ##(b: Num): Num = {ChiselErrors += ChiselError("## not defined on " + this.getClass.toString + " " + b.getClass.toString, Thread.currentThread().getStackTrace); this}
  def ===(b: Num): Bool = {ChiselErrors += ChiselError("=== not defined on " + this.getClass.toString + " " + b.getClass.toString, Thread.currentThread().getStackTrace); Bool(false)};
  def !=(b: Num): Bool = {ChiselErrors += ChiselError("!= not defined on " + this.getClass.toString + " " + b.getClass.toString, Thread.currentThread().getStackTrace); Bool(false)};
  def >(b: Num): Bool = {ChiselErrors += ChiselError("> not defined on " + this.getClass.toString + " " + b.getClass.toString, Thread.currentThread().getStackTrace); Bool(false)};
  def <=(b: Num): Bool = {ChiselErrors += ChiselError("<= not defined on " + this.getClass.toString + " " + b.getClass.toString, Thread.currentThread().getStackTrace); Bool(false)};
  def >=(b: Num): Bool = {ChiselErrors += ChiselError(">= not defined on " + this.getClass.toString + " " + b.getClass.toString, Thread.currentThread().getStackTrace); Bool(false)};
  def &&(b: Num): Bool = {ChiselErrors += ChiselError("&& not defined on " + this.getClass.toString + " " + b.getClass.toString, Thread.currentThread().getStackTrace); Bool(false)};
  def ||(b: Num): Bool = {ChiselErrors += ChiselError("|| not defined on " + this.getClass.toString + " " + b.getClass.toString, Thread.currentThread().getStackTrace); Bool(false)};
  def &(b: Num): Num = {ChiselErrors += ChiselError("& not defined on " + this.getClass.toString + " " + b.getClass.toString, Thread.currentThread().getStackTrace); this}
  def |(b: Num): Num = {ChiselErrors += ChiselError("| not defined on " + this.getClass.toString + " " + b.getClass.toString, Thread.currentThread().getStackTrace); this}
}
