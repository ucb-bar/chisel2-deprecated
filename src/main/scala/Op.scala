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

/** Base class for nodes that appear as operators in the generated graph.
  */
abstract class Op extends Node {
  override def slug = "und"
  val opInfix = "und"
}

/** Marks operations whose inputs width need to match in sizes.
  */
trait SymetricOpand {
}


/** Base class for nodes that appear as unary operators in the generated graph.
  */
abstract class UnaryOp(opandNode: Node) extends Op {

  this.inputs.append(opandNode)

  val opPrefix = "und"
  def opand: Node = this.inputs(0)
}


/* XXX Prevent Verilog syntax errors when indexing constants
 which we do on masks.
 */
class WrapOp(opand: Node) extends UnaryOp(opand) {

  inferWidth = new WidthOf(0)

  override def slug = ""
  override val opPrefix = ""
}


/** Bitwise reverse
*/
class BitwiseRevOp(opand: Node) extends UnaryOp(opand) {

  inferWidth = new WidthOf(0)

  override def slug = "not"
  override val opPrefix = "~"
}


/** Logical Negation
*/
class LogicalNegOp(opand: Node) extends UnaryOp(opand) {

  inferWidth = new FixedWidth(1)

  override def slug = "not"
  override val opPrefix = "!"
}


/** Sign reversal for integers
*/
class SignRevOp(opand: Node) extends UnaryOp(opand) {
  inferWidth = new WidthOf(0)

  override def slug = "neg"
  override val opPrefix = "-"
}


/** Base class for binary operators
  */
abstract class BinaryOp(leftNode: Node, rightNode: Node) extends Op {

  this.inputs.append(leftNode)
  this.inputs.append(rightNode)

  def left: Node = inputs(0)
  def right: Node = inputs(1)
}

class LeftShiftOp(left: Node, right: Node) extends BinaryOp(left, right) {
  inferWidth = new lshWidthOf(0, right)

  override def slug = "<<"
}


class RightShiftOp(left: Node, right: Node) extends BinaryOp(left, right) {

  inferWidth = new rshWidthOf(0, right)

  override def slug = ">>"
}


class RightShiftSOp(left: Node, right: Node)
    extends RightShiftOp(left, right) {
}


class AddOp(left: Node, right: Node) extends BinaryOp(left, right)
    with SymetricOpand {

  inferWidth = new maxWidth()

  override def slug = "add"
  override val opInfix = "+"
}


class AndOp(left: Node, right: Node) extends BinaryOp(left, right)
    with SymetricOpand {

  inferWidth = new maxWidth()

  override def slug = "and"
  override val opInfix = "&"
}


class ExtractOp(opandN: Node, hiBit: Node, loBit: Node) extends Op {

  this.inputs.append(opandN)
  this.inputs.append(hiBit)
  this.inputs.append(loBit)

  inferWidth = if( hi.isInstanceOf[Literal] && lo.isInstanceOf[Literal] ) new FixedWidth(hi.asInstanceOf[Literal].value.toInt - lo.asInstanceOf[Literal].value.toInt + 1) else new WidthOf(0)

  def opand: Node = this.inputs(0)

  def hi: Node = this.inputs(1)

  def lo: Node = this.inputs(2)

  override def slug = "extract"

  override def toString: String =
    (inputs(0) + "(" + hi + (if (hi == lo) "" else (", " + hi)) + ")")
}


class FillOp(opand: Node, val n: Int) extends UnaryOp(opand) {

  inferWidth = new WidthOf(0)

  override def slug = "fill"

  override def toString: String = name + "(" + inputs(0) + ", " + n + ")";
}


class DivOp(left: Node, right: Node) extends BinaryOp(left, right) {

  inferWidth = new WidthOf(0)

  override def slug = "div"
  override val opInfix = "/"
}


class DivSOp(left: Node, right: Node) extends DivOp(left, right) {
  override def slug = "divs"
}


class DivSUOp(left: Node, right: Node) extends DivOp(left, right) {
  override def slug = "divsu"
}


class DivUSOp(left: Node, right: Node) extends DivOp(left, right) {
  inferWidth = new WidthOf(0, -1)

  override def slug = "divus"
}


class Log2Op(opand: Node, val nbits: Int) extends UnaryOp(opand) {
  inferWidth = new FixedWidth(nbits)

  override def slug = "log2"
}


class MulOp(left: Node, right: Node) extends BinaryOp(left, right) {
  inferWidth = new SumWidth()

  override def slug = "mul"
  override val opInfix = "*"
}


class MulSOp(left: Node, right: Node) extends MulOp(left, right) {
  override def slug = "muls"
}


class MulSUOp(left: Node, right: Node) extends MulOp(left, right) {
  inferWidth = new SumWidth(-1)

  override def slug = "mulsu"
}

/** XXX elseNode null because of regEnable */
class MuxOp(condNode: Node, thenNodeP: Node, elseNode: Node = null ) extends Op {
  override def slug = "mux";

  inferWidth = new maxWidth()
  inputs.append(condNode)
  inputs.append(thenNodeP)
  if( elseNode != null ) inputs.append(elseNode)

  def cond: Node = inputs(0)
  def thenNode: Node = inputs(1)
  def otherwise: Node = if( inputs.length > 2 ) inputs(2) else null

  def ::(a: Node): MuxOp = { inputs(2) = a; this }


  /* Returns an enable signal for a Mux tree. */
  def enable(): Node = {
    val thenEnable = thenNode match {
      case mux: MuxOp =>
        new LogicalOrOp(cond, mux.enable)
      case _ => cond
    }

    otherwise match {
      case mux: MuxOp =>
        new LogicalOrOp(thenEnable, mux.enable)
      case _ => thenEnable
    }
  }

}


class RemOp(left: Node, right: Node) extends BinaryOp(left, right) {

  inferWidth = new minWidth()

  override def slug = "rem"
  override val opInfix = "%"
}


class RemSOp(left: Node, right: Node) extends RemOp(left, right) {
  override def slug = "rems"
}


class RemSUOp(left: Node, right: Node) extends RemOp(left, right) {

  inferWidth = new RemWidthOf(0, 1)

  override def slug = "remsu"
}


class RemUSOp(left: Node, right: Node) extends RemOp(left, right) {

  inferWidth = new RemWidthOf(1, 0)

  override def slug = "remus"
}


class OrOp(left: Node, right: Node) extends BinaryOp(left, right)
    with SymetricOpand {

  inferWidth = new maxWidth()

  override def slug = "or"
  override val opInfix = "|"
}


/** Substraction operator
  */
class SubOp(left: Node, right: Node) extends BinaryOp(left, right)
    with SymetricOpand {

  inferWidth = new maxWidth()

  override def slug = "sub"
  override val opInfix = "-"
}


class XorOp(left: Node, right: Node) extends BinaryOp(left, right)
    with SymetricOpand {

  inferWidth = new maxWidth()

  override def slug = "xor"
  override val opInfix = "^"
}


/** Bus concatenation operator
  */
class CatOp(left: Node, right: Node) extends BinaryOp(left, right) {

  inferWidth = new SumWidth()

  override def slug = "cat"
  override val opInfix = "##"
}


class LogicalOp(left: Node, right: Node) extends BinaryOp(left, right)
    with SymetricOpand {

  inferWidth = new maxToFixedWidth(1)
}

class EqlOp(left: Node, right: Node) extends LogicalOp(left, right) {
  override def slug = "eq"
  override val opInfix = "=="
}


class NeqOp(left: Node, right: Node) extends LogicalOp(left, right) {
  override def slug = "neq"
  override val opInfix = "!="
}


class GteOp(left: Node, right: Node) extends LogicalOp(left, right) {
  override def slug = "gte"
  override val opInfix = ">="
}


class GteSOp(left: Node, right: Node) extends GteOp(left, right) {
  override def slug = "gtes"
}


class GtrOp(left: Node, right: Node) extends LogicalOp(left, right) {
  override def slug = "gt"
  override val opInfix = ">"
}


class GtrSOp(left: Node, right: Node) extends GtrOp(left, right) {
  override def slug = "gts"
}


class LtnOp(left: Node, right: Node) extends LogicalOp(left, right) {
  override def slug = "lt"
  override val opInfix = "<"
}


class LtnSOp(left: Node, right: Node) extends LtnOp(left, right) {
  override def slug = "lts"
}


class LteOp(left: Node, right: Node) extends LogicalOp(left, right) {
  override def slug = "lte"
  override val opInfix = "<="
}


class LteSOp(left: Node, right: Node) extends LteOp(left, right) {
  override def slug = "ltes"
}

class LogicalAndOp(left: Node, right: Node) extends  LogicalOp(left, right) {
  override def slug = "andl"
  override val opInfix = "&&"
}

class LogicalOrOp(left: Node, right: Node) extends  LogicalOp(left, right) {
  override def slug = "orl"
  override val opInfix = "||"
}


class ReduceOp(opand: Node) extends UnaryOp(opand) {
  inferWidth = new FixedWidth(1)
}

class ReduceAndOp(opand: Node) extends ReduceOp(opand) {
  override def slug = "andR"
}


class ReduceOrOp(opand: Node) extends ReduceOp(opand) {
  override def slug = "orR"
}


class ReduceXorOp(opand: Node) extends ReduceOp(opand) {
  override def slug = "xorR"
}


// XXX      case "?"   => Multiplex(x, y, null);


