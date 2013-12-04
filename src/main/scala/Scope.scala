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

import scala.collection.mutable.Stack
import scala.collection.mutable.ArrayBuffer

/** The programming model to construct a circuit is equivalent
  to a recursive descent parser from a top ``Module``.

  As such, scopes are pushed and popped along the way.
  */
class Scope {

  /** defines the Scope of conditions set through *when* calls.
    Each tuple contains the condition typed condition node and
    a boolean that indicates if the condition is part of
    an otherwise clause. */
  val conds = new Stack[(Bool, Boolean)]();

  val compStack = new Stack[Module]();
  var stackIndent = 0;
  val printStackStruct = new ArrayBuffer[(Int, Module)]();

  def genCond(): Node = conds.top._1.node;

  def topCond: Bool = conds.top._1

  /** Returns the module currently in scope.

    Backends create Node outside a scope yet we still initialize a component
    field in ``Node`` constructor so we need to special case the empty stack
    here.
    */
  def topModule(): Module = if(compStack.length > 0) compStack.top else {
    //XXXthrow new RuntimeException("module stack is empty")
    null
  }

  /** Returns true if the condition can be used as a default value. */
  def isDefaultCond(): Boolean = {
    /* true is the conditional at the top of the stack is bound
     to an 'always true' variable and false otherwise. */
    genCond() match {
      case lit: Literal => lit.value == 1
      case _ =>
        /* We only have Bool(true) and an otherwise condition on the stack. */
        conds.length == 2 && conds.top._2
    }
  }

  /** defines the Scope of Bits set by *switch* call and used for *is* */
  val keys = new Stack[Bits]();

  val clocks = new Stack[Clock]();

  val resets = new Stack[Bool]();

  def clock: Clock = clocks.top

  def reset: Bool = resets.top

  /** Returns the first element that was pushed on the clocks stack.
    */
  def implicitClock: Clock = clocks.head

  /** Returns the first element that was pushed on the resets stack.
    */
  def implicitReset: Bool = resets.head

  def initImplicits() {
    if( conds.isEmpty ) conds.push((Bool(true), true))
    if( resets.isEmpty ) {
      val reset = Bool(INPUT)
      reset.node.isIo = true
      reset.node.nameIt("reset")
      resets.push(reset)
    }
    if( clocks.isEmpty ) {
      clocks.push((new Clock(this.reset)).nameIt("clk"))
    }
  }

  def push(c: Module) {
    if( !Module.trigger ) {
      ChiselError.error(
        c.getClass.getName + " was not properly wrapped into a module() call.")
    }
    Module.trigger = false
    compStack.push(c);
    printStackStruct += ((stackIndent, c));
    stackIndent += 1;

    /* Initialized here because the constructor for a ``Node``
     requires an initialized module stack. */
    initImplicits()

   /* Clock and/or reset were not specified so we use
     the one currently in scope.
    This code needs to be executed after the implicit
    clock and reset were initialized of course. */
    if( c.clock == null ) c.clock = this.clock
    if( c.reset == null ) c.reset = this.reset
  }

  def pop() {
    val c = compStack.pop;
    if( !compStack.isEmpty ) {
      val dad = compStack.top;
      c.parent = dad;
      dad.children += c;
    }
    stackIndent -= 1;
    c.level = 0;
    for(child <- c.children) {
      c.level = math.max(c.level, child.level + 1);
    }
  }
}
