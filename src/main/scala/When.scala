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

import ChiselError._

/** Attach *elsewhen* and *otherwise* statements to a *when* such that
  it is possible to write the following scala code:

  when( signal ) {
  ...
  } elsewhen ( signal2 ) {
  ...
  } otherwise {
  ...
  }

  Implementation Note: class and object names need to match case-wise.
*/
class when (prevCond: Bool) {
  def elsewhen (cond: Bool)(block: => Unit): when = {
    when.execWhen(!prevCond && cond){ block }
    new when(prevCond || cond);
  }
  def otherwise (block: => Unit) {
    Module.scope.conds.push((Module.scope.topCond && !prevCond, true));
    block;
    Module.scope.conds.pop();
  }
}

/** uses conds from the scope */
object when {
  def execWhen(cond: Bool)(block: => Unit) {
    Module.scope.conds.push((Module.scope.topCond && cond, false));
    block;
    Module.scope.conds.pop();
  }

  /** Perform the statements in *block* on a positive clock edge
    if the condition *cond* is true, treating the left hand assignments
    as synchronous.

    This is similar to how Verilog uses always @ (posedge clk) to specify
    synchronous logic.
  */
  def apply(cond: Bool)(block: => Unit): when = {
    execWhen(cond){ block }
    new when(cond);
  }
}


/** Perform the statements in *block* on a positive clock edge
    if the condition *cond* is false, treating the left hand assignments
    as synchronous.

    This is similar to how Verilog uses always @ (posedge clk) to specify
    synchronous logic.
*/
object unless {
  def apply(cond: Bool)(block: => Unit) {
    when (!cond) { block }
  }
}

/**
    switch(op) {
      is(add_op) { rc := ra + rb }
      is(imm_op) { rc := (rai << UInt(8)) | rbi }
    }

  Uses keys from the scope.
*/
object switch {
  def apply(c: Bits)(block: => Unit) {
    Module.scope.keys.push(c);
    block;
    Module.scope.keys.pop();
  }
}

object is {
  def apply(v: Bits)(block: => Unit) {
    if (Module.scope.keys.length == 0) {
      ChiselError.error("using 'is(bits)' outside a 'switch(bits)' statement");
    } else {
      val c = Module.scope.keys(0) === v;
      when (c) { block }
    }
  }
}
