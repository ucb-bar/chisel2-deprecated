/*
 Copyright (c) 2011 - 2016 The Regents of the University of
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

package Chisel.testers
import Chisel._

// In Chisel2, the BasicTester has an IO interface which it uses to communicate with the Chisel2 Tester Scala code.
class BasicTester extends Module {

  val io = new Bundle {
    val running       = Bool(INPUT)
    val error         = Bool(OUTPUT)
    val pc            = UInt(OUTPUT, 32)
    val done          = Bool(OUTPUT)
  }

  val setDone = Reg(init = Bool(false))
  val setError = Reg(init = Bool(false))

  def popCount(n: Long): Int = n.toBinaryString.count(_=='1')

  /** Ends the test reporting success.
    *
    * Does not fire when in reset (defined as the encapsulating Module's
    * reset). If your definition of reset is not the encapsulating Module's
    * reset, you will need to gate this externally.
    */
  def stop() {
    when (!reset) {
      setDone := Bool(true)
      printf("STOP %d\n", io.done)
    }
  }

  def error(message: String = "") {
    setError := Bool(true)
    printf("ERROR: %s\n".format(message))
    stop()
  }

  def finish(): Unit = {}
}
