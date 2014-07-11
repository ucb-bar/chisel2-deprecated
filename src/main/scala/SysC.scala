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
import scala.collection.mutable.ArrayBuffer
import scala.math._
import java.io.InputStream
import java.io.OutputStream
import java.io.PrintStream
import scala.sys.process._
import sys.process.stringSeqToProcess
import Node._
import Reg._
import ChiselError._
import Literal._
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap

class SysCBackend extends CppBackend {
   override def elaborate(c: Module): Unit = {
      super.elaborate(c)
      println(c)
      println(c.name)

      //Create component definition for System C
      val top_bundle = c.io.asInstanceOf[Bundle] //Is this safe?      
      val cdef = new ComponentDef()
      cdef.ctype = c.name + "_t"
      cdef.name = c.name
      for ((name, elt) <- top_bundle.elements) {
         elt match {
            case delt:DecoupledIO[Bits] =>
               val is_input = delt.bits.dir == INPUT
               val vtype = "dat_t<" + delt.bits.width + ">"
               val entry = new CEntry(name, is_input, vtype, delt.bits.name, delt.ready.name, delt.valid.name)
               cdef.entries.add(entry)
            case _ =>
               throw new RuntimeException("SystemC requires that all top-level wires are decoupled!")
         }
      }

      //Print out component definition
      println(cdef)

      //Generate file
      val filename = "generated/SCWrapped" + c.name + ".cpp"
      SCWrapper.genwrapper(cdef, filename)
   }
}

