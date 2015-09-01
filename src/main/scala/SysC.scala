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


class SysCBackend extends CppBackend {
   override def elaborate(c: Module): Unit = {
      super.elaborate(c)
      println(c)
      println(c.name)

      //Create component definition for System C
    val top_bundle = c.io.asInstanceOf[Bundle] //Is this safe?
    //  No, but it will throw an exception that should explain the issue reasonably
    val cdef = new ComponentDef(c.name + "_t", c.name)
    val badElements = scala.collection.mutable.HashMap[String, Data]()
    for ((name, elt) <- top_bundle.elements) {
      elt match {
        case delt:DecoupledIO[_] => {
          delt.bits match {
            case bits: Bits => {
              val is_input = bits.dir == INPUT
              val vtype = "sc_uint<" + bits.width + ">"; var tcast = ""
			  if (is_input) { tcast = ".to_uint64()" }
			  else { tcast = ".to_ulong()" }
			  val vcast = tcast
              val entry = new CEntry(name, is_input, vtype, vcast, bits.width, bits.name, delt.ready.name, delt.valid.name)
              cdef.entries += (entry)
			  cdef.valid_ready = true
            }
            case aggregate: Aggregate => {
              // Collect all the inputs and outputs.
              val inputs = aggregate.flatten.filter(_._2.dir == INPUT)
              if (inputs.length > 0) {
			    for (in <- inputs) {
				  var ttype = ""; var tcast = ""
				   in._2 match {
					case inUBool: Bool => ttype = "bool"; tcast = ""
					case inSInt: SInt => ttype = "sc_int<" + in._2.width + ">";  tcast = ".to_uint64()"
					//Bits is implemented as UInt
					case inUInt: UInt => ttype = "sc_uint<" + in._2.width + ">"; tcast = ".to_uint64()"
				  }
				  val vtype = ttype; val vcast = tcast
                  val entry = new CEntry(name, true, vtype, vcast, in._2.width, in._2.name, "ready", "valid")
                  cdef.entries += (entry)
				}
              }
              val outputs = aggregate.flatten.filter(_._2.dir == OUTPUT)
              if (outputs.length > 0) {
			    for (out <- outputs) {
				  var ttype = ""; var tcast = ""
				   out._2 match {
					case outUBool: Bool => ttype = "bool"; tcast = ".to_ulong()"
					case outSInt: SInt => ttype = "sc_int<" + out._2.width + ">";  tcast = ".to_ulong()"
					//Bits is implemented as UInt
					case outUInt: UInt => ttype = "sc_uint<" + out._2.width + ">"; tcast = ".to_ulong()"
				  }
				  val vtype = ttype; val vcast = tcast
                  val entry = new CEntry(name, false, vtype, vcast, out._2.width, out._2.name, "ready", "valid")
                  cdef.entries += (entry)
				}
              }
			  cdef.valid_ready = true
            }
            case _ => badElements(name) = elt
          }
        }
        case bool: Bool => {
          val is_input = bool.dir == INPUT
          val vtype = "bool"; var tcast = ""
		  if (is_input) { tcast = "" }
		  else { tcast = ".to_ulong()" }
		  val vcast = tcast
          val entry = new CEntry(name, is_input, vtype, vcast, bool.width, bool.name, "ready", "valid")
          cdef.entries += (entry)
		  cdef.valid_ready = false
        }
        case sint: SInt => {
          val is_input = sint.dir == INPUT
          val vtype = "sc_int<" + sint.width + ">"; var tcast = ""
		  if (is_input) { tcast = ".to_uint64()" }
		  else { tcast = ".to_ulong()" }
		  val vcast = tcast
          val entry = new CEntry(name, is_input, vtype, vcast, sint.width, sint.name, "ready", "valid")
          cdef.entries += (entry)
		  cdef.valid_ready = false
        }
		//Bits is implemented as UInt
        case uint: UInt => {
          val is_input = uint.dir == INPUT
          val vtype = "sc_uint<" + uint.width + ">"; var tcast = ""
		  if (is_input) { tcast = ".to_uint64()" }
		  else { tcast = ".to_ulong()" }
		  val vcast = tcast
          val entry = new CEntry(name, is_input, vtype, vcast, uint.width, uint.name, "ready", "valid")
          cdef.entries += (entry)
		  cdef.valid_ready = false
        }
        case _ => badElements(name) = elt
      }
    }

    if (badElements.size > 0) {
      val invalidIOMessage = "SystemC requires that all top-level wires are decoupled bits - <%s>"
      for ((name, elt) <- badElements) {
        // If we have a line number for the element, use it.
        if (elt.line != null) {
          ChiselError.error(invalidIOMessage.format(name), elt.line)
        } else {
          ChiselError.error(invalidIOMessage.format(name))
        }
      }
    } else {

      //Print out the component definition.
      println(cdef)

      //Generate SCWrapped files
      val out_h = createOutputFile("SCWrapped" + c.name + ".h");
      val template_h = "template_h.txt"
      SCWrapper.genwrapper(cdef, out_h, template_h)
      val out_cpp = createOutputFile("SCWrapped" + c.name + ".cpp");
      val template_cpp = "template_cpp.txt"
      SCWrapper.genwrapper(cdef, out_cpp, template_cpp)
    }
  }
}
