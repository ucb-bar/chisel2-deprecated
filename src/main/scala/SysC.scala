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
              val vtype = "dat_t<" + bits.width + ">" // direct use of width here?
              val entry = new CEntry(name, is_input, vtype, bits.width, bits.name, delt.ready.name, delt.valid.name)
              cdef.entries += (entry)
            }
            case aggregate: Aggregate => {
              // Collect all the inputs and outputs.
              val inputs = aggregate.flatten.filter(_._2.dir == INPUT)
              if (inputs.length > 0) {
			    for (in <- inputs) { 
				  val vtype = "dat_t<" + in._2.width + ">" 
                  val entry = new CEntry(name, true, vtype, in._2.width, in._2.name, "ready", "valid")
                  cdef.entries += (entry)
				}
				//val aName = "cs_" + aggregate.name + "_i"
                //cdef.structs(aName)= new CStruct(aName, inputs)
                //val entry = new CEntry(name, true, aName, 1, aggregate.name, delt.ready.name, delt.valid.name)
              }
              val outputs = aggregate.flatten.filter(_._2.dir == OUTPUT)
              if (outputs.length > 0) {
			    for (out <- outputs) { 
				  val vtype = "dat_t<" + out._2.width + ">" 
                  val entry = new CEntry(name, false, vtype, out._2.width, out._2.name, "ready", "valid")
                  cdef.entries += (entry)
				}
                //val aName = "cs_" + aggregate.name + "_o"
                //cdef.structs(aName) = new CStruct(aName, outputs)
                //val entry = new CEntry(name, false, aName, 1, aggregate.name, delt.ready.name, delt.valid.name)
              }
            }
            case _ => badElements(name) = elt
          }
        }
        case bits: Bits => {
          val is_input = bits.dir == INPUT
          val vtype = "dat_t<" + bits.width + ">" // direct use of width here?
          val entry = new CEntry(name, is_input, vtype, bits.width, bits.name, "ready", "valid")
          cdef.entries += (entry)
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
