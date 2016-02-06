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

package Chisel

import scala.collection.mutable.{ArrayBuffer, HashMap}
import java.io._

object SCWrapper {
  type ReplacementMap = HashMap[String, String]

  def main(args: Array[String]) {
    //Read in template
    val fileContents = read_resource("template.txt")

    //Generate replacements
    val ecdef = example_component_def2()
    val replacements = generate_replacements(ecdef)

    //Fill template and write out
    val filled = fill_template(fileContents, replacements)
    write_file("generated/generated.cpp", filled)

    //Debug
    System.out.println(filled) //DEBUG
  }

  def example_component_def(): ComponentDef = {
    val cdef = new ComponentDef("GCD_t", "GCD")
    cdef.entries += new CEntry("a", true, "dat_t<1>", "GCD__io_a", "GCD__io_r1", "GCD__io_v1")
    cdef.entries += new CEntry("z", false, "dat_t<1>", "GCD__io_z", "GCD__io_rz", "GCD__io_vz")
    cdef
  }

  def example_component_def2(): ComponentDef = {
    val cdef = new ComponentDef("AddFilter_t", "AddFilter")
    cdef.entries += new CEntry("a", true, "dat_t<16>", "AddFilter__io_a", "AddFilter__io_ar", "AddFilter__io_av")
    cdef.entries += new CEntry("b", false, "dat_t<16>", "AddFilter__io_b", "AddFilter__io_br", "AddFilter__io_bv")
    cdef
  }

  def genwrapper(c: ComponentDef, filename:  String) {
    //Read in template
    val template = read_resource("template.txt")

    //Generate replacements
    val replacements = generate_replacements(c)

    //Fill template and write out
    val filled = fill_template(template, replacements)
    write_file(filename, filled)

    //Debug
    System.out.println(filled)
  }

  def genwrapper(c: ComponentDef, filewriter: java.io.FileWriter){
    //Read in template
    val template = read_resource("template.txt")

    //Generate replacements
    val replacements = generate_replacements(c)

    //Fill template and write out
    val filled = fill_template(template, replacements)
    write_file(filewriter, filled)

    //Debug
    // System.out.println(filled)
  }

  def generate_replacements(c: ComponentDef): ReplacementMap = {
    val replacements = new ReplacementMap()

    //Header file
    replacements += (("header_file", c.name + ".h"))

    //Component name and type
    replacements += (("name", "SCWrapped" + c.name))
    replacements += (("component_type", c.ctype))

    //I/O Fifos
    /*begin*/{
      var input_fifos = ""
      var output_fifos = ""
      for( e <- c.entries) {
        val decl = "sc_fifo<%s >* %s;\n  ".format(e.ctype, e.name)
        if(e.is_input) {
          input_fifos += decl
        } else {
          output_fifos += decl
        }
      }
      replacements += (("input_fifos", input_fifos))
      replacements += (("output_fifos", output_fifos))
    }

    /*Initialize output fifos*/{
      //Pull out output fifos
      val fifos = ArrayBuffer[CEntry]();
      for(e <- c.entries) {
        if(!e.is_input) {
          fifos += e;
        }
      }
      //Initialize
      var init = "";
      for( i <- 0 until fifos.size) {
        init += "%s = new sc_fifo<%s >(1);\n  ".format(fifos(i).name, fifos(i).ctype)
      }
      replacements += (("init_output_fifos", init))
    }

    /*Check input queues*/{
      //Pull out input fifos
      val dvar = ArrayBuffer[String]()
      val fvar = ArrayBuffer[String]()
      val fifos = ArrayBuffer[CEntry]()
      for( e <- c.entries) {
        if(e.is_input) {
          dvar += genvar("dat")
          fvar += genvar("filled")
          fifos += e
        }
      }
      //Initialize
      var init = ""
      var fill = ""
      var check = ""
      for( i <- 0 until fifos.size) {
        val ctype = fifos(i).ctype
        val data = dvar(i)
        val filled = fvar(i)
        val in = fifos(i).name
        val in_data = fifos(i).data
        val ready = fifos(i).ready
        val valid = fifos(i).valid
        init += "%s %s;\n    ".format(ctype, data)
        init += "int %s = 0;\n    ".format(filled)
        fill += "if(!%s){%s = %s->nb_read(%s);}\n      "format(filled, filled, in, data)
        // Is this a structured data-type?
        if (ctype == in_data) {
          // Unpack and distribute the inputs.
          for((name, bits) <- c.structs(ctype).fields) {
            fill += "c->%s = %s.%s;\n      "format(name, data, name)
          }
        } else {
          fill += "c->%s = %s;\n      "format(in_data, data)
        }
        fill += "c->%s = LIT<1>(%s);\n      "format(valid, filled)
        check += "if(c->%s.values[0]) %s = 0;\n      "format(ready, filled)
      }
      replacements += (("input_buffers", init))
      replacements += (("fill_input", fill))
      replacements += (("check_input", check))
    }

    /*Check Output Queues*/{
      //Pull out output fifos
      val fifos = ArrayBuffer[CEntry]()
          for (e <- c.entries) {
            if(!e.is_input) {
              fifos += e
            }
          }
      //Check
      var check = ""
      var valid_output = "";
      for(i <- 0 until fifos.size) {
        val ctype = fifos(i).ctype
        val valid = fifos(i).valid
        val data = fifos(i).data
        val ready = fifos(i).ready
        val out = fifos(i).name
        check += "c->%s = LIT<1>(%s->num_free() > 0);\n      "format(ready, out)
        // Is this a structured data-type?
        if (ctype == data) {
          // Pack and distribute the inputs.
          val indent = "      "
          valid_output += "if(c->%s.values[0]) {\n%s"format(valid, indent)
          valid_output += "  %s dato;\n%s".format(ctype, indent)
          for((name, bits) <- c.structs(ctype).fields) {
            valid_output += "  dato.%s = c->%s;\n%s"format(name, name, indent)
          }
          valid_output += "  %s->nb_write(dato);\n%s}\n    "format(out, indent)
        } else {
          valid_output += "if(c->%s.values[0]) %s->nb_write(c->%s);\n    "format(valid, out, data)
        }
      }
      replacements += (("check_output", check))
      replacements += (("valid_output", valid_output))
    }

    // If we have structured FIFO elements, we need to generate the struct definitions
    //  and the ostream "<<" definition to keep SystemC happy.
    val ostream_lsh = ArrayBuffer[String]()
    for((name, struct) <- c.structs) {
      ostream_lsh += struct.toString + "inline ostream& operator << (ostream& os, const %s& arg){  return os; }\n".format(name)
    }
    replacements += (("ostream_lsh", ostream_lsh.mkString("\n")))
    replacements
  }

  private var unique_counter: Int = 0
  private def genvar(prefix:  String):  String = {
    val c = unique_counter
    unique_counter += 1
    prefix + c;
  }

  def read_file(filename: String): String = {
    val buffer = new StringBuilder
    try {
      val reader = new BufferedReader(new FileReader(filename))
      var line = ""
      while({line = reader.readLine(); line != null}) {
        buffer.append(line)
        buffer.append("\n")
      }
      reader.close()
    } catch {
      case e: IOException => {
        System.err.println("Error reading file " + filename)
        System.exit(-1)
      }
    }
    buffer.toString()
  }

  def read_resource(resourcename:  String):  String = {
    val resourcestreamReader = new InputStreamReader(getClass().getResourceAsStream("/" + resourcename))
    val buffer = new StringBuilder
    try {
      val reader = new BufferedReader(resourcestreamReader)
      var line = ""
      while({line = reader.readLine(); line != null}) {
        buffer.append(line + "\n");
      }
      reader.close()
    } catch {
      case e: IOException => {
        System.err.println("Error reading resource " + resourcename)
        System.exit(-1)
      }
    }
    buffer.toString()
  }

  def write_file(filename: String, file: String) {
    try {
      val writer = new BufferedWriter(new FileWriter(filename))
          writer.write(file)
          writer.close()
    } catch {
      case e: IOException => {
        System.err.println("Error writing file " + filename)
        System.exit(-1)
      }
    }
  }

  def write_file(filewriter: java.io.FileWriter, file:  String) {
    try {
      val writer = new BufferedWriter(filewriter)
          writer.write(file)
          writer.close()
    } catch {
      case e:IOException => {
        System.err.println("Error writing file " + filewriter)
        System.exit(-1)
      }
    }
  }

  def fill_template(template: String, replacements: ReplacementMap): String = {
    var expansion = template
    for( (key,value) <- replacements) {
      val regex = "\\{\\!" + key + "\\!\\}"
      expansion = regex.r.replaceAllIn(expansion, value)
    }
    expansion
  }
}

class CEntry(a_name: String, input: Boolean, a_type: String, a_data: String, a_ready: String, a_valid: String) {
   val name = a_name
   val is_input = input
   val ctype = a_type
   val data = a_data
   val ready = a_ready
   val valid = a_valid

   override def toString(): String = {
     name + " " +
     is_input + " " +
     ctype + " " +
     data + " " +
     ready + " " +
     valid
   }
}

class ComponentDef(a_type: String, a_name: String) {
  val ctype: String = a_type
  val name: String = a_name
  val entries = ArrayBuffer[CEntry]()
  val structs = scala.collection.mutable.LinkedHashMap[String, CStruct]()

  override def toString(): String = {
    var accum: String = ":["
    for(e <- entries){
      accum += e + ", "
    }
    accum += "]"
    "Component " + ctype + " " + name + accum
  }
}

case class CStruct(val name: String, val fields: Array[(String, Bits)]) {
   override def toString(): String = {
     "struct " + name +
     " {\n" +
     fields.map { case (name, bits) => "  dat_t<" + bits.width + "> " + name + ";" }.mkString("\n") +
     "\n};\n"
   }
}
