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

package Chisel.hwiotesters

import Chisel._

import scala.collection.mutable
import scala.util.matching.Regex

// scalastyle:off regex
// scalastyle:off method.name

/**
 * named access and type information about the IO bundle of a module
 * used for building testing harnesses
 */
class IOAccessor(val device_io_data: Data, verbose: Boolean = true) {
  val device_io: Bundle = device_io_data match {
    case b: Bundle => b
    case _ => {
      ChiselError.error("DUT io isn't a Bundle")
      new Bundle()
    }
  }
  val dut_inputs  = device_io.flatten.map( { case (name, port) => port } ).filter( _.dir == INPUT)
  val dut_outputs = device_io.flatten.map( { case (name, port) => port } ).filter( _.dir == OUTPUT)
  val ports_referenced = new mutable.HashSet[Data]

  val referenced_inputs          = new mutable.HashSet[Data]()
  val referenced_outputs         = new mutable.HashSet[Data]()

  val name_to_decoupled_port = new mutable.HashMap[String, DecoupledIO[Data]]()
  val name_to_valid_port     = new mutable.HashMap[String, ValidIO[Data]]()

  val port_to_name = {
    val port_to_name_accumulator = new mutable.HashMap[Data, String]()

    def checkDecoupledOrValid(port: Data, name: String): Unit = {
      port match {
        case decoupled_port : DecoupledIO[Data] =>
          name_to_decoupled_port(name) = decoupled_port
        case valid_port : ValidIO[Data] =>
          name_to_valid_port(name) = valid_port
        case _ =>
      }
    }

    def parseBundle(b: Bundle, name: String = ""): Unit = {
      for ((n, e) <- b.elements) {
        val new_name = name + (if(name.length > 0 ) "." else "" ) + n
        port_to_name_accumulator(e) = new_name

        e match {
          case bb: Bundle     => parseBundle(bb, new_name)
          case vv: Vec[_]  => parseVecs(vv, new_name)
          case ee: Bits    =>
          case _           =>
            throw new Exception(s"bad bundle member $new_name $e")
        }
        checkDecoupledOrValid(e, new_name)
      }
    }
    def parseVecs[T<:Data](b: Vec[T], name: String = ""): Unit = {
      for ((e, i) <- b.zipWithIndex) {
        val new_name = name + s"($i)"
        port_to_name_accumulator(e) = new_name

        e match {
          case bb: Bundle     => parseBundle(bb, new_name)
          case vv: Vec[_]  => parseVecs(vv, new_name)
          case ee: Bits    =>
          case _           =>
            throw new Exception(s"bad bundle member $new_name $e")
        }
        checkDecoupledOrValid(e, new_name)
      }
    }

    parseBundle(device_io)
    port_to_name_accumulator
  }
  val name_to_port = port_to_name.map(_.swap)

  //noinspection ScalaStyle
  def showPorts(pattern : Regex): Unit = {
    def orderPorts(a: Data, b: Data) : Boolean = {
      port_to_name(a) < port_to_name(b)
    }
    def showDecoupledCode(port_name:String): String = {
      if(name_to_decoupled_port.contains(port_name)) "D"
      else if(name_to_valid_port.contains(port_name)) "V"
      else if(findParentDecoupledPortName(port_name).nonEmpty) "D."
      else if(findParentValidPortName(port_name).nonEmpty) "V."
      else ""

    }
    def showDecoupledParent(port_name:String): String = {
      findParentDecoupledPortName(port_name) match {
        case Some(decoupled_name) => s"$decoupled_name"
        case _                    => findParentValidPortName(port_name).getOrElse("")
      }
    }
    def show_dir(dir: IODirection) = dir match {
      case INPUT  => "I"
      case OUTPUT => "O"
      case _      => "-"
    }

    println("=" * 80)
    println("Device under test: io bundle")
    println("%3s  %3s  %-4s  %4s   %-25s %s".format(
            "#", "Dir", "D/V", "Used", "Name", "Parent"
    ))
    println("-" * 80)

    for((port,index) <- port_to_name.keys.toList.sortWith(orderPorts).zipWithIndex) {
      val port_name = port_to_name(port)
      println("%3d  %3s   %-4s%4s    %-25s %s".format(
        index,
        show_dir(port.toBits.dir),
        showDecoupledCode(port_name),
        if(ports_referenced.contains(port)) "y" else "",
        port_name,
        showDecoupledParent(port_name)
      ))
    }
    if(verbose) {
      println("=" * 80)
    }
  }

  def findParentDecoupledPortName(name: String): Option[String] = {
    val possible_parents = name_to_decoupled_port.keys.toList.filter(s => name.startsWith(s))
    if(possible_parents.isEmpty) return None
    possible_parents.sorted.lastOption
  }
  def findParentValidPortName(name: String): Option[String] = {
    val possible_parents = name_to_valid_port.keys.toList.filter(s => name.startsWith(s))
    if(possible_parents.isEmpty) return None
    possible_parents.sorted.lastOption
  }

  def contains(port: Data) : Boolean = {
    ports_referenced.contains(port)
  }
}
