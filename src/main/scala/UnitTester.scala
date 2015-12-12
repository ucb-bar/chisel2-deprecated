/*
 Copyright (c) 2011 - 2015 The Regents of the University of
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

import Chisel._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Chisel2State {
  var args = Array[String]()
}

trait UnitTestRunners {
  def execute(t: => UnitTester)(implicit args: Array[String]): Boolean = {
    Chisel2State.args = args
    try {
      // Construct the combined circuit, containing all the required
      //  poke()'s and expect()'s as arrays of data.
      val mod = Driver(Chisel2State.args, () => Module(t), true)
      if (Driver.isTesting) {
        // Initialize a tester with tracing turned on.
        val c = new Tester(mod, true)
        // Run the testing circuit until we see io.done.
        while(c.peek(mod.io.done) == 0) {
          c.step(1)
        }
        val error = c.peek(mod.io.error)
        val pc = c.peek(mod.io.pc)
        if (error != 0)
          c.fail

	// Do an additional step to get any printf output.
        c.step(1)
        c.finish
      }
      true
    } catch {
      case _: Throwable => 
        false
    }
  }

  def elaborate(t: => Module): Module = {
    val removeArgs = Array("--compile", "--test", "--genHarness")
    val filteredArgs = Chisel2State.args.filterNot(removeArgs.contains(_))
    val mod = Driver(filteredArgs ++ Array("--compile", "--genHarness"), () => t, true)
    mod
  }
}

case class Step(input_map: mutable.HashMap[Data,Int], output_map: mutable.HashMap[Data,Int])

class UnitTester extends Module with UnitTestRunners {
  override val io = new Bundle {
    val running       = Bool(INPUT)
    val error         = Bool(OUTPUT)
    val pc            = UInt(OUTPUT)
    val done          = Bool(OUTPUT)
  }
  var max_width = Width(0)
  val set_input_op :: wait_for_op :: expect_op :: Nil = Enum(UInt(), 3)

  def port_name(dut: Module, port_to_find: Data) : String = {
    dut.wires.foreach { case (name, port) =>
        if( port == port_to_find) return name
    }
    port_to_find.toString
  }

  // Scala stuff
  val test_actions = new ArrayBuffer[Step]()
  step(1) // gives us a slot to put in our input and outputs from beginning

  def poke(io_port: Bits, value: Int): Unit = {
//    println(s"io_port $io_port, len ${test_actions.last.input_map.size} " +
//            s"ip_port.dir ${io_port.dir}")

    require(io_port.dir == INPUT, s"poke error: $io_port not an input")
//    require(test_actions.last.input_map.contains(io_port) == false,
//      s"second poke to $io_port without step\nkeys ${test_actions.last.input_map.keys.mkString(",")}")

    test_actions.last.input_map(io_port) = value
  }

  def expect(io_port: Bits, value: Int): Unit = {
    require(io_port.dir == OUTPUT, s"expect error: $io_port not an output")
    require(!test_actions.last.output_map.contains(io_port), s"second expect to $io_port without step")

//    println(s"io_port $io_port ip_port.dir ${io_port.dir}")
    test_actions.last.output_map(io_port) = value
  }

  def step(number_of_cycles: Int): Unit = {
    test_actions ++= Array.fill(number_of_cycles) {
      new Step(
        new mutable.HashMap[Data, Int](),
        new mutable.HashMap[Data, Int]()
      )
    }
  }

  def install[T <: Module](dut: T): Unit = {
    /**
     * connect to the device under test by connecting each of it's io ports to an appropriate register
     */
    val dut_inputs = dut.wires.flatMap { case (name, element) =>
      if(element.dir == INPUT) Some(element) else None
    }
    val dut_outputs = dut.wires.flatMap { case (name, element) =>
      if(element.dir == OUTPUT) Some(element) else None
    }

    /**
     * prints out a table form of input and expected outputs
     */
    println(
      "%6s".format("step") +
        dut_inputs.map { dut_input => "%8s".format(port_name(dut, dut_input))}.mkString +
        dut_outputs.map { dut_output => "%8s".format(port_name(dut, dut_output))}.mkString
    )
    def val_str(hash : mutable.HashMap[Data, Int], key: Data) : String = {
      if( hash.contains(key) ) hash(key).toString else "-"
    }
    test_actions.zipWithIndex.foreach { case (step, step_number) =>
      print("%6d".format(step_number))
      for(port <- dut_inputs) {
        print("%8s".format(val_str(step.input_map, port)))
      }
      for(port <- dut_outputs) {
        print("%8s".format(val_str(step.output_map, port)))
      }
      println()
    }
    io.done  := Bool(false)
    io.error := Bool(false)


    val pc             = Reg(init=UInt(0, 8))

    io.pc := pc

    dut_inputs.foreach { input_port =>
      var default_value = 0
      val input_values = Vec(
        test_actions.map { step =>
          default_value = step.input_map.getOrElse(input_port, default_value)
          UInt(default_value, input_port.width)
        }
      )
      input_port := input_values(pc)
    }

    dut_outputs.foreach { output_port =>
      val output_values = Vec(
        test_actions.map { step =>
          output_port.fromBits(UInt(step.output_map.getOrElse(output_port, 0)))
        }
      )
      val ok_to_test_output_values = Vec(
        test_actions.map { step =>
          Bool(step.output_map.contains(output_port))
        }
      )


//      when(ok_to_test_output_values(pc) && output_port === output_values(pc))) {
      when(ok_to_test_output_values(pc)) {
        when(output_port.toBits() =/= output_values(pc).toBits()) {
          io.error := Bool(true)
          io.done  := Bool(true)
          printf("Bad value: %x != %x\n", output_port.toBits(), output_values(pc).toBits())
        } otherwise {
          printf("Good value: %x == %x\n", output_port.toBits(), output_values(pc).toBits())
        }
      }
    }

    when(pc >= UInt(test_actions.length)) {
      io.done := Bool(true)
    } otherwise {
      pc := pc + UInt(1)
      printf("bump pc: 0x%x\n", pc)
    }

  }
}

object UnitTester {
  def apply[T <: Module](gen: () => T): T = {
    gen()
  }
}
