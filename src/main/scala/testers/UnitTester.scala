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

package Chisel.testers

import Chisel._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Chisel2State {
  var args = Array[String]()
}

trait UnitTestRunners {
  def execute(t: => UnitTester)(implicit optionArgs: Array[String]): Boolean = {
    Chisel2State.args = optionArgs
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
      case e: Throwable =>
        println(e)
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

/**
 * Use a UnitTester to constuct a test harness for a chisel module
 * this module will be canonically referred to as the device_under_test, often simply as c in
 * a unit test, and also dut
 * The UnitTester is used to put series of values (as chisel.Vec's) into the ports of the dut io which are INPUTs
 * At specified times it check the dut's io OUTPUT ports to see that they match a specific value
 * The vec's are assembled through the following API
 * poke, expect and step, pokes
 *
 * Example:
 *
 * class Adder(width:Int) extends Module {
 *   val io = new Bundle {
 *     val in0 : UInt(INPUT, width=width)
 *     val in1 : UInt(INPUT, width=width)
 *     val out : UInt(OUTPUT, width=width)
 *   }
 * class AdderTester extends UnitTester {
 *   val device_under_test = Module( new Adder(32) )
 *   val c = device_under_test
 *   poke(c.io.in0, 5); poke(c.io.in1, ); poke(
 *
 */
class UnitTester extends Module with UnitTestRunners {
  case class Step(input_map: mutable.HashMap[Data,BigInt], output_map: mutable.HashMap[Data,BigInt])

  val rnd = new scala.util.Random(Driver.testerSeed)

  def int(x: Bits):    BigInt = x.litValue()

  override val io = new Bundle {
    val running       = Bool(INPUT)
    val error         = Bool(OUTPUT)
    val pc            = UInt(OUTPUT, 32)
    val done          = Bool(OUTPUT)
  }
  val setDone = Reg(init = Bool(false))
  val setError = Reg(init = Bool(false))

  var max_width = Width(0)
  val set_input_op :: wait_for_op :: expect_op :: Nil = Enum(UInt(), 3)

  val ports_referenced = new mutable.HashSet[Data]

  // Scala stuff
  val test_actions = new ArrayBuffer[Step]()
  step(1) // gives us a slot to put in our input and outputs from beginning

  def poke(io_port: Bits, value: BigInt): Unit = {
    require(io_port.dir == INPUT, s"poke error: $io_port not an input")
    require(test_actions.last.input_map.contains(io_port) == false,
      s"second poke to $io_port without step\nkeys ${test_actions.last.input_map.keys.mkString(",")}")

    ports_referenced += io_port
    test_actions.last.input_map(io_port) = value
  }
//  def poke(io_port: Data, bool_value: Boolean) = poke(io_port, if(bool_value) 1 else 0)

  def expect(io_port: Bits, value: BigInt): Unit = {
    require(io_port.dir == OUTPUT, s"expect error: $io_port not an output")
    require(!test_actions.last.output_map.contains(io_port), s"second expect to $io_port without step")

    ports_referenced += io_port
    test_actions.last.output_map(io_port) = value
  }
  def expect(io_port: Bits, bool_value: Boolean): Unit = expect(io_port, BigInt(if(bool_value) 1 else 0))

  def step(number_of_cycles: Int): Unit = {
    test_actions ++= Array.fill(number_of_cycles) {
      new Step(
        new mutable.HashMap[Data, BigInt](),
        new mutable.HashMap[Data, BigInt]()
      )
    }
  }

  def install[T <: Module](dut: T): Unit = {
    step(1) // Ensure we have at least one trailing unused action.
    /**
     * connect to the device under test by connecting each of it's io ports to an appropriate register
     */
    val dut_inputs = dut.wires.flatMap { case (name, element) =>
      if(element.dir == INPUT) Some(element) else None
    }
    val dut_outputs = dut.wires.flatMap { case (name, element) =>
      if(element.dir == OUTPUT) Some(element) else None
    }

    val port_to_name = {
      val port_to_name_accumulator = new mutable.HashMap[Data, String]()
      def port_name(dut: Module, port_to_find: Data) : String = {
        dut.wires.foreach { case (name, port) =>
            if( port == port_to_find) return name
        }
        port_to_find.toString
      }

      println("="*80)
      println("Device under test: io bundle")
      println("%10s %10s %s".format("direction", "referenced", "name"))
      println("-"*80)
      def parse_bundle(b: Bundle, name: String = ""): Unit = {
        for ((n, e) <- b.elements) {
          val new_name = name + (if(name.length > 0 ) "." else "" ) + n
          port_to_name_accumulator(e) = new_name
          println("%10s %5s      %s".format(e.toBits.dir, if( ports_referenced.contains(e)) "Y" else " ", new_name))

          e match {
            case bb: Bundle  => parse_bundle(bb, new_name)
            case vv: Vec[_]  => parse_vecs(vv, new_name)
            case ee: Bits => {}
            case _           => {
              throw new Exception(s"bad bundle member ${new_name} $e")
            }
          }
        }
      }
      def parse_vecs[T<:Data](b: Vec[T], name: String = ""): Unit = {
        for ((e, i) <- b.zipWithIndex) {
          val new_name = name + s"($i)"
          port_to_name_accumulator(e) = new_name
          println("%10s %5s      %s".format(e.toBits.dir, if( ports_referenced.contains(e)) "Y" else " ", new_name))

          e match {
            case bb: Bundle  => parse_bundle(bb, new_name)
            case vv: Vec[_]  => parse_vecs(vv, new_name)
            case ee: Bits => {}
            case _           => {
              throw new Exception(s"bad bundle member ${new_name} $e")
            }
          }
        }
      }

      dut.io match {
        case b: Bundle => parse_bundle(b)
        case x: Data => port_to_name_accumulator(x) = port_name(dut, x)
      }
      println("="*80)

      port_to_name_accumulator
    }
    /**
     *  Print a title for the testing state table
     */
    val max_col_width = 2 + (if (ports_referenced.isEmpty) {
      0
    } else {
      ports_referenced.map { port =>
        Array(port_to_name(port).length, port.getWidth / 4).max  // width/4 is how wide value might be in hex
      }.max
    })
    val (string_col_template, number_col_template) = (s"%${max_col_width}s", s"%${max_col_width}x")

    println("="*80)
    println("UnitTester state table")
    println(
      "%6s".format("step") +
        dut_inputs.map  { dut_input  => string_col_template.format(port_to_name(dut_input))}.mkString +
        dut_outputs.map { dut_output => string_col_template.format(port_to_name(dut_output))}.mkString
    )
    println("-"*80)
    /**
     * prints out a table form of input and expected outputs
     */
    def val_str(hash : mutable.HashMap[Data, BigInt], key: Data) : String = {
      if( hash.contains(key) ) "%x".format(hash(key)) else "-"
    }
    test_actions.zipWithIndex.foreach { case (step, step_number) =>
      print("%6d".format(step_number))
      for(port <- dut_inputs) {
        print(string_col_template.format(val_str(step.input_map, port)))
      }
      for(port <- dut_outputs) {
        print(string_col_template.format(val_str(step.output_map, port)))
      }
      println()
    }
    println("="*80)

    io.done  := setDone
    io.error := setError

    val pc             = Reg(init=UInt(0, log2Up(test_actions.length) + 1))

    io.pc := pc

    def log_referenced_ports: Unit = {
      val format_statement = new StringBuilder()
      val port_to_display  = new ArrayBuffer[Data]()
      format_statement.append("pc: %x")
      port_to_display.append(pc)

      for( dut_input <- dut_inputs ) {
        format_statement.append(",  " + port_to_name(dut_input)+": %x")
        port_to_display.append(dut_input)
      }
      for( dut_output <- dut_outputs ) {
        format_statement.append(",   " + port_to_name(dut_output)+": %x")
        port_to_display.append(dut_output)
      }
      printf(format_statement.toString() + "\n", port_to_display.map{_.toBits()}.toSeq :_* )
    }

    log_referenced_ports


    def create_vectors_for_input(input_port: Data): Unit = {
      var default_value = BigInt(0)
      val input_values = Vec(
        test_actions.map { step =>
          default_value = step.input_map.getOrElse(input_port, default_value)
          UInt(default_value, input_port.width)
        }
      )
      input_port := input_values(pc)
    }

    dut_inputs.foreach { port => create_vectors_for_input(port) }

    def create_vectors_and_tests_for_output(output_port: Data): Unit = {
      val output_values = Vec(
        test_actions.map { step =>
          output_port.fromBits(UInt(step.output_map.getOrElse(output_port, BigInt(0))))
        }
      )
      val ok_to_test_output_values = Vec(
        test_actions.map { step =>
          Bool(step.output_map.contains(output_port))
        }
      )

      when(ok_to_test_output_values(pc)) {
        when(output_port.toBits() === output_values(pc).toBits()) {
//          printf("    passed -- " + port_to_name(output_port) + ":  0x%x\n",
//            output_port.toBits()
//          )
        }.otherwise {
          printf("    failed -- port " + port_to_name(output_port) + ":  0x%x expected 0x%x\n",
            output_port.toBits(),
            output_values(pc).toBits()
          )
          io.error := Bool(true)
          io.done  := Bool(true)
        }
      }
    }

    dut_outputs.foreach { port => create_vectors_and_tests_for_output(port) }

    when(pc >= UInt(test_actions.length)) {
      printf(s"Stopping, end of tests, ${test_actions.length} steps\n")
      io.done := Bool(true)
    }.otherwise {
      pc := pc + UInt(1)
//      printf("bump pc: 0x%x\n", pc)
    }

  }
}

object UnitTester {
  def apply[T <: Module](gen: () => T): T = {
    gen()
  }
}
