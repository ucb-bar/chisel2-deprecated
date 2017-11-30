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

import scala.util.Random
import org.scalatest.Args
import org.scalatest.tagobjects.Slow

import Chisel._

// We use the ScalaTest Suite non-JUint classes in order to be able to tag the more extensive tests as Slow.
//  Trying to tag them with the @Slow annotation does not seem to work.
class ExtractSuite extends FunTestSuite {
  val maxWidth = 1024
  var testMultiplier = 4
  var runMultiplier = 4
  lazy val testCount = 1 * testMultiplier
  lazy val runCount = 1 * runMultiplier

  // Provide a run method so we can access the arguments (specifically the filter)
  //  used when running the tests.
  override def run(testName: Option[String], args: Args): org.scalatest.Status = {
    // If we're excluding Slow tests, reduce the test and run multipliers.
    if (args.filter.tagsToExclude.contains("org.scalatest.tags.Slow")) {
      testMultiplier = 1
      runMultiplier = 1
    }
    super.run(testName, args)
  }

  // Generate a list of test widths from 1 to maxWidth.
  // If we're generating enough values, use variations (-1, 0, +1) on the special powers of 2 for the 2nd through 4th elements.
  def genMaxWidths(rnd: Random, maxWidth: Int, nTests: Int): List[Int] = {
    val specialPow2 = rnd.shuffle(List(8, 16, 32, 64, 128, 256)).head
    // Pick a random power of 2 from 4 to maxWidth - 1
    val range = 4 to maxWidth
    for {
      t <- List.range(0, nTests)
      pow2 = t match {
        case 1 if (nTests > 2) => specialPow2 - 1
        case 2 => specialPow2
        case 3 => specialPow2 + 1
        case _ => range(rnd.nextInt(range.length))
      }
      inputWidth = pow2
    }
      yield inputWidth
  }

  // Generate a list of hi, lo specs where hi >= lo and (hi - lo + 1) <= inputWidth.
  //  The second element in the list will have hi == lo, and the third will have lo = 0, hi = inputWidth - 1
  //  All other elements (including the first) will have random values.
  def genHiLo(rnd: Random, inputWidth: Int, nTests: Int): List[(Int, Int)] = {
    for {
      t <- List.range(0, nTests)
      extractWidth = t match {
        case 1 => 1
        case 2 => inputWidth
        case _ => rnd.nextInt(inputWidth - 1) + 1 // first and remaining cases.
      }
      lo = t match {
        case 2 => 0
        case _ => rnd.nextInt(inputWidth - extractWidth)
      }
      hi = lo + extractWidth - 1
    }
      yield (hi, lo)
  }

  // Generate a list of widths where 1 < width <= inputWidth, ensuring widths of 1 and inputWidth are in the list,
  //  (assuming the list is long enough).
  def genExtractWidths(rnd: Random, inputWidth: Int, nTests: Int): List[Int] = {
    var l1 = false
    var lmax = false
    val l1Test = nTests - 1
    val lmaxTest = nTests - 2
    val result = for {
        t <- List.range(0, nTests)
        extractWidth = {
          val ni = t match {
            case `l1Test` if (!l1 && nTests > l1Test + 1) => 1
            case `lmaxTest` if (!lmax && nTests > lmaxTest + 2) => inputWidth
            case _ => rnd.nextInt(inputWidth) + 1
          }
          ni match {
            case 1 => l1 = true
            case `inputWidth` => lmax = true
            case _ =>
          }
          ni
        }
      }
        yield extractWidth

    result
  }

  /** Bad Width Inference in Extract #621
   *
   */
  test("extractWidthInfer") {
    println("\nextractWidthInfer ...")
    class ExtractWidthInfer extends Module {
      val io = new Bundle {
              val in = UInt(INPUT, width = 64)
              val data = UInt(OUTPUT)
      }
      val offset = Reg(UInt(width = 32))
      io.data := io.in(UInt(63) - offset, UInt(0))
    }

    class TestExtractWidthInfer(m: ExtractWidthInfer) extends Tester(m) {
      List(0, 16, 4, 7, 12, 64).map { i =>
        val data = BigInt(i)
        poke(m.io.in, data)
        step(1)
        expect(m.io.data, data & 0xffffffff)
      }
    }

    for (backend <- Array("c", "v")) {
      if (backend == "v" && !Driver.isVCSAvailable) {
        println("vcs unavailable - skipping Verilog test")
      } else {
        chiselMainTest(chiselEnvironmentArguments() ++ Array[String]("--backend", backend, "--compile", "--genHarness", "--test", "--targetDir", dir.getPath.toString()),
          () => Module(new ExtractWidthInfer)) {
            c => new TestExtractWidthInfer(c)
        }
      }
    }
  }

  // Test static hi, lo.
  test("extractStatic") {
    println("\nextractStatic ...")
    class ExtractStatic(w: Int, hi: Int, lo: Int) extends Module {
      val io = new Bundle {
              val in = UInt(INPUT, width = w)
              val data = UInt(OUTPUT)
      }
      io.data := io.in(UInt(hi), UInt(lo))
    }

    class TestExtractStaticWidthHiLo(m: ExtractStatic, maxWidth: Int, hi: Int, lo: Int, nRuns: Int) extends Tester(m) {
      val mask = (BigInt(1) << (hi - lo + 1)) - 1
      println("width: %d, hi: %d, lo: %d".format(maxWidth, hi, lo))
      for (r <- 1 to nRuns) {
        val data = BigInt(maxWidth, rnd)

        poke(m.io.in, data)
        step(1)
        expect(m.io.data, (data >> lo) & mask)
      }
    }

    class TestExtractStatic(rnd: Random, maxWidth: Int, nTests: Int, nRuns: Int) {

      // Pick a random power of 2 from 4 to maxWidth
      for (inputWidth <- genMaxWidths(rnd, maxWidth, nTests)) {
        for ((hi, lo) <- genHiLo(rnd, inputWidth, nRuns)) {
          for (tester <- Array("c", "v")) {
            if (tester == "v" && !Driver.isVCSAvailable) {
              println("vcs unavailable - skipping Verilog test")
            } else {
              chiselMainTest(chiselEnvironmentArguments() ++ Array[String]("--backend", tester, "--compile", "--genHarness", "--test", "--targetDir", dir.getPath.toString()),
                () => Module(new ExtractStatic(inputWidth, hi, lo))) {
                  c => new TestExtractStaticWidthHiLo(c, inputWidth, hi, lo, nRuns)
              }
            }
          }
        }
      }
    }

    val t = new TestExtractStatic(new Random(Driver.testerSeed), maxWidth, testCount, runCount)
  }

  // Test dynamic hi, lo.
  test("extractDynamic", Slow) {
    println("\nextractDynamic ...")
    class ExtractDynamic(w: Int) extends Module {
      val io = new Bundle {
        val in = UInt(INPUT, width = w)
        val data = UInt(OUTPUT)
        val hi = UInt(INPUT, width = 16)
        val lo = UInt(INPUT, width = 16)
      }
      io.data := UInt(0)

      // Protect against bad random startup values.
      val hi = UInt(INPUT, 16)
      val lo = UInt(INPUT, 16)
      hi := UInt(0, 16)
      lo := UInt(0, 16)
      when(io.hi >= io.lo && io.hi < UInt(w)) {
        hi := io.hi
        lo := io.lo
      }
      io.data := io.in(hi, lo)
    }

    class TestExtractDynamicHiLo(m: ExtractDynamic, maxWidth: Int, nRuns: Int) extends Tester(m) {
      for ((hi, lo) <- genHiLo(rnd, maxWidth, nRuns)) {
        val data = BigInt(maxWidth, rnd)
        val mask = (BigInt(1) << (hi - lo + 1)) - 1
        println("width: %d, hi: %d, lo: %d, mask: 0x%x".format(maxWidth, hi, lo, mask))
        poke(m.io.in, data)
        poke(m.io.hi, hi)
        poke(m.io.lo, lo)
        step(1)
        expect(m.io.data, (data >> lo) & mask)
      }
    }


    class TestExtractDynamic(rnd: Random, maxWidth: Int, nTests: Int, nRuns: Int) {

      // Pick a random power of 2 from 4 to maxWidth
      for (inputWidth <- genMaxWidths(rnd, maxWidth, nTests)) {
        for (tester <- Array("c", "v")) {
          if (tester == "v" && !Driver.isVCSAvailable) {
            println("vcs unavailable - skipping Verilog test")
          } else {
            chiselMainTest(chiselEnvironmentArguments() ++ Array[String]("--backend", tester, "--compile", "--genHarness", "--test", "--targetDir", dir.getPath.toString()),
              () => Module(new ExtractDynamic(inputWidth))) {
                c => new TestExtractDynamicHiLo(c, inputWidth, nRuns)
            }
          }
        }
      }
    }

    val t = new TestExtractDynamic(new Random(Driver.testerSeed), maxWidth, testCount, runCount)
  }

  // Test dynamic hi, static lo.
  test("extractDynamicHi", Slow) {
    println("\nextractDynamicHi ...")
    class ExtractDynamic(w: Int, lo: Int) extends Module {
      val io = new Bundle {
        val in = UInt(INPUT, width = w)
        val data = UInt(OUTPUT)
        val hi = UInt(INPUT, width = 16)
      }
      io.data := UInt(0)

      // Protect against bad random startup values.
      val hi = UInt(INPUT, 16)
      hi := UInt(lo)
      when(io.hi >= UInt(lo) && io.hi < UInt(w)) {
        hi := io.hi
      }
      io.data := io.in(hi, UInt(lo))
    }

    class TestExtractDynamicHi(m: ExtractDynamic, maxWidth: Int, nRuns: Int, lo: Int) extends Tester(m) {
      for (width <- genExtractWidths(rnd, maxWidth - lo, nRuns)) {
        val hi = lo + width - 1
        val data = BigInt(maxWidth, rnd)
        val mask = (BigInt(1) << width) - 1
        println("width: %d, hi: %d, lo: %d, mask: 0x%x".format(maxWidth, hi, lo, mask))
        poke(m.io.in, data)
        poke(m.io.hi, hi)
        step(1)
        expect(m.io.data, (data >> lo) & mask)
      }
    }

    class TestExtractDynamic(rnd: Random, maxWidth: Int, nTests: Int, nRuns: Int) {

      // Pick a random power of 2 from 4 to maxWidth
      for (inputWidth <- genMaxWidths(rnd, maxWidth, nTests)) {
        for (width <- genExtractWidths(rnd, inputWidth, nRuns)) {
          val lo = inputWidth - width
          for (tester <- Array("c", "v")) {
            if (tester == "v" && !Driver.isVCSAvailable) {
              println("vcs unavailable - skipping Verilog test")
            } else {
              chiselMainTest(chiselEnvironmentArguments() ++ Array[String]("--backend", tester, "--compile", "--genHarness", "--test", "--targetDir", dir.getPath.toString()),
                () => Module(new ExtractDynamic(inputWidth, lo))) {
                  c => new TestExtractDynamicHi(c, inputWidth, nRuns, lo)
              }
            }
          }
        }
      }
    }

    val t = new TestExtractDynamic(new Random(Driver.testerSeed), maxWidth, testCount, runCount)
  }

  // Test static hi, dynmic lo.
  test("extractDynamicLo", Slow) {
    println("\nextractDynamicLo ...")
    class ExtractDynamic(w: Int, hi: Int) extends Module {
      val io = new Bundle {
        val in = UInt(INPUT, width = w)
        val data = UInt(OUTPUT)
        val lo = UInt(INPUT, width = 16)
      }
      io.data := UInt(0)

      // Protect against bad random startup values.
      val lo = UInt(INPUT, 16)
      lo := UInt(0, 16)
      when(UInt(hi) >= io.lo) {
        lo := io.lo
      }
      io.data := io.in(UInt(hi), lo)
    }

    class TestExtractDynamicLo(m: ExtractDynamic, maxWidth: Int, nRuns: Int, hi: Int) extends Tester(m) {
      for (width <- genExtractWidths(rnd, hi + 1, nRuns)) {
        val lo = hi - width + 1
        val data = BigInt(maxWidth, rnd)
        val mask = (BigInt(1) << width) - 1
        println("width: %d, hi: %d, lo: %d, mask: 0x%x".format(maxWidth, hi, lo, mask))
        poke(m.io.in, data)
        poke(m.io.lo, lo)
        step(1)
        expect(m.io.data, (data >> lo) & mask)
      }
    }


    class TestExtractDynamic(rnd: Random, maxWidth: Int, nTests: Int, nRuns: Int) {

     // Pick a random power of 2 from 4 to maxWidth
      for (inputWidth <- genMaxWidths(rnd, maxWidth, nTests)) {
        for (width <- genExtractWidths(rnd, inputWidth, nRuns)) {
          val hi = width - 1
          for (tester <- Array("c", "v")) {
            if (tester == "v" && !Driver.isVCSAvailable) {
              println("vcs unavailable - skipping Verilog test")
            } else {
              chiselMainTest(chiselEnvironmentArguments() ++ Array[String]("--backend", tester, "--compile", "--genHarness", "--test", "--targetDir", dir.getPath.toString()),
                () => Module(new ExtractDynamic(inputWidth, hi))) {
                  c => new TestExtractDynamicLo(c, inputWidth, nRuns, hi)
              }
            }
          }
        }
      }
    }

    val t = new TestExtractDynamic(new Random(Driver.testerSeed), maxWidth, testCount, runCount)
  }

  // Test dynamic onebit (hi == lo).
  test("extractDynamicOneBit") {
    println("\nextractDynamicOneBit ...")
    class ExtractDynamic(w: Int) extends Module {
      val io = new Bundle {
        val in = UInt(INPUT, width = w)
        val data = UInt(OUTPUT)
        val hi = UInt(INPUT, width = 16)
      }
      io.data := UInt(0)

      // Protect against bad random startup values.
      val hi = UInt(INPUT, 16)
      hi := UInt(0, 16)
      when(io.hi < UInt(w)) {
        hi := io.hi
      }
      io.data := io.in(hi, hi)
    }

    class TestExtractDynamicOneBit(m: ExtractDynamic, maxWidth: Int, nRuns: Int) extends Tester(m) {
      val width = 1
      for ((hi, dummy) <- genHiLo(rnd, maxWidth, nRuns)) {
        val lo = hi
        val data = BigInt(maxWidth, rnd)
        val mask = (BigInt(1) << width) - 1
        println("width: %d, hi: %d, lo: %d, mask: 0x%x".format(maxWidth, hi, lo, mask))
        poke(m.io.in, data)
        poke(m.io.hi, hi)
        step(1)
        expect(m.io.data, (data >> lo) & mask)
      }
    }

    class TestExtractDynamic(rnd: Random, maxWidth: Int, nTests: Int, nRuns: Int) {

     // Pick a random power of 2 from 4 to maxWidth
      for (inputWidth <- genMaxWidths(rnd, maxWidth, nTests)) {
        for (tester <- Array("c", "v")) {
          if (tester == "v" && !Driver.isVCSAvailable) {
            println("vcs unavailable - skipping Verilog test")
          } else {
            chiselMainTest(chiselEnvironmentArguments() ++ Array[String]("--backend", tester, "--compile", "--genHarness", "--test", "--targetDir", dir.getPath.toString()),
              () => Module(new ExtractDynamic(inputWidth))) {
                c => new TestExtractDynamicOneBit(c, inputWidth, nRuns)
            }
          }
        }
      }
    }

    val t = new TestExtractDynamic(new Random(Driver.testerSeed), maxWidth, testCount, runCount)
  }

  // Test dynamic val_t (extract bits from a created temporary).
  test("extractDynamicVal_t") {
    println("\nextractDynamicVal_t ...")
    class ExtractDynamic(w: Int) extends Module {
      val wTop = w/2
      val wBottom = w - wTop
      val io = new Bundle {
        val inTop = UInt(INPUT, width = wTop)
        val inBottom = UInt(INPUT, width = wBottom)
        val data = UInt(OUTPUT)
        val hi = UInt(INPUT, width = 16)
        val lo = UInt(INPUT, width = 16)
      }
      io.data := UInt(0)

      // Protect against bad random startup values.
      val hi = UInt(INPUT, 16)
      hi := UInt(0, 16)
      val lo = UInt(INPUT, 16)
      lo := UInt(0, 16)
      when(io.hi >= io.lo && io.hi < UInt(w)) {
        hi := io.hi
        lo := io.lo
      }
      val in = Cat(io.inTop, io.inBottom)
      io.data := in(hi, lo)
    }

    class TestExtractDynamicVal_t(m: ExtractDynamic, maxWidth: Int, nRuns: Int) extends Tester(m) {
      for ((hi, lo) <- genHiLo(rnd, maxWidth, nRuns)) {
        val width = hi - lo + 1
        val wTop = maxWidth/2
        val wBottom = maxWidth - wTop
        val data = BigInt(maxWidth, rnd)
        val mask = (BigInt(1) << width) - 1
        val maskTop = (BigInt(1) << wTop) - 1
        val maskBottom = (BigInt(1) << wBottom) - 1
        println("maxWidth: %d, data: 0x%x, hi: %d, lo: %d, mask: 0x%x, wTop: %d, maskTop 0x%x, wBottom: %d, maskBottom: 0x%x".format(maxWidth, data, hi, lo, mask, wTop, maskTop, wBottom, maskBottom))
        poke(m.io.inBottom, data & maskBottom)
        poke(m.io.inTop, (data >> wBottom) & maskTop)
        poke(m.io.hi, hi)
        poke(m.io.lo, lo)
        step(1)
        expect(m.io.data, (data >> lo) & mask)
      }
    }

    class TestExtractDynamic(rnd: Random, maxWidth: Int, nTests: Int, nRuns: Int) {

      // Pick a random power of 2 from 4 to maxWidth
      for (inputWidth <- genMaxWidths(rnd, maxWidth, nTests)) {
        for (tester <- Array("c", "v")) {
          if (tester == "v" && !Driver.isVCSAvailable) {
            println("vcs unavailable - skipping Verilog test")
          } else {
            chiselMainTest(chiselEnvironmentArguments() ++ Array[String]("--backend", tester, "--compile", "--genHarness", "--test", "--targetDir", dir.getPath.toString()),
              () => Module(new ExtractDynamic(inputWidth))) {
                c => new TestExtractDynamicVal_t(c, inputWidth, nRuns)
            }
          }
        }
      }
    }

    val t = new TestExtractDynamic(new Random(Driver.testerSeed), maxWidth, testCount, runCount)
  }
}
