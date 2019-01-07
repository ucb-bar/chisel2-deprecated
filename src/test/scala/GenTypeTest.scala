
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Ignore
import org.junit.Test

import Chisel._

class GenTypeTest extends TestSuite {
  class SubMod[T <: Bits]( genType : T ) extends Module {
    val io = new Bundle {
      val in = genType.cloneType.asInput
      val out = genType.cloneType.asOutput
    }
    val ZERO = genType.getLit(BigInt(0))
    val reg = Reg(init = ZERO)
    reg := io.in
    io.out := reg
  }

  @Test def testUInt() {
    class UserModUInt extends Module {
      val io = new Bundle {
        val in = UInt(INPUT, 4)
        val out = UInt(OUTPUT, 4)
      }
      val sub = Module(new SubMod[UInt](UInt(0, 4)))
      sub.io <> io
    }

    class UIntTest(c : UserModUInt) extends Tester(c) {
      poke(c.io.in, BigInt(3))
      expect(c.io.out, BigInt(0))
      step(1)
      expect(c.io.out, BigInt(3))
    }

    launchCppTester( (c : UserModUInt) => { new UIntTest(c) })
  }

  @Test def testBool() {
    class UserModBool extends Module {
      val io = new Bundle {
        val in = Bool(INPUT)
        val out = Bool(OUTPUT)
      }
      val sub = Module(new SubMod[Bool](Bool(false)))
      sub.io <> io
    }

    class BoolTest(c : UserModBool) extends Tester(c) {
      poke(c.io.in, BigInt(1))
      expect(c.io.out, BigInt(0))
      step(1)
      expect(c.io.out, BigInt(1))
    }

    launchCppTester( (c : UserModBool) => { new BoolTest(c) })
  }

  @Test def testSInt() {
    class UserModSInt extends Module {
      val io = new Bundle {
        val in = SInt(INPUT, 4)
        val out = SInt(OUTPUT, 4)
      }
      val sub = Module(new SubMod[SInt](SInt(0, 4)))
      sub.io <> io
    }

    class SIntTest(c : UserModSInt) extends Tester(c) {
      poke(c.io.in, BigInt(-1))
      expect(c.io.out, BigInt(0))
      step(1)
      expect(c.io.out, BigInt(-1))
    }

    launchCppTester( (c : UserModSInt) => { new SIntTest(c) })
  }

  @Test def testFlo() {
    class UserModFlo extends Module {
      val io = new Bundle {
        val in = Flo(INPUT)
        val out = Flo(OUTPUT)
      }
      val sub = Module(new SubMod[Flo](Flo(0)))
      sub.io <> io
    }

   class FloTest(c : UserModFlo) extends Tester(c) {
      poke(c.io.in, (1.5).toFloat)
      expect(c.io.out, (0).toFloat)
      step(1)
      expect(c.io.out, (1.5).toFloat)
    }

    launchCppTester( (c : UserModFlo) => { new FloTest(c) })
  }

  @Test def testDbl() {
    class UserModDbl extends Module {
      val io = new Bundle {
        val in = Dbl(INPUT)
        val out = Dbl(OUTPUT)
      }
      val sub = Module(new SubMod[Dbl](Dbl(0)))
      sub.io <> io
    }

    class DblTest(c : UserModDbl) extends Tester(c) {
      poke(c.io.in, 1.5)
      expect(c.io.out, 0.0)
      step(1)
      expect(c.io.out, 1.5)
    }

    launchCppTester( (c : UserModDbl) => { new DblTest(c) })
  }

  @Test def testFixed() {
    class UserModFixed extends Module {
      val io = new Bundle {
        val in = Fixed(INPUT, 18, 10)
        val out = Fixed(OUTPUT, 18, 10)
      }
      val sub = Module(new SubMod[Fixed](Fixed(0, 18, 10)))
      sub.io <> io
    }

    class FixedTest(c : UserModFixed) extends Tester(c) {
      poke(c.io.in, BigInt(10))
      expect(c.io.out, BigInt(0))
      step(1)
      expect(c.io.out, BigInt(10))
    }

    launchCppTester( (c : UserModFixed) => { new FixedTest(c) })
  }
}
