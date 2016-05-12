//package ChiselTests
import Chisel._
import org.junit.Test
import org.junit.Assert._

class VecSuite extends TestSuite {
  @Test def testMiscVec() {
    println("\ntestMiscVec ...")

    class VecApp(n: Int, W: Int) extends Module {
      class MyBundle(aWidth: Int) extends Bundle {
        val aUInt = UInt(width = aWidth)
      }
      val io = new Bundle {
        val a = UInt(INPUT, n)
        val i = Vec(n, Bits(INPUT, W))
        val d = Bits(OUTPUT, W)
      }
      io.d := io.i(io.a)
    }
    
    val testArgs = chiselEnvironmentArguments() ++ Array("--targetDir", dir.getPath.toString(),
          "--minimumCompatibility", "3.0.0", "--wError", "--backend", "null")
    chiselMain(testArgs, () => Module(new VecApp(8, 9)))
    assertFalse(ChiselError.hasErrors)
  }

  @Test def testEmptyVec() {
    println("\ntestEmptyVec ...")

    class EmptyVec(n: Int) extends Module {
      class MyBundle(aWidth: Int) extends Bundle {
        val aUInt = UInt(width = aWidth)
      }
      val io = new Bundle {
        val empty = Vec(0, new MyBundle(n).asOutput)
      }
    }
    
    val testArgs = chiselEnvironmentArguments() ++ Array("--targetDir", dir.getPath.toString(),
          "--minimumCompatibility", "3.0.0", "--wError", "--backend", "null")
    intercept[IllegalStateException] {
      chiselMain(testArgs, () => Module(new EmptyVec(8)))
    }
    assertTrue(ChiselError.hasErrors)
  }
}
