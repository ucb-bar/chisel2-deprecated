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
  // Issue #718 - Vec.fill with vec initializer
  @Test def testVecFillFromVec() {
    println("\ntestVecFillFromVec ...")

    class VecApp(n: Int) extends Module {
      val io = new Bundle {
        val a = UInt(INPUT, n)
      }
      val z = Vec(32, UInt(32, width=16))
      var i = 0
      val x = Vec.fill( 32 )
      {
        val y = z(i)
        i = i + 1
        y
      }
    }
    
    val testArgs = chiselEnvironmentArguments() ++ Array("--targetDir", dir.getPath.toString(),
          /* "--minimumCompatibility", "3.0.0", "--wError",*/ "--backend", "null")
    chiselMain(testArgs, () => Module(new VecApp(32)))
    assertFalse(ChiselError.hasErrors)
    
  }

  @Test def constVecTest {
    class UserMod( testCond : Boolean ) extends Module {
      val io = new Bundle {
        val in = UInt( INPUT, 4 )
        val out = UInt( OUTPUT, 4 )
      }
      val myVec = Vec( List( UInt( 3, 4 ), UInt( 1, 4 ), UInt( 5, 4 ), UInt( 8, 4 ) ) )
      val idx = {
        if ( testCond )
          UInt( 2, 4 )
        else
          io.in
      }
      io.out := myVec(idx)
    }

    chiselMain(Array[String]("--backend", "v",
      "--targetDir", dir.getPath.toString()),
      () => Module(new UserMod(true)))
    assertFile("VecSuite_UserMod_1.v")
  }

}
