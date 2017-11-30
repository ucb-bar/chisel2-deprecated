//package ChiselTests
import Chisel._
import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore

class Coord extends Bundle {
  val x = UInt(width = 32)
  val y = UInt(width = 32)
  // We leave this as "clone" (we support the more correct "cloneType")
  //  to ensure that the "old" usage is still supported.
  override def clone: this.type = {
    val res = new Coord()
    res.x.dir = this.x.dir
    res.y.dir = this.y.dir
    res.asInstanceOf[this.type]
  }
}

 class BundleWireSuite extends TestSuite {

 @Test def testBundleWire() {
    println("\ntestBundleWire ...")

    class BundleWire extends Module {
      val io = new Bundle {
        val in   = (new Coord).asInput
        val outs = Vec(4, (new Coord).asOutput)
      }
      val coords = Wire(Vec(4, new Coord))
      for (i <- 0 until 4) {
        coords(i)  := io.in
        io.outs(i) := coords(i)
      }
    }

    trait BundleWireTests extends Tests {
      def tests(c: BundleWire) {
       for (t <- 0 until 4) {
         val test_in_x = rnd.nextInt(256)
         val test_in_y = rnd.nextInt(256)
         poke(c.io.in.x, test_in_x)
         poke(c.io.in.y, test_in_y)
         step(1)
         for (i <- 0 until 4) {
           expect(c.io.outs(i).x, test_in_x)
           expect(c.io.outs(i).y, test_in_y)
         }
       }
      }
    }

    class BundleWireTester(c: BundleWire) extends Tester(c) with BundleWireTests {
      tests(c)
    }
    val args = chiselEnvironmentArguments() ++ Array[String]("--backend", "c",
      "--targetDir", dir.getPath.toString(), "--genHarness", "--compile", "--test")
    chiselMainTest(args, () => Module(new BundleWire())) {m => new BundleWireTester(m)}
  }

  @Test def testBundleWire2() {
    println("\ntestBundleWire2 ...")

    class BundleWire extends Module {
      val io = new Bundle {
        val in   = (new Coord).asInput
        val outs = Vec(4, (new Coord).asOutput)
      }
      val coords = Wire(new Bundle {
        val a0 = new Coord
        val a1 = new Coord
        val a2 = new Coord
        val a3 = new Coord
        def apply(i: Int) = i match {
          case 0 => a0
          case 1 => a1
          case 2 => a2
          case 3 => a3
        }
      })
      for (i <- 0 until 4) {
        coords(i)  := io.in
        io.outs(i) := coords(i)
      }
    }

    class BundleWireTester(c: BundleWire) extends Tester(c) {
      for (t <- 0 until 4) {
        val test_in_x = rnd.nextInt(256)
        val test_in_y = rnd.nextInt(256)
        poke(c.io.in.x, test_in_x)
        poke(c.io.in.y, test_in_y)
        step(1)
        for (i <- 0 until 4) {
          expect(c.io.outs(i).x, test_in_x)
          expect(c.io.outs(i).y, test_in_y)
        }
      }
    }

    val args = chiselEnvironmentArguments() ++ Array[String]("--backend", "c",
      "--targetDir", dir.getPath.toString(), "--genHarness", "--compile", "--test")
    chiselMainTest(args, () => Module(new BundleWire())) {m => new BundleWireTester(m)}
  }
}
