import org.scalatest.junit.AssertionsForJUnit
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Before

import Chisel._

class MyFloat extends Bundle {
val sign = Bool()
val exponent = Bits(width = 8)
val significand = Bits(width = 23)
}

class MyFPUnit extends Component {
  val io = new MyFloat()
}

class ExampleSuite extends AssertionsForJUnit {

  @Test def verifyEasy() {
    // Uses JUnit-style assertions, we are only interested
    // in generating code coverage for now.
    chiselMain(Array[String](), () => new MyFPUnit())
    assertTrue(true)
  }
}

