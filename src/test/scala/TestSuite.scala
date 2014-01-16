import org.scalatest.junit.AssertionsForJUnit
import org.junit.Before
import java.io.File

class TestSuite extends AssertionsForJUnit {

  val dir = new File("test-outputs")

  @Before def initialize() {
    dir.mkdir
  }

  def assertFile( filename: String ) {
    val reffile = scala.io.Source.fromURL(getClass.getResource(filename))
    val content = reffile.mkString
    reffile.close()
    val source = scala.io.Source.fromFile(
      dir.getPath + "/" + filename, "utf-8")
    val lines = source.mkString
    source.close()
    assert(lines === content)
  }
}
