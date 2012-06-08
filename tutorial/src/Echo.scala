package Tutorial

import Chisel._
import Node._
import java.io.File
import java.io.ByteArrayInputStream
import javax.sound.sampled._
import scala.collection.mutable.HashMap

class Echo extends Component {
  val io = new Bundle {
    val in = Bits(8, INPUT)
    val out = Bits(8, OUTPUT)
  }

  val samples = 4096
  require(isPow2(samples))

  val inSigned = (io.in - UFix(128)).toFix // convert to two's complement

  val history = Mem(samples) { Fix(width = 8) }
  val pos = Reg(resetVal = UFix(0, log2Up(samples)))
  pos := pos + UFix(1)

  // y[n] = x[n] + 0.5 * y[n - samples]
  val out = inSigned + (history(pos) >> UFix(1))

  history(pos) := out

  val outUnsigned = out + UFix(128) // convert back to excess-128 format
  io.out := outUnsigned
}

class EchoTests(c: Echo, val infile: String, val outfile: String) extends Tester(c, Array(c.io)) {  
  defTests {
    val svars = new HashMap[Node, Node]()
    val ovars = new HashMap[Node, Node]()

    val ais = AudioSystem.getAudioInputStream(new File(infile))
    if (ais.getFormat.getChannels != 1 || ais.getFormat.getSampleSizeInBits != 8) {
      println(infile + " must be 8-bit monoaural")
      System.exit(-1)
    }
    val out = new EchoOutput(ais.getFormat)

    var sample = ais.read
    while (sample != -1) {
      svars(c.io.in) = Fix(sample)
      step(svars, ovars, isTrace = false)
      out += ovars(c.io.out).litValue().toByte
      sample = ais.read
    }

    AudioSystem.write(out, AudioFileFormat.Type.WAVE, new File(outfile));
    true
  }
}

class EchoOutput(f: AudioFormat) extends AudioInputStream(new ByteArrayInputStream(Array[Byte]()), f, AudioSystem.NOT_SPECIFIED) {
  val buf = collection.mutable.ArrayBuffer[Byte]()
  var pos = 0
  def += (s: Byte) = buf += s

  override def available: Int = buf.length - pos
  override def read(out: Array[Byte], offs: Int, len: Int): Int = {
    val bytes = math.min(available, len)
    for (i <- 0 until bytes)
      out(offs + i) = buf(pos + i)
    pos += bytes
    bytes
  }
}
