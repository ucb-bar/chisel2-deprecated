package Tutorial

import java.io.File
import java.io.ByteArrayInputStream
import javax.sound.sampled._

object WavIn {
  def apply(filename: String) = {
    val ais = AudioSystem.getAudioInputStream(new File(filename))
    if (ais.getFormat.getChannels != 1 || ais.getFormat.getSampleSizeInBits != 8) {
      println(filename + " must be 8-bit monoaural")
      System.exit(-1)
    }
    new WavIn(ais)
  }
}
class WavIn(val stream: AudioInputStream) {
  def read      = stream.read
  def close     = stream.close
  def getFormat = stream.getFormat
}

object WavOut {
  def apply(filename: String, f: AudioFormat) = {
    new WavOut(filename, f)
  }
}
class WavOut(val filename: String, f: AudioFormat) extends AudioInputStream(new ByteArrayInputStream(Array[Byte]()), f, AudioSystem.NOT_SPECIFIED) {
  val buf = collection.mutable.ArrayBuffer[Byte]()
  def += (s: Byte) = buf += s
  def flush = 
    AudioSystem.write(this, AudioFileFormat.Type.WAVE, new File(filename))
}

