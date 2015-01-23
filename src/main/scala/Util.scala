package Chisel

import scala.collection.mutable.HashMap
import java.io.BufferedReader
import java.io.InputStreamReader

/* General utility functions.
 * 
 */
object Util {

  def editLine(line: String, replacements: HashMap[String, String]): String = {
    // Break the line into "tokens"
    val tokens = line.split("""(?=(@\w+@)|[^@\w]+)""")
    // Reassemble the line plugging in expansions for "macro"
    val outputLine = tokens.map(s => {
      if (s.length > 2 && s.charAt(0) == '@' && s.takeRight(1) == "@") {
        replacements(s)
      } else {
        s
      }
    }) mkString ""
    outputLine
  }

  def editString(string: String, replacements: HashMap[String, String]): String = {
    val outString = new StringBuilder("")
    for (line <- string.split('\n')) {
      val outputLine = editLine(line, replacements)
      outString.append(outputLine + "\n")
    }
    outString.toString
  }

  def editResource(resourceName: String, replacements: HashMap[String, String]):String = {
    val outString = new StringBuilder("")
    val resourceStream = getClass().getResourceAsStream("/" + resourceName)
    if( resourceStream != null ) {
      val br = new BufferedReader(new InputStreamReader(resourceStream, "US-ASCII"))
      while(br.ready()) {
        val inputLine = br.readLine()
        val outputLine = editLine(inputLine, replacements)
        outString.append(outputLine + "\n")
      }
      br.close()
    } else {
      println(s"WARNING: Unable to find '$resourceName'" )
    }
    outString.toString
  }

}