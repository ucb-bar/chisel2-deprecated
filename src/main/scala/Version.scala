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

package Chisel

import scala.util.matching.Regex

object Version {
  val versionRE = """(^\d+){0,1}((\.(\d+)){0,2})$""".r

  def apply(s: String): Version = {
    new Version(s)
  }

  implicit def stringToVersion(s: String): Version = {
    Version(s)
  }
}

/** Version - dotted string with up to three components.
  *  Comparisons are done component by component for the smallest number of components.
  *  So:
  *  "3" >= "2.99.99"
  *  "3" < "4"
  *  "3" >= "3.99.99"
  *  "3.9" >= "3"
  *  "3.9" >= "3.0"
  *  "3.9" >= "3.9.99"
  *
  *  The empty string "" is the "maximum" version and compares > all other versions.
  */
class Version(val dottedString: String) extends scala.math.Ordered[Version] {
  // Ensure we have a valid version string.
  require(Version.versionRE.pattern.matcher(dottedString).matches)
  val maxVersion: Boolean = dottedString == ""

  def compare(that: Version):Int = {
    // If both versions are maximum, they're equal.
    if (this.maxVersion && that.maxVersion) {
      0
    } else if (this.maxVersion) {
      1
    } else if (that.maxVersion) {
      -1
    } else {
      // Neither version is maximum. Compare components.
      val vtuples: Array[(String, String)] = this.dottedString.split('.') zip that.dottedString.split('.')
      vtuples.find(vtuple => vtuple._1 != vtuple._2) match {
        case Some((vsthis, vsthat)) => vsthis.toInt - vsthat.toInt
        case None => 0
      }
    }
  }

  override def toString = dottedString

  /** Determine Version equality.  */
  override def equals(other: Any): Boolean = other match {
    case that: Version =>
      (that canEqual this) && (this.dottedString == that.dottedString)
    case _ =>
      false
  }

  /** Determine if two Versions are comparable. */
  def canEqual(other: Any): Boolean = other.isInstanceOf[Version]
}

