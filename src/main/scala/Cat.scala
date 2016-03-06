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

object Cat {
  /** Combine data elements together
    * @param mod Data to combine with
    * @param mods any number of other Data elements to be combined in order
    * @return A UInt which is all of the bits combined together
    */
  def apply[T <: Data](mod: T, mods: T*): UInt = apply(mod :: mods.toList)
  /** Combine data elements together
    * @param mods any number of other Data elements to be combined in order
    * @return A UInt which is all of the bits combined together
    */
  def apply[T <: Data](mods: Seq[T]): UInt =
    UInt(OUTPUT).fromNode(Concatenate(mods))
}

object Concatenate {
  /** Combine nodes together
    * @param mod Data to combine with
    * @param mods any number of other Data elements to be combined in order
    * @return A Node which is all of the bits combined together
    */
  def apply(mod: Node, mods: Node*): Node = apply(mod :: mods.toList)
  /** Combine nodes together
    * @param mods any number of other Data elements to be combined in order
    * @return A Node which is all of the bits combined together
    */
  def apply(mods: Seq[Node]): Node = doCat(mods)
  private def doCat(mods: Seq[Node]): Node =
    if (mods.tail.isEmpty) mods.head
    else BinaryOp(doCat(mods.slice(0, mods.length/2)), doCat(mods.slice(mods.length/2, mods.length)), "##")
}
