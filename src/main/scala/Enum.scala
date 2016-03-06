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

/** An object for creating C style Enums */
object Enum {
  import Literal.sizeof
  /** create n enum values of given type for n <= 22
    * @param nodeType the type of node to create
    * @param n the number of nodes to create
    * @example {{{ val s1 :: s2 :: s3 :: Nil = Enum(UInt(), 3) }}}
    * @note When declaring vals the first character must be lower case, ie S1 is illegal
    * @note The 22 state size limitation is due to the maximum Scala tuple size of 22 (other methods avoid this limitation)*/
  def apply[T <: UInt](nodeType: T, n: Int): List[T] = (Range(0, n, 1).map(x => (Lit(x, sizeof(n-1))(nodeType)))).toList;

  /** create enum values of given type and names
    * @param nodeType the type of node to create
    * @param l any number of Symbols to create as an Enum
    * @return a map from Symbol to the node created*/
  def apply[T <: UInt](nodeType: T, l: Symbol *): Map[Symbol, T] = (l.toList zip (Range(0, l.length, 1).map(x => Lit(x, sizeof(l.length-1))(nodeType)))).toMap;

  /** create enum values of given type and names
    * @param nodeType the type of node to create
    * @param l a list of symbols
    * @return a map of symbols to a node created
    * @example {{{
    * val states = Enum(UInt(), List('s1, 's2, 's3))
    * val state = Reg(UInt(), init = states('s1))
    * }}}*/
  def apply[T <: UInt](nodeType: T, l: List[Symbol]): Map[Symbol, T] = (l zip (Range(0, l.length, 1).map(x => Lit(x, sizeof(l.length-1))(nodeType)))).toMap;

}
