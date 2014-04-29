/*
 Copyright (c) 2011, 2012, 2013, 2014 The Regents of the University of
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
import Params._
import Node._
import Module._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.util.hashing._
import java.lang.reflect.{Type, ParameterizedType}
import scala.io.Source
import java.io._

import oscar.cp.modeling._
import oscar.cp.core._
import oscar.algo.search._
import oscar.util._
import oscar.cp.search.BinaryFirstFailBranching

case class InvalidJackParameterException(msg: String) extends Exception

object Jackhammer {

  val spaceName = "space.prm"

  def dump(dir: String, mode: String, n: Int) = {
    Params.buildingSpace = false
    mode match {
      case "space" => Params.dump_file(dir + "/" + spaceName, Params.space)
      case "point" => process(Params.space,dir,n)
      case _ => throw new InvalidJackParameterException("Invalid Jack parameter: " + mode)
    }
  }

  def process(space: Space, dir: String, n:Int) = {
    var sol = new ArrayBuffer[Tuple2[String,ArrayBuffer[Int]]]
    var newspace = new Params.Space
    for(t <- space){
      newspace += t
    }
    if(n <= 0) sol = solve(space,n) else {
      while(sol.length != n) {
        val ret = solve(space,1)
	      if(sol.isEmpty) {
	        sol ++= ret 
	      } else {
          if(sol.map(x => {!(x._1 == ret(0)._1) }).reduce((a,b) => a && b)) { 
	          sol ++= ret;
	        }
	      }
      }
    }
    for(d <- sol){
      var newDesign = new Params.Space
      for(t <- newspace){
        val p = new ValueParam(t._2.pname,d._2(t._2.index))
        p.gID = t._3
        newDesign += ((t._1,p,t._3))
      }
      writeDesign(dir,newDesign)
    }
  }

  def solve(space: Space, nbSol:Int):ArrayBuffer[Tuple2[String,ArrayBuffer[Int]]] = {
    val cp = CPSolver()
    val sol = new ArrayBuffer[Tuple2[String,ArrayBuffer[Int]]]

    var x = new ArrayBuffer[CPVarInt]
    for(t <- space){
      x = cvt(t,cp,x) 
    }
    cp.onSolution {
      val ret = x.map(x => x.getValue);
      sol += ((ret.toString,ret))
    }
    cp.solve subjectTo {
    } search {
      new BinaryFirstFailBranching(x,trueRandom(_))
    }
    val stats = if (nbSol <= 0) cp.start() else cp.start(nbSol)
    //println(stats)
    sol
  }
  def trueRandom(x:CPVarInt):Int = {
    val r = math.random
    math.rint(r*(x.max - x.min)).toInt + x.min
  }


  def writeDesign(dir: String, design: Params.Space) = {
    val design_tag = formatDesign(design)
    val design_filename = "design"+design_tag+".prm"
    Params.dump_file(dir+"/"+design_filename, design)
  }

  def formatDesign(design: Params.Space) : String = {
    val str = new StringBuilder("_")
    for((mod,p,gID) <- design) {
      str ++= p.pname + "-" + p.init+"_"
    }
    if(str.length<40) {
      str.toString + MurmurHash3.stringHash(design.toString,1).toHexString 
    } else {
      str.substring(0,40) + "_" + MurmurHash3.stringHash(design.toString,1).toHexString 
    }
 }

  def load(dir: String, designName: String) = {
    Params.buildingSpace = false
    Params.design = Params.load_file(dir + "/" + designName)
    Params.gID = 0
    //Params.design.map(println(_))
  }



  def cvt(t: Tuple3[String,Param[Any],Int], cp: CPSolver, x: ArrayBuffer[CPVarInt]): ArrayBuffer[CPVarInt] = {
    t._2 match {
      case RangeParam(n,init,min,max)       => t._2.index = x.length; x += (CPVarInt(cp,min to max)); x
      case LessParam(n,init,min,par)        => t._2.index = x.length; x += (CPVarInt(cp,min to t._2.max)); cp.add(x(x.length-1) < x(par.index)); println("par index: " + par.index); x
      case LessEqParam(n,init,min,par)      => t._2.index = x.length; x += (CPVarInt(cp,min to t._2.max)); cp.add(x(x.length-1) <= x(par.index)); println("par index: " + par.index); x
      case GreaterParam(n,init,par,max)     => t._2.index = x.length; x += (CPVarInt(cp,t._2.min to max)); cp.add(x(x.length-1) > x(par.index)); println("par index: " + par.index); x
      case GreaterEqParam(n,init,par,max)   => t._2.index = x.length; x += (CPVarInt(cp,t._2.min to max)); cp.add(x(x.length-1) >= x(par.index)); println("par index: " + par.index); x
      case DivisorParam(n,init,min,max,par) => t._2.index = x.length; x += (CPVarInt(cp,min to max)); x += (CPVarInt(cp,min to par.max)); cp.add(x(x.length-2)*x(x.length-1)==x(par.index)); x
      case ValueParam(n,value)                   => t._2.index = x.length; x += (CPVarInt(cp,value.toString().toInt)); x
      case EnumParam(n,value, values)            => t._2.index = x.length; val a = values.map(a => a.toInt).toList.toSet; x += (CPVarInt(cp,a)); x
      case _                                   => throw new ParamInvalidException("Invalid param type!"); x
    }
  }

}


/*
object Jackhammer {
  val spaceName = "space.prm"

  def dump(dir: String, mode: String) = {
    mode match {
      case "space" => Params.dump_file(dir + "/" + spaceName, Params.space)
      case "all"   => { for(design <- getDesigns(Params.space)) writeDesign(dir, design) }
    }
  }

  def load(dir: String, designName: String) = {
    Params.buildingSpace = false
    Params.design = Params.load_file(dir + "/" + designName)
  }

  def writeDesign(dir: String, design: Params.Space) = {
    val design_tag = JackFormatter.formatDesign(design)+MurmurHash3.stringHash(design.toString,1).toHexString
    val design_filename = "design"+design_tag+".prm"
    Params.dump_file(dir+"/"+design_filename, design)
  }
    
  def getDesigns(myspace: Params.Space) : List[Params.Space]= {
    var space = myspace
    var designs:List[Params.Space] = Nil
    for((mname,mhash) <- space){
      for((pname, pparam) <- mhash){
        var evals = getEvals(pparam)
        var new_designs: List[Params.Space] = Nil
        for(e <- evals) {  
          val d_temp = updateList(mname,pname,e,designs)
          new_designs = d_temp ::: new_designs
        }
        designs = new_designs
      }
    }
    designs
  }
  
  def getEvals(param: Param[Any]) : List[Any] = {
    var evals:List[Any] = Nil
    param match {
      case ValueParam(init) => {
        init :: evals
      }
      case IIRangeParam(init, min, max, func) => {
        var i = min;
        while(i <= max) { evals = i :: evals; i = param.step(i) }
        evals
      }
      case IIRangeParam(init, min, maxR, func) => {
        var i = min;
        while(i <= max) { evals = i :: evals; i = param.step(i) }
        evals
      }
      case _ => { evals }
    }
  }

  def updateList[T<:Any](mname: String, pname: String, value: T, list: List[Params.Space]): List[Params.Space] = {
    var myList:List[Params.Space] = Nil
    if(list.isEmpty){
      val hash = new Params.Space
      myList = addParam(hash, mname, pname, new ValueParam(value)) :: myList
    } else {
      for(x <- list){
        myList = addParam(x, mname, pname, new ValueParam(value)) :: myList
      }
    }
    myList
  }

  def addParam(myspace: Params.Space, mname: String, pname: String, param: Param[Any]): Params.Space = {
    var space = new Params.Space
    if(!space.contains(mname)) {
      space(mname) = new HashMap[String,Param[Any]]
    }
    for((m,h) <- myspace){
      for((n,p) <- h){
        space(m)(n) = p
      }
    }
    space(mname)(pname) = param
    space
  }


}

object JackFormatter {
}
*/
