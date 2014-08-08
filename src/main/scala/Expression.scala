//////////////////////////////////////////////////////////////////////////
// Open System-on-a-Chip (OpenSoC) is Copyright (c) 2014,               //
// The Regents of the University of California, through Lawrence        //
// Berkeley National Laboratory (subject to receipt of any required     //
// approvals from the U.S. Dept. of Energy).  All rights reserved.      //
//                                                                      //
// Portions may be copyrighted by others, as may be noted in specific   //
// copyright notices within specific files.                             //
//                                                                      //
// AUTHOR: J. Bachan                                                    //
//////////////////////////////////////////////////////////////////////////

package Chisel

import scala.collection.immutable.{Seq=>Seq, Iterable=>Iterable}
import scala.{collection=>readonly}
import scala.collection.mutable

abstract class Ex[T] {
  override def toString = Ex.pretty(this)
}

case class IntEx (expr:Ex[Int]) {
  def === (x:IntEx):Ex[Boolean] = (ExEq[Int](expr,x.expr))
  def +   (x:IntEx):Ex[Int] = ExAdd(expr,x.expr)
  def -   (x:IntEx):Ex[Int] = ExSub(expr,x.expr)
  def *   (x:IntEx):Ex[Int] = ExMul(expr,x.expr)
  def %   (x:IntEx):Ex[Int] = ExMod(expr,x.expr)
  def <   (x:IntEx):Ex[Boolean] = ExLt(expr,x.expr)
  def >   (x:IntEx):Ex[Boolean] = ExGt(expr,x.expr)
  def <=  (x:IntEx):Ex[Boolean] = ExLte(expr,x.expr)
  def >=  (x:IntEx):Ex[Boolean] = ExGte(expr,x.expr)
}

case class BoolEx (expr:Ex[Boolean]) {
  def &&  (x:BoolEx):Ex[Boolean] = ExAnd(expr,x.expr)
  def ||  (x:BoolEx):Ex[Boolean] = ExOr(expr,x.expr)
  def ^   (x:BoolEx):Ex[Boolean] = ExXor(expr,x.expr)
  def === (x:BoolEx):Ex[Boolean] = ExEq[Boolean](expr,x.expr)
  def !== (x:BoolEx):Ex[Boolean] = ExEq[Boolean](expr,x.expr)
}

object Implicits {
  implicit def ExIntToIntEx(i:Ex[Int]):IntEx = IntEx(i)
  implicit def IntToIntEx(i:Int):IntEx = IntEx(ExLit[Int](i))
  implicit def ExBoolToBoolEx(b:Ex[Boolean]):BoolEx = BoolEx(b)
  implicit def BoolToIntEx(b:Boolean):BoolEx = BoolEx(ExLit[Boolean](b))
}

final case class ExLit[T](value:T) extends Ex[T]
final case class ExVar[T](name:Any) extends Ex[T]


final case class ExAnd(a:Ex[Boolean], b:Ex[Boolean]) extends Ex[Boolean]
final case class ExOr(a:Ex[Boolean], b:Ex[Boolean]) extends Ex[Boolean]
final case class ExXor(a:Ex[Boolean], b:Ex[Boolean]) extends Ex[Boolean]

final case class ExEq[T](a:Ex[T], b:Ex[T]) extends Ex[Boolean]
final case class ExNeq[T](a:Ex[T], b:Ex[T]) extends Ex[Boolean]
final case class ExLt(a:Ex[Int], b:Ex[Int]) extends Ex[Boolean]
final case class ExLte(a:Ex[Int], b:Ex[Int]) extends Ex[Boolean]
final case class ExGt(a:Ex[Int], b:Ex[Int]) extends Ex[Boolean]
final case class ExGte(a:Ex[Int], b:Ex[Int]) extends Ex[Boolean]
final case class ExAdd(a:Ex[Int], b:Ex[Int]) extends Ex[Int]
final case class ExSub(a:Ex[Int], b:Ex[Int]) extends Ex[Int]
final case class ExMul(a:Ex[Int], b:Ex[Int]) extends Ex[Int]
final case class ExMod(a:Ex[Int], b:Ex[Int]) extends Ex[Int]

object Ex {
  // evaluate an expression given a context that maps variable names to values
  def eval[T](e:Ex[T], ctx:Any=>Any):T = e match {
    case ExLit(v) => v.asInstanceOf[T]
    case ExVar(nm) => ctx(nm).asInstanceOf[T]
    case ExAnd(a,b) => eval(a,ctx) && eval(b,ctx)
    case ExOr(a,b) => eval(a,ctx) || eval(b,ctx)
    case ExXor(a,b) => eval(a,ctx) ^ eval(b,ctx)
    case e:ExEq[u] => eval(e.a,ctx) == eval(e.b,ctx)
    case e:ExNeq[u] => eval(e.a,ctx) != eval(e.b,ctx)
    case ExLt(a,b) => eval(a,ctx) < eval(b,ctx)
    case ExLte(a,b) => eval(a,ctx) <= eval(b,ctx)
    case ExGt(a,b) => eval(a,ctx) > eval(b,ctx)
    case ExGte(a,b) => eval(a,ctx) >= eval(b,ctx)
    case ExAdd(a,b) => eval(a,ctx) + eval(b,ctx)
    case ExSub(a,b) => eval(a,ctx) - eval(b,ctx)
    case ExMul(a,b) => eval(a,ctx) * eval(b,ctx)
    case ExMod(a,b) => eval(a,ctx) % eval(b,ctx)
  }
  
  // get shallow list of subexpressions
  def subExs(e:Ex[_]):List[Ex[_]] = e match {
    case ExLit(_) => Nil
    case ExVar(_) => Nil
    case ExAnd(a,b) => List(a,b)
    case ExOr(a,b) => List(a,b)
    case ExXor(a,b) => List(a,b)
    case ExEq(a,b) => List(a,b)
    case ExNeq(a,b) => List(a,b)
    case ExLt(a,b) => List(a,b)
    case ExLte(a,b) => List(a,b)
    case ExGt(a,b) => List(a,b)
    case ExGte(a,b) => List(a,b)
    case ExAdd(a,b) => List(a,b)
    case ExSub(a,b) => List(a,b)
    case ExMul(a,b) => List(a,b)
    case ExMod(a,b) => List(a,b)
  }
  
  // get all subexpressions including the expression given
  def unfurl(e:Ex[_]):List[Ex[_]] = 
    e :: (subExs(e) flatMap unfurl)
  
  // pretty-print expression
  def pretty(e:Ex[_]):String = {
    // precedence rank for deciding where to put parentheses
    def rank(e:Ex[_]):Int = e match {
      case e:ExAnd => 40
      case e:ExOr => 50
      case e:ExXor => 50
      case e:ExEq[_] => 30
      case e:ExNeq[_] => 30
      case e:ExLt => 30
      case e:ExLte => 30
      case e:ExGt => 30
      case e:ExGte => 30
      case e:ExAdd => 20
      case e:ExSub => 20
      case e:ExMul => 20
      case e:ExMod => 20
      case e:ExLit[_] => 0
      case e:ExVar[_] => 0
    }
    
    val r = rank(e)
    
    def term(t:Ex[_]):String = {
      val rt = rank(t)
      //if(rt >= r)
        "( " + t.toString + " )"
      //else
        //t.toString
    }

    import Implicits._
    e match {
      case ExLit(v) => v.toString
      case e:ExVar[_]=> "$"+e.name
      case ExAnd(a,b) => term(a)+" && "+term(b)
      case ExOr(a,b) => term(a)+" || "+term(b)
      case ExXor(a,b) => term(a)+" ^ "+term(b)
      case ExEq(a,b) => term(a)+" = "+term(b)
      case ExNeq(a,b) => term(a)+" != "+term(b)
      case ExLt(a,b) => term(a)+" < "+term(b)
      case ExLte(a,b) => term(a)+" <= "+term(b)
      case ExGt(a,b) => term(a)+" > "+term(b)
      case ExGte(a,b) => term(a)+" >= "+term(b)
      case ExAdd(a,b) => term(a)+" + "+term(b)
      case ExSub(a,b) => term(a)+" - "+term(b)
      case ExMul(a,b) => term(a)+" * "+term(b)
      case ExMod(a,b) => term(a)+" % "+term(b)
    }
  }
}
