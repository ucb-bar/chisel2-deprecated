package Chisel
import Node._
import scala.collection.mutable.ArrayBuffer

object ListLookup {
  def apply[T <: Bits](addr: Bits, default: List[T], mapping: Array[(Bits, List[T])]): List[T] = {
    val defaultNode = ListNode(default)
    val mappingNode = mapping.map(x => MapNode(x))
    val ll = new ListLookup[T]()
    ll.initOf("", widthOf(1), List(addr, defaultNode) ++ mappingNode)
    ll.wires = default.map(x => ListLookupRef(x, ll))
    // TODO: GENERALIZE AND SHARE THIS
    (default zip ll.wires).map{case(x, xRef) => {
      val res = x match {
	case bool: Bool => Bool(OUTPUT);
	case ufix: UFix => UFix(OUTPUT);
	case fix: Fix => Fix(OUTPUT);
	case bits: Bits => Bits(OUTPUT);
	case any => Bits(OUTPUT);
      }
      res.setIsTypeNode;
      xRef.nameHolder = res;
      res.inputs += xRef
      res.asInstanceOf[T]
    }}
    
  }
}

class ListLookup[T <: Bits] extends Node {
  var wires: List[ListLookupRef[T]] = null
  
  def addr = inputs(0)

  def defaultWires = inputs(1).inputs

  def map = {
    val mapArray = inputs.slice(2, inputs.length)
    val res = ArrayBuffer[(Node, ArrayBuffer[Node])]()
    for(elm <- mapArray)
      res += (elm.asInstanceOf[MapNode].addr -> elm.asInstanceOf[MapNode].data)
    res
  }

  override def toString: String = "LISTLOOKUP(" + inputs(0) + ")";

  override def isByValue: Boolean = false;
}

object ListLookupRef {
  def apply[T <: Bits](x: T, ll: ListLookup[T]): ListLookupRef[T] = {
    val res = new ListLookupRef[T]()
    res.init("", widthOf(1), ll, x)
    res
  }
}

class ListLookupRef[T <: Bits]() extends Node {
  override def toString: String = name
}

object ListNode {
  def apply[T <: Data](nodes: List[T]): ListNode = {
    val res = new ListNode()
    res.init("", widthOf(0), nodes: _*)
    res
  }
}

class ListNode extends Node {
}

object MapNode {
  def apply[T <: Bits](map: (Bits, List[T])): MapNode = {
    val res = new MapNode()
    res.initOf("", widthOf(0), List(map._1) ++ map._2)
    res
  }
}

class MapNode extends Node {
  def addr = inputs(0)
  def data = inputs.slice(1, inputs.length)
}
