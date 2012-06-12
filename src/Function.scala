package Chisel
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import Component._

object Function {
  def apply(reg: Reg, out: Node, enable: Node, nodes: HashSet[Node]) = {
    val newName = reg.emitRef + "__cond_update"
    val params  = new ArrayBuffer[Node];
    val args    = new ArrayBuffer[Node];
    val argMap  = new HashMap[Node, Node];
    val res     = if(isClockGatingUpdates) new Function(reg, out) else new FunctionInline(reg, out);
    res.init(newName, out.width);
    val traced = new HashSet[Node];
    def trace (node: Node): Unit = {
      if (!traced.contains(node)) {
        traced += node;
        var i = 0;
        for (c <- node.inputs) {
          if (nodes.contains(c)) {
            trace(c);
          } else if (argMap.contains(c)) {
            node.inputs(i) = argMap(c);
            c.consumers   -= node;
          } else {
            if (c.isInstanceOf[Literal]) {
              val in = Literal(c.value, c.width)
              node.inputs(i) = in
              c.consumers -= node
              argMap(c) = in
            }
            else if (c.isByValue) {
              val in         = Bits(c.getWidth(), INPUT);
              in.name        = if (c.isLit) "T" + c.emitIndex else c.name;
              node.inputs(i) = in;
              c.consumers   -= node;
              c.consumers   += res;
              res.params    += in;
              args          += c;
              argMap(c)      = in;
            }
          }
          i += 1;
        }
        res.nodes += node;
      }
    }
    trace(out);
    // println("OUT " + name + " WIDTH " + out.width);
    res.setName(newName);
    res.inputs  += enable;
    res.inputs ++= args;
    res
  }
}

  abstract class AbstractFunction extends Node {
    val params = new ArrayBuffer[Node];
    val inMap  = new HashMap[Node, Node];
    val nodes  = new ArrayBuffer[Node];

    def decString: String;
    def defString(c: Component): String;
  }

  class Function(val default: Node, val out: Node) extends AbstractFunction {
    def returnTypeString = "dat_t<" + out.width + ">";
    def parametersString = {
      var res = "(";
      for (i <- 0 until params.size) {
        if (i > 0) res += ", ";
        res += "dat_t<" + params(i).width + "> " + params(i).emitRef;
        // res += " /* " + params(i) + " */ ";
      }
      res + ")";
    }
    def decString = "  " + returnTypeString + " do_" + name + parametersString + ";\n";
    def defString(c: Component) = {
      var res = returnTypeString + " " + c.name + "_t::do_" + name + parametersString + "{\n";
      for (node <- nodes) {
        res += node.emitDefLoC;
      }
      res += "  return " + out.emitRef + ";\n";
      res + "}\n";
    }
    override def emitDefLoC: String = {
      // TODO: COULD BE IMPROVED TO USE ?: EXPRESSION
      var res = "  dat_t<" + out.width + "> " + name + " = " + default.emitRef + ";\n";
      res += "  if (" + inputs(0).emitRef + ".to_bool())\n";
      res += "    " + name + " = do_" + name + "(";
      for (i <- 1 until inputs.size) {
        if (i > 1) res += ", ";
        res += inputs(i).emitRef;
        // res += " /* " + inputs(i) + " */ ";
      }
      res += ");\n";
      res
    }
  }



class FunctionInline(val default: Node, val out: Node) extends AbstractFunction {
  def decString = ""
  def defString(c: Component) = ""

  override def emitDefLoC: String = {
    // TODO: COULD BE IMPROVED TO USE ?: EXPRESSION
    var res = "  dat_t<" + out.width + "> " + name + " = " + default.emitRef + ";\n";
    res += "  if (" + inputs(0).emitRef + ".to_bool()) {\n";
    for (i <- 1 until inputs.size) {
      res += "    " + params(i-1).emitTmp + " = " + inputs(i).emitRef + ";\n"
    }
    for (node <- nodes) {
      res += "  " + node.emitDefLoC;
    }
    res += "    " + name + " = " + out.emitRef + ";\n";
    res += "  }\n";
    res
  }
}
