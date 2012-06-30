package Chisel
import Node._
import Reg._
import Component._
import ChiselError._

class DotBackend extends Backend {
  override def emitTmp(node: Node): String = 
    emitRef(node)

  def isDottable (m: Node) = {
    if (m == m.component.reset) {
      false
    } else {
      m match {
        case x: Literal  => false;
        case x: MapNode  => false;
        case x: ListNode => false;
        case _           => true;
      }
    }
  }

  def compile(c: Component, base_name: String): Unit = {
    var gn = -1;
    val out_cd = new java.io.FileWriter(base_name + c.name + "_c.dot");
    out_cd.write("digraph TopTop {\n");
    out_cd.write("rankdir = LR;\n");
    def genNum = { gn += 1; gn };
    def dumpComponent (c: Component): Unit = {
      out_cd.write("subgraph cluster" + c.name + "{\n");
      out_cd.write("label = \"" + c.name + "\";\n");
      def dumpIo (n: String, d: Data): Unit = {
        d match {
          case b: Bundle => 
            out_cd.write("subgraph cluster" + n + "__" + genNum + "{\n");
            out_cd.write("node [shape=box];\n");
            out_cd.write("label = \"" + n + "\";\n");
            for ((cn, cd) <- b.elements)
              dumpIo(cn, cd);
            out_cd.write("}\n");
          case o => 
            out_cd.write(emitRef(d) + "[label=\"" + n + "\"];\n");
            for (in <- d.inputs) 
              if (isDottable(in))
                out_cd.write(emitRef(in) + " -> " + emitRef(d) + "[label=\"" + in.getWidth + "\"];\n");
        }
      }
      dumpIo("io", c.io);
      for (cc <- c.children) 
        dumpComponent(cc);
      out_cd.write("}\n");
    }
    dumpComponent(c);
    out_cd.write("}");
    out_cd.close();
    val out_d = new java.io.FileWriter(base_name + c.name + ".dot");
    out_d.write("digraph " + c.name + "{\n");
    out_d.write("rankdir = LR;\n");
    for (m <- c.mods) {
      if (isDottable(m)) {
        out_d.write(emitRef(m));
        var label  = m.dotName;
        val anyLit = m.inputs.find(x => !isDottable(x));
        if (!anyLit.isEmpty) {
          var i = 0;
          label += "(";
          for (in <- m.inputs) {
            if (i != 0) label += ", ";
            label += (if (in.isLit) emitRef(in) else "_");
            i += 1;
          }
          label += ")";
        }
        out_d.write("[label=\"" + label + "\"];\n");
      }
    }
    for (m <- c.mods) {
      for (in <- m.inputs) {
        if (isDottable(m) && isDottable(in)) 
          out_d.write("  " + emitRef(in) + " -> " + emitRef(m) + "[label=\"" + in.getWidth + "\"];\n");
      }
    }
    out_d.write("}");
    out_d.close();
  }
}
