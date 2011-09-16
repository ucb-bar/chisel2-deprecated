package Chisel {
import IOdir._;

object makeCell {
  def apply(x: Bits, y: Bits, op: String): Bits = {
    val res = new BinaryNodeCell(op)(Bits());
    res.io.X := x;
    res.io.Y := y;
    res.io.Z
  }
  def apply(op: String): BinaryNodeCell[Bits] = {
    new BinaryNodeCell(op)(Bits());
  }
}

object AND {
  def apply(x: Bits, y: Bits): Bits = {
    makeCell(x, y, "&");
  }
  def apply(): BinaryNodeCell[Bits] = {
    makeCell("&");
  }
}

object OR {
  def apply(x: Bits, y: Bits): Bits = {
    makeCell(x, y, "|");
  }
  def apply(): BinaryNodeCell[Bits] = {
    makeCell("|");
  }
}

object XOR {
  def apply(x: Bits, y: Bits): Bits = {
    makeCell(x, y, "^");
  }
  def apply(): BinaryNodeCell[Bits] = {
    makeCell("^")
  }
}

object NOT {
  def apply(in: Bits): Bits = {
    val res = new UnaryNodeCell("!")(Bits());
    res.io.In := in;
    res.io.Out
  }
  def apply(): UnaryNodeCell[Bits] = {
     new UnaryNodeCell("!")(Bits());
  }
}


}
