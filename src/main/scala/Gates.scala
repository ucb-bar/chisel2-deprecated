package Chisel {
import IOdir._;

object makeCell {
  def apply(x: int_t, y: int_t, op: String): int_t = {
    val res = new BinaryNodeCell(op);
    res.io.X := x;
    res.io.Y := y;
    res.io.Z
  }
  def apply(op: String): BinaryNodeCell = {
    new BinaryNodeCell(op);
  }
}

object AND {
  def apply(x: int_t, y: int_t): int_t = {
    makeCell(x, y, "&");
  }
  def apply(): BinaryNodeCell = {
    makeCell("&");
  }
}

object OR {
  def apply(x: int_t, y: int_t): int_t = {
    makeCell(x, y, "|");
  }
  def apply(): BinaryNodeCell = {
    makeCell("|");
  }
}

object XOR {
  def apply(x: int_t, y: int_t): int_t = {
    makeCell(x, y, "^");
  }
  def apply(): BinaryNodeCell = {
    makeCell("^")
  }
}

object NOT {
  def apply(in: int_t): int_t = {
    val res = new UnaryNodeCell("!");
    res.io.In := in;
    res.io.Out
  }
  def apply(): UnaryNodeCell = {
     new UnaryNodeCell("!");
  }
}


}
