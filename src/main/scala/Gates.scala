package Chisel {
import IOdir._;

object makeCell {
  def apply(x: Fix, y: Fix, op: String): Fix = {
    val res = new BinaryNodeCell(op)(Fix());
    res.io.X := x;
    res.io.Y := y;
    res.io.Z
  }
  def apply(op: String): BinaryNodeCell[Fix] = {
    new BinaryNodeCell(op)(Fix());
  }
}

object AND {
  def apply(x: Fix, y: Fix): Fix = {
    makeCell(x, y, "&");
  }
  def apply(): BinaryNodeCell[Fix] = {
    makeCell("&");
  }
}

object OR {
  def apply(x: Fix, y: Fix): Fix = {
    makeCell(x, y, "|");
  }
  def apply(): BinaryNodeCell[Fix] = {
    makeCell("|");
  }
}

object XOR {
  def apply(x: Fix, y: Fix): Fix = {
    makeCell(x, y, "^");
  }
  def apply(): BinaryNodeCell[Fix] = {
    makeCell("^")
  }
}

object NOT {
  def apply(in: Fix): Fix = {
    val res = new UnaryNodeCell("!")(Fix());
    res.io.In := in;
    res.io.Out
  }
  def apply(): UnaryNodeCell[Fix] = {
     new UnaryNodeCell("!")(Fix());
  }
}


}
