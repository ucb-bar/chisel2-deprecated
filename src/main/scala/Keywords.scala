package Chisel{

object KeywordCheck {
  
  def apply(word: String): Boolean = keywords contains word

  val keywords = List("reset", "clk");

}


}
