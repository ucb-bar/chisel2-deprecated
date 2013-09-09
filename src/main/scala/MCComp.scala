package Chisel

import Module._

class TransactionalBundle extends Bundle {
  
  val req: ValidIO[Data] = new ValidIO(Bits()).flip()
  val resp = Bits(OUTPUT)
  
}

abstract class TransactionalComponent extends Module {
  val io: TransactionalBundle
  tcomponents += this

  var acceptBackPressure = true
  val req_ready: Bool = Bool(false)
  val resp_ready: Bool = Bool(false)
  val resp_valid: Bool = Bool(false)
  

}
