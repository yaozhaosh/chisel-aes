// Chisel implementation of AES
// Function: AES trail test top
// Author: Yao Zhao

package aes

import chisel3._

object AesTrialMain extends App {
  iotesters.Driver.execute(args, () => new AesTrial(8)) {
    c => new AesTrialUnitTester(c)
  }
}

object AesTrialRepl extends App {
  iotesters.Driver.executeFirrtlRepl(args, () => new AesTrial(8))
}
