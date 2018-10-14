// Chisel implementation of AES
// Function: AES engine
// Author: Yao Zhao

package aes

import chisel3._
import chisel3.util._

class PipelineCipher (Nk: Int=4) (text: UInt, expKey: Vec[UInt]) {
  def Nr = Nk+6

  def addRoundKey(round: Int)(state: UInt): UInt = state ^ expKey(round)

  def cipherIteration(round: Int) (state: UInt): UInt = {
    val state1 = ShiftRows(SubBytes(state))
    addRoundKey(round)(if (round == Nr) state1 else MixColumns(state1))
  }
}

object PipelineCipher {
  def apply(Nk: Int)(text:UInt, expKey: Vec[UInt]): UInt = {
    val pc = new PipelineCipher(Nk)(text, expKey)
    val iniState = RegNext(pc.addRoundKey(0)(text)) //may be combined with next pipe
    val iterState = Reg(Vec(pc.Nr, UInt(128.W)))
    for (i <- 0 to pc.Nr-1) {
      iterState(i) := pc.cipherIteration(i+1)(if (i==0) iniState else iterState(i-1))
    }
    iterState(pc.Nr-1)
  }
}

class PipelineInvCipher (Nk: Int=4) (cipher: UInt, expKey: Vec[UInt]) {
  def Nr = Nk+6

  def addRoundKey(round: Int)(state: UInt): UInt = state ^ expKey(round)

  def invCipherIteration(round: Int) (state: UInt): UInt = {
    val state1 = addRoundKey(round)(InvSubBytes(InvShiftRows(state)))
    if (round == 0) state1 else InvMixColumns(state1)
  }
}

object PipelineInvCipher {
  def apply(Nk: Int)(cipher:UInt, expKey: Vec[UInt]): UInt = {
    val ipc = new PipelineInvCipher(Nk)(cipher, expKey)
    val iniState = RegNext(ipc.addRoundKey(ipc.Nr)(cipher)) //may be combined with next pipe
    val iterState = Reg(Vec(ipc.Nr, UInt(128.W)))
    for (i <- ipc.Nr-1 to 0 by -1) {
      iterState(i) := ipc.invCipherIteration(i)(if (i==ipc.Nr-1) iniState else iterState(i+1))
    }
    iterState(0)
  }
}