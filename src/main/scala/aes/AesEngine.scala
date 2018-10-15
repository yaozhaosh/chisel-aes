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

  def latencyCycles(Nk: Int): Int = Nk + 7 // latency: Nr+1
  def busyCycles(Nk: Int): Int = 0 // ready for each cycle

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

class IteratedCipher (Nk: Int=4) (text: UInt, expKey: Vec[UInt]) {
  def Nr = Nk+6

  def addRoundKey(round: UInt, state: UInt): UInt = state ^ expKey(round)

  def cipherIteration(round: UInt, state: UInt): UInt = {
    val state1 = ShiftRows(SubBytes(state))
    val state2 = Wire(UInt(128.W))
    when (round === Nr.U(4.W)) {
      state2 := state1
    } .otherwise {
      state2 := MixColumns(state1)
    }
    addRoundKey(round, state2)
  }
}

object IteratedCipher {

  def latencyCycles(Nk: Int): Int = Nk + 7 // latency: Nr+1
  def busyCycles(Nk: Int): Int = Nk + 6 // busy for Nr cycles

  def apply(Nk: Int)(text:UInt, textValid:Bool, expKey: Vec[UInt]): UInt = {
    val pc = new IteratedCipher(Nk)(text, expKey)
    val round = RegInit(0.U(4.W))
    val iterState = Reg(UInt(128.W))

    val stIDLE :: stRUN :: Nil = Enum(2)
    val encState = RegInit(stIDLE)

    when (encState === stIDLE) {
      when (textValid) {
        encState := stRUN
        round := 1.U
        iterState := pc.addRoundKey(0.U, text) // round 0
      }
    } .elsewhen(encState === stRUN) {
      when (round === (pc.Nr).U) {
        encState := stIDLE
        round := 0.U
      } .otherwise {
        round := round + 1.U
      }
      iterState := pc.cipherIteration(round, iterState)
    }

    iterState
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

  def latencyCycles(Nk: Int): Int = Nk + 7 // latency: Nr+1
  def busyCycles(Nk: Int): Int = 0 //  ready for each cycle

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

class IteratedInvCipher (Nk: Int=4) (cipher: UInt, expKey: Vec[UInt]) {
  def Nr = Nk+6

  def addRoundKey(round: UInt, state: UInt): UInt = state ^ expKey(round)

  def invCipherIteration(round: UInt, state: UInt): UInt = {
    val state1 = addRoundKey(round, InvSubBytes(InvShiftRows(state)))
    val state2 = Wire(UInt(128.W))
    when (round === 0.U(4.W)) {
      state2 := state1
    } .otherwise {
      state2 := InvMixColumns(state1)
    }
    state2
  }
}

object IteratedInvCipher {

  def latencyCycles(Nk: Int): Int = Nk + 7 // latency: Nr+1
  def busyCycles(Nk: Int): Int = Nk + 6 //  ready for each cycle

  def apply(Nk: Int)(cipher:UInt, cipherValid: Bool, expKey: Vec[UInt]): UInt = {
    val ipc = new IteratedInvCipher(Nk)(cipher, expKey)
    val round = RegInit(0.U(4.W))
    val iterState = Reg(UInt(128.W))

    val stIDLE :: stRUN :: Nil = Enum(2)
    val decState = RegInit(stIDLE)

    when (decState === stIDLE) {
      when (cipherValid) {
        decState := stRUN
        round := (ipc.Nr-1).U
        iterState := ipc.addRoundKey((ipc.Nr).U(4.W), cipher) // round Nr
      }
    } .elsewhen(decState === stRUN) {
      when (round === 0.U) {
        decState := stIDLE
        round := (ipc.Nr).U
      } .otherwise {
        round := round - 1.U
      }
      iterState := ipc.invCipherIteration(round, iterState)
    }

    iterState
  }
}