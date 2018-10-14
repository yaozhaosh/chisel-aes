// Chisel implementation of AES
// Function: Key expansion
// Author: Yao Zhao

package aes

import chisel3._
import chisel3.util._

// key expansion: currently 1 iteration per cycle
class KeyExpansion (Nk: Int = 4)(key: UInt, startKeyExp: Bool){
  def Nr = Nk + 6
  def Nb = 4
  val iterCycles = (Nr+1)-Nk/4 // Nk=4:10, Nk=6:12, Nk=8:13
  val keyExpLen = (Nr+1)*Nb + (if (Nk==6) 2 else 0)

  // state machine
  val stateIDLE :: stateLOOP :: stateDONE :: Nil = Enum(3)
  val keyExpState = RegInit(stateIDLE)
  val loopCnt = RegInit(0.U(4.W)) // max 12 (Nk=8: 60 words, 13 rounds)
  val expKeyValid = RegInit(false.B)

  when (keyExpState === stateIDLE) {
    when (startKeyExp) {
      keyExpState := stateLOOP
      expKeyValid := false.B
    }
    loopCnt := 0.U
  } .elsewhen(keyExpState === stateLOOP) {
    when (loopCnt === (iterCycles.U - 1.U)) {
      keyExpState := stateDONE
    }
    loopCnt := loopCnt + 1.U
  } .elsewhen(keyExpState === stateDONE) {
    keyExpState := stateIDLE
    loopCnt := 0.U
    expKeyValid := true.B
  }

  // Nk=4: Nr=10, 44*32 (11*128)
  // Nk=6, Nr=12, 52*32 (13*128)
  // Nk=8: Nr=14, 60*32 (15*128)
  val w = RegInit(VecInit(Seq.fill(keyExpLen)(0.U(32.W)))) // [Nk*8-1:0] keyExpVec [0:Nr*4-1]

  val iw = Wire(Vec(4, UInt((Nb*8).W)))

  def rotWord (w:UInt): UInt = Cat(w(23,0), w(31,24))
  def subWord (w:UInt): UInt = Cat(Sbox(w(31,24)), Sbox(w(23,16)), Sbox(w(15,8)), Sbox(w(7,0)))

  // rcon Table
  val rconTable = Wire(Vec(10, UInt(8.W)))
  rconTable := VecInit(Array(0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1b, 0x36).map(_&0xff).map(_.U(8.W)))

  def loopCnt192b (loopCnt: UInt): UInt = {
    val lut = Wire(Vec(16, UInt(8.W)))
    lut := VecInit(Array(0, 1, 1, 2, 3, 3, 4, 5, 5, 6, 7, 7, 8, 9, 9, 10).map(_.U))
    lut(loopCnt)
  }

  // per round
  if (Nk == 4) { //128-bit
    iw(0) := subWord(rotWord(w(keyExpLen-1))) ^ Cat(rconTable(loopCnt(3,0)), 0.U(24.W)) ^ w(keyExpLen-Nk)
    for (i <- 1 to 3) {
      iw(i) := iw(i-1) ^ w(keyExpLen-Nk+i)
    }
  } else if (Nk == 6) { //192-bit
    when (VecInit(Array(0, 3, 6, 9).map(_.U(4.W))).contains(loopCnt)) { // rcon at the 1st word
      iw(0) := subWord(rotWord(w(keyExpLen - 1))) ^ Cat(rconTable(loopCnt192b(loopCnt)), 0.U(24.W)) ^ w(keyExpLen - Nk)
      for (i <- 1 to 3) {
        iw(i) := iw(i - 1) ^ w(keyExpLen - Nk + i)
      }
    } .elsewhen (VecInit(Array(1, 4, 7, 10).map(_.U(4.W))).contains(loopCnt))  { // rcon at the 3rd word
      iw(0) := w(keyExpLen-1) ^ w(keyExpLen-Nk)
      iw(1) := iw(0) ^ w(keyExpLen-Nk+1)
      iw(2) := subWord(rotWord(iw(1))) ^ Cat(rconTable(loopCnt192b(loopCnt)), 0.U(24.W)) ^ w(keyExpLen-Nk+2)
      iw(3) := iw(2) ^ w(keyExpLen-Nk+3)
    } .otherwise {
      iw(0) := w(keyExpLen-1) ^ w(keyExpLen-Nk)
      for (i <- 1 to 3) {
        iw(i) := iw(i-1) ^ w(keyExpLen-Nk+i)
      }
    }
  } else if (Nk == 8) { //256-bit
    when (loopCnt(0).toBool) { // odd cycle
      iw(0) := subWord(w(keyExpLen-1)) ^ w(keyExpLen-Nk)
    } .otherwise { // even cycle
      iw(0) := subWord(rotWord(w(keyExpLen-1))) ^ Cat(rconTable(loopCnt(3,1)), 0.U(24.W)) ^ w(keyExpLen-Nk)
    }
    for (i <- 1 to 3) {
      iw(i) := iw(i-1) ^ w(keyExpLen-Nk+i)
    }
  }

  // left shift the vector
  when (keyExpState === stateIDLE) {
    when (startKeyExp) {
      for (i <- 0 to keyExpLen-Nk-1) {
        w(i) := 0.U
      }
      for (i <- 0 to Nk-1) {
        w(keyExpLen-Nk+i) := VecInit((3 to 0 by -1).map(j => key(i*32+j*8+7, i*32+j*8))).asUInt
      }
    }
  } .elsewhen (keyExpState === stateLOOP) {
    for (i <- 0 to keyExpLen-5) {
      w(i) := w(i+4)
    }
    for (i <- 0 to 3) {
      w(keyExpLen-4+i) := iw(i)
    }
  }

}

object KeyExpansion {
  def apply(Nk: Int)(key: UInt, startKeyExp: Bool) = {
    val keyExpansion = new KeyExpansion(Nk)(key, startKeyExp)
    val keyExpSize = keyExpansion.Nr+1
    def reverseBytes(x: UInt):UInt = VecInit((3 to 0 by -1).map(i => x(8*i+7, 8*i))).asUInt
    (keyExpansion.expKeyValid, VecInit((0 until keyExpSize).map(i => VecInit((0 to 3).map(j => reverseBytes(keyExpansion.w(4*i+j)))).asUInt)))
  }
}