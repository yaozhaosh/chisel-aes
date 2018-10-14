// Chisel implementation of AES
// Function: AES utilities
// Author: Yao Zhao

package aes

import chisel3._
import chisel3.util._
import aesref.AesLut

object Sbox {
  val subByteTable = Wire(Vec(256, UInt(8.W)))
  subByteTable := VecInit(AesLut.subByteTable.map(_&0xff).map(_.U(8.W)))
  def apply(x: UInt):UInt = subByteTable(x)
}

object InvSbox {
  val invSubByteTable = Wire(Vec(256, UInt(8.W)))
  invSubByteTable := VecInit(AesLut.invSubByteTable.map(_&0xff).map(_.U(8.W)))
  def apply(x: UInt):UInt = invSubByteTable(x)
}

object States {
  def apply(x:UInt):Vec[UInt] = VecInit((0 to 15).map(i => x(i*8+7, i*8)))
}

object SubBytes {
  def apply(x: UInt):UInt = VecInit(States(x).map(a => Sbox(a))).asUInt
}

object InvSubBytes {
  def apply(x: UInt):UInt = VecInit(States(x).map(a => InvSbox(a))).asUInt
}

object ShiftRows {
  def apply(x: UInt):UInt = {
    val states = States(x)
    VecInit(
       Array(states(0), states(5), states(10), states(15),
             states(4), states(9), states(14), states(3),
             states(8), states(13), states(2), states(7),
             states(12), states(1), states(6), states(11))
    ).asUInt
  }
}

object InvShiftRows {
  def apply(x: UInt):UInt = {
    val states = States(x)
    VecInit(
      Array(states(0), states(13), states(10), states(7),
            states(4), states(1), states(14), states(11),
            states(8), states(5), states(2), states(15),
            states(12), states(9), states(6), states(3))
    ).asUInt
  }
}

trait PolyMult {
  def x01(x:UInt):UInt = x
  def x02(x:UInt):UInt = Mux(x(7).toBool, Cat(x(6,0), 0.U(1.W)) ^ 0x1b.U(8.W), Cat(x(6,0), 0.U(1.W)))
  def x03(x:UInt):UInt = x02(x) ^ x

  def x09(b: UInt): UInt = x02(x02(x02(b))) ^ b
  def x0b(b: UInt): UInt = x02(x02(x02(b))) ^ x02(b) ^ b
  def x0d(b: UInt): UInt = x02(x02(x02(b))) ^ x02(x02(b)) ^ b
  def x0e(b: UInt): UInt = x02(x02(x02(b))) ^ x02(x02(b)) ^ x02(b)
}

object MixColumns extends PolyMult {
  def apply(x: UInt): UInt = {
    val states = States(x)
    VecInit(
      (0 to 3).map(i => VecInit(
        Array(
          x02(states(i*4+0)) ^ x03(states(i*4+1)) ^ x01(states(i*4+2)) ^ x01(states(i*4+3)),
          x01(states(i*4+0)) ^ x02(states(i*4+1)) ^ x03(states(i*4+2)) ^ x01(states(i*4+3)),
          x01(states(i*4+0)) ^ x01(states(i*4+1)) ^ x02(states(i*4+2)) ^ x03(states(i*4+3)),
          x03(states(i*4+0)) ^ x01(states(i*4+1)) ^ x01(states(i*4+2)) ^ x02(states(i*4+3))
        )
      ).asUInt)
    ).asUInt
  }
}

object InvMixColumns extends PolyMult {
  def apply(x: UInt): UInt = {
    val states = States(x)
    VecInit(
      (0 to 3).map(i => VecInit(
        Array(
          x0e(states(i*4+0)) ^ x0b(states(i*4+1)) ^ x0d(states(i*4+2)) ^ x09(states(i*4+3)),
          x09(states(i*4+0)) ^ x0e(states(i*4+1)) ^ x0b(states(i*4+2)) ^ x0d(states(i*4+3)),
          x0d(states(i*4+0)) ^ x09(states(i*4+1)) ^ x0e(states(i*4+2)) ^ x0b(states(i*4+3)),
          x0b(states(i*4+0)) ^ x0d(states(i*4+1)) ^ x09(states(i*4+2)) ^ x0e(states(i*4+3))
        )
      ).asUInt)
    ).asUInt
  }
}