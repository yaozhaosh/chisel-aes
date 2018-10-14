// Chisel implementation of AES
// Function: AES top
// Author: Yao Zhao

package aes

import chisel3._
import chisel3.util._

class AesEngIntf (val engNum: Int=1) extends Bundle {
  val text = Input(Vec(engNum, UInt(128.W)))
  val textValid = Input(Bool())
  val cipher = Output(Vec(engNum, UInt(128.W)))
  val cipherValid = Output(Bool())
}

class AesTop(Nk: Int=4, encEngNum: Int=1, decEngNum: Int=1) extends Module {
  val io = IO(new Bundle {
    val Nr = Nk+6
    val encIntf = new AesEngIntf(encEngNum)
    val decIntf = Flipped(new AesEngIntf(decEngNum))
    val key = Input(UInt((Nk*4*8).W))
    val startKeyExp = Input(Bool())
  })

  val startKeyExp = RegInit(false.B)
  startKeyExp := io.startKeyExp
  val (expKeyValid, expKey) = KeyExpansion(Nk)(io.key, startKeyExp)

  // currently expKeyValid is not used, we need to guarantee encryption/decryption starts after expKeyValid=1

  io.encIntf.cipherValid := ShiftRegister(io.encIntf.textValid, io.Nr+1)
  io.encIntf.cipher := io.encIntf.text.map(PipelineCipher(Nk)(_, expKey))

  io.decIntf.textValid := ShiftRegister(io.decIntf.cipherValid, io.Nr+1)
  io.decIntf.text := io.decIntf.cipher.map(PipelineInvCipher(Nk)(_, expKey))
}

class AesTrial(Nk: Int = 4) extends Module{
  val io = IO(new Bundle{
    val Nr = Nk+6
    val startKeyExp = Input(Bool())
    val key = Input(UInt((Nk*4*8).W))
    val text = Input(UInt(128.W))
    val textValid = Input(Bool())
    val invCipher = Output(UInt(128.W))
    val cipher = Output(UInt(128.W))
  })

  val aesTop = Module(new AesTop(Nk, 1, 1))
  aesTop.io.key := io.key
  aesTop.io.startKeyExp := io.startKeyExp
  aesTop.io.encIntf.text(0) := io.text
  aesTop.io.encIntf.textValid := io.textValid

  io.cipher := aesTop.io.encIntf.cipher(0)
  aesTop.io.decIntf.cipherValid := aesTop.io.encIntf.cipherValid
  aesTop.io.decIntf.cipher := aesTop.io.encIntf.cipher
  io.invCipher := aesTop.io.decIntf.text(0)
}

