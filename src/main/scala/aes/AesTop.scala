// Chisel implementation of AES
// Function: AES top
// Author: Yao Zhao

package aes

import chisel3._
import chisel3.util._

class AesEngIntf (val engNum: Int=1) extends Bundle {
  val text = Input(Valid(Vec(engNum, UInt(128.W))))
  val cipher = Output(Valid(Vec(engNum, UInt(128.W))))
}

// user should not assert startKeyExp when keyExpReady=false or encEngReady=false or decEngReady=false
// user should not assert text.valid when encEngReady=false, and should not assert cipher.valid when decEngReady=false
class AesTop(Nk: Int=4, pipelineEng:Boolean, encEngNum: Int=1, decEngNum: Int=1) extends Module {
  val io = IO(new Bundle {
    val Nr = Nk+6
    val encIntf = new AesEngIntf(encEngNum)
    val decIntf = Flipped(new AesEngIntf(decEngNum))
    val key = Input(UInt((Nk*4*8).W))
    val startKeyExp = Input(Bool())
    val keyExpReady = Output(Bool())
    val encEngReady = Output(Bool())
    val decEngReady = Output(Bool())
  })

  val startKeyExp = RegInit(false.B)
  startKeyExp := io.startKeyExp
  val (expKeyValid, expKey) = KeyExpansion(Nk)(io.key, startKeyExp)

  def calcKeyExpReady (startKeyExp:Bool, keyExpValid:Bool): Bool = {
    val stKeyExpIDLE :: stKeyExpRUN :: Nil = Enum(2)
    val keyExpState = RegInit(stKeyExpIDLE)

    when (keyExpState === stKeyExpIDLE) {
      when (startKeyExp) {
        keyExpState := stKeyExpRUN
      }
    } .elsewhen (keyExpState === stKeyExpRUN) {
      when (expKeyValid) {
        keyExpState := stKeyExpIDLE
      }
    }

    (keyExpState === stKeyExpIDLE)
  }

  def calcEngReady (busyCycles: Int)(expKeyValid:Bool, engStart:Bool): Bool = {
    val stIDLE :: stREADY :: stRUN :: Nil = Enum(3)
    val engState = RegInit(stIDLE)
    val busyCnt = RegInit(0.U(5.W))

    when (engState === stIDLE) {
      when (expKeyValid) {
        engState := stREADY
      }
    } .elsewhen (engState === stREADY) {
      when (!io.keyExpReady) {
        engState := stIDLE
      } .elsewhen(engStart) {
        engState := (if (busyCycles == 0) stREADY else stRUN)
        busyCnt := busyCycles.U
      }
    } .elsewhen (engState === stRUN) {
      when (busyCnt === 1.U) {
        engState := stREADY
      } .otherwise {
        busyCnt := busyCnt - 1.U
      }
    }

    (engState === stREADY)
  }

  io.encEngReady := calcEngReady(if (pipelineEng) PipelineCipher.busyCycles(Nk) else IteratedCipher.busyCycles(Nk))(expKeyValid, io.encIntf.text.valid)
  io.decEngReady := calcEngReady(if (pipelineEng) PipelineInvCipher.busyCycles(Nk) else IteratedInvCipher.busyCycles(Nk))(expKeyValid, io.decIntf.cipher.valid)
  io.keyExpReady := calcKeyExpReady(startKeyExp, expKeyValid)

  io.encIntf.cipher.valid := ShiftRegister(io.encIntf.text.valid, if (pipelineEng) PipelineCipher.latencyCycles(Nk) else IteratedCipher.latencyCycles(Nk))
  io.encIntf.cipher.bits := io.encIntf.text.bits.map(if (pipelineEng) PipelineCipher(Nk)(_, expKey) else IteratedCipher(Nk)(_, io.encIntf.text.valid, expKey))

  io.decIntf.text.valid := ShiftRegister(io.decIntf.cipher.valid, if (pipelineEng) PipelineInvCipher.latencyCycles(Nk) else IteratedInvCipher.latencyCycles(Nk))
  io.decIntf.text.bits := io.decIntf.cipher.bits.map(if (pipelineEng) PipelineInvCipher(Nk)(_, expKey) else IteratedInvCipher(Nk)(_, io.decIntf.cipher.valid, expKey))
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
    val invCipherValid = Output(Bool())
    val keyExpReady = Output(Bool())
    val encEngReady = Output(Bool())
    val decEngReady = Output(Bool())
  })

  val aesTop = Module(new AesTop(Nk, false, 1, 1))
  aesTop.io.key := io.key
  aesTop.io.startKeyExp := io.startKeyExp
  aesTop.io.encIntf.text.bits(0) := io.text
  aesTop.io.encIntf.text.valid := io.textValid

  io.cipher := aesTop.io.encIntf.cipher.bits(0)
  aesTop.io.decIntf.cipher.valid := aesTop.io.encIntf.cipher.valid
  aesTop.io.decIntf.cipher := aesTop.io.encIntf.cipher
  io.invCipher := aesTop.io.decIntf.text.bits(0)

  io.invCipherValid := aesTop.io.decIntf.text.valid
  io.keyExpReady := aesTop.io.keyExpReady
  io.encEngReady := aesTop.io.encEngReady
  io.decEngReady := aesTop.io.decEngReady
}

