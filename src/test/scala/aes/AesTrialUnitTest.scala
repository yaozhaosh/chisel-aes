// Chisel implementation of AES
// Function: AES peek poke tester
// Author: Yao Zhao

package aes

import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

class AesTrialUnitTester(c: AesTrial) extends PeekPokeTester(c) {
  private val enc = c

  poke(enc.io.startKeyExp, false)
  poke(enc.io.textValid, false)
  poke(enc.io.key, BigInt(1, (0x1f to 0x0 by -1).map(_.toByte).toArray))
  poke(enc.io.text, BigInt(1, Array(0xff, 0xee, 0xdd, 0xcc, 0xbb, 0xaa, 0x99, 0x88, 0x77, 0x66, 0x55, 0x44, 0x33, 0x22, 0x11, 0x00).map(_.toByte)))
  step(1)
  poke(enc.io.startKeyExp, true)
  step(1)
  poke(enc.io.startKeyExp, false)
  step(20)
  poke(enc.io.textValid, true)
  step(1)
  poke(enc.io.textValid, false)
  step(80)
  val invCipher = peek (enc.io.invCipher)
  val cipher = peek(enc.io.cipher)
  printf("key:      %x\n", peek(enc.io.key))
  printf("text:     %x\n", peek(enc.io.text))
  printf("cipher:   %x\n", cipher)
  printf("invCipher:%x\n", invCipher)
}

class AesTrialTester extends ChiselFlatSpec {
  private val backendNames = if(firrtl.FileUtils.isCommandAvailable("verilator")) {
    Array("firrtl", "verilator")
  }
  else {
    Array("firrtl")
  }
  for ( backendName <- backendNames ) {
    "AesTrial" should s"calculate proper greatest common denominator (with $backendName)" in {
      Driver(() => new AesTrial, backendName) {
        c => new AesTrialUnitTester(c)
      } should be (true)
    }
  }

  "Basic test using Driver.execute" should "be used as an alternative way to run specification" in {
    iotesters.Driver.execute(Array(), () => new AesTrial) {
      c => new AesTrialUnitTester(c)
    } should be (true)
  }

  "using --backend-name verilator" should "be an alternative way to run using verilator" in {
    if(backendNames.contains("verilator")) {
      iotesters.Driver.execute(Array("--backend-name", "verilator"), () => new AesTrial) {
        c => new AesTrialUnitTester(c)
      } should be(true)
    }
  }

  "running with --is-verbose" should "show more about what's going on in your tester" in {
    iotesters.Driver.execute(Array("--is-verbose"), () => new AesTrial) {
      c => new AesTrialUnitTester(c)
    } should be(true)
  }

  "running with --fint-write-vcd" should "create a vcd file from your test" in {
    iotesters.Driver.execute(Array("--fint-write-vcd"), () => new AesTrial) {
      c => new AesTrialUnitTester(c)
    } should be(true)
  }

  "using --help" should s"show the many options available" in {
    iotesters.Driver.execute(Array("--help"), () => new AesTrial) {
      c => new AesTrialUnitTester(c)
    } should be (true)
  }
}
