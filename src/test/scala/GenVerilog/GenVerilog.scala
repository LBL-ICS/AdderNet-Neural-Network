package GenVerilog

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.simulator.WriteVcdAnnotation

import Fundamental_IC._  // your modules

class FPAddSubTest10CyclesUnsigned extends AnyFlatSpec with ChiselScalatestTester {

  val bw = 4
  val pd = 1

  // Decode bits to float, unsigned FP format (sign always zero)
  def binToFloat(bin: Int, bw: Int): Double = {
    val expWidth = 2
    val fracWidth = bw - 1 - expWidth
    val exp = (bin >> fracWidth) & ((1 << expWidth) - 1)
    val frac = bin & ((1 << fracWidth) - 1)
    val bias = (1 << (expWidth - 1)) - 1
    val exponent = exp - bias
    val mantissa = 1.0 + frac.toDouble / (1 << fracWidth)
    if (bin == 0) 0.0 else mantissa * math.pow(2, exponent)
  }

  // Encode float to unsigned FP bits (sign bit = 0 always)
  def floatToBin(value: Double, bw: Int): Int = {
    val expWidth = 2
    val fracWidth = bw - 1 - expWidth
    if (value <= 0.0) return 0
    val bias = (1 << (expWidth - 1)) - 1
    val exponent = math.floor(math.log(value) / math.log(2)).toInt
    val expBits = exponent + bias
    val frac = value / math.pow(2, exponent) - 1.0
    val fracBits = (frac * (1 << fracWidth)).toInt
    (expBits << fracWidth) | (fracBits & ((1 << fracWidth) - 1))
  }

  behavior of s"FPAdder (bw=$bw, pd=$pd) unsigned inputs only"

  it should "test first 10 input combos a=0, b=0..9" in {
    test(new FPAdder(bw, pd)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val inputs = (0 until 10).map(b => (0, b))
      val expectedQueue = scala.collection.mutable.Queue.empty[(Int, Int, Int)]

      for (((a, b), cycle) <- inputs.zipWithIndex) {
        val expected = floatToBin(binToFloat(a, bw) + binToFloat(b, bw), bw)

        dut.io.in_a.poke(a.U)
        dut.io.in_b.poke(b.U)
        dut.io.valid_in.poke(true.B)

        expectedQueue.enqueue((a, b, expected))

        dut.clock.step(1)

        if (expectedQueue.nonEmpty) {
          val (ea, eb, expOut) = expectedQueue.dequeue()
          val out = dut.io.out.peek().litValue.toInt

          val aFP = binToFloat(ea, bw)
          val bFP = binToFloat(eb, bw)
          val outFP = binToFloat(out, bw)
          val expFP = binToFloat(expOut, bw)

          val aBin = ea.toBinaryString.reverse.padTo(bw, '0').reverse
          val bBin = eb.toBinaryString.reverse.padTo(bw, '0').reverse
          val oBin = out.toBinaryString.reverse.padTo(bw, '0').reverse
          val expBin = expOut.toBinaryString.reverse.padTo(bw, '0').reverse

          val status = if (out == expOut) "✅" else "❌"

          println(f"[Cycle $cycle%2d] A: $aBin ($aFP%1.4f), B: $bBin ($bFP%1.4f) => Out: $oBin ($outFP%1.4f), Expected: $expBin ($expFP%1.4f) $status")
        }
      }

      for (cycle <- inputs.size until inputs.size + pd) {
        dut.io.in_a.poke(0.U)
        dut.io.in_b.poke(0.U)
        dut.io.valid_in.poke(false.B)

        dut.clock.step(1)

        if (expectedQueue.nonEmpty) {
          val (ea, eb, expOut) = expectedQueue.dequeue()
          val out = dut.io.out.peek().litValue.toInt

          val aFP = binToFloat(ea, bw)
          val bFP = binToFloat(eb, bw)
          val outFP = binToFloat(out, bw)
          val expFP = binToFloat(expOut, bw)

          val aBin = ea.toBinaryString.reverse.padTo(bw, '0').reverse
          val bBin = eb.toBinaryString.reverse.padTo(bw, '0').reverse
          val oBin = out.toBinaryString.reverse.padTo(bw, '0').reverse
          val expBin = expOut.toBinaryString.reverse.padTo(bw, '0').reverse

          val status = if (out == expOut) "✅" else "❌"

          println(f"[Cycle $cycle%2d] A: $aBin ($aFP%1.4f), B: $bBin ($bFP%1.4f) => Out: $oBin ($outFP%1.4f), Expected: $expBin ($expFP%1.4f) $status")
        }
      }
    }
  }

  behavior of s"FPSubtractorAbs (bw=$bw, pd=$pd) unsigned inputs only"

  it should "test first 10 input combos a=0, b=0..9" in {
    test(new FPSubtractorAbs(bw, pd)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val inputs = (0 until 10).map(b => (0, b))
      val expectedQueue = scala.collection.mutable.Queue.empty[(Int, Int, Int)]

      for (((a, b), cycle) <- inputs.zipWithIndex) {
        // Use absolute difference, so expected = |a - b|
        val expected = floatToBin(math.abs(binToFloat(a, bw) - binToFloat(b, bw)), bw)

        dut.io.in_a.poke(a.U)
        dut.io.in_b.poke(b.U)
        dut.io.valid_in.poke(true.B)

        expectedQueue.enqueue((a, b, expected))

        dut.clock.step(1)

        if (expectedQueue.nonEmpty) {
          val (ea, eb, expOut) = expectedQueue.dequeue()
          val out = dut.io.out.peek().litValue.toInt

          val aFP = binToFloat(ea, bw)
          val bFP = binToFloat(eb, bw)
          val outFP = binToFloat(out, bw)
          val expFP = binToFloat(expOut, bw)

          val aBin = ea.toBinaryString.reverse.padTo(bw, '0').reverse
          val bBin = eb.toBinaryString.reverse.padTo(bw, '0').reverse
          val oBin = out.toBinaryString.reverse.padTo(bw, '0').reverse
          val expBin = expOut.toBinaryString.reverse.padTo(bw, '0').reverse

          val status = if (out == expOut) "✅" else "❌"

          println(f"[Cycle $cycle%2d] A: $aBin ($aFP%1.4f), B: $bBin ($bFP%1.4f) => Out: $oBin ($outFP%1.4f), Expected: $expBin ($expFP%1.4f) $status")
        }
      }

      for (cycle <- inputs.size until inputs.size + pd) {
        dut.io.in_a.poke(0.U)
        dut.io.in_b.poke(0.U)
        dut.io.valid_in.poke(false.B)

        dut.clock.step(1)

        if (expectedQueue.nonEmpty) {
          val (ea, eb, expOut) = expectedQueue.dequeue()
          val out = dut.io.out.peek().litValue.toInt

          val aFP = binToFloat(ea, bw)
          val bFP = binToFloat(eb, bw)
          val outFP = binToFloat(out, bw)
          val expFP = binToFloat(expOut, bw)

          val aBin = ea.toBinaryString.reverse.padTo(bw, '0').reverse
          val bBin = eb.toBinaryString.reverse.padTo(bw, '0').reverse
          val oBin = out.toBinaryString.reverse.padTo(bw, '0').reverse
          val expBin = expOut.toBinaryString.reverse.padTo(bw, '0').reverse

          val status = if (out == expOut) "✅" else "❌"

          println(f"[Cycle $cycle%2d] A: $aBin ($aFP%1.4f), B: $bBin ($bFP%1.4f) => Out: $oBin ($outFP%1.4f), Expected: $expBin ($expFP%1.4f) $status")
        }
      }
    }
  }
}
