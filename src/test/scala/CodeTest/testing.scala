import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.simulator.WriteVcdAnnotation
import Fundamental_IC._
import FP_submodules._
import org.scalatest.matchers.must.Matchers  // your modules


class FPAddSubTest10CyclesUnsigned extends AnyFlatSpec with ChiselScalatestTester {

  val bw = 4
  val pd = 1

  // Decode bits to float, unsigned FP format (sign always zero)
  //  def binToFloat(bin: Int, bw: Int): Double = {
  //    val expWidth = 2
  //    val fracWidth = bw - 1 - expWidth
  //    val exp = (bin >> fracWidth) & ((1 << expWidth) - 1)
  //    val frac = bin & ((1 << fracWidth) - 1)
  //    val bias = (1 << (expWidth - 1)) - 1
  //    val exponent = exp - bias
  //    val mantissa = 1.0 + frac.toDouble / (1 << fracWidth)
  //    if (bin == 0) 0.0 else mantissa * math.pow(2, exponent)
  //  }
  def binToFloat(bin: Int, bw: Int): Double = {
    val expWidth  = 2
    val fracWidth = bw - expWidth
    val expMask   = (1 << expWidth) - 1
    val fracMask  = (1 << fracWidth) - 1

    val exp  = (bin >> fracWidth) & expMask          // top 2 bits
    val frac = bin & fracMask                        // low 2 bits
    val bias = 1

    if (exp == 0) {
      // subnormal: value = (0.MM) * 2^(1-bias) -> here = (frac / 4) * 1
      frac.toDouble / (1 << fracWidth)
    } else {
      val mant = 1.0 + frac.toDouble / (1 << fracWidth)
      val eUnb = exp - bias                          // 01->0, 10->1, 11->2
      mant * math.pow(2.0, eUnb.toDouble)
    }
  }

  // Encode float to unsigned FP bits (sign bit = 0 always)
  //  def floatToBin(value: Double, bw: Int): Int = {
  //    val expWidth = 2
  //    val fracWidth = bw - 1 - expWidth
  //    if (value <= 0.0) return 0
  //    val bias = (1 << (expWidth - 1)) - 1
  //    val exponent = math.floor(math.log(value) / math.log(2)).toInt
  //    val expBits = exponent + bias
  //    val frac = value / math.pow(2, exponent) - 1.0
  //    val fracBits = (frac * (1 << fracWidth)).toInt
  //    (expBits << fracWidth) | (fracBits & ((1 << fracWidth) - 1))
  //  }

  def floatToBin(v: Double, bw: Int): Int = {
    val expWidth  = 2
    val fracWidth = bw - expWidth
    val bias      = 1
    val maxUnbE   = 2        // we allow E=11 as a normal exponent (no specials)

    if (v <= 0.0) return 0

    if (v < 1.0) {
      // subnormal: E=00, v = MM / 2^fracWidth
      val mm = math.round(v * (1 << fracWidth)).toInt
      val mmClamped = mm.max(0).min((1 << fracWidth) - 1)
      mmClamped
    } else {
      // normal
      var e = math.floor(math.log(v) / math.log(2)).toInt   // unbiased exponent
      if (e > maxUnbE) e = maxUnbE                          // clamp
      val scale = math.pow(2.0, e.toDouble)
      val mant  = (v / scale).min(2.0 - math.ulp(2.0)).max(1.0) // keep in [1,2)
      val mm    = math.round((mant - 1.0) * (1 << fracWidth)).toInt
      val mmClamped = mm.max(0).min((1 << fracWidth) - 1)
      val E = e + bias // rebias: 0->1,1->2,2->3
      (E << fracWidth) | mmClamped
    }
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

import scala.util.{Random => ScalaRandom}

class FPAddSubTest10CyclesUnsigned2 extends AnyFlatSpec with ChiselScalatestTester {

  val bw = 4
  val pd = 1

  // --- EE/MM helpers (no sign, subnormals) ---
  def binToFloat(bin: Int, bw: Int): Double = {
    val expWidth  = 2
    val fracWidth = bw - expWidth
    val expMask   = (1 << expWidth) - 1
    val fracMask  = (1 << fracWidth) - 1
    val exp  = (bin >> fracWidth) & expMask
    val frac = bin & fracMask
    val bias = 1
    if (exp == 0) frac.toDouble / (1 << fracWidth)
    else {
      val mant = 1.0 + frac.toDouble / (1 << fracWidth)
      val eUnb = exp - bias
      mant * math.pow(2.0, eUnb.toDouble)
    }
  }

  def floatToBin(v: Double, bw: Int): Int = {
    val expWidth  = 2
    val fracWidth = bw - expWidth
    val bias      = 1
    val maxUnbE   = 2
    if (v <= 0.0) return 0
    if (v < 1.0) {
      val mm = math.round(v * (1 << fracWidth)).toInt
      (mm max 0) min ((1 << fracWidth) - 1)
    } else {
      var e = math.floor(math.log(v) / math.log(2)).toInt
      if (e > maxUnbE) e = maxUnbE
      val scale = math.pow(2.0, e.toDouble)
      val mant  = (v / scale).min(2.0 - 1e-12).max(1.0)
      val mm    = math.round((mant - 1.0) * (1 << fracWidth)).toInt
      val mmC   = (mm max 0) min ((1 << fracWidth) - 1)
      val E     = e + bias
      (E << fracWidth) | mmC
    }
  }

  private def bN(x: Int, w: Int) = x.toBinaryString.reverse.padTo(w, '0').reverse

  behavior of s"FPAdder (bw=$bw, pd=$pd)"

  it should "print compact random checks" in {
    test(new FPAdder(bw, pd)) { dut =>
      val rnd = new ScalaRandom(0xC0FFEE)
      val N   = 40
      val Q   = scala.collection.mutable.Queue.empty[(Int, Int, Int)]

      for (cycle <- 0 until N) {
        val a  = rnd.nextInt(1 << bw)
        val b  = rnd.nextInt(1 << bw)
        val vi = true // set to rnd.nextBoolean() if you want bubbles

        val expBits = floatToBin(binToFloat(a, bw) + binToFloat(b, bw), bw)
        dut.io.in_a.poke(a.U)
        dut.io.in_b.poke(b.U)
        dut.io.valid_in.poke(vi.B)
        if (vi) Q.enqueue((a, b, expBits))

        dut.clock.step(1)

        if (dut.io.valid_out.peek().litToBoolean && Q.nonEmpty) {
          val (ea, eb, expOut) = Q.dequeue()
          val outBits = dut.io.out.peek().litValue.toInt

          val line =
            f"[Cycle $cycle%2d] A: ${bN(ea,bw)} (${binToFloat(ea,bw)}%1.4f), " +
              f"B: ${bN(eb,bw)} (${binToFloat(eb,bw)}%1.4f) => " +
              f"Out: ${bN(outBits,bw)} (${binToFloat(outBits,bw)}%1.4f), " +
              f"Expected: ${bN(expOut,bw)} (${binToFloat(expOut,bw)}%1.4f) " +
              (if (outBits == expOut) "✅" else "❌")

          println(line)
        }
      }

      // drain
      dut.io.valid_in.poke(false.B)
      for (_ <- 0 until pd) {
        dut.clock.step(1)
        if (dut.io.valid_out.peek().litToBoolean && Q.nonEmpty) {
          val (ea, eb, expOut) = Q.dequeue()
          val outBits = dut.io.out.peek().litValue.toInt
          val line =
            f"[drain] A: ${bN(ea,bw)} (${binToFloat(ea,bw)}%1.4f), " +
              f"B: ${bN(eb,bw)} (${binToFloat(eb,bw)}%1.4f) => " +
              f"Out: ${bN(outBits,bw)} (${binToFloat(outBits,bw)}%1.4f), " +
              f"Expected: ${bN(expOut,bw)} (${binToFloat(expOut,bw)}%1.4f) " +
              (if (outBits == expOut) "✅" else "❌")
          println(line)
        }
      }
    }
  }

  it should "print compact exhaustive checks (16x16)" in {
    test(new FPAdder(bw, pd)) { dut =>
      val Q = scala.collection.mutable.Queue.empty[(Int, Int, Int)]
      var cycle = 0

      for (a <- 0 until (1 << bw); b <- 0 until (1 << bw)) {
        val expBits = floatToBin(binToFloat(a, bw) + binToFloat(b, bw), bw)
        dut.io.in_a.poke(a.U)
        dut.io.in_b.poke(b.U)
        dut.io.valid_in.poke(true.B)
        Q.enqueue((a, b, expBits))

        dut.clock.step(1)

        if (dut.io.valid_out.peek().litToBoolean && Q.nonEmpty) {
          val (ea, eb, expOut) = Q.dequeue()
          val outBits = dut.io.out.peek().litValue.toInt
          val line =
            f"[Cycle $cycle%3d] A: ${bN(ea,bw)} (${binToFloat(ea,bw)}%1.4f), " +
              f"B: ${bN(eb,bw)} (${binToFloat(eb,bw)}%1.4f) => " +
              f"Out: ${bN(outBits,bw)} (${binToFloat(outBits,bw)}%1.4f), " +
              f"Expected: ${bN(expOut,bw)} (${binToFloat(expOut,bw)}%1.4f) " +
              (if (outBits == expOut) "✅" else "❌")
          println(line)
        }
        cycle += 1
      }

      // drain
      dut.io.valid_in.poke(false.B)
      for (_ <- 0 until pd) {
        dut.clock.step(1)
        if (dut.io.valid_out.peek().litToBoolean && Q.nonEmpty) {
          val (ea, eb, expOut) = Q.dequeue()
          val outBits = dut.io.out.peek().litValue.toInt
          val line =
            f"[drain] A: ${bN(ea,bw)} (${binToFloat(ea,bw)}%1.4f), " +
              f"B: ${bN(eb,bw)} (${binToFloat(eb,bw)}%1.4f) => " +
              f"Out: ${bN(outBits,bw)} (${binToFloat(outBits,bw)}%1.4f), " +
              f"Expected: ${bN(expOut,bw)} (${binToFloat(expOut,bw)}%1.4f) " +
              (if (outBits == expOut) "✅" else "❌")
          println(line)
        }
      }
    }
  }
}