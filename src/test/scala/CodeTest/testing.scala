import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.simulator.WriteVcdAnnotation
import Fundamental_IC._
import FP_submodules._
import chisel3.util.log2Ceil
import org.scalatest.matchers.must.Matchers
import SAD_Code._
import scala.math.pow
import scala.util.{Random => ScalaRandom}


class FPAddTest extends AnyFlatSpec with ChiselScalatestTester {


  val bw      = 5
  val expSize = 2
  val pd      = 1

  private val biasCalc = (1 << (expSize - 1)) - 1


  def binToFloat(bin: Int, bw: Int): Double = {
    val expWidth  = expSize
    val fracWidth = bw - expWidth
    val bias      = biasCalc
    val expMask   = (1 << expWidth) - 1
    val fracMask  = (1 << fracWidth) - 1

    val exp  = (bin >>> fracWidth) & expMask
    val frac = bin & fracMask

    if (exp == 0) {
      if (frac == 0) 0.0
      else (frac.toDouble / (1 << fracWidth)) * math.pow(2.0, (1 - bias).toDouble)
    } else if (exp == expMask) {
      if (frac == 0) Double.PositiveInfinity else Double.NaN
    } else {
      val mant = 1.0 + frac.toDouble / (1 << fracWidth)
      val eUnb = exp - bias
      mant * math.pow(2.0, eUnb.toDouble)
    }
  }

  def floatToBin(v: Double, bw: Int): Int = {
    val expWidth  = expSize
    val fracWidth = bw - expWidth
    val bias      = biasCalc

    val E_INF     = (1 << expWidth) - 1
    val minNormE  = 1 - bias
    val maxNormE  = (1 << expWidth) - 2 - bias


    if (v.isNaN)        return (E_INF << fracWidth) | (1 << (fracWidth - 1))
    if (v.isInfinite)   return if (v > 0.0) (E_INF << fracWidth) else 0
    if (v <= 0.0)       return 0

    def rintEven(x: Double): Long = {
      val f = math.floor(x)
      val d = x - f
      if (d > 0.5) (f + 1).toLong
      else if (d < 0.5) f.toLong
      else {
        if ((f % 2) == 0) f.toLong else (f + 1).toLong
      }
    }

    val minNormVal = math.pow(2.0, minNormE.toDouble)


    if (v < minNormVal) {
      val scale  = math.pow(2.0, (1 - bias).toDouble)
      val raw    = v / scale * (1 << fracWidth)
      val fracR  = rintEven(raw)
      if (fracR <= 0) return 0
      if (fracR >= (1L << fracWidth)) {
        val E = 1
        return (E << fracWidth)
      }
      return fracR.toInt
    }


    var e = math.floor(math.log(v) / math.log(2.0)).toInt
    if (e > maxNormE) return (E_INF << fracWidth)
    if (e < minNormE) e = minNormE

    val scale  = math.pow(2.0, e.toDouble)

    val m      = math.min(math.max(v / scale, 1.0), math.nextAfter(2.0, 1.0))
    val raw    = (m - 1.0) * (1 << fracWidth)
    val fracR  = rintEven(raw)
    var Eenc   = e + bias
    var frac   = fracR


    if (frac >= (1L << fracWidth)) {
      frac  = 0
      Eenc += 1
      if (Eenc >= E_INF) return (E_INF << fracWidth)
    }

    ((Eenc << fracWidth) | frac.toInt)
  }

  private def bN(x: Int, w: Int) = x.toBinaryString.reverse.padTo(w, '0').reverse

  behavior of s"FPAdder (bw=$bw, pd=$pd, exp=$expSize)"

  it should "do random checks" in {
    test(new FPAdder2(bw, pd, expSize)) { dut =>
      val rnd = new scala.util.Random(0xC0FFEE)
      val N   = 64
      val Q   = scala.collection.mutable.Queue.empty[(Int, Int, Int)]

      for (cycle <- 0 until N) {
        val a  = rnd.nextInt(1 << bw)
        val b  = rnd.nextInt(1 << bw)
        val vi = true

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
            f"[Cycle $cycle%2d] A: ${bN(ea,bw)} (${binToFloat(ea,bw)}%s), " +
              f"B: ${bN(eb,bw)} (${binToFloat(eb,bw)}%s) => " +
              f"Out: ${bN(outBits,bw)} (${binToFloat(outBits,bw)}%s), " +
              f"Expected: ${bN(expOut,bw)} (${binToFloat(expOut,bw)}%s) " +
              (if (outBits == expOut) "✅" else "❌")

          println(line)
        }
      }


      dut.io.valid_in.poke(false.B)
      for (_ <- 0 until pd) {
        dut.clock.step(1)
        if (dut.io.valid_out.peek().litToBoolean && Q.nonEmpty) {
          val (ea, eb, expOut) = Q.dequeue()
          val outBits = dut.io.out.peek().litValue.toInt
          val line =
            f"[drain] A: ${bN(ea,bw)} (${binToFloat(ea,bw)}%s), " +
              f"B: ${bN(eb,bw)} (${binToFloat(eb,bw)}%s) => " +
              f"Out: ${bN(outBits,bw)} (${binToFloat(outBits,bw)}%s), " +
              f"Expected: ${bN(expOut,bw)} (${binToFloat(expOut,bw)}%s) " +
              (if (outBits == expOut) "✅" else "❌")
          println(line)
        }
      }
    }
  }

  it should "print exhaustive checks" in {
    test(new FPAdder2(bw, pd, expSize)) { dut =>
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
            f"[Cycle $cycle%3d] A: ${bN(ea,bw)} (${binToFloat(ea,bw)}%s), " +
              f"B: ${bN(eb,bw)} (${binToFloat(eb,bw)}%s) => " +
              f"Out: ${bN(outBits,bw)} (${binToFloat(outBits,bw)}%s), " +
              f"Expected: ${bN(expOut,bw)} (${binToFloat(expOut,bw)}%s) " +
              (if (outBits == expOut) "✅" else "❌")
          println(line)
        }
        cycle += 1
      }


      dut.io.valid_in.poke(false.B)
      for (_ <- 0 until pd) {
        dut.clock.step(1)
        if (dut.io.valid_out.peek().litToBoolean && Q.nonEmpty) {
          val (ea, eb, expOut) = Q.dequeue()
          val outBits = dut.io.out.peek().litValue.toInt
          val line =
            f"[drain] A: ${bN(ea,bw)} (${binToFloat(ea,bw)}%s), " +
              f"B: ${bN(eb,bw)} (${binToFloat(eb,bw)}%s) => " +
              f"Out: ${bN(outBits,bw)} (${binToFloat(outBits,bw)}%s), " +
              f"Expected: ${bN(expOut,bw)} (${binToFloat(expOut,bw)}%s) " +
              (if (outBits == expOut) "✅" else "❌")
          println(line)
        }
      }
    }
  }
}

class FPSubTest extends AnyFlatSpec with ChiselScalatestTester {


  val bw      = 5
  val expSize = 2
  val pd      = 1

  private val biasCalc = (1 << (expSize - 1)) - 1


  def binToFloat(bin: Int, bw: Int): Double = {
    val expWidth  = expSize
    val fracWidth = bw - expWidth
    val bias      = biasCalc
    val expMask   = (1 << expWidth) - 1
    val fracMask  = (1 << fracWidth) - 1

    val exp  = (bin >>> fracWidth) & expMask
    val frac = bin & fracMask

    if (exp == 0) {
      if (frac == 0) 0.0
      else (frac.toDouble / (1 << fracWidth)) * math.pow(2.0, (1 - bias).toDouble)
    } else if (exp == expMask) {
      if (frac == 0) Double.PositiveInfinity else Double.NaN
    } else {
      val mant = 1.0 + frac.toDouble / (1 << fracWidth)
      val eUnb = exp - bias
      mant * math.pow(2.0, eUnb.toDouble)
    }
  }

  def floatToBin(v: Double, bw: Int): Int = {
    val expWidth  = expSize
    val fracWidth = bw - expWidth
    val bias      = biasCalc

    val E_INF     = (1 << expWidth) - 1
    val minNormE  = 1 - bias
    val maxNormE  = (1 << expWidth) - 2 - bias


    @inline def rne(x: Double): Long = {
      val f   = math.floor(x)
      val df  = x - f
      if (df > 0.5) (f + 1).toLong
      else if (df < 0.5) f.toLong
      else {
        val fl = f.toLong
        if ((fl & 1L) == 1L) fl + 1L else fl
      }
    }


    if (v.isNaN)        return (E_INF << fracWidth) | (1 << math.max(0, fracWidth - 1))
    if (v.isInfinite)   return if (v > 0.0) (E_INF << fracWidth) else 0
    if (v <= 0.0)       return 0

    val minNormVal = math.pow(2.0, minNormE.toDouble)


    if (v < minNormVal) {

      val x = v / math.pow(2.0, (1 - bias).toDouble) * (1L << fracWidth)
      val fr = rne(x)
      if (fr <= 0L) return 0

      if (fr >= (1L << fracWidth)) {
        val E = 1
        return (E << fracWidth)
      }
      return fr.toInt
    }


    var e = math.floor(math.log(v) / math.log(2)).toInt
    if (e > maxNormE) return (E_INF << fracWidth)
    if (e < minNormE) e = minNormE

    val scale = math.pow(2.0, e.toDouble)
    val m     = v / scale
    val x     = (m - 1.0) * (1L << fracWidth)

    var frac  = rne(x)
    var Eenc  = e + bias


    if (frac >= (1L << fracWidth)) {
      frac = 0
      Eenc += 1
      if (Eenc >= E_INF) return (E_INF << fracWidth)
    }

    (Eenc << fracWidth) | frac.toInt
  }

  private def bN(x: Int, w: Int) = x.toBinaryString.reverse.padTo(w, '0').reverse

  behavior of s"FPSubtractorAbs (bw=$bw, pd=$pd, exp=$expSize)"

  it should "print random checks" in {
    test(new FPSubtractorAbs2(bw, pd, expSize)) { dut =>
      val rnd = new scala.util.Random(0xC0FFEE)
      val N   = 64
      val Q   = scala.collection.mutable.Queue.empty[(Int, Int, Int)]

      for (cycle <- 0 until N) {
        val a  = rnd.nextInt(1 << bw)
        val b  = rnd.nextInt(1 << bw)
        val vi = true

        val da = binToFloat(a, bw)
        val db = binToFloat(b, bw)
        val expBits = floatToBin(math.abs(da - db), bw)

        dut.io.in_a.poke(a.U)
        dut.io.in_b.poke(b.U)
        dut.io.valid_in.poke(vi.B)
        if (vi) Q.enqueue((a, b, expBits))

        dut.clock.step(1)

        if (dut.io.valid_out.peek().litToBoolean && Q.nonEmpty) {
          val (ea, eb, expOut) = Q.dequeue()
          val outBits = dut.io.out.peek().litValue.toInt

          val line =
            f"[Cycle $cycle%2d] A: ${bN(ea,bw)} (${binToFloat(ea,bw)}%s), " +
              f"B: ${bN(eb,bw)} (${binToFloat(eb,bw)}%s) => " +
              f"Out: ${bN(outBits,bw)} (${binToFloat(outBits,bw)}%s), " +
              f"Expected: ${bN(expOut,bw)} (${binToFloat(expOut,bw)}%s) " +
              (if (outBits == expOut) "✅" else "❌")

          println(line)
        }
      }


      dut.io.valid_in.poke(false.B)
      for (_ <- 0 until pd) {
        dut.clock.step(1)
        if (dut.io.valid_out.peek().litToBoolean && Q.nonEmpty) {
          val (ea, eb, expOut) = Q.dequeue()
          val outBits = dut.io.out.peek().litValue.toInt
          val line =
            f"[drain] A: ${bN(ea,bw)} (${binToFloat(ea,bw)}%s), " +
              f"B: ${bN(eb,bw)} (${binToFloat(eb,bw)}%s) => " +
              f"Out: ${bN(outBits,bw)} (${binToFloat(outBits,bw)}%s), " +
              f"Expected: ${bN(expOut,bw)} (${binToFloat(expOut,bw)}%s) " +
              (if (outBits == expOut) "✅" else "❌")
          println(line)
        }
      }
    }
  }

  it should "print exhaustive checks" in {
    test(new FPSubtractorAbs2(bw, pd, expSize)) { dut =>
      val Q = scala.collection.mutable.Queue.empty[(Int, Int, Int)]
      var cycle = 0

      for (a <- 0 until (1 << bw); b <- 0 until (1 << bw)) {
        val da = binToFloat(a, bw)
        val db = binToFloat(b, bw)
        val expBits = floatToBin(math.abs(da - db), bw)

        dut.io.in_a.poke(a.U)
        dut.io.in_b.poke(b.U)
        dut.io.valid_in.poke(true.B)
        Q.enqueue((a, b, expBits))

        dut.clock.step(1)

        if (dut.io.valid_out.peek().litToBoolean && Q.nonEmpty) {
          val (ea, eb, expOut) = Q.dequeue()
          val outBits = dut.io.out.peek().litValue.toInt
          val line =
            f"[Cycle $cycle%3d] A: ${bN(ea,bw)} (${binToFloat(ea,bw)}%s), " +
              f"B: ${bN(eb,bw)} (${binToFloat(eb,bw)}%s) => " +
              f"Out: ${bN(outBits,bw)} (${binToFloat(outBits,bw)}%s), " +
              f"Expected: ${bN(expOut,bw)} (${binToFloat(expOut,bw)}%s) " +
              (if (outBits == expOut) "✅" else "❌")
          println(line)
        }
        cycle += 1
      }


      dut.io.valid_in.poke(false.B)
      for (_ <- 0 until pd) {
        dut.clock.step(1)
        if (dut.io.valid_out.peek().litToBoolean && Q.nonEmpty) {
          val (ea, eb, expOut) = Q.dequeue()
          val outBits = dut.io.out.peek().litValue.toInt
          val line =
            f"[drain] A: ${bN(ea,bw)} (${binToFloat(ea,bw)}%s), " +
              f"B: ${bN(eb,bw)} (${binToFloat(eb,bw)}%s) => " +
              f"Out: ${bN(outBits,bw)} (${binToFloat(outBits,bw)}%s), " +
              f"Expected: ${bN(expOut,bw)} (${binToFloat(expOut,bw)}%s) " +
              (if (outBits == expOut) "✅" else "❌")
          println(line)
        }
      }
    }
  }
}

class SADTest extends AnyFlatSpec with ChiselScalatestTester {
  "module" should "perform SAD" in {
    test(new SAD2(5, 1, 3, 2)) { dut =>
      dut.io.in_vec_a(0).poke("b01110".U)
      dut.io.in_vec_a(1).poke("b00100".U)
      dut.io.in_vec_a(2).poke("b00100".U)



      dut.io.in_vec_b(0).poke("b00100".U)
      dut.io.in_vec_b(1).poke("b10000".U)
      dut.io.in_vec_b(2).poke("b01000".U)


      dut.io.valid_in.poke(true.B)



      dut.clock.step(3)

      dut.io.out_s.expect("b10101".U)



    }
  }
}

class SAD2Random_with_NaN extends AnyFlatSpec with ChiselScalatestTester {


  val bw       = 5
  val adderExp = 2
  val pd       = 1
  val vecLen   = 3

  behavior of s"SAD2(bw=$bw, exp=$adderExp, pd=$pd, vecLen=$vecLen)"


  private val expSize   = adderExp
  private val fracWidth = bw - expSize
  private val biasCalc  = (1 << (expSize - 1)) - 1

  private def bN(x: Int, w: Int): String = x.toBinaryString.reverse.padTo(w, '0').reverse

  private def binToFloat(bin: Int): Double = {
    val expMask   = (1 << expSize) - 1
    val fracMask  = (1 << fracWidth) - 1
    val exp  = (bin >>> fracWidth) & expMask
    val frac = bin & fracMask

    if (exp == 0) {
      if (frac == 0) 0.0
      else (frac.toDouble / (1 << fracWidth)) * math.pow(2.0, (1 - biasCalc).toDouble)
    } else if (exp == expMask) {
      if (frac == 0) Double.PositiveInfinity else Double.NaN
    } else {
      val mant = 1.0 + frac.toDouble / (1 << fracWidth)
      val eUnb = exp - biasCalc
      mant * math.pow(2.0, eUnb.toDouble)
    }
  }

  private def floatToBin(v: Double): Int = {
    val E_INF     = (1 << expSize) - 1
    val minNormE  = 1 - biasCalc
    val maxNormE  = (1 << expSize) - 2 - biasCalc

    if (v.isNaN)        return (E_INF << fracWidth) | (1 << (fracWidth - 1))
    if (v.isInfinite)   return (E_INF << fracWidth)
    if (v <= 0.0)       return 0

    val minNormVal = math.pow(2.0, minNormE.toDouble)

    if (v < minNormVal) {
      val scale = math.pow(2.0, (1 - biasCalc).toDouble)
      val frac  = math.floor(v / scale * (1 << fracWidth)).toInt
      return frac.max(0).min((1 << fracWidth) - 1)
    }

    var e = math.floor(math.log(v) / math.log(2)).toInt
    if (e > maxNormE) return (E_INF << fracWidth)
    if (e < minNormE) e = minNormE

    val scale = math.pow(2.0, e.toDouble)
    val mRaw  = (v / scale).max(1.0).min(math.nextAfter(2.0, 1.0))
    val frac  = math.floor((mRaw - 1.0) * (1 << fracWidth)).toInt
    val E     = e + biasCalc
    (E << fracWidth) | frac.max(0).min((1 << fracWidth) - 1)
  }

  private def refSAD(a: Seq[Int], b: Seq[Int]): Int = {
    val diffs = a.zip(b).map { case (ai, bi) =>
      val va = binToFloat(ai); val vb = binToFloat(bi)
      math.abs(va - vb)
    }
    val sum = diffs.sum
    floatToBin(sum)
  }

  def floatToBinRNE(vIn: Double): Int = {
    val E_INF     = (1 << expSize) - 1
    val minNormE  = 1 - biasCalc
    val maxNormE  = (1 << expSize) - 2 - biasCalc
    val twoPowMin = math.pow(2.0, minNormE.toDouble)


    if (vIn.isNaN)      return (E_INF << fracWidth) | (1 << (fracWidth - 1))
    if (vIn.isInfinite) return (E_INF << fracWidth)
    if (vIn <= 0.0)     return 0

    if (vIn < twoPowMin) {

      val scale = math.pow(2.0, (1 - biasCalc).toDouble)
      val x = vIn / scale * (1 << fracWidth)
      val floor = math.floor(x)
      val fracRem = x - floor

      val rounded =
        if (fracRem > 0.5) floor + 1
        else if (fracRem < 0.5) floor
        else {
          if (floor % 2 == 0) floor else floor + 1
        }
      val frac = rounded.toInt
      if (frac == 0) 0
      else if (frac >= (1 << fracWidth)) {
        val E = 1
        (E << fracWidth)
      } else frac
    } else {
      var e = math.floor(math.log(vIn) / math.log(2)).toInt
      if (e > maxNormE) return (E_INF << fracWidth)
      if (e < minNormE) e = minNormE

      val scale = math.pow(2.0, e.toDouble)
      val m = vIn / scale
      val fExact = (m - 1.0) * (1 << fracWidth)
      val fFloor = math.floor(fExact)
      val rem = fExact - fFloor


      var frac = (if (rem > 0.5) fFloor + 1
      else if (rem < 0.5) fFloor
      else { if (fFloor % 2 == 0) fFloor else fFloor + 1 }
        ).toLong

      var E = e + biasCalc


      if (frac >= (1L << fracWidth)) {
        frac = 0L
        E += 1
        if (E >= E_INF) return (E_INF << fracWidth)
      }
      ((E << fracWidth) | frac.toInt)
    }
  }


  def refSAD_stageWiseRNE(a: Seq[Int], b: Seq[Int]): Int = {

    val diffsEncoded: Seq[Int] = a.zip(b).map { case (ai, bi) =>
      floatToBinRNE(math.abs(binToFloat(ai) - binToFloat(bi)))
    }


    @annotation.tailrec
    def reduceStage(xs: Seq[Int]): Int = {
      if (xs.length == 1) xs.head
      else {
        val paired = xs.grouped(2).map {
          case Seq(x, y) => floatToBinRNE(binToFloat(x) + binToFloat(y))
          case Seq(x)    => x
        }.toSeq
        reduceStage(paired)
      }
    }
    reduceStage(diffsEncoded)
  }

  it should "run random vectors" in {
    test(new SAD2(bw, pd, vecLen, adderExp)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val rnd  = new scala.util.Random(0xD15EA5E)
      val N    = 300


      dut.io.valid_in.poke(false.B)
      for (_ <- 0 until 3) dut.clock.step()

      case class Txn(a: Seq[Int], b: Seq[Int], expect: Int)
      val q = scala.collection.mutable.Queue.empty[Txn]

      val totalLatency = (log2Ceil(vecLen) * pd) + pd

      for (cycle <- 0 until N) {
        // Random vectors
        val vecA = Seq.fill(vecLen)(rnd.nextInt(1 << bw))
        val vecB = Seq.fill(vecLen)(rnd.nextInt(1 << bw))

        val expBits = refSAD_stageWiseRNE(vecA, vecB)


        for (i <- 0 until vecLen) {
          dut.io.in_vec_a(i).poke(vecA(i).U)
          dut.io.in_vec_b(i).poke(vecB(i).U)
        }
        dut.io.valid_in.poke(true.B)
        q.enqueue(Txn(vecA, vecB, expBits))


        dut.clock.step(1)


        if (dut.io.valid_out.peek().litToBoolean && q.nonEmpty) {
          val got = dut.io.out_s.peek().litValue.toInt
          val Txn(a, b, expBits) = q.dequeue()


          val aStr = a.map(x => s"${bN(x, bw)}(${binToFloat(x)})").mkString(", ")
          val bStr = b.map(x => s"${bN(x, bw)}(${binToFloat(x)})").mkString(", ")
          val line =
            f"[Cycle $cycle%4d] A=[$aStr], B=[$bStr] => Out=${bN(got,bw)}(${binToFloat(got)})  " +
              f"Exp=${bN(expBits,bw)}(${binToFloat(expBits)}) " +
              (if (got == expBits) "✅" else "❌")
          println(line)

          assert(got == expBits, s"Mismatch: got ${bN(got,bw)}, expected ${bN(expBits,bw)}")
        }
      }

      dut.io.valid_in.poke(false.B)
      for (_ <- 0 until (totalLatency + 5)) {
        dut.clock.step(1)
        if (dut.io.valid_out.peek().litToBoolean && q.nonEmpty) {
          val got = dut.io.out_s.peek().litValue.toInt
          val Txn(a, b, expBits) = q.dequeue()
          val line =
            f"[drain] Out=${bN(got,bw)}(${binToFloat(got)})  " +
              f"Exp=${bN(expBits,bw)}(${binToFloat(expBits)}) " +
              (if (got == expBits) "✅" else "❌")
          println(line)
          assert(got == expBits, s"Mismatch in drain: got ${bN(got,bw)}, expected ${bN(expBits,bw)}")
        }
      }
    }
  }
}

class SAD2Random_no_Nan extends AnyFlatSpec with ChiselScalatestTester {


  val bw       = 5
  val adderExp = 3
  val pd       = 1
  val vecLen   = 3

  behavior of s"SAD2(bw=$bw, exp=$adderExp, pd=$pd, vecLen=$vecLen)"

  private val expSize   = adderExp
  private val fracWidth = bw - expSize
  private val biasCalc  = (1 << (expSize - 1)) - 1

  private val expMask   = (1 << expSize) - 1
  private val E_INF     = expMask

  private def expField(x: Int): Int = (x >>> fracWidth) & expMask
  private def isNaNbits(x: Int): Boolean = expField(x) == E_INF && (x & ((1 << fracWidth) - 1)) != 0
  private def isInfbits(x: Int): Boolean = expField(x) == E_INF && (x & ((1 << fracWidth) - 1)) == 0
  private def isSpecialbits(x: Int): Boolean = expField(x) == E_INF


  private def nextFinite(rnd: scala.util.Random): Int = {
    var v = 0
    do {
      v = rnd.nextInt(1 << bw)
    } while (isSpecialbits(v))
    v
  }

  private def bN(x: Int, w: Int): String = x.toBinaryString.reverse.padTo(w, '0').reverse

  private def binToFloat(bin: Int): Double = {
    val exp  = (bin >>> fracWidth) & expMask
    val frac = bin & ((1 << fracWidth) - 1)

    if (exp == 0) {
      if (frac == 0) 0.0
      else (frac.toDouble / (1 << fracWidth)) * math.pow(2.0, (1 - biasCalc).toDouble)
    } else if (exp == expMask) {
      if (frac == 0) Double.PositiveInfinity else Double.NaN
    } else {
      val mant = 1.0 + frac.toDouble / (1 << fracWidth)
      val eUnb = exp - biasCalc
      mant * math.pow(2.0, eUnb.toDouble)
    }
  }

  private def floatToBin(v: Double): Int = {
    val minNormE  = 1 - biasCalc
    val maxNormE  = (1 << expSize) - 2 - biasCalc

    if (v.isNaN)        return (E_INF << fracWidth) | (1 << (fracWidth - 1))
    if (v.isInfinite)   return (E_INF << fracWidth)
    if (v <= 0.0)       return 0

    val minNormVal = math.pow(2.0, minNormE.toDouble)

    if (v < minNormVal) {
      val scale = math.pow(2.0, (1 - biasCalc).toDouble)
      val frac  = math.floor(v / scale * (1 << fracWidth)).toInt
      return frac.max(0).min((1 << fracWidth) - 1)
    }

    var e = math.floor(math.log(v) / math.log(2)).toInt
    if (e > maxNormE) return (E_INF << fracWidth)
    if (e < minNormE) e = minNormE

    val scale = math.pow(2.0, e.toDouble)
    val mRaw  = (v / scale).max(1.0).min(math.nextAfter(2.0, 1.0))
    val frac  = math.floor((mRaw - 1.0) * (1 << fracWidth)).toInt
    val E     = e + biasCalc
    (E << fracWidth) | frac.max(0).min((1 << fracWidth) - 1)
  }

  private def refSAD(a: Seq[Int], b: Seq[Int]): Int = {
    val diffs = a.zip(b).map { case (ai, bi) =>
      val va = binToFloat(ai); val vb = binToFloat(bi)
      math.abs(va - vb)
    }
    floatToBin(diffs.sum)
  }

  def floatToBinRNE(vIn: Double): Int = {
    val minNormE  = 1 - biasCalc
    val maxNormE  = (1 << expSize) - 2 - biasCalc
    val twoPowMin = math.pow(2.0, minNormE.toDouble)

    if (vIn.isNaN)      return (E_INF << fracWidth) | (1 << (fracWidth - 1))
    if (vIn.isInfinite) return (E_INF << fracWidth)
    if (vIn <= 0.0)     return 0

    if (vIn < twoPowMin) {
      val scale = math.pow(2.0, (1 - biasCalc).toDouble)
      val x = vIn / scale * (1 << fracWidth)
      val floor = math.floor(x)
      val fracRem = x - floor
      val rounded =
        if (fracRem > 0.5) floor + 1
        else if (fracRem < 0.5) floor
        else { if (floor % 2 == 0) floor else floor + 1 }
      val frac = rounded.toInt
      if (frac == 0) 0
      else if (frac >= (1 << fracWidth)) { val E = 1; (E << fracWidth) }
      else frac
    } else {
      var e = math.floor(math.log(vIn) / math.log(2)).toInt
      if (e > maxNormE) return (E_INF << fracWidth)
      if (e < minNormE) e = minNormE

      val scale = math.pow(2.0, e.toDouble)
      val m = vIn / scale
      val fExact = (m - 1.0) * (1 << fracWidth)
      val fFloor = math.floor(fExact)
      val rem = fExact - fFloor

      var frac = (if (rem > 0.5) fFloor + 1
      else if (rem < 0.5) fFloor
      else { if (fFloor % 2 == 0) fFloor else fFloor + 1 }).toLong

      var E = e + biasCalc
      if (frac >= (1L << fracWidth)) {
        frac = 0L
        E += 1
        if (E >= E_INF) return (E_INF << fracWidth)
      }
      ((E << fracWidth) | frac.toInt)
    }
  }

  def refSAD_stageWiseRNE(a: Seq[Int], b: Seq[Int]): Int = {
    val diffsEncoded: Seq[Int] = a.zip(b).map { case (ai, bi) =>
      floatToBinRNE(math.abs(binToFloat(ai) - binToFloat(bi)))
    }
    @annotation.tailrec
    def reduceStage(xs: Seq[Int]): Int =
      if (xs.length == 1) xs.head
      else {
        val paired = xs.grouped(2).map {
          case Seq(x, y) => floatToBinRNE(binToFloat(x) + binToFloat(y))
          case Seq(x)    => x
        }.toSeq
        reduceStage(paired)
      }
    reduceStage(diffsEncoded)
  }

  it should "run random vectors, no Nan/Inf inputs" in {
    test(new SAD2(bw, pd, vecLen, adderExp)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val rnd  = new scala.util.Random(0xD15EA5E)
      val N    = 300


      dut.io.valid_in.poke(false.B)
      for (_ <- 0 until 3) dut.clock.step()

      case class Txn(a: Seq[Int], b: Seq[Int], expect: Int)
      val q = scala.collection.mutable.Queue.empty[Txn]

      val totalLatency = (log2Ceil(vecLen) * pd) + pd

      for (cycle <- 0 until N) {

        val vecA = Seq.fill(vecLen)(nextFinite(rnd))
        val vecB = Seq.fill(vecLen)(nextFinite(rnd))


        val expBits = refSAD_stageWiseRNE(vecA, vecB)


        for (i <- 0 until vecLen) {
          dut.io.in_vec_a(i).poke(vecA(i).U)
          dut.io.in_vec_b(i).poke(vecB(i).U)
        }
        dut.io.valid_in.poke(true.B)
        q.enqueue(Txn(vecA, vecB, expBits))


        dut.clock.step(1)


        if (dut.io.valid_out.peek().litToBoolean && q.nonEmpty) {
          val got = dut.io.out_s.peek().litValue.toInt
          val Txn(a, b, expBits) = q.dequeue()

          val aStr = a.map(x => s"${bN(x, bw)}(${binToFloat(x)})").mkString(", ")
          val bStr = b.map(x => s"${bN(x, bw)}(${binToFloat(x)})").mkString(", ")
          val line =
            f"[Cycle $cycle%4d] A=[$aStr], B=[$bStr] => Out=${bN(got,bw)}(${binToFloat(got)})  " +
              f"Exp=${bN(expBits,bw)}(${binToFloat(expBits)}) " +
              (if (got == expBits) "✅" else "❌")
          println(line)

          assert(got == expBits, s"Mismatch: got ${bN(got,bw)}, expected ${bN(expBits,bw)}")
        }
      }


      dut.io.valid_in.poke(false.B)
      for (_ <- 0 until (totalLatency + 5)) {
        dut.clock.step(1)
        if (dut.io.valid_out.peek().litToBoolean && q.nonEmpty) {
          val got = dut.io.out_s.peek().litValue.toInt
          val Txn(_, _, expBits) = q.dequeue()
          val line =
            f"[drain] Out=${bN(got,bw)}(${binToFloat(got)})  " +
              f"Exp=${bN(expBits,bw)}(${binToFloat(expBits)}) " +
              (if (got == expBits) "✅" else "❌")
          println(line)
          assert(got == expBits, s"Mismatch in drain: got ${bN(got,bw)}, expected ${bN(expBits,bw)}")
        }
      }
    }
  }
}