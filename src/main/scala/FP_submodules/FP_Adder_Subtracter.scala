package FP_submodules

import chisel3._
import chisel3.util._

import scala.language.postfixOps





// Leading Zero Counter for normalization
//class LZC(val width: Int) extends Module {
//  val io = IO(new Bundle {
//    val in = Input(UInt(width.W))
//    val out = Output(UInt(log2Ceil(width + 1).W))
//  })
//
//  io.out := PriorityEncoder(io.in.asBools.reverse)
//}

class LZC(val w: Int) extends Module {
  val io = IO(new Bundle {
    val in     = Input(UInt(w.W))
    val out    = Output(UInt(log2Ceil(w + 1).W))
    val isZero = Output(Bool())
  })
  val rev   = Reverse(io.in)
  val any   = rev.orR
  io.isZero := !any
  io.out    := Mux(any, PriorityEncoder(rev), w.U)
}


class FPAdder(val bw: Int, val pd: Int) extends Module {
  override def desiredName: String = s"FPAdder_${bw}_${pd}"
  val io = IO(new Bundle {
    val in_a = Input(UInt(bw.W))
    val in_b = Input(UInt(bw.W))
    val valid_in = Input(Bool())
    val out = Output(UInt(bw.W))
    val valid_out = Output(Bool())
  })

  def zextTo(u: UInt, w: Int): UInt = { require(w >= u.getWidth); Cat(0.U((w-u.getWidth).W), u) }
  def sextTo(s: SInt, w: Int): SInt = {
    require(w >= s.getWidth); val sw = s.getWidth; val sign = s(sw-1)
    Cat(Fill(w-sw, sign), s.asUInt).asSInt
  }


  val expWidth = 2
  val fracWidth = bw - expWidth//bw - expWidth - 1 // 1 sign bit
  val mantWidth = fracWidth + 1
  val sumWidth = mantWidth + 1
  val bias = 1

  val eUnbW = expWidth + 1
  val eCalcW = eUnbW + 1

  // Extract fields
  //val signA = io.in_a(bw - 1)
  val expA = io.in_a(bw - 1, fracWidth)//io.in_a(bw - 2, fracWidth)
  val fracA = io.in_a(fracWidth - 1, 0)

  //val signB = io.in_b(bw - 1)
  val expB = io.in_b(bw - 1, fracWidth)//io.in_b(bw - 2, fracWidth)
  val fracB = io.in_b(fracWidth - 1, 0)

  val expAbool = expA === 0.U
  val expBbool = expB === 0.U

  // Add implicit 1 for normalized values and 0 for subnormal
  val normFracA = Wire(UInt((fracWidth + 1).W))
  val normFracB = Wire(UInt((fracWidth + 1).W))

  normFracA := Mux(expAbool, Cat(0.U(1.W), fracA), Cat(1.U(1.W), fracA))
  normFracB := Mux(expBbool, Cat(0.U(1.W), fracB), Cat(1.U(1.W), fracB))

  //  val normFracA = Cat(1.U(1.W), fracA)
  //  val normFracB = Cat(1.U(1.W), fracB)
  def unbias(e: UInt): SInt = {
    val eS   = zextTo(e, eUnbW).asSInt
    val wide = sextTo(eS, eCalcW) - bias.S(eCalcW.W)
    Mux(e === 0.U, 0.S(eCalcW.W), wide)
  }
  val eA: SInt = unbias(expA)
  val eB: SInt = unbias(expB)


  // Compare exponents
  val expDiffs: SInt = eA - eB//Wire(UInt((expWidth + 1).W))
  val expAGtB = eA > eB
  val expDiff: UInt = Mux(expDiffs < 0.S(eCalcW.W), (-expDiffs).asUInt, expDiffs.asUInt)//Mux(expAGtB, expA - expB, expB - expA)

  val alignedLarge = Wire(UInt(sumWidth.W))//Wire(UInt((fracWidth + 2).W))
  val alignedSmall = Wire(UInt(sumWidth.W))//Wire(UInt((fracWidth + 2).W))
  val expOut = Wire(SInt(eCalcW.W)) // added + 1

  when(expAGtB) {
    alignedLarge := normFracA.pad(sumWidth)
    alignedSmall := normFracB.pad(sumWidth) >> Mux(expDiff > sumWidth.U, sumWidth.U, expDiff)
    expOut := eA
  }.otherwise {
    //    alignedLarge := normFracB
    //    alignedSmall := normFracA >> expDiff
    //    expOut := expB
    alignedLarge := normFracB.pad(sumWidth)
    alignedSmall := normFracA.pad(sumWidth) >> Mux(expDiff > sumWidth.U, sumWidth.U, expDiff)
    expOut := eB
  }

  // Add aligned mantissas
  val fracSum = alignedLarge +& alignedSmall // added &
  val carryOut = fracSum(sumWidth - 1) // changed from fracWidth + 1

  // Leading Zero Counter for normalization
  val lzc = Module(new LZC(mantWidth))
  lzc.io.in := fracSum(mantWidth - 1, 0)
  val shiftAmt = lzc.io.out
  val isZero = lzc.io.isZero

  val shiftedFrac = Wire(UInt((mantWidth).W))
  val newExp = Wire(SInt((eCalcW).W)) // added + 1

  //  when(carryOut) {
  //    // Overflow, shift right
  //    shiftedFrac := fracSum >> 1
  //    newExp := expOut + 1.U
  //  }.elsewhen(fracSum(fracWidth + 1) === 0.U && fracSum(fracWidth) === 1.U) {
  //    // Already normalized
  //    shiftedFrac := fracSum
  //    newExp := expOut
  //  }.otherwise {
  //    // Normalize by left shifting
  //    shiftedFrac := fracSum << shiftAmt
  //    newExp := Mux(expOut >= shiftAmt, expOut - shiftAmt, 0.U)
  //  }

  val shiftAmts: SInt = zextTo(shiftAmt, eCalcW).asSInt

  when(carryOut) {
    shiftedFrac := fracSum(sumWidth -1, 1)
    newExp := expOut + 1.S(eCalcW.W)
  }.otherwise {
    when(isZero) {
      shiftedFrac := 0.U
      newExp := 0.S(eCalcW.W)
    }.otherwise{
      shiftedFrac := (fracSum(mantWidth -1, 0) << shiftAmt)(mantWidth - 1, 0)
      newExp := expOut - shiftAmts
    }
  }

  //  val normFrac = shiftedFrac(fracWidth - 1, 0)
  //  val normExp = newExp
  val outExp  = Wire(UInt(expWidth.W))
  val outFrac = Wire(UInt(fracWidth.W))


  // added code snippet needs verification
  val maxEncExp = ((1 << expWidth) - 1).U(expWidth.W)
  val maxUnb    = ((1 << expWidth) - 1 - bias).S(eCalcW.W)

  when (shiftedFrac === 0.U) {
    outExp  := 0.U
    outFrac := 0.U
  }.elsewhen (newExp < 0.S(eCalcW.W)) {
    val k    = (-newExp).asUInt
    val kCap = Mux(k > mantWidth.U, mantWidth.U, k)
    val den  = (shiftedFrac >> kCap)(mantWidth - 1, 0)
    outExp  := 0.U
    outFrac := den(mantWidth - 2, 0)
  }.elsewhen (newExp > maxUnb) {
    outExp  := maxEncExp
    outFrac := ((1 << fracWidth) - 1).U(fracWidth.W)
  }.otherwise {
    val encE = (newExp + bias.S(eCalcW.W)).asUInt
    outExp  := encE(expWidth - 1, 0)
    outFrac := shiftedFrac(mantWidth - 2, 0)
  }


  val result = Cat(outExp, outFrac)//Cat(normExp, normFrac)//Cat(0.U(1.W), normExp, normFrac)

  // Pipeline shift registers with valid gating
  val sr_valid = RegInit(VecInit(Seq.fill(pd)(false.B)))
  val sr_data = Reg(Vec(pd, UInt(bw.W)))

  // Shift pipeline
  sr_valid(0) := io.valid_in
  for (i <- 1 until pd) sr_valid(i) := sr_valid(i - 1)

  when(io.valid_in) {
    sr_data(0) := result
  }

  for (i <- 1 until pd) {
    sr_data(i) := sr_data(i - 1)
  }

  io.valid_out := sr_valid(pd - 1)
  io.out := sr_data(pd - 1)

  // ---------------- DEBUG ---------------- //
  //  when(io.valid_in) {
  //    printf("[STEP 1: Extract Fields]\n")
  //    //printf("  A = %b => signA: %b | expA: %b | fracA: %b\n", io.in_a, signA, expA, fracA)
  //    //printf("  B = %b => signB: %b | expB: %b | fracB: %b\n", io.in_b, signB, expB, fracB)
  //
  //    printf("[STEP 2: Compare Exponents]\n")
  //    printf("  expA: %d, expB: %d\n", expA, expB)
  //    printf("  expDiff: %d => expAGtB: %b\n", expDiff, expAGtB)
  //
  //    printf("[STEP 3: Align Mantissas]\n")
  //    printf("  normFracA (with hidden 1): %b\n", normFracA)
  //    printf("  normFracB (with hidden 1): %b\n", normFracB)
  //    printf("  alignedLarge: %b | alignedSmall: %b\n", alignedLarge, alignedSmall)
  //    printf("  selectedExpOut: %d\n", expOut)
  //
  //    printf("[STEP 4: Add Aligned Mantissas]\n")
  //    printf("  sum: %b (carryOut = %b)\n", fracSum, carryOut)
  //
  //    printf("[STEP 5: Normalize Result]\n")
  //    when(carryOut) {
  //      printf("  CarryOut = 1 => shift right by 1\n")
  //    }.elsewhen(fracSum(fracWidth + 1) === 0.U && fracSum(fracWidth) === 1.U) {
  //      printf("  Already normalized => no shift needed\n")
  //    }.otherwise {
  //      printf("  No CarryOut => LZC = %d | shift left\n", shiftAmt)
  //    }
  //    //printf("  normFrac: %b | normExp: %d\n", normFrac, normExp)
  //
  //    printf("[STEP 6: Final Result Reassembly]\n")
  //    printf("  Final result = %b\n", result)
  //
  //    printf("[Cycle Info] bw = %d | pd = %d => valid_out: %b | output: %b\n",
  //      bw.U, pd.U, io.valid_out, io.out)
  //  }
}

// FP Subtractor Absolute Value for unsigned FP numbers
// Just compute difference and return result (absolute by design since inputs unsigned)
// Final Integrated FPSubtractor with correct IO for testbench


class FPSubtractorAbs(val bw: Int, val pd: Int) extends Module {
  override def desiredName: String = s"FPSubtractor_${bw}_${pd}"

  val io = IO(new Bundle {
    val in_a     = Input(UInt(bw.W))
    val in_b     = Input(UInt(bw.W))
    val valid_in = Input(Bool())
    val out      = Output(UInt(bw.W))
    val valid_out = Output(Bool())
  })

  val expWidth = 2
  val manWidth = bw - expWidth - 1

  // Order inputs so subtraction is always positive (absolute value)
  val aVal = Wire(UInt(bw.W))
  val bVal = Wire(UInt(bw.W))
  when(io.in_a >= io.in_b) {
    aVal := io.in_a
    bVal := io.in_b
  } .otherwise {
    aVal := io.in_b
    bVal := io.in_a
  }

  val expA = aVal(bw - 2, bw - expWidth - 1)
  val manA = aVal(bw - expWidth - 2, 0)
  val expB = bVal(bw - 2, bw - expWidth - 1)
  val manB = bVal(bw - expWidth - 2, 0)

  val expDiff = Mux(expA >= expB, expA - expB, expB - expA)

  val aExt = Cat(1.U(1.W), manA, 0.U(1.W))
  val bExt = Cat(1.U(1.W), manB, 0.U(1.W))

  val alignedA = Wire(UInt((manWidth + 2).W))
  val alignedB = Wire(UInt((manWidth + 2).W))
  val maxExp = Wire(UInt(expWidth.W))

  when(expA >= expB) {
    alignedA := aExt
    alignedB := bExt >> expDiff
    maxExp := expA
  }.otherwise {
    alignedA := aExt >> expDiff
    alignedB := bExt
    maxExp := expB
  }

  val diff = alignedA - alignedB

  val lzc = Module(new LZC(diff.getWidth))
  lzc.io.in := diff

  val shift = lzc.io.out
  val normalizedMantFull = diff << shift
  val normalizedMant = normalizedMantFull(diff.getWidth - 2, diff.getWidth - 1 - manWidth)

  val adjustedExp = Mux(maxExp >= shift, maxExp - shift, 0.U)
  val result = Cat(0.U(1.W), adjustedExp, normalizedMant)

  val sr_array = RegInit(VecInit(Seq.fill(pd)(0.U(bw.W))))
  val valid_sr = RegInit(VecInit(Seq.fill(pd)(false.B)))

  sr_array(0) := result
  valid_sr(0) := io.valid_in
  for (i <- 1 until pd) {
    sr_array(i) := sr_array(i - 1)
    valid_sr(i) := valid_sr(i - 1)
  }

  io.out := sr_array.last
  io.valid_out := valid_sr.last
}