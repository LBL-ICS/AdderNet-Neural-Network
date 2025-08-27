package FP_submodules

import chisel3._
import chisel3.util._

import scala.language.postfixOps


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

class FPSubtractorAbs(val bw: Int, val pd: Int, val exp: Int) extends Module {
  override def desiredName: String = s"FPSubtractorAbs_${bw}_${pd}"
  val io = IO(new Bundle {
    val in_a      = Input(UInt(bw.W))
    val in_b      = Input(UInt(bw.W))
    val valid_in  = Input(Bool())
    val out       = Output(UInt(bw.W))
    val valid_out = Output(Bool())
  })


  private def zextTo(u: UInt, w: Int): UInt = { require(w >= u.getWidth); Cat(0.U((w - u.getWidth).W), u) }
  private def sextTo(s: SInt, w: Int): SInt = { require(w >= s.getWidth); val sw = s.getWidth; val sign = s(sw-1); Cat(Fill(w - sw, sign), s.asUInt).asSInt }
  private def lzcUInt(x: UInt, w: Int): (UInt, Bool) = {
    val isZero = x === 0.U
    val cnt    = PriorityEncoder(Reverse(x))
    (Mux(isZero, w.U, cnt), isZero)
  }

  def rneAtCut(full: UInt, keptHi: Int, keptLo: Int): (UInt, Bool) = {
    val kept =
      if (fracWidth > 0) full(keptHi, keptLo) else 0.U(fracWidth.W)

    val gIdx = keptLo - 1
    val rIdx = keptLo - 2
    val sHi  = keptLo - 3
    val sLo  = 0

    val guardBit  = if (fracWidth > 0) full(gIdx) else 0.U(1.W)
    val roundBit  = if (fracWidth > 0) full(rIdx) else 0.U(1.W)
    val stickyBit = if (fracWidth > 0 && sHi >= sLo) full(sHi, sLo).orR else false.B

    val lsbKept   = if (fracWidth > 0) kept(0).asBool else false.B
    val roundUp   = guardBit.asBool && (roundBit.asBool || stickyBit || lsbKept)

    val sum       = Cat(0.U(1.W), kept) + roundUp.asUInt
    val carryOut  = sum(fracWidth)
    val rounded   = sum(fracWidth - 1, 0)

    (rounded, carryOut)
  }

  val expWidth   = exp
  val fracWidth  = bw - expWidth
  val mantWidth  = fracWidth + 1
  val gaurdBits = 3
  val alignWidth = mantWidth + gaurdBits

  val bias       = (1 << (expWidth - 1)) - 1
  val minNormalUnb = 1 - bias
  val maxNormalUnb = ((1 << expWidth) - 2) - bias

  val eUnbW  = expWidth + 1
  val eCalcW = eUnbW + 1

  val expA  = io.in_a(bw - 1, fracWidth)
  val fracA = if (fracWidth > 0) io.in_a(fracWidth - 1, 0) else 0.U(0.W)
  val expB  = io.in_b(bw - 1, fracWidth)
  val fracB = if (fracWidth > 0) io.in_b(fracWidth - 1, 0) else 0.U(0.W)

  val expA_isZero = expA === 0.U
  val expB_isZero = expB === 0.U

  val maxEncExp = ((1 << expWidth) - 1).U(expWidth.W)
  val aFracNZ: Bool = if (fracWidth > 0) (fracA =/= 0.U(fracWidth.W)) else false.B
  val bFracNZ: Bool = if (fracWidth > 0) (fracB =/= 0.U(fracWidth.W)) else false.B
  val aIsNaN = (expA === maxEncExp) && aFracNZ
  val bIsNaN = (expB === maxEncExp) && bFracNZ
  val aIsInf = (expA === maxEncExp) && !aFracNZ
  val bIsInf = (expB === maxEncExp) && !bFracNZ

  val anyNaN  = aIsNaN || bIsNaN
  val bothInf = aIsInf && bIsInf
  val oneInf  = aIsInf ^ bIsInf

  val qNaNFrac: UInt = if (fracWidth > 0) Cat(1.U(1.W), 0.U((fracWidth - 1).W)) else 0.U(fracWidth.W)

  val specialHit = WireInit(false.B)
  val specialOut = WireInit(0.U(bw.W))
  when (anyNaN || bothInf) {
    specialHit := true.B
    specialOut := Cat(maxEncExp, qNaNFrac)
  } .elsewhen (oneInf) {
    specialHit := true.B
    specialOut := Cat(maxEncExp, 0.U(fracWidth.W))
  }

  val normFracA = Mux(expA_isZero, Cat(0.U(1.W), fracA), Cat(1.U(1.W), fracA))
  val normFracB = Mux(expB_isZero, Cat(0.U(1.W), fracB), Cat(1.U(1.W), fracB))


  private def unbias(e: UInt): SInt = {
    val eS = zextTo(e, eUnbW).asSInt
    val w  = sextTo(eS, eCalcW) - bias.S(eCalcW.W)
    val e0 = (1 - bias).S(eCalcW.W)
    Mux(e === 0.U, e0, w)
  }
  val eA: SInt = unbias(expA)
  val eB: SInt = unbias(expB)


  val expAGtB   = eA > eB
  val expsEq    = eA === eB
  val mantAGtB  = normFracA > normFracB
  val aIsLarger = Mux(expsEq, mantAGtB, expAGtB)

  val expDiffS = eA - eB
  val expDiffU = Mux(expDiffS < 0.S(eCalcW.W), (-expDiffS).asUInt, expDiffS.asUInt)

  val largeExt = Wire(UInt(alignWidth.W))
  val smallExt = Wire(UInt(alignWidth.W))
  val expOut   = Wire(SInt(eCalcW.W))
  when (aIsLarger) {
    largeExt := Cat(normFracA, 0.U(gaurdBits.W))
    smallExt := Cat(normFracB, 0.U(gaurdBits.W))
    expOut   := eA
  } .otherwise {
    largeExt := Cat(normFracB, 0.U(gaurdBits.W))
    smallExt := Cat(normFracA, 0.U(gaurdBits.W))
    expOut   := eB
  }

  val shRaw      = expDiffU
  val coreMax    = (alignWidth - 1).U
  val coreSh     = Mux(shRaw > coreMax, coreMax, shRaw)
  val shifted    = smallExt >> coreSh

  val mask       = (1.U(alignWidth.W) << coreSh) - 1.U
  val fellOff    = (smallExt & mask) =/= 0.U

  val overShift  = shRaw > coreMax
  val smallNZ    = smallExt =/= 0.U
  val stickyBit  = (fellOff || (overShift && smallNZ)).asUInt

  val smallAligned = shifted | stickyBit

  val diffExt    = largeExt - smallAligned

  val baseExp = expOut

  val (shiftAmt, isZero) = lzcUInt(diffExt, alignWidth)
  val shiftedExt = Wire(UInt(alignWidth.W))
  val newExp     = Wire(SInt(eCalcW.W))
  when (isZero) {
    shiftedExt := 0.U
    newExp     := 0.S
  } .otherwise {
    shiftedExt := (diffExt << shiftAmt)(alignWidth - 1, 0)
    val shiftS = zextTo(shiftAmt, eCalcW).asSInt
    newExp     := baseExp - shiftS
  }

  val resultExp  = Wire(UInt(expWidth.W))
  val resultFrac = Wire(UInt(fracWidth.W))

  when (isZero) {
    resultExp  := 0.U
    resultFrac := 0.U
  } .elsewhen (newExp < minNormalUnb.S(eCalcW.W)) {
    val under   = (minNormalUnb.S(eCalcW.W) - newExp).asUInt
    val sCap    = Mux(under > alignWidth.U, alignWidth.U, under)
    val denFull = (shiftedExt >> sCap)(alignWidth - 1, 0)

    val hi      = alignWidth - 2
    val lo      = alignWidth - 1 - fracWidth

    val (fracRnd, carryFrac) = rneAtCut(denFull, hi, lo)

    when (carryFrac) {
      resultExp  := 1.U(expWidth.W)
      resultFrac := 0.U(fracWidth.W)
    } .otherwise {
      resultExp  := 0.U
      resultFrac := fracRnd
    }
  } .otherwise {
    val encE0 = (newExp + bias.S(eCalcW.W)).asUInt

    val hi    = alignWidth - 2
    val lo    = alignWidth - 1 - fracWidth
    val (fracRnd, carryFrac) = rneAtCut(shiftedExt, hi, lo)

    val encE1 = encE0 + carryFrac
    resultExp := encE1(expWidth - 1, 0)
    resultFrac := Mux(carryFrac, 0.U(fracWidth.W), fracRnd)
  }

  val packedFinite = Cat(resultExp, resultFrac)
  val result = Mux(specialHit, specialOut, packedFinite)

  val sr_valid = RegInit(VecInit(Seq.fill(pd)(false.B)))
  val sr_data  = RegInit(VecInit(Seq.fill(pd)(0.U(bw.W))))
  sr_valid(0) := io.valid_in
  when (io.valid_in) { sr_data(0) := result }
  for (i <- 1 until pd) {
    sr_valid(i) := sr_valid(i - 1)
    sr_data(i)  := sr_data(i - 1)
  }
  io.valid_out := sr_valid(pd - 1)
  io.out       := sr_data(pd - 1)
}

class FPAdder(val bw: Int, val pd: Int, val exp: Int) extends Module {
  override def desiredName: String = s"FPAdder_${bw}_${pd}"
  val io = IO(new Bundle {
    val in_a     = Input(UInt(bw.W))
    val in_b     = Input(UInt(bw.W))
    val valid_in = Input(Bool())
    val out      = Output(UInt(bw.W))
    val valid_out= Output(Bool())
  })

  def zextTo(u: UInt, w: Int): UInt = { require(w >= u.getWidth); Cat(0.U((w-u.getWidth).W), u) }
  def sextTo(s: SInt, w: Int): SInt = {
    require(w >= s.getWidth); val sw = s.getWidth; val sign = s(sw-1)
    Cat(Fill(w-sw, sign), s.asUInt).asSInt
  }

  val expWidth = exp
  val fracWidth = bw - expWidth
  val mantWidth = fracWidth + 1
  val sumWidth  = mantWidth + 1
  val bias = (1 << (expWidth - 1)) - 1

  val minNormalUnb = 1 - bias
  val maxNormalUnb = ((1 << expWidth) - 2) - bias

  val eUnbW  = expWidth + 1
  val eCalcW = eUnbW + 1


  val expA  = io.in_a(bw - 1, fracWidth)
  val fracA = io.in_a(fracWidth - 1, 0)

  val expB  = io.in_b(bw - 1, fracWidth)
  val fracB = io.in_b(fracWidth - 1, 0)

  val expAisZero = expA === 0.U
  val expBisZero = expB === 0.U

  val maxEncExp  = ((1 << expWidth) - 1).U(expWidth.W)
  val hasFrac    = fracWidth > 0

  val aFracNZ: Bool = if (hasFrac) (fracA =/= 0.U(fracWidth.W)) else false.B
  val bFracNZ: Bool = if (hasFrac) (fracB =/= 0.U(fracWidth.W)) else false.B

  val aIsNaN   = (expA === maxEncExp) && aFracNZ
  val bIsNaN   = (expB === maxEncExp) && bFracNZ
  val aIsInf   = (expA === maxEncExp) && !aFracNZ
  val bIsInf   = (expB === maxEncExp) && !bFracNZ
  val anyNaNIn = aIsNaN || bIsNaN
  val anyInfIn = aIsInf || bIsInf

  val qNaNFrac: UInt =
    if (fracWidth > 0) Cat(1.U(1.W), 0.U((fracWidth - 1).W)) else 0.U(fracWidth.W)


  val normFracA = Wire(UInt(mantWidth.W))
  val normFracB = Wire(UInt(mantWidth.W))
  normFracA := Mux(expAisZero, Cat(0.U(1.W), fracA), Cat(1.U(1.W), fracA))
  normFracB := Mux(expBisZero, Cat(0.U(1.W), fracB), Cat(1.U(1.W), fracB))


  def unbias(e: UInt): SInt = {
    val eS   = zextTo(e, eUnbW).asSInt
    val wide = sextTo(eS, eCalcW) - bias.S(eCalcW.W)
    val e0Map = (1 - bias).S(eCalcW.W)
    Mux(e === 0.U, e0Map, wide)
  }
  val eA: SInt = unbias(expA)
  val eB: SInt = unbias(expB)


  val expDiffs: SInt = eA - eB
  val expAGtB = eA > eB
  val expDiff: UInt = Mux(expDiffs < 0.S(eCalcW.W), (-expDiffs).asUInt, expDiffs.asUInt)

  val expOut = Wire(SInt(eCalcW.W))
  val largePre  = Wire(UInt(sumWidth.W))
  val smallPre  = Wire(UInt(sumWidth.W))
  val sVal      = Wire(UInt(log2Ceil(sumWidth + 1).W))
  val stickyAlign = Wire(Bool())

  when(expAGtB) {
    largePre := normFracA.pad(sumWidth)
    smallPre := normFracB.pad(sumWidth)
    expOut   := eA
  }.otherwise {
    largePre := normFracB.pad(sumWidth)
    smallPre := normFracA.pad(sumWidth)
    expOut   := eB
  }
  sVal := Mux(expDiff > sumWidth.U, sumWidth.U, expDiff)

  val alignedLarge = largePre
  val alignedSmall = smallPre >> sVal

  val alignMask = Mux(sVal === 0.U, 0.U, ((1.U(sumWidth.W) << sVal) - 1.U))
  stickyAlign := (smallPre & alignMask).orR


  val fracSum  = alignedLarge +& alignedSmall.asUInt
  val carryOut = fracSum(sumWidth - 1)


  val lzc = Module(new LZC(mantWidth))
  lzc.io.in := fracSum(mantWidth - 1, 0)
  val shiftAmt = lzc.io.out
  val isZero   = lzc.io.isZero

  val shiftedFrac = Wire(UInt(mantWidth.W))
  val newExp      = Wire(SInt(eCalcW.W))

  when(carryOut) {
    shiftedFrac := fracSum(sumWidth - 1, 1)
    newExp      := expOut + 1.S(eCalcW.W)
  }.otherwise {
    when(isZero) {
      shiftedFrac := 0.U
      newExp      := 0.S(eCalcW.W)
    }.otherwise {
      shiftedFrac := (fracSum(mantWidth - 1, 0) << shiftAmt)(mantWidth - 1, 0)
      newExp      := expOut - zextTo(shiftAmt, eCalcW).asSInt
    }
  }

  val maxNormalUnb_s = maxNormalUnb.S(eCalcW.W)
  val resultNormalExp  = Wire(UInt(expWidth.W))
  val resultNormalFrac = Wire(UInt(fracWidth.W))


  val pre = fracSum(mantWidth - 1, 0)
  val k   = shiftAmt

  val roundBitRight = fracSum(0)
  val stickyRight   = stickyAlign


  val roundBitLeft_kpos  = (pre >> (k - 1.U))(0)
  val lowerMaskLeft      = ((1.U(mantWidth.W) << (k - 1.U)) - 1.U)
  val stickyLeftLower    = (pre & lowerMaskLeft).orR
  val stickyLeft_kpos    = stickyAlign || stickyLeftLower


  val roundBitLeft_k0 =
    Mux(sVal === 0.U, false.B, ((smallPre >> (sVal - 1.U))(0)).asBool)
  val stickyLeft_k0 =
    Mux(sVal <= 1.U, false.B,
      (smallPre & ((1.U(sumWidth.W) << (sVal - 1.U)) - 1.U)).orR)


  val roundBitLeft = Mux(k === 0.U, roundBitLeft_k0, roundBitLeft_kpos)
  val stickyLeft   = Mux(k === 0.U, stickyLeft_k0,  stickyLeft_kpos)


  val roundBit = Mux(carryOut, roundBitRight, roundBitLeft)
  val stickyAll= Mux(carryOut, stickyRight,   stickyLeft)


  val lsb     = shiftedFrac(0)
  val incrRNE = roundBit && (stickyAll || lsb)


  val roundedWide = Cat(0.U(1.W), shiftedFrac) + incrRNE.asUInt
  val carryRnd    = roundedWide(mantWidth)
  val mantAfter   = Mux(carryRnd, roundedWide(mantWidth, 1), roundedWide(mantWidth - 1, 0))
  val expAfter    = newExp + Mux(carryRnd, 1.S(eCalcW.W), 0.S(eCalcW.W))


  when (shiftedFrac === 0.U) {
    resultNormalExp  := 0.U
    resultNormalFrac := 0.U
  }.elsewhen (expAfter < minNormalUnb.S(eCalcW.W)) {
    val preMant    = fracSum(mantWidth - 1, 0)
    val preExp     = expOut
    val kExp       = (minNormalUnb.S(eCalcW.W) - preExp).asUInt
    val dropHidden = preMant(mantWidth - 1)
    val sRaw       = kExp + dropHidden.asUInt
    val sCap       = Mux(sRaw > mantWidth.U, mantWidth.U, sRaw)
    val den        = (preMant >> sCap)(mantWidth - 1, 0)
    resultNormalExp  := 0.U
    resultNormalFrac := den(mantWidth - 2, 0)
  }.elsewhen (expAfter > maxNormalUnb_s) {
    resultNormalExp  := maxEncExp
    resultNormalFrac := 0.U
  }.otherwise {
    val encE = (expAfter + bias.S(eCalcW.W)).asUInt
    resultNormalExp  := encE(expWidth - 1, 0)
    resultNormalFrac := mantAfter(mantWidth - 2, 0)
  }

  val packedNormal  = Cat(resultNormalExp, resultNormalFrac)
  val packedSpecial = WireDefault(0.U(bw.W))
  when (anyNaNIn)       { packedSpecial := Cat(maxEncExp, qNaNFrac) }
    .elsewhen (anyInfIn){ packedSpecial := Cat(maxEncExp, 0.U(fracWidth.W)) }

  val result = Mux(anyNaNIn || anyInfIn, packedSpecial, packedNormal)

  val sr_valid = RegInit(VecInit(Seq.fill(pd)(false.B)))
  val sr_data  = Reg(Vec(pd, UInt(bw.W)))

  sr_valid(0) := io.valid_in
  for (i <- 1 until pd) sr_valid(i) := sr_valid(i - 1)
  when(io.valid_in) { sr_data(0) := result }
  for (i <- 1 until pd) { sr_data(i) := sr_data(i - 1) }

  io.valid_out := sr_valid(pd - 1)
  io.out       := sr_data(pd - 1)
}


