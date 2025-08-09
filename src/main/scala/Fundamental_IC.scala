package Fundamental_IC
import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util._
import java.io.PrintWriter
import scala.collection.mutable
import scala.language.postfixOps

class wire_connection(bw:Int) extends Module{
  val io=IO(new Bundle {
    val in_a = Input(UInt(bw.W))
    val out_b = Output(UInt(bw.W))
  })
  val w1 = Wire(UInt(bw.W))
  val w2 = Wire(Vec(2, UInt(bw.W)))
  w1 := 7.U(bw.W) & io.in_a
  w2(0) := 3.U(bw.W) | io.in_a
  w2(1) := 5.U(bw.W) & w1
  io.out_b := w2(0) & w2(1)
}

class adder(bw:Int) extends Module{
  val io=IO(new Bundle {
    val in_a = Input(UInt(bw.W))
    val in_b = Input(UInt(bw.W))
    val out_s = Output(UInt((bw+1).W))
  })
  io.out_s := io.in_a + io.in_b
}

class parallel_adder(n: Int, bw: Int) extends Module {
  require(bw == 16 || bw == 32 || bw == 64 || bw == 128)
  val io = IO(new Bundle {
    val in_a = Input(Vec(n, UInt(bw.W)))
    val in_b = Input(Vec(n, UInt(bw.W)))
    val out_s = Output(Vec(n, UInt(bw.W)))
  })
  //val adders = Vector.fill(n)(Module(new adder(bw)).io)
  //adders.zipWithIndex.map(x=>x._1.in_a := io.in_a(x._2))
  //adders.zipWithIndex.map(x=>x._1.in_b := io.in_b(x._2))
  //adders.zipWithIndex.map(x=>io.out_s(x._2) := x._1.out_s)
  val register_layer = RegInit(VecInit.fill(n)(0.U(bw.W)))
  val adder_layer = for (i <-0 until n) yield {
    val adder = Module(new adder(bw)).io
    adder.in_a := io.in_a(i)
    adder.in_b := io.in_b(i)
    register_layer(i) := adder.out_s
    io.out_s(i) := register_layer(i)
  }
}

class register_file(n: Int, bw: Int) extends Module {
  val io = IO(new Bundle {
    val in_d = Input(Vec(n, UInt(bw.W)))
    val out_q = Output(Vec(n, UInt(bw.W)))
  })
  val register_file = RegInit(VecInit.fill(n)(0.U(bw.W)))
  val register_layer = for (i <-0 until n) yield {
    register_file(i) := io.in_d(i)
    io.out_q(i) := register_file(i)
  }
}

class register(bw: Int) extends Module {
  val io = IO(new Bundle {
    val in_d = Input(UInt(bw.W))
    val out_q = Output(UInt(bw.W))
  })
  val register = RegInit(0.U(bw.W))
  register := io.in_d
  io.out_q := register
}

class counter(bw:Int) extends Module{
  val io=IO(new Bundle {
    val in_en = Input(UInt(1.W))
    val out_cnt = Output(UInt(bw.W))
  })
  val cnt = RegInit(0.U(bw.W))
  val nxt_cnt = Wire(UInt(bw.W))
  nxt_cnt := Mux(io.in_en===1.U, Mux(cnt===9.U, 0.U, cnt+1.U), cnt)
  cnt := nxt_cnt
  io.out_cnt := cnt
}

class mux(bw:Int) extends Module {
  val io = IO(new Bundle {
    val in_sel = Input(UInt(1.W))
    val in_a   = Input(UInt(bw.W))
    val in_b   = Input(UInt(bw.W))
    val out_c   = Output(UInt(bw.W))
  })
  when (io.in_sel === 1.U) {
    io.out_c := io.in_a
  }.otherwise {
    io.out_c := io.in_b
  }
}

//if-else cannot be used for constructing hardware
class mux_ifelse(bw:Int) extends Module {
  val io = IO(new Bundle {
    val in_sel = Input(UInt(1.W))
    val in_a   = Input(UInt(bw.W))
    val in_b   = Input(UInt(bw.W))
    val out_c   = Output(UInt(bw.W))
  })
  if (io.in_sel==1.U) {
    io.out_c := io.in_a
  } else {
    io.out_c := io.in_b
  }
}

class syn_ram(bw: Int, depth: Int) extends Module {
  val io = IO {
    new Bundle() {
      val ena = Input(Bool())
      val enb = Input(Bool())
      val wea = Input(UInt((bw/8).W))
      val addra = Input(UInt((log2Ceil(depth).W)))
      val addrb = Input(UInt((log2Ceil(depth).W)))
      val dina = Input(UInt(bw.W))
      val doutb = Output(UInt(bw.W))
    }
  }
  val mem = RegInit(VecInit.fill(depth)(VecInit.fill(bw / 8)(0.U(8.W))))
  when(io.ena) {
    for (i <- 0 until bw / 8) {
      when(io.wea(i)) {
        mem(io.addra)(i) := io.dina(8 * (i + 1) - 1, 8 * i)
      }
    }
  }

  val data_out = RegInit(0.U(bw.W))
  when(io.enb) {
    data_out := Cat(mem(io.addrb).reverse)
  }
  io.doutb := data_out
}

class asyn_ram(bw: Int, depth: Int) extends Module {
  val io = IO(new Bundle {
    val clka = Input(Clock()) // Write clock
    val clkb = Input(Clock()) // Read clock
    val ena = Input(Bool()) // Enable write
    val enb = Input(Bool()) // Enable read
    val wea = Input(Vec(bw / 8, Bool())) // Write enable mask (one bit per byte)
    val addra = Input(UInt(log2Ceil(depth).W)) // Write address
    val addrb = Input(UInt(log2Ceil(depth).W)) // Read address
    val dina = Input(UInt(bw.W)) // Write data
    val doutb = Output(UInt(bw.W)) // Read data
  })

  // Memory Array (depth x bw bits wide)
  val mem = SyncReadMem(depth, UInt(bw.W)) // Memory for storing bw-bit wide words

  // Write logic on clka with byte-level write enable mask
  withClock(io.clka) {
    val data_in = WireDefault(0.U(bw.W))
    when(io.ena) {
      // Byte-wise write masking
      for (i <- 0 until bw / 8) {
        when(io.wea(i)) { // Only write the byte if the corresponding bit in wea is set
          data_in := io.dina(8 * (i + 1) - 1, 8 * i)
        }
      }
      mem.write(io.addra, data_in) // Write back the modified word
    }
  }

  // Read logic on clkb
  withClock(io.clkb) {
    val data_out = Wire(UInt(bw.W))
    when(io.enb) {
      data_out := mem.read(io.addrb) // Read from memory on clkb
    }.otherwise {
      data_out := 0.U // Output zero when not enabled
    }
    io.doutb := data_out
  }
}

class shift_register(depth: Int,bw: Int) extends Module{
  val io = IO(new Bundle() {
    val in = Input(UInt(bw.W))
    val out = Output(UInt(bw.W))
  })
  val reg = RegInit(VecInit.fill(depth)(0.U(bw.W)))
  reg(0) := io.in
  for(i <- 1 until depth){
    reg(i) := reg(i-1)
  }
  io.out := reg(depth - 1)
}



class PipelineReg[T <: Data](gen: T, val depth: Int) extends Module {
  val io = IO(new Bundle {
    val in  = Input(gen)
    val en  = Input(Bool())
    val out = Output(gen)
  })

  require(depth >= 1)
  if (depth == 1) {
    val reg = RegEnable(io.in, 0.U.asTypeOf(gen), io.en)
    io.out := reg
  } else {
    val stages = Seq.fill(depth)(Reg(gen))
    when(io.en) {
      stages.head := io.in
      for (i <- 1 until depth) stages(i) := stages(i - 1)
    }
    io.out := stages.last
  }
}
// Leading Zero Counter for normalization
class LZC(val width: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(width.W))
    val out = Output(UInt(log2Ceil(width + 1).W))
  })

  io.out := PriorityEncoder(io.in.asBools.reverse)
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

  val expWidth = 2
  val fracWidth = bw - expWidth - 1 // 1 sign bit

  // Extract fields
  val signA = io.in_a(bw - 1)
  val expA = io.in_a(bw - 2, fracWidth)
  val fracA = io.in_a(fracWidth - 1, 0)

  val signB = io.in_b(bw - 1)
  val expB = io.in_b(bw - 2, fracWidth)
  val fracB = io.in_b(fracWidth - 1, 0)

  // Add implicit 1 for normalized values
  val normFracA = Cat(1.U(1.W), fracA)
  val normFracB = Cat(1.U(1.W), fracB)

  // Compare exponents
  val expDiff = Wire(UInt((expWidth + 1).W))
  val expAGtB = expA > expB
  expDiff := Mux(expAGtB, expA - expB, expB - expA)

  val alignedLarge = Wire(UInt((fracWidth + 2).W))
  val alignedSmall = Wire(UInt((fracWidth + 2).W))
  val expOut = Wire(UInt(expWidth.W))

  when(expAGtB) {
    alignedLarge := normFracA
    alignedSmall := normFracB >> expDiff
    expOut := expA
  }.otherwise {
    alignedLarge := normFracB
    alignedSmall := normFracA >> expDiff
    expOut := expB
  }

  // Add aligned mantissas
  val fracSum = alignedLarge + alignedSmall
  val carryOut = fracSum(fracWidth + 1)

  // Leading Zero Counter for normalization
  val lzc = Module(new LZC(fracWidth + 2))
  lzc.io.in := fracSum
  val shiftAmt = lzc.io.out

  val shiftedFrac = Wire(UInt((fracWidth + 2).W))
  val newExp = Wire(UInt(expWidth.W))

  when(carryOut) {
    // Overflow, shift right
    shiftedFrac := fracSum >> 1
    newExp := expOut + 1.U
  }.elsewhen(fracSum(fracWidth + 1) === 0.U && fracSum(fracWidth) === 1.U) {
    // Already normalized
    shiftedFrac := fracSum
    newExp := expOut
  }.otherwise {
    // Normalize by left shifting
    shiftedFrac := fracSum << shiftAmt
    newExp := Mux(expOut >= shiftAmt, expOut - shiftAmt, 0.U)
  }

  val normFrac = shiftedFrac(fracWidth - 1, 0)
  val normExp = newExp

  val result = Cat(0.U(1.W), normExp, normFrac)

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
  when(io.valid_in) {
    printf("[STEP 1: Extract Fields]\n")
    printf("  A = %b => signA: %b | expA: %b | fracA: %b\n", io.in_a, signA, expA, fracA)
    printf("  B = %b => signB: %b | expB: %b | fracB: %b\n", io.in_b, signB, expB, fracB)

    printf("[STEP 2: Compare Exponents]\n")
    printf("  expA: %d, expB: %d\n", expA, expB)
    printf("  expDiff: %d => expAGtB: %b\n", expDiff, expAGtB)

    printf("[STEP 3: Align Mantissas]\n")
    printf("  normFracA (with hidden 1): %b\n", normFracA)
    printf("  normFracB (with hidden 1): %b\n", normFracB)
    printf("  alignedLarge: %b | alignedSmall: %b\n", alignedLarge, alignedSmall)
    printf("  selectedExpOut: %d\n", expOut)

    printf("[STEP 4: Add Aligned Mantissas]\n")
    printf("  sum: %b (carryOut = %b)\n", fracSum, carryOut)

    printf("[STEP 5: Normalize Result]\n")
    when(carryOut) {
      printf("  CarryOut = 1 => shift right by 1\n")
    }.elsewhen(fracSum(fracWidth + 1) === 0.U && fracSum(fracWidth) === 1.U) {
      printf("  Already normalized => no shift needed\n")
    }.otherwise {
      printf("  No CarryOut => LZC = %d | shift left\n", shiftAmt)
    }
    printf("  normFrac: %b | normExp: %d\n", normFrac, normExp)

    printf("[STEP 6: Final Result Reassembly]\n")
    printf("  Final result = %b\n", result)

    printf("[Cycle Info] bw = %d | pd = %d => valid_out: %b | output: %b\n",
      bw.U, pd.U, io.valid_out, io.out)
  }
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

class SAD(val bw: Int, val pd: Int, val vecLen: Int = 4) extends Module {
  val io = IO(new Bundle {
    val in_vec_a = Input(Vec(vecLen, UInt(bw.W)))
    val in_vec_b = Input(Vec(vecLen, UInt(bw.W)))
    val valid_in = Input(Bool())
    val out = Output(UInt(bw.W))
    val valid_out = Output(Bool())
  })

  // Instantiate FPSubtractorAbs modules
  val subModules = Seq.fill(vecLen)(Module(new FPSubtractorAbs(bw, pd)))
  val addModules = Seq.fill(vecLen - 1)(Module(new FPAdder(bw, pd)))

  // Connect inputs to subtractor modules
  for (i <- 0 until vecLen) {
    subModules(i).io.in_a := io.in_vec_a(i)
    subModules(i).io.in_b := io.in_vec_b(i)
    subModules(i).io.valid_in := io.valid_in
  }

  // Connect output of subtractors to first adder
  addModules(0).io.in_a := subModules(0).io.out
  addModules(0).io.in_b := subModules(1).io.out
  addModules(0).io.valid_in := subModules(0).io.valid_out && subModules(1).io.valid_out

  // Chain remaining adders
  for (i <- 1 until addModules.length) {
    addModules(i).io.in_a := addModules(i - 1).io.out
    addModules(i).io.in_b := subModules(i + 1).io.out
    addModules(i).io.valid_in := addModules(i - 1).io.valid_out && subModules(i + 1).io.valid_out
  }

  // Output from last adder
  io.out := addModules.last.io.out
  io.valid_out := addModules.last.io.valid_out
}








object Main extends App {
  emitVerilog(new FPAdder(6, 10 ))
  emitVerilog(new FPSubtractorAbs(6, 10))


}
