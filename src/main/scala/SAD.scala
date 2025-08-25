package SAD_Code

import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util._
import java.io.PrintWriter
import scala.collection.mutable
import scala.language.postfixOps
import FP_submodules._


class SAD(val bw: Int, val pd: Int, val vecLen: Int = 4, val adderExp: Int) extends Module {
  val io = IO(new Bundle {
    val in_vec_a = Input(Vec(vecLen, UInt(bw.W)))
    val in_vec_b = Input(Vec(vecLen, UInt(bw.W)))
    val valid_in = Input(Bool())
    val out = Output(UInt(bw.W))
    val valid_out = Output(Bool())
  })

  // Instantiate FPSubtractorAbs modules
  val subModules = Seq.fill(vecLen)(Module(new FPSubtractorAbs2(bw, pd, adderExp)))
  val addModules = Seq.fill(vecLen - 1)(Module(new FPAdder2(bw, pd, adderExp)))

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

class SAD2 (val bw: Int, val pd: Int, val vecLen: Int = 4, val adderExp: Int) extends Module {
  override def desiredName: String = s"SAD_bw${bw}_e${adderExp}m${bw - adderExp}"
  require(bw == 4 || bw == 5 || bw == 6)
  val io = IO(new Bundle {
    val in_vec_a = Input(Vec(vecLen, UInt(bw.W)))
    val in_vec_b = Input(Vec(vecLen, UInt(bw.W)))
    val valid_in = Input(Bool())
    //val in_en = Input(Bool())
    val out_s = Output(UInt(bw.W))
    val valid_out = Output(Bool())
  })

  val num_layers = log2Ceil(vecLen)

  val Sub_Wire = Wire(Vec(vecLen, UInt(bw.W)))
  val Sub_valid_wire = Wire(Vec(vecLen, Bool()))
  for (i <- 0 until vecLen) {
    val SubInst = Module(new FPSubtractorAbs2(bw, pd, adderExp ))
    SubInst.io.in_a := io.in_vec_a(i)
    SubInst.io.in_b := io.in_vec_b(i)
    SubInst.io.valid_in := io.valid_in
    //SubInst.io.in_en := io.in_en
    Sub_Wire(i) := SubInst.io.out
    Sub_valid_wire(i) := SubInst.io.valid_out

    //    when(io.valid_in) {
    //      printf(p"[Sub $i] A=${Binary(io.in_vec_a(i))} B=${Binary(io.in_vec_b(i))} => Out=${Binary(Sub_Wire(i))}\n")
    //    }
  }
  var currentlayer = Sub_Wire
  var currentValid = Sub_valid_wire

  for (j <- 0 until num_layers) {
    val layerSize = (currentlayer.length + 1) / 2
    val adder_Wire  = WireInit(VecInit(Seq.fill(layerSize)(0.U(bw.W))))
    val nextValid   = WireInit(VecInit(Seq.fill(layerSize)(false.B)))

    for (h <- 0 until layerSize) {
      if (2 * h + 1 < currentlayer.length) {
        val adderInst = Module(new FPAdder2(bw, pd, adderExp ))
        adderInst.io.in_a := currentlayer(2 * h)
        adderInst.io.in_b := currentlayer(2 * h + 1)
        adderInst.io.valid_in := currentValid(2 * h) && currentValid(2 * h + 1)
        //adderInst.io.in_en := io.in_en
        adder_Wire(h) := adderInst.io.out
        nextValid(h)  := adderInst.io.valid_out

        //        when(io.valid_in) {
        //          printf(p"[Adder L$j-$h] A=${Binary(currentlayer(2 * h))} B=${Binary(currentlayer(2 * h + 1))} => Out=${Binary(adder_Wire(h))}\n")
        //        }
      }
      else {
        adder_Wire(h) := ShiftRegister(currentlayer(2 * h), pd)
        nextValid(h)  := ShiftRegister(currentValid(2 * h), pd)
      }
    }
    currentlayer = adder_Wire
    currentValid = nextValid
  }

  io.out_s := currentlayer.head//Mux(io.in_en, currentlayer.head, 0.U)
  io.valid_out := currentValid.head//ShiftRegister(io.valid_in, (num_layers * pd) + pd)

  //  when(io.valid_out) {
  //    printf(p"[SAD2 OUT] => ${Binary(io.out_s)} (valid)\n")
  //  }

}


