package SAD_Code

import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util._
import java.io.PrintWriter
import scala.collection.mutable
import scala.language.postfixOps
import FP_submodules._


class SAD2 (val bw: Int, val pd: Int, val vecLen: Int, val adderExp: Int) extends Module {
  override def desiredName: String = s"SAD_bw${bw}_e${adderExp}m${bw - adderExp}"
  require(bw == 4 || bw == 5 || bw == 6)
  val io = IO(new Bundle {
    val in_vec_a = Input(Vec(vecLen, UInt(bw.W)))
    val in_vec_b = Input(Vec(vecLen, UInt(bw.W)))
    val valid_in = Input(Bool())
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
    Sub_Wire(i) := SubInst.io.out
    Sub_valid_wire(i) := SubInst.io.valid_out

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
        adder_Wire(h) := adderInst.io.out
        nextValid(h)  := adderInst.io.valid_out

      }
      else {
        adder_Wire(h) := ShiftRegister(currentlayer(2 * h), pd)
        nextValid(h)  := ShiftRegister(currentValid(2 * h), pd)
      }
    }
    currentlayer = adder_Wire
    currentValid = nextValid
  }

  io.out_s := currentlayer.head
  io.valid_out := currentValid.head

}


