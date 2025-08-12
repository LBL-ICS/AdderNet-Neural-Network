import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util._
import java.io.PrintWriter
import scala.collection.mutable
import scala.language.postfixOps
import FP_submodules._





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