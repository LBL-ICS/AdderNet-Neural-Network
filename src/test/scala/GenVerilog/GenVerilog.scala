package GenVerilog

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.simulator.WriteVcdAnnotation
import Fundamental_IC._
import FP_submodules._
import org.scalatest.matchers.must.Matchers  // your modules


object Main extends App {
  emitVerilog(new FPAdder(6, 10 ))
  emitVerilog(new FPSubtractorAbs(6, 10))


}
