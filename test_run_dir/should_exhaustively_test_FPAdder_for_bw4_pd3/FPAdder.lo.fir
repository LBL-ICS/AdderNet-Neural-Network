FIRRTL version 1.2.0
circuit FPAdder :
  module FPAdder : @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\test\\scala\\GenVerilog\\GenVerilog.scala 20:9]
    input clock : Clock @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\test\\scala\\GenVerilog\\GenVerilog.scala 20:9]
    input reset : UInt<1> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\test\\scala\\GenVerilog\\GenVerilog.scala 20:9]
    input io_in_a : UInt<4> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\test\\scala\\GenVerilog\\GenVerilog.scala 21:16]
    input io_in_b : UInt<4> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\test\\scala\\GenVerilog\\GenVerilog.scala 21:16]
    output io_out : UInt<4> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\test\\scala\\GenVerilog\\GenVerilog.scala 21:16]

    node _io_out_T = add(io_in_a, io_in_b) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\test\\scala\\GenVerilog\\GenVerilog.scala 28:23]
    io_out <= bits(_io_out_T, 3, 0) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\test\\scala\\GenVerilog\\GenVerilog.scala 28:12]
