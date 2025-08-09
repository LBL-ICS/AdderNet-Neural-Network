FIRRTL version 1.2.0
circuit FPAdder :
  module FPAdder : @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\test\\scala\\GenVerilog\\GenVerilog.scala 13:9]
    input clock : Clock @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\test\\scala\\GenVerilog\\GenVerilog.scala 13:9]
    input reset : UInt<1> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\test\\scala\\GenVerilog\\GenVerilog.scala 13:9]
    input io_in_a : UInt<4> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\test\\scala\\GenVerilog\\GenVerilog.scala 14:16]
    input io_in_b : UInt<4> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\test\\scala\\GenVerilog\\GenVerilog.scala 14:16]
    output io_out : UInt<4> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\test\\scala\\GenVerilog\\GenVerilog.scala 14:16]

    node full_sum = add(io_in_a, io_in_b) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\test\\scala\\GenVerilog\\GenVerilog.scala 21:28]
    node _io_out_T = gt(full_sum, UInt<4>("hf")) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\test\\scala\\GenVerilog\\GenVerilog.scala 25:28]
    node _io_out_T_1 = mux(_io_out_T, UInt<4>("hf"), full_sum) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\test\\scala\\GenVerilog\\GenVerilog.scala 25:18]
    io_out <= bits(_io_out_T_1, 3, 0) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\test\\scala\\GenVerilog\\GenVerilog.scala 25:12]
