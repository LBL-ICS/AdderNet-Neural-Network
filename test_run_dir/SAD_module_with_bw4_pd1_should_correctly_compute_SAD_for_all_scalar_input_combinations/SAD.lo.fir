FIRRTL version 1.2.0
circuit SAD :
  module Subtractor : @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 231:7]
    input clock : Clock @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 231:7]
    input reset : UInt<1> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 231:7]
    input io_a : UInt<4> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 232:14]
    input io_b : UInt<4> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 232:14]
    output io_diff : SInt<5> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 232:14]

    node _io_diff_T = asSInt(io_a) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 237:19]
    node _io_diff_T_1 = asSInt(io_b) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 237:33]
    node _io_diff_T_2 = sub(_io_diff_T, _io_diff_T_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 237:26]
    node _io_diff_T_3 = tail(_io_diff_T_2, 1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 237:26]
    node _io_diff_T_4 = asSInt(_io_diff_T_3) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 237:26]
    io_diff <= pad(_io_diff_T_4, 5) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 237:11]

  module AbsVal : @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 243:7]
    input clock : Clock @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 243:7]
    input reset : UInt<1> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 243:7]
    input io_in : SInt<5> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 244:14]
    output io_out : UInt<5> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 244:14]

    node _io_out_T = lt(io_in, asSInt(UInt<1>("h0"))) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 248:23]
    node _io_out_T_1 = sub(asSInt(UInt<1>("h0")), io_in) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 248:31]
    node _io_out_T_2 = tail(_io_out_T_1, 1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 248:31]
    node _io_out_T_3 = asSInt(_io_out_T_2) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 248:31]
    node _io_out_T_4 = asUInt(_io_out_T_3) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 248:39]
    node _io_out_T_5 = asUInt(io_in) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 248:53]
    node _io_out_T_6 = mux(_io_out_T, _io_out_T_4, _io_out_T_5) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 248:16]
    io_out <= _io_out_T_6 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 248:10]

  module SAD : @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 254:7]
    input clock : Clock @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 254:7]
    input reset : UInt<1> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 254:7]
    input io_valid_in : UInt<1> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 261:14]
    input io_a : UInt<4> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 261:14]
    input io_b : UInt<4> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 261:14]
    output io_valid_out : UInt<1> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 261:14]
    output io_sum : UInt<9> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 261:14]

    inst sub of Subtractor @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 269:19]
    inst abs of AbsVal @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 270:19]
    reg validRegs_0 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), validRegs_0) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 277:26]
    reg sumReg : UInt<9>, clock with :
      reset => (UInt<1>("h0"), sumReg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 295:23]
    node absDelay = abs.io_out @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 276:22 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 280:14]
    node _sumReg_T = add(sumReg, absDelay) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 297:22]
    node _sumReg_T_1 = tail(_sumReg_T, 1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 297:22]
    node _GEN_0 = mux(validRegs_0, _sumReg_T_1, sumReg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 295:23 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 296:24 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 297:12]
    node _validRegs_WIRE_0 = UInt<1>("h0") @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 277:34 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 277:34]
    io_valid_out <= validRegs_0 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 301:16]
    io_sum <= sumReg @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 300:10]
    sub.clock <= clock
    sub.reset <= reset
    sub.io_a <= io_a @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 272:12]
    sub.io_b <= io_b @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 273:12]
    abs.clock <= clock
    abs.reset <= reset
    abs.io_in <= sub.io_diff @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 274:13]
    validRegs_0 <= mux(reset, _validRegs_WIRE_0, io_valid_in) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 277:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 277:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 290:16]
    sumReg <= mux(reset, UInt<9>("h0"), _GEN_0) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 295:23 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 295:23]
