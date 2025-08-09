FIRRTL version 1.2.0
circuit SAD :
  module Subtractor : @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 223:7]
    input clock : Clock @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 223:7]
    input reset : UInt<1> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 223:7]
    input io_a : UInt<4> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 224:14]
    input io_b : UInt<4> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 224:14]
    output io_diff : SInt<5> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 224:14]

    node _io_diff_T = asSInt(io_a) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 229:19]
    node _io_diff_T_1 = asSInt(io_b) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 229:33]
    node _io_diff_T_2 = sub(_io_diff_T, _io_diff_T_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 229:26]
    node _io_diff_T_3 = tail(_io_diff_T_2, 1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 229:26]
    node _io_diff_T_4 = asSInt(_io_diff_T_3) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 229:26]
    io_diff <= pad(_io_diff_T_4, 5) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 229:11]

  module AbsVal : @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 233:7]
    input clock : Clock @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 233:7]
    input reset : UInt<1> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 233:7]
    input io_in : SInt<5> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 234:14]
    output io_out : UInt<5> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 234:14]

    node _io_out_T = lt(io_in, asSInt(UInt<1>("h0"))) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 238:23]
    node _io_out_T_1 = sub(asSInt(UInt<1>("h0")), io_in) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 238:31]
    node _io_out_T_2 = tail(_io_out_T_1, 1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 238:31]
    node _io_out_T_3 = asSInt(_io_out_T_2) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 238:31]
    node _io_out_T_4 = asUInt(_io_out_T_3) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 238:39]
    node _io_out_T_5 = asUInt(io_in) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 238:53]
    node _io_out_T_6 = mux(_io_out_T, _io_out_T_4, _io_out_T_5) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 238:16]
    io_out <= _io_out_T_6 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 238:10]

  module AdderTree : @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 242:7]
    input clock : Clock @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 242:7]
    input reset : UInt<1> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 242:7]
    input io_in_0 : UInt<5> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 243:14]
    input io_in_1 : UInt<5> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 243:14]
    input io_in_2 : UInt<5> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 243:14]
    input io_in_3 : UInt<5> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 243:14]
    output io_out : UInt<7> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 243:14]

    node _io_out_pairs_T = add(io_in_0, io_in_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 255:37]
    reg io_out_pairs_0 : UInt<6>, clock with :
      reset => (UInt<1>("h0"), io_out_pairs_0) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 255:34]
    node _io_out_pairs_T_1 = add(io_in_2, io_in_3) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 255:37]
    reg io_out_pairs_1 : UInt<6>, clock with :
      reset => (UInt<1>("h0"), io_out_pairs_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 255:34]
    node _io_out_pairs_T_2 = add(io_out_pairs_0, io_out_pairs_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 255:37]
    reg io_out_pairs_0_1 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), io_out_pairs_0_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 255:34]
    io_out <= io_out_pairs_0_1 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 262:10]
    io_out_pairs_0 <= _io_out_pairs_T @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 255:34]
    io_out_pairs_1 <= _io_out_pairs_T_1 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 255:34]
    io_out_pairs_0_1 <= _io_out_pairs_T_2 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 255:34]

  module SAD : @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 266:7]
    input clock : Clock @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 266:7]
    input reset : UInt<1> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 266:7]
    input io_valid_in : UInt<1> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 270:14]
    input io_a_0 : UInt<4> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 270:14]
    input io_a_1 : UInt<4> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 270:14]
    input io_a_2 : UInt<4> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 270:14]
    input io_a_3 : UInt<4> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 270:14]
    input io_b_0 : UInt<4> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 270:14]
    input io_b_1 : UInt<4> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 270:14]
    input io_b_2 : UInt<4> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 270:14]
    input io_b_3 : UInt<4> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 270:14]
    output io_valid_out : UInt<1> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 270:14]
    output io_sum : UInt<13> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 270:14]

    inst subtractors_0 of Subtractor @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 279:44]
    inst subtractors_1 of Subtractor @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 279:44]
    inst subtractors_2 of Subtractor @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 279:44]
    inst subtractors_3 of Subtractor @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 279:44]
    inst absvals_0 of AbsVal @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 280:40]
    inst absvals_1 of AbsVal @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 280:40]
    inst absvals_2 of AbsVal @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 280:40]
    inst absvals_3 of AbsVal @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 280:40]
    inst adderTree of AdderTree @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 295:25]
    reg validRegs_0 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), validRegs_0) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:26]
    reg validRegs_1 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), validRegs_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:26]
    reg validRegs_2 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), validRegs_2) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:26]
    reg validRegs_3 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), validRegs_3) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:26]
    reg validRegs_4 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), validRegs_4) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:26]
    reg validRegs_5 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), validRegs_5) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:26]
    reg validRegs_6 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), validRegs_6) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:26]
    node absDiffs_0 = absvals_0.io_out @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 289:22 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 291:17]
    node absDiffs_1 = absvals_1.io_out @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 289:22 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 291:17]
    node absDiffs_2 = absvals_2.io_out @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 289:22 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 291:17]
    node absDiffs_3 = absvals_3.io_out @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 289:22 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 291:17]
    node _validRegs_WIRE_0 = UInt<1>("h0") @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:34 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:34]
    node _validRegs_WIRE_1 = UInt<1>("h0") @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:34 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:34]
    node _validRegs_WIRE_2 = UInt<1>("h0") @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:34 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:34]
    node _validRegs_WIRE_3 = UInt<1>("h0") @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:34 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:34]
    node _validRegs_WIRE_4 = UInt<1>("h0") @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:34 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:34]
    node _validRegs_WIRE_5 = UInt<1>("h0") @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:34 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:34]
    node _validRegs_WIRE_6 = UInt<1>("h0") @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:34 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:34]
    io_valid_out <= validRegs_6 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 307:16]
    io_sum <= pad(adderTree.io_out, 13) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 306:10]
    subtractors_0.clock <= clock
    subtractors_0.reset <= reset
    subtractors_0.io_a <= io_a_0 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 283:25]
    subtractors_0.io_b <= io_b_0 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 284:25]
    subtractors_1.clock <= clock
    subtractors_1.reset <= reset
    subtractors_1.io_a <= io_a_1 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 283:25]
    subtractors_1.io_b <= io_b_1 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 284:25]
    subtractors_2.clock <= clock
    subtractors_2.reset <= reset
    subtractors_2.io_a <= io_a_2 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 283:25]
    subtractors_2.io_b <= io_b_2 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 284:25]
    subtractors_3.clock <= clock
    subtractors_3.reset <= reset
    subtractors_3.io_a <= io_a_3 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 283:25]
    subtractors_3.io_b <= io_b_3 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 284:25]
    absvals_0.clock <= clock
    absvals_0.reset <= reset
    absvals_0.io_in <= subtractors_0.io_diff @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 285:22]
    absvals_1.clock <= clock
    absvals_1.reset <= reset
    absvals_1.io_in <= subtractors_1.io_diff @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 285:22]
    absvals_2.clock <= clock
    absvals_2.reset <= reset
    absvals_2.io_in <= subtractors_2.io_diff @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 285:22]
    absvals_3.clock <= clock
    absvals_3.reset <= reset
    absvals_3.io_in <= subtractors_3.io_diff @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 285:22]
    adderTree.clock <= clock
    adderTree.reset <= reset
    adderTree.io_in_0 <= absDiffs_0 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 296:19]
    adderTree.io_in_1 <= absDiffs_1 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 296:19]
    adderTree.io_in_2 <= absDiffs_2 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 296:19]
    adderTree.io_in_3 <= absDiffs_3 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 296:19]
    validRegs_0 <= mux(reset, _validRegs_WIRE_0, io_valid_in) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 300:16]
    validRegs_1 <= mux(reset, _validRegs_WIRE_1, validRegs_0) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 302:18]
    validRegs_2 <= mux(reset, _validRegs_WIRE_2, validRegs_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 302:18]
    validRegs_3 <= mux(reset, _validRegs_WIRE_3, validRegs_2) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 302:18]
    validRegs_4 <= mux(reset, _validRegs_WIRE_4, validRegs_3) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 302:18]
    validRegs_5 <= mux(reset, _validRegs_WIRE_5, validRegs_4) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 302:18]
    validRegs_6 <= mux(reset, _validRegs_WIRE_6, validRegs_5) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 302:18]
