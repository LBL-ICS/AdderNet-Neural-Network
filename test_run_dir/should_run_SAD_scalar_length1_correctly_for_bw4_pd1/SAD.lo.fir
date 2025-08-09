FIRRTL version 1.2.0
circuit SAD :
  module Subtractor : @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 226:7]
    input clock : Clock @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 226:7]
    input reset : UInt<1> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 226:7]
    input io_a : UInt<4> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 227:14]
    input io_b : UInt<4> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 227:14]
    output io_diff : SInt<5> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 227:14]

    node _io_diff_T = asSInt(io_a) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 232:19]
    node _io_diff_T_1 = asSInt(io_b) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 232:33]
    node _io_diff_T_2 = sub(_io_diff_T, _io_diff_T_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 232:26]
    node _io_diff_T_3 = tail(_io_diff_T_2, 1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 232:26]
    node _io_diff_T_4 = asSInt(_io_diff_T_3) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 232:26]
    io_diff <= pad(_io_diff_T_4, 5) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 232:11]

  module AbsVal : @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 238:7]
    input clock : Clock @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 238:7]
    input reset : UInt<1> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 238:7]
    input io_in : SInt<5> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 239:14]
    output io_out : UInt<5> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 239:14]

    node _io_out_T = lt(io_in, asSInt(UInt<1>("h0"))) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 243:23]
    node _io_out_T_1 = sub(asSInt(UInt<1>("h0")), io_in) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 243:31]
    node _io_out_T_2 = tail(_io_out_T_1, 1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 243:31]
    node _io_out_T_3 = asSInt(_io_out_T_2) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 243:31]
    node _io_out_T_4 = asUInt(_io_out_T_3) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 243:39]
    node _io_out_T_5 = asUInt(io_in) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 243:53]
    node _io_out_T_6 = mux(_io_out_T, _io_out_T_4, _io_out_T_5) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 243:16]
    io_out <= _io_out_T_6 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 243:10]

  module AdderTree : @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 250:7]
    input clock : Clock @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 250:7]
    input reset : UInt<1> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 250:7]
    input io_in_0 : UInt<5> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 252:14]
    output io_out : UInt<5> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 252:14]

    io_out <= io_in_0 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 269:10]

  module SAD : @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 275:7]
    input clock : Clock @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 275:7]
    input reset : UInt<1> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 275:7]
    input io_valid_in : UInt<1> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 284:14]
    input io_a_0 : UInt<4> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 284:14]
    input io_b_0 : UInt<4> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 284:14]
    output io_valid_out : UInt<1> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 284:14]
    output io_sum : UInt<5> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 284:14]

    inst subs_0 of Subtractor @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 293:38]
    inst abss_0 of AbsVal @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 294:38]
    inst adderTree of AdderTree @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 313:25]
    reg validRegs_0 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), validRegs_0) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 317:26]
    node absVecRaw_0 = abss_0.io_out @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 302:23 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 303:44]
    node _validRegs_WIRE_0 = UInt<1>("h0") @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 317:34 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 317:34]
    io_valid_out <= validRegs_0 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 324:16]
    io_sum <= adderTree.io_out @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 323:16]
    subs_0.clock <= clock
    subs_0.reset <= reset
    subs_0.io_a <= io_a_0 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 297:18]
    subs_0.io_b <= io_b_0 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 298:18]
    abss_0.clock <= clock
    abss_0.reset <= reset
    abss_0.io_in <= subs_0.io_diff @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:19]
    adderTree.clock <= clock
    adderTree.reset <= reset
    adderTree.io_in_0 <= absVecRaw_0 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 314:19]
    validRegs_0 <= mux(reset, _validRegs_WIRE_0, io_valid_in) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 317:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 317:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 318:16]
