FIRRTL version 1.2.0
circuit SAD :
  module Subtractor : @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 224:7]
    input clock : Clock @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 224:7]
    input reset : UInt<1> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 224:7]
    input io_a : UInt<6> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 225:14]
    input io_b : UInt<6> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 225:14]
    output io_diff : SInt<7> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 225:14]

    node _a_ext_T = pad(io_a, 7) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 231:23]
    node a_ext = asSInt(_a_ext_T) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 231:32]
    node _b_ext_T = pad(io_b, 7) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 232:23]
    node b_ext = asSInt(_b_ext_T) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 232:32]
    node _io_diff_T = sub(a_ext, b_ext) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 234:20]
    node _io_diff_T_1 = tail(_io_diff_T, 1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 234:20]
    node _io_diff_T_2 = asSInt(_io_diff_T_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 234:20]
    io_diff <= _io_diff_T_2 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 234:11]

  module AbsVal : @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 238:7]
    input clock : Clock @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 238:7]
    input reset : UInt<1> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 238:7]
    input io_in : SInt<7> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 239:14]
    output io_out : UInt<7> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 239:14]

    node _io_out_T = lt(io_in, asSInt(UInt<1>("h0"))) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 243:23]
    node _io_out_T_1 = sub(asSInt(UInt<1>("h0")), io_in) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 243:31]
    node _io_out_T_2 = tail(_io_out_T_1, 1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 243:31]
    node _io_out_T_3 = asSInt(_io_out_T_2) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 243:31]
    node _io_out_T_4 = asUInt(_io_out_T_3) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 243:39]
    node _io_out_T_5 = asUInt(io_in) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 243:53]
    node _io_out_T_6 = mux(_io_out_T, _io_out_T_4, _io_out_T_5) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 243:16]
    io_out <= _io_out_T_6 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 243:10]

  module AdderTree : @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 246:7]
    input clock : Clock @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 246:7]
    input reset : UInt<1> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 246:7]
    input io_in_0 : UInt<7> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 248:14]
    input io_in_1 : UInt<7> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 248:14]
    input io_in_2 : UInt<7> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 248:14]
    input io_in_3 : UInt<7> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 248:14]
    output io_out : UInt<9> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 248:14]

    node _io_out_next_T = add(io_in_0, io_in_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 258:37]
    reg io_out_next_0 : UInt<8>, clock with :
      reset => (UInt<1>("h0"), io_out_next_0) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 258:34]
    node _io_out_next_T_1 = add(io_in_2, io_in_3) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 258:37]
    reg io_out_next_1 : UInt<8>, clock with :
      reset => (UInt<1>("h0"), io_out_next_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 258:34]
    node _io_out_next_T_2 = add(io_out_next_0, io_out_next_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 258:37]
    reg io_out_next_0_1 : UInt<9>, clock with :
      reset => (UInt<1>("h0"), io_out_next_0_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 258:34]
    io_out <= io_out_next_0_1 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 265:10]
    io_out_next_0 <= _io_out_next_T @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 258:34]
    io_out_next_1 <= _io_out_next_T_1 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 258:34]
    io_out_next_0_1 <= _io_out_next_T_2 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 258:34]

  module SAD : @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 268:7]
    input clock : Clock @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 268:7]
    input reset : UInt<1> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 268:7]
    input io_valid_in : UInt<1> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 277:14]
    input io_a_0 : UInt<6> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 277:14]
    input io_a_1 : UInt<6> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 277:14]
    input io_a_2 : UInt<6> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 277:14]
    input io_a_3 : UInt<6> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 277:14]
    input io_b_0 : UInt<6> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 277:14]
    input io_b_1 : UInt<6> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 277:14]
    input io_b_2 : UInt<6> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 277:14]
    input io_b_3 : UInt<6> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 277:14]
    output io_valid_out : UInt<1> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 277:14]
    output io_sum : UInt<9> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 277:14]

    inst subs_0 of Subtractor @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 286:37]
    inst subs_1 of Subtractor @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 286:37]
    inst subs_2 of Subtractor @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 286:37]
    inst subs_3 of Subtractor @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 286:37]
    inst abss_0 of AbsVal @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 287:37]
    inst abss_1 of AbsVal @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 287:37]
    inst abss_2 of AbsVal @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 287:37]
    inst abss_3 of AbsVal @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 287:37]
    inst adderTree of AdderTree @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 312:25]
    reg absVecPiped_0_REG : UInt<7>, clock with :
      reset => (UInt<1>("h0"), absVecPiped_0_REG) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 304:32]
    reg absVecPiped_1_REG : UInt<7>, clock with :
      reset => (UInt<1>("h0"), absVecPiped_1_REG) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 304:32]
    reg absVecPiped_2_REG : UInt<7>, clock with :
      reset => (UInt<1>("h0"), absVecPiped_2_REG) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 304:32]
    reg absVecPiped_3_REG : UInt<7>, clock with :
      reset => (UInt<1>("h0"), absVecPiped_3_REG) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 304:32]
    reg validRegs_0 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), validRegs_0) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 316:26]
    reg validRegs_1 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), validRegs_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 316:26]
    reg validRegs_2 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), validRegs_2) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 316:26]
    reg validRegs_3 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), validRegs_3) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 316:26]
    reg validRegs_4 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), validRegs_4) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 316:26]
    reg validRegs_5 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), validRegs_5) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 316:26]
    reg validRegs_6 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), validRegs_6) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 316:26]
    node absVecRaw_0 = abss_0.io_out @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 295:23 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 297:18]
    node absVecRaw_1 = abss_1.io_out @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 295:23 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 297:18]
    node absVecRaw_2 = abss_2.io_out @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 295:23 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 297:18]
    node absVecRaw_3 = abss_3.io_out @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 295:23 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 297:18]
    node absVecPiped_0 = absVecPiped_0_REG @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 301:25 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 304:22]
    node absVecPiped_1 = absVecPiped_1_REG @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 301:25 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 304:22]
    node absVecPiped_2 = absVecPiped_2_REG @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 301:25 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 304:22]
    node absVecPiped_3 = absVecPiped_3_REG @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 301:25 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 304:22]
    node _validRegs_WIRE_0 = UInt<1>("h0") @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 316:34 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 316:34]
    node _validRegs_WIRE_1 = UInt<1>("h0") @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 316:34 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 316:34]
    node _validRegs_WIRE_2 = UInt<1>("h0") @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 316:34 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 316:34]
    node _validRegs_WIRE_3 = UInt<1>("h0") @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 316:34 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 316:34]
    node _validRegs_WIRE_4 = UInt<1>("h0") @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 316:34 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 316:34]
    node _validRegs_WIRE_5 = UInt<1>("h0") @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 316:34 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 316:34]
    node _validRegs_WIRE_6 = UInt<1>("h0") @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 316:34 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 316:34]
    io_valid_out <= validRegs_6 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 323:16]
    io_sum <= adderTree.io_out @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 322:10]
    subs_0.clock <= clock
    subs_0.reset <= reset
    subs_0.io_a <= io_a_0 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 290:18]
    subs_0.io_b <= io_b_0 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 291:18]
    subs_1.clock <= clock
    subs_1.reset <= reset
    subs_1.io_a <= io_a_1 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 290:18]
    subs_1.io_b <= io_b_1 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 291:18]
    subs_2.clock <= clock
    subs_2.reset <= reset
    subs_2.io_a <= io_a_2 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 290:18]
    subs_2.io_b <= io_b_2 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 291:18]
    subs_3.clock <= clock
    subs_3.reset <= reset
    subs_3.io_a <= io_a_3 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 290:18]
    subs_3.io_b <= io_b_3 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 291:18]
    abss_0.clock <= clock
    abss_0.reset <= reset
    abss_0.io_in <= subs_0.io_diff @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 292:19]
    abss_1.clock <= clock
    abss_1.reset <= reset
    abss_1.io_in <= subs_1.io_diff @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 292:19]
    abss_2.clock <= clock
    abss_2.reset <= reset
    abss_2.io_in <= subs_2.io_diff @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 292:19]
    abss_3.clock <= clock
    abss_3.reset <= reset
    abss_3.io_in <= subs_3.io_diff @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 292:19]
    absVecPiped_0_REG <= absVecRaw_0 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 304:32]
    absVecPiped_1_REG <= absVecRaw_1 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 304:32]
    absVecPiped_2_REG <= absVecRaw_2 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 304:32]
    absVecPiped_3_REG <= absVecRaw_3 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 304:32]
    adderTree.clock <= clock
    adderTree.reset <= reset
    adderTree.io_in_0 <= absVecPiped_0 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 313:19]
    adderTree.io_in_1 <= absVecPiped_1 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 313:19]
    adderTree.io_in_2 <= absVecPiped_2 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 313:19]
    adderTree.io_in_3 <= absVecPiped_3 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 313:19]
    validRegs_0 <= mux(reset, _validRegs_WIRE_0, io_valid_in) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 316:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 316:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 317:16]
    validRegs_1 <= mux(reset, _validRegs_WIRE_1, validRegs_0) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 316:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 316:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 319:18]
    validRegs_2 <= mux(reset, _validRegs_WIRE_2, validRegs_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 316:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 316:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 319:18]
    validRegs_3 <= mux(reset, _validRegs_WIRE_3, validRegs_2) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 316:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 316:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 319:18]
    validRegs_4 <= mux(reset, _validRegs_WIRE_4, validRegs_3) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 316:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 316:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 319:18]
    validRegs_5 <= mux(reset, _validRegs_WIRE_5, validRegs_4) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 316:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 316:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 319:18]
    validRegs_6 <= mux(reset, _validRegs_WIRE_6, validRegs_5) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 316:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 316:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 319:18]
