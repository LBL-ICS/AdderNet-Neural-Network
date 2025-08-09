FIRRTL version 1.2.0
circuit FP4Subtractor :
  module FP4Subtractor : @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 265:7]
    input clock : Clock @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 265:7]
    input reset : UInt<1> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 265:7]
    input io_in_en : UInt<1> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 269:14]
    input io_in_valid : UInt<1> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 269:14]
    input io_a : UInt<4> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 269:14]
    input io_b : UInt<4> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 269:14]
    output io_out : UInt<4> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 269:14]
    output io_out_valid : UInt<1> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 269:14]

    node a_exp = bits(io_a, 3, 2) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 282:19]
    node b_exp = bits(io_b, 3, 2) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 283:19]
    node _a_man_T = bits(io_a, 1, 0) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 284:33]
    node a_man = cat(UInt<1>("h1"), _a_man_T) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 284:18]
    node _b_man_T = bits(io_b, 1, 0) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 285:33]
    node b_man = cat(UInt<1>("h1"), _b_man_T) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 285:18]
    node _exp_diff_T = asSInt(a_exp) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 288:25]
    node _exp_diff_T_1 = asSInt(b_exp) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 288:40]
    node _exp_diff_T_2 = sub(_exp_diff_T, _exp_diff_T_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 288:32]
    node _exp_diff_T_3 = tail(_exp_diff_T_2, 1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 288:32]
    node exp_diff = asSInt(_exp_diff_T_3) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 288:32]
    node _T = gt(exp_diff, asSInt(UInt<1>("h0"))) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 294:17]
    node _aligned_b_man_T = asUInt(exp_diff) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 296:40]
    node _aligned_b_man_T_1 = dshr(b_man, _aligned_b_man_T) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 296:28]
    node _aligned_a_man_T = sub(asSInt(UInt<1>("h0")), exp_diff) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:32]
    node _aligned_a_man_T_1 = tail(_aligned_a_man_T, 1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:32]
    node _aligned_a_man_T_2 = asSInt(_aligned_a_man_T_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:32]
    node _aligned_a_man_T_3 = asUInt(_aligned_a_man_T_2) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:43]
    node _aligned_a_man_T_4 = dshr(a_man, _aligned_a_man_T_3) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:28]
    node _GEN_0 = mux(_T, a_man, _aligned_a_man_T_4) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 294:24 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 295:19 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 299:19]
    node _GEN_1 = mux(_T, _aligned_b_man_T_1, b_man) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 294:24 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 296:19 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 300:19]
    node _GEN_2 = mux(_T, a_exp, b_exp) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 294:24 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 297:19 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 301:19]
    node aligned_a_man = _GEN_0 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 290:27]
    node _diff_sint_T = asSInt(aligned_a_man) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 305:34]
    node aligned_b_man = _GEN_1 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 291:27]
    node _diff_sint_T_1 = asSInt(aligned_b_man) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 305:57]
    node _diff_sint_T_2 = sub(_diff_sint_T, _diff_sint_T_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 305:41]
    node _diff_sint_T_3 = tail(_diff_sint_T_2, 1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 305:41]
    node diff_sint = asSInt(_diff_sint_T_3) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 305:41]
    node _abs_diff_T = lt(diff_sint, asSInt(UInt<1>("h0"))) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 306:32]
    node _abs_diff_T_1 = sub(asSInt(UInt<1>("h0")), diff_sint) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 306:40]
    node _abs_diff_T_2 = tail(_abs_diff_T_1, 1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 306:40]
    node _abs_diff_T_3 = asSInt(_abs_diff_T_2) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 306:40]
    node _abs_diff_T_4 = asUInt(_abs_diff_T_3) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 306:52]
    node _abs_diff_T_5 = asUInt(diff_sint) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 306:70]
    node abs_diff = mux(_abs_diff_T, _abs_diff_T_4, _abs_diff_T_5) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 306:21]
    node shift_amount_left = bits(abs_diff, 2, 1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 315:19]
    node shift_amount_right = bits(abs_diff, 0, 0) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 316:20]
    node shift_amount_leftCount_left = bits(shift_amount_left, 1, 1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 315:19]
    node shift_amount_leftCount_right = bits(shift_amount_left, 0, 0) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 316:20]
    node _shift_amount_leftCount_leftCount_T = not(shift_amount_leftCount_left) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 312:8]
    node shift_amount_leftCount_leftCount = bits(_shift_amount_leftCount_leftCount_T, 0, 0) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 312:11]
    node _shift_amount_leftCount_rightCount_T = not(shift_amount_leftCount_right) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 312:8]
    node shift_amount_leftCount_rightCount = bits(_shift_amount_leftCount_rightCount_T, 0, 0) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 312:11]
    node shift_amount_leftCount_leftAllZeros = eq(shift_amount_leftCount_left, UInt<1>("h0")) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 320:31]
    node _shift_amount_leftCount_T = add(shift_amount_leftCount_rightCount, UInt<1>("h1")) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 322:36]
    node _shift_amount_leftCount_T_1 = tail(_shift_amount_leftCount_T, 1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 322:36]
    node shift_amount_leftCount = mux(shift_amount_leftCount_leftAllZeros, _shift_amount_leftCount_T_1, shift_amount_leftCount_leftCount) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 322:10]
    node _shift_amount_rightCount_T = not(shift_amount_right) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 312:8]
    node shift_amount_rightCount = bits(_shift_amount_rightCount_T, 0, 0) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 312:11]
    node shift_amount_leftAllZeros = eq(shift_amount_left, UInt<1>("h0")) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 320:31]
    node _shift_amount_T = add(shift_amount_rightCount, UInt<2>("h2")) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 322:36]
    node _shift_amount_T_1 = tail(_shift_amount_T, 1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 322:36]
    node shift_amount = mux(shift_amount_leftAllZeros, _shift_amount_T_1, shift_amount_leftCount) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 322:10]
    node norm_man = dshl(abs_diff, shift_amount) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 327:27]
    node aligned_exp = _GEN_2 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 292:27]
    node _norm_exp_T = geq(aligned_exp, shift_amount) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 328:34]
    node _norm_exp_T_1 = sub(aligned_exp, shift_amount) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 328:63]
    node _norm_exp_T_2 = tail(_norm_exp_T_1, 1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 328:63]
    node norm_exp = mux(_norm_exp_T, _norm_exp_T_2, UInt<1>("h0")) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 328:21]
    node _result_T = bits(norm_man, 1, 0) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 330:38]
    node result = cat(norm_exp, _result_T) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 330:19]
    reg result_reg_r : UInt<4>, clock with :
      reset => (UInt<1>("h0"), result_reg_r) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 333:33]
    node _GEN_3 = mux(io_in_en, result, result_reg_r) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 333:33 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 333:33 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 333:33]
    reg result_reg_r_1 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), result_reg_r_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 333:33]
    node _GEN_4 = mux(io_in_en, result_reg_r, result_reg_r_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 333:33 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 333:33 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 333:33]
    reg result_reg_r_2 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), result_reg_r_2) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 333:33]
    node _GEN_5 = mux(io_in_en, result_reg_r_1, result_reg_r_2) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 333:33 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 333:33 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 333:33]
    reg result_reg_r_3 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), result_reg_r_3) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 333:33]
    node _GEN_6 = mux(io_in_en, result_reg_r_2, result_reg_r_3) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 333:33 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 333:33 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 333:33]
    reg result_reg_r_4 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), result_reg_r_4) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 333:33]
    node _GEN_7 = mux(io_in_en, result_reg_r_3, result_reg_r_4) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 333:33 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 333:33 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 333:33]
    reg result_reg_r_5 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), result_reg_r_5) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 333:33]
    node _GEN_8 = mux(io_in_en, result_reg_r_4, result_reg_r_5) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 333:33 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 333:33 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 333:33]
    reg result_reg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), result_reg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 333:33]
    node _GEN_9 = mux(io_in_en, result_reg_r_5, result_reg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 333:33 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 333:33 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 333:33]
    reg valid_reg_r : UInt<1>, clock with :
      reset => (UInt<1>("h0"), valid_reg_r) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 334:32]
    node _GEN_10 = mux(io_in_en, io_in_valid, valid_reg_r) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 334:32 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 334:32 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 334:32]
    reg valid_reg_r_1 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), valid_reg_r_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 334:32]
    node _GEN_11 = mux(io_in_en, valid_reg_r, valid_reg_r_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 334:32 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 334:32 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 334:32]
    reg valid_reg_r_2 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), valid_reg_r_2) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 334:32]
    node _GEN_12 = mux(io_in_en, valid_reg_r_1, valid_reg_r_2) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 334:32 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 334:32 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 334:32]
    reg valid_reg_r_3 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), valid_reg_r_3) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 334:32]
    node _GEN_13 = mux(io_in_en, valid_reg_r_2, valid_reg_r_3) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 334:32 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 334:32 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 334:32]
    reg valid_reg_r_4 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), valid_reg_r_4) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 334:32]
    node _GEN_14 = mux(io_in_en, valid_reg_r_3, valid_reg_r_4) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 334:32 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 334:32 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 334:32]
    reg valid_reg_r_5 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), valid_reg_r_5) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 334:32]
    node _GEN_15 = mux(io_in_en, valid_reg_r_4, valid_reg_r_5) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 334:32 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 334:32 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 334:32]
    reg valid_reg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), valid_reg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 334:32]
    node _GEN_16 = mux(io_in_en, valid_reg_r_5, valid_reg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 334:32 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 334:32 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 334:32]
    io_out <= result_reg @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 336:10]
    io_out_valid <= valid_reg @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 337:16]
    result_reg_r <= _GEN_3
    result_reg_r_1 <= _GEN_4
    result_reg_r_2 <= _GEN_5
    result_reg_r_3 <= _GEN_6
    result_reg_r_4 <= _GEN_7
    result_reg_r_5 <= _GEN_8
    result_reg <= _GEN_9
    valid_reg_r <= _GEN_10
    valid_reg_r_1 <= _GEN_11
    valid_reg_r_2 <= _GEN_12
    valid_reg_r_3 <= _GEN_13
    valid_reg_r_4 <= _GEN_14
    valid_reg_r_5 <= _GEN_15
    valid_reg <= _GEN_16
