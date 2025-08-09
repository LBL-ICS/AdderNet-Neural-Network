FIRRTL version 1.2.0
circuit FP4Adder :
  module FP4Adder : @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 226:7]
    input clock : Clock @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 226:7]
    input reset : UInt<1> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 226:7]
    input io_in_en : UInt<1> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 230:14]
    input io_in_valid : UInt<1> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 230:14]
    input io_a : UInt<5> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 230:14]
    input io_b : UInt<5> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 230:14]
    output io_out : UInt<5> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 230:14]
    output io_out_valid : UInt<1> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 230:14]

    reg a_reg : UInt<5>, clock with :
      reset => (UInt<1>("h0"), a_reg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 245:24]
    node _GEN_0 = mux(io_in_en, io_a, a_reg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 245:24 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 245:24 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 245:24]
    reg b_reg : UInt<5>, clock with :
      reset => (UInt<1>("h0"), b_reg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 246:24]
    node _GEN_1 = mux(io_in_en, io_b, b_reg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 246:24 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 246:24 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 246:24]
    reg in_valid_reg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), in_valid_reg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 247:31]
    node _GEN_2 = mux(io_in_en, io_in_valid, in_valid_reg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 247:31 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 247:31 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 247:31]
    reg in_en_reg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), in_en_reg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 248:26]
    node exp_a = bits(a_reg, 3, 2) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 251:20]
    node exp_b = bits(b_reg, 3, 2) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 252:20]
    node man_a = bits(a_reg, 1, 0) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 253:20]
    node man_b = bits(b_reg, 1, 0) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 254:20]
    node frac_a = cat(UInt<1>("h1"), man_a) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 257:19]
    node frac_b = cat(UInt<1>("h1"), man_b) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 258:19]
    node _exp_diff_T = geq(exp_a, exp_b) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 261:28]
    node _exp_diff_T_1 = sub(exp_a, exp_b) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 261:44]
    node _exp_diff_T_2 = tail(_exp_diff_T_1, 1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 261:44]
    node _exp_diff_T_3 = sub(exp_b, exp_a) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 261:59]
    node _exp_diff_T_4 = tail(_exp_diff_T_3, 1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 261:59]
    node exp_diff = mux(_exp_diff_T, _exp_diff_T_2, _exp_diff_T_4) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 261:21]
    node a_is_bigger = geq(exp_a, exp_b) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 262:27]
    reg a_is_bigger_reg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), a_is_bigger_reg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 265:34]
    node _GEN_3 = mux(io_in_en, a_is_bigger, a_is_bigger_reg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 265:34 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 265:34 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 265:34]
    reg exp_diff_reg : UInt<2>, clock with :
      reset => (UInt<1>("h0"), exp_diff_reg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 266:31]
    node _GEN_4 = mux(io_in_en, exp_diff, exp_diff_reg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 266:31 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 266:31 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 266:31]
    reg frac_a_reg : UInt<3>, clock with :
      reset => (UInt<1>("h0"), frac_a_reg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 267:29]
    node _GEN_5 = mux(io_in_en, frac_a, frac_a_reg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 267:29 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 267:29 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 267:29]
    reg frac_b_reg : UInt<3>, clock with :
      reset => (UInt<1>("h0"), frac_b_reg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 268:29]
    node _GEN_6 = mux(io_in_en, frac_b, frac_b_reg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 268:29 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 268:29 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 268:29]
    reg exp_a_reg : UInt<2>, clock with :
      reset => (UInt<1>("h0"), exp_a_reg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 269:28]
    node _GEN_7 = mux(io_in_en, exp_a, exp_a_reg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 269:28 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 269:28 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 269:28]
    reg exp_b_reg : UInt<2>, clock with :
      reset => (UInt<1>("h0"), exp_b_reg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 270:28]
    node _GEN_8 = mux(io_in_en, exp_b, exp_b_reg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 270:28 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 270:28 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 270:28]
    node ref_frac = mux(a_is_bigger_reg, frac_a_reg, frac_b_reg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 273:21]
    node shift_frac = mux(a_is_bigger_reg, frac_b_reg, frac_a_reg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 274:23]
    node ref_exp = mux(a_is_bigger_reg, exp_a_reg, exp_b_reg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 275:20]
    node _aligned_shift_T = geq(exp_diff_reg, UInt<2>("h3")) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 278:40]
    node _aligned_shift_T_1 = dshr(shift_frac, exp_diff_reg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 278:72]
    node aligned_shift = mux(_aligned_shift_T, UInt<1>("h0"), _aligned_shift_T_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 278:26]
    node frac_sum = add(ref_frac, aligned_shift) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 281:27]
    reg frac_sum_reg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), frac_sum_reg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 284:31]
    node _GEN_9 = mux(io_in_en, frac_sum, frac_sum_reg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 284:31 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 284:31 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 284:31]
    reg ref_exp_reg : UInt<2>, clock with :
      reset => (UInt<1>("h0"), ref_exp_reg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 285:30]
    node _GEN_10 = mux(io_in_en, ref_exp, ref_exp_reg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 285:30 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 285:30 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 285:30]
    node carry = bits(frac_sum_reg, 3, 3) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 288:27]
    node _norm_frac_T = shr(frac_sum_reg, 1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 289:43]
    node norm_frac = mux(carry, _norm_frac_T, frac_sum_reg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 289:22]
    node _norm_exp_T = add(ref_exp_reg, UInt<1>("h1")) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 290:41]
    node _norm_exp_T_1 = tail(_norm_exp_T, 1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 290:41]
    node norm_exp = mux(carry, _norm_exp_T_1, ref_exp_reg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 290:21]
    node overflow = gt(norm_exp, UInt<2>("h3")) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 293:27]
    node saturated_out = cat(UInt<2>("h3"), UInt<2>("h3")) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 298:26]
    node out_man = bits(norm_frac, 1, 0) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 301:26]
    node is_zero = eq(norm_frac, UInt<1>("h0")) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 304:27]
    node _final_out_T = cat(norm_exp, out_man) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 309:10]
    node _final_out_T_1 = mux(overflow, saturated_out, _final_out_T) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 308:8]
    node final_out = mux(is_zero, UInt<1>("h0"), _final_out_T_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 307:22]
    node _io_out_valid_T = and(in_valid_reg, in_en_reg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 314:46]
    reg io_out_valid_r : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_valid_r) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 314:32]
    node _GEN_11 = mux(UInt<1>("h1"), _io_out_valid_T, io_out_valid_r) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 314:32 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 314:32 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 314:32]
    reg io_out_valid_r_1 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_valid_r_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 314:32]
    node _GEN_12 = mux(UInt<1>("h1"), io_out_valid_r, io_out_valid_r_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 314:32 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 314:32 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 314:32]
    reg io_out_valid_r_2 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_valid_r_2) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 314:32]
    node _GEN_13 = mux(UInt<1>("h1"), io_out_valid_r_1, io_out_valid_r_2) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 314:32 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 314:32 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 314:32]
    io_out <= pad(final_out, 5) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 313:10]
    io_out_valid <= io_out_valid_r_2 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 314:16]
    a_reg <= _GEN_0
    b_reg <= _GEN_1
    in_valid_reg <= _GEN_2
    in_en_reg <= mux(reset, UInt<1>("h0"), io_in_en) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 248:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 248:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 248:26]
    a_is_bigger_reg <= _GEN_3
    exp_diff_reg <= _GEN_4
    frac_a_reg <= _GEN_5
    frac_b_reg <= _GEN_6
    exp_a_reg <= _GEN_7
    exp_b_reg <= _GEN_8
    frac_sum_reg <= _GEN_9
    ref_exp_reg <= _GEN_10
    io_out_valid_r <= _GEN_11
    io_out_valid_r_1 <= _GEN_12
    io_out_valid_r_2 <= _GEN_13
