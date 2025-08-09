FIRRTL version 1.2.0
circuit FPAdder :
  module FPAdder : @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 234:7]
    input clock : Clock @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 234:7]
    input reset : UInt<1> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 234:7]
    input io_in_a : UInt<6> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 242:14]
    input io_in_b : UInt<6> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 242:14]
    output io_out : UInt<6> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 242:14]
    output io_valid : UInt<1> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 242:14]

    node sign_a = bits(io_in_a, 5, 5) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 250:23]
    node exp_a = bits(io_in_a, 4, 3) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 251:22]
    node man_a = bits(io_in_a, 2, 0) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 252:22]
    node sign_b = bits(io_in_b, 5, 5) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 253:23]
    node exp_b = bits(io_in_b, 4, 3) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 254:22]
    node man_b = bits(io_in_b, 2, 0) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 255:22]
    node norm_man_a = cat(UInt<1>("h1"), man_a) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 257:23]
    node norm_man_b = cat(UInt<1>("h1"), man_b) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 258:23]
    node _T = gt(exp_a, exp_b) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 265:14]
    node _exp_diff_T = sub(exp_a, exp_b) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 266:23]
    node _exp_diff_T_1 = tail(_exp_diff_T, 1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 266:23]
    node _exp_diff_T_2 = sub(exp_b, exp_a) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 271:23]
    node _exp_diff_T_3 = tail(_exp_diff_T_2, 1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 271:23]
    node _GEN_0 = mux(_T, _exp_diff_T_1, _exp_diff_T_3) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 265:23 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 266:14 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 271:14]
    node exp_diff = _GEN_0 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 260:22]
    node _man_b_shifted_T = dshr(norm_man_b, exp_diff) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 268:33]
    node _man_a_shifted_T = dshr(norm_man_a, exp_diff) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 272:33]
    node _GEN_1 = mux(_T, norm_man_a, _man_a_shifted_T) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 265:23 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 267:19 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 272:19]
    node _GEN_2 = mux(_T, _man_b_shifted_T, norm_man_b) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 265:23 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 268:19 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 273:19]
    node _GEN_3 = mux(_T, exp_a, exp_b) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 265:23 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 269:17 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 274:17]
    node man_a_shifted = _GEN_1 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 261:27]
    node man_b_shifted = _GEN_2 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 262:27]
    node sum_mant = add(man_a_shifted, man_b_shifted) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 278:32]
    node carry = bits(sum_mant, 4, 4) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 279:23]
    node _T_1 = eq(carry, UInt<1>("h1")) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 283:14]
    node _sum_norm_T = shr(sum_mant, 1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 284:26]
    node aligned_exp = _GEN_3 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 263:25]
    node _exp_out_T = add(aligned_exp, UInt<1>("h1")) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 285:28]
    node _exp_out_T_1 = tail(_exp_out_T, 1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 285:28]
    node _GEN_4 = mux(_T_1, _sum_norm_T, sum_mant) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 283:23 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 284:14 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 287:14]
    node _GEN_5 = mux(_T_1, _exp_out_T_1, aligned_exp) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 283:23 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 285:13 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 288:13]
    node sum_norm = bits(_GEN_4, 3, 0) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 280:22]
    node final_mant = bits(sum_norm, 2, 0) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 292:28]
    node exp_out = _GEN_5 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 281:21]
    node _final_exp_T = gt(exp_out, UInt<2>("h3")) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 293:31]
    node final_exp = mux(_final_exp_T, UInt<2>("h3"), exp_out) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 293:22]
    node out_hi = cat(UInt<1>("h0"), final_exp) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 295:16]
    node out = cat(out_hi, final_mant) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 295:16]
    reg io_out_r : UInt<6>, clock with :
      reset => (UInt<1>("h0"), io_out_r) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 297:26]
    node _GEN_6 = mux(UInt<1>("h1"), out, io_out_r) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 297:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 297:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 297:26]
    reg io_out_r_1 : UInt<6>, clock with :
      reset => (UInt<1>("h0"), io_out_r_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 297:26]
    node _GEN_7 = mux(UInt<1>("h1"), io_out_r, io_out_r_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 297:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 297:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 297:26]
    reg io_out_r_2 : UInt<6>, clock with :
      reset => (UInt<1>("h0"), io_out_r_2) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 297:26]
    node _GEN_8 = mux(UInt<1>("h1"), io_out_r_1, io_out_r_2) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 297:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 297:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 297:26]
    reg io_out_r_3 : UInt<6>, clock with :
      reset => (UInt<1>("h0"), io_out_r_3) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 297:26]
    node _GEN_9 = mux(UInt<1>("h1"), io_out_r_2, io_out_r_3) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 297:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 297:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 297:26]
    reg io_out_r_4 : UInt<6>, clock with :
      reset => (UInt<1>("h0"), io_out_r_4) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 297:26]
    node _GEN_10 = mux(UInt<1>("h1"), io_out_r_3, io_out_r_4) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 297:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 297:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 297:26]
    reg io_out_r_5 : UInt<6>, clock with :
      reset => (UInt<1>("h0"), io_out_r_5) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 297:26]
    node _GEN_11 = mux(UInt<1>("h1"), io_out_r_4, io_out_r_5) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 297:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 297:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 297:26]
    reg io_out_r_6 : UInt<6>, clock with :
      reset => (UInt<1>("h0"), io_out_r_6) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 297:26]
    node _GEN_12 = mux(UInt<1>("h1"), io_out_r_5, io_out_r_6) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 297:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 297:26 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 297:26]
    reg io_valid_r : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_valid_r) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 298:28]
    node _GEN_13 = mux(UInt<1>("h1"), UInt<1>("h1"), io_valid_r) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 298:28 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 298:28 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 298:28]
    reg io_valid_r_1 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_valid_r_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 298:28]
    node _GEN_14 = mux(UInt<1>("h1"), io_valid_r, io_valid_r_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 298:28 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 298:28 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 298:28]
    reg io_valid_r_2 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_valid_r_2) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 298:28]
    node _GEN_15 = mux(UInt<1>("h1"), io_valid_r_1, io_valid_r_2) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 298:28 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 298:28 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 298:28]
    reg io_valid_r_3 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_valid_r_3) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 298:28]
    node _GEN_16 = mux(UInt<1>("h1"), io_valid_r_2, io_valid_r_3) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 298:28 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 298:28 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 298:28]
    reg io_valid_r_4 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_valid_r_4) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 298:28]
    node _GEN_17 = mux(UInt<1>("h1"), io_valid_r_3, io_valid_r_4) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 298:28 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 298:28 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 298:28]
    reg io_valid_r_5 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_valid_r_5) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 298:28]
    node _GEN_18 = mux(UInt<1>("h1"), io_valid_r_4, io_valid_r_5) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 298:28 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 298:28 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 298:28]
    reg io_valid_r_6 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_valid_r_6) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 298:28]
    node _GEN_19 = mux(UInt<1>("h1"), io_valid_r_5, io_valid_r_6) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 298:28 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 298:28 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 298:28]
    io_out <= io_out_r_6 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 297:10]
    io_valid <= io_valid_r_6 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 298:12]
    io_out_r <= _GEN_6
    io_out_r_1 <= _GEN_7
    io_out_r_2 <= _GEN_8
    io_out_r_3 <= _GEN_9
    io_out_r_4 <= _GEN_10
    io_out_r_5 <= _GEN_11
    io_out_r_6 <= _GEN_12
    io_valid_r <= _GEN_13
    io_valid_r_1 <= _GEN_14
    io_valid_r_2 <= _GEN_15
    io_valid_r_3 <= _GEN_16
    io_valid_r_4 <= _GEN_17
    io_valid_r_5 <= _GEN_18
    io_valid_r_6 <= _GEN_19
