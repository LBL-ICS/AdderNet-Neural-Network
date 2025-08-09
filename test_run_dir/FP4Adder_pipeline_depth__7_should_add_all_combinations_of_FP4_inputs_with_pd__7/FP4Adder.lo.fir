FIRRTL version 1.2.0
circuit FP4Adder :
  module FP4Adder : @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 201:7]
    input clock : Clock @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 201:7]
    input reset : UInt<1> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 201:7]
    input io_in_en : UInt<1> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 205:14]
    input io_in_valid : UInt<1> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 205:14]
    input io_a : UInt<4> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 205:14]
    input io_b : UInt<4> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 205:14]
    output io_out : UInt<4> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 205:14]
    output io_out_valid : UInt<1> @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 205:14]

    node a_exp = bits(io_a, 3, 2) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 218:19]
    node b_exp = bits(io_b, 3, 2) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 219:19]
    node a_is_zero = eq(io_a, UInt<1>("h0")) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 220:24]
    node b_is_zero = eq(io_b, UInt<1>("h0")) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 221:24]
    node _a_man_T = bits(io_a, 1, 0) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 223:53]
    node _a_man_T_1 = cat(UInt<1>("h1"), _a_man_T) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 223:38]
    node a_man = mux(a_is_zero, UInt<1>("h0"), _a_man_T_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 223:18]
    node _b_man_T = bits(io_b, 1, 0) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 224:53]
    node _b_man_T_1 = cat(UInt<1>("h1"), _b_man_T) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 224:38]
    node b_man = mux(b_is_zero, UInt<1>("h0"), _b_man_T_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 224:18]
    node _exp_diff_T = sub(a_exp, b_exp) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 226:25]
    node _exp_diff_T_1 = tail(_exp_diff_T, 1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 226:25]
    node exp_diff = asSInt(_exp_diff_T_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 226:34]
    node _T = gt(exp_diff, asSInt(UInt<1>("h0"))) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 232:17]
    node _aligned_b_man_T = asUInt(exp_diff) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 234:40]
    node _aligned_b_man_T_1 = dshr(b_man, _aligned_b_man_T) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 234:28]
    node _aligned_a_man_T = sub(asSInt(UInt<1>("h0")), exp_diff) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 237:32]
    node _aligned_a_man_T_1 = tail(_aligned_a_man_T, 1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 237:32]
    node _aligned_a_man_T_2 = asSInt(_aligned_a_man_T_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 237:32]
    node _aligned_a_man_T_3 = asUInt(_aligned_a_man_T_2) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 237:43]
    node _aligned_a_man_T_4 = dshr(a_man, _aligned_a_man_T_3) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 237:28]
    node _GEN_0 = mux(_T, a_man, _aligned_a_man_T_4) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 232:24 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 233:19 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 237:19]
    node _GEN_1 = mux(_T, _aligned_b_man_T_1, b_man) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 232:24 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 234:19 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 238:19]
    node _GEN_2 = mux(_T, a_exp, b_exp) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 232:24 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 235:19 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 239:19]
    node aligned_a_man = _GEN_0 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 228:27]
    node aligned_b_man = _GEN_1 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 229:27]
    node _sum_man_T = add(aligned_a_man, aligned_b_man) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 242:31]
    node sum_man = tail(_sum_man_T, 1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 242:31]
    node carry_out = bits(sum_man, 2, 2) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 243:26]
    node _T_1 = eq(carry_out, UInt<1>("h1")) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 248:18]
    node _norm_sum_man_T = bits(sum_man, 2, 1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 249:28]
    node aligned_exp = _GEN_2 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 230:27]
    node _norm_exp_T = eq(aligned_exp, UInt<2>("h3")) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 250:33]
    node _norm_exp_T_1 = add(aligned_exp, UInt<1>("h1")) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 250:65]
    node _norm_exp_T_2 = tail(_norm_exp_T_1, 1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 250:65]
    node _norm_exp_T_3 = mux(_norm_exp_T, UInt<2>("h3"), _norm_exp_T_2) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 250:20]
    node _norm_sum_man_T_1 = bits(sum_man, 1, 0) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 252:28]
    node _GEN_3 = mux(_T_1, _norm_sum_man_T, _norm_sum_man_T_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 248:27 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 249:18 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 252:18]
    node _GEN_4 = mux(_T_1, _norm_exp_T_3, aligned_exp) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 248:27 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 250:14 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 253:14]
    node _result_T = and(a_is_zero, b_is_zero) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 256:31]
    node norm_exp = _GEN_4 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 246:26]
    node norm_sum_man = _GEN_3 @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 245:26]
    node _result_T_1 = cat(norm_exp, norm_sum_man) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 256:54]
    node result = mux(_result_T, UInt<1>("h0"), _result_T_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 256:19]
    reg out_valid_reg_r : UInt<1>, clock with :
      reset => (UInt<1>("h0"), out_valid_reg_r) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 258:36]
    node _GEN_5 = mux(io_in_en, io_in_valid, out_valid_reg_r) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 258:36 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 258:36 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 258:36]
    reg out_valid_reg_r_1 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), out_valid_reg_r_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 258:36]
    node _GEN_6 = mux(io_in_en, out_valid_reg_r, out_valid_reg_r_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 258:36 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 258:36 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 258:36]
    reg out_valid_reg_r_2 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), out_valid_reg_r_2) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 258:36]
    node _GEN_7 = mux(io_in_en, out_valid_reg_r_1, out_valid_reg_r_2) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 258:36 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 258:36 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 258:36]
    reg out_valid_reg_r_3 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), out_valid_reg_r_3) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 258:36]
    node _GEN_8 = mux(io_in_en, out_valid_reg_r_2, out_valid_reg_r_3) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 258:36 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 258:36 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 258:36]
    reg out_valid_reg_r_4 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), out_valid_reg_r_4) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 258:36]
    node _GEN_9 = mux(io_in_en, out_valid_reg_r_3, out_valid_reg_r_4) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 258:36 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 258:36 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 258:36]
    reg out_valid_reg_r_5 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), out_valid_reg_r_5) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 258:36]
    node _GEN_10 = mux(io_in_en, out_valid_reg_r_4, out_valid_reg_r_5) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 258:36 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 258:36 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 258:36]
    reg out_valid_reg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), out_valid_reg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 258:36]
    node _GEN_11 = mux(io_in_en, out_valid_reg_r_5, out_valid_reg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 258:36 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 258:36 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 258:36]
    reg result_reg_r : UInt<4>, clock with :
      reset => (UInt<1>("h0"), result_reg_r) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 259:36]
    node _GEN_12 = mux(io_in_en, result, result_reg_r) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 259:36 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 259:36 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 259:36]
    reg result_reg_r_1 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), result_reg_r_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 259:36]
    node _GEN_13 = mux(io_in_en, result_reg_r, result_reg_r_1) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 259:36 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 259:36 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 259:36]
    reg result_reg_r_2 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), result_reg_r_2) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 259:36]
    node _GEN_14 = mux(io_in_en, result_reg_r_1, result_reg_r_2) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 259:36 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 259:36 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 259:36]
    reg result_reg_r_3 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), result_reg_r_3) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 259:36]
    node _GEN_15 = mux(io_in_en, result_reg_r_2, result_reg_r_3) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 259:36 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 259:36 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 259:36]
    reg result_reg_r_4 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), result_reg_r_4) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 259:36]
    node _GEN_16 = mux(io_in_en, result_reg_r_3, result_reg_r_4) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 259:36 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 259:36 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 259:36]
    reg result_reg_r_5 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), result_reg_r_5) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 259:36]
    node _GEN_17 = mux(io_in_en, result_reg_r_4, result_reg_r_5) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 259:36 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 259:36 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 259:36]
    reg result_reg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), result_reg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 259:36]
    node _GEN_18 = mux(io_in_en, result_reg_r_5, result_reg) @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 259:36 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 259:36 \\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 259:36]
    io_out <= result_reg @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 261:10]
    io_out_valid <= out_valid_reg @[\\REU Project Working\\Chisel-for-Fundamental-IC\\src\\main\\scala\\Fundamental_IC.scala 262:16]
    out_valid_reg_r <= _GEN_5
    out_valid_reg_r_1 <= _GEN_6
    out_valid_reg_r_2 <= _GEN_7
    out_valid_reg_r_3 <= _GEN_8
    out_valid_reg_r_4 <= _GEN_9
    out_valid_reg_r_5 <= _GEN_10
    out_valid_reg <= _GEN_11
    result_reg_r <= _GEN_12
    result_reg_r_1 <= _GEN_13
    result_reg_r_2 <= _GEN_14
    result_reg_r_3 <= _GEN_15
    result_reg_r_4 <= _GEN_16
    result_reg_r_5 <= _GEN_17
    result_reg <= _GEN_18
