FIRRTL version 1.2.0
circuit FP4Adder :
  module FP4Adder : @[\\src\\main\\scala\\Fundamental_IC.scala 201:7]
    input clock : Clock @[\\src\\main\\scala\\Fundamental_IC.scala 201:7]
    input reset : UInt<1> @[\\src\\main\\scala\\Fundamental_IC.scala 201:7]
    input io_in_en : UInt<1> @[\\src\\main\\scala\\Fundamental_IC.scala 205:14]
    input io_in_valid : UInt<1> @[\\src\\main\\scala\\Fundamental_IC.scala 205:14]
    input io_a : UInt<4> @[\\src\\main\\scala\\Fundamental_IC.scala 205:14]
    input io_b : UInt<4> @[\\src\\main\\scala\\Fundamental_IC.scala 205:14]
    output io_out : UInt<4> @[\\src\\main\\scala\\Fundamental_IC.scala 205:14]
    output io_out_valid : UInt<1> @[\\src\\main\\scala\\Fundamental_IC.scala 205:14]

    node a_exp = bits(io_a, 3, 2) @[\\src\\main\\scala\\Fundamental_IC.scala 218:19]
    node b_exp = bits(io_b, 3, 2) @[\\src\\main\\scala\\Fundamental_IC.scala 219:19]
    node _a_man_T = bits(io_a, 1, 0) @[\\src\\main\\scala\\Fundamental_IC.scala 220:33]
    node a_man = cat(UInt<1>("h1"), _a_man_T) @[\\src\\main\\scala\\Fundamental_IC.scala 220:18]
    node _b_man_T = bits(io_b, 1, 0) @[\\src\\main\\scala\\Fundamental_IC.scala 221:33]
    node b_man = cat(UInt<1>("h1"), _b_man_T) @[\\src\\main\\scala\\Fundamental_IC.scala 221:18]
    node _exp_diff_T = sub(a_exp, b_exp) @[\\src\\main\\scala\\Fundamental_IC.scala 223:25]
    node _exp_diff_T_1 = tail(_exp_diff_T, 1) @[\\src\\main\\scala\\Fundamental_IC.scala 223:25]
    node exp_diff = asSInt(_exp_diff_T_1) @[\\src\\main\\scala\\Fundamental_IC.scala 223:34]
    node _T = gt(exp_diff, asSInt(UInt<1>("h0"))) @[\\src\\main\\scala\\Fundamental_IC.scala 229:17]
    node _aligned_b_man_T = asUInt(exp_diff) @[\\src\\main\\scala\\Fundamental_IC.scala 231:41]
    node _aligned_b_man_T_1 = dshr(b_man, _aligned_b_man_T) @[\\src\\main\\scala\\Fundamental_IC.scala 231:29]
    node _aligned_a_man_T = sub(asSInt(UInt<1>("h0")), exp_diff) @[\\src\\main\\scala\\Fundamental_IC.scala 234:33]
    node _aligned_a_man_T_1 = tail(_aligned_a_man_T, 1) @[\\src\\main\\scala\\Fundamental_IC.scala 234:33]
    node _aligned_a_man_T_2 = asSInt(_aligned_a_man_T_1) @[\\src\\main\\scala\\Fundamental_IC.scala 234:33]
    node _aligned_a_man_T_3 = asUInt(_aligned_a_man_T_2) @[\\src\\main\\scala\\Fundamental_IC.scala 234:44]
    node _aligned_a_man_T_4 = dshr(a_man, _aligned_a_man_T_3) @[\\src\\main\\scala\\Fundamental_IC.scala 234:29]
    node _GEN_0 = mux(_T, a_man, _aligned_a_man_T_4) @[\\src\\main\\scala\\Fundamental_IC.scala 229:24 230:19 234:19]
    node _GEN_1 = mux(_T, _aligned_b_man_T_1, b_man) @[\\src\\main\\scala\\Fundamental_IC.scala 229:24 231:19 235:19]
    node _GEN_2 = mux(_T, a_exp, b_exp) @[\\src\\main\\scala\\Fundamental_IC.scala 229:24 232:19 236:19]
    node aligned_a_man = _GEN_0 @[\\src\\main\\scala\\Fundamental_IC.scala 225:27]
    node aligned_b_man = _GEN_1 @[\\src\\main\\scala\\Fundamental_IC.scala 226:27]
    node _sum_man_T = add(aligned_a_man, aligned_b_man) @[\\src\\main\\scala\\Fundamental_IC.scala 239:31]
    node sum_man = tail(_sum_man_T, 1) @[\\src\\main\\scala\\Fundamental_IC.scala 239:31]
    node carry_out = bits(sum_man, 2, 2) @[\\src\\main\\scala\\Fundamental_IC.scala 240:26]
    node _T_1 = eq(carry_out, UInt<1>("h1")) @[\\src\\main\\scala\\Fundamental_IC.scala 245:18]
    node _norm_sum_man_T = bits(sum_man, 2, 1) @[\\src\\main\\scala\\Fundamental_IC.scala 246:28]
    node aligned_exp = _GEN_2 @[\\src\\main\\scala\\Fundamental_IC.scala 227:27]
    node _norm_exp_T = add(aligned_exp, UInt<1>("h1")) @[\\src\\main\\scala\\Fundamental_IC.scala 247:29]
    node _norm_exp_T_1 = tail(_norm_exp_T, 1) @[\\src\\main\\scala\\Fundamental_IC.scala 247:29]
    node _norm_sum_man_T_1 = bits(sum_man, 1, 0) @[\\src\\main\\scala\\Fundamental_IC.scala 249:28]
    node _GEN_3 = mux(_T_1, _norm_sum_man_T, _norm_sum_man_T_1) @[\\src\\main\\scala\\Fundamental_IC.scala 245:27 246:18 249:18]
    node _GEN_4 = mux(_T_1, _norm_exp_T_1, aligned_exp) @[\\src\\main\\scala\\Fundamental_IC.scala 245:27 247:14 250:14]
    node norm_exp = _GEN_4 @[\\src\\main\\scala\\Fundamental_IC.scala 243:26]
    node norm_sum_man = _GEN_3 @[\\src\\main\\scala\\Fundamental_IC.scala 242:26]
    node result = cat(norm_exp, norm_sum_man) @[\\src\\main\\scala\\Fundamental_IC.scala 253:19]
    reg out_valid_reg_r : UInt<1>, clock with :
      reset => (UInt<1>("h0"), out_valid_reg_r) @[\\src\\main\\scala\\Fundamental_IC.scala 256:36]
    node _GEN_5 = mux(io_in_en, io_in_valid, out_valid_reg_r) @[\\src\\main\\scala\\Fundamental_IC.scala 256:{36,36,36}]
    reg out_valid_reg_r_1 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), out_valid_reg_r_1) @[\\src\\main\\scala\\Fundamental_IC.scala 256:36]
    node _GEN_6 = mux(io_in_en, out_valid_reg_r, out_valid_reg_r_1) @[\\src\\main\\scala\\Fundamental_IC.scala 256:{36,36,36}]
    reg out_valid_reg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), out_valid_reg) @[\\src\\main\\scala\\Fundamental_IC.scala 256:36]
    node _GEN_7 = mux(io_in_en, out_valid_reg_r_1, out_valid_reg) @[\\src\\main\\scala\\Fundamental_IC.scala 256:{36,36,36}]
    reg result_reg_r : UInt<4>, clock with :
      reset => (UInt<1>("h0"), result_reg_r) @[\\src\\main\\scala\\Fundamental_IC.scala 257:33]
    node _GEN_8 = mux(io_in_en, result, result_reg_r) @[\\src\\main\\scala\\Fundamental_IC.scala 257:{33,33,33}]
    reg result_reg_r_1 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), result_reg_r_1) @[\\src\\main\\scala\\Fundamental_IC.scala 257:33]
    node _GEN_9 = mux(io_in_en, result_reg_r, result_reg_r_1) @[\\src\\main\\scala\\Fundamental_IC.scala 257:{33,33,33}]
    reg result_reg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), result_reg) @[\\src\\main\\scala\\Fundamental_IC.scala 257:33]
    node _GEN_10 = mux(io_in_en, result_reg_r_1, result_reg) @[\\src\\main\\scala\\Fundamental_IC.scala 257:{33,33,33}]
    io_out <= result_reg @[\\src\\main\\scala\\Fundamental_IC.scala 259:10]
    io_out_valid <= out_valid_reg @[\\src\\main\\scala\\Fundamental_IC.scala 260:16]
    out_valid_reg_r <= _GEN_5
    out_valid_reg_r_1 <= _GEN_6
    out_valid_reg <= _GEN_7
    result_reg_r <= _GEN_8
    result_reg_r_1 <= _GEN_9
    result_reg <= _GEN_10
