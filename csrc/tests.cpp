#include <string>
#include <string.h>
#include "emulator.h"

template <int w> dat_t<w> LITS(const char* str) { 
  dat_t<w> dat;
  assert(dat_from_hex<w>(str, dat));
  return dat;
}

template <int w>
void test (const char* name, dat_t<w> tst, dat_t<w> val) {
  if (tst == val) {
    printf("%9s passed 0x%s\n", name, dat_to_str(val).c_str());
  } else {
    printf("%9s failed 0x%s != 0x%s\n", name, dat_to_str(tst).c_str(), dat_to_str(val).c_str());
  }
}

void test (const char* name, bool tst, dat_t<1> val) {
  return test(name, dat_t<1>(tst), val);
}

int main (int argc, char* argv[]) {
  // test("stdb-1-0",    str_to_dat<4>("0b1010"), LIT<4>(0xa));
  // test("stdx-1-0",    str_to_dat<32>("0x12345678"), LIT<32>(0x12345678));
  // test("stdx-1-0",    str_to_dat<32>("0x12345678"), LIT<32>(0x12345678));
  // test("stdx-1-1",    str_to_dat<64>("0x0123456789abcdef"), LIT<64>(0x0123456789abcdef));
  // test("std-2-0",     str_to_dat<80>("0x01230123456789abcdef"), LIT<80>(0x0123, 0x0123456789abcdef));

  printf("N BITS %d\n", CeilLog<sizeof(val_t)*8>::v);

  test("ltu-1-0", LIT<2>(1) < LIT<2>(0), LIT<1>(0));
  test("ltu-1-1", LIT<2>(0) < LIT<2>(1), LIT<1>(1));
  test("ltu-1-2", LIT<2>(0) < LIT<2>(0), LIT<1>(0));
  test("lteu-1-0", LIT<2>(1) <= LIT<2>(0), LIT<1>(0));
  test("lteu-1-1", LIT<2>(0) <= LIT<2>(1), LIT<1>(1));
  test("lteu-1-2", LIT<2>(0) <= LIT<2>(0), LIT<1>(1));
  
  test("eq-1-0",  LIT<2>(0) == LIT<2>(0), LIT<1>(1));
  test("eq-1-1",  LIT<2>(0) == LIT<2>(1), LIT<1>(0));
  test("eqz-1-1", LITZ<32>(0x00000013, 0x000003ff) == LIT<32>(0x60000013), LIT<1>(1));  
  test("eqz-1-0", LIT<32>(0x60000013) == LITZ<32>(0x00000013, 0x000003ff), LIT<1>(1));  
  test("lsh-1-0", LIT<2>(1) << LIT<1>(1), LIT<2>(2));
  test("lsh-1-1", LIT<3>(1) << LIT<2>(2), LIT<3>(4));
  test("lsh-1-2", LIT<4>(3) << LIT<2>(2), LIT<4>(0xc));
  test("lsh-1-3", LITS<34>("0xffffffff") << LIT<2>(2), LITS<34>("0x3fffffffc"));
  test("lsh-2-0", LITS<66>("0xffffffffffffffff") << LIT<2>(2), LITS<66>("0x3fffffffffffffffc"));
  test("lsh-2-1", LITS<66>("0x1") << LIT<16>(64), LITS<66>("0x10000000000000000"));
  test("rsh-1-0", LIT<2>(1) >> LIT<1>(1), LIT<2>(0));
  test("rsh-1-1", LIT<3>(6) >> LIT<2>(2), LIT<3>(1));
  test("rsh-1-2", LIT<4>(0xa) >> LIT<2>(2), LIT<4>(0x2));
  test("rsh-1-3", LITS<32>("0xffffffff") >> LIT<2>(2), LITS<32>("0x3fffffff"));
  test("rsh-2-0", LITS<68>("0xfffffffffffffffff") >> LIT<2>(1), LITS<68>("0x7ffffffffffffffff"));
  test("rsh-3-0", LITS<130>("0x3ffffffffffffffffffffffffffffffff") >> LIT<5>(0x1a), LITS<130>("0xffffffffffffffffffffffffff"));
  test("rsh-3-1", LITS<130>("0x3ffffffffffffffffffffffffffffffff") >> LIT<7>(67), LITS<130>("0x7fffffffffffffff"));
  test("rsh-4-0", LITS<194>("0x3ffffffffffffffffffffffffffffffffffffffffffffffff") >> LIT<5>(0x1a), LITS<194>("0xffffffffffffffffffffffffffffffffffffffffff"));
  // 0xffffffffffffffff, 0x7ffbffffffffffff, 0xfffffffeffffffff, 0xffffffffffffffff, 0xffffffffffffffff, 0x3
  test("rsh-6-0", LITS<322>("0x3fffffffffffffffffffffffffffffffffffffffeffffffff7ffbffffffffffffffffffffffffffff") >> LIT<1>(0), LITS<322>("0x3fffffffffffffffffffffffffffffffffffffffeffffffff7ffbffffffffffffffffffffffffffff"));
  test("msk-1-0", LIT<1>(1).fill<31,31>(), LIT<31>(0x7fffffff));
  test("msk-1-1", LIT<1>(1).fill<64,64>(), LITS<64>("0xffffffffffffffff"));
  test("msk-2-0", LIT<1>(1).fill<65,65>(), LITS<65>("0x1ffffffffffffffff"));
  test("cat-1-0", cat<2>(LIT<1>(1), LIT<1>(0)), LIT<2>(2)); 
  test("cat-1-1", cat<3>(LIT<1>(1), LIT<2>(0)), LIT<3>(4));
  test("cat-1-2", cat<4>(cat<3>(LIT<1>(1), LIT<2>(2)), LIT<1>(1)), LIT<4>(0xd));
  test("cat-2-0", cat<66>(LITS<64>("0xffffffffffffffff"), LIT<2>(0x3)), LITS<66>("0x3ffffffffffffffff"));
  test("cat-2-1", cat<108>(LIT<1>(1), LITS<107>("0x7ffffffffffffffffffffffffff")), LITS<108>("0xfffffffffffffffffffffffffff"));
  test("cat-2-2", DAT<108>(LIT<1>(1)) << 107, LITS<108>("0x800000000000000000000000000"));
  test("cat-2-3", cat<65>(cat<13>(LIT<1>(1), LIT<12>(0)), cat<52>(cat<23>(LIT<1>(1), LIT<22>(0x2fff80)), LIT<29>(0))), LITS<65>("0x1000dfff000000000"));
  test("not-1-0", ~LIT<16>(0x0), LIT<16>(0xffff));
  test("not-2-0", ~LIT<65>(0x0), LITS<65>("0x1ffffffffffffffff"));
  test("or-1-0",  LIT<2>(2) | LIT<2>(1), LIT<2>(3));
  test("or-1-1",  LIT<3>(1) | LIT<3>(4), LIT<3>(5));
  test("or-1-2",  LIT<4>(0xc) | LIT<4>(1), LIT<4>(0xd));
  test("or-2-0",  LITS<66>("0x2aaaaaaaaaaaaaaaa") | LITS<66>("0x15555555555555555"), LITS<66>("0x3ffffffffffffffff"));
  test("ext-1-0", LIT<8>(0xAA).extract<6>(LIT<3>(5), LIT<1>(0)), LIT<6>(0x2a));
  test("ext-1-1", LIT<8>(0xAA).extract<4>(LIT<3>(7), LIT<2>(4)), LIT<4>(0xa));
  test("ext-1-2", LIT<64>(0x4030002000000000L).extract<52>(LIT<6>(51),LIT<1>(0)), LIT<52>(0x0002000000000));
  test("ext-2-0", LITS<68>("0x32480002000000000").extract<8>(LIT<7>(68),LIT<7>(60)), LIT<8>(0x32));
  test("inj-1-0", LITS<8>("0x00").inject<8>(LITS<4>("0xf"),LIT<3>(7),LIT<3>(4)), LITS<8>("0xf0"));
  test("inj-1-1", LITS<8>("0x00").inject<8>(LITS<4>("0xf"),LIT<3>(6),LIT<3>(3)), LITS<8>("0x78"));
  test("inj-1-2", LITS<16>("0x7fff").inject<16>(LITS<4>("0x0"),LIT<4>(13),LIT<4>(10)), LITS<16>("0x43ff"));
  test("inj-1-3", LITS<16>("0x7fff").inject<16>(LITS<4>("0x0"),LIT<4>(14),LIT<4>(11)), LITS<16>("0x07ff"));
  test("add-1-0", LITS<32>("0xfffffff8") + LIT<32>(0x14), LIT<32>(0xc));
  test("add-1-1", LITS<33>("0x80000000") + LITS<33>("0x80000000"), LITS<33>("0x100000000"));
  test("add-2-1", LITS<65>("0x8000000000000000") + LITS<65>("0x8000000000000000"), LITS<65>("0x10000000000000000"));
  test("add-2-2", (LIT<68>(1) << LIT<7>(64)) + (LIT<68>(1) << LIT<7>(39)), LITS<68>("0x10000008000000000"));
  test("mul-1-0", LITS<64>("0x1f800000020000") * LITS<64>("0x19b6d3007ba345"), LITS<128>("0x329fef68f6a04a380f7468a0000"));
  test("mul-1-1", LITS<64>("0xffffffff80000000") * LITS<64>("0xffffffffffff8000"), LITS<128>("0xffffffff7fff80000000400000000000"));
  test("div-1-0", LITS<128>("0x329fef68f6a04a380f7468a0000") / LITS<64>("0x1f800000020000"), LITS<128>("0x19b6d3007ba345"));
  test("div-1-1", LITS<128>("0x329fef68f6a04a380f7468a0000") / LITS<64>("0x19b6d3007ba345"), LITS<128>("0x1f800000020000"));
  test("neg-1-0", -LITS<16>("0x0001"), LITS<16>("0xffff"));
  test("neg-2-0", -LITS<32>("0x3fffc002"), LITS<32>("0xc0003ffe"));
  test("neg-2-1", - (LIT<68>(1) << LIT<7>(39)), LITS<68>("0xfffffff8000000000"));
  test("neg-3-0", -LITS<64>("0x3fffc002"), LITS<64>("0xffffffffc0003ffe"));
  test("sub-1-0", LITS<33>("0x10000000") - LITS<33>("0x10000000"), LITS<33>("0x000000000"));
  test("sub-1-1", LITS<34>("0x100000000") - LITS<34>("0x00000001"), LITS<34>("0x0FFFFFFFF"));
  test("sub-2-2", (LIT<68>(1) << LIT<7>(64)) - (LIT<68>(1) << LIT<7>(39)), LITS<68>("0x0ffffff8000000000"));
  test("lt-1-0",  LITS<32>("0x0000001") < LITS<32>("0x00000002"), LITS<1>("0x1"));
  test("lt-1-1",  LITS<32>("0x0000002") < LITS<32>("0x00000001"), LITS<1>("0x0"));
  test("lg2-1-0", LITS<32>("0x0000000").log2<5>(), LITS<5>("0x0"));
  test("lg2-1-1", LITS<32>("0x0000001").log2<5>(), LITS<5>("0x0"));
  test("lg2-1-2", LITS<32>("0x0000002").log2<5>(), LITS<5>("0x1"));
  test("lg2-1-3", LITS<32>("0x0000060").log2<5>(), LIT<5>(6));
  test("lg2-2-0", LITS<66>("0x20000000000000000").log2<8>(), LIT<8>(65));
  test("lg2-2-1", LITS<65>("0x10000000000000000").log2<8>(), LIT<8>(64));
  
  test("lt-1-1",  LIT< 5>(0x1a).lt (LIT< 5>(1)), LIT<1>(1)); //-6
  test("lt-1-0",  LITS< 32>("0xffffffff").lt (LIT< 31>(1)), LIT<1>(1));
  
  test("lt-1-1",  LITS< 32>("0xffffffff").lt (LIT< 32>(1)), LIT<1>(1));
  test("lt-1-1",  LIT< 32>( 1).lt (LIT< 32>(0xffffffff)), LIT<1>(0));
  test("lt-2-0",  LITS< 80>("0xffffffffffffffffffff").lt (LIT< 80>(1)), LIT<1>(1));
  test("lt-2-1",  LIT< 80>( 1).lt (LITS< 80>("0xffffffffffffffffffff")), LIT<1>(0));
  test("lt-3-0",  LITS<140>("0xfffffffffffffffffffffffffffffffffff").lt (LIT<140>(1)), LIT<1>(1));
  test("lt-3-1",  LIT<140>( 1).lt (LITS<140>("0xfffffffffffffffffffffffffffffffffff")), LIT<1>(0));
  test("lt-n-0",  LITS<256>("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff").lt (LIT<256>(1)), LIT<1>(1));
  test("lt-n-1",  LIT<256>( 1).lt (LITS<256>("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")), LIT<1>(0));
  
  test("lte-1-0", LIT< 32>(0xffffffff).lte(LIT< 32>(1)), LIT<1>(1));
  test("lte-1-1", LIT< 32>( 1).lte(LIT< 32>(0xffffffff)), LIT<1>(0));
  test("lte-2-0", LITS< 80>("0xffffffffffffffffffff").lte(LIT< 80>(1)), LIT<1>(1));
  test("lte-2-1", LIT< 80>( 1).lte(LITS< 80>("0xffffffffffffffffffff")), LIT<1>(0));
  test("lte-3-0", LITS<140>("0xfffffffffffffffffffffffffffffffffff").lte(LIT<140>(1)), LIT<1>(1));
  test("lte-3-1", LIT<140>( 1).lte(LITS<140>("0xfffffffffffffffffffffffffffffffffff")), LIT<1>(0));
  test("lte-n-0", LITS<256>("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff").lte(LIT<256>(1)), LIT<1>(1));
  test("lte-n-1", LIT<256>( 1).lte(LITS<256>("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")), LIT<1>(0));
  
  test("rsha-1-0", LIT<2>(1).rsha(LIT<1>(1)), LIT<2>(0));
  test("rsha-1-1", LIT<3>(6).rsha(LIT<2>(2)), LIT<3>(7));
  test("rsha-1-2", LIT<4>(6).rsha(LIT<2>(2)), LIT<4>(1));
  test("rsha-1-2", LIT<5>(0xa).rsha(LIT<2>(2)), LIT<5>(0x2));
  test("rsha-1-2", LIT<4>(0xa).rsha(LIT<2>(2)), LIT<4>(0xe));
  test("rsha-1-3", LITS<32>("0xffffffff").rsha(LIT<2>(2)), LITS<32>("0xffffffff"));
  test("rsha-1-4", LITS<32>("0xffffffff").rsha(LIT<6>(31)), LITS<32>("0xffffffff"));
  test("rsha-1-5", LITS<31>("0x7fffffff").rsha(LIT<2>(2)), LITS<31>("0x7fffffff"));
  test("rsha-1-6", LIT<32>(0xFFFFFFFF).rsha(LIT<2>(2)), LITS<32>("0xffffffff"));
  test("rsha-1-7", LIT<64>(0xffffffff80000000).rsha(LIT<6>(0)), LIT<64>(0xffffffff80000000));
  
  test("rsha-2-0", LITS<68>("0xfffffffffffffffff").rsha(LIT<2>(1)), LITS<68>("0xfffffffffffffffff"));
  test("rsha-2-1", LITS<68>("0xf8100ffeeddccbbaa").rsha(LIT<2>(4)), LITS<68>("0xff8100ffeeddccbba"));
  test("rsha-2-2", LITS<68>("0xf7100ffeeddccbbaa").rsha(LIT<2>(4)), LITS<68>("0xff7100ffeeddccbba"));
  test("rsha-2-3", LITS<68>("0xfffffffffffffffff").rsha(LIT<7>(66)), LITS<68>("0xfffffffffffffffff"));
  test("rsha-2-4", LITS<68>("0x77100ffeeddccbbaa").rsha(LIT<7>(66)), LIT<68>(0x1));
  test("rsha-2-5", LITS<128>("0xffffffff80000000ffffffff80000000").rsha(LIT<7>(0)), LITS<128>("0xffffffff80000000ffffffff80000000"));
  test("rsha-2-6", LITS<128>("0xffffffff80000000ffffffff80000000").rsha(LIT<7>(1)), LITS<128>("0xffffffffc00000007fffffffc0000000"));
  test("rsha-2-7", LITS<128>("0xffffffff80000001ffffffff80000000").rsha(LIT<7>(1)), LITS<128>("0xffffffffc0000000ffffffffc0000000"));
  test("rsha-3-0", LITS<130>("0x3ffffffffffffffffffffffffffffffff").rsha(LIT<5>(0x1a)), LITS<130>("0x3ffffffffffffffffffffffffffffffff"));
  test("rsha-3-1", LITS<132>("0xAffffffffffffffffffffffffffffffff").rsha(LIT<5>(4)), LITS<132>("0xfAfffffffffffffffffffffffffffffff"));
  test("rsha-3-2", LITS<132>("0xAffffffffffffffffffffffffffffffff").rsha(LIT<5>(4)), LITS<132>("0xfAfffffffffffffffffffffffffffffff"));
  test("rsha-3-3", LITS<132>("0xAffffffffffffffffffffffffffffffff").rsha(LIT<5>(3)), LITS<132>("0xf5fffffffffffffffffffffffffffffff"));
  test("rsha-3-4", LITS<130>("0x3ffffffffffffffffffffffffffffffff").rsha(LIT<5>(60)), LITS<130>("0x3ffffffffffffffffffffffffffffffff"));

  test("rsha-3-5", LITS<130>("0x3ffffffffffffffffffffffffffffffff").rsha(LIT<7>(68)), LITS<130>("0x3ffffffffffffffffffffffffffffffff"));
  test("rsha-3-6", LITS<192>("0xffffffffffffffffffffffffffffffffffffffffffffffff").rsha(LIT<7>(132)), LITS<192>("0xffffffffffffffffffffffffffffffffffffffffffffffff"));
  test("rsha-3-7", LITS<192>("0xffffffffffffffffffffffffffffffffffffffffffffffff").rsha(LIT<7>(68)), LITS<192>("0xffffffffffffffffffffffffffffffffffffffffffffffff"));
  test("rsha-3-8", LITS<192>("0xffffffffffffffffffffffffffffffffffffffffffffffff").rsha(LIT<7>(64)), LITS<192>("0xffffffffffffffffffffffffffffffffffffffffffffffff"));
  test("rsha-3-9", LITS<192>("0xffffffffffffffffffffffffffffffffffffffffffffffff").rsha(LIT<10>(128)), LITS<192>("0xffffffffffffffffffffffffffffffffffffffffffffffff"));
  test("rsha-3-10", LITS<192>("0x7fffffffffffffffffffffffffffffffffffffffffffffff").rsha(LIT<7>(64)), LITS<192>("0x000000000000000007fffffffffffffffffffffffffffffff"));
  test("rsha-3-11", LITS<192>("0x7fffffffffffffffffffffffffffffffffffffffffffffff").rsha(LIT<10>(128)), LITS<192>("0x000000000000000000000000000000007fffffffffffffff"));
  test("rsha-3-12", LITS<192>("0x7fffffffffffffffffffffffffffffffffffffffffffffff").rsha(LIT<32>(190)),LITS<192>("0x000000000000000000000000000000000000000000000001"));

  test("rsha-3-13", LITS<130>("0x1ffffffffffffffffffffffffffffffff").rsha(LIT<7>(67)), LITS<130>("0x3fffffffffffffff"));
  test("rsha-4-0", LITS<194>("0x3ffffffffffffffffffffffffffffffffffffffffffffffff").rsha(LIT<5>(0x1a)), LITS<194>("0x3ffffffffffffffffffffffffffffffffffffffffffffffff"));
  test("rsha-6-0", LITS<322>("0x3fffffffffffffffffffffffffffffffffffffffeffffffff7ffbffffffffffffffffffffffffffff").rsha(LIT<1>(0)), LITS<322>("0x3fffffffffffffffffffffffffffffffffffffffeffffffff7ffbffffffffffffffffffffffffffff"));

  test("rsh-4-0", LITS<194>("0x3ffffffffffffffffffffffffffffffffffffffffffffffff") >> LIT<5>(0x1a), LITS<194>("0xffffffffffffffffffffffffffffffffffffffffff"));
  test("rsh-4-0", LITS<194>("0x3ffffffffffffffffffffffffffffffffffffffffffffffff") >> LIT<5>(0x1a), LITS<194>("0x0000000ffffffffffffffffffffffffffffffffffffffffff"));
  
  // printf("%llx\n", (((val_t)1) << val_n_bits()));
  // printf("%llx\n", (((val_t)1) << val_n_bits()));
  // printf("%llx\n", (((val_t)1) << val_n_bits())-1L);
}
