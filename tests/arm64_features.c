#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if !defined(__APPLE__) && defined(VGA_arm64)
#include <sys/auxv.h>
#endif

// This file determines arm64 features a processor supports.
// Arm processors do not have a x86-like cpuinfo instruction. Instead the
// getauxval() syscall is used with capabilities parameters: getauxval(AT_HWCAP)
// and getauxval(AT_HWCAP2).
//
// We return:
// - 0 if the machine has the asked-for feature.
// - 1 if the machine doesn't have the asked-for feature.
// - 2 if the asked-for feature isn't recognised (this will always be the case
//     for any feature if run on a non-arm64 machine).
// - 3 if there was a usage error (it also prints an error message).
#define FEATURE_PRESENT       0
#define FEATURE_NOT_PRESENT   1
#define UNRECOGNISED_FEATURE  2
#define USAGE_ERROR           3

#define False  0
#define True   1
typedef int    Bool;

#if defined(VGA_arm64)

// The processor's capabilities/features are returned by getauxval() as an
// unsigned long with each bit representing a capability/feature.
#define HWCAP_FP            (1 << 0)
#define HWCAP_ASIMD         (1 << 1)
#define HWCAP_EVTSTRM       (1 << 2)
#define HWCAP_AES           (1 << 3)
#define HWCAP_PMULL         (1 << 4)
#define HWCAP_SHA1          (1 << 5)
#define HWCAP_SHA2          (1 << 6)
#define HWCAP_CRC32         (1 << 7)
#define HWCAP_ATOMICS       (1 << 8)
#define HWCAP_FPHP          (1 << 9)
#define HWCAP_ASIMDHP       (1 << 10)
#define HWCAP_CPUID         (1 << 11)
#define HWCAP_ASIMDRDM      (1 << 12)
#define HWCAP_JSCVT         (1 << 13)
#define HWCAP_FCMA          (1 << 14)
#define HWCAP_LRCPC         (1 << 15)
#define HWCAP_DCPOP         (1 << 16)
#define HWCAP_SHA3          (1 << 17)
#define HWCAP_SM3           (1 << 18)
#define HWCAP_SM4           (1 << 19)
#define HWCAP_ASIMDDP       (1 << 20)
#define HWCAP_SHA512        (1 << 21)
#define HWCAP_SVE           (1 << 22)
#define HWCAP_ASIMDFHM      (1 << 23)
#define HWCAP_DIT           (1 << 24)
#define HWCAP_USCAT         (1 << 25)
#define HWCAP_ILRCPC        (1 << 26)
#define HWCAP_FLAGM         (1 << 27)
#define HWCAP_SSBS          (1 << 28)
#define HWCAP_SB            (1 << 29)
#define HWCAP_PACA          (1 << 30)
#define HWCAP_PACG          (1UL << 31)

#define HWCAP2_DCPODP       (1 << 0)
#define HWCAP2_SVE2         (1 << 1)
#define HWCAP2_SVEAES       (1 << 2)
#define HWCAP2_SVEPMULL     (1 << 3)
#define HWCAP2_SVEBITPERM   (1 << 4)
#define HWCAP2_SVESHA3      (1 << 5)
#define HWCAP2_SVESM4       (1 << 6)
#define HWCAP2_FLAGM2       (1 << 7)
#define HWCAP2_FRINT        (1 << 8)

unsigned long hwcaps[] = {
   HWCAP_FP,     HWCAP_ASIMD,  HWCAP_EVTSTRM, HWCAP_AES,     HWCAP_PMULL,
   HWCAP_SHA1,   HWCAP_SHA2,   HWCAP_CRC32,   HWCAP_ATOMICS, HWCAP_FPHP,
   HWCAP_ASIMDHP,HWCAP_CPUID,  HWCAP_ASIMDRDM,HWCAP_JSCVT,   HWCAP_FCMA,
   HWCAP_LRCPC,  HWCAP_DCPOP,  HWCAP_SHA3,    HWCAP_SM3,     HWCAP_SM4,
   HWCAP_ASIMDDP,HWCAP_SHA512, HWCAP_SVE,     HWCAP_ASIMDFHM,HWCAP_DIT,
   HWCAP_USCAT,  HWCAP_ILRCPC, HWCAP_FLAGM,   HWCAP_SSBS,    HWCAP_SB,
   HWCAP_PACA,   HWCAP_PACG,   0ul};

unsigned long hwcaps2[] = {
   HWCAP2_DCPODP,     HWCAP2_SVE2,    HWCAP2_SVEAES, HWCAP2_SVEPMULL,
   HWCAP2_SVEBITPERM, HWCAP2_SVESHA3, HWCAP2_SVESM4, HWCAP2_FLAGM2,
   HWCAP2_FRINT,      0ul};

typedef struct
{
   char name[16];
   unsigned long cap_bit;
} capability;

capability capabilities[] = {
   {"fp",       HWCAP_FP},       {"asimd",    HWCAP_ASIMD},
   {"evtstrm",  HWCAP_EVTSTRM},  {"aes",      HWCAP_AES},
   {"pmull",    HWCAP_PMULL},    {"sha1",     HWCAP_SHA1},
   {"sha2",     HWCAP_SHA2},     {"crc32",    HWCAP_CRC32},
   {"atomics",  HWCAP_ATOMICS},  {"fphp",     HWCAP_FPHP},
   {"asimdhp",  HWCAP_ASIMDHP},  {"cpuid",    HWCAP_CPUID},
   {"asimdrdm", HWCAP_ASIMDRDM}, {"jscvt",    HWCAP_JSCVT},
   {"fcma",     HWCAP_FCMA},     {"lrcpc",    HWCAP_LRCPC},
   {"dcpop",    HWCAP_DCPOP},    {"sha3",     HWCAP_SHA3},
   {"sm3",      HWCAP_SM3},      {"sm4",      HWCAP_SM4},
   {"asimddp",  HWCAP_ASIMDDP},  {"sha512",   HWCAP_SHA512},
   {"sve",      HWCAP_SVE},      {"asimdfhm", HWCAP_ASIMDFHM},
   {"dit",      HWCAP_DIT},      {"uscat",    HWCAP_USCAT},
   {"ilrcpc",   HWCAP_ILRCPC},   {"flagm",    HWCAP_FLAGM},
   {"ssbs",     HWCAP_SSBS},     {"sb",       HWCAP_SB},
   {"paca",     HWCAP_PACA},     {"pacg",     HWCAP_PACG},
   {"",         0ul}
};

capability capabilities2[] = {
   {"dcpodp",     HWCAP2_DCPODP},     {"sve2",       HWCAP2_SVE2},
   {"sveaes",     HWCAP2_SVEAES},     {"svepmull",   HWCAP2_SVEPMULL},
   {"svebitperm", HWCAP2_SVEBITPERM}, {"svesha3",    HWCAP2_SVESHA3},
   {"svesm4",     HWCAP2_SVESM4},     {"flagm2",     HWCAP2_FLAGM2},
   {"frint",      HWCAP2_FRINT},      {"",           0ul}
};

typedef struct
{
   unsigned long hwcap;
   unsigned long hwcap2;
} hwc;

#define CAPABILITIES_SEARCH_LOOP(hwcversion) \
   for (int i = 0; capabilities ## hwcversion[i].cap_bit; ++i) \
      if (strcmp(name, capabilities ## hwcversion[i].name) == 0) { \
         caps->hwcap ## hwcversion = capabilities ## hwcversion[i].cap_bit; \
         return True; \
      } \

static Bool get_feature_from_string(const char *name, hwc *caps)
{
   caps->hwcap = caps->hwcap2 = 0;
   CAPABILITIES_SEARCH_LOOP()
   CAPABILITIES_SEARCH_LOOP(2)
   return False;
}

static int go(const char* feature_name)
{
   hwc hw;
   unsigned long hwcap = getauxval(AT_HWCAP);
   unsigned long hwcap2 = getauxval(AT_HWCAP2);

   if (!get_feature_from_string(feature_name, &hw))
      return UNRECOGNISED_FEATURE;

   if ((hw.hwcap & hwcap) || (hw.hwcap2 & hwcap2))
      return FEATURE_PRESENT;

   return FEATURE_NOT_PRESENT;
}

#else

static Bool go(const char* feature_name)
{
   // Feature not recognised (non-arm64 machine!)
   return UNRECOGNISED_FEATURE;
}

#endif   // defined(VGA_arm64)


//---------------------------------------------------------------------------
// main
//---------------------------------------------------------------------------
int main(int argc, char **argv)
{
   if (argc != 2) {
      fprintf(stderr, "usage: arm64_features <feature>\n");
      exit(USAGE_ERROR);
   }
   return go(argv[1]);
}
