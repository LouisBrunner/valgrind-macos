/* Test "convert to fixed" and "convert to logical" insns.

   We already know via bfp-emit.pl:

     Testing:  cfebr %r0,0,%f1
     Frontend: cfebr     %r0,0,%f1
     Assembly: cfebr     %r5,0,%f7

     Testing:  celfbr %f0,0,%r1,0
     Frontend: celfbr    %f0,0,%r1,0
     Assembly: celfbr    %f7,0,%r4,0

   So the only thing to test for here is the condition code.

   Note, no testing for "convert from ..." insns here, as they are
   covered in bfd-emit.pl and do not set the condition code.
*/
#include <stdio.h>
#include <stdint.h>
#include <math.h>
#include <assert.h>
#include "rounding.h"

// Convert to fixed
#define CFEBR "0xb398"
#define CFDBR "0xb399"
#define CFXBR "0xb39a"
#define CGEBR "0xb3a8"
#define CGDBR "0xb3a8"
#define CGXBR "0xb3aa"

// Convert to logical
#define CLFEBR "0xb39c"
#define CLFDBR "0xb39d"
#define CLFXBR "0xb39e"
#define CLGEBR "0xb3ac"
#define CLGDBR "0xb3ad"
#define CLGXBR "0xb3ae"

#define convert_to_int(mnm,opcode,src_type,dst_type,round,value,fmt) \
   do {                                                              \
      src_type src = value;                                          \
      dst_type dst;                                                  \
      unsigned cc;                                                   \
                                                                     \
      __asm__ volatile(".insn rrf," opcode "0000," "%[r1],%[r2],%[m3],0\n\t" \
                        "ipm %[cc]\n\t"                              \
                        "srl %[cc],28\n\t"                           \
                        : [r1] "=d"(dst), [cc] "=d"(cc)              \
                        : [r2] "f"(src),  [m3] "i"(round)            \
                        : "cc");                                     \
                                                                     \
      printf("%s "fmt"\tcc = %u\n", mnm, src, cc);                   \
} while (0)

#define convert_to_logical(mnm,opcode,src_type,dst_type,round,value,fmt) \
   do {                                                              \
      src_type src = value;                                          \
      dst_type dst;                                                  \
      unsigned cc;                                                   \
                                                                     \
      __asm__ volatile(".insn rrf," opcode "0000," "%[r1],%[r2],%[m3],0\n\t" \
                       "ipm %[cc]\n\t"                               \
                       "srl %[cc],28\n\t"                            \
                       : [r1] "=d"(dst), [cc] "=d"(cc)               \
                       : [r2] "f"(src),  [m3] "i"(round)             \
                       : "cc");                                      \
                                                                     \
      printf("%s "fmt"\tcc = %u\n", mnm, src, cc);                   \
} while (0)

/* Convenience macros */
#define cfebr(value,round)                                           \
        convert_to_int("cfebr",CFEBR,float,int32_t,round,value,"%f")
#define cfdbr(value,round)                                           \
        convert_to_int("cfdbr",CFDBR,double,int32_t,round,value,"%f")
#define cfxbr(value,round)                                           \
        convert_to_int("cfxbr",CFXBR,long double,int32_t,round,value,"%Lf")
#define cgebr(value,round)                                           \
        convert_to_int("cgebr",CGEBR,float,int64_t,round,value,"%f")
#define cgdbr(value,round)                                           \
        convert_to_int("cgdbr",CGDBR,double,int64_t,round,value,"%f")
#define cgxbr(value,round)                                           \
        convert_to_int("cgxbr",CGXBR,long double,int64_t,round,value,"%Lf")

#define clfebr(value,round)                                           \
        convert_to_logical("clfebr",CLFEBR,float,uint32_t,round,value,"%f")
#define clfdbr(value,round)                                           \
        convert_to_logical("clfdbr",CLFDBR,double,uint32_t,round,value,"%f")
#define clfxbr(value,round)                                           \
        convert_to_logical("clfxbr",CLFXBR,long double,uint32_t,round,value,"%Lf")
#define clgebr(value,round)                                           \
        convert_to_logical("clgebr",CLGEBR,float,uint64_t,round,value,"%f")
#define clgdbr(value,round)                                           \
        convert_to_logical("clgdbr",CLGDBR,double,uint64_t,round,value,"%f")
#define clgxbr(value,round)                                           \
        convert_to_logical("clgxbr",CLGXBR,long double,uint64_t,round,value,"%Lf")

#define convert_to_int_m3_tests(mode)                                   \
        printf("...setting M3 rounding mode to %s\n", m3_rtext(mode));  \
                                                                        \
        /* f32 -> i32 */                                                \
        printf("......f32 -> i32\n");                                   \
        for (int j = 0; j < sizeof values / sizeof values[0]; ++j)      \
           cfebr(values[j], mode);                                      \
                                                                        \
        /* f32 -> i64 */                                                \
        printf("......f32 -> i64\n");                                   \
        for (int j = 0; j < sizeof values / sizeof values[0]; ++j)      \
           cgebr(values[j], mode);                                      \
                                                                        \
        /* f64 -> i32 */                                                \
        printf("......f64 -> i32\n");                                   \
        for (int j = 0; j < sizeof values / sizeof values[0]; ++j)      \
           cfdbr(values[j], mode);                                      \
                                                                        \
        /* f64 -> i64 */                                                \
        printf("......f64 -> i64\n");                                   \
        for (int j = 0; j < sizeof values / sizeof values[0]; ++j)      \
           cgdbr(values[j], mode);                                      \
                                                                        \
        /* f128 -> i32 */                                               \
        printf("......f128 -> i32\n");                                  \
        for (int j = 0; j < sizeof values / sizeof values[0]; ++j)      \
           cfxbr(values[j], mode);                                      \
                                                                        \
        /* f128 -> i64 */                                               \
        printf("......f128 -> i64\n");                                  \
        for (int j = 0; j < sizeof values / sizeof values[0]; ++j)      \
           cgxbr(values[j], mode);

#define convert_to_logical_m3_tests(mode)                               \
        printf("...setting M3 rounding mode to %s\n", m3_rtext(mode));  \
                                                                        \
        /* f32 -> u32 */                                                \
        printf("......f32 -> u32\n");                                   \
        for (int j = 0; j < sizeof values / sizeof values[0]; ++j)      \
           clfebr(values[j], mode);                                     \
                                                                        \
        /* f32 -> u64 */                                                \
        printf("......f32 -> u64\n");                                   \
        for (int j = 0; j < sizeof values / sizeof values[0]; ++j)      \
           clgebr(values[j], mode);                                     \
                                                                        \
        /* f64 -> u32 */                                                \
        printf("......f64 -> u32\n");                                   \
        for (int j = 0; j < sizeof values / sizeof values[0]; ++j)      \
           clfdbr(values[j], mode);                                     \
                                                                        \
        /* f64 -> u64 */                                                \
        printf("......f64 -> u64\n");                                   \
        for (int j = 0; j < sizeof values / sizeof values[0]; ++j)      \
           clgdbr(values[j], mode);                                     \
                                                                        \
        /* f128 -> u32 */                                               \
        printf("......f128 -> u32\n");                                  \
        for (int j = 0; j < sizeof values / sizeof values[0]; ++j)      \
           clfxbr(values[j], mode);                                     \
                                                                        \
        /* f128 -> u64 */                                               \
        printf("......f128 -> u64\n");                                  \
        for (int j = 0; j < sizeof values / sizeof values[0]; ++j)      \
           clgxbr(values[j], mode);

static void
set_rounding_mode(unsigned mode)
{
   __asm__ volatile ("sfpc %[r1]" : : [r1]"d"(mode));
}

static const char *
fpc_rtext(unsigned fpc_round)
{
   switch (fpc_round) {
   case FPC_BFP_ROUND_NEAREST_EVEN:  return "[-> near]";
   case FPC_BFP_ROUND_ZERO:          return "[-> zero]";
   case FPC_BFP_ROUND_POSINF:        return "[-> +inf]";
   case FPC_BFP_ROUND_NEGINF:        return "[-> -inf]";
   case FPC_BFP_ROUND_PREPARE_SHORT: return "[-> prepare-short]";
   }
   assert(0);
}

static const char *
m3_rtext(unsigned m3_round)
{
   switch (m3_round) {
   case M3_BFP_ROUND_PER_FPC:       return "[per fpc]";
   case M3_BFP_ROUND_NEAREST_AWAY:  return "[-> nearest with ties away from 0]";
   case M3_BFP_ROUND_PREPARE_SHORT: return "[-> prepare-short]";
   case M3_BFP_ROUND_NEAREST_EVEN:  return "[-> nearest with ties to even]";
   case M3_BFP_ROUND_ZERO:          return "[-> zero]";
   case M3_BFP_ROUND_POSINF:        return "[-> +inf]";
   case M3_BFP_ROUND_NEGINF:        return "[-> -inf]";
   }
   assert(0);
}

/* Can be converted to 64-bit and 128-bit values without issues. */
static const float values[] = {
   0.0f, -0.25f, 2.0f, NAN, INFINITY
};

static void
convert_to_int_fpc_tests(unsigned mode)
{
   set_rounding_mode(mode);

   /* f32 -> i32 */
   printf("......f32 -> i32\n");
   for (int j = 0; j < sizeof values / sizeof values[0]; ++j)
      cfebr(values[j], M3_BFP_ROUND_PER_FPC);

   /* f32 -> i64 */
   printf("......f32 -> i64\n");
   for (int j = 0; j < sizeof values / sizeof values[0]; ++j)
      cgebr(values[j], M3_BFP_ROUND_PER_FPC);

   /* f64 -> i32 */
   printf("......f64 -> i32\n");
   for (int j = 0; j < sizeof values / sizeof values[0]; ++j)
      cfdbr(values[j], M3_BFP_ROUND_PER_FPC);

   /* f64 -> i64 */
   printf("......f64 -> i64\n");
   for (int j = 0; j < sizeof values / sizeof values[0]; ++j)
      cgdbr(values[j], M3_BFP_ROUND_PER_FPC);

   /* f128 -> i32 */
   printf("......f128 -> i32\n");
   for (int j = 0; j < sizeof values / sizeof values[0]; ++j)
      cfxbr(values[j], M3_BFP_ROUND_PER_FPC);

   /* f128 -> i64 */
   printf("......f128 -> i64\n");
   for (int j = 0; j < sizeof values / sizeof values[0]; ++j)
      cgxbr(values[j], M3_BFP_ROUND_PER_FPC);
}

static void
convert_to_logical_fpc_tests(unsigned mode)
{
   set_rounding_mode(mode);

   /* f32 -> u32 */
   printf("......f32 -> u32\n");
   for (int j = 0; j < sizeof values / sizeof values[0]; ++j)
      clfebr(values[j], M3_BFP_ROUND_PER_FPC);

   /* f32 -> u64 */
   printf("......f32 -> u64\n");
   for (int j = 0; j < sizeof values / sizeof values[0]; ++j)
      clgebr(values[j], M3_BFP_ROUND_PER_FPC);

   /* f64 -> u32 */
   printf("......f64 -> u32\n");
   for (int j = 0; j < sizeof values / sizeof values[0]; ++j)
      clfdbr(values[j], M3_BFP_ROUND_PER_FPC);

   /* f64 -> u64 */
   printf("......f64 -> u64\n");
   for (int j = 0; j < sizeof values / sizeof values[0]; ++j)
      clgdbr(values[j], M3_BFP_ROUND_PER_FPC);

   /* f128 -> u32 */
   printf("......f128 -> u32\n");
   for (int j = 0; j < sizeof values / sizeof values[0]; ++j)
      clfxbr(values[j], M3_BFP_ROUND_PER_FPC);

   /* f128 -> u64 */
   printf("......f128 -> u64\n");
   for (int j = 0; j < sizeof values / sizeof values[0]; ++j)
      clgxbr(values[j], M3_BFP_ROUND_PER_FPC);
}

int
main(void)
{
   assert(sizeof(long double) == 16);

   static const unsigned fpc_rmodes[] = {
      FPC_BFP_ROUND_NEAREST_EVEN,
      FPC_BFP_ROUND_ZERO,
      FPC_BFP_ROUND_POSINF,
      FPC_BFP_ROUND_NEGINF,
      FPC_BFP_ROUND_PREPARE_SHORT
   };

   //----------------------------------------------------------
   // Convert to int
   //----------------------------------------------------------
   printf("============ Convert to int =============\n");
   printf("Rounding as 'per FPC'\n");
   for (int i = 0; i < sizeof fpc_rmodes / sizeof fpc_rmodes[0]; ++i) {
      printf("...setting FPC rounding mode to %s\n", fpc_rtext(fpc_rmodes[i]));
      convert_to_int_fpc_tests(fpc_rmodes[i]);
   }

   printf("Rounding as 'per M3'\n");
   convert_to_int_m3_tests(M3_BFP_ROUND_NEAREST_AWAY);
   convert_to_int_m3_tests(M3_BFP_ROUND_PREPARE_SHORT);
   convert_to_int_m3_tests(M3_BFP_ROUND_NEAREST_EVEN);
   convert_to_int_m3_tests(M3_BFP_ROUND_ZERO);
   convert_to_int_m3_tests(M3_BFP_ROUND_POSINF);
   convert_to_int_m3_tests(M3_BFP_ROUND_NEGINF);

   //----------------------------------------------------------
   // Convert to logical
   //----------------------------------------------------------
   putchar('\n');
   printf("============ Convert to logical =============\n");
   printf("Rounding as 'per FPC'\n");
   for (int i = 0; i < sizeof fpc_rmodes / sizeof fpc_rmodes[0]; ++i) {
      printf("...setting FPC rounding mode to %s\n", fpc_rtext(fpc_rmodes[i]));
      convert_to_logical_fpc_tests(fpc_rmodes[i]);
   }

   printf("Rounding as 'per M3'\n");
   convert_to_logical_m3_tests(M3_BFP_ROUND_NEAREST_AWAY);
   convert_to_logical_m3_tests(M3_BFP_ROUND_PREPARE_SHORT);
   convert_to_logical_m3_tests(M3_BFP_ROUND_NEAREST_EVEN);
   convert_to_logical_m3_tests(M3_BFP_ROUND_ZERO);
   convert_to_logical_m3_tests(M3_BFP_ROUND_POSINF);
   convert_to_logical_m3_tests(M3_BFP_ROUND_NEGINF);

   return 0;
}
