
#include <string.h>
#include <stdio.h>
#include <assert.h>

typedef  unsigned int   UInt;
typedef  signed int     Int;
typedef  unsigned char  UChar;
typedef  unsigned long long int ULong;
typedef  UChar          Bool;
#define False ((Bool)0)
#define True  ((Bool)1)

//typedef  unsigned char  V128[16];
typedef
   union {
      UChar uChar[16];
      UInt  uInt[4];
   }
   V128;

static UChar fromhex(char x) {
   if      (x >= '0' && x <= '9') { return(x - '0'); }
   else if (x >= 'A' && x <= 'F') { return(x - 'A' + 10); }
   else if (x >= 'a' && x <= 'f') { return(x - 'a' + 10); }
   else assert(0);
}

static void expand ( V128* dst, char* summary )
{
   Int i;
   assert( strlen(summary) == 32 );
   for (i = 0; i < 16; i++) {
      UChar xx = 0;
      UChar x = summary[31-2*i];
      UChar yy = 0;
      UChar y = summary[31-2*i-1];
      xx = fromhex (x);
      yy = fromhex (y);

      assert(xx < 16);
      assert(yy < 16);
      xx = (yy << 4) | xx;
      assert(xx < 256);
      dst->uChar[i] = xx;
   }
}

static int tohex (int nib)
{
   if (nib < 10)
      return '0' + nib;
   else
      return 'a' + nib - 10;
}
static void unexpand ( V128* dst, char* summary )
{
   Int i;
   for (i = 0; i < 16; i++) {
      *summary++ = tohex((dst->uChar[i] >> 4) & 0xf);
      *summary++ = tohex(dst->uChar[i] & 0xf);
   }
   *summary = 0;
}

static void AESDEC(char *s_argL, char *s_argR, char *s_exp)
{
   /*
     ; xmm1 and xmm2 hold two 128-bit inputs (xmm1 = State; xmm2 = Round key).
     ; The result is delivered in xmm1.
   */
   V128 argL, argR;
   V128 res;
   char s_res[33];
   V128 exp;
   expand(&argL, s_argL);
   expand(&argR, s_argR);
   __asm__ __volatile__(
      "subq      $1024,  %%rsp"             "\n\t"
      "movdqu    %1,     %%xmm1"            "\n\t"
      "movdqu    %2,     %%xmm2"            "\n\t"
      "aesdec    %%xmm2, %%xmm1"            "\n\t"
      "movdqu    %%xmm1, %0"                "\n\t"
      "addq      $1024,  %%rsp"             "\n\t"
      : /*out*/ "=m"(res)
      : "m"/*in*/(argL), "m"/*in*/(argR)
      : /*trash*/ "xmm1", "xmm2"
   );

   if (strlen(s_exp) > 0) {
      expand(&exp,  s_exp);
      assert (0 == memcmp(&res, &exp, 16));
   }
   unexpand (&res, s_res);
   printf ("aesdec %s %s result %s\n", s_argL, s_argR, s_res);
}

static void AESDECLAST(char *s_argL, char *s_argR, char *s_exp)
{
   /*
     ; xmm1 and xmm2 hold two 128-bit inputs (xmm1 = State; xmm2 = Round key).
     ; The result is delivered in xmm1.
   */
   V128 argL, argR;
   V128 res;
   char s_res[33];
   V128 exp;
   expand(&argL, s_argL);
   expand(&argR, s_argR);
   __asm__ __volatile__(
      "subq       $1024,  %%rsp"             "\n\t"
      "movdqu     %1,     %%xmm1"            "\n\t"
      "movdqu     %2,     %%xmm2"            "\n\t"
      "aesdeclast %%xmm2, %%xmm1"            "\n\t"
      "movdqu     %%xmm1, %0"                "\n\t"
      "addq       $1024,  %%rsp"             "\n\t"
      : /*out*/ "=m"(res)
      : "m"/*in*/(argL), "m"/*in*/(argR)
      : /*trash*/ "xmm1", "xmm2"
   );

   if (strlen(s_exp) > 0) {
      expand(&exp,  s_exp);
      assert (0 == memcmp(&res, &exp, 16));
   }
   unexpand (&res, s_res);
   printf ("aesdeclast %s %s result %s\n", s_argL, s_argR, s_res);
}

static void AESENC(char *s_argL, char *s_argR, char *s_exp)
{
   /*
     ; xmm1 and xmm2 hold two 128-bit inputs (xmm1 = State; xmm2 = Round key).
     ; The result is delivered in xmm1.
   */
   V128 argL, argR;
   V128 res;
   char s_res[33];
   V128 exp;
   expand(&argL, s_argL);
   expand(&argR, s_argR);
   __asm__ __volatile__(
      "subq      $1024,  %%rsp"             "\n\t"
      "movdqu    %1,     %%xmm1"            "\n\t"
      "movdqu    %2,     %%xmm2"            "\n\t"
      "aesenc    %%xmm2, %%xmm1"            "\n\t"
      "movdqu    %%xmm1, %0"                "\n\t"
      "addq      $1024,  %%rsp"             "\n\t"
      : /*out*/ "=m"(res)
      : "m"/*in*/(argL), "m"/*in*/(argR)
      : /*trash*/ "xmm1", "xmm2"
   );

   if (strlen(s_exp) > 0) {
      expand(&exp,  s_exp);
      assert (0 == memcmp(&res, &exp, 16));
   }
   unexpand (&res, s_res);
   printf ("aesenc %s %s result %s\n", s_argL, s_argR, s_res);
}

static void AESENCLAST(char *s_argL, char *s_argR, char *s_exp)
{
   /*
     ; xmm1 and xmm2 hold two 128-bit inputs (xmm1 = State; xmm2 = Round key)
     ; The result delivered in xmm1
   */
   V128 argL, argR;
   V128 res;
   char s_res[33];
   V128 exp;
   expand(&argL, s_argL);
   expand(&argR, s_argR);
   __asm__ __volatile__(
      "subq       $1024,  %%rsp"             "\n\t"
      "movdqu     %1,     %%xmm1"            "\n\t"
      "movdqu     %2,     %%xmm2"            "\n\t"
      "aesenclast %%xmm2, %%xmm1"            "\n\t"
      "movdqu     %%xmm1, %0"                "\n\t"
      "addq       $1024,  %%rsp"             "\n\t"
      : /*out*/ "=m"(res)
      : "m"/*in*/(argL), "m"/*in*/(argR)
      : /*trash*/ "xmm1", "xmm2"
   );

   if (strlen(s_exp) > 0) {
      expand(&exp,  s_exp);
      assert (0 == memcmp(&res, &exp, 16));
   }
   unexpand (&res, s_res);
   printf ("aesenclast %s %s result %s\n", s_argL, s_argR, s_res);
}

static void AESIMC(char *s_argR, char *s_exp)
{
   /* We test another way to pass input and get results */
   /* ; argR hold one 128-bit inputs (argR = Round key)
      ; result delivered in xmm5 */

   V128 argR;
   V128 res;
   char s_res[33];
   V128 exp;
   expand(&argR, s_argR);

   __asm__ __volatile__(
      "subq       $1024,  %%rsp"             "\n\t"
      "aesimc     %1,     %%xmm5"            "\n\t"
      "movdqu     %%xmm5, %0"                "\n\t"
      "addq       $1024,  %%rsp"             "\n\t"
      : /*out*/ "=m"(res)
      : "m"/*in*/(argR)
      : /*trash*/ "xmm5"
   );

   if (strlen(s_exp) > 0) {
      expand(&exp,  s_exp);
      assert (0 == memcmp(&res, &exp, 16));
   }
   unexpand (&res, s_res);
   printf ("aesimc %s result %s\n", s_argR, s_res);
}

static void AESKEYGENASSIST(int imm, char* s_argL, char* s_exp)
{
   /*
     ; xmm2 holds a 128-bit input; imm8 holds the RCON value
     ; result delivered in xmm1
   */

   V128 argL;
   V128 res;
   char s_res[33];
   V128 exp;
   expand(&argL, s_argL);
   if (imm == 1)
      __asm__ __volatile__(
         "subq       $1024,  %%rsp"             "\n\t"
         "movdqu     %1,     %%xmm2"            "\n\t"
         "aeskeygenassist $1,%%xmm2, %%xmm1"    "\n\t"
         "movdqu     %%xmm1, %0"                "\n\t"
         "addq       $1024,  %%rsp"             "\n\t"
         : /*out*/ "=m"(res)
         : "m"/*in*/(argL)
         : /*trash*/ "xmm1", "xmm2"
      );
   else if (imm == 2)
      __asm__ __volatile__(
         "subq       $1024,  %%rsp"             "\n\t"
         "movdqu     %1,     %%xmm2"            "\n\t"
         "aeskeygenassist $2,%%xmm2, %%xmm1"    "\n\t"
         "movdqu     %%xmm1, %0"                "\n\t"
         "addq       $1024,  %%rsp"             "\n\t"
         : /*out*/ "=m"(res)
         : "m"/*in*/(argL)
         : /*trash*/ "xmm1", "xmm2"
      );
   else if (imm == 8)
      __asm__ __volatile__(
         "subq       $1024,  %%rsp"             "\n\t"
         "movdqu     %1,     %%xmm2"            "\n\t"
         "aeskeygenassist $8,%%xmm2, %%xmm1"    "\n\t"
         "movdqu     %%xmm1, %0"                "\n\t"
         "addq       $1024,  %%rsp"             "\n\t"
         : /*out*/ "=m"(res)
         : "m"/*in*/(argL)
         : /*trash*/ "xmm1", "xmm2"
      );
   else assert (0);

   if (strlen(s_exp) > 0) {
      expand(&exp,  s_exp);
      assert (0 == memcmp(&res, &exp, 16));
   }
   unexpand (&res, s_res);
   printf ("aeskeygenassist %d %s result %s\n", imm, s_argL, s_res);
}

typedef struct Aes_Args {
   char* argL;
   char* argR;
   int imm; // only for aeskeygenassist
} Aes_Args;

/* Just a bunch of various data to compare a native run
   with a run under Valgrind. */
static const Aes_Args aes_args[] = {
   {"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
    8},
   {"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
    "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    8},
   {"3243f6a8885a308d313198a2e0370734",
    "2b7e151628aed2a6abf7158809cf4f3c",
    2},
   {"193de3bea0f4e22b9ac68d2ae9f84808",
    "d42711aee0bf98f1b8b45de51e415230",
    2},
   {"d4bf5d30e0b452aeb84111f11e2798e5",
    "046681e5e0cb199a48f8d37a2806264c",
    1},
   {"a0fafe1788542cb123a339392a6c7605",
    "a49c7ff2689f352b6b5bea43026a5049",
    1},
   {"49ded28945db96f17f39871a7702533b",
    "49db873b453953897f02d2f177de961a",
    8},
   {"584dcaf11b4b5aacdbe7caa81b6bb0e5",
    "f2c295f27a96b9435935807a7359f67f",
    8},
   {"aa8f5f0361dde3ef82d24ad26832469a",
    "ac73cf7befc111df13b5d6b545235ab8",
    2},
   {"acc1d6b8efb55a7b1323cfdf457311b5",
    "75ec0993200b633353c0cf7cbb25d0dc",
    2},
   {"e9317db5cb322c723d2e895faf090794",
    "d014f9a8c9ee2589e13f0cc8b6630ca6",
    1},
   {NULL,
    NULL,
    0}
};

int main ( void )
{
   int i;

   /* test the various instructions, using the examples provided
      in  "White Paper Intel Advanced Encryption Standard AES
          instruction set" January 2010 (26/1/2010)
          Rev. 3.0
          by Shay Gueron */
   AESKEYGENASSIST(1,
                   "3c4fcf098815f7aba6d2ae2816157e2b",
                   "01eb848beb848a013424b5e524b5e434");
   AESENC("7b5b54657374566563746f725d53475d",
          "48692853686179295b477565726f6e5d",
          "a8311c2f9fdba3c58b104b58ded7e595");
   AESENCLAST("7b5b54657374566563746f725d53475d",
              "48692853686179295b477565726f6e5d",
              "c7fb881e938c5964177ec42553fdc611");
   AESDEC("7b5b54657374566563746f725d53475d",
          "48692853686179295b477565726f6e5d",
          "138ac342faea2787b58eb95eb730392a");
   AESDECLAST("7b5b54657374566563746f725d53475d",
              "48692853686179295b477565726f6e5d",
              "c5a391ef6b317f95d410637b72a593d0");
   /* ??? the AESIMC example given in the Intel White paper
      seems wrong.
      The below fails both under Valgrind and natively.
      AESIMC("48692853686179295b477565726f6e5d",
             "627a6f6644b109c82b18330a81c3b3e5");
      So we use the example given for the InvMixColums 
      transformation. */
   AESIMC("8dcab9dc035006bc8f57161e00cafd8d",
          "d635a667928b5eaeeec9cc3bc55f5777");


   /* and now a bunch of other calls. The below are verified
      using the aes.stdout.exp (produced by a native run). */
   
   for (i = 0; aes_args[i].argL != NULL; i++) {
      AESKEYGENASSIST(aes_args[i].imm, aes_args[i].argL, "");
      AESENC(aes_args[i].argL, aes_args[i].argR, "");
      AESENCLAST(aes_args[i].argL, aes_args[i].argR, "");
      AESDEC(aes_args[i].argL, aes_args[i].argR, "");
      AESDECLAST(aes_args[i].argL, aes_args[i].argR, "");
      AESIMC(aes_args[i].argL, "");
   }
   return 0;
}
