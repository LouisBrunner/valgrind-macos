
#include <stdio.h>

typedef  unsigned long long int  ULong;

/* ------------------------ SRADI ------------------------ */

#define INSN_SRADI(nnn)                                                    \
   void do_sradi_##nnn ( ULong arg, /*OUT*/ULong* res, /*OUT*/ULong* xer ) \
   {                                                                       \
     ULong argW = arg;                                                     \
     ULong resW = 0;                                                       \
     ULong xerW = 0;                                                       \
     __asm__ __volatile__(                                                 \
        "sradi %0,%2, " #nnn "\n\t"                                        \
        "mfxer %1"                                                         \
        : /*out*/ "=b"(resW),  "=b"(xerW)                                  \
        : /*in*/  "b"(argW)                                                \
        : /*trash*/ "cc"                                                   \
     );                                                                    \
     *res = resW;                                                          \
     *xer = xerW;                                                          \
   }

INSN_SRADI(0)
INSN_SRADI(1)
INSN_SRADI(2)
INSN_SRADI(3)
INSN_SRADI(4)
INSN_SRADI(5)
INSN_SRADI(6)
INSN_SRADI(7)
INSN_SRADI(8)
INSN_SRADI(9)
INSN_SRADI(10)
INSN_SRADI(11)
INSN_SRADI(12)
INSN_SRADI(13)
INSN_SRADI(14)
INSN_SRADI(15)
INSN_SRADI(16)
INSN_SRADI(17)
INSN_SRADI(18)
INSN_SRADI(19)
INSN_SRADI(20)
INSN_SRADI(21)
INSN_SRADI(22)
INSN_SRADI(23)
INSN_SRADI(24)
INSN_SRADI(25)
INSN_SRADI(26)
INSN_SRADI(27)
INSN_SRADI(28)
INSN_SRADI(29)
INSN_SRADI(30)
INSN_SRADI(31)
INSN_SRADI(32)
INSN_SRADI(33)
INSN_SRADI(34)
INSN_SRADI(35)
INSN_SRADI(36)
INSN_SRADI(37)
INSN_SRADI(38)
INSN_SRADI(39)
INSN_SRADI(40)
INSN_SRADI(41)
INSN_SRADI(42)
INSN_SRADI(43)
INSN_SRADI(44)
INSN_SRADI(45)
INSN_SRADI(46)
INSN_SRADI(47)
INSN_SRADI(48)
INSN_SRADI(49)
INSN_SRADI(50)
INSN_SRADI(51)
INSN_SRADI(52)
INSN_SRADI(53)
INSN_SRADI(54)
INSN_SRADI(55)
INSN_SRADI(56)
INSN_SRADI(57)
INSN_SRADI(58)
INSN_SRADI(59)
INSN_SRADI(60)
INSN_SRADI(61)
INSN_SRADI(62)
INSN_SRADI(63)

static void* all_sradi[64] 
  = {
       (void*)&do_sradi_0,
       (void*)&do_sradi_1,
       (void*)&do_sradi_2,
       (void*)&do_sradi_3,
       (void*)&do_sradi_4,
       (void*)&do_sradi_5,
       (void*)&do_sradi_6,
       (void*)&do_sradi_7,
       (void*)&do_sradi_8,
       (void*)&do_sradi_9,
       (void*)&do_sradi_10,
       (void*)&do_sradi_11,
       (void*)&do_sradi_12,
       (void*)&do_sradi_13,
       (void*)&do_sradi_14,
       (void*)&do_sradi_15,
       (void*)&do_sradi_16,
       (void*)&do_sradi_17,
       (void*)&do_sradi_18,
       (void*)&do_sradi_19,
       (void*)&do_sradi_20,
       (void*)&do_sradi_21,
       (void*)&do_sradi_22,
       (void*)&do_sradi_23,
       (void*)&do_sradi_24,
       (void*)&do_sradi_25,
       (void*)&do_sradi_26,
       (void*)&do_sradi_27,
       (void*)&do_sradi_28,
       (void*)&do_sradi_29,
       (void*)&do_sradi_30,
       (void*)&do_sradi_31,
       (void*)&do_sradi_32,
       (void*)&do_sradi_33,
       (void*)&do_sradi_34,
       (void*)&do_sradi_35,
       (void*)&do_sradi_36,
       (void*)&do_sradi_37,
       (void*)&do_sradi_38,
       (void*)&do_sradi_39,
       (void*)&do_sradi_40,
       (void*)&do_sradi_41,
       (void*)&do_sradi_42,
       (void*)&do_sradi_43,
       (void*)&do_sradi_44,
       (void*)&do_sradi_45,
       (void*)&do_sradi_46,
       (void*)&do_sradi_47,
       (void*)&do_sradi_48,
       (void*)&do_sradi_49,
       (void*)&do_sradi_50,
       (void*)&do_sradi_51,
       (void*)&do_sradi_52,
       (void*)&do_sradi_53,
       (void*)&do_sradi_54,
       (void*)&do_sradi_55,
       (void*)&do_sradi_56,
       (void*)&do_sradi_57,
       (void*)&do_sradi_58,
       (void*)&do_sradi_59,
       (void*)&do_sradi_60,
       (void*)&do_sradi_61,
       (void*)&do_sradi_62,
       (void*)&do_sradi_63
   };

/* ------------------------ SRAWI ------------------------ */

#define INSN_SRAWI(nnn)                                                    \
   void do_srawi_##nnn ( ULong arg, /*OUT*/ULong* res, /*OUT*/ULong* xer ) \
   {                                                                       \
     ULong argW = arg;                                                     \
     ULong resW = 0;                                                       \
     ULong xerW = 0;                                                       \
     __asm__ __volatile__(                                                 \
        "srawi %0,%2, " #nnn "\n\t"                                        \
        "mfxer %1"                                                         \
        : /*out*/ "=b"(resW),  "=b"(xerW)                                  \
        : /*in*/  "b"(argW)                                                \
        : /*trash*/ "cc"                                                   \
     );                                                                    \
     *res = resW;                                                          \
     *xer = xerW;                                                          \
   }

INSN_SRAWI(0)
INSN_SRAWI(1)
INSN_SRAWI(2)
INSN_SRAWI(3)
INSN_SRAWI(4)
INSN_SRAWI(5)
INSN_SRAWI(6)
INSN_SRAWI(7)
INSN_SRAWI(8)
INSN_SRAWI(9)
INSN_SRAWI(10)
INSN_SRAWI(11)
INSN_SRAWI(12)
INSN_SRAWI(13)
INSN_SRAWI(14)
INSN_SRAWI(15)
INSN_SRAWI(16)
INSN_SRAWI(17)
INSN_SRAWI(18)
INSN_SRAWI(19)
INSN_SRAWI(20)
INSN_SRAWI(21)
INSN_SRAWI(22)
INSN_SRAWI(23)
INSN_SRAWI(24)
INSN_SRAWI(25)
INSN_SRAWI(26)
INSN_SRAWI(27)
INSN_SRAWI(28)
INSN_SRAWI(29)
INSN_SRAWI(30)
INSN_SRAWI(31)

static void* all_srawi[32] 
  = {
       (void*)&do_srawi_0,
       (void*)&do_srawi_1,
       (void*)&do_srawi_2,
       (void*)&do_srawi_3,
       (void*)&do_srawi_4,
       (void*)&do_srawi_5,
       (void*)&do_srawi_6,
       (void*)&do_srawi_7,
       (void*)&do_srawi_8,
       (void*)&do_srawi_9,
       (void*)&do_srawi_10,
       (void*)&do_srawi_11,
       (void*)&do_srawi_12,
       (void*)&do_srawi_13,
       (void*)&do_srawi_14,
       (void*)&do_srawi_15,
       (void*)&do_srawi_16,
       (void*)&do_srawi_17,
       (void*)&do_srawi_18,
       (void*)&do_srawi_19,
       (void*)&do_srawi_20,
       (void*)&do_srawi_21,
       (void*)&do_srawi_22,
       (void*)&do_srawi_23,
       (void*)&do_srawi_24,
       (void*)&do_srawi_25,
       (void*)&do_srawi_26,
       (void*)&do_srawi_27,
       (void*)&do_srawi_28,
       (void*)&do_srawi_29,
       (void*)&do_srawi_30,
       (void*)&do_srawi_31
   };

/* ------------------------ SRAD ------------------------ */

void do_srad ( ULong arg1, ULong arg2, 
               /*OUT*/ULong* res, /*OUT*/ULong* xer )
{
   ULong arg1W = arg1;
   ULong arg2W = arg2;
   ULong resW  = 0;
   ULong xerW  = 0;
   __asm__ __volatile__(
     "srad %0,%2,%3\n\t"
     "mfxer %1"
     : /*out*/ "=b"(resW),  "=b"(xerW)
     : /*in*/  "b"(arg1W), "b"(arg2W)
     : /*trash*/ "cc"
  );
  *res = resW;
  *xer = xerW;
}


/* ------------------------ SRAW ------------------------ */

void do_sraw ( ULong arg1, ULong arg2, 
               /*OUT*/ULong* res, /*OUT*/ULong* xer )
{
   ULong arg1W = arg1;
   ULong arg2W = arg2;
   ULong resW  = 0;
   ULong xerW  = 0;
   __asm__ __volatile__(
     "sraw %0,%2,%3\n\t"
     "mfxer %1"
     : /*out*/ "=b"(resW),  "=b"(xerW)
     : /*in*/  "b"(arg1W), "b"(arg2W)
     : /*trash*/ "cc"
  );
  *res = resW;
  *xer = xerW;
}

/* ------------------------ SRD ------------------------ */

void do_srd ( ULong arg1, ULong arg2, 
              /*OUT*/ULong* res, /*OUT*/ULong* xer )
{
   ULong arg1W = arg1;
   ULong arg2W = arg2;
   ULong resW  = 0;
   ULong xerW  = 0;
   __asm__ __volatile__(
     "srd %0,%2,%3\n\t"
     "mfxer %1"
     : /*out*/ "=b"(resW),  "=b"(xerW)
     : /*in*/  "b"(arg1W), "b"(arg2W)
     : /*trash*/ "cc"
  );
  *res = resW;
  *xer = xerW;
}


/* ------------------------ SRW ------------------------ */

void do_srw ( ULong arg1, ULong arg2, 
              /*OUT*/ULong* res, /*OUT*/ULong* xer )
{
   ULong arg1W = arg1;
   ULong arg2W = arg2;
   ULong resW  = 0;
   ULong xerW  = 0;
   __asm__ __volatile__(
     "srw %0,%2,%3\n\t"
     "mfxer %1"
     : /*out*/ "=b"(resW),  "=b"(xerW)
     : /*in*/  "b"(arg1W), "b"(arg2W)
     : /*trash*/ "cc"
  );
  *res = resW;
  *xer = xerW;
}


/* ------------------------ SLD ------------------------ */

void do_sld ( ULong arg1, ULong arg2, 
              /*OUT*/ULong* res, /*OUT*/ULong* xer )
{
   ULong arg1W = arg1;
   ULong arg2W = arg2;
   ULong resW  = 0;
   ULong xerW  = 0;
   __asm__ __volatile__(
     "sld %0,%2,%3\n\t"
     "mfxer %1"
     : /*out*/ "=b"(resW),  "=b"(xerW)
     : /*in*/  "b"(arg1W), "b"(arg2W)
     : /*trash*/ "cc"
  );
  *res = resW;
  *xer = xerW;
}


/* ------------------------ SLW ------------------------ */

void do_slw ( ULong arg1, ULong arg2, 
              /*OUT*/ULong* res, /*OUT*/ULong* xer )
{
   ULong arg1W = arg1;
   ULong arg2W = arg2;
   ULong resW  = 0;
   ULong xerW  = 0;
   __asm__ __volatile__(
     "slw %0,%2,%3\n\t"
     "mfxer %1"
     : /*out*/ "=b"(resW),  "=b"(xerW)
     : /*in*/  "b"(arg1W), "b"(arg2W)
     : /*trash*/ "cc"
  );
  *res = resW;
  *xer = xerW;
}


/* ------------------------  ------------------------ */
/* ------------------------  ------------------------ */
/* ------------------------  ------------------------ */

#define N_ARGS64 41

ULong args64[N_ARGS64] = {
  0x0000000000000000ULL,

  0x0000000000000001ULL,
  0x0000000031415927ULL,
  0x000000007FFFFFFFULL,
  0x0000000080000000ULL,
  0x00000000FFFFFFFFULL,

  0x0000000100000000ULL,
  0x3141592700000000ULL,
  0x7FFFFFFF00000000ULL,
  0x8000000000000000ULL,
  0xFFFFFFFF00000000ULL,

  0x7FFFFFFF00000001ULL,
  0x7FFFFFFF31415927ULL,
  0x7FFFFFFF7FFFFFFFULL,
  0x7FFFFFFF80000000ULL,
  0x7FFFFFFFFFFFFFFFULL,

  0x000000017FFFFFFFULL,
  0x314159277FFFFFFFULL,
  0x7FFFFFFF7FFFFFFFULL,
  0x800000007FFFFFFFULL,
  0xFFFFFFFF7FFFFFFFULL,

  0x8000000000000001ULL,
  0x8000000031415927ULL,
  0x800000007FFFFFFFULL,
  0x8000000080000000ULL,
  0x80000000FFFFFFFFULL,

  0x0000000180000000ULL,
  0x3141592780000000ULL,
  0x7FFFFFFF80000000ULL,
  0x8000000080000000ULL,
  0xFFFFFFFF80000000ULL,

  0xFFFFFFFF00000001ULL,
  0xFFFFFFFF31415927ULL,
  0xFFFFFFFF7FFFFFFFULL,
  0xFFFFFFFF80000000ULL,
  0xFFFFFFFFFFFFFFFFULL,

  0x00000001FFFFFFFFULL,
  0x31415927FFFFFFFFULL,
  0x7FFFFFFFFFFFFFFFULL,
  0x80000000FFFFFFFFULL,
  0xFFFFFFFFFFFFFFFFULL
};

void do_unary ( char* name, void** fns, int n_fns )
{
   int i, j;
   ULong arg, res, xer;
   void(*fn)(ULong,ULong*,ULong*);
   for (i = 0; i < n_fns; i++) { /* shift */
      for (j = 0; j < N_ARGS64; j++) { /* arg */
         arg = args64[j];
         res = xer = 0;
         fn = fns[i];
         fn( arg, &res, &xer );
         printf("%5s(0x%016llx, %2d) = 0x%016llx, %d\n", 
                name, arg, (int)i, res, (int)((xer >> 29 & 1)));
      }
   }
}

void do_binary ( char* name, void* fnV )
{
   int i, j;
   ULong arg1, arg2, res, xer;
   void(*fn)(ULong,ULong,ULong*,ULong*);
   for (i = 0; i < 64+10; i++) { /* shift */
      for (j = 0; j < N_ARGS64; j++) { /* arg */
         arg1 = args64[j];
         arg2 = i;
         res = xer = 0;
         fn = fnV;
         fn( arg1, arg2, &res, &xer );
         printf("%5s(0x%016llx, %2d) = 0x%016llx, %d\n", 
                name, arg1, (int)arg2, res, (int)((xer >> 29 & 1)));
      }
   }
}

int main ( void )
{
   do_unary("sradi", all_sradi, 64);
   do_unary("srawi", all_srawi, 32);
   do_binary("srad", do_srad);
   do_binary("sraw", do_sraw);
   do_binary("srd",  do_srd);
   do_binary("srw",  do_srw);
   do_binary("sld",  do_sld);
   do_binary("slw",  do_slw);
   return 0;
}

/*
0
1
2
3
4
5
6
7
8
9
10
11
12
13
14
15
16
17
18
19
20
21
22
23
24
25
26
27
28
29
30
31
32
33
34
35
36
37
38
39
40
41
42
43
44
45
46
47
48
49
50
51
52
53
54
55
56
57
58
59
60
61
62
63
*/

