/*  Copyright (C) 2012 IBM

 Author: Maynard Johnson <maynardj@us.ibm.com>

 This program is free software; you can redistribute it and/or
 modify it under the terms of the GNU General Public License as
 published by the Free Software Foundation; either version 2 of the
 License, or (at your option) any later version.

 This program is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 02111-1307, USA.

 The GNU General Public License is contained in the file COPYING.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#if defined(HAS_DFP)

register double f14 __asm__ ("fr14");
register double f15 __asm__ ("fr15");
register double f16 __asm__ ("fr16");
register double f17 __asm__ ("fr17");
register double f18 __asm__ ("fr18");
register double f19 __asm__ ("fr19");


typedef unsigned char Bool;
#define True 1
#define False 0


#define ALLCR "cr0","cr1","cr2","cr3","cr4","cr5","cr6","cr7"

#define SET_CR(_arg) \
      __asm__ __volatile__ ("mtcr  %0" : : "b"(_arg) : ALLCR );

#define SET_XER(_arg) \
      __asm__ __volatile__ ("mtxer %0" : : "b"(_arg) : "xer" );

#define GET_CR(_lval) \
      __asm__ __volatile__ ("mfcr %0"  : "=b"(_lval) )

#define GET_XER(_lval) \
      __asm__ __volatile__ ("mfxer %0" : "=b"(_lval) )

#define GET_CR_XER(_lval_cr,_lval_xer) \
   do { GET_CR(_lval_cr); GET_XER(_lval_xer); } while (0)

#define SET_CR_ZERO \
      SET_CR(0)

#define SET_XER_ZERO \
      SET_XER(0)

#define SET_CR_XER_ZERO \
   do { SET_CR_ZERO; SET_XER_ZERO; } while (0)

#define SET_FPSCR_ZERO \
   do { double _d = 0.0; \
        __asm__ __volatile__ ("mtfsf 0xFF, %0" : : "f"(_d) ); \
   } while (0)

#define GET_FPSCR(_arg) \
    __asm__ __volatile__ ("mffs %0"  : "=f"(_arg) )

#define SET_FPSCR_DRN \
    __asm__ __volatile__ ("mtfsf  1, %0, 0, 1" :  : "f"(f14) )


// The assembly-level instructions being tested
static void _test_drintx(int R, int RMC)
{
   if (RMC < 0 || RMC > 3 || R < 0 || R > 1) {
      fprintf(stderr, "Invalid inputs to asm test: a=%d, b=%d\n", R, RMC);
      return;
   }
   switch (RMC) {
      case 0:
         if (R)
            __asm__ __volatile__ ("drintx 1, %0, %1, 0" : "=f" (f18) : "f" (f16));
         else
            __asm__ __volatile__ ("drintx 0, %0, %1, 0" : "=f" (f18) : "f" (f16));
         break;
      case 1:
         if (R)
            __asm__ __volatile__ ("drintx 1, %0, %1, 1" : "=f" (f18) : "f" (f16));
         else
            __asm__ __volatile__ ("drintx 0, %0, %1, 1" : "=f" (f18) : "f" (f16));
         break;
      case 2:
         if (R)
            __asm__ __volatile__ ("drintx 1, %0, %1, 2" : "=f" (f18) : "f" (f16));
         else
            __asm__ __volatile__ ("drintx 0, %0, %1, 2" : "=f" (f18) : "f" (f16));
         break;
      case 3:
         if (R)
            __asm__ __volatile__ ("drintx 1, %0, %1, 3" : "=f" (f18) : "f" (f16));
         else
            __asm__ __volatile__ ("drintx 0, %0, %1, 3" : "=f" (f18) : "f" (f16));
         break;
      default:
         break;
   }
}

static void _test_drintn(int R, int RMC)
{
   if (RMC < 0 || RMC > 3 || R < 0 || R > 1) {
      fprintf(stderr, "Invalid inputs to asm test: a=%d, b=%d\n", R, RMC);
      return;
   }
   switch (RMC) {
      case 0:
         if (R)
            __asm__ __volatile__ ("drintn 1, %0, %1, 0" : "=f" (f18) : "f" (f16));
         else
            __asm__ __volatile__ ("drintn 0, %0, %1, 0" : "=f" (f18) : "f" (f16));
         break;
      case 1:
         if (R)
            __asm__ __volatile__ ("drintn 1, %0, %1, 1" : "=f" (f18) : "f" (f16));
         else
            __asm__ __volatile__ ("drintn 0, %0, %1, 1" : "=f" (f18) : "f" (f16));
         break;
      case 2:
         if (R)
            __asm__ __volatile__ ("drintn 1, %0, %1, 2" : "=f" (f18) : "f" (f16));
         else
            __asm__ __volatile__ ("drintn 0, %0, %1, 2" : "=f" (f18) : "f" (f16));
         break;
      case 3:
         if (R)
            __asm__ __volatile__ ("drintn 1, %0, %1, 3" : "=f" (f18) : "f" (f16));
         else
            __asm__ __volatile__ ("drintn 0, %0, %1, 3" : "=f" (f18) : "f" (f16));
         break;
      default:
         break;
   }
}


static void _test_diex(int a __attribute__((unused)), int b __attribute__((unused)))
{
   __asm__ __volatile__ ("diex  %0, %1, %2" : "=f" (f18) : "f" (f14),"f" (f16));
}

static void _test_dxex(int a __attribute__((unused)), int b __attribute__((unused)))
{
   __asm__ __volatile__ ("dxex  %0, %1" : "=f" (f18) : "f" (f16));
}

static void _test_dcmpo(int BF, int x __attribute__((unused)))
{
   if (BF < 0 || BF > 7) {
      fprintf(stderr, "Invalid input to asm test: a=%d\n", BF);
      return;
   }
   switch (BF) {
      case 0:
         __asm__ __volatile__ ("dcmpo  0, %0, %1" :  : "f" (f14),"f" (f16));
         break;
      case 1:
         __asm__ __volatile__ ("dcmpo  1, %0, %1" :  : "f" (f14),"f" (f16));
         break;
      case 2:
         __asm__ __volatile__ ("dcmpo  2, %0, %1" :  : "f" (f14),"f" (f16));
         break;
      case 3:
         __asm__ __volatile__ ("dcmpo  3, %0, %1" :  : "f" (f14),"f" (f16));
         break;
      case 4:
         __asm__ __volatile__ ("dcmpo  4, %0, %1" :  : "f" (f14),"f" (f16));
         break;
      case 5:
         __asm__ __volatile__ ("dcmpo  5, %0, %1" :  : "f" (f14),"f" (f16));
         break;
      case 6:
         __asm__ __volatile__ ("dcmpo  6, %0, %1" :  : "f" (f14),"f" (f16));
         break;
      case 7:
         __asm__ __volatile__ ("dcmpo  7, %0, %1" :  : "f" (f14),"f" (f16));
         break;
      default:
         break;
   }
}

static void _test_dcmpu(int BF, int x __attribute__((unused)))
{
   if (BF < 0 || BF > 7) {
      fprintf(stderr, "Invalid input to asm test: a=%d\n", BF);
      return;
   }
   switch (BF) {
      case 0:
         __asm__ __volatile__ ("dcmpu  0, %0, %1" :  : "f" (f14),"f" (f16));
         break;
      case 1:
         __asm__ __volatile__ ("dcmpu  1, %0, %1" :  : "f" (f14),"f" (f16));
         break;
      case 2:
         __asm__ __volatile__ ("dcmpu  2, %0, %1" :  : "f" (f14),"f" (f16));
         break;
      case 3:
         __asm__ __volatile__ ("dcmpu  3, %0, %1" :  : "f" (f14),"f" (f16));
         break;
      case 4:
         __asm__ __volatile__ ("dcmpu  4, %0, %1" :  : "f" (f14),"f" (f16));
         break;
      case 5:
         __asm__ __volatile__ ("dcmpu  5, %0, %1" :  : "f" (f14),"f" (f16));
         break;
      case 6:
         __asm__ __volatile__ ("dcmpu  6, %0, %1" :  : "f" (f14),"f" (f16));
         break;
      case 7:
         __asm__ __volatile__ ("dcmpu  7, %0, %1" :  : "f" (f14),"f" (f16));
         break;
      default:
         break;
   }
}

// Quad instruction testing
static void _test_drintxq(int R, int RMC)
{
   if (RMC < 0 || RMC > 3 || R < 0 || R > 1) {
      fprintf(stderr, "Invalid inputs to asm test: a=%d, b=%d\n", R, RMC);
      return;
   }
   switch (RMC) {
      case 0:
         if (R)
            __asm__ __volatile__ ("drintxq 1, %0, %1, 0" : "=f" (f18) : "f" (f16));
         else
            __asm__ __volatile__ ("drintxq 0, %0, %1, 0" : "=f" (f18) : "f" (f16));
         break;
      case 1:
         if (R)
            __asm__ __volatile__ ("drintxq 1, %0, %1, 1" : "=f" (f18) : "f" (f16));
         else
            __asm__ __volatile__ ("drintxq 0, %0, %1, 1" : "=f" (f18) : "f" (f16));
         break;
      case 2:
         if (R)
            __asm__ __volatile__ ("drintxq 1, %0, %1, 2" : "=f" (f18) : "f" (f16));
         else
            __asm__ __volatile__ ("drintxq 0, %0, %1, 2" : "=f" (f18) : "f" (f16));
         break;
      case 3:
         if (R)
            __asm__ __volatile__ ("drintxq 1, %0, %1, 3" : "=f" (f18) : "f" (f16));
         else
            __asm__ __volatile__ ("drintxq 0, %0, %1, 3" : "=f" (f18) : "f" (f16));
         break;
      default:
         break;
   }
}

static void _test_drintnq(int R, int RMC)
{
   if (RMC < 0 || RMC > 3 || R < 0 || R > 1) {
      fprintf(stderr, "Invalid inputs to asm test: a=%d, b=%d\n", R, RMC);
      return;
   }
   switch (RMC) {
      case 0:
         if (R)
            __asm__ __volatile__ ("drintnq 1, %0, %1, 0" : "=f" (f18) : "f" (f16));
         else
            __asm__ __volatile__ ("drintnq 0, %0, %1, 0" : "=f" (f18) : "f" (f16));
         break;
      case 1:
         if (R)
            __asm__ __volatile__ ("drintnq 1, %0, %1, 1" : "=f" (f18) : "f" (f16));
         else
            __asm__ __volatile__ ("drintnq 0, %0, %1, 1" : "=f" (f18) : "f" (f16));
         break;
      case 2:
         if (R)
            __asm__ __volatile__ ("drintnq 1, %0, %1, 2" : "=f" (f18) : "f" (f16));
         else
            __asm__ __volatile__ ("drintnq 0, %0, %1, 2" : "=f" (f18) : "f" (f16));
         break;
      case 3:
         if (R)
            __asm__ __volatile__ ("drintnq 1, %0, %1, 3" : "=f" (f18) : "f" (f16));
         else
            __asm__ __volatile__ ("drintnq 0, %0, %1, 3" : "=f" (f18) : "f" (f16));
         break;
      default:
         break;
   }
}

static void _test_diexq(int a __attribute__((unused)), int b __attribute__((unused)))
{
   __asm__ __volatile__ ("diexq  %0, %1, %2" : "=f" (f18) : "f" (f14),"f" (f16));
}

static void _test_dxexq(int a __attribute__((unused)), int b __attribute__((unused)))
{
   __asm__ __volatile__ ("dxexq  %0, %1" : "=f" (f18) : "f" (f16));
}

static void _test_dcmpoq(int BF, int x __attribute__((unused)))
{
   if (BF < 0 || BF > 7) {
      fprintf(stderr, "Invalid input to asm test: a=%d\n", BF );
      return;
   }
   switch (BF) {
      case 0:
         __asm__ __volatile__ ("dcmpoq  0, %0, %1" :  : "f" (f14),"f" (f16));
         break;
      case 1:
         __asm__ __volatile__ ("dcmpoq  1, %0, %1" :  : "f" (f14),"f" (f16));
         break;
      case 2:
         __asm__ __volatile__ ("dcmpoq  2, %0, %1" :  : "f" (f14),"f" (f16));
         break;
      case 3:
         __asm__ __volatile__ ("dcmpoq  3, %0, %1" :  : "f" (f14),"f" (f16));
         break;
      case 4:
         __asm__ __volatile__ ("dcmpoq  4, %0, %1" :  : "f" (f14),"f" (f16));
         break;
      case 5:
         __asm__ __volatile__ ("dcmpoq  5, %0, %1" :  : "f" (f14),"f" (f16));
         break;
      case 6:
         __asm__ __volatile__ ("dcmpoq  6, %0, %1" :  : "f" (f14),"f" (f16));
         break;
      case 7:
         __asm__ __volatile__ ("dcmpoq  7, %0, %1" :  : "f" (f14),"f" (f16));
         break;
      default:
         break;
   }
}

static void _test_dcmpuq(int BF, int x __attribute__((unused)))
{
   if (BF < 0 || BF > 7) {
      fprintf(stderr, "Invalid input to asm test: a=%d\n", BF);
      return;
   }
   switch (BF) {
      case 0:
         __asm__ __volatile__ ("dcmpuq  0, %0, %1" :  : "f" (f14),"f" (f16));
         break;
      case 1:
         __asm__ __volatile__ ("dcmpuq  1, %0, %1" :  : "f" (f14),"f" (f16));
         break;
      case 2:
         __asm__ __volatile__ ("dcmpuq  2, %0, %1" :  : "f" (f14),"f" (f16));
         break;
      case 3:
         __asm__ __volatile__ ("dcmpuq  3, %0, %1" :  : "f" (f14),"f" (f16));
         break;
      case 4:
         __asm__ __volatile__ ("dcmpuq  4, %0, %1" :  : "f" (f14),"f" (f16));
         break;
      case 5:
         __asm__ __volatile__ ("dcmpuq  5, %0, %1" :  : "f" (f14),"f" (f16));
         break;
      case 6:
         __asm__ __volatile__ ("dcmpuq  6, %0, %1" :  : "f" (f14),"f" (f16));
         break;
      case 7:
         __asm__ __volatile__ ("dcmpuq  7, %0, %1" :  : "f" (f14),"f" (f16));
         break;
      default:
         break;
   }
}

static void _test_drrnd(int x __attribute__((unused)), int RMC)
{
   if (RMC < 0 || RMC > 31) {
      fprintf(stderr, "Invalid input to asm test: a=%d\n", RMC);
      return;
   }
   switch (RMC) {
      case 0:
         __asm__ __volatile__ ("drrnd %0, %1, %2, 0" : "=f" (f18) : "f" (f14), "f" (f16));
         break;
      case 1:
         __asm__ __volatile__ ("drrnd %0, %1, %2, 1" : "=f" (f18) : "f" (f14), "f" (f16));
         break;
      case 2:
         __asm__ __volatile__ ("drrnd %0, %1, %2, 2" : "=f" (f18) : "f" (f14), "f" (f16));
         break;
      case 3:
         __asm__ __volatile__ ("drrnd %0, %1, %2, 3" : "=f" (f18) : "f" (f14), "f" (f16));
         break;
      default:
         break;
   }
}

static void _test_drrndq(int x __attribute__((unused)), int RMC)
{
   if (RMC < 0 || RMC > 3) {
      fprintf(stderr, "Invalid input to asm test: a=%dn", RMC);
      return;
   }
   switch (RMC) {
      case 0:
         __asm__ __volatile__ ("drrndq %0, %1, %2, 0" : "=f" (f18) : "f" (f14), "f" (f16));
         break;
      case 1:
         __asm__ __volatile__ ("drrndq %0, %1, %2, 1" : "=f" (f18) : "f" (f14), "f" (f16));
         break;
      case 2:
         __asm__ __volatile__ ("drrndq %0, %1, %2, 2" : "=f" (f18) : "f" (f14), "f" (f16));
         break;
      case 3:
         __asm__ __volatile__ ("drrndq %0, %1, %2, 3" : "=f" (f18) : "f" (f14), "f" (f16));
         break;
      default:
         break;
   }
}

static void _test_dqua(int x __attribute__((unused)), int RMC)
{
   if (RMC < 0 || RMC > 3) {
      fprintf(stderr, "Invalid input to asm test: a=%d\n", RMC);
      return;
   }
   switch (RMC) {
      case 0:
         __asm__ __volatile__ ("dqua %0, %1, %2, 0" : "=f" (f18) : "f" (f14), "f" (f16));
         break;
      case 1:
         __asm__ __volatile__ ("dqua %0, %1, %2, 1" : "=f" (f18) : "f" (f14), "f" (f16));
         break;
      case 2:
         __asm__ __volatile__ ("dqua %0, %1, %2, 2" : "=f" (f18) : "f" (f14), "f" (f16));
         break;
      case 3:
         __asm__ __volatile__ ("dqua %0, %1, %2, 3" : "=f" (f18) : "f" (f14), "f" (f16));
         break;
      default:
         break;
   }
}

static void _test_dquaq(int x __attribute__((unused)), int RMC)
{
   if (RMC < 0 || RMC > 3) {
      fprintf(stderr, "Invalid input to asm test: a=%d\n", RMC);
      return;
   }
   switch (RMC) {
      case 0:
         __asm__ __volatile__ ("dquaq %0, %1, %2, 0" : "=f" (f18) : "f" (f14), "f" (f16));
         break;
      case 1:
         __asm__ __volatile__ ("dquaq %0, %1, %2, 1" : "=f" (f18) : "f" (f14), "f" (f16));
         break;
      case 2:
         __asm__ __volatile__ ("dquaq %0, %1, %2, 2" : "=f" (f18) : "f" (f14), "f" (f16));
         break;
      case 3:
         __asm__ __volatile__ ("dquaq %0, %1, %2, 3" : "=f" (f18) : "f" (f14), "f" (f16));
         break;
      default:
         break;
   }
}

static int TE_vals[] = { -16, -2, 0, 5};
#define TE_VAL_LEN sizeof(TE_vals)/sizeof(int)
static Bool __is_TE_val(int x)
{
   int i;
   for (i = 0; i < TE_VAL_LEN; i++) {
      if (x==TE_vals[i])
         return True;
   }
   return False;
}

static void _test_dquai(int TE, int RMC)
{
   if (RMC < 0 || RMC > 3 || !__is_TE_val(TE)) {
      fprintf(stderr, "Invalid inputs to asm test: a=%d, b=%d\n", TE, RMC);
      return;
   }
   switch (RMC) {
      case 0:
         switch (TE) {
            case -16:
               __asm__ __volatile__ ("dquai -16, %0, %1, 0" : "=f" (f18) : "f" (f16));
               break;
            case -2:
               __asm__ __volatile__ ("dquai  -2, %0, %1, 0" : "=f" (f18) : "f" (f16));
               break;
            case 0:
               __asm__ __volatile__ ("dquai   0, %0, %1, 0" : "=f" (f18) : "f" (f16));
               break;
            case 5:
               __asm__ __volatile__ ("dquai   5, %0, %1, 0" : "=f" (f18) : "f" (f16));
               break;
            default:
               break;
         }
         break;
      case 1:
         switch (TE) {
            case -16:
               __asm__ __volatile__ ("dquai -16, %0, %1, 1" : "=f" (f18) : "f" (f16));
               break;
            case -2:
               __asm__ __volatile__ ("dquai  -2, %0, %1, 1" : "=f" (f18) : "f" (f16));
               break;
            case 0:
               __asm__ __volatile__ ("dquai   0, %0, %1, 1" : "=f" (f18) : "f" (f16));
               break;
            case 5:
               __asm__ __volatile__ ("dquai   5, %0, %1, 1" : "=f" (f18) : "f" (f16));
               break;
            default:
               break;
         }
         break;
      case 2:
         switch (TE) {
            case -16:
               __asm__ __volatile__ ("dquai -16, %0, %1, 2" : "=f" (f18) : "f" (f16));
               break;
            case -2:
               __asm__ __volatile__ ("dquai  -2, %0, %1, 2" : "=f" (f18) : "f" (f16));
               break;
            case 0:
               __asm__ __volatile__ ("dquai   0, %0, %1, 2" : "=f" (f18) : "f" (f16));
               break;
            case 5:
               __asm__ __volatile__ ("dquai   5, %0, %1, 2" : "=f" (f18) : "f" (f16));
               break;
            default:
               break;
         }
         break;
      case 3:
         switch (TE) {
            case -16:
               __asm__ __volatile__ ("dquai -16, %0, %1, 3" : "=f" (f18) : "f" (f16));
               break;
            case -2:
               __asm__ __volatile__ ("dquai  -2, %0, %1, 3" : "=f" (f18) : "f" (f16));
               break;
            case 0:
               __asm__ __volatile__ ("dquai   0, %0, %1, 3" : "=f" (f18) : "f" (f16));
               break;
            case 5:
               __asm__ __volatile__ ("dquai   5, %0, %1, 3" : "=f" (f18) : "f" (f16));
               break;
            default:
               break;
         }
         break;
      default:
         break;
   }
}

static void _test_dquaiq(int TE, int RMC)
{
   if (RMC < 0 || RMC > 3 || !__is_TE_val(TE)) {
      fprintf(stderr, "Invalid inputs to asm test: a=%d, b=%d\n", TE, RMC);
      return;
   }
   switch (RMC) {
      case 0:
         switch (TE) {
            case -16:
               __asm__ __volatile__ ("dquaiq -16, %0, %1, 0" : "=f" (f18) : "f" (f16));
               break;
            case -2:
               __asm__ __volatile__ ("dquaiq  -2, %0, %1, 0" : "=f" (f18) : "f" (f16));
               break;
            case 0:
               __asm__ __volatile__ ("dquaiq   0, %0, %1, 0" : "=f" (f18) : "f" (f16));
               break;
            case 5:
               __asm__ __volatile__ ("dquaiq   5, %0, %1, 0" : "=f" (f18) : "f" (f16));
               break;
            default:
               break;
         }
         break;
      case 1:
         switch (TE) {
            case -16:
               __asm__ __volatile__ ("dquaiq -16, %0, %1, 1" : "=f" (f18) : "f" (f16));
               break;
            case -2:
               __asm__ __volatile__ ("dquaiq  -2, %0, %1, 1" : "=f" (f18) : "f" (f16));
               break;
            case 0:
               __asm__ __volatile__ ("dquaiq   0, %0, %1, 1" : "=f" (f18) : "f" (f16));
               break;
            case 5:
               __asm__ __volatile__ ("dquaiq   5, %0, %1, 1" : "=f" (f18) : "f" (f16));
               break;
            default:
               break;
         }
         break;
      case 2:
         switch (TE) {
            case -16:
               __asm__ __volatile__ ("dquaiq -16, %0, %1, 2" : "=f" (f18) : "f" (f16));
               break;
            case -2:
               __asm__ __volatile__ ("dquaiq  -2, %0, %1, 2" : "=f" (f18) : "f" (f16));
               break;
            case 0:
               __asm__ __volatile__ ("dquaiq   0, %0, %1, 2" : "=f" (f18) : "f" (f16));
               break;
            case 5:
               __asm__ __volatile__ ("dquaiq   5, %0, %1, 2" : "=f" (f18) : "f" (f16));
               break;
            default:
               break;
         }
         break;
      case 3:
         switch (TE) {
            case -16:
               __asm__ __volatile__ ("dquaiq -16, %0, %1, 3" : "=f" (f18) : "f" (f16));
               break;
            case -2:
               __asm__ __volatile__ ("dquaiq  -2, %0, %1, 3" : "=f" (f18) : "f" (f16));
               break;
            case 0:
               __asm__ __volatile__ ("dquaiq   0, %0, %1, 3" : "=f" (f18) : "f" (f16));
               break;
            case 5:
               __asm__ __volatile__ ("dquaiq   5, %0, %1, 3" : "=f" (f18) : "f" (f16));
               break;
            default:
               break;
         }
         break;
      default:
         break;
   }
}


typedef void (*test_func_t)(int a, int b);
typedef void (*test_driver_func_t)(void);
typedef struct test_table
{
   test_driver_func_t test_category;
   char * name;
} test_table_t;

/*
 *  345.0DD (0x2207c00000000000 0xe50)
 *  1.2300e+5DD (0x2207c00000000000 0x14c000)
 *  -16.0DD (0xa207c00000000000 0xe0)
 *  0.00189DD (0x2206c00000000000 0xcf)
 *  -4.1235DD (0xa205c00000000000 0x10a395bcf)
 *  9.8399e+20DD (0x2209400000000000 0x253f1f534acdd4)
 *  0DD (0x2208000000000000 0x0)
 *  0DD (0x2208000000000000 0x0)
 *  infDD (0x7800000000000000 0x0)
 *  nanDD (0x7c00000000000000 0x0
 */
static unsigned long long dfp128_vals[] = {
                                    // Some finite numbers
                                    0x2207c00000000000ULL, 0x0000000000000e50ULL,
                                    0x2207c00000000000ULL, 0x000000000014c000ULL,
                                    0xa207c00000000000ULL, 0x00000000000000e0ULL,
                                    0x2206c00000000000ULL, 0x00000000000000cfULL,
                                    0xa205c00000000000ULL, 0x000000010a395bcfULL,
                                    0x6209400000fd0000ULL, 0x00253f1f534acdd4ULL, // huge number
                                    0x000400000089b000ULL, 0x0a6000d000000049ULL, // very small number
                                    // flavors of zero
                                    0x2208000000000000ULL, 0x0000000000000000ULL,
                                    0xa208000000000000ULL, 0x0000000000000000ULL, // negative
                                    0xa248000000000000ULL, 0x0000000000000000ULL,
                                    // flavors of NAN
                                    0x7c00000000000000ULL, 0x0000000000000000ULL, // quiet
                                    0xfc00000000000000ULL, 0xc00100035b007700ULL,
                                    0x7e00000000000000ULL, 0xfe000000d0e0a0d0ULL, // signaling
                                    // flavors of Infinity
                                    0x7800000000000000ULL, 0x0000000000000000ULL,
                                    0xf800000000000000ULL, 0x0000000000000000ULL, // negative
                                    0xf900000000000000ULL, 0x0000000000000000ULL
};

static unsigned long long dfp64_vals[] = {
                                 // various finite numbers
                                 0x2234000000000e50ULL,
                                 0x223400000014c000ULL,
                                 0xa2340000000000e0ULL,// negative
                                 0x22240000000000cfULL,
                                 0xa21400010a395bcfULL,// negative
                                 0x6e4d3f1f534acdd4ULL,// huge number
                                 0x000400000089b000ULL,// very small number
                                 // flavors of zero
                                 0x2238000000000000ULL,
                                 0xa238000000000000ULL,
                                 0x4248000000000000ULL,
                                 // flavors of NAN
                                 0x7e34000000000111ULL,
                                 0xfe000000d0e0a0d0ULL,//signaling
                                 0xfc00000000000000ULL,//quiet
                                 // flavors of Infinity
                                 0x7800000000000000ULL,
                                 0xf800000000000000ULL,//negative
                                 0x7a34000000000000ULL,
};

// Both Long and Quad arrays of DFP values should have the same length.
// If that length is changed, t
#define NUM_DFP_VALS (sizeof(dfp64_vals)/8)

typedef struct dfp_test_args {
   int fra_idx;
   int frb_idx;
} dfp_test_args_t;


// Index pairs from dfp64_vals array to be used with dfp_two_arg_tests
static dfp_test_args_t dfp_2args_x1[] = {
                                    {0, 1},
                                    {2, 1},
                                    {3, 4},
                                    {0, 6},
                                    {2, 4},
                                    {5, 1},
                                    {5, 2},
                                    {7, 1},
                                    {7, 2},
                                    {8, 0},
                                    {8, 1},
                                    {8, 2},
                                    {7, 8},
                                    {12, 14},
                                    {12, 1},
                                    {12, 13},
                                    {12, 12},
                                    {12, 11},
                                    {11, 14},
                                    {11, 0},
                                    {11, 13},
                                    {11, 11},
                                    {14, 14},
                                    {14, 3},
                                    {14, 15},
};

typedef enum {
   LONG_TEST,
   QUAD_TEST
} precision_type_t;

typedef struct dfp_test
{
   test_func_t test_func;
   const char * name;
   dfp_test_args_t * targs;
   int num_tests;
   precision_type_t precision;
   const char * op;
} dfp_test_t;

typedef struct dfp_one_arg_test
{
   test_func_t test_func;
   const char * name;
   precision_type_t precision;
   const char * op;
} dfp_one_arg_test_t;


static dfp_one_arg_test_t
dfp_quai_tests[] = {
                    { &_test_dquai, "dquai", LONG_TEST, "[QI]"},
                    { &_test_dquaiq, "dquaiq", QUAD_TEST, "[QI]"},
                    { NULL, NULL, 0, NULL}
};

static void test_dfp_quai_ops(void)
{
   test_func_t func;
   unsigned long long u0, u0x;
   double res, d0, *d0p, d0x, *d0xp;

   int k = 0;
   u0 = u0x = 0;
   d0p = &d0;
   d0xp = &d0x;

   while ((func = dfp_quai_tests[k].test_func)) {
      int i;
      dfp_one_arg_test_t test_def = dfp_quai_tests[k];

      for (i = 0; i < NUM_DFP_VALS; i++) {
         int TE, RMC;

         if (test_def.precision == LONG_TEST) {
            u0 = dfp64_vals[i];
         } else {
            u0 = dfp128_vals[i * 2];
            u0x = dfp128_vals[(i * 2) + 1];
         }
         *(unsigned long long *)d0p = u0;
         f16 = d0;
         if (test_def.precision == QUAD_TEST) {
            *(unsigned long long *)d0xp = u0x;
            f17 = d0x;
         }

         for (TE = 0; TE < TE_VAL_LEN; TE++) {
            for (RMC = 0; RMC < 4; RMC++) {
               (*func)(TE_vals[TE], RMC);
               res = f18;
               printf("%s (RMC=%2d, TE=%3d) %s %016llx", test_def.name, RMC,
                      TE_vals[TE], test_def.op, u0);
               if (test_def.precision == LONG_TEST) {
                  printf(" => %016llx\n",
                         *((unsigned long long *)(&res)));
               } else {
                  double resx = f19;
                  printf(" %016llx ==> %016llx %016llx\n",
                         u0x, *((unsigned long long *)(&res)), *((unsigned long long *)(&resx)));
               }
            }
         }
      }
      k++;
      printf( "\n" );
   }
}


static dfp_test_t
dfp_qua_tests[] = {
                   { &_test_dqua, "dqua", dfp_2args_x1, 25, LONG_TEST, "[Q]"},
                   { &_test_dquaq, "dquaq", dfp_2args_x1, 25, QUAD_TEST, "[Q]"},
                   { NULL, NULL, NULL, 0, 0, NULL}
};

static void test_dfp_qua_ops(void)
{
   test_func_t func;
   unsigned long long u0, u0x, u1, u1x;
   double res, d0, d1, *d0p, *d1p;
   double d0x, d1x, *d0xp, *d1xp;
   int k = 0;
   u0x = u1x = 0;
   d0p = &d0;
   d0xp = &d0x;
   d1p = &d1;
   d1xp = &d1x;

   while ((func = dfp_qua_tests[k].test_func)) {
      int i, RMC;
      dfp_test_t test_def = dfp_qua_tests[k];

      for (i = 0; i < test_def.num_tests; i++) {
         if (test_def.precision == LONG_TEST) {
            u0 = dfp64_vals[test_def.targs[i].fra_idx];
            u1 = dfp64_vals[test_def.targs[i].frb_idx];
         } else {
            u0 = dfp128_vals[test_def.targs[i].fra_idx * 2];
            u0x = dfp128_vals[(test_def.targs[i].fra_idx * 2) + 1];
            u1 = dfp128_vals[test_def.targs[i].frb_idx * 2];
            u1x = dfp128_vals[(test_def.targs[i].frb_idx * 2) + 1];
         }
         *(unsigned long long *)d0p = u0;
         *(unsigned long long *)d1p = u1;
         f14 = d0;
         f16 = d1;
         if (test_def.precision == QUAD_TEST) {
            *(unsigned long long *)d0xp = u0x;
            *(unsigned long long *)d1xp = u1x;
            f15 = d0x;
            f17 = d1x;
         }
         for (RMC = 0; RMC < 4; RMC++) {
            (*func)(-1, RMC);
            res = f18;
            printf("%s (RMC=%2d) %s %016llx", test_def.name, RMC, test_def.op, u0);
            if (test_def.precision == LONG_TEST) {
               printf(", %016llx => %016llx\n", u1, *((unsigned long long *)(&res)));
            } else {
               double resx = f19;
               printf(" %016llx, %016llx %016llx ==> %016llx %016llx\n",u0x, u1, u1x,
                      *((unsigned long long *)(&res)), *((unsigned long long *)(&resx)));
            }
         }
      }
      k++;
      printf( "\n" );
   }
}


static dfp_one_arg_test_t
dfp_rrnd_tests[] = {
                    { &_test_drrnd, "drrnd", LONG_TEST, "[RR]"},
                    { &_test_drrndq, "drrndq", QUAD_TEST, "[RR]"},
                    { NULL, NULL, 0, NULL}
};

static void test_dfp_rrnd_ops(void)
{
   test_func_t func;
   unsigned long long u0, u0x;
   double res, d0, *d0p, d0x, *d0xp, reference_sig, *reference_sig_p;
   long long reference_sig_vals[] = {0ULL, 2ULL, 6ULL, 63ULL};
   int num_reference_sig_vals = sizeof(reference_sig_vals)/sizeof(long long);

   int k = 0;
   u0 = u0x = 0;
   d0p = &d0;
   d0xp = &d0x;
   reference_sig_p = &reference_sig;

   while ((func = dfp_rrnd_tests[k].test_func)) {
      int i, j;
      dfp_one_arg_test_t test_def = dfp_rrnd_tests[k];

      for (i = 0; i < NUM_DFP_VALS; i++) {
         int RMC;

         if (test_def.precision == LONG_TEST) {
            u0 = dfp64_vals[i];
         } else {
            u0 = dfp128_vals[i * 2];
            u0x = dfp128_vals[(i * 2) + 1];
         }
         *(unsigned long long *)d0p = u0;
         f16 = d0;
         if (test_def.precision == QUAD_TEST) {
            *(unsigned long long *)d0xp = u0x;
            f17 = d0x;
         }

         for (j = 0; j < num_reference_sig_vals; j++) {
            *(long long *)reference_sig_p = reference_sig_vals[j];
            f14 = reference_sig;
            for (RMC = 0; RMC < 4; RMC++) {
               (*func)(-1, RMC);
               res = f18;
               printf("%s (RMC=%d, ref sig=%d) %s%016llx", test_def.name, RMC,
                      (int)reference_sig_vals[j], test_def.op, u0);
               if (test_def.precision == LONG_TEST) {
                  printf(" => %016llx\n",
                         *((unsigned long long *)(&res)));
               } else {
                  double resx = f19;
                  printf(" %016llx ==> %016llx %016llx\n",
                         u0x, *((unsigned long long *)(&res)), *((unsigned long long *)(&resx)));
               }
            }
         }
      }
      k++;
      printf( "\n" );
   }
}


static dfp_one_arg_test_t
dfp_xiex_tests[] = {
                       { &_test_diex, "diex", LONG_TEST, ">>"},
                       { &_test_diexq, "diexq", QUAD_TEST, ">>"},
                       { &_test_dxex, "dxex", LONG_TEST, "<<"},
                       { &_test_dxexq, "dxexq", QUAD_TEST, "<<"},
                       { NULL, NULL, 0, NULL}
};

static void test_dfp_xiex_ops(void)
{
   test_func_t func;
   unsigned long long u0, u0x;
   double res, d0, *d0p, d0x, *d0xp, target_exp, *target_exp_p;
   /* The first two positions are placeholders and will be filled in later,
    * based on the precision of the DFP argument.
    */
   long long target_exp_vals[] = {0ULL, 0ULL, 0ULL, -1ULL, -2ULL, -3ULL, -4ULL, -5ULL};
   int num_exp_vals = sizeof(target_exp_vals)/sizeof(long long);
   int k = 0;
   u0 = u0x = 0;
   d0p = &d0;
   d0xp = &d0x;
   target_exp_p = &target_exp;

   while ((func = dfp_xiex_tests[k].test_func)) {
      int i;
      Bool insert_insn = False;
      dfp_one_arg_test_t test_def = dfp_xiex_tests[k];

      if (!strncmp(test_def.name, "di", 2))
         insert_insn = True;

      if (test_def.precision == QUAD_TEST) {
         target_exp_vals[0] = 12288ULL; // > max biased exponent
         target_exp_vals[1] = 5235ULL;
      } else {
         target_exp_vals[0] = 768ULL; // > max biased exponent
         target_exp_vals[1] = 355ULL;
      }

      for (i = 0; i < NUM_DFP_VALS; i++) {
         unsigned int j;

         if (test_def.precision == QUAD_TEST) {
            u0 = dfp128_vals[i * 2];
            u0x = dfp128_vals[(i * 2) + 1];
         } else {
            u0 = dfp64_vals[i];
         }
         *(unsigned long long *)d0p = u0;
         f16 = d0;
         if (test_def.precision == QUAD_TEST) {
            *(unsigned long long *)d0xp = u0x;
            f17 = d0x;
         }

         if (!insert_insn) {
            // This is just for extract insns (dexex[q])
            (*func)(0, 0);
            res = f18;
            printf("%s %s ", test_def.name, test_def.op);
            if (test_def.precision == LONG_TEST) {
               printf("%016llx => %016llx\n", u0,
                      *((unsigned long long *)(&res)));
            } else {
               double resx = f19;
               printf("%016llx %016llx ==> %016llx %016llx\n", u0, u0x,
                      *((unsigned long long *)(&res)), *((unsigned long long *)(&resx)));
            }
            continue;
         }
         // The following for-loop is just for insert insns (diex[q])
         for (j = 0; j < num_exp_vals; j++) {
            *(long long *)target_exp_p = target_exp_vals[j];
            f14 = target_exp;
            (*func)(0, 0);
            res = f18;
            printf("%s %s %5d, ", test_def.name, test_def.op, (int)target_exp_vals[j]);

            if (test_def.precision == LONG_TEST) {
               printf("%016llx => %016llx\n", u0,
                      *((unsigned long long *)(&res)));
            } else {
               double resx = f19;
               printf("%016llx %016llx ==> %016llx %016llx\n", u0, u0x,
                      *((unsigned long long *)(&res)), *((unsigned long long *)(&resx)));
            }
         }
      }
      k++;
      printf( "\n" );
   }
}

static dfp_one_arg_test_t
dfp_rint_tests[] = {
                    { &_test_drintn, "drintn", LONG_TEST, "~"},
                    { &_test_drintnq, "drintnq", QUAD_TEST, "~"},
                    { &_test_drintx, "drintx", LONG_TEST, "~"},
                    { &_test_drintxq, "drintxq", QUAD_TEST, "~"},
                    { NULL, NULL, 0, NULL}
};

static void test_dfp_rint_ops(void)
{
   test_func_t func;
   unsigned long long u0, u0x;
   double res, d0, *d0p, d0x, *d0xp;
   int k = 0;
   u0 = u0x = 0;
   d0p = &d0;
   d0xp = &d0x;

   while ((func = dfp_rint_tests[k].test_func)) {
      int i;
      dfp_one_arg_test_t test_def = dfp_rint_tests[k];

      for (i = 0; i < NUM_DFP_VALS; i++) {
         int R, RMC;

         if (test_def.precision == LONG_TEST) {
            u0 = dfp64_vals[i];
         } else {
            u0 = dfp128_vals[i * 2];
            u0x = dfp128_vals[(i * 2) + 1];
         }
         *(unsigned long long *)d0p = u0;
         f16 = d0;
         if (test_def.precision == QUAD_TEST) {
            *(unsigned long long *)d0xp = u0x;
            f17 = d0x;
         }

         for (R = 0; R < 2; R++) {
            for (RMC = 0; RMC < 4; RMC++) {
               (*func)(R, RMC);
               res = f18;
               printf("%s (RM=%d) %s%016llx", test_def.name, (RMC + (R << 2)), test_def.op, u0);
               if (test_def.precision == LONG_TEST) {
                  printf(" => %016llx\n",
                         *((unsigned long long *)(&res)));
               } else {
                  double resx = f19;
                  printf(" %016llx ==> %016llx %016llx\n",
                         u0x, *((unsigned long long *)(&res)), *((unsigned long long *)(&resx)));
               }
            }
         }
      }
      k++;
      printf( "\n" );
   }
}

static dfp_test_t
dfp_cmp_tests[] = {
                     { &_test_dcmpo, "dcmpo", dfp_2args_x1, 25, LONG_TEST, "<>"},
                     { &_test_dcmpoq, "dcmpoq", dfp_2args_x1, 25, QUAD_TEST, "<>"},
                     { &_test_dcmpu, "dcmpu", dfp_2args_x1, 25, LONG_TEST, "<>"},
                     { &_test_dcmpuq, "dcmpuq", dfp_2args_x1, 25, QUAD_TEST, "<>"},
                     { NULL, NULL, NULL, 0, 0, NULL}
};

static void test_dfp_cmp_ops(void)
{
   test_func_t func;
   unsigned long long u0, u0x, u1, u1x;
   double d0, d1, *d0p, *d1p;
   double d0x, d1x, *d0xp, *d1xp;
   /* BF is a 3-bit instruction field that indicates the CR field in which the
    * result of the compare should be placed.  We won't iterate through all
    * 8 possible BF values since storing compare results to a given field is
    * a well-tested mechanism in VEX.  But we will test two BF values, just as
    * a sniff-test.
    */
   int k = 0, BF;
   u0x = u1x = 0;
   d0p = &d0;
   d0xp = &d0x;
   d1p = &d1;
   d1xp = &d1x;

   while ((func = dfp_cmp_tests[k].test_func)) {
      int i, repeat = 1;
      dfp_test_t test_def = dfp_cmp_tests[k];
      BF = 0;

again:
      for (i = 0; i < test_def.num_tests; i++) {
         unsigned int condreg;
         unsigned int flags;

         if (test_def.precision == LONG_TEST) {
            u0 = dfp64_vals[test_def.targs[i].fra_idx];
            u1 = dfp64_vals[test_def.targs[i].frb_idx];
         } else {
            u0 = dfp128_vals[test_def.targs[i].fra_idx * 2];
            u0x = dfp128_vals[(test_def.targs[i].fra_idx * 2) + 1];
            u1 = dfp128_vals[test_def.targs[i].frb_idx * 2];
            u1x = dfp128_vals[(test_def.targs[i].frb_idx * 2) + 1];
         }
         *(unsigned long long *)d0p = u0;
         *(unsigned long long *)d1p = u1;
         f14 = d0;
         f16 = d1;
         if (test_def.precision == QUAD_TEST) {
            *(unsigned long long *)d0xp = u0x;
            *(unsigned long long *)d1xp = u1x;
            f15 = d0x;
            f17 = d1x;
         }

         SET_FPSCR_ZERO;
         SET_CR_XER_ZERO;
         (*func)(BF, 0);
         GET_CR(flags);

         condreg = ((flags >> (4 * (7-BF)))) & 0xf;
         printf("%s %016llx", test_def.name, u0);
         if (test_def.precision == LONG_TEST) {
            printf(" %s %016llx => %x (BF=%d)\n",
                   test_def.op, u1, condreg, BF);
         } else {
            printf(" %016llx %s %016llx %016llx ==> %x (BF=%d)\n",
                   u0x, test_def.op, u1, u1x,
                   condreg, BF);
         }
      }
      if (repeat) {
         repeat = 0;
         BF = 5;
         goto again;
      }
      k++;
      printf( "\n" );
   }
}


static test_table_t
         all_tests[] =
{
                    { &test_dfp_cmp_ops,
                      "Test DFP compare instructions"},
                    { &test_dfp_rint_ops,
                      "Test DFP round instructions"},
                    { &test_dfp_xiex_ops,
                      "Test DFP insert/extract instructions"},
                    { &test_dfp_rrnd_ops,
                      "Test DFP reround instructions"},
                    { &test_dfp_qua_ops,
                      "Test DFP quantize instructions"},
                    { &test_dfp_quai_ops,
                      "Test DFP quantize immediate instructions"},
                    { NULL, NULL }
};
#endif // HAS_DFP

int main() {
#if defined(HAS_DFP)

   test_table_t aTest;
   test_driver_func_t func;
   int i = 0;

   while ((func = all_tests[i].test_category)) {
      aTest = all_tests[i];
      printf( "%s\n", aTest.name );
      (*func)();
      i++;
   }

#endif // HAS_DFP
   return 0;
}
