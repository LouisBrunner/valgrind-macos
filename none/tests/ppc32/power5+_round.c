/*  Copyright (C) 2007 IBM

    Author: Pete Eberlein  eberlein@us.ibm.com

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, see <http://www.gnu.org/licenses/>.

    The GNU General Public License is contained in the file COPYING.
*/



#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#define POS_NORMAL   0x4000
#define NEG_NORMAL   0x8000
#define POS_INF      0x5000
#define NEG_INF      0x9000
#define POS_ZERO     0x2000
#define NEG_ZERO     0x12000
#define POS_DENORMAL 0x14000
#define NEG_DENORMAL 0x18000
#define NAN          0x11000
#define FPRF_MASK    0x1F000


int main(int argc, char *argv[])
{

   double inf, neg0, nan;
   union {
      double d;
      struct {
         unsigned int dummy, dummy2:15, fprf:17;
      };
   } fpscr;

   inf = strtod("inf", NULL);
   neg0 = strtod("-0", NULL);
   nan = strtod("nan", NULL);


   /* This set is disabled until fprf is implemented. */
   if (0) {
      double set[] = { inf, 1.5, 0, neg0, -1.5, -inf, nan };
      int i, j, fprf;
      for (i = 0; i < 7; ++i) {
         for (j = 0; j < 7; ++j) {
          asm("fcmpu 1, %1, %2\n\t" "mffs %0\n":"=f"(fpscr.d)
          :    "f"(set[i]), "f"(set[j])
                );

            if (i == 6 || j == 6) {
               fprf = 0x1000;   // Unordered
            } else if (i == j || (i == 2 && j == 3) || (i == 3 && j == 2)) {
               fprf = 0x2000;   // Equal
            } else if (i < j) {
               fprf = 0x4000;   // Greater Than
            } else if (i > j) {
               fprf = 0x8000;   // Less Than
            }

            printf("fcmpu\t%.1f\t%.1f\t%x\t%s\n", set[i], set[j],
                   fpscr.fprf, fpscr.fprf == fprf ? "PASS" : "FAIL");
         }
      }
   }

   {
      double set[] = { inf, 1.9, 1.1, 0, neg0, -1.1, -1.9, -inf, nan };
      double frin[] = { inf, 2.0, 1.0, 0, neg0, -1.0, -2.0, -inf, nan };
      double friz[] = { inf, 1.0, 1.0, 0, neg0, -1.0, -1.0, -inf, nan };
      double frip[] = { inf, 2.0, 2.0, 0, neg0, -1.0, -1.0, -inf, nan };
      double frim[] = { inf, 1.0, 1.0, 0, neg0, -2.0, -2.0, -inf, nan };
      double set2[] = { 0.9, 0.1, -0.1, -0.9, 1e-40, -1e-40 };
      double frin2[] = { 1.0, 0.0, -0.0, -1.0, 0.0, -0.0 };
      double friz2[] = { 0.0, 0.0, -0.0, -0.0, 0.0, -0.0 };
      double frip2[] = { 1.0, 1.0, -0.0, -0.0, 1.0, -0.0 };
      double frim2[] = { 0.0, 0.0, -1.0, -1.0, 0.0, -1.0 };
      double ret;
      int i;

#define DO_TEST(op,in,out,rf)  for (i=0; i<sizeof(in)/sizeof(double); ++i) { \
    asm (#op" %0, %2\n\t" \
       "mffs %1\n" \
    : "=f" (ret), "=f" (fpscr.d) \
    : "f" (in[i]) \
    ); \
    printf(#op"\t%g\t%g\t%x\t%s\n", in[i], ret, fpscr.fprf, \
           (!bcmp(&ret, &out[i], sizeof(double))) /*&& (rf[i] == fpscr.fprf)*/ \
	   ? "PASS" : "FAIL"); \
  }
      /* Note: fprf check above is disabled until fprf is implemented. */


      DO_TEST(frin, set, frin, fprf);
      DO_TEST(frin, set2, frin2, frin2rf);
      DO_TEST(friz, set, friz, fprf);
      DO_TEST(friz, set2, friz2, friz2rf);
      DO_TEST(frip, set, frip, fprf);
      DO_TEST(frip, set2, frip2, frip2rf);
      DO_TEST(frim, set, frim, fprf);
      DO_TEST(frim, set2, frim2, frim2rf);
   }

   /* This set is disabled until fprf is implemented. */
   if (0) {
      double set1[] = { inf, 0.9, 0.1, 0, neg0, -0.1, -0.9, -inf, nan };
      double frsp1[] =
          { inf, 0.9f, 0.1f, 0, neg0, -0.1f, -0.9f, -inf, nan };
      double set2[] =
          { 1.2e-38, 1.1e-38, 1e-40, 8e-44, 9e-44, 8e-46, 7e-46 };
      double frsp2[] =
          { 1.2e-38f, 1.1e-38f, 1e-40f, 8e-44f, 9e-44f, 8e-46f, 0.0 };
      double set3[] =
          { -1.2e-38, -1.1e-38, -1e-40, -8e-44, -9e-44, -8e-46, -7e-46 };
      double frsp3[] =
          { -1.2e-38f, -1.1e-38f, -1e-40f, -8e-44f, -9e-44f, -8e-46f,
-0.0 };
      double ret;
      int i;
      DO_TEST(frsp, set1, frsp1, fprf1);
      DO_TEST(frsp, set2, frsp2, fprf2);
      DO_TEST(frsp, set3, frsp3, fprf3);
   }


   return 0;
}
