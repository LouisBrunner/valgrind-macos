#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#ifdef HAS_ISA_2_06

#include <string.h>
#include <malloc.h>
#include <altivec.h>

#define NUM_ARGS 14
#define MAX_RESULTS  NUM_ARGS
#define START_i 0
#define STOP_i  14

#define START_j 0
#define STOP_j  14

#define START_k  0
#define STOP_k  14

union convert_t {
   unsigned int u;
   float f;
} convert;

#define PRINT_FLOAT 0
#define PRINT_HEX 1

void print_vector_elements(vector unsigned long src) {
   unsigned int element0, element1, element2, element3;

   element0 = (unsigned int)((src[0] >> 32) & 0xFFFFFFFF);
   element1 = (unsigned int)(src[0] & 0xFFFFFFFF);
   element2 = (unsigned int)((src[1] >> 32) & 0xFFFFFFFF);
   element3 = (unsigned int)(src[1] & 0xFFFFFFFF);

   convert.u = element0;

#if PRINT_FLOAT
   printf(" %8.4e,", convert.f);
#endif
#if PRINT_HEX
   printf(" 0x%08x,", convert.u);
#endif

   convert.u = element1;
#if PRINT_FLOAT
   printf(" %8.4e,", convert.f);
#endif
#if PRINT_HEX
   printf(" 0x%08x,", convert.u);
#endif

   convert.u = element2;
#if PRINT_FLOAT
   printf(" %8.4e,", convert.f);
#endif
#if PRINT_HEX
   printf(" 0x%08x,", convert.u);
#endif

   convert.u = element3;
#if PRINT_FLOAT
   printf(" %8.4e", convert.f);
#endif
#if PRINT_HEX
   printf(" 0x%08x", convert.u);
#endif
   printf("\n");
}

void do_tests( void ) {

   vector unsigned long srcA, srcB, srcC, arg_list[NUM_ARGS], dst;
   int i, j, k;

   srcA[0] = 0;  // initialize to remove compiler warnings
   srcA[1] = 0;
   srcB[0] = 0;  // initialize to remove compiler warnings
   srcB[1] = 0;
   srcC[0] = 0;  // initialize to remove compiler warnings
   srcC[1] = 0;

   arg_list[0][0] = 0x8000012480000124ULL;
   arg_list[0][1] = 0x8000012480000124ULL;

   arg_list[1][0] = 0x8000012480000124ULL;
   arg_list[1][1] = 0x8000012480000124ULL;

   arg_list[2][0] = 0x000000060000000CULL;
   arg_list[2][1] = 0x000000080000000AULL;

   arg_list[3][0] = 0xFFFFFAFAFFFFFBFBULL;
   arg_list[3][1] = 0xFFFFFEFCFFFFFDFDULL;

   arg_list[4][0] = 0x0000024800000258ULL;
   arg_list[4][1] = 0x0000026800000278ULL;

   arg_list[5][0] = 0x0000000200000002ULL;
   arg_list[5][1] = 0x0000000400000008ULL;

   arg_list[6][0] = 0xC3000000C3020000ULL;
   arg_list[6][1] = 0xC2FE0000C2FD0000ULL;

   arg_list[7][0] = 0xc3000000c3020000ULL;
   arg_list[7][1] = 0xc2fe0000c2fd0000ULL;

   arg_list[8][0] = 0xC2FE0000C2FD0000ULL;
   arg_list[8][1] = 0x0000000400000002ULL;

   arg_list[9][0] = 0xC2FE0000C2FD0000ULL;
   arg_list[9][1] = 0x7E9676997E700022ULL;

   arg_list[10][0] = 0x80B0000180B00002ULL;
   arg_list[10][1] = 0x80B0000480B00006ULL;

   arg_list[11][0] = 0x80B0000080B00001ULL;
   arg_list[11][1] = 0x80B0000280B00004ULL;

   arg_list[12][0] = 0x4E8000004E800000ULL;
   arg_list[12][1] = 0x4E8000004E800000ULL;

   arg_list[13][0] = 0xB0000000B0000000ULL;
   arg_list[13][1] = 0xB0000000B0000000ULL;

   for ( i = START_i; i < STOP_i; i++) {
     dst[0] = 0xFFFFFFFFFFFFFFFF;
     dst[1] = 0xFFFFFFFFFFFFFFFF;

     srcA[0] = arg_list[i][0];
     srcA[1] = arg_list[i][1];

     printf ("srcA   = 0x%016lx 0x%016lx\n\n", (unsigned long)srcA[1],
             (unsigned long) srcA[0]);
     __asm__ __volatile__ ("vcfsx %0,%1,31" : "=v" (dst): "v" (srcA));
     printf ("   vcfsx(srcA) result = ");
     print_vector_elements(dst);

     __asm__ __volatile__ ("vcfux %0,%1,31" : "=v" (dst): "v" (srcA));
     printf ("   vcfux(srcA) result = ");
     print_vector_elements(dst);

     __asm__ __volatile__ ("vrfim %0,%1" : "=v" (dst): "v" (srcA));
     printf ("   vrfim(srcA) result = ");
     print_vector_elements(dst);

     __asm__ __volatile__ ("vrfin %0,%1" : "=v" (dst): "v" (srcA));
     printf ("   vrfin(srcA) result = ");
     print_vector_elements(dst);

     __asm__ __volatile__ ("vrfiz %0,%1" : "=v" (dst): "v" (srcA));
     printf ("   vrfiz(srcA) result = ");
     print_vector_elements(dst);


     __asm__ __volatile__ ("vexptefp %0,%1" : "=v" (dst): "v" (srcA));
     printf ("   vexptefp(srcA) result = ");
     print_vector_elements(dst);

     /* Smallest representable floating point input does not generate
        a subnormal result.  */
     __asm__ __volatile__ ("vlogefp %0,%1" : "=v" (dst): "v" (srcA));
     printf ("   vlogefp(srcA) result = ");
     print_vector_elements(dst);

     __asm__ __volatile__ ("vrefp %0,%1" : "=v" (dst): "v" (srcA));
     printf ("   vrefp(srcA) result = ");

     print_vector_elements(dst);

     /* Square root of the smallest representable number is a normal
        number. Square root can't generate a subnormal result.  */
     __asm__ __volatile__ ("vrsqrtefp %0,%1" : "=v" (dst): "v" (srcA));
     printf ("   vrsqrtefp(srcA) result   = 0x%016lx 0x%016lx\n\n",
             (unsigned long) dst[1], (unsigned long) dst[0]);

     for ( j = START_j; j < STOP_j; j++) {
       srcB[0] = arg_list[j][0];
       srcB[1] = arg_list[j][1];
       dst[0] = 0xFFFFFFFFFFFFFFFF;
       dst[1] = 0xFFFFFFFFFFFFFFFF;

       printf ("srcB   = 0x%016lx 0x%016lx\n\n", (unsigned long) srcB[1],
               (unsigned long) srcB[0]);
       __asm__ __volatile__ ("vaddfp %0,%1,%2" : "=v" (dst): "v" (srcA), "v" (srcB));
       printf ("    vaddfp(srcA,srcB) result = ");
       print_vector_elements(dst);

       __asm__ __volatile__ ("vsubfp %0,%1,%2" : "=v" (dst): "v" (srcA), "v" (srcB));
       printf ("    vsubfp(srcA,srcB) result = ");
       print_vector_elements(dst);

       __asm__ __volatile__ ("vmaxfp %0,%1,%2" : "=v" (dst): "v" (srcA), "v" (srcB));
       printf ("    vmax(srcA,srcB) result = ");
       print_vector_elements(dst);

       __asm__ __volatile__ ("vminfp %0,%1,%2" : "=v" (dst): "v" (srcA), "v" (srcB));
       printf ("    vmin(srcA,srcB) result = ");
       print_vector_elements(dst);

       __asm__ __volatile__ ("vcmpbfp %0,%1,%2" : "=v" (dst): "v" (srcA), "v" (srcB));
       printf ("    vcmpbfp(srcA,srcB) result = ");
       print_vector_elements(dst);

       __asm__ __volatile__ ("vcmpeqfp %0,%1,%2" : "=v" (dst): "v" (srcA), "v" (srcB));
       printf ("    vcmpeqfp(srcA,srcB) result = ");
       print_vector_elements(dst);

       __asm__ __volatile__ ("vcmpgefp %0,%1,%2" : "=v" (dst): "v" (srcA), "v" (srcB));
       printf ("    vcmpgefp(srcA,srcB) result = ");
       print_vector_elements(dst);

       __asm__ __volatile__ ("vcmpgtfp %0,%1,%2" : "=v" (dst): "v" (srcA), "v" (srcB));
       printf ("    vcmpgtfp(srcA,srcB) result = ");
       print_vector_elements(dst);


       for ( k = START_k; k < STOP_k; k++) {
	 srcC[0] = arg_list[k][0];
	 srcC[1] = arg_list[k][1];

	 dst[0] = 0xFFFFFFFFFFFFFFFF;
	 dst[1] = 0xFFFFFFFFFFFFFFFF;

	 printf ("srcC   = 0x%016lx 0x%016lx\n\n", (unsigned long) srcC[1],
                 (unsigned long) srcC[0]);

	 __asm__ __volatile__ ("vmaddfp %0,%1,%2,%3" : "=v" (dst): "v" (srcA), "v" (srcC), "v" (srcB));
	 printf("i=%d, j=%d, k=%d  ", i, j, k);
	 printf ("    vmaddfp(srcA,srcC,srcB) result = ");
         print_vector_elements(dst);

	 __asm__ __volatile__ ("vnmsubfp %0,%1,%2,%3" : "=v" (dst): "v" (srcA), "v" (srcC), "v" (srcB));
	 printf("i=%d, j=%d, k=%d  ", i, j, k);
	 printf ("    vnmsubfp(srcA,srcC,srcB) result = ");
         print_vector_elements(dst);
       }
     }
   }
   return;
}
#endif

int main()
{
#ifdef HAS_ISA_2_06
  register vector unsigned long vreg, initial_vscr_value;

  /* The VSCR[NJ] bit controlls how subnormal (denormal) results are handled
   * by the various vector float instructions.
   *
   * If VSCR[NJ] = 0 then the subnormal result is returned.
   * If VSCR[NJ] = 1 then the subnormal result is set to zero the sign is
   *                 not changed.
   *
   * This tests verifies the VSCR[NJ] functionality for the vector float
   * instructions.
   */

  /* Save the VSCR setting and restore it at the end.  Don't want to screw
   * up other tests with a different setting of the VSCR.  */
   __asm__ __volatile__ ("mfvscr %0"  : "=v"(initial_vscr_value));

   // set VSCR[nj] to 0 and run test
   printf("Attempt to set VSR[NJ] to 0, run test\n");

   vreg[0] = 0;
   vreg[1] = 0;
   __asm__ __volatile__ ("mtvscr %0"  :: "v"(vreg));

   // Read VSCR
   __asm__ __volatile__ ("mfvscr %0"  : "=v"(vreg));

#ifdef VGP_ppc64le_linux
   printf("vscr set to 0x%lx\n", (unsigned long) vreg[0]);
#else
   printf("vscr set to 0x%lx\n", (unsigned long) vreg[1]);
#endif

   do_tests();

   // set VSCR[nj] to 1 and run test
   printf("Attempt to set VSR[NJ] to 1, run test\n");

   /* The upper 95 bits are set to zero when writting to the register.  So,
      I can get away with setting the NJ bit for BE and LE and the HW will
      take care of clearing the one I don't want.
   */
   vreg[0] = 1 << (127 - 111);   // Sets bit for LE case
   vreg[1] = 1 << (127 - 111);   // Sets bit for BE case

   __asm__ __volatile__ ("mtvscr %0"  :: "v"(vreg));

   vreg[0] = 0;
   vreg[1] = 0;

   // Read VSCR
   __asm__ __volatile__ ("mfvscr %0"  : "=v"(vreg));

#ifdef VGP_ppc64le_linux
   printf("vscr set to 0x%lx\n", (unsigned long) vreg[0]);
#else
   printf("vscr set to 0x%lx\n", (unsigned long) vreg[1]);
#endif

   do_tests();

   /* Restore the VSCR settings.  */
   __asm__ __volatile__ ("mtvscr %0"  :: "v"(initial_vscr_value));
#else
   printf("No ISA 2.06 support\n");
#endif

   return 0;
}
