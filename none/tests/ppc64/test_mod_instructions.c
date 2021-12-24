#include <stdio.h>

#ifdef HAS_ISA_3_00
long test_modsd( long srcA, long srcB)
{
  long dst;
  __asm__ __volatile__ ("modsd %0,%1,%2" : "=r" (dst): "r" (srcA), "r" (srcB));

   return dst;
}

unsigned long test_modud( unsigned long srcA, unsigned long srcB)
{
  unsigned long dst;
  __asm__ __volatile__ ("modud %0,%1,%2" : "=r" (dst): "r" (srcA), "r" (srcB));

   return dst;
}

int test_modsw( int srcA, int srcB)
{
  int dst;
  __asm__ __volatile__ ("modsw %0,%1,%2" : "=r" (dst): "r" (srcA), "r" (srcB));

   return dst;
}

unsigned test_moduw( unsigned srcA, unsigned srcB)
{
  unsigned dst;
  __asm__ __volatile__ ("moduw %0,%1,%2" : "=r" (dst): "r" (srcA), "r" (srcB));

   return dst;
}
#endif

int main()
{
#ifdef HAS_ISA_3_00
   int srcA_si, srcB_si, dst_si;
   unsigned int srcA_ui, srcB_ui, dst_ui;
   long srcA_sl, srcB_sl, dst_sl;
   unsigned long srcA_ul, srcB_ul, dst_ul;
   int i_si, j_si;
   long int i_sl, j_sl;
   int i_ui, j_ui;
   long int i_ul, j_ul;

#define SI_NEGATIVE_START  0x80000000
#define SI_NEGATIVE_STOP   0x80000008
#define SI_POSITIVE_START  0x7FFFFFF8
#define SI_POSITIVE_STOP   0x7FFFFFFF
#define SI_ZERO_START      -5
#define SI_ZERO_STOP       5

#define DI_NEGATIVE_START  0x8000000000000000LL
#define DI_NEGATIVE_STOP   0x8000000000000008LL
#define DI_POSITIVE_START  0x7FFFFFFFFFFFFFF8LL
#define DI_POSITIVE_STOP   0x7FFFFFFFFFFFFFFFLL
#define DI_ZERO_START      -5
#define DI_ZERO_STOP       5

#define UI_START           0xFFFFFFF8
#define UI_STOP            0xFFFFFFFF
#define UI_ZERO_START      0
#define UI_ZERO_STOP       10

#define UL_START           0xFFFFFFFFFFFFFFF8ULL
#define UL_STOP            0xFFFFFFFFFFFFFFFFULL
#define UL_ZERO_START      0
#define UL_ZERO_STOP       10

   /* Signed tests need to check the most negative values,
      the most positive values and values around zero.  */

   /* Signed integer tests */
   for (i_si = SI_NEGATIVE_START; i_si < SI_NEGATIVE_STOP; i_si++)
     for (j_si = SI_NEGATIVE_START; j_si < SI_NEGATIVE_STOP; j_si++)
       {
	 srcA_si = i_si;
	 srcB_si = j_si;

	 dst_si = test_modsw( srcA_si, srcB_si);
#ifdef debug
	 printf("srcA_si = %d \n", srcA_si);
	 printf("srcB_si = %d \n", srcB_si);
	 printf ("modsw result = %d\n\n", dst_si);
#else
	 printf ("modsw result = %d\n", dst_si);
#endif	 
       }

   for (i_si = SI_ZERO_START; i_si < SI_ZERO_STOP; i_si++)
     for (j_si = SI_ZERO_START; j_si < SI_ZERO_STOP; j_si++)
       {
	 srcA_si = i_si;
	 srcB_si = j_si;

	 dst_si = test_modsw( srcA_si, srcB_si);

#ifdef debug
	 printf("srcA_si = %d \n", srcA_si);
	 printf("srcB_si = %d \n", srcB_si);
	 printf ("modsw result = %d\n\n", dst_si);
#else
	 printf ("modsw result = %d\n", dst_si);
#endif
       }

   for (i_si = SI_POSITIVE_START; i_si < SI_POSITIVE_STOP; i_si++)
     for (j_si = SI_POSITIVE_START; j_si < SI_POSITIVE_STOP; j_si++)
       {
	 srcA_si = i_si;
	 srcB_si = j_si;

	 dst_si = test_modsw( srcA_si, srcB_si);

#ifdef debug
	 printf("srcA_si = %d \n", srcA_si);
	 printf("srcB_si = %d \n", srcB_si);
	 printf ("modsw result = %d\n\n", dst_si);
#else
	 printf ("modsw result = %d\n", dst_si);
#endif
       }

   /* Signed long integer tests */
   for (i_sl = DI_NEGATIVE_START; i_sl < DI_NEGATIVE_STOP; i_sl++)
     for (j_sl = DI_NEGATIVE_START; j_sl < DI_NEGATIVE_STOP; j_sl++)
       {
	 srcA_sl = i_sl;
	 srcB_sl = j_sl;

	 dst_sl = test_modsd( srcA_sl, srcB_sl);

#ifdef debug
	 printf("srcA_sl = %ld \n", srcA_sl);
	 printf("srcB_sl = %ld \n", srcB_sl);
	 printf ("modsd result = %ld\n\n", dst_sl);
#else
	 printf ("modsd result = %ld\n", dst_sl);
#endif
       }

   for (i_sl = DI_ZERO_START; i_sl < DI_ZERO_STOP; i_sl++)
     for (j_sl = DI_ZERO_START; j_sl < DI_ZERO_STOP; j_sl++)
       {
	 srcA_sl = i_sl;
	 srcB_sl = j_sl;

	 dst_sl = test_modsd( srcA_sl, srcB_sl);

#ifdef debug
	 printf("srcA_sl = %ld \n", srcA_sl);
	 printf("srcB_sl = %ld \n", srcB_sl);
	 printf ("modsd result = %ld\n\n", dst_sl);
#else
	 printf ("modsd result = %ld\n", dst_sl);
#endif
       }

   for (i_sl = DI_POSITIVE_START; i_sl < DI_POSITIVE_STOP; i_sl++)
     for (j_sl = DI_POSITIVE_START; j_sl < DI_POSITIVE_STOP; j_sl++)
       {
	 srcA_sl = i_sl;
	 srcB_sl = j_sl;

	 dst_sl = test_modsd( srcA_sl, srcB_sl);

#ifdef debug
	 printf("srcA_sl = %ld \n", srcA_sl);
	 printf("srcB_sl = %ld \n", srcB_sl);
	 printf ("modsd result = %ld\n\n", dst_sl);
#else
	 printf ("modsd result = %ld\n", dst_sl);
#endif
       }

   /* Unsigned tests need to check the most positive values
      and the values around zero.  */

   /* Unsigned integer tests */
   for (i_ui = UI_START; i_ui < UI_STOP; i_ui++)
     for (j_ui = UI_START; j_ui < UI_STOP; j_ui++)
       {
	 srcA_ui = i_ui;
	 srcB_ui = j_ui;

	 dst_ui = test_moduw( srcA_ui, srcB_ui);

#ifdef debug
	 printf("srcA_ui = %u \n", srcA_ui);
	 printf("srcB_ui = %u \n", srcB_ui);
	 printf ("moduw result = %u\n\n", dst_ui);
#else
	 printf ("moduw result = %u\n", dst_ui);
#endif
       }

   for (i_ui = SI_ZERO_START; i_ui < SI_ZERO_STOP; i_ui++)
     for (j_ui = SI_ZERO_START; j_ui < SI_ZERO_STOP; j_ui++)
       {
	 srcA_ui = i_ui;
	 srcB_ui = j_ui;

	 dst_ui = test_moduw( srcA_ui, srcB_ui);

#ifdef debug
	 printf("srcA_ui = %u \n", srcA_ui);
	 printf("srcB_ui = %u \n", srcB_ui);
	 printf ("moduw result = %u\n", dst_ui);
#else
	 printf ("moduw result = %u\n\n", dst_ui);
#endif
       }

   /* Unsigned long integer tests */
   for (i_ul = UL_START; i_ul < UL_STOP; i_ul++)
     for (j_ul = UL_START; j_ul < UL_STOP; j_ul++)
       {
	 srcA_ul = i_ul;
	 srcB_ul = j_ul;

	 dst_ul = test_modud( srcA_ul, srcB_ul);

#ifdef debug
	 printf("srcA_ul = %lu \n", srcA_ul);
	 printf("srcB_ul = %lu \n", srcB_ul);
	 printf ("modud result = %lu\n\n", dst_ul);
#else
	 printf ("modud result = %lu\n", dst_ul);
#endif
       }

   for (i_ul = UL_ZERO_START; i_ul < UL_ZERO_STOP; i_ul++)
     for (j_ul = UL_ZERO_START; j_ul < UL_ZERO_STOP; j_ul++)
       {
	 srcA_ul = i_ul;
	 srcB_ul = j_ul;

	 dst_ul = test_modud( srcA_ul, srcB_ul);

#ifdef debug
	 printf("srcA_ul = %lu \n", srcA_ul);
	 printf("srcB_ul = %lu \n", srcB_ul);
	 printf ("modud result = %lu\n\n", dst_ul);
#else
	 printf ("modud result = %lu\n", dst_ul);
#endif
       }
#else
  printf("HAS_ISA_3_00 not detected.\n");
#endif  // HAS_ISA_3_0
   return 0;
}

