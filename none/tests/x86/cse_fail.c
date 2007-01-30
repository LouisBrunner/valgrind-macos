
/* This isn't really an x86 specific test.  It checks for an
   iropt CSE failure that appeared in 3.2.2:

ex: the `impossible' happened:
   eqIRConst
vex storage: T total 68337344 bytes allocated

valgrind: the 'impossible' happened:
   LibVEX called failure_exit().
==23986==    at 0x38017803: report_and_quit (m_libcassert.c:136)
==23986==    by 0x38017941: panic (m_libcassert.c:210)
==23986==    by 0x38017997: vgPlain_core_panic_at (m_libcassert.c:215)
==23986==    by 0x380179B5: vgPlain_core_panic (m_libcassert.c:220)
==23986==    by 0x3802A650: failure_exit (m_translate.c:487)
==23986==    by 0x38071678: vpanic (vex_util.c:225)
==23986==    by 0x3806A1B0: eqIRConst (irdefs.c:2576)
==23986==    by 0x3810BB84: do_cse_BB (iropt.c:2279)
==23986==    by 0x3810CBBD: do_iropt_BB (iropt.c:4208)
==23986==    by 0x3807010B: LibVEX_Translate (vex_main.c:478)
==23986==    by 0x38029365: vgPlain_translate (m_translate.c:1097)
==23986==    by 0x38037610: vgPlain_scheduler (scheduler.c:693)
==23986==    by 0x38052FBE: run_a_thread_NORETURN (syswrap-linux.c:87)
*/

#include <stdio.h>

int main ( void )
{
   /* This bombs 3.2.2 w/ V128 non-match in eqIRConst. */
   printf("V128 cse:\n");
   __asm__ __volatile__(
      "xorps  %%xmm0,%%xmm0\n\t"
      "movaps %%xmm1,%%xmm2\n\t"
      "addps  %%xmm0,%%xmm1\n\t"
      "addps  %%xmm0,%%xmm2\n\t"
      "addps  %%xmm1,%%xmm2\n\t"
      : : : "xmm0","xmm1", "st"
   );

   /* This ought to cause it to fail w/ F64i non-match in eqIRConst,
      but it doesn't.  I don't understand why not. */
   printf("F64i cse:\n");
   __asm__ __volatile__(
      "fninit\n\t"

      "fldz\n\t"
      "fldz\n\t"
      "fstp %%st(4)\n\t"
      "fstp %%st(3)\n\t"

      "fldpi\n\t"
      "fldpi\n\t"
      "fsqrt\n\t"
      "fxch %%st(1)\n\t"
      "fsqrt\n\t"
      "faddp %%st(1)\n\t"

      : : : "st"
      );
   return 0;
}
