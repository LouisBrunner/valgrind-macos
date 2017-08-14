
/* Testing framework, for developing code to copy vex's x87 simulation
   state to and from a real x87 state image (the 108-byte thing). 

   Includes code from fp_80_64.c.
*/

#include "../pub/libvex_basictypes.h"
#include "../pub/libvex_ir.h"
#include "../priv/guest-x86/gdefs.h"
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>

/* Get definitions of convert_f64le_to_f80le and
   convert_f80le_to_f64le. */
#define USED_AS_INCLUDE
#include "fp_80_64.c"
#undef  USED_AS_INCLUDE


////////////////////////////////////////////////////////////////

/* Layout of the real x87 state. */

typedef
   struct {
      UShort env[14];
      UChar  reg[80];
   }
   Fpu_State;

/* Offsets, in 16-bit ints, into the FPU environment (env) area. */
#define FP_ENV_CTRL   0
#define FP_ENV_STAT   2
#define FP_ENV_TAG    4
#define FP_ENV_IP     6 /* and 7 */
#define FP_ENV_CS     8
#define FP_ENV_OPOFF  10 /* and 11 */
#define FP_ENV_OPSEL  12
#define FP_REG(ii)    (10*(7-(ii)))


/* Layout of vex's FP state is defined in ../priv/guest-x86/gdefs.h */

static void x87_to_vex ( /*IN*/UChar* x87_state, /*OUT*/UChar* vex_state )
{
   Int        r;
   UInt       tag;
   Double*    vexRegs = (Double*)(vex_state + OFFB_F0);
   UChar*     vexTags = (UChar*)(vex_state + OFFB_FTAG0);
   Fpu_State* x87     = (Fpu_State*)x87_state;
   UInt       ftop    = (x87->env[FP_ENV_STAT] >> 11) & 7;
   UInt       tagw    = x87->env[FP_ENV_TAG];

   /* Copy registers and tags */
   for (r = 0; r < 8; r++) {
      tag = (tagw >> (2*r)) & 3;
      if (tag == 3) {
         /* register is empty */
         vexRegs[r] = 0.0;
         vexTags[r] = 0;
      } else {
         /* register is non-empty */
         convert_f80le_to_f64le( &x87->reg[FP_REG(r)], (UChar*)&vexRegs[r] );
         vexTags[r] = 1;
      }
   }

   /* stack pointer */
   *(UInt*)(vex_state + OFFB_FTOP) = ftop;

   /* TODO: Check the CW is 037F.  Or at least, bottom 6 bits are 1
      (all exceptions masked), and 11:10, which is rounding control,
      is set to ..?
   */
}


static void vex_to_x87 ( /*IN*/UChar* vex_state, /*OUT*/UChar* x87_state )
{
   Int        i, r;
   UInt       tagw;
   Double*    vexRegs = (Double*)(vex_state + OFFB_F0);
   UChar*     vexTags = (UChar*)(vex_state + OFFB_FTAG0);
   Fpu_State* x87     = (Fpu_State*)x87_state;
   UInt       ftop    = *(UInt*)(vex_state + OFFB_FTOP);

   for (i = 0; i < 14; i++)
      x87->env[i] = 0;

   x87->env[1] = x87->env[3] = x87->env[5] = x87->env[13] = 0xFFFF;
   x87->env[FP_ENV_CTRL] = 0x037F;
   x87->env[FP_ENV_STAT] = (ftop & 7) << 11;

   tagw = 0;
   for (r = 0; r < 8; r++) {
      if (vexTags[r] == 0) {
         /* register is empty */
         tagw |= (3 << (2*r));
         convert_f64le_to_f80le( (UChar*)&vexRegs[r], &x87->reg[FP_REG(r)] );
      } else {
         /* register is full. */
         tagw |= (0 << (2*r));
         convert_f64le_to_f80le( (UChar*)&vexRegs[r],  &x87->reg[FP_REG(r)] );
      }
   }
   x87->env[FP_ENV_TAG] = tagw;
}

////////////////////////////////////////////////////////////////

// fwds ...
static void printFpuState ( UChar* fpu_state );
static void printVexState ( UChar* vex_state );


/* Capture the FPU state.  Convert it to vex.  Convert it back
   to x87.  Print it at all stages.
*/
void capture_convert_show ( /* preallocated storage */
                            UChar* x87_state0,
                            UChar* x87_state1,
                            UChar* vex_state )
{
   asm volatile ("fsave (%0)"
                 :
                 : "r" (x87_state0)
                 : "memory" );
   x87_to_vex(x87_state0, vex_state);
   vex_to_x87(vex_state, x87_state1);
   printf("\n\n=================================================\n\n");
   printFpuState(x87_state0);
   printf("\n\n");
   printVexState(vex_state);
   printf("\n\n");
#if 0
   asm volatile("frstor (%0) ; fsave (%0)"
                 :
                 : "r" (x87_state1)
                 : "memory" );
#endif
   printFpuState(x87_state1);
   printf("\n\n");
   x87_to_vex(x87_state1, vex_state);
   printVexState(vex_state);
   printf("\n\n");
}

int main ( void )
{
  UChar*  x87_state0 = malloc(sizeof(Fpu_State));
  UChar* x87_state1 = malloc(sizeof(Fpu_State));
  UChar* vex_state = malloc(1000);
  asm volatile ("finit");
  capture_convert_show(x87_state0, x87_state1, vex_state);
  asm volatile ("fldpi");
  capture_convert_show(x87_state0, x87_state1, vex_state);
  asm volatile ("fldz ; fld1 ; fdiv %st(1)");
  asm volatile ("fldln2 ; fldlg2 ; fchs ; fsqrt");
  capture_convert_show(x87_state0, x87_state1, vex_state);
  return 1;
}

////////////////////////////////////////////////////////////////

/* Bitfield offsets for exceptions in the FPU status and control words. */
#define FP_E_INVAL    0
#define FP_E_DENOR    1
#define FP_E_DIVZ     2
#define FP_E_OVERF    3
#define FP_E_UNDER    4
#define FP_E_LOS      5

/* More bitfield offsets, but for the status word only. */
#define FP_E_STACKF   6
#define FP_E_SUMMARY  7
#define FP_F_C0       8
#define FP_F_C1       9
#define FP_F_C2      10
#define FP_F_C3      14
/* top-of-stack ptr is bits 13,12,11 of the word */
#define FP_F_TOS_LO  11
#define FP_F_TOS_HI  13

/* Register tags. */
#define FP_TAG_VALID 0
#define FP_TAG_ZERO  1
#define FP_TAG_SPEC  2
#define FP_TAG_EMPTY 3

char* fp_tag_names[4]
   = { "Valid", "Zero", "Spec", "Empty" };

char* fp_exception_names[6]
   = { "INVAL", "DENOR", "DIVZ", "OVERF", "UNDERF", "LOS" };


UInt fp_get_tos ( Fpu_State* x87 )
{
   return (x87->env[FP_ENV_STAT] >> FP_F_TOS_LO) & 7;
}

UInt fp_get_tag ( Fpu_State* x87, UInt regno )
{
   assert(!(regno < 0 || regno > 7));
   return (x87->env[FP_ENV_TAG] >> (2*regno)) & 3;
}

UInt fp_get_statusword_flag ( Fpu_State* x87, UInt flagno )
{
   assert(!(flagno < 0 || flagno > 15));
   return (x87->env[FP_ENV_STAT] >> flagno) & 0x1;
}

UInt fp_get_controlword_flag ( Fpu_State* x87, UInt flagno )
{
   assert(!(flagno < 0 || flagno > 15));
   return (x87->env[FP_ENV_CTRL] >> flagno) & 0x1;
}


static void printFpuState ( UChar* fpu_state )
{
   Fpu_State* x87 = (Fpu_State*)fpu_state;

   Int i, j, k;
   assert(sizeof(Fpu_State)==108);
   for (i = 7; i >= 0; i--) {
      printf ( " %s fpreg%d: 0x", 
               (UInt)i == fp_get_tos(x87) ? "**" : "  ", i );
      for (k = 0, j = FP_REG(i)+9; k < 10; k++,j--)
         printf ( "%02x", (UInt)x87->reg[j]);
      printf ( "  %5s  ", fp_tag_names[fp_get_tag(x87,i)] );
      printf("\n");
      //printf ( "%20.16e\n", fp_get_reg(i) );
   }
   printf("     fctrl:     0x%04x  masked: ", 
          (UInt)x87->env[FP_ENV_CTRL] );
   for (i = FP_E_INVAL; i <= FP_E_LOS; i++)
      if (fp_get_controlword_flag(x87,i))
         printf ( "%s ", fp_exception_names[i] );
   printf ( "\n" );

   printf("     fstat:     0x%04x  except:", 
          (UInt)x87->env[FP_ENV_STAT] );
   for (i = FP_E_INVAL; i <= FP_E_LOS; i++)
      if (fp_get_statusword_flag(x87,i))
         printf ( "%s ", fp_exception_names[i] );
   printf ( "  top: %d  ", fp_get_tos(x87) );
   printf ( "c3210: %d%d%d%d",
            fp_get_statusword_flag(x87,FP_F_C3),
            fp_get_statusword_flag(x87,FP_F_C2),
            fp_get_statusword_flag(x87,FP_F_C1),
            fp_get_statusword_flag(x87,FP_F_C0) );
   printf ( "  STACKF: %d\n", fp_get_statusword_flag(x87,FP_E_STACKF) );

   printf("      ftag:     0x%04x  ", (UInt)x87->env[FP_ENV_TAG] );
   for (i = 7; i >= 0; i--)
      printf ( "%d:%s ", i, fp_tag_names[fp_get_tag(x87,i)] );
   printf("\n");

   printf("       fip: 0x%08x\n", 
           (((UInt)x87->env[FP_ENV_IP+1]) << 16) |
            ((UInt)x87->env[FP_ENV_IP]) );
   printf("       fcs:     0x%04x\n", 
           ((UInt)x87->env[FP_ENV_CS]) );
   printf("    fopoff: 0x%08x\n", 
           (((UInt)x87->env[FP_ENV_OPOFF+1]) << 16) |
            ((UInt)x87->env[FP_ENV_OPOFF]) );
   printf("    fopsel:     0x%04x\n", 
           ((UInt)x87->env[FP_ENV_OPSEL]) );
}


static void printVexState ( UChar* vex_state )
{
   Int r;
   ULong*     vexRegs = (ULong*)(vex_state + OFFB_F0);
   UChar*     vexTags = (UChar*)(vex_state + OFFB_FTAG0);
   UInt       ftop    = *(UInt*)(vex_state + OFFB_FTOP);

   for (r = 7; r >= 0; r--) {
      printf("%s %%f%d:  0x%llx  %s\n", 
              r == ftop ? "##" : "  ",
              r,
              vexRegs[r], 
	      vexTags[r] == 0 ? "Empty" : "Full" );
   }

}
