
#include <stdio.h>
#include <assert.h>


/* ------------------------------------------------- */

typedef unsigned char          UChar;
typedef unsigned short         UShort;
typedef unsigned int           UInt;
typedef unsigned long long int ULong;

typedef signed char     Char;
typedef short           Short;
typedef int             Int;
typedef long long int   Long;

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

Fpu_State m_fpu_state;



UInt fp_get_tos ( void )
{
   return (m_fpu_state.env[FP_ENV_STAT] >> FP_F_TOS_LO) & 7;
}

UInt fp_get_tag ( UInt regno )
{
   assert(!(regno < 0 || regno > 7));
   return (m_fpu_state.env[FP_ENV_TAG] >> (2*regno)) & 3;
}

UInt fp_get_statusword_flag ( UInt flagno )
{
   assert(!(flagno < 0 || flagno > 15));
   return (m_fpu_state.env[FP_ENV_STAT] >> flagno) & 0x1;
}

UInt fp_get_controlword_flag ( UInt flagno )
{
   assert(!(flagno < 0 || flagno > 15));
   return (m_fpu_state.env[FP_ENV_CTRL] >> flagno) & 0x1;
}

static 
void printFpuState ( void )
{
   Int i, j, k;
   assert(sizeof(Fpu_State)==108);
   for (i = 7; i >= 0; i--) {
      printf ( " %s fpreg%d: 0x", 
               (UInt)i == fp_get_tos() ? "**" : "  ", i );
      for (k = 0, j = FP_REG(i)+9; k < 10; k++,j--)
         printf ( "%02x", (UInt)m_fpu_state.reg[j]);
      printf ( "  %5s  ", fp_tag_names[fp_get_tag(i)] );
      printf("\n");
      //printf ( "%20.16e\n", fp_get_reg(i) );
   }
   printf("     fctrl:     0x%04x  masked: ", 
          (UInt)m_fpu_state.env[FP_ENV_CTRL] );
   for (i = FP_E_INVAL; i <= FP_E_LOS; i++)
      if (fp_get_controlword_flag(i))
         printf ( "%s ", fp_exception_names[i] );
   printf ( "\n" );

   printf("     fstat:     0x%04x  except:", 
          (UInt)m_fpu_state.env[FP_ENV_STAT] );
   for (i = FP_E_INVAL; i <= FP_E_LOS; i++)
      if (fp_get_statusword_flag(i))
         printf ( "%s ", fp_exception_names[i] );
   printf ( "  top: %d  ", fp_get_tos() );
   printf ( "c3210: %d%d%d%d",
            fp_get_statusword_flag(FP_F_C3),
            fp_get_statusword_flag(FP_F_C2),
            fp_get_statusword_flag(FP_F_C1),
            fp_get_statusword_flag(FP_F_C0) );
   printf ( "  STACKF: %d\n", fp_get_statusword_flag(FP_E_STACKF) );

   printf("      ftag:     0x%04x  ", (UInt)m_fpu_state.env[FP_ENV_TAG] );
   for (i = 7; i >= 0; i--)
      printf ( "%d:%s ", i, fp_tag_names[fp_get_tag(i)] );
   printf("\n");

   printf("       fip: 0x%08x\n", 
           (((UInt)m_fpu_state.env[FP_ENV_IP+1]) << 16) |
            ((UInt)m_fpu_state.env[FP_ENV_IP]) );
   printf("       fcs:     0x%04x\n", 
           ((UInt)m_fpu_state.env[FP_ENV_CS]) );
   printf("    fopoff: 0x%08x\n", 
           (((UInt)m_fpu_state.env[FP_ENV_OPOFF+1]) << 16) |
            ((UInt)m_fpu_state.env[FP_ENV_OPOFF]) );
   printf("    fopsel:     0x%04x\n", 
           ((UInt)m_fpu_state.env[FP_ENV_OPSEL]) );
}


/* ------------------------------------------------- */


/* Initialise the FPU, dump its state, and print it. */


void show ( unsigned char* st )
{
  int i;
  for (i = 0; i < 28; i++) {
    printf("%02x ", st[i]);
    if (i > 0 && ((i & 3) == 3)) printf("\n");
  }
  for (i = 0; i < 80; i++) {
    printf("%02x ", st[i+28]);
    if (i > 0 && ((i % 10) == 9)) printf("\n");
  }
  printf("\n");
}

int main ( void ) 
{
  Fpu_State* st = &m_fpu_state;
  assert(sizeof(m_fpu_state) == 108);
  asm volatile ("finit ; fnsave %0"
                : "=m" (m_fpu_state)
                : 
                : "memory" );
  printFpuState();
  printf("\n\n");

  asm volatile ("fildl 0(%%esp) ; pushl $17 ; fildl 0(%%esp) ; addl $4, %%esp ;  fnsave %0"
                : "=m" (m_fpu_state)
                : 
                : "memory" );
  printFpuState();
  printf("\n");
  show(st);
  return 0;
}
