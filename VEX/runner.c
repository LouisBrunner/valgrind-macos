
#include <stdlib.h>
#include <malloc.h>
#include <assert.h>

/* --------------------------------------------------------- */
/* TRANSLATION TABLE/CACHE                                   */
/* --------------------------------------------------------- */


#define N_TT_ENTRIES 1000

typedef
   struct {
      char* orig;
      int   orig_size;
      char* trans;
      int   trans_size;
   }
   TTEntry;

int n_transtab_used = 0;
TTEntry transtab[N_TT_ENTRIES];


/* Called by Haskell to add a translation to the trans cache.
   Supplied translation is in mallocville.  add_translation should
   copy it out as the caller will free it on return.  */

/* EXPORTED */
void add_translation ( char* orig, int orig_size, char* trans, int trans_size )
{
   int i;
   assert(n_transtab_used < N_TT_ENTRIES);
   transtab[n_transtab_used].orig       = orig;
   transtab[n_transtab_used].orig_size  = orig_size;
   transtab[n_transtab_used].trans_size = trans_size;

   transtab[n_transtab_used].trans = malloc(trans_size);
   assert(transtab[n_transtab_used].trans != NULL);
   for (i = 0; i < trans_size; i++)
      transtab[n_transtab_used].trans[i] = trans[i];

   n_transtab_used++;
}

static
char* find_translation ( char* orig )
{
   int i;
   for (i = 0; i < n_transtab_used; i++)
      if (transtab[i].orig == orig)
         return transtab[i].trans;
   return NULL;
}

/* --------------------------------------------------------- */
/* SIMULATED STATE                                           */
/* --------------------------------------------------------- */

typedef unsigned int Word;

/* ARM state */
/* r0 .. r15, flags */
Word regs_arm[16+1];

#define REG_PC 15
#define REG_SP 14

Word* sim_stack;

//---------------------------------------------

/* Calling convention: enter the translation with r0 pointing at
   regs_arm.  Translation may trash r1 .. r12 inclusive. */

static
void run_translation ( char* trans )
{
  /* r0 holds trans */
  __asm __volatile
     ("stmfd   sp!, {r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13}\n\t"
      "mov     r12, %0\n\t"
      "mov     r0, %1\n\t"
      "bl      r12\n\t"
      "ldmea   sp!, {r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13}\n\t"
      :
      : "ir" (trans), "ir" (&regs_arm[0]) );
}



/* Called by Haskell to initialise the simulated machine, and tell it
   its starting pc. */

/* EXPORTED */
void initialise_machine ( char* first_pc )
{
   n_transtab_used = 0;
   regs_arm[REG_PC] = (Word)first_pc;
   sim_stack = malloc(10000 * sizeof(Word));
   regs_arm[REG_SP] = (Word)(&sim_stack[9999]);
}

/* Run the simulated machine for a while.  Returns when a new BB needs
   to be translated, and returns its address. */

/* EXPORTED */
char* run_machine ( void )
{
   char* trans;
   while (1) {
      trans = find_translation((char*)(regs_arm[15]));
      if (trans == NULL) return (char*)(regs_arm[15]);
      run_translation(trans);
   }
}


