
/*---------------------------------------------------------------*/
/*--- begin                                        dispatch.c ---*/
/*---------------------------------------------------------------*/

#include "basictypes.h"


/* --------------------------------------------------------- */
/* TRANSLATION TABLE/CACHE                                   */
/* --------------------------------------------------------- */

static
char* find_translation ( char* orig )
{
   int i;
   for (i = 0; i < n_transtab_used; i++)
      if (transtab[i].orig == orig)
         return transtab[i].trans;
   return NULL;
}


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


/* Call here to add a translation to the trans cache.
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

#ifdef arm_TARGET_ARCH
   arm_notify_new_code(transtab[n_transtab_used].trans, trans_size);
#endif

   n_transtab_used++;
}

/* Run the simulated machine for a while.  Returns when a new BB needs
   to be translated, and returns its address.  Returns NULL when we
   want to stop. */

/* EXPORTED */
char* run_machine ( void )
{
   char* nextpc_orig;
   char* nextpc_trans;
   while (1) {
      nextpc_orig = (char*)(regs_arm[REG_PC]);
      if (nextpc_orig == stop_at)
         return NULL;
      nextpc_trans = find_translation(nextpc_orig);
      if (nextpc_trans == NULL)
         return nextpc_orig;
      run_translation(nextpc_trans, (char*) &regs_arm[0] );
   }
}


/* HOW TO USE:
 
   for a main fn :: void main ( void )

   * load .o's, link, etc

   * call initialise_machine with & main

   * call run_machine repeatedly.  If it returns NULL, stop.  Else
     make a translation of the returned address, pass it to
     add_translation, and resume running by calling run_machine.

*/
