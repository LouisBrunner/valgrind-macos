#include <stdio.h>
#include "macro_load_store.h"

int main()
{
   int i;
   int s1 = sizeof(int);
   int s2 = sizeof(unsigned long long);
   init_reg_val2();
   /**********************************************************************/
   /*-------------------------------LOAD---------------------------------*/
   /**********************************************************************/
   /* lb */
   for (i = 0; i < N*s1; i++)
      TEST1("lb", i, reg_val1);

   for (i = 0; i < N*s2; i++)
      TEST1("lb", i, reg_val2);

   /* lbu */
   for (i = 0; i < N*s1; i++)
      TEST1("lbu", i, reg_val1);

   for (i = 0; i < N*s2; i++)
      TEST1("lbu", i, reg_val2);

   /* ld */
   for (i = 0; i < N*s1; i = i+8)
      TEST1("ld", i, reg_val1);

   for (i = 0; i < N*s2; i = i+8)
      TEST1("ld", i, reg_val2);

   /* ldl */
   for (i = 0; i < N*s1; i++)
      TEST1("ldl", i, reg_val1);

   for (i = 0; i < N*s2; i++)
      TEST1("ldl", i, reg_val2);

   /* ldr */
   for (i = 0; i < N*s1; i++)
      TEST1("ldr", i, reg_val1);

   for (i = 0; i < N*s2; i++)
      TEST1("ldr", i, reg_val2);

   /* lh */
   for (i = 0; i < N*s1; i = i+2)
      TEST1("lh", i, reg_val1);

   for (i = 0; i < N*s2; i = i+2)
      TEST1("lh", i, reg_val2);

   /* lhu */
   for (i = 0; i < N*s1; i = i+2)
      TEST1("lhu", i, reg_val1);

   for (i = 0; i < N*s2; i = i+2)
      TEST1("lhu", i, reg_val2);

   /* lw */
   for (i = 0; i < N*s1; i = i+4)
      TEST1("lw", i, reg_val1);

   for (i = 0; i < N*s2; i = i+4)
      TEST1("lw", i, reg_val2);

   /* lwl */
   for (i = 0; i < N*s1; i++)
      TEST1("lwl", i, reg_val1);

   for (i = 0; i < N*s2; i++)
      TEST1("lwl", i, reg_val2);

   /* lwr */
   for (i = 0; i < N*s1; i++)
      TEST1("lwr", i, reg_val1);

   for (i = 0; i < N*s2; i++)
      TEST1("lwr", i, reg_val2);

   /* lwu */
   for (i = 0; i < N*s1; i = i+4)
      TEST1("lwu", i, reg_val1);

   for (i = 0; i < N*s2; i = i+4)
      TEST1("lwu", i, reg_val2);

   /**********************************************************************/
   /*-------------------------------STORE--------------------------------*/
   /**********************************************************************/
   init_reg_val_zero();
   /* sb */
   for (i = 0; i < (N-1)*s2; i++)
      TEST2("sb", i);

   init_reg_val_zero();
   /* sd */
   for (i = 0; i < (N-1)*s2; i = i+8)
      TEST2("sd", i);

   init_reg_val_zero();
   /* sdl */
   for (i = 0; i < (N-1)*s2; i++)
      TEST2("sdl", i);

   init_reg_val_zero();
   /* sdr */
   for (i = 8; i < (N-1)*s2; i++)
      TEST2("sdr", i);

   init_reg_val_zero();
   /* sh */
   for (i = 0; i < (N-1)*s2; i = i+2)
      TEST2("sh", i);

   init_reg_val_zero();
   /* sw */
   for (i = 0; i < (N-1)*s2; i = i+4)
      TEST2("sw", i);

   init_reg_val_zero();
   /* swl */
   for (i = 4; i < (N-1)*s2; i++)
      TEST2("swl", i);

   init_reg_val_zero();
   /* swr */
   for (i = 4; i < (N-1)*s2; i++)
      TEST2("swr", i);

   return 0;
}
