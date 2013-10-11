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
   printf("--- LDC1 ---\n");
   for (i = 0; i < N*s1; i = i+8)
      TEST3("ldc1", i, reg_val1);

   for (i = 0; i < N*s2; i = i+8)
      TEST3("ldc1", i, reg_val2);

   printf("--- LDXC1 ---\n");
   for (i = 0; i < N*s1; i = i+8)
      TEST5("ldxc1", i, reg_val1);

   for (i = 0; i < N*s2; i = i+8)
      TEST5("ldxc1", i, reg_val2);

   printf("--- LWC1 ---\n");
   for (i = 0; i < N*s1; i = i+4)
      TEST3w("lwc1", i, reg_val1);

   for (i = 0; i < N*s2; i = i+4)
      TEST3w("lwc1", i, reg_val2);

   printf("--- LWXC1 ---\n");
   for (i = 0; i < N*s1; i = i+4)
      TEST5w("lwxc1", i, reg_val1);

   for (i = 0; i < N*s2; i = i+4)
      TEST5w("lwxc1", i, reg_val2);

   /**********************************************************************/
   /*-------------------------------STORE--------------------------------*/
   /**********************************************************************/
   init_reg_val_zero();
   printf("--- SDC1 ---\n");
   for (i = 0; i < N*s1; i = i+8) {
      TEST4("sdc1", i);
   }

   init_reg_val_zero();
   printf("--- SWC1 ---\n");
   for (i = 0; i < (N-1)*s1; i = i+4) {
      TEST4("swc1", i);
   }

   init_reg_val_zero();
   printf("--- SDXC1 ---\n");
   for (i = 0; i < N*s1; i = i+8) {
      TEST6("sdxc1", i);
   }

   init_reg_val_zero();
   printf("--- SWXC1 ---\n");
   for (i = 0; i < (N-1)*s1; i = i+4) {
      TEST6("swxc1", i);
   }

   return 0;
}
