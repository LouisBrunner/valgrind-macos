#include "scalar.h"

int main(void)
{
   long* px  = malloc(sizeof(long));
   x0  = px[0];

   /* SYS_pdfork                  518 */
   GO(SYS_pdfork, "2s 0m");
   SY(SYS_pdfork, x0+1, x0+12); FAIL;

   return(0);
}

