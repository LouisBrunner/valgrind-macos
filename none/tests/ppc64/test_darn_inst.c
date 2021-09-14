#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#ifdef HAS_ISA_3_00
#include <altivec.h>
#endif

#define TRUE  1
#define FALSE 0
#define ERROR 0xFFFFFFFFFFFFFFFFULL

int main()
{
#ifdef HAS_ISA_3_00
  unsigned long long rand;
  int success = TRUE;

  /* The random number instruction returns 0xFFFFFFFFFFFFFFFFULL on error
     and an unsigned 64-bit value between 0 and 0xFFFFFFFFFFFFFFFEULL on
     success.  */
  __asm__ __volatile__ (".machine push; .machine power9;" \
                        "darn %0,0; .machine pop" : "=r" (rand));
  if (rand == ERROR) {
     success = FALSE;
     printf ("Error darn 0 result = 0%llx not in expected range.\n", rand);
  }

  __asm__ __volatile__ (".machine push; .machine power9;" \
                        "darn %0,1; .machine pop" : "=r" (rand));
  if (rand == ERROR) {
     success = FALSE;
     printf ("Error darn 1 result = 0%llx not in expected range.\n", rand);
  }

  __asm__ __volatile__ (".machine push; .machine power9;" \
                        "darn %0,2; .machine pop" : "=r" (rand));
  if (rand == ERROR) {
     success = FALSE;
       printf ("Error darn 2 result = 0%llx not in expected range.\n", rand);
  }

  if (success)
     printf("Success.\n");
  else
     printf("Failure.\n");

#else
  printf("HAS_ISA_3_00 not detected.\n");
#endif
  return 0;
}
