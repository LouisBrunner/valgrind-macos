
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void announce ( char* str )
{
   printf("------ %s ------\n", str);
}

int main ( void )
{
  int i;
  char* a1 = malloc(100);
  char* a2 = malloc(100);
  strcpy(a1,"here is a stringHERE IS A STRING");

  announce("lswi n == 8 (fe special cased)");
  asm volatile("li 5,0\n\t"
               "lswi 3,%0, 8\n\t" 
               "stw 3,0(%1)\n\t"
               "stw 4,4(%1)\n\t"
               "stw 5,8(%1)\n\t"
               : : "r"(a1), "r"(a2) : "r3", "r4", "r5", 
                                      "cc", "memory" );
  printf("%s\n", a2);
  for (i = 0; i < 12; i++)
    printf("%d = 0x%2x\n", i, a2[i]);
  printf("\n");


  announce("lswi n /= 8");
  asm volatile("lswi 3,%0, 9\n\t" 
               "stw 3,0(%1)\n\t"
               "stw 4,4(%1)\n\t"
               "stw 5,8(%1)\n\t"
               : : "r"(a1), "r"(a2) : "r3", "r4", "r5", 
                                      "cc", "memory" );
  printf("%s\n", a2);
  for (i = 0; i < 12; i++)
    printf("%d = 0x%2x\n", i, a2[i]);
  printf("\n");


  announce("lswx");
  free(a2);
  a2 = malloc(100);
  asm volatile("li  8, 11\n\t"
               "mtxer 8\n\t"
               "lswx 3,%0,%2\n\t" 
               "stw 3,0(%1)\n\t"
               "stw 4,4(%1)\n\t"
               "stw 5,8(%1)\n\t"
               : : "r"(a1), "r"(a2), "r"(16) : "r3", "r4", "r5", "r8", 
                                               "cc", "memory" );
  printf("%s\n", a2);
  for (i = 0; i < 12; i++)
     printf("%d = 0x%2x\n", i, a2[i]);
  printf("\n");


  announce("stswi n == 8 (fe special cased)");
  free(a2);
  a2 = calloc(100,1);
  asm volatile("lswi 3,%0, 19\n\t"
               "stswi 3,%1, 8\n"
	       : : "r"(a1), "r"(a2) : "r3","r4","r5","r6","r7",
                                      "cc", "memory" );
  printf("%s\n", a2);
  printf("\n");


  announce("stswi n /= 8");
  free(a2);
  a2 = calloc(100,1);
  asm volatile("lswi 3,%0, 19\n\t"
               "stswi 3,%1, 17\n"
	       : : "r"(a1), "r"(a2) : "r3","r4","r5","r6","r7",
                                      "cc", "memory" );
  printf("%s\n", a2);
  printf("\n");


  announce("stswx");
  free(a2);
  a2 = calloc(100,1);
  asm volatile("li  8, 11\n\t"
               "mtxer 8\n\t"
               "lswx  3,%0,%2\n\t" 
               "stswx 3,%1,%2\n\t" 
               : : "r"(a1), "r"(a2), "r"(16) : "r3", "r4", "r5", "r8", 
                                               "cc", "memory" );
  printf("%s\n", a2+16);
  printf("\n");

  return 0;
}
