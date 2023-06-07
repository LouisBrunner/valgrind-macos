#include <stdio.h>
#include <stdlib.h>
#include "../memcheck.h"
#include "leak.h"

char *b10;
char *b21;
char *b32_33[2];
char *b42_43[2];
static void breakme() {};
void f(void)
{
   int i;

   b10 = malloc (10);

   fprintf(stderr, "expecting details 10 bytes reachable\n"); fflush(stderr); breakme();
   VALGRIND_DO_LEAK_CHECK;

   fprintf(stderr, "expecting to have NO details\n"); fflush(stderr); breakme();
   VALGRIND_DO_ADDED_LEAK_CHECK;

   b10--; // lose b10
   b21 = malloc (21);
   fprintf(stderr, "expecting details +10 bytes lost, +21 bytes reachable\n"); fflush(stderr); breakme();
   VALGRIND_DO_ADDED_LEAK_CHECK;

   for (i = 0; i < 2; i ++)
      b32_33[i] = malloc (32+i);
   fprintf(stderr, "expecting details +65 bytes reachable\n"); fflush(stderr); breakme();
   VALGRIND_DO_ADDED_LEAK_CHECK;

   fprintf(stderr, "expecting to have NO details\n"); fflush(stderr); breakme();
   VALGRIND_DO_ADDED_LEAK_CHECK;

   b10++;
   fprintf(stderr, "expecting details +10 bytes reachable\n"); fflush(stderr); breakme();
   VALGRIND_DO_ADDED_LEAK_CHECK;

   b10--;
   fprintf(stderr, "expecting details -10 bytes reachable, +10 bytes lost\n"); fflush(stderr); breakme();
   VALGRIND_DO_CHANGED_LEAK_CHECK;

   b10++;
   fprintf(stderr, "expecting details -10 bytes lost, +10 bytes reachable\n"); fflush(stderr); breakme();
   VALGRIND_DO_CHANGED_LEAK_CHECK;

   b32_33[0]--;
   fprintf(stderr, "expecting details 32 (+32) bytes lost, 33 (-32) bytes reachable\n"); fflush(stderr); breakme();
   VALGRIND_DO_CHANGED_LEAK_CHECK;

   for (i = 0; i < 2; i ++)
      b42_43[i] = malloc (42+i);

   b42_43[0]--;
   fprintf(stderr, "expecting details 42 (+42) bytes lost, 43 (+43) bytes reachable\n"); fflush(stderr); breakme();
   VALGRIND_DO_NEW_LEAK_CHECK;

   b42_43[1]--;
   fprintf(stderr, "expecting to have NO details\n"); fflush(stderr); breakme();
   VALGRIND_DO_NEW_LEAK_CHECK;

   fprintf(stderr, "finished\n");
}

int main(void)
{
   DECLARE_LEAK_COUNTERS;

   GET_INITIAL_LEAK_COUNTS;

   f();   // see leak-cases.c


   GET_FINAL_LEAK_COUNTS;

   PRINT_LEAK_COUNTS(stderr);

   return 0;
}
