
#include <stdio.h>
#include <stdlib.h>

/* The original test driver machinery. */
#define N_TEST_TRANSACTIONS 500
#define N_TEST_ARR 2000

#define M_TEST_MALLOC 1000

void* test_arr[N_TEST_ARR];

int main ( int argc, char** argv )
{
   int i, j, k, nbytes;
   unsigned char* chp;

   for (i = 0; i < N_TEST_ARR; i++)
      test_arr[i] = NULL;

   for (i = 0; i < N_TEST_TRANSACTIONS; i++) {
      j = random() % N_TEST_ARR;
      if (test_arr[j]) {
         free(test_arr[j]);
         test_arr[j] = NULL;
      } else {
         nbytes = 1 + random() % M_TEST_MALLOC;
         if (random()%64 == 32) 
            nbytes *= 17;
         test_arr[j] = malloc( nbytes );
         chp = test_arr[j];
         for (k = 1; k < nbytes; k++) 
            chp[k] = (unsigned char)(k + 99);
      }
   }

   for (i = 0; test_arr[i] == NULL; i++) ;
   free(test_arr[i]);
   ((char*)test_arr[i])[0] = 0;

   for (i = 0; i < N_TEST_ARR; i++) {
      if (test_arr[i]) {
         free(test_arr[i]);
         test_arr[i] = NULL;
      }
   }

   return 0;
}
