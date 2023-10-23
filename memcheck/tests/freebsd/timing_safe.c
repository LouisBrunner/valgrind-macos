#include <string.h>
#include <assert.h>
#include <stdlib.h>

int main(void)
{
   char* s1 = "the the the";
   char* s2 = "the cat zat";
   char* d1;
   double undef1;
   double undef2;
   int res;
   
   res = timingsafe_bcmp(s1, s2, 3);
   assert(res == 0);
   res = timingsafe_bcmp(s1, s2, 5);
   assert(res != 0);
   res = timingsafe_bcmp(s1, s1+4, 3);
   assert(res == 0);
   
   res = timingsafe_memcmp(s1, s2, 3);
   assert(res == 0);
   res = timingsafe_memcmp(s1, s2, 5);
   assert(res > 0);
   res = timingsafe_memcmp(s1+8, s2+8, 3);
   assert(res < 0);
   res = timingsafe_memcmp(s1, s1+4, 3);
   assert(res == 0);
   
   timingsafe_bcmp(&undef1, &undef2, 8);
   timingsafe_memcmp(&undef1, &undef2, 8);
   
   d1 = strdup(s1);
   
   timingsafe_bcmp(s1, d1, 13);
   timingsafe_memcmp(s1, d1, 13);
   
   free(d1);
   
   timingsafe_bcmp(s1, d1, 10);
   timingsafe_memcmp(s1, d1, 10);
}
