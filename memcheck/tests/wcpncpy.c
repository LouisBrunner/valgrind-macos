#include <wchar.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

int main(void)
{
   const wchar_t in[]       = {L'H', L'e', L'l', L'l', L'o', 0};
    
   wchar_t* dest1 = malloc(5*sizeof(wchar_t) + 2);
   wchar_t* dest2 = malloc(11*sizeof(wchar_t));
   
   // uninit read
   wcpncpy(dest1, dest2, 3);
   
   wchar_t* end = wcpncpy(dest1, in, 3);
   
   assert(3 == end - dest1);
   assert(0 == wmemcmp(in , dest1, 3));
   
   end = wcpncpy(dest2, in, 10);
   assert(5 == end - dest2);
   assert(0 == wmemcmp(dest2 , in, 6));
   assert(0 == dest2[9]);
   
   // too small - invalid write
   end = wcpncpy(dest1, in, 6);
   
   wcpncpy(dest2, in, 5);
   wcpncpy(dest2+5, in, 6);
   
   // overlap
   // sss
   //   ddd
   wcpncpy(dest2, dest2+2, 3);
   
   wcpncpy(dest2, in, 5);
   wcpncpy(dest2+5, in, 6);
   
   // overlap
   //   sss
   // ddd
   wcpncpy(dest2+2, dest2, 3);
   
   free(dest1);
   free(dest2);
}
