#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>

int main()
{

   const wchar_t in[]       = {L'a', L'b', L'c', 0};
   wchar_t       out[3 + 1] = {
      0,
   };

   size_t res = wcsxfrm(out, in, 3);
   printf("%zu\n", res);

   wchar_t* in2 = malloc(sizeof(wchar_t) * 4);
   memcpy(in2, in, sizeof(in));
   res = wcsxfrm(out, in2, 3);
   printf("%zu\n", res);
   free(in2);

   wchar_t* in3 = malloc(sizeof(wchar_t) * 4);
   memcpy(in3, in, sizeof(in));
   res = wcsxfrm(out, in3, 3);
   printf("%zu\n", res);
   free(in3);
}
