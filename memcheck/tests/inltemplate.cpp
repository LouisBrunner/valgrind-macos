#include <stdio.h>
#include <valgrind.h>
#define INLINE    inline __attribute__((always_inline))

class X
{
public:

   template <typename T>
   static INLINE T temp_member_func_b(T argb) {
      static T locb = 0;
      if (argb > 0)
         locb += argb;
      return locb;
   }

   template <typename T>
   static /*INLINE*/ T temp_member_func_noinline(T arga) {
       return temp_member_func_b(arga);
   }

};


int main() {
   int result;
   result = X::temp_member_func_noinline(result);
   return 0;
}

