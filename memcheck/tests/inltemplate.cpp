#include <stdio.h>
#include <valgrind.h>

/* GCC 3.4.6 will not compile inlined member template functions.
   Let's assume GCC 4.x does */
#ifdef __GNUC__
#if __GNUC__ > 3
#define INLINE    inline __attribute__((always_inline))
#else
#define INLINE
#endif
#endif

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

