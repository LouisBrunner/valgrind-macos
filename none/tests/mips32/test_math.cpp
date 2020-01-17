#include <fenv.h>
#include <limits.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>

static void DivideByZero() {
  // volatile to prevent compiler optimizations.
  volatile float zero = 0.0f;
  volatile float result __attribute__((unused)) = 123.0f / zero;
}

volatile double cube = 27.0;

int main () {
   /* Testing lrint. */
   fesetround(FE_UPWARD); // lrint/lrintf/lrintl obey the rounding mode.
   printf("fesetround(FE_UPWARD)\n");
   printf("lrint(1234.01): %ld\n", lrint(1234.01));
   printf("lrintf(1234.01f): %ld\n", lrintf(1234.01f));
   printf("lrintl(1234.01): %ld\n", lrintl(1234.01));
   fesetround(FE_TOWARDZERO); // lrint/lrintf/lrintl obey the rounding mode.
   printf("fesetround(FE_TOWARDZERO)\n");
   printf("lrint(1234.01): %ld\n", lrint(1234.01));
   printf("lrintf(1234.01f): %ld\n", lrintf(1234.01f));
   printf("lrintl(1234.01): %ld\n", lrintl(1234.01));
   fesetround(FE_UPWARD); // llrint/llrintf/llrintl obey the rounding mode.
   printf("fesetround(FE_UPWARD)\n");
   printf("llrint(1234.01): %lld\n", llrint(1234.01));
   printf("llrintf(1234.01f): %lld\n", llrintf(1234.01f));
   printf("llrintf(1234.01f): %lld\n", llrintl(1234.01));
   fesetround(FE_TOWARDZERO); // llrint/llrintf/llrintl obey the rounding mode.
   printf("fesetround(FE_TOWARDZERO)\n");
   printf("llrint(1234.01): %lld\n", llrint(1234.01));
   printf("llrintf(1234.01f): %lld\n", llrintf(1234.01f));
   printf("llrintl(1234.01): %lld\n", llrintl(1234.01));

   /* Tesing rint. */
   fesetround(FE_UPWARD); // rint/rintf/rintl obey the rounding mode.
   printf("fesetround(FE_UPWARD)\n");
   feclearexcept(FE_ALL_EXCEPT); // rint/rintf/rintl do set the FE_INEXACT flag.
   printf("feclearexcept(FE_ALL_EXCEPT)\n");
   printf("rint(1234.0): %f\n", rint(1234.0));
   printf("(fetestexcept(FE_ALL_EXCEPT) & FE_INEXACT): %d\n",
           (fetestexcept(FE_ALL_EXCEPT) & FE_INEXACT));
   printf("rint(1234.01): %f\n", rint(1234.01));
   printf("(fetestexcept(FE_ALL_EXCEPT) & FE_INEXACT): %d\n",
           (fetestexcept(FE_ALL_EXCEPT) & FE_INEXACT));

   feclearexcept(FE_ALL_EXCEPT); // rint/rintf/rintl do set the FE_INEXACT flag.
   printf("feclearexcept(FE_ALL_EXCEPT)\n");
   printf("rintf(1234.0f): %f\n", rintf(1234.0f));
   printf("(fetestexcept(FE_ALL_EXCEPT) & FE_INEXACT): %d\n",
           (fetestexcept(FE_ALL_EXCEPT) & FE_INEXACT));
   printf("rintf(1234.01f): %f\n", rintf(1234.01f));
   printf("(fetestexcept(FE_ALL_EXCEPT) & FE_INEXACT): %d\n",
           (fetestexcept(FE_ALL_EXCEPT) & FE_INEXACT));

   feclearexcept(FE_ALL_EXCEPT); // rint/rintf/rintl do set the FE_INEXACT flag.
   printf("feclearexcept(FE_ALL_EXCEPT)\n");
   printf("rintl(1234.0): %Lf\n", rintl(1234.0));
   printf("(fetestexcept(FE_ALL_EXCEPT) & FE_INEXACT): %d\n",
           (fetestexcept(FE_ALL_EXCEPT) & FE_INEXACT));
   printf("rintl(1234.01): %Lf\n", rintl(1234.01));
   printf("(fetestexcept(FE_ALL_EXCEPT) & FE_INEXACT): %d\n",
           (fetestexcept(FE_ALL_EXCEPT) & FE_INEXACT));

   fesetround(FE_TOWARDZERO); // rint/rintf obey the rounding mode.
   printf("fesetround(FE_TOWARDZERO)\n");
   printf("rint(1234.01): %f\n", rint(1234.01));
   printf("rintf(1234.01f): %f\n", rintf(1234.01f));
   printf("rintl(1234.01): %Lf\n", rintl(1234.01));

   /* Testing nearbyint. */
   fesetround(FE_UPWARD); // nearbyint/nearbyintf/nearbyintl obey the rounding mode.
   printf("fesetround(FE_UPWARD)\n");
   feclearexcept(FE_ALL_EXCEPT); // nearbyint/nearbyintf/nearbyintl don't set the FE_INEXACT flag.
   printf("feclearexcept(FE_ALL_EXCEPT)\n");
   printf("nearbyint(1234.0): %f\n", nearbyint(1234.0));
   printf("nearbyint(1234.01): %f\n", nearbyint(1234.01));

   feclearexcept(FE_ALL_EXCEPT);
   printf("feclearexcept(FE_ALL_EXCEPT)\n");
   printf("nearbyintf(1234.0f): %f\n", nearbyintf(1234.0f));
   printf("nearbyintf(1234.01f): %f\n", nearbyintf(1234.01f));

   feclearexcept(FE_ALL_EXCEPT); // nearbyint/nearbyintf/nearbyintl don't set the FE_INEXACT flag.
   printf("feclearexcept(FE_ALL_EXCEPT)\n");
   printf("nearbyintl(1234.0f): %Lf\n", nearbyintl(1234.0f));
   printf("nearbyintl(1234.01f): %Lf\n", nearbyintl(1234.01f));

   fesetround(FE_TOWARDZERO); // nearbyint/nearbyintf/nearbyintl obey the rounding mode.
   printf("fesetround(FE_TOWARDZERO)\n");
   printf("nearbyint(1234.01): %f\n", nearbyint(1234.01));
   printf("nearbyintf(1234.01f): %f\n", nearbyintf(1234.01f));
   printf("nearbyintl(1234.01): %Lf\n", nearbyintl(1234.01));

   /* Test log. */
   printf("log(M_E): %lf\n", log(M_E));

   #define EPS 0.0000001
   /* Test tgamma. */
   double tgamma5 = tgamma(5.0);
   printf(tgamma5 <= 24.0 + EPS && tgamma5 >= 24.0 - EPS ? "tgamma(5.0): PASS\n" : "tgamma(5.0): FAIL\n");

   /* Test cbrt. */
   double cbrt27 = cbrt(cube);
   printf(cbrt27 <= 3.0 + EPS && cbrt27 >= 3.0 - EPS ? "cbrt(27.0): PASS\n" : "cbrt(27.0): FAIL\n");
   #undef EPS

   /* Test dividing by zero. */
   // Clearing clears.
   printf("feclearexcept(FE_ALL_EXCEPT): %d\n", feclearexcept(FE_ALL_EXCEPT));

   // Dividing by zero sets FE_DIVBYZERO.
   DivideByZero();
   int raised = fetestexcept(FE_DIVBYZERO | FE_OVERFLOW);
   printf("raised: %d\n", raised);

   return 0;
}
