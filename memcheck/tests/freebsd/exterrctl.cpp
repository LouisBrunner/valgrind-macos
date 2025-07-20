#include <sys/exterrvar.h>
#include <cstdlib>

static long x0;

int main()
{
   // int exterrctl(u_int op, u_int flags, void *ptr);
   // op should be 0 or EXTERRCTLF_FORCE (1)
   // flags EXTERRCTL_ENABLE EXTERRCTL_DISABLE EXTERRCTL_UD (1 2 3) 
   // see sys/sys/_uexterror.h for ptr
   long *px{static_cast<long*>(malloc(2*sizeof(long)))};
   x0 = px[0];

   char *foo = new char [48+128];
   delete [] foo;

   exterrctl(x0, x0+2, x0+foo); 
}

