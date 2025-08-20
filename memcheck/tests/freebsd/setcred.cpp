#include <sys/ucred.h>
#include <cstring>
#include <cstdlib>

static long x0;

int main()
{
   long *px{static_cast<long*>(malloc(2*sizeof(long)))};
   x0 = px[0];
   struct setcred cred1;
   struct setcred* cred2;
   int flags1{0};
   int flags2;
   size_t size1{sizeof(cred1)};

   std::memset(&cred1, 250, sizeof(cred1));

   // needs to be root to work correctly
   setcred(flags1, &cred1, size1);

   // not accessible
   setcred(flags1, nullptr, size1);

   // uninit
   setcred(flags2, (struct setcred*)x0, size1+x0);

   cred2 = new struct setcred;

   // uninit memory
   setcred(flags1, cred2, size1);

   delete cred2;
   free(px);
}

