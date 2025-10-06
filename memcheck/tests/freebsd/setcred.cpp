#include <sys/ucred.h>
#include <cstring>
#include <cstdlib>
#include <cerrno>
#include <cassert>

static long x0;

int main()
{
   long *px{static_cast<long*>(malloc(2*sizeof(long)))};
   x0 = px[0];
   struct setcred cred1;
   struct setcred* cred2;
   int flags1{SETCREDF_RUID};
   int flags2{SETCREDF_SUPP_GROUPS};
   size_t size1{sizeof(cred1)};
   int ret;

   std::memset(&cred1, 250, sizeof(cred1));

   // needs to be root to work correctly
   ret = setcred(flags1, &cred1, size1);
   assert(ret == -1);
   assert(errno == EPERM);

   // not accessible
   ret = setcred(flags1, nullptr, size1);
   assert(ret == -1);
   assert(errno == EFAULT);

   // uninit
   ret = setcred(flags2+x0, (struct setcred*)x0, size1+x0);
   assert(ret == -1);
   assert(errno == EFAULT);

   // invalid flags
   ret = setcred(9999+x0, &cred1, sizeof(cred1));
   assert(ret == -1);
   assert(errno == EINVAL);

   // invalid size
   ret = setcred(flags1, &cred1, 3+x0);
   assert(ret == -1);
   assert(errno == EINVAL);

   cred2 = new struct setcred;

   // uninit memory
   ret = setcred(flags1, cred2, size1);
   assert(ret == -1);
   assert(errno == EPERM);

   delete cred2;
   free(px);
}

