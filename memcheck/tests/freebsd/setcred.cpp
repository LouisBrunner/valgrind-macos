#include <sys/ucred.h>
#include <cstring>

int main()
{
   struct setcred cred1;
   struct setcred* cred2;
   int flags1{0};
   int flags2;
   size_t size1{sizeof(cred1)};
   size_t size2;

   std::memset(&cred1, 250, sizeof(cred1));

   // needs to be root to work correctly
   setcred(flags1, &cred1, size1);

   // not accessible
   setcred(flags1, nullptr, size1);

   // uninit
   setcred(flags2, cred2, size2);

   cred2 = new struct setcred;

   // uninit memory
   setcred(flags1, cred2, size1);

   delete cred2;
}

