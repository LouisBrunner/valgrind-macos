#include <sys/types.h>
#include <sys/umtx.h>
#include <unistd.h>

int main(int argc, char** argv)
{
   char key[64];
   int fd = _umtx_op(nullptr, UMTX_OP_SHM, UMTX_SHM_CREAT, key, nullptr);
   if (1 == argc)
   {
      close(fd);
   }
}
