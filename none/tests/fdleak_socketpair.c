#include <sys/types.h>
#include <sys/socket.h>
#include <unistd.h>
#include "fdleak.h"
#include <sys/errno.h>

int main (int argc, char **argv)
{
   int fds[2];



   (void) DO( socketpair(AF_UNIX, SOCK_STREAM, PF_UNSPEC, fds) );

   return 0;
}
