#include <sys/socket.h>
#include <unistd.h>
#include "fdleak.h"

int
main (int argc, char **argv)
{
   int fds[2];




   CLOSE_INHERITED_FDS;

   socketpair(AF_UNIX, SOCK_STREAM, PF_UNIX, fds);
   return 0;
}
