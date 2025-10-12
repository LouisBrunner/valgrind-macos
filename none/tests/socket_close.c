#define _GNU_SOURCE
#include <unistd.h>
#include <stdio.h>
#include <sys/socket.h>
#include <stdlib.h>
#include <string.h>
#include <sys/un.h>
#include "fdleak.h"

const char *SPATH = "/tmp/vgtest-foofrob";
int socket_fd;

void open_socket()
{
  unlink (SPATH); /* Make sure socket path doesn't exist yet.  */

  socket_fd = socket(AF_UNIX, SOCK_STREAM, 0);
  fprintf (stderr, "Open socket %d\n", socket_fd);
  struct sockaddr_un  my_addr;

  memset(&my_addr, 0, sizeof(my_addr));
  my_addr.sun_family = AF_UNIX;
  strncpy(my_addr.sun_path, SPATH, sizeof(my_addr.sun_path) - 1);
  bind(socket_fd, (struct sockaddr *) &my_addr, sizeof(my_addr));
}

int main ()
{


  open_socket();

  if (socket_fd != -1)
   {
      fprintf(stderr, "close socket_fd %d\n", socket_fd);
      close (socket_fd);
   }

  fprintf (stderr, "and close the socket again %d\n", socket_fd);
  close (socket_fd);

  return 0;
}

