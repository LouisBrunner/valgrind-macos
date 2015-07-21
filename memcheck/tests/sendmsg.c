#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/ip.h>

#define PORT 12345

int
main (int argc, char **argv)
{
  int fd;
  struct sockaddr_in sa;
  struct msghdr msg;
  struct iovec iov[2];

  fd = socket (AF_INET, SOCK_DGRAM, 0);
  if (fd == -1)
    {
      perror ("socket()");
      exit (EXIT_FAILURE);
    }

  sa.sin_family = AF_INET;
  sa.sin_addr.s_addr = htonl (INADDR_LOOPBACK);
  sa.sin_port = htons (PORT);
  if (connect (fd, (struct sockaddr *) &sa, sizeof (sa)) == -1)
    {
      perror ("connect ()");
      exit (EXIT_FAILURE);
    }

  // Create msg_hdr. Oops, we forget to set msg_name...
  msg.msg_namelen = 0;
  iov[0].iov_base = "one";
  iov[0].iov_len = 3;
  iov[1].iov_base = "two";
  iov[1].iov_len = 3;
  msg.msg_iov = &iov[0];
  msg.msg_iovlen = 2;
  msg.msg_control = NULL;
  msg.msg_controllen = 0;

  size_t s = sendmsg (fd, &msg, 0);

  // Note how we now do set msg_name, but don't set msg_flags.
  // The msg_flags field is ignored by sendmsg.
  msg.msg_name = NULL;

  fd = socket (AF_INET, SOCK_DGRAM, 0);
  if (fd == -1)
    {
      perror ("socket()");
      exit (EXIT_FAILURE);
    }

  if (connect (fd, (struct sockaddr *) &sa, sizeof (sa)) == -1)
    {
      perror ("connect ()");
      exit (EXIT_FAILURE);
    }

  s = sendmsg (fd, &msg, 0);
  if (s == -1)
    {
      perror ("sendmsg ()");
      exit (EXIT_FAILURE);
    }
  else
    fprintf (stderr, "sendmsg: %d\n", (int) s);

  return 0;
}
