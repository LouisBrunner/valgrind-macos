/* This is based on /src/tools/test/stress2/misc/sctp.sh
 * but it uses fork and the server exits rather than
 * loops indefinitely */

#include <sys/types.h>
#include <sys/socket.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <libgen.h>
#include <netinet/in.h>
#include <netinet/sctp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int my_port_num = 62324;

static void
die(const char *s)
{
   perror(s);
   exit(1);
}

static void
server(void)
{
   struct sctp_sndrcvinfo sndrcvinfo;
   struct sockaddr_in servaddr = {
      .sin_family = AF_INET,
      .sin_addr.s_addr = htonl(INADDR_ANY),
      .sin_port = htons(my_port_num),
   };
   struct sctp_initmsg initmsg = {
      .sinit_num_ostreams = 5,
      .sinit_max_instreams = 5,
      .sinit_max_attempts = 4,
   };
   int listen_fd, conn_fd, flags, ret, in;

   listen_fd = socket(AF_INET, SOCK_STREAM, IPPROTO_SCTP);
   if (listen_fd < 0)
      die("socket");

   ret = bind(listen_fd, (struct sockaddr *) &servaddr, sizeof(servaddr));
   if (ret < 0)
      die("bind");

   ret = setsockopt(listen_fd, IPPROTO_SCTP, SCTP_INITMSG, &initmsg,
         sizeof(initmsg));
   if (ret < 0)
      die("setsockopt");

   ret = listen(listen_fd, initmsg.sinit_max_instreams);
   if (ret < 0)
      die("listen");

   char buffer[1024];

   printf("Waiting for connection\n");
   fflush(stdout);

   conn_fd = accept(listen_fd, (struct sockaddr *) NULL, NULL);
   if(conn_fd < 0)
      die("accept()");

   printf("New client connected\n");
   fflush(stdout);

   /* Note that flags is uninitialized here */
   in = sctp_recvmsg(conn_fd, buffer, sizeof(buffer), NULL, 0,
         &sndrcvinfo, &flags);
   if (in > 0) {
      printf("Received data: %s\n", buffer);
      fflush(stdout);
   }

   close(conn_fd);
}

static void
client(void)
{
   struct sockaddr_in servaddr = {
      .sin_family = AF_INET,
      .sin_port = htons(my_port_num),
      .sin_addr.s_addr = inet_addr("127.0.0.1"),
   };
   int conn_fd, ret;
   const char *msg = "Hello, Server!";

   conn_fd = socket(AF_INET, SOCK_STREAM, IPPROTO_SCTP);
   if (conn_fd < 0)
      die("socket()");

   ret = connect(conn_fd, (struct sockaddr *) &servaddr, sizeof(servaddr));
   if (ret < 0)
      die("connect()");

   ret = sctp_sendmsg(conn_fd, (void *) msg, strlen(msg) + 1, NULL, 0, 0, 0, 0, 0, 0 );
   if (ret < 0)
      die("sctp_sendmsg");

   close(conn_fd);
}

int
main(int argc __unused, char *argv[])
{
   switch (fork())
   {
   case 0:
      sleep(1);
      client();
      exit(EXIT_SUCCESS);
   default:
      server();
      exit(EXIT_SUCCESS);
   case -1:
      exit(EXIT_FAILURE);
   }
}
