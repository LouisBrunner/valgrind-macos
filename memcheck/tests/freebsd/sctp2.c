
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
#include <stdint.h>
#include "../../memcheck.h"

int main()
{
   int conn_fd;
   char msg[10];
   char buffer[10];
   struct sockaddr to;
   socklen_t tolen = sizeof (struct sockaddr);
   uint32_t ppid;
   uint32_t flags;
   uint16_t stream_no;
   uint32_t timetolive;
   uint32_t context;
   struct sctp_sndrcvinfo sinfo;
   int rflags;
   size_t undef_size = 10U;
   
   VALGRIND_MAKE_MEM_UNDEFINED(&undef_size, sizeof(undef_size));
   VALGRIND_MAKE_MEM_UNDEFINED(&tolen, sizeof(tolen));

   sctp_sendmsg(conn_fd, msg, undef_size, &to, tolen, ppid, flags, stream_no, timetolive, context);
   
   VALGRIND_MAKE_MEM_NOACCESS(&to, sizeof(to));
   VALGRIND_MAKE_MEM_UNDEFINED(&tolen, sizeof(tolen));
   VALGRIND_MAKE_MEM_NOACCESS(&sinfo, sizeof(sinfo));
   VALGRIND_MAKE_MEM_NOACCESS(&rflags, sizeof(rflags));

   sctp_recvmsg(conn_fd, buffer, undef_size, &to, &tolen, &sinfo, &rflags);
}
