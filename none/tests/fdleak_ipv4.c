#include <sys/types.h>
#include <sys/wait.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include "fdleak.h"

void server ()
{
   int s, x;
   struct sockaddr_in baddr;
   struct sockaddr_in addr;
   socklen_t baddrsize = sizeof(baddr);
   int one = 1;
   
   s = DO( socket(PF_INET, SOCK_STREAM, 0) );

   DO( setsockopt(s, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(int)) );
   memset(&addr, 0, sizeof(addr));
   addr.sin_family = AF_INET;
   addr.sin_addr.s_addr = inet_addr("127.0.0.1");
   addr.sin_port = 12321;

   DO( bind(s, (struct sockaddr *)&addr, sizeof(addr)) );

   DO( listen(s, 5) );

   memset(&baddr, 0, sizeof(baddr));
   x = DO( accept(s, (struct sockaddr *)&baddr, &baddrsize) );

   DO( write(x, "hello", 6) );
}

void client ()
{
   int s, count = 0, ret;
   struct sockaddr_in addr;
   char buf[1024];

   s = DO( socket(PF_INET, SOCK_STREAM, 0) );

   addr.sin_family = AF_INET;
   addr.sin_addr.s_addr = inet_addr("127.0.0.1");
   addr.sin_port = 12321;

   do {
     count++;
     ret = connect(s, (struct sockaddr *)&addr, sizeof(addr));
     if (ret == -1) sleep(1);
   } while (count < 10 && ret == -1);

   if (ret == -1) {
      perror("connect");
      exit(1);
   }

   DO( read(s, buf, sizeof(buf)) );

   printf("%s\n", buf);
}


int main (int argc, char **argv)
{
   int pid, status;

   CLOSE_INHERITED_FDS;

   if ((pid = fork()) == 0) {
      server();
      return 0;
   }

   client();

   wait(&status);

   return 0;
}
