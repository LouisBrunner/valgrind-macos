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

void
server ()
{
   int s, x;
   struct sockaddr_in baddr;
   struct sockaddr_in addr;
   int baddrsize = sizeof(baddr);
   int one = 1;
   
   s = socket(PF_INET, SOCK_STREAM, 0);
   if(s == -1) {
      perror("socket");
      exit(1);
   }

   setsockopt(s, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(int));
   memset(&addr, 0, sizeof(addr));
   addr.sin_family = AF_INET;
   addr.sin_addr.s_addr = inet_addr("127.0.0.1");
   addr.sin_port = 12321;

   if(bind(s, (struct sockaddr *)&addr, sizeof(addr)) == -1) {
      perror("bind");
      exit(1);
   }

   if(listen(s, 5) == -1) {
      perror("listen");
      exit(1);
   }

   memset(&baddr, 0, sizeof(baddr));
   x = accept(s, (struct sockaddr *)&baddr, &baddrsize);
   if(x == -1) {
      perror("accept");
      exit(1);
   }

   write(x, "hello", 6);
}

void
client ()
{
   int s, count = 0, ret;
   struct sockaddr_in addr;
   char buf[1024];

   s = socket(PF_INET, SOCK_STREAM, 0);
   if(s == -1) {
      perror("socket");
      exit(1);
   }

   addr.sin_family = AF_INET;
   addr.sin_addr.s_addr = inet_addr("127.0.0.1");
   addr.sin_port = 12321;

   do {
     count++;
     ret = connect(s, (struct sockaddr *)&addr, sizeof(addr));
     if(ret == -1) sleep(1);
   } while (count < 10 && ret == -1);

   if(ret == -1) {
      perror("connect");
      exit(1);
   }

   read(s, buf, sizeof(buf));

   printf("%s\n", buf);
}


int
main (int argc, char **argv)
{
   int pid, status;

   /*
    * Fedora Core 1's Perl opens /dev/pts/2 as fd 10.  Let's close it
    * now to get consistent results across different releases.
    */

   close(10);

   if((pid = fork()) == 0) {
      server();
      return 0;
   }

   client();

   wait(&status);

   return 0;
}
