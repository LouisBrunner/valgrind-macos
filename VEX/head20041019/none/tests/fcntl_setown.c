#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <unistd.h>

int main(int argc, char **argv)
{
   int s;
  
   if ((s = socket(PF_INET, SOCK_STREAM, 0)) < 0)
   {
      perror("socket");
      exit(1);
   }

   if (fcntl(s, F_SETOWN, getpid()) < 0)
   {
      perror("fcntl(F_SETOWN)");
      exit(1);
   }
   
   if (close(s) < 0)
   {
      perror("close");
      exit(1);
   }
  
   exit(0);
}
