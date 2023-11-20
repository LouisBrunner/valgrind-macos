#include <unistd.h>
#include <stdio.h>

int main(int argc, char** argv)
{
   char buffer[] = "boxbackup.recombinetemp.1.XXXXXX";
   if(mkstemp(buffer) < 0)
   {
      perror("mkstemp");
   }
   return 0;
}       
