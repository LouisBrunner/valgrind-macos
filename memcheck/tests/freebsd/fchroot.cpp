#include <unistd.h>
#include <fcntl.h>

int main()
{
   int fd1;
   int* fd2{new int};

   fd1 = open("..", O_DIRECTORY | O_RDONLY);
   // will fail unless run as root
   fchroot(fd1);

   fchroot(*fd2);

   delete fd2;
}

