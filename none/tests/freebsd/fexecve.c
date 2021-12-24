#include <fcntl.h> // open
#include <stdio.h> // perror
#include <unistd.h> // getopt
#include <stdlib.h> // exit

int main(int argc, char **argv, char** envp)
{
   char *exe = "./hello_world";
   int open_flags = 0;
   int opt;

   while ((opt = getopt(argc, argv, "erst")) != -1)
   {
      switch (opt)
      {
         case 'e':
            open_flags |= O_EXEC;
            break;
         case 'r':
            open_flags |= O_RDONLY;
            break;
         case 's':
            exe = "./test.sh";
            break;
         case 't':
            exe = "./fexecve.c";
            break;
         default:
            fprintf(stderr, "bad usage, options are\n"
                            "\texec flag\t-e\n"
                            "\trdonly flag\t-r\n"
                            "\texec script\t-s\n"
                            "\ntext file\n-t");
            exit(-1);
      }
   }

   int fd = open(exe, open_flags);
   if (-1 == fd)
   {
      perror("open failed:");
      exit(-1);
   }
   char *new_argv[] = {
      exe,
      NULL
   };
   if (-1 == fexecve(fd, new_argv, envp))
   {
      perror("fexecv failed:");
      exit(-1);
   }
}
