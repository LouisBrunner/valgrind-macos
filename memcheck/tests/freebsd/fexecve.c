#include <fcntl.h> // open
#include <stdio.h> // perror
#include <string.h> // strdup
#include <stdlib.h> // exit
#include <unistd.h> // fexecve

int main(int argc, char **argv, char** envp)
{
   char *exe = "/usr/bin/true";

   int fd = open(exe, O_RDONLY);
   if (-1 == fd)
   {
      perror("open failed:");
      exit(-1);
   }
   char ** new_argv = malloc(2*sizeof(char *));
   char ** new_envp = malloc(2*sizeof(char *));
   char * arg1 = strdup("./fexecve");
   char * env1 = strdup("FOO=bar");
   int * new_fd = malloc(sizeof(int));
   *new_fd += fd;
   new_argv[1] = new_envp[1] = NULL;
   new_argv[0] = arg1;
   new_envp[0] = env1;

   free(arg1);
   free(env1);
   if (-1 == fexecve(*new_fd, new_argv, new_envp))
   {
      perror("fexecv failed:");
      exit(-1);
   }
}
