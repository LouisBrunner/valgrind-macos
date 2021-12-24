#include <sys/eventfd.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

#define handle_error(msg) \
do { \
perror(msg); \
exit(EXIT_FAILURE); \
} while (0)

int
main(int argc, char *argv[])
{
   eventfd_t u;
   int efd, j;
   int error;

   if (argc < 2) {
      fprintf(stderr, "Usage: %s <num>...\n", argv[0]);
      exit(EXIT_FAILURE);
   }

   efd = eventfd(0, EFD_CLOEXEC);
   if (efd == -1)
      handle_error("eventfd");

   switch (fork()) {
   case 0:
      for (j = 1; j < argc; j++) {
         printf("Child writing %s to efd\n", argv[j]);
         u = strtoull(argv[j], NULL, 0);

         error = eventfd_write(efd, u);
         if (error != 0)
            handle_error("write");
      }
      printf("Child completed write loop\n");

      exit(EXIT_SUCCESS);

   default:
      sleep(2);

      printf("Parent about to read\n");
      error = eventfd_read(efd, &u);
      if (error != 0)
         handle_error("read");
      printf("Parent read %llu (0x%llx) from efd\n",
             (unsigned long long) u, (unsigned long long) u);
      exit(EXIT_SUCCESS);

   case -1:

      handle_error("fork");

   }
}
