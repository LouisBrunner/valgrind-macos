/* Test that /proc/{self,$PID}/auxv can be opened and read simultaneously
   using two different file descriptors. */

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <sys/auxv.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

/* Reads one auxv_t entry from the input file. */
int read_entry(int fi, auxv_t *out)
{
   size_t toread = sizeof(*out);
   char *ptr = (char*)out;

   while (toread) {
      ssize_t r;

      r = read(fi, ptr, toread);
      if (r == 0) {
         fprintf(stderr, "unexpected EOF\n");
         return 1;
      }
      if (r == -1) {
         perror("read");
         return 1;
      }

      ptr += r;
      toread -= r;

      assert(toread >= 0);
   }

   return 0;
}

int main(void)
{
   auxv_t vector[2][4];
   int fi[2] = {-1, -1};
   size_t i;
   int res = 1;

   /* Open the first input file. */
   if ((fi[0] = open("/proc/self/auxv", O_RDONLY)) == -1) {
      perror("open");
      goto out;
   }

   /* Read the first two entries from the first file. */
   for (i = 0; i < 2; i++)
      if (read_entry(fi[0], &vector[0][i]))
         goto out;

   /* Open the second input file. */
   if ((fi[1] = open("/proc/self/auxv", O_RDONLY)) == -1) {
      perror("open");
      goto out;
   }

   /* Read the first two entries from the first file. */
   for (i = 2; i < 4; i++)
      if (read_entry(fi[0], &vector[0][i]))
         goto out;

   /* Read the first four entries from the second file. */
   for (i = 0; i < 4; i++)
      if (read_entry(fi[1], &vector[1][i]))
         goto out;

   /* Compare read vectors. */
   if (memcmp(vector[0], vector[1], 4 * sizeof(vector[0][0]))) {
      fprintf(stderr, "vectors differ\n");
      goto out;
   }

   res = 0;

out:
   for (i = 0; i < 2; i++)
      if (fi[i] >= 0)
         close(fi[i]);

   return res;
}

