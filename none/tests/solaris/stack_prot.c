#include <procfs.h>
#include <stdio.h>

#define ARRAY_LENGTH(array) (sizeof((array)) / sizeof(0[(array)]))

int main(void)
{
   int res = 1;
   FILE *fi = NULL;
   prxmap_t map[32];
   int found = 0;
   int done = 0;
   /* Obtain an address inside the stack using a dirty trick. */
   uintptr_t local = (uintptr_t)&fi;

   /* Open /proc/self/xmap for reading. */
   fi = fopen("/proc/self/xmap", "r");
   if (!fi) {
      perror("fopen");
      goto out;
   }

   /* Read the file until EOF or the stack is found. */
   while (!done && !found) {
      size_t i, items;

      items = fread(map, sizeof(map[0]), ARRAY_LENGTH(map), fi);
      if (items != ARRAY_LENGTH(map)) {
         if (ferror(fi)) {
            perror("fread");
            goto out;
         }
         done = 1;
      }

      /* Scan the read mappings. */
      for (i = 0; i < items; i++)
         if (map[i].pr_vaddr <= local
             && local < map[i].pr_vaddr + map[i].pr_size) {
            /* Stack was found, validate it. */
            found = 1;
            if ((map[i].pr_mflags & (MA_READ | MA_WRITE | MA_EXEC))
                != (MA_READ | MA_WRITE)) {
               fprintf(stderr, "Incorrect stack mapping detected.\n");
               goto out;
            }
         }
   }

   /* Check if the stack was indeed found. */
   if (!found) {
      fprintf(stderr, "Stack not found.\n");
      goto out;
   }

   res = 0;

out:
   /* Cleanup. */
   if (fi)
      fclose(fi);

   return res;
}

