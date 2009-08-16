// This is a test for access_extended(), one of the more ridiculous syscalls
// ever devised.  For bug 200760.  See the comments on the wrapper in
// syswrap-darwin.c to understand what is going on here.

#include <sys/syscall.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(void)
{
   char* name1 = "access_extended.c";
   char* name2 = "no_such_file";
   // Space for three descriptors and the two strings (and NUL chars for them).
   size_t entries_szB =
      sizeof(struct accessx_descriptor) * 3 +
      strlen(name1) + 1 +
      strlen(name2) + 1;
   struct accessx_descriptor* entries = malloc(entries_szB);
   char* string1 = (char*)&entries[3];
   char* string2 = string1 + strlen(name1) + 1;
   int results[3];
   int retval;

   entries[0].ad_name_offset = string1 - (char*)entries;
   entries[1].ad_name_offset = 0;   // reuse the previous entry's string
   entries[2].ad_name_offset = string2 - (char*)entries;
   entries[0].ad_flags       = F_OK;      // succeeds
   entries[1].ad_flags       = X_OK;      // fails
   entries[2].ad_flags       = F_OK;      // fails
   strcpy(string1, name1);
   strcpy(string2, name2);

   retval = syscall(SYS_access_extended, entries, entries_szB, results,
                    /*uid--unused?*/0);

   fprintf(stderr, "retval = %d\n", retval);
   fprintf(stderr, "%s(F_OK) = %d (%s)\n",
      name1, results[0], strerror(results[0]));
   fprintf(stderr, "%s(X_OK) = %d (%s)\n",
      name1, results[1], strerror(results[1]));
   fprintf(stderr, "%s(F_OK) = %d (%s)\n",
      name2, results[2], strerror(results[2]));

   return 0;
}

