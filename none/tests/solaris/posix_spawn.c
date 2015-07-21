/* Test that an error from posix_spawn() is correctly propagated to the
   parent. */

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <spawn.h>

int main(void)
{
   int res = 1;
   int err;
   posix_spawn_file_actions_t file_actions;
   char *argv_exe[] = {"true", NULL};
   char *envv_exe[] = {NULL};

   err = posix_spawn_file_actions_init(&file_actions);
   if (err != 0) {
      errno = err;
      perror("posix_spawn_file_actions_init");
      return 1;
   }

   err = posix_spawn_file_actions_adddup2(&file_actions, 3, 4);
   if (err != 0) {
      errno = err;
      perror("posix_spawn_file_actions_adddup2");
      goto out;
   }

   /* The following call to posix_spawn() should fail because the requested
      dup2 action cannot be performed. */
   err = posix_spawn(NULL, "/bin/true", &file_actions, NULL, argv_exe,
                     envv_exe);
   if (err != 0) {
      errno = err;
      perror("posix_spawn");
      goto out;
   }

   res = 0;

out:
   err = posix_spawn_file_actions_destroy(&file_actions);
   if (err != 0) {
      errno = err;
      perror("posix_spawn_file_actions_destroy");
      res = 1;
   }

   return res;
}

