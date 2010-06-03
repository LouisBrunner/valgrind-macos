
/* This program handles linking the tool executables, statically and
   at an alternative load address.  Linking them statically sidesteps
   all sorts of complications to do with having two copies of the
   dynamic linker (valgrind's and the client's) coexisting in the same
   process.  The alternative load address is needed because Valgrind
   itself will load the client at whatever address it specifies, which
   is almost invariably the default load address.  Hence we can't
   allow Valgrind itself (viz, the tool executable) to be loaded at
   that address.

   Unfortunately there's no standard way to do 'static link at
   alternative address', so this program handles the per-platform
   hoop-jumping.
*/

/* What we get passed here is:
   first arg
      the alternative load address
   all the rest of the args
      the gcc invokation to do the final link, that
      the build system would have done, left to itself

   We just let assertions fail rather than do proper error reporting.
   We don't expect the users to run this directly.  It is only run
   from as part of the build process, with carefully constrained
   inputs.
*/

/* ------------------------- LINUX ------------------------- */

#if defined(VGO_linux)

/* Scheme is simple: pass the specified command to the linker as-is,
   except, add "-static" and "-Ttext=<argv[1]>" to it.

   Also apparently we need --build-id=none.  For older ld's (2.18
   vintage) the first two flags are fine.  For newer ones (2.20), a
   .note.gnu.build-id is nevertheless created at the default text
   segment address, which of course means the resulting executable is
   unusable.  So we have to tell ld not to generate that, with
   --build-id=none.

   As to "how far back is this flag supported", it's available at
   least in ld 2.18 and 2.20 and gold 2.20.
*/

// Don't NDEBUG this; the asserts are necesary for
// safety checks.
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>  /* WEXITSTATUS */

int main ( int argc, char** argv )
{
   int         i;
   int/*bool*/ failed = 0;
   size_t      reqd = 0;

   // expect at least: alt-load-address gcc -o foo bar.o
   assert(argc > 5);

   // check for plausible-ish alt load address
   char* ala = argv[1];
   assert(ala[0] == '0');
   assert(ala[1] == 'x');

   // We'll need to invoke this to do the linking
   char* gcc = argv[2];

   // and the 'restargs' are argv[3 ..]

   // so, build up the complete command here:
   // 'gcc' -static -Ttext='ala' 'restargs'

   // first, do length safety checks
   reqd += 1+ strlen(gcc);
   reqd += 1+ 100/*let's say*/ + strlen(ala);
   for (i = 3; i < argc; i++)
      reqd += 1+ strlen(argv[i]);

   reqd += 1;
   char* cmd = calloc(reqd,1);
   assert(cmd);

   char ttext[100];
   assert(strlen(ala) < 30);
   memset(ttext, 0, sizeof(ttext));
   sprintf(ttext, " -static -Wl,-Ttext=%s -Wl,--build-id=none", ala);

   strcpy(cmd, gcc);
   strcat(cmd, ttext);
   for (i = 3; i < argc; i++) {
     strcat(cmd, " ");
     strcat(cmd, argv[i]);
   }

   assert(cmd[reqd-1] == 0);

   if (0) printf("\n");
   printf("link_tool_exe: %s\n", cmd);
   if (0) printf("\n");

   int r = system(cmd);
   if (r == -1 || WEXITSTATUS(r) != 0)
      failed = 1;

   free(cmd);

   // return the result of system.
   return failed ? 1 : 0;
}

/* ------------------------- DARWIN ------------------------ */

#elif defined(VGO_darwin)

/* Run the specified command as-is; ignore the specified load address
   (argv[1]). */

// Don't NDEBUG this; the asserts are necesary for
// safety checks.
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

int main ( int argc, char** argv )
{
   int         i;
   int/*bool*/ failed = 0;
   size_t      reqd = 0;

   // expect at least: alt-load-address gcc -o foo bar.o
   assert(argc > 5);

   // check for plausible-ish alt load address
   char* ala = argv[1];
   assert(ala[0] == '0');
   assert(ala[1] == 'x');

   // command to run is argv[2 ..]

   // so, build up the complete command here:
   // argv[2 ..]

   // first, do length safety checks
   for (i = 2; i < argc; i++)
      reqd += 1+ strlen(argv[i]);

   reqd += 1;
   char* cmd = calloc(reqd,1);
   assert(cmd);

   for (i = 2; i < argc; i++) {
     strcat(cmd, " ");
     strcat(cmd, argv[i]);
   }

   assert(cmd[reqd-1] == 0);

   if (0) printf("\n");
   printf("link_tool_exe: %s\n", cmd);
   if (0) printf("\n");

   int r = system(cmd);
   if (r == -1 || WEXITSTATUS(r) != 0)
      failed = 1;

   free(cmd);

   // return the result of system.
   return failed ? 1 : 0;
}


#else
#  error "Unsupported OS"
#endif
