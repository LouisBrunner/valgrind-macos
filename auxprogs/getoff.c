#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <config.h>

#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <link.h>
#include <dlfcn.h>

/* true if arg matches the provided option */
static
int is_opt(char* arg, const char *option)
{
   int option_len = strlen(option);
   if (option[option_len-1] == '=')
      return (0 == strncmp(option, arg, option_len));
   else
      return (0 == strcmp(option, arg));
}

static int verbose = 0;

static
void usage (char* progname)
{
   fprintf(stderr,
"Usage: %s [--help] [-h] [-v] [-o <outputfile>]\n"
"Outputs various user space offsets\n"
"By default, outputs on stdout.\n"
"Use -o to output to <outputfile>\n"
"-v : be more verbose\n",
progname);

}
/* Currently, only computes and output lm_modid_offset in struct link_map
   of the dynamic linker. In theory, we should also compute the offset needed
   to get the dtv from the thread register/pointer/...
   Currently, the various valgrind-low-xxxxxx.c files are hardcoding this
   offset as it is deemed (?) stable, and there is no clear way how to
   compute this dtv offset.
*/
int main (int argc, char** argv)
{
   int i;
   FILE *outputfile;
   int nr_errors = 0;
   
   outputfile = stdout;

   i = 1;
   while (i < argc) {
      if (is_opt(argv[i], "--help") || is_opt(argv[i], "-h")) {
         usage(argv[0]);
         exit(0);
      } else if (is_opt(argv[i], "-v")) {
         verbose++;
      } else if (is_opt(argv[i], "-o")) {
         if (i+1 == argc) {
            fprintf(stderr, 
                    "missing output file for -o option\n"
                    "Use --help for more information.\n");
            exit (1);
         }
         i++;
         outputfile = fopen(argv[i], "w");
         if (outputfile == NULL) {
            fprintf(stderr, "Could not fopen %s in write mode\n", argv[i]);
            perror ("fopen output file failed");
            exit (1);
         }
      } else {
         fprintf (stderr, 
                  "unknown or invalid argument %s\n"
                  "Use --help for more information.\n",
                  argv[i]);
         exit(1);
      }
      i++;
   }

#ifdef HAVE_DLINFO_RTLD_DI_TLS_MODID
   /* Compute offset of lm_modid in struct link_map.
      This is needed to support QGetTlsAddr gdbsrv query.
      Computation is done using an ugly hack, but less ugly than
      hardcoding the offset depending on the glibc version and
      platform.
      The below works, based the assumption that RTLD_DI_TLS_MODID
      just access and returns directly the field in the dummy
      link_map structure we have prepared.

      If glibc debug info is installed on your system, you can
      also find this offset by doing in GDB:
          p &((struct link_map*)0x0)->l_tls_modid
      (see also coregrind/m_gdbserver/valgrind_low.h target_get_dtv
       comments).
   */
   {
      #define MAX_LINKMAP_WORDS 10000
      size_t dummy_link_map[MAX_LINKMAP_WORDS];
      size_t off;
      size_t modid_offset;
      for (off = 0; off < MAX_LINKMAP_WORDS; off++)
         dummy_link_map[off] = off;
      if (dlinfo ((void*)dummy_link_map, RTLD_DI_TLS_MODID, 
                  &modid_offset) == 0) {
         assert(modid_offset >= 0 && modid_offset < MAX_LINKMAP_WORDS);
         fprintf(outputfile,
                 "lm_modid_offset 0x%x\n", modid_offset*sizeof(size_t));
      } else {
         fprintf(stderr, 
                 "Error computing lm_modid_offset.\n"
                 "dlinfo error %s\n", dlerror());
         nr_errors++;
      }
      #undef MAX_LINKMAP_WORDS
   }
   
   if (outputfile != stdout)
      if (fclose (outputfile) != 0) {
         perror ("fclose output file failed\n");
         nr_errors++;
      }
#else
   if (verbose)
      fprintf(stderr, 
              "cannot compute lm_modid_offset.\n"
              "configure did not define HAVE_DLINFO_RTLD_DI_TLS_MODID.\n");
#endif

   if (nr_errors == 0)
      exit(0);
   else
      exit(1);
}
