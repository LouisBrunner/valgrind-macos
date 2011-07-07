/* -*- mode: C; c-basic-offset: 3; -*- */

#include <setjmp.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>     // isspace
#include <fcntl.h>     // open
#include <unistd.h>    // lseek
#include <sys/stat.h>  // S_IRUSR

// This file determines s390x features a processor supports.
//
// We return:
// - 0 if the machine provides the asked-for feature and the cpu
//     model, if specified, matches the machine
// - 1 the machine does not provide the asked-for feature or the
//     cpu model, if specified, does not match the machine
// - 2 if the asked-for feature isn't recognised (this will be the case for
//     any feature if run on a non-s390x machine).
// - 2 for an unknown cpu model in /proc/cpu_info
// - 3 if there was a usage error (it also prints an error message).
//
// USAGE:
//
//    s390x_features <feature> [<machine-model>]
//
// The machine_model is optional and it can be something like:
//
//   z9        -- Host needs to be a z9 (and nothing else)
//   z9:       -- Host needs to be a z9 or any later model
//   :z9       -- Host needs to be a model up to and including z9
//   z900:z9   -- Host needs to be at least a z900 and at most a z9.
//                Any model in between is OK, too.

jmp_buf env;

#if defined(VGA_s390x)

void handle_sigill(int signum)
{
   longjmp(env, 1);
}

unsigned long long stfle(void)
{

   unsigned long long ret;

   signal(SIGILL, handle_sigill);
   if (setjmp(env)) {
      /* stfle not available: assume no facilities */
      return 0;
   } else {
      asm volatile("lghi 0, 0\n"
                   ".insn s,0xb2b00000,%0\n" /* stfle */
      : "=Q" (ret)::"0", "cc");
      return ret;
   }
}


/* Read /proc/cpuinfo. Look for lines like these

      processor 0: version = FF,  identification = 0117C9,  machine = 2064

   and return the machine model or NULL on error.
   Adapted from function VG_(get_machine_model) in coregrind/m_machine.c */

typedef struct {
   const char *cpuinfo_name;
   const char *real_name;
} model_info;

/* Array needs to be sorted chronologically. Oldest to newest */
model_info models[] = {
   { "2064", "z900"   },
   { "2066", "z800"   },
   { "2084", "z990"   },
   { "2086", "z890"   },
   { "2094", "z9-ec"  },
   { "2096", "z9-bc"  },
   { "2097", "z10-ec" },
   { "2098", "z10-bc" },
   { "2817", "z196"   },
};


/* Locate a machine model by name. Name can be either the cpuinfo
   name or the external name. */
static model_info *locate_model(const char *name)
{
   model_info *p;

   /* Try cpuinfo name first */
   for (p = models; p != models + sizeof models / sizeof models[0]; ++p) {
      if (strcmp(p->cpuinfo_name, name) == 0) return p;  // found it
   }

   /* Now try external name */
   for (p = models; p != models + sizeof models / sizeof models[0]; ++p) {
      if (strcmp(p->real_name, name) == 0) return p;  // found it
   }

   return NULL;
}


static model_info *get_host(void)
{
   int    n, fh;
   size_t num_bytes, file_buf_size;
   char  *p, *m, *model_name, *file_buf;
   model_info *model;

   /* Slurp contents of /proc/cpuinfo into FILE_BUF */
   //fh = open("/proc/cpuinfo", O_RDONLY, S_IRUSR);
   fh = open("/proc/cpuinfo", O_RDONLY, S_IRUSR);
   if (fh < 0) return NULL;

   /* Determine the size of /proc/cpuinfo.
      Work around broken-ness in /proc file system implementation.
      fstat returns a zero size for /proc/cpuinfo although it is
      claimed to be a regular file. */
   num_bytes = 0;
   file_buf_size = 1000;
   file_buf = malloc(file_buf_size + 1);

   while (42) {
      n = read(fh, file_buf, file_buf_size);
      if (n < 0) break;

      num_bytes += n;
      if (n < file_buf_size) break;  /* reached EOF */
   }

   if (n < 0) num_bytes = 0;   /* read error; ignore contents */

   if (num_bytes > file_buf_size) {
      free(file_buf);
      lseek(fh, 0, SEEK_SET);
      file_buf = malloc(num_bytes + 1);
      n = read(fh, file_buf, num_bytes);
      if (n < 0) num_bytes = 0;
   }

   file_buf[num_bytes] = '\0';
   close(fh);

   /* Parse file */
   model = models + sizeof models / sizeof models[0];
   for (p = file_buf; *p; ++p) {
      /* Beginning of line */
      if (strncmp(p, "processor", sizeof "processor" - 1 ) != 0) continue;

      m = strstr(p, "machine");
      if (m == NULL) continue;

      p = m + sizeof "machine" - 1;
      while (isspace(*p) || *p == '=') {
         if (*p == '\n') goto next_line;
         ++p;
      }

      model_name = p;
      for (n = 0; n < sizeof models / sizeof models[0]; ++n) {
         model_info *mm = models + n;
         size_t len = strlen(mm->cpuinfo_name);
         if (strncmp(mm->cpuinfo_name, model_name, len) == 0 &&
             isspace(model_name[len])) {
            /* In case there are different CPUs in this cluster return the
               one with the dewest capabilities ("oldest" model). */
            if (mm < model) model = mm;
            p = model_name + len;
            break;
         }
      }
      /* Skip until end-of-line */
      while (*p != '\n')
         ++p;
   next_line: ;
   }

   free(file_buf);

   if (model == models + sizeof models / sizeof models[0]) return NULL;

   return model;
}

static int go(char *feature, char *cpu)
{
   unsigned long long facilities;
   unsigned long long match;
   model_info *host, *from, *to, *p;
   char *colon;

   facilities = stfle();

   if        (strcmp(feature, "s390x-zarch") == 0 ) {
     match = (facilities & (1ULL << 62) && (facilities & (1ULL << 61)));
   } else if (strcmp(feature, "s390x-n3") == 0 ) {
     match = (facilities & (1ULL << 63));
   } else if (strcmp(feature, "s390x-stfle") == 0 ) {
     match = (facilities & (1ULL << 56));
   } else if (strcmp(feature, "s390x-ldisp") == 0 ) {
     match = (facilities & (1ULL << 45) && (facilities & (1ULL << 44)));
   } else if (strcmp(feature, "s390x-eimm") == 0 ) {
     match = (facilities & (1ULL << 42));
   } else if (strcmp(feature, "s390x-stckf") == 0 ) {
     match = (facilities & (1ULL << 38));
   } else if (strcmp(feature, "s390x-genins") == 0 ) {
     match = (facilities & (1ULL << 29));
   } else if (strcmp(feature, "s390x-exrl") == 0 ) {
     match = (facilities & (1ULL << 28));
   } else {
     return 2;          // Unrecognised feature.
   }

   if (match == 0) return 1;   // facility not provided

   /* Host provides facility. If no CPU was specified, we're done. */
   if (cpu == NULL) return 0;

   host = get_host();
   if (host == NULL) return 2;  // unknown model

   //   printf("host = %s (%s)\n", host->cpuinfo_name, host->real_name);

   /* Determine interval of models in which to search for HOST. */
   from = to = NULL;
   colon = strchr(cpu, ':');

   if (colon == NULL) {
      // match exact
      from = to = locate_model(cpu);
   } else if (colon == cpu) {
      // :NAME  match machines up to and including CPU
      from = models;
      to   = locate_model(cpu + 1);
   } else if (colon[1] == '\0') {
      // NAME:  match machines beginning with CPU or later
      *colon = '\0';
      from = locate_model(cpu);
      to   = models + sizeof models / sizeof models[0] - 1;
      *colon = ':';
   } else {
      // NAME:NAME  match machines in interval
      *colon = '\0';
      from = locate_model(cpu);
      to   = locate_model(colon + 1);
      *colon = ':';
   }

   if (from == NULL || to == NULL || from > to) {
      fprintf(stderr, "invalid cpu specification '%s'\n", cpu);
      return 3;
   }

#if 0
   printf("from  %s (%s)  to  %s (%s)\n", from->cpuinfo_name, from->real_name,
          to->cpuinfo_name, to->real_name);
#endif

   /* Search for HOST. */
   for (p = from; p <= to; ++p) {
      if (p == host) return 0;
   }

   return 1; // host does not match CPU specification
}

#else

static int go(char *feature, char *cpu)
{
   return 2;      // Feature not recognised (non-s390x machine!)
}

#endif


//---------------------------------------------------------------------------
// main
//---------------------------------------------------------------------------
int main(int argc, char **argv)
{
   int rc;

   if (argc < 2 || argc > 3) {
      fprintf( stderr, "usage: s390x_features <feature> [<machine-model>]\n" );
      exit(3);                // Usage error.
   }

   rc = go(argv[1], argv[2]);

   //   printf("rc = %d\n", rc);

   return rc;
}
