// This file determines MIPS features a processor supports.
//
// We return:
// - 0 if the machine matches the asked-for feature.
// - 1 if the machine does not.
// - 2 if the asked-for feature isn't recognised (this will be the case for
//     any feature if run on a non-MIPS machine).
// - 3 if there was a usage error (it also prints an error message).
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#define FEATURE_PRESENT       0
#define FEATURE_NOT_PRESENT   1
#define UNRECOGNISED_FEATURE  2
#define USAGE_ERROR           3

#if defined(VGA_mips32) || defined(VGA_mips64)
static int mipsCPUInfo(const char *search_string) {
   const char *file_name = "/proc/cpuinfo";
   /* Simple detection of MIPS DSP ASE at runtime for Linux.
   * It is based on /proc/cpuinfo, which reveals hardware configuration
   * to user-space applications. */

   char cpuinfo_line[256];

   FILE *f = NULL;
   if ((f = fopen (file_name, "r")) == NULL)
     return 0;

   while (fgets (cpuinfo_line, sizeof (cpuinfo_line), f) != NULL)
   {
     if (strstr (cpuinfo_line, search_string) != NULL)
     {
         fclose (f);
         return 1;
     }
   }

   fclose (f);

   /* Did not find string in the /proc/cpuinfo file. */
   return 0;
}

static int go(char *feature)
{
   int cpuinfo;
   if (strcmp(feature, "fpu") == 0) {
#if defined(__mips_hard_float)
      /* This is not a runtime detection.
         If mips_features is built as hard-float, the assumption is that
         the target MIPS platform has a floating-point unit. */
      return FEATURE_PRESENT;
#else
      return FEATURE_NOT_PRESENT;
#endif
   } else if (strcmp(feature, "mips-msa") == 0) {
      const char *msa = "msa";
      cpuinfo = mipsCPUInfo(msa);
      if (cpuinfo == 1) {
         return FEATURE_PRESENT;
      } else{
         return FEATURE_NOT_PRESENT;
      }
   } else if (strcmp(feature, "mips32-dsp") == 0) {
      const char *dsp = "dsp";
      cpuinfo = mipsCPUInfo(dsp);
      if (cpuinfo == 1) {
         return FEATURE_PRESENT;
      } else{
         return FEATURE_NOT_PRESENT;
      }
   } else if (strcmp(feature, "mips32-dspr2") == 0) {
      const char *dsp2 = "dsp2";
      cpuinfo = mipsCPUInfo(dsp2);
      if (cpuinfo == 1) {
         return FEATURE_PRESENT;
      } else{
         return FEATURE_NOT_PRESENT;
      }
   } else if (strcmp(feature, "cavium-octeon") == 0) {
      const char *cavium = "Cavium Octeon";
      cpuinfo = mipsCPUInfo(cavium);
      if (cpuinfo == 1) {
         return FEATURE_PRESENT;
      } else{
         return FEATURE_NOT_PRESENT;
      }
   } else if (strcmp(feature, "cavium-octeon2") == 0) {
      const char *cavium2 = "Cavium Octeon II";
      cpuinfo = mipsCPUInfo(cavium2);
      if (cpuinfo == 1) {
         return FEATURE_PRESENT;
      } else{
         return FEATURE_NOT_PRESENT;
      }
   } else if (strcmp(feature, "mips-be") == 0) {
#if defined (_MIPSEB)
     return FEATURE_PRESENT;
#else
     return FEATURE_NOT_PRESENT;
#endif
   } else if (strcmp(feature, "mipsr6") == 0) {
#if (__mips_isa_rev < 6)
      return FEATURE_NOT_PRESENT;
#else
      return FEATURE_PRESENT;
#endif
   } else {
      return UNRECOGNISED_FEATURE;
   }

}

#else

static int go(char *feature)
{
   /* Feature is not recognised. (non-MIPS machine!) */
   return UNRECOGNISED_FEATURE;
}

#endif


//---------------------------------------------------------------------------
// main
//---------------------------------------------------------------------------
int main(int argc, char **argv)
{
   if (argc != 2) {
      fprintf( stderr, "usage: mips_features <feature>\n" );
      exit(USAGE_ERROR);
   }
   return go(argv[1]);
}
