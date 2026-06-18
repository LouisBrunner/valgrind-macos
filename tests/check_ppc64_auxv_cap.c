/* ---------------------------------------------------------------------
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2026  Abhay Kandpal <abhay@linux.ibm.com>

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
   ------------------------------------------------------------------ */

/* check_ppc64_auxv_cap.c

   Replacement for the historical 'tests/check_ppc64_auxv_cap' shell
   script.  Used as a .vgtest 'prereq:' helper by ~51 tests under
   none/tests/ppc{32,64}/ and memcheck/tests/ppc{32,64}/ to decide,
   at test-run time, whether the host machine supports a given
   PowerPC capability (altivec, vsx, dfp, arch_2_05, arch_2_06,
   arch_2_07, arch_3_00, arch_3_1, htm, mma).

   The original shell script parsed the textual output of
       LD_SHOW_AUXV=1 /bin/true | grep ^AT_HWCAP
   for human-readable capability words such as 'altivec'.  glibc 2.42
   removed the PowerPC-specific _dl_procinfo() hook (formerly in
   sysdeps/powerpc/dl-procinfo.h, commits 6cb703b / 1d60b9d), so
   LD_SHOW_AUXV now prints raw hex for AT_HWCAP / AT_HWCAP2 and the
   'grep -w altivec' pattern in the script never matches.  The
   resulting non-zero exit caused vg_regtest.in to skip every test
   that depends on this prereq, silently disabling essentially the
   whole PowerPC instruction-level regression suite.

   This C version uses getauxval() to read AT_HWCAP / AT_HWCAP2
   directly.  That is the documented, stable interface to the auxv,
   and it is unaffected by the glibc change.

   Calling convention (preserved from the old script so the existing
   .vgtest files need no modification):
       check_ppc64_auxv_cap <capability>
       exit code 0  - capability is present
       exit code 1  - capability is absent, unknown, or usage error
*/

#include "config.h"
#include <stdio.h>
#include <string.h>

#ifdef HAVE_GETAUXVAL

#include <sys/auxv.h>

/* Capability bit table.  Values are taken from
   /usr/include/bits/hwcap.h on a glibc-2.41-or-earlier system, and
   cross-checked against arch/powerpc/include/uapi/asm/cputable.h in
   the Linux kernel.  AT_HWCAP / AT_HWCAP2 bit assignments are part of
   the kernel ABI and are stable, so it is safe to hardcode them here
   rather than depend on the PowerPC-specific bits/hwcap.h header
   being installed at build time. */

typedef
   struct {
      const char*   name;
      unsigned long type;   /* AT_HWCAP or AT_HWCAP2 */
      unsigned long bit;
   }
   CapEntry;

static const CapEntry cap_table[] = {
   /* AT_HWCAP  */
   { "altivec",   AT_HWCAP,  0x10000000UL },
   { "vsx",       AT_HWCAP,  0x00000080UL },
   { "dfp",       AT_HWCAP,  0x00000400UL },
   { "arch_2_05", AT_HWCAP,  0x00001000UL },
   { "arch_2_06", AT_HWCAP,  0x00000100UL },
   /* AT_HWCAP2 */
   { "arch_2_07", AT_HWCAP2, 0x80000000UL },
   { "arch_3_00", AT_HWCAP2, 0x00800000UL },
   { "arch_3_1",  AT_HWCAP2, 0x00040000UL },
   { "htm",       AT_HWCAP2, 0x40000000UL },
   { "mma",       AT_HWCAP2, 0x00020000UL },
};

#define N_CAP_TABLE  (sizeof cap_table / sizeof cap_table[0])

int main(int argc, char** argv)
{
   unsigned int  i;
   unsigned long hwcap;
   const char*   progname
      = (argc > 0 && argv[0] != NULL) ? argv[0] : "check_ppc64_auxv_cap";

   if (argc != 2) {
      fprintf(stderr, "usage: %s <capability>\n", progname);
      return 1;
   }

   for (i = 0; i < N_CAP_TABLE; i++) {
      if (strcmp(argv[1], cap_table[i].name) == 0) {
         hwcap = getauxval(cap_table[i].type);
         return (hwcap & cap_table[i].bit) ? 0 : 1;
      }
   }

   /* Unknown capability name.  Report 'not found', matching the
      behaviour of the original grep-based script when the requested
      capability word did not appear in the LD_SHOW_AUXV output. */
   fprintf(stderr, "%s: unknown capability '%s'\n", progname, argv[1]);
   return 1;
}

#else /* !HAVE_GETAUXVAL */

/* getauxval() is not available on this platform (e.g. FreeBSD, Solaris,
   macOS).  None of those platforms currently use Valgrind's PowerPC
   port, but the program must still build and link there.  Return 1 for
   every query - equivalent to "capability not detected" - which causes
   the .vgtest harness to skip any test using this as a prereq. */

int main(int argc, char** argv)
{
   (void)argc;
   (void)argv;
   return 1;
}

#endif /* HAVE_GETAUXVAL */
