/*-
 * Copyright (c) 2024 Rozhuk Ivan <rozhuk.im@gmail.com>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * Author: Rozhuk Ivan <rozhuk.im@gmail.com>
 *
 */

/*
 * From the FreeBSD Bugzilla
 * https://bugs.freebsd.org/bugzilla/show_bug.cgi?id=278566
 */

#include <errno.h>
#include <inttypes.h>
#include <stdio.h>  /* snprintf, fprintf */
#include <string.h> /* memset */
#include <sys/param.h>
#include <sys/sysctl.h>
#include <sys/types.h>

/* cc sysctlbyname_broken.c -O0 -DDEBUG -o sysctlbyname_broken */
/* valgrind --tool=memcheck --leak-check=yes --leak-resolution=high
 * --track-origins=yes --undef-value-errors=yes --show-leak-kinds=all
 * --track-fds=yes --trace-children=no --vgdb=no --show-reachable=yes --verbose
 * --error-exitcode=1 ./sysctlbyname_broken */

/* Syntetic exapmle based on libdrm: xf86drm.c code. */

int main(int argc, char** argv)
{
   char   sysctl_name[32], sysctl_val[256];
   size_t sysctl_len;

   snprintf(sysctl_name, sizeof(sysctl_name), "kern.compiler_version");
   sysctl_len = sizeof(sysctl_val);
   memset(sysctl_val, 0x00, sizeof(sysctl_val));
   if (sysctlbyname(sysctl_name, sysctl_val, &sysctl_len, NULL, 0)) {
      fprintf(stdout, "sysctlbyname() - FAIL!\n");
   } else if (argc > 1) {
      fprintf(stdout, "%s\n", sysctl_val);
   }

   return (0);
}
