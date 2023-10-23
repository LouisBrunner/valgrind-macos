
/*--------------------------------------------------------------------*/
/*--- Launching valgrind                              m_launcher.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2009 Julian Seward
      jseward@acm.org
   Copyright (C) 2018-2021 Paul Floyd
      pjfloyd@wanadoo.fr

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

/* Note: this is a "normal" program and not part of Valgrind proper,
   and so it doesn't have to conform to Valgrind's arcane rules on
   no-glibc-usage etc. */

#include <assert.h>
#include <ctype.h>
#include <elf.h>
#include <errno.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/sysctl.h>
/* #include <sys/user.h> */
#include <unistd.h>
#include <limits.h>

#include "pub_core_debuglog.h"
#include "pub_core_vki.h"       // Avoids warnings from
// pub_core_libcfile.h
#include "pub_core_libcproc.h"  // For VALGRIND_LIB, VALGRIND_LAUNCHER
#include "pub_core_ume.h"

#ifndef EM_X86_64
#define EM_X86_64 62    // elf.h doesn't define this on some older systems
#endif

/* Report fatal errors */
__attribute__((noreturn))
static void barf ( const char *format, ... )
{
   va_list vargs;

   va_start(vargs, format);
   fprintf(stderr, "valgrind: Cannot continue: ");
   vfprintf(stderr, format, vargs);
   fprintf(stderr, "\n");
   va_end(vargs);

   exit(1);
   /*NOTREACHED*/
   assert(0);
}

/* Search the path for the client program */
static const char *find_client(const char *clientname)
{
   static char fullname[PATH_MAX];
   const char *path = getenv("PATH");
   const char *colon;

   while (path) {
      if ((colon = strchr(path, ':')) == NULL) {
         strlcpy(fullname, path, PATH_MAX);
         path = NULL;
      } else {
         memcpy(fullname, path, colon - path);
         fullname[colon - path] = '\0';
         path = colon + 1;
      }

      strlcat(fullname, "/", PATH_MAX);
      strlcat(fullname, clientname, PATH_MAX);

      if (access(fullname, R_OK|X_OK) == 0) {
         return fullname;
      }
   }

   return clientname;
}

/* Examine the client and work out which platform it is for */
static const char *select_platform(const char *clientname)
{
   int fd;
   char header[4096];
   ssize_t n_bytes;
   const char *platform = NULL;

   VG_(debugLog)(2, "launcher", "selecting platform for '%s'\n", clientname);

   if (strchr(clientname, '/') == NULL) {
      clientname = find_client(clientname);
   }

   if ((fd = open(clientname, O_RDONLY)) < 0) {
      return NULL;
   }
   //   barf("open(%s): %s", clientname, strerror(errno));

   n_bytes = read(fd, header, sizeof(header));
   close(fd);
   if (n_bytes < 2) {
      return NULL;
   }

   if (header[0] == '#' && header[1] == '!') {
      int i = 2;
      char *interp = (char *)header + 2;

      // Skip whitespace.
      while (1) {
         if (i == n_bytes) {
            return NULL;
         }
         if (' ' != header[i] && '\t' != header[i]) {
            break;
         }
         i++;
      }

      // Get the interpreter name.
      interp = &header[i];
      while (1) {
         if (i == n_bytes) {
            break;
         }
         if (isspace(header[i])) {
            break;
         }
         i++;
      }
      if (i == n_bytes) {
         return NULL;
      }
      header[i] = '\0';

      platform = select_platform(interp);

   } else if (n_bytes >= SELFMAG && memcmp(header, ELFMAG, SELFMAG) == 0) {

      if ((size_t)n_bytes >= sizeof(Elf32_Ehdr) && header[EI_CLASS] == ELFCLASS32) {
         const Elf32_Ehdr *ehdr = (Elf32_Ehdr *)header;

         if (header[EI_DATA] == ELFDATA2LSB) {
            if (ehdr->e_machine == EM_386 &&
                  ehdr->e_ident[EI_OSABI] == ELFOSABI_FREEBSD) {
               platform = "x86-freebsd";
            }
         }
      } else if ((size_t)n_bytes >= sizeof(Elf64_Ehdr) && header[EI_CLASS] == ELFCLASS64) {
         const Elf64_Ehdr *ehdr = (Elf64_Ehdr *)header;

         if (header[EI_DATA] == ELFDATA2LSB) {
            if (ehdr->e_machine == EM_X86_64 &&
                  ehdr->e_ident[EI_OSABI] == ELFOSABI_FREEBSD) {
               platform = "amd64-freebsd";
            }
         }
      }
   }

   VG_(debugLog)(2, "launcher", "selected platform '%s'\n",
                 platform ? platform : "unknown");

   return platform;
}

/* Where we expect to find all our aux files */
static const char *valgrind_lib = VG_LIBDIR;

int main(int argc, char** argv, char** envp)
{
   int i, j, loglevel, r;
   const char *toolname = NULL;
   const char *clientname = NULL;
   const char *platform;
   const char *default_platform = VG_PLATFORM;
   const char *cp;
   char *toolfile;
   char launcher_name[PATH_MAX+1];
   char* new_line;
   char** new_env;
   int oid[4];
   vki_size_t len;

   /* Start the debugging-log system ASAP.  First find out how many
      "-d"s were specified.  This is a pre-scan of the command line.
      At the same time, look for the tool name. */
   loglevel = 0;
   for (i = 1; i < argc; i++) {
      if (argv[i][0] != '-') {
         clientname = argv[i];
         break;
      }
      if (0 == strcmp(argv[i], "--")) {
         if (i+1 < argc) {
            clientname = argv[i+1];
         }
         break;
      }
      if (0 == strcmp(argv[i], "-d")) {
         loglevel++;
      }
      if (0 == strncmp(argv[i], "--tool=", 7)) {
         toolname = argv[i] + 7;
      }
   }

   /* ... and start the debug logger.  Now we can safely emit logging
      messages all through startup. */
   VG_(debugLog_startup)(loglevel, "Stage 1");

   /* Make sure we know which tool we're using */
   if (toolname) {
      VG_(debugLog)(1, "launcher", "tool '%s' requested\n", toolname);
   } else {
      VG_(debugLog)(1, "launcher",
                    "no tool requested, defaulting to 'memcheck'\n");
      toolname = "memcheck";
   }

   /* Work out what platform to use, or use the default platform if
      not possible. */
   if (clientname == NULL) {
      VG_(debugLog)(1, "launcher",
                    "no client specified, defaulting platform to '%s'\n",
                    default_platform);
      platform = default_platform;
   } else if ((platform = select_platform(clientname)) != NULL) {
      VG_(debugLog)(1, "launcher", "selected platform '%s'\n", platform);
   } else {
      VG_(debugLog)(1, "launcher",
                    "no platform detected, defaulting platform to '%s'\n",
                    default_platform);
      platform = default_platform;
   }

   /* Figure out the name of this executable (viz, the launcher), so
      we can tell stage2.  stage2 will use the name for recursive
      invocations of valgrind on child processes. */
   memset(launcher_name, 0, PATH_MAX+1);

   oid[0] = CTL_KERN;
   oid[1] = KERN_PROC;
   oid[2] = KERN_PROC_PATHNAME;
   oid[3] = getpid();
   len = PATH_MAX;
   r = sysctl(oid, 4, launcher_name, &len, 0, 0);
   if (r != 0) {
      fprintf(stderr, "valgrind: warning (non-fatal): "
              "sysctl(\"kern.proc.pathname\") failed.\n");
      fprintf(stderr, "valgrind: continuing, however --trace-children=yes "
              "will not work.\n");
   }

   /* tediously augment the env: VALGRIND_LAUNCHER=launcher_name */
   new_line = malloc(strlen(VALGRIND_LAUNCHER) + 1
                     + strlen(launcher_name) + 1);
   if (new_line == NULL) {
      barf("malloc of new_line failed.");
   }
   strcpy(new_line, VALGRIND_LAUNCHER);
   strcat(new_line, "=");
   strcat(new_line, launcher_name);

   for (j = 0; envp[j]; j++) {
      // do nothing
   }
   new_env = malloc((j+2) * sizeof(char*));
   if (new_env == NULL) {
      barf("malloc of new_env failed.");
   }
   for (i = 0; i < j; i++) {
      new_env[i] = envp[i];
   }
   new_env[i++] = new_line;
   new_env[i++] = NULL;
   assert(i == j+2);

   /* Establish the correct VALGRIND_LIB. */
   cp = getenv(VALGRIND_LIB);

   if (cp != NULL) {
      valgrind_lib = cp;
   }

   /* Build the stage2 invocation, and execve it.  Bye! */
   toolfile = malloc(strlen(valgrind_lib) + strlen(toolname) + strlen(platform) + 3);
   if (toolfile == NULL) {
      barf("malloc of toolfile failed.");
   }
   sprintf(toolfile, "%s/%s-%s", valgrind_lib, toolname, platform);

   VG_(debugLog)(1, "launcher", "launching %s\n", toolfile);

   execve(toolfile, argv, new_env);

   fprintf(stderr, "valgrind: failed to start tool '%s' for platform '%s': %s\n",
           toolname, platform, strerror(errno));

   exit(1);
}
