
/*--------------------------------------------------------------------*/
/*--- Launching valgrind                              m_launcher.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2010 Julian Seward 
      jseward@acm.org

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

/* Note: this is a "normal" program and not part of Valgrind proper,
   and so it doesn't have to conform to Valgrind's arcane rules on
   no-glibc-usage etc. */

/* Include valgrind headers before system headers to avoid problems
   with the system headers #defining things which are used as names
   of structure members in vki headers. */

#include "pub_core_debuglog.h"
#include "pub_core_vki.h"       // Avoids warnings from
                                // pub_core_libcfile.h
#include "pub_core_libcproc.h"  // For VALGRIND_LIB, VALGRIND_LAUNCHER
#include "pub_core_ume.h"

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
#include <sys/user.h>
#include <unistd.h>



#define PATH_MAX 4096 /* POSIX refers to this a lot but I dunno
                         where it is defined */

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

   while (path)
   {
      if ((colon = strchr(path, ':')) == NULL)
      {
         strcpy(fullname, path);
         path = NULL;
      }
      else
      {
         memcpy(fullname, path, colon - path);
         fullname[colon - path] = '\0';
         path = colon + 1;
      }

      strcat(fullname, "/");
      strcat(fullname, clientname);

      if (access(fullname, R_OK|X_OK) == 0)
         return fullname;
   }

   return clientname;
}

/* Examine the client and work out which platform it is for */
static const char *select_platform(const char *clientname)
{
   int fd;
   uint8_t header[4096];
   ssize_t n_bytes;
   const char *platform = NULL;

   VG_(debugLog)(2, "launcher", "selecting platform for '%s'\n", clientname);

   if (strchr(clientname, '/') == NULL)
      clientname = find_client(clientname);

   VG_(debugLog)(2, "launcher", "selecting platform for '%s'\n", clientname);

   if ((fd = open(clientname, O_RDONLY)) < 0)
      return NULL;
   //   barf("open(%s): %s", clientname, strerror(errno));

   VG_(debugLog)(2, "launcher", "opened '%s'\n", clientname);

   n_bytes = read(fd, header, sizeof(header));
   close(fd);
   if (n_bytes < 2) {
      return NULL;
   }

   VG_(debugLog)(2, "launcher", "read %ld bytes from '%s'\n",
                    (long int)n_bytes, clientname);

   if (header[0] == '#' && header[1] == '!') {
      int i = 2;
      char *interp = (char *)header + 2;

      // Skip whitespace.
      while (1) {
         if (i == n_bytes) return NULL;
         if (' ' != header[i] && '\t' != header[i]) break;
         i++;
      }

      // Get the interpreter name.
      interp = &header[i];
      while (1) {
         if (i == n_bytes) break;
         if (isspace(header[i])) break;
         i++;
      }
      if (i == n_bytes) return NULL;
      header[i] = '\0';

      platform = select_platform(interp);

   } else if (n_bytes >= SELFMAG && memcmp(header, ELFMAG, SELFMAG) == 0) {

      if (n_bytes >= sizeof(Elf32_Ehdr) && header[EI_CLASS] == ELFCLASS32) {
         const Elf32_Ehdr *ehdr = (Elf32_Ehdr *)header;

         if (header[EI_DATA] == ELFDATA2LSB) {
            if (ehdr->e_machine == EM_386 &&
                (ehdr->e_ident[EI_OSABI] == ELFOSABI_SYSV ||
                 ehdr->e_ident[EI_OSABI] == ELFOSABI_LINUX)) {
               platform = "x86-linux";
            }
            else 
            if (ehdr->e_machine == EM_ARM &&
                (ehdr->e_ident[EI_OSABI] == ELFOSABI_SYSV ||
                 ehdr->e_ident[EI_OSABI] == ELFOSABI_LINUX)) {
               platform = "arm-linux";
            }
         }
         else if (header[EI_DATA] == ELFDATA2MSB) {
            if (ehdr->e_machine == EM_PPC &&
                (ehdr->e_ident[EI_OSABI] == ELFOSABI_SYSV ||
                 ehdr->e_ident[EI_OSABI] == ELFOSABI_LINUX)) {
               platform = "ppc32-linux";
            }
         }

      } else if (n_bytes >= sizeof(Elf64_Ehdr) && header[EI_CLASS] == ELFCLASS64) {
         const Elf64_Ehdr *ehdr = (Elf64_Ehdr *)header;

         if (header[EI_DATA] == ELFDATA2LSB) {
            if (ehdr->e_machine == EM_X86_64 &&
                (ehdr->e_ident[EI_OSABI] == ELFOSABI_SYSV ||
                 ehdr->e_ident[EI_OSABI] == ELFOSABI_LINUX)) {
               platform = "amd64-linux";
            }
         } else if (header[EI_DATA] == ELFDATA2MSB) {
            if (ehdr->e_machine == EM_PPC64 &&
                (ehdr->e_ident[EI_OSABI] == ELFOSABI_SYSV ||
                 ehdr->e_ident[EI_OSABI] == ELFOSABI_LINUX)) {
               platform = "ppc64-linux";
            } else if (ehdr->e_machine == EM_S390 &&
                       (ehdr->e_ident[EI_OSABI] == ELFOSABI_SYSV ||
                        ehdr->e_ident[EI_OSABI] == ELFOSABI_LINUX)) {
               platform = "s390x-linux";
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
   const char *default_platform;
   const char *cp;
   char *toolfile;
   char launcher_name[PATH_MAX+1];
   char* new_line;
   char** new_env;

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
         if (i+1 < argc)
            clientname = argv[i+1];
         break;
      }
      if (0 == strcmp(argv[i], "-d")) 
         loglevel++;
      if (0 == strncmp(argv[i], "--tool=", 7)) 
         toolname = argv[i] + 7;
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

   /* Select a platform to use if we can't decide that by looking at
      the executable (eg because it's a shell script).  Note that the
      default_platform is not necessarily either the primary or
      secondary build target.  Instead it's chosen to maximise the
      chances that /bin/sh will work on it.  Hence for a primary
      target of ppc64-linux we still choose ppc32-linux as the default
      target, because on most ppc64-linux setups, the basic /bin,
      /usr/bin, etc, stuff is built in 32-bit mode, not 64-bit
      mode. */
   if ((0==strcmp(VG_PLATFORM,"x86-linux"))   ||
       (0==strcmp(VG_PLATFORM,"amd64-linux")) ||
       (0==strcmp(VG_PLATFORM,"ppc32-linux")) ||
       (0==strcmp(VG_PLATFORM,"ppc64-linux")) ||
       (0==strcmp(VG_PLATFORM,"arm-linux"))   ||
       (0==strcmp(VG_PLATFORM,"s390x-linux")))
      default_platform = VG_PLATFORM;
   else
      barf("Unknown VG_PLATFORM '%s'", VG_PLATFORM);

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
   r = readlink("/proc/self/exe", launcher_name, PATH_MAX);
   if (r == -1) {
      /* If /proc/self/exe can't be followed, don't give up.  Instead
         continue with an empty string for VALGRIND_LAUNCHER.  In the
         sys_execve wrapper, this is tested, and if found to be empty,
         fail the execve. */
      fprintf(stderr, "valgrind: warning (non-fatal): "
                      "readlink(\"/proc/self/exe\") failed.\n");
      fprintf(stderr, "valgrind: continuing, however --trace-children=yes "
                      "will not work.\n");
   }

   /* tediously augment the env: VALGRIND_LAUNCHER=launcher_name */
   new_line = malloc(strlen(VALGRIND_LAUNCHER) + 1 
                     + strlen(launcher_name) + 1);
   if (new_line == NULL)
      barf("malloc of new_line failed.");
   strcpy(new_line, VALGRIND_LAUNCHER);
   strcat(new_line, "=");
   strcat(new_line, launcher_name);

   for (j = 0; envp[j]; j++)
      ;
   new_env = malloc((j+2) * sizeof(char*));
   if (new_env == NULL)
      barf("malloc of new_env failed.");
   for (i = 0; i < j; i++)
      new_env[i] = envp[i];
   new_env[i++] = new_line;
   new_env[i++] = NULL;
   assert(i == j+2);

   /* Establish the correct VALGRIND_LIB. */
   cp = getenv(VALGRIND_LIB);

   if (cp != NULL)
      valgrind_lib = cp;

   /* Build the stage2 invocation, and execve it.  Bye! */
   toolfile = malloc(strlen(valgrind_lib) + strlen(toolname) + strlen(platform) + 3);
   if (toolfile == NULL)
      barf("malloc of toolfile failed.");
   sprintf(toolfile, "%s/%s-%s", valgrind_lib, toolname, platform);

   VG_(debugLog)(1, "launcher", "launching %s\n", toolfile);

   execve(toolfile, argv, new_env);

   fprintf(stderr, "valgrind: failed to start tool '%s' for platform '%s': %s\n",
                   toolname, platform, strerror(errno));

   exit(1);
}
