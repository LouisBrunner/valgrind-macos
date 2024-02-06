/* -*- mode: C; c-basic-offset: 3; -*- */

/*--------------------------------------------------------------------*/
/*--- Launching valgrind                              m_launcher.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2017 Julian Seward 
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
   along with this program; if not, see <http://www.gnu.org/licenses/>.

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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#ifndef EM_X86_64
#define EM_X86_64 62    // elf.h doesn't define this on some older systems
#endif

#ifndef EM_AARCH64
#define EM_AARCH64 183  // ditto
#endif

#ifndef EM_PPC64
#define EM_PPC64 21  // ditto
#endif

#ifndef EM_NANOMIPS
#define EM_NANOMIPS 249
#endif

#ifndef E_MIPS_ABI_O32
#define E_MIPS_ABI_O32 0x00001000
#endif

#ifndef E_MIPS_ABI2
#define E_MIPS_ABI2    0x00000020
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
static char *find_client(const char *clientname)
{
   char *fullname;
   const char *path = getenv("PATH");
   const char *colon;

   assert(clientname != NULL);

   if (path == NULL) return strdup(clientname);

   /* Make the size of the FULLNAME buffer large enough. */
   unsigned need = strlen(path) + strlen("/") + strlen(clientname) + 1;

   fullname = malloc(need);
   if (fullname == NULL)
      barf("malloc of fullname failed.");

   while (path)
   {
      if ((colon = strchr(path, ':')) == NULL)
      {
         strcpy(fullname, path);
         path = NULL;
      }
      else
      {
         strncpy(fullname, path, colon - path);
         fullname[colon - path] = '\0';
         path = colon + 1;
      }

      strcat(fullname, "/");
      strcat(fullname, clientname);

      if (access(fullname, R_OK|X_OK) == 0)
         return fullname;
      else if (access(fullname, X_OK) == 0)
	 barf("Need read permission on %s", fullname);
   }
   free(fullname);

   return strdup(clientname);
}

/* Examine the client and work out which platform it is for */
static const char *select_platform(const char *clientname)
{
   int fd;
   union {
      char c[4096];
      Elf32_Ehdr ehdr32;
      Elf64_Ehdr ehdr64;
   } header;
   ssize_t n_bytes;
   const char *platform = NULL;
   char *client;

   VG_(debugLog)(2, "launcher", "selecting platform for '%s'\n", clientname);

   if (strchr(clientname, '/') == NULL)
      client = find_client(clientname);
   else
      client = strdup(clientname);

   if (strcmp (client, clientname) != 0)
      VG_(debugLog)(2, "launcher", "selecting platform for '%s'\n", client);

   if ((fd = open(client, O_RDONLY)) < 0) {
     return_null:
      free (client);
      return NULL;
   }
   //   barf("open(%s): %s", clientname, strerror(errno));

   VG_(debugLog)(2, "launcher", "opened '%s'\n", client);

   n_bytes = read(fd, header.c, sizeof(header));
   close(fd);
   if (n_bytes < 2) {
      goto return_null;
   }

   VG_(debugLog)(2, "launcher", "read %ld bytes from '%s'\n",
                    (long int)n_bytes, client);

   if (header.c[0] == '#' && header.c[1] == '!') {
      int i = 2;

      STATIC_ASSERT(VKI_BINPRM_BUF_SIZE < sizeof header);
      if (n_bytes > VKI_BINPRM_BUF_SIZE)
         n_bytes = VKI_BINPRM_BUF_SIZE - 1;
      header.c[n_bytes] = '\0';
      char *eol = strchr(header.c, '\n');
      if (eol != NULL)
         *eol = '\0';
 
      // Skip whitespace.
      while (header.c[i] == ' '|| header.c[i] == '\t')
         i++;

      // Get the interpreter name.
      const char *interp = header.c + i;

      if (header.c[i] == '\0') {
         // No interpreter was found; fall back to default shell
#  if defined(VGPV_arm_linux_android) \
      || defined(VGPV_x86_linux_android) \
      || defined(VGPV_mips32_linux_android) \
      || defined(VGPV_arm64_linux_android)
         interp = "/system/bin/sh";
#  else
         interp = "/bin/sh";
#  endif
      } else {
         while (header.c[i]) {
            if (header.c[i] == ' ' || header.c[i] == '\t') break;
            i++;
         }
         header.c[i] = '\0';
      }

      platform = select_platform(interp);

   } else if (n_bytes >= SELFMAG && memcmp(header.c, ELFMAG, SELFMAG) == 0) {

      if (n_bytes >= sizeof(Elf32_Ehdr) && header.c[EI_CLASS] == ELFCLASS32) {

         if (header.c[EI_DATA] == ELFDATA2LSB) {
#           if defined(VGO_solaris)
            if (header.ehdr32.e_machine == EM_386 &&
                (header.ehdr32.e_ident[EI_OSABI] == ELFOSABI_SYSV ||
                 header.ehdr32.e_ident[EI_OSABI] == ELFOSABI_SOLARIS)) {
               platform = "x86-solaris";
            }
            else
#           endif
            if (header.ehdr32.e_machine == EM_386 &&
                (header.ehdr32.e_ident[EI_OSABI] == ELFOSABI_SYSV ||
                 header.ehdr32.e_ident[EI_OSABI] == ELFOSABI_LINUX)) {
               platform = "x86-linux";
            }
            else 
            if (header.ehdr32.e_machine == EM_ARM &&
                (header.ehdr32.e_ident[EI_OSABI] == ELFOSABI_SYSV ||
                 header.ehdr32.e_ident[EI_OSABI] == ELFOSABI_LINUX)) {
               platform = "arm-linux";
            }
            else
            if (header.ehdr32.e_machine == EM_MIPS &&
                (header.ehdr32.e_ident[EI_OSABI] == ELFOSABI_SYSV ||
                 header.ehdr32.e_ident[EI_OSABI] == ELFOSABI_LINUX) &&
                 (header.ehdr32.e_flags & E_MIPS_ABI_O32)) {
               platform = "mips32-linux";
            }
            else
            if (header.ehdr32.e_machine == EM_MIPS &&
                (header.ehdr32.e_ident[EI_OSABI] == ELFOSABI_SYSV ||
                 header.ehdr32.e_ident[EI_OSABI] == ELFOSABI_LINUX) &&
                 (header.ehdr32.e_flags & E_MIPS_ABI2)) {
               platform = "mips64-linux";
            }
            else
            if (header.ehdr32.e_machine == EM_NANOMIPS &&
                (header.ehdr32.e_ident[EI_OSABI] == ELFOSABI_SYSV ||
                 header.ehdr32.e_ident[EI_OSABI] == ELFOSABI_LINUX)) {
               platform = "nanomips-linux";
            }
         }
         else if (header.c[EI_DATA] == ELFDATA2MSB) {
            if (header.ehdr32.e_machine == EM_PPC &&
                (header.ehdr32.e_ident[EI_OSABI] == ELFOSABI_SYSV ||
                 header.ehdr32.e_ident[EI_OSABI] == ELFOSABI_LINUX)) {
               platform = "ppc32-linux";
            }
            else 
            if (header.ehdr32.e_machine == EM_MIPS &&
                (header.ehdr32.e_ident[EI_OSABI] == ELFOSABI_SYSV ||
                 header.ehdr32.e_ident[EI_OSABI] == ELFOSABI_LINUX) &&
                 (header.ehdr32.e_flags & E_MIPS_ABI_O32)) {
               platform = "mips32-linux";
            }
            else
            if (header.ehdr32.e_machine == EM_MIPS &&
                (header.ehdr32.e_ident[EI_OSABI] == ELFOSABI_SYSV ||
                 header.ehdr32.e_ident[EI_OSABI] == ELFOSABI_LINUX) &&
                 (header.ehdr32.e_flags & E_MIPS_ABI2)) {
               platform = "mips64-linux";
            }
            else
            if (header.ehdr32.e_machine == EM_NANOMIPS &&
                (header.ehdr32.e_ident[EI_OSABI] == ELFOSABI_SYSV ||
                 header.ehdr32.e_ident[EI_OSABI] == ELFOSABI_LINUX)) {
               platform = "nanomips-linux";
            }
         }

      } else if (n_bytes >= sizeof(Elf64_Ehdr) && header.c[EI_CLASS] == ELFCLASS64) {

         if (header.c[EI_DATA] == ELFDATA2LSB) {
#           if defined(VGO_solaris)
            if (header.ehdr64.e_machine == EM_X86_64 &&
                (header.ehdr64.e_ident[EI_OSABI] == ELFOSABI_SYSV ||
                 header.ehdr64.e_ident[EI_OSABI] == ELFOSABI_SOLARIS)) {
               platform = "amd64-solaris";
            }
            else
#           endif
            if (header.ehdr64.e_machine == EM_X86_64 &&
                (header.ehdr64.e_ident[EI_OSABI] == ELFOSABI_SYSV ||
                 header.ehdr64.e_ident[EI_OSABI] == ELFOSABI_LINUX)) {
               platform = "amd64-linux";
            } else if (header.ehdr64.e_machine == EM_MIPS &&
                (header.ehdr64.e_ident[EI_OSABI] == ELFOSABI_SYSV ||
                 header.ehdr64.e_ident[EI_OSABI] == ELFOSABI_LINUX)) {
               platform = "mips64-linux";
            } else if (header.ehdr64.e_machine == EM_AARCH64 &&
                (header.ehdr64.e_ident[EI_OSABI] == ELFOSABI_SYSV ||
                 header.ehdr64.e_ident[EI_OSABI] == ELFOSABI_LINUX)) {
               platform = "arm64-linux";
            } else if (header.ehdr64.e_machine == EM_PPC64 &&
                (header.ehdr64.e_ident[EI_OSABI] == ELFOSABI_SYSV ||
                 header.ehdr64.e_ident[EI_OSABI] == ELFOSABI_LINUX)) {
               platform = "ppc64le-linux";
            }
         } else if (header.c[EI_DATA] == ELFDATA2MSB) {
#           if !defined(VGPV_arm_linux_android) \
               && !defined(VGPV_x86_linux_android) \
               && !defined(VGPV_mips32_linux_android) \
               && !defined(VGPV_arm64_linux_android)
            if (header.ehdr64.e_machine == EM_PPC64 &&
                (header.ehdr64.e_ident[EI_OSABI] == ELFOSABI_SYSV ||
                 header.ehdr64.e_ident[EI_OSABI] == ELFOSABI_LINUX)) {
               platform = "ppc64be-linux";
            } 
            else 
            if (header.ehdr64.e_machine == EM_S390 &&
                (header.ehdr64.e_ident[EI_OSABI] == ELFOSABI_SYSV ||
                 header.ehdr64.e_ident[EI_OSABI] == ELFOSABI_LINUX)) {
               platform = "s390x-linux";
            } else if (header.ehdr64.e_machine == EM_MIPS &&
                (header.ehdr64.e_ident[EI_OSABI] == ELFOSABI_SYSV ||
                 header.ehdr64.e_ident[EI_OSABI] == ELFOSABI_LINUX)) {
               platform = "mips64-linux";
            }
#           endif
         }
      }
   }

   VG_(debugLog)(2, "launcher", "selected platform '%s'\n",
                 platform ? platform : "unknown");

   free (client);

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
   const char *linkname;
   char *toolfile;
   const char *launcher_name;
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
      the executable (eg because it's a shell script).  VG_PLATFORM is the
      default_platform. Its value is defined in coregrind/Makefile.am and
      typically it is the primary build target. Unless the primary build
      target is not built is not built in which case VG_PLATFORM is the
      secondary build target. */
#  if defined(VGO_linux)
   if ((0==strcmp(VG_PLATFORM,"x86-linux"))    ||
       (0==strcmp(VG_PLATFORM,"amd64-linux"))  ||
       (0==strcmp(VG_PLATFORM,"ppc32-linux"))  ||
       (0==strcmp(VG_PLATFORM,"ppc64be-linux"))  ||
       (0==strcmp(VG_PLATFORM,"ppc64le-linux"))  ||
       (0==strcmp(VG_PLATFORM,"arm-linux"))    ||
       (0==strcmp(VG_PLATFORM,"arm64-linux"))  ||
       (0==strcmp(VG_PLATFORM,"s390x-linux"))  ||
       (0==strcmp(VG_PLATFORM,"mips32-linux")) ||
       (0==strcmp(VG_PLATFORM,"mips64-linux")) ||
       (0==strcmp(VG_PLATFORM,"nanomips-linux")))
      default_platform = VG_PLATFORM;
#  elif defined(VGO_solaris)
   if ((0==strcmp(VG_PLATFORM,"x86-solaris")) ||
       (0==strcmp(VG_PLATFORM,"amd64-solaris")))
      default_platform = SOLARIS_LAUNCHER_DEFAULT_PLATFORM;
#  else
#    error Unknown OS
#  endif
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
#  if defined(VGO_linux)
   linkname = "/proc/self/exe";
#  elif defined(VGO_solaris)
   linkname = "/proc/self/path/a.out";
#  else
#    error Unknown OS
#  endif
   unsigned bufsiz = 0;
   char *buf = NULL;

   while (42) {
      bufsiz += 500;
      buf = realloc(buf, bufsiz);
      if (buf == NULL)
         barf("realloc of buf failed.");
      r = readlink(linkname, buf, bufsiz);
      if (r == -1) {
        /* If /proc/self/exe (/proc/self/path/a.out) can't be followed, don't
           give up. Instead continue with an empty string for VALGRIND_LAUNCHER.
           In the sys_execve wrapper, this is tested, and if found to be empty,
           fail the execve. */
        fprintf(stderr, "valgrind: warning (non-fatal): "
                "readlink(\"%s\") failed.\n", linkname);
        fprintf(stderr, "valgrind: continuing, however --trace-children=yes "
                "will not work.\n");
        launcher_name = "";
        break;
      }
      if (r == bufsiz) continue;   // buffer to small; retry

      assert(r < bufsiz);   // paranoia

      buf[r] = '\0';
      launcher_name = buf;
      break;
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
