
/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2004 Julian Seward 
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

#define _FILE_OFFSET_BITS 64

#include "vg_include.h"
#include "ume.h"
#include "ume_arch.h"
#include "ume_archdefs.h"

#include <string.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <dlfcn.h>
#include <stdio.h>
#include <unistd.h>
#include <assert.h>
#include <fcntl.h>
#include <errno.h>
#include <dirent.h>

#ifndef AT_SYSINFO
#define AT_SYSINFO		32
#endif /* AT_SYSINFO */

#ifndef AT_SYSINFO_EHDR
#define AT_SYSINFO_EHDR		33
#endif /* AT_SYSINFO_EHDR */

#ifndef AT_SECURE
#define AT_SECURE 23   /* secure mode boolean */
#endif	/* AT_SECURE */

/* Amount to reserve for Valgrind's internal heap */
#define VALGRIND_HEAPSIZE	(128*1024*1024)

/* Amount to reserve for Valgrind's internal mappings */
#define VALGRIND_MAPSIZE	(128*1024*1024)

/* redzone gap between client address space and shadow */
#define REDZONE_SIZE		(1 * 1024*1024)

/* size multiple for client address space */
#define CLIENT_SIZE_MULTIPLE	(64 * 1024*1024)

extern char kickstart_base;	/* linker-defined base address */

/* Where we expect to find all our aux files */
static const char *valgrind_lib = VG_LIBDIR;

/* Parameters we're going to pass to Valgrind */
static KickstartParams kp;

/* Shape of the client address space */
static addr_t client_base, client_end, client_size;

/* Look for our AUXV table */
static int scan_auxv(void)
{
   const struct ume_auxv *auxv = find_auxv((int *)ume_exec_esp);
   int found = 0;

   for(; auxv->a_type != AT_NULL; auxv++)
      switch(auxv->a_type) {
      case AT_UME_PADFD:
	 as_setpadfd(auxv->a_val);
	 found |= 1;
	 break;

      case AT_UME_EXECFD:
	 kp.vgexecfd = auxv->a_val;
	 found |= 2;
	 break;
      }

   return found == (1|2);
}

/* Scan a colon-separated list, and call a function on each element.
   The string must be mutable, because we insert a temporary '\0', but
   the string will end up unmodified.  (*func) should return 1 if it
   doesn't need to see any more.
*/
static void scan_colsep(char *colsep, int (*func)(const char *))
{
   char *cp, *entry;
   int end;

   if (colsep == NULL ||
       *colsep == '\0')
      return;

   entry = cp = colsep;

   do {
      end = (*cp == '\0');

      if (*cp == ':' || *cp == '\0') {
	 char save = *cp;

	 *cp = '\0';
	 if ((*func)(entry))
	    end = 1;
	 *cp = save;
	 entry = cp+1;
      }
      cp++;
   } while(!end);
}

/* Add a string onto the string table, and return its address */
static char *copy_str(char **tab, const char *str)
{
   char *cp = *tab;
   char *orig = cp;

   while(*str)
      *cp++ = *str++;
   *cp++ = '\0';

   if (0)
      printf("copied %p \"%s\" len %d\n",
	     orig, orig, cp-orig);

   *tab = cp;

   return orig;
}

/* 
   This sets up the client's initial stack, containing the args,
   environment and aux vector.

   The format of the stack is:

   higher address +-----------------+
		  | Trampoline code |
		  +-----------------+
                  |                 |
		  : string table    :
		  |                 |
		  +-----------------+
		  | AT_NULL         |
		  -                 -
		  | auxv            |
		  +-----------------+
		  |  NULL           |
		  -                 -
		  | envp            |
		  +-----------------+
		  |  NULL           |
		  -                 -
		  | argv            |
		  +-----------------+
		  | argc            |
   lower address  +-----------------+ <- esp
                  | undefined       |
		  :                 :
 */
static Addr setup_client_stack(char **orig_argv, char **orig_envp, 
			       const struct ume_auxv *orig_auxv,
			       const struct exeinfo *info)
{
   char **cpp;
   char *strtab;		/* string table */
   char *stringbase;
   addr_t *ptr;
   struct ume_auxv *auxv;
   const struct ume_auxv *cauxv;
   unsigned stringsize;		/* total size of strings in bytes */
   unsigned auxsize;		/* total size of auxv in bytes */
   int argc;			/* total argc */
   int envc;			/* total number of env vars */
   unsigned stacksize;		/* total client stack size */
   addr_t cl_esp;		/* client stack base (initial esp) */
   addr_t cl_stacktop;		/* top of stack */

   /* ==================== compute sizes ==================== */

   /* first of all, work out how big the client stack will be */
   stringsize = 0;

   /* paste on the extra args if the loader needs them (ie, the #! 
      interpreter and its argument) */
   argc = 0;
   if (info->argv0 != NULL) {
      argc++;
      stringsize += strlen(info->argv0) + 1;
   }
   if (info->argv1 != NULL) {
      argc++;
      stringsize += strlen(info->argv1) + 1;
   }

   /* now scan the args we're given... */
   for(cpp = orig_argv; *cpp; cpp++) {
      argc++;
      stringsize += strlen(*cpp) + 1;
   }
   
   /* ...and the environment */
   envc = 0;
   for(cpp = orig_envp; cpp && *cpp; cpp++) {
      envc++;
      stringsize += strlen(*cpp) + 1;
   }

   /* now, how big is the auxv? */
   auxsize = sizeof(*auxv);	/* there's always at least one entry: AT_NULL */
   for(cauxv = orig_auxv; cauxv->a_type != AT_NULL; cauxv++) {
      if (cauxv->a_type == AT_PLATFORM)
	 stringsize += strlen(cauxv->a_ptr) + 1;
      auxsize += sizeof(*cauxv);
   }

   /* OK, now we know how big the client stack is */
   stacksize =
      sizeof(int) +			/* argc */
      sizeof(char **)*argc +		/* argv */
      sizeof(char **) +			/* terminal NULL */
      sizeof(char **)*envc +		/* envp */
      sizeof(char **) +			/* terminal NULL */
      auxsize +				/* auxv */
      ROUNDUP(stringsize, sizeof(int)) +/* strings (aligned) */
      VKI_BYTES_PER_PAGE;		/* page for trampoline code */

   /* cl_esp is the client's stack pointer */
   cl_esp = client_end - stacksize;
   cl_esp = ROUNDDN(cl_esp, 16); /* make stack 16 byte aligned */

   cl_stacktop = client_end;
   cl_stacktop -= VKI_BYTES_PER_PAGE;

   kp.cl_tramp_code = cl_stacktop;

   if (0)
      printf("stringsize=%d auxsize=%d stacksize=%d\n",
	     stringsize, auxsize, stacksize);


   /* base of the string table (aligned) */
   stringbase = strtab = (char *)(cl_stacktop - ROUNDUP(stringsize, sizeof(int)));

   kp.clstk_base = PGROUNDDN(cl_esp);
   kp.clstk_end = client_end;

   /* ==================== allocate space ==================== */

   /* allocate a stack - mmap enough space for the stack */
   mmap((void *)PGROUNDDN(cl_esp),
	client_end - PGROUNDDN(cl_esp),
	PROT_READ | PROT_WRITE | PROT_EXEC, 
	MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED, -1, 0);
   

   /* ==================== copy client stack ==================== */

   ptr = (addr_t *)cl_esp;

   /* --- argc --- */
   *ptr++ = argc;		/* client argc */

   /* --- argv --- */
   if (info->argv0) {
      *ptr++ = (addr_t)copy_str(&strtab, info->argv0);
      free(info->argv0);
   }
   if (info->argv1) {
      *ptr++ = (addr_t)copy_str(&strtab, info->argv1);
      free(info->argv1);
   }
   for(cpp = orig_argv; *cpp; ptr++, cpp++) {
      *ptr = (addr_t)copy_str(&strtab, *cpp);
   }
   *ptr++ = 0;

   /* --- envp --- */
   kp.client_envp = (Char **)ptr;
   for(cpp = orig_envp; cpp && *cpp; ptr++, cpp++)
      *ptr = (addr_t)copy_str(&strtab, *cpp);
   *ptr++ = 0;

   /* --- auxv --- */
   auxv = (struct ume_auxv *)ptr;
   kp.client_auxv = (UInt *)auxv;

   for(; orig_auxv->a_type != AT_NULL; auxv++, orig_auxv++) {
      /* copy the entry... */
      *auxv = *orig_auxv;

      /* ...and fix up the copy */
      switch(auxv->a_type) {
      case AT_PHDR:
	 if (info->phdr == 0)
	    auxv->a_type = AT_IGNORE;
	 else
	    auxv->a_val = info->phdr;
	 break;

      case AT_PHNUM:
	 if (info->phdr == 0)
	    auxv->a_type = AT_IGNORE;
	 else
	    auxv->a_val = info->phnum;
	 break;

      case AT_BASE:
	 if (info->interp_base == 0)
	    auxv->a_type = AT_IGNORE;
	 else
	    auxv->a_val = info->interp_base;
	 break;

      case AT_PLATFORM:		/* points to a platform description string */
	 auxv->a_ptr = copy_str(&strtab, orig_auxv->a_ptr);
	 break;

      case AT_ENTRY:
	 auxv->a_val = info->entry;
	 break;

      case AT_IGNORE:
      case AT_EXECFD:
      case AT_PHENT:
      case AT_PAGESZ:
      case AT_FLAGS:
      case AT_NOTELF:
      case AT_UID:
      case AT_EUID:
      case AT_GID:
      case AT_EGID:
      case AT_CLKTCK:
      case AT_HWCAP:
      case AT_FPUCW:
      case AT_DCACHEBSIZE:
      case AT_ICACHEBSIZE:
      case AT_UCACHEBSIZE:
	 /* All these are pointerless, so we don't need to do anything
	    about them. */
	 break;

      case AT_SECURE:
	 /* If this is 1, then it means that this program is running
	    suid, and therefore the dynamic linker should be careful
	    about LD_PRELOAD, etc.  However, since stage1 (the thing
	    the kernel actually execve's) should never be SUID, and we
	    need LD_PRELOAD/LD_LIBRARY_PATH to work for the client, we
	    set AT_SECURE to 0. */
	 auxv->a_val = 0;
	 break;

      case AT_SYSINFO:
	 /* Leave this unmolested for now, but we'll update it later
	    when we set up the client trapoline code page */
	 break;

      case AT_SYSINFO_EHDR:
	 /* Trash this, because we don't reproduce it */
	 auxv->a_type = AT_IGNORE;
	 break;

      default:
	 /* stomp out anything we don't know about */
	 if (0)
	    printf("stomping auxv entry %d\n", auxv->a_type);
	 auxv->a_type = AT_IGNORE;
	 break;
	 
      }
   }
   *auxv = *orig_auxv;
   assert(auxv->a_type == AT_NULL);

   assert((strtab-stringbase) == stringsize);

   return cl_esp;
}

/* Find and load a tool.  Also looks to see if there's a matching
   vgpreload_*.so file, and returns its name in *preloadpath. */
static void *load_tool(const char *name, char **preloadpath)
{
   int len = strlen(valgrind_lib) + strlen(name)*2 + 16;
   char buf[len];
   void *ret;

   *preloadpath = NULL;

   if (strchr(name, '/') != 0) {
      /* name contains '/', and so must be a pathname */
      ret = dlopen(name, RTLD_NOW);
   } else {
      /* just try in the libdir */
      snprintf(buf, len, "%s/vgskin_%s.so", valgrind_lib, name);
      ret = dlopen(buf, RTLD_NOW);

      if (ret != NULL) {
	 snprintf(buf, len, "%s/vgpreload_%s.so", valgrind_lib, name);
	 if (access(buf, R_OK) == 0)
	    *preloadpath = strdup(buf);
      }
   }

   return ret;
}

/* list the available tools */
static void list_tools(void)
{
   DIR *dir = opendir(valgrind_lib);
   struct dirent *de;
   int first = 1;

   if (dir == NULL) {
      fprintf(stderr, "Can't open %s: %s (installation problem?)\n",
	      valgrind_lib, strerror(errno));
      return;
   }

   while((de = readdir(dir)) != NULL) {
      int len = strlen(de->d_name);

      /* look for vgskin_TOOL.so names */
      if (len > (7+1+3) &&	/* "vgskin_" + at least on character tool name + ".so" */
	  strncmp(de->d_name, "vgskin_", 7) == 0 &&
	  strcmp(de->d_name + len - 3, ".so") == 0) {
	 if (first) {
	    printf("Available tools:\n");
	    first = 0;
	 }
	 de->d_name[len-3] = '\0';
	 printf("\t%s\n", de->d_name+7);
      }
   }

   closedir(dir);

   if (first)
      printf("No tools available in \"%s\" (installation problem?)\n",
	     valgrind_lib);
}

/* 
   Prepare the client's environment.  This is basically a copy of our
   environment, except:
   1. LD_LIBRARY_PATH=$VALGRINDLIB:$LD_LIBRARY_PATH
   2. LD_PRELOAD=$VALGRINDLIB/vg_inject.so:($VALGRINDLIB/vgpreload_TOOL.so:)?$LD_PRELOAD
   3. LD_ASSUME_KERNEL=2.4.1

   If any of these is missing, then it is added.

   Yummy.  String hacking in C.

   XXX If this needs to handle any more variables it should be hacked
   into something table driven.
 */
static char **fix_environment(char **origenv, const char *preload)
{
   static const char inject_so[] = "vg_inject.so";
   static const char ld_library_path[]   = "LD_LIBRARY_PATH=";
   static const int  ld_library_path_len = sizeof(ld_library_path)-1;
   static const char ld_preload[]   = "LD_PRELOAD=";
   static const int  ld_preload_len = sizeof(ld_preload)-1;
   static const char ld_assume_kernel[]   = "LD_ASSUME_KERNEL=";
   static const int  ld_assume_kernel_len = sizeof(ld_assume_kernel)-1;
   static const char valgrind_clo[]   = VALGRINDCLO "=";
   static const char valgrind_clo_len = sizeof(valgrind_clo)-1;
   int ld_preload_done       = 0;
   int ld_library_path_done  = 0;
   int ld_assume_kernel_done = 0;
   char *inject_path;
   int   inject_path_len;
   int vgliblen = strlen(valgrind_lib);
   char **cpp;
   char **ret;
   int envc;
   const int preloadlen = (preload == NULL) ? 0 : strlen(preload);

   /* Find the vg_inject.so; also make room for the tool preload
      library */
   inject_path_len = sizeof(inject_so) + vgliblen + preloadlen + 16;
   inject_path = malloc(inject_path_len);

   if (preload)
      snprintf(inject_path, inject_path_len, "%s/%s:%s", 
	       valgrind_lib, inject_so, preload);
   else
      snprintf(inject_path, inject_path_len, "%s/%s", 
	       valgrind_lib, inject_so);
   
   /* Count the original size of the env */
   envc = 0;			/* trailing NULL */
   for(cpp = origenv; cpp && *cpp; cpp++)
      envc++;

   /* Allocate a new space */
   ret = malloc(sizeof(char *) * (envc+3+1)); /* 3 new entries + NULL */

   /* copy it over */
   for(cpp = ret; *origenv; )
      *cpp++ = *origenv++;
   *cpp = NULL;
   
   assert(envc == (cpp - ret));

   /* Walk over the new environment, mashing as we go */
   for(cpp = ret; cpp && *cpp; cpp++) {
      if (memcmp(*cpp, ld_library_path, ld_library_path_len) == 0) {
	 int done = 0;
	 int contains(const char *p) {
	    if (strcmp(p, valgrind_lib) == 0) {
	       done = 1;
	       return 1;
	    }
	    return 0;
	 }

	 /* If the LD_LIBRARY_PATH already contains libdir, then don't
	    bother adding it again, even if it isn't the first (it
	    seems that the Java runtime will keep reexecing itself
	    unless its paths are at the front of LD_LIBRARY_PATH) */
	 scan_colsep(*cpp + ld_library_path_len, contains);

	 if (!done) {
	    int len = strlen(*cpp) + vgliblen*2 + 16;
	    char *cp = malloc(len);

	    snprintf(cp, len, "%s%s:%s",
		     ld_library_path, valgrind_lib,
		     (*cpp)+ld_library_path_len);

	    *cpp = cp;
	 }

	 ld_library_path_done = 1;
      } else if (memcmp(*cpp, ld_preload, ld_preload_len) == 0) {
	 int len = strlen(*cpp) + inject_path_len;
	 char *cp = malloc(len);

	 snprintf(cp, len, "%s%s:%s",
		  ld_preload, inject_path, (*cpp)+ld_preload_len);

	 *cpp = cp;
	 
	 ld_preload_done = 1;
      } else if (memcmp(*cpp, ld_assume_kernel, ld_assume_kernel_len) == 0) {
	 *cpp = "LD_ASSUME_KERNEL=2.4.1";
	 ld_assume_kernel_done = 1;
      } else if (memcmp(*cpp, valgrind_clo, valgrind_clo_len) == 0) {
	 *cpp = "";
      }
   }

   /* Add the missing bits */

   if (!ld_library_path_done) {
      int len = ld_library_path_len + vgliblen*2 + 16;
      char *cp = malloc(len);

      snprintf(cp, len, "%s%s",
	       ld_library_path, valgrind_lib);

      ret[envc++] = cp;
   }

   if (!ld_preload_done) {
      int len = ld_preload_len + inject_path_len;
      char *cp = malloc(len);
      
      snprintf(cp, len, "%s%s",
	       ld_preload, inject_path);
      
      ret[envc++] = cp;
   }

   if (!ld_assume_kernel_done)
      ret[envc++] = "LD_ASSUME_KERNEL=2.4.1";
      
   ret[envc] = NULL;

   return ret;
}

extern char **environ;		/* our environment */

int main(int argc, char **argv)
{
   int vg_argc;
   char **vg_argv;
   char **cl_argv;
   const char *tool = NULL;
   const char *exec = NULL;
   float ratio = 0.;	/* ratio of client bytes:shadow bytes */
   addr_t valgrind_base, valgrind_mmap_end;
   addr_t shadow_base, shadow_end, shadow_size;
   struct exeinfo info;
   ToolInfo *toolinfo = NULL;
   void *dl_handle;
   char *preload;		/* tool-specific LD_PRELOAD .so */
   int ok;
   char *env_clo;
   int i;

   if (!scan_auxv()) {
      fprintf(stderr, "stage2 must be launched by stage1\n");
      exit(127);
   }

   if (0) {
      int prmap(void *start, void *end, const char *perm, off_t off, int maj, int min, int ino) {
	 printf("mapping %10p-%10p %s %02x:%02x %d\n",
		start, end, perm, maj, min, ino);
	 return 1;
      }
      printf("========== stage2 ==========\n");
      foreach_map(prmap);
   }

   /* Pad out client's part of memory, so that we don't accidentally
      put things there - do this before anything else */
   valgrind_mmap_end = (addr_t)&kickstart_base; /* end of valgrind's mmaps */
   valgrind_base =  valgrind_mmap_end - VALGRIND_MAPSIZE;
   as_pad((void *)CLIENT_BASE, (void *)valgrind_base);

   /* Work out if we're getting our main set of arguments from the
      command line or from the environment (_VALGRIND_CLO). */
   env_clo = getenv(VALGRINDCLO);
   if (env_clo != NULL && *env_clo != '\0') {
      char *cp;
      char **cpp;

      /* OK, we're getting all our arguments from the environment -
	 the entire command line belongs to the client (including
	 argv[0]) */
      vg_argc = 1;		/* argv[0] */
      for(cp = env_clo; *cp; cp++)
	 if (*cp == '\01')
	    vg_argc++;

      vg_argv = malloc(sizeof(char **) * (vg_argc + 1));

      cpp = vg_argv;

      *cpp++ = "valgrind";	/* nominal argv[0] */
      *cpp++ = env_clo;

      for(cp = env_clo;
	  *cp; cp++) {
	 if (*cp == '\01') {
	    *cp++ = '\0';	/* chop it up in place */
	    *cpp++ = cp;
	 }
      }
      *cpp = NULL;
      cl_argv = argv;

      if (0)
	 for(i = 0; i < vg_argc; i++)
	    printf("vg_argv[%d]=\"%s\"\n", i, vg_argv[i]);
   } else {
      /* Count the arguments on the command line. */
      vg_argv = argv;

      for(vg_argc = 1; vg_argc < argc; vg_argc++) {
	 if (argv[vg_argc][0] != '-') /* exe name */
	    break;
	 if (strcmp(argv[vg_argc], "--") == 0) { /* dummy arg */
	    vg_argc++;
	    break;
	 }
      }
      
      cl_argv = &argv[vg_argc];

      if (cl_argv[0] == NULL) {
	 VG_(usage)();
	 exit(1);
      }
   }

   /* look for additional user-set options from the environment (VALGRIND_OPTS) */
   env_clo = getenv(VALGRINDOPTS);
   if (env_clo && *env_clo) {
      /* ' ' separated extra options */
      char *cp;
      char **from;
      char **to;
      char **new_argv;
      int count;
      
      count = 1;
      for(cp = env_clo; *cp; cp++)
	 if (*cp == ' ' || *cp == '\t') {
	    while(cp[1] == ' ' || cp[1] == '\t')
	       cp++;
	    count++;
	 }

      if (0)
	 printf("VALGRIND_OPTS=%s argc=%d\n",
		env_clo, count);

      new_argv = malloc((vg_argc + count + 3) * sizeof(char **));
      
      from = vg_argv;
      to = new_argv;

      *to++ = *from++;		/* copy argv[0] */

      /* add args out of environment, skipping multiple spaces and --
	 args */
      *to++ = env_clo;
      for(cp = env_clo; *cp; cp++) 
	 if (*cp == ' ' || *cp == '\t') {
	    *cp = '\0';
	    while(cp[1] == ' ' || cp[1] == '\t')
	       cp++;
	    if (strcmp(to[-1], "--") == 0) {
	       to--;
	    }

	    *to++ = cp+1;
	 }

      /* copy original arguments, stopping at command or -- */
      while(*from) {
	 if (**from != '-')
	    break;
	 if (strcmp(*from, "--") == 0) {
	    from++;		/* skip -- */
	    break;
	 }
	 *to++ = *from++;
      }

      /* add -- if not already present */
      if (strcmp(to[-1], "--") != 0)
	 *to++ = "--";

      vg_argc = to-new_argv;

      /* reset of original command line */
      while(*from)
	 *to++ = *from++;

      *to = NULL;
      
      vg_argv = new_argv;

      if (0)
	 for(i = 0; i < vg_argc; i++)
	    printf("vg_argv[%d]=%s\n",
		   i, vg_argv[i]);
   }

   /* parse the options we have */
   for(i = 1; i < vg_argc; i++) {
      if (0)
	 printf("vg_argv[%d]=%s\n",
		i, vg_argv[i]);
      if (strncmp(vg_argv[i], "--tool=", 7) == 0 ||
	  strncmp(vg_argv[i], "--skin=", 7) == 0)
	 tool = &vg_argv[i][7];
	 
      if (strncmp(vg_argv[i], "--exec=", 7) == 0)
	 exec = &vg_argv[i][7];
   }

   if (tool == NULL) {
      if (tool == NULL)
	 list_tools();

      if (0) {
	 for(i = 0; i < argc; i++)
	    fprintf(stderr, "argv[%d%s]=%s\n", i, i==vg_argc?"*":"", argv[i]);
	 fprintf(stderr, "tool=%s\n", tool);
      }

      VG_(usage)();
      exit(1);
   }

   /* If they didn't specify an executable, then use client argv[0] - 
      search $PATH if it isn't already specified */
   if (exec == NULL) {
      exec = cl_argv[0];

      if (strchr(exec, '/') == NULL) {
	 /* no '/' - we need to search the path */
	 char *path = getenv("PATH");
	 int pathlen = path ? strlen(path) : 0;

	 int match_exe(const char *entry) {
	    char buf[pathlen + strlen(entry) + 3];

	    /* empty PATH element means . */
	    if (*entry == '\0')
	       entry = ".";

	    snprintf(buf, sizeof(buf), "%s/%s", entry, exec);

	    if (access(buf, R_OK|X_OK) == 0) {
	       exec = strdup(buf);
	       return 1;
	    }
	    return 0;
	 }

	 scan_colsep(path, match_exe);
      }
   }

   /* Look to see if we should be finding our pieces elsewhere */
   {
      char *cp = getenv(VALGRINDLIB);
      if (cp != NULL)
	 valgrind_lib = cp;
   }

   /* While the client part of the address space is all padded
      out, map in valgrind+tool proper */
   ok = 1;

   dl_handle = load_tool(tool, &preload);

   if (dl_handle == NULL) {
      fprintf(stderr, "Can't open tool \"%s\": %s\n", tool, dlerror());
      ok = 0;
   } 
   if (ok) {
      Int *vg_malloc_redzonep = NULL;

      toolinfo = dlsym(dl_handle, "vgSkin_tool_info");

      if (toolinfo == NULL) {
	 fprintf(stderr, "Tool \"%s\" doesn't define SK_(tool_info) - add VG_DETERMINE_INTERFACE_VERSION?\n", tool);
	 ok = 0;
      }

      if (ok && 
	  (toolinfo->sizeof_ToolInfo != sizeof(*toolinfo) ||
	   toolinfo->interface_major_version != VG_CORE_INTERFACE_MAJOR_VERSION ||
	   toolinfo->sk_pre_clo_init == NULL)) {
	 fprintf(stderr, "Error:\n"
		 "  Tool and core interface versions do not match.\n"
		 "  Interface version used by core is: %d.%d (size %d)\n"
		 "  Interface version used by tool is: %d.%d (size %d)\n"
		 "  The major version numbers must match.\n",
		 VG_CORE_INTERFACE_MAJOR_VERSION, 
		 VG_CORE_INTERFACE_MINOR_VERSION,
		 sizeof(*toolinfo),
		 toolinfo->interface_major_version,
		 toolinfo->interface_minor_version, 
		 toolinfo->sizeof_ToolInfo);
	 fprintf(stderr, "  You need to at least recompile, and possibly update,\n");
	 if (VG_CORE_INTERFACE_MAJOR_VERSION > toolinfo->interface_major_version)
	    fprintf(stderr, "  your skin to work with this version of Valgrind.\n");
	 else
	    fprintf(stderr, "  your version of Valgrind to work with this skin.\n");
	 fprintf(stderr, "  Aborting, sorry.\n");

	 ok = 0;
      }

      ratio = toolinfo->shadow_ratio;

      /* If the tool wants a different malloc heap redzone size, then
	 set it up here, before we start using the core's
	 allocator. */
      if (ok && (vg_malloc_redzonep = dlsym(dl_handle, STR(VG_(vg_malloc_redzone_szB)))) != NULL)
	 VG_(vg_malloc_redzone_szB) = *vg_malloc_redzonep;
   }

   if (!ok) {
      if (dl_handle != NULL)
	 dlclose(dl_handle);

      fprintf(stderr, "Aborting: couldn't initialize valgrind\n");
      list_tools();
      exit(127);
   }
   
   /* Work out overall shape of the address space. This tries to give
      the client as large as possible address space while taking into
      account the tool's shadow needs.  */
   client_size = ROUNDDN((valgrind_base - REDZONE_SIZE) / (1. + ratio), CLIENT_SIZE_MULTIPLE);
   client_end = CLIENT_BASE + client_size;

   shadow_size = PGROUNDUP(client_size * ratio);
   shadow_base = client_end + REDZONE_SIZE;
   shadow_end = shadow_base + shadow_size;

   if (0)
      printf("valgrind_base=%x client_size=%x shadow=%x-%x (%x)\n", 
	     valgrind_base, client_size,
	     shadow_base, shadow_end, shadow_size);
   
   /* make the redzone inaccessible */
   mmap((void *)client_end, REDZONE_SIZE, PROT_NONE,
	MAP_FIXED|MAP_ANONYMOUS|MAP_PRIVATE, -1, 0);

   munmap(CLIENT_BASE, client_size);	/* make client hole */

   kp.client_mapbase = PGROUNDDN((client_size/4)*3); /* where !FIXED mmap goes */

   info.exe_base = client_base;
   info.exe_end  = client_end;
   info.map_base = kp.client_mapbase;

   info.setbrk = 0;
   info.argv = cl_argv;

   kp.clexecfd = open(exec, O_RDONLY);

   {
      int ret = do_exec(exec, &info);
      if (ret != 0) {
	 fprintf(stderr, "do_exec(%s) failed: %s\n", exec, strerror(ret));
	 exit(127);
      }
   }

   /* Shadow memory - initially all inaccessible, but then
      incrementally initialized as it is used */
   if (shadow_size != 0)
      mmap((char *)shadow_base, shadow_size, PROT_NONE,
	   MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED, -1, 0);

   /* unpad us */
   as_unpad((void *)shadow_end, (void *)~0);

   {
      char **env;
      const struct ume_auxv *auxv;

      /* use our own auxv as a prototype */
      auxv = find_auxv(ume_exec_esp);

      /* set up the client's environment */
      env = fix_environment(environ, preload);

      /* client's stack and eip */
      kp.client_esp = setup_client_stack(cl_argv, env, auxv, &info);
      kp.client_eip = info.init_eip;
   }

   /* Valgrind's argc/argv */
   kp.argc = vg_argc;
   kp.argv = (Char **)vg_argv;
   kp.libdir = valgrind_lib;

   /* client address space */
   kp.client_base = CLIENT_BASE;
   kp.client_end = client_end;
   kp.client_brkbase = info.brkbase;

   /* shadow segment */
   kp.shadow_base = shadow_base;
   kp.shadow_end = shadow_end;

   /* valgrind address space */
   kp.vg_base = valgrind_base;
   kp.vg_mmap_end = valgrind_mmap_end;
   kp.vg_end = ROUNDUP((Addr)&argc, 0x10000);		/* stack */

   as_closepadfile();		/* no more padding */

   if (0)
      printf("entry=%x client esp=%x vg_argc=%d brkbase=%x\n",
	     kp.client_eip, kp.client_esp, kp.argc, kp.client_brkbase);

#if 1
   VG_(main)(&kp, toolinfo->sk_pre_clo_init, dl_handle);
#else
   ume_go(kp.client_eip, kp.client_esp);
#endif

   abort();
}
