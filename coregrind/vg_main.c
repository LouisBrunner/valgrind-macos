
/*--------------------------------------------------------------------*/
/*--- Startup: the real stuff                            vg_main.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Julian Seward 
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

#include "core.h"
#include "ume.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_debuglog.h"
#include "pub_core_errormgr.h"
#include "pub_core_execontext.h"
#include "pub_core_options.h"
#include "pub_core_syscalls.h"
#include "pub_core_tooliface.h"
#include "pub_core_transtab.h"

#include <dirent.h>
#include <dlfcn.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/ptrace.h>
#include <sys/wait.h>
#include <unistd.h>

#include "memcheck/memcheck.h"

#ifndef AT_DCACHEBSIZE
#define AT_DCACHEBSIZE		19
#endif /* AT_DCACHEBSIZE */

#ifndef AT_ICACHEBSIZE
#define AT_ICACHEBSIZE		20
#endif /* AT_ICACHEBSIZE */

#ifndef AT_UCACHEBSIZE
#define AT_UCACHEBSIZE		21
#endif /* AT_UCACHEBSIZE */

#ifndef AT_SYSINFO
#define AT_SYSINFO		32
#endif /* AT_SYSINFO */

#ifndef AT_SYSINFO_EHDR
#define AT_SYSINFO_EHDR		33
#endif /* AT_SYSINFO_EHDR */

#ifndef AT_SECURE
#define AT_SECURE 23   /* secure mode boolean */
#endif	/* AT_SECURE */

/* redzone gap between client address space and shadow */
#define REDZONE_SIZE		(1 * 1024*1024)

/* size multiple for client address space */
#define CLIENT_SIZE_MULTIPLE	(1 * 1024*1024)

/* Proportion of client space for its heap (rest is for mmaps + stack) */
#define CLIENT_HEAP_PROPORTION   0.333

/* Number of file descriptors that Valgrind tries to reserve for
   it's own use - just a small constant. */
#define N_RESERVED_FDS (10)

/*====================================================================*/
/*=== Global entities not referenced from generated code           ===*/
/*====================================================================*/

/* ---------------------------------------------------------------------
   Startup stuff                            
   ------------------------------------------------------------------ */

/* Client address space, lowest to highest (see top of ume.c) */
Addr VG_(client_base);           /* client address space limits */
Addr VG_(client_end);
Addr VG_(client_mapbase);
Addr VG_(client_trampoline_code);
Addr VG_(clstk_base);
Addr VG_(clstk_end);

Addr VG_(brk_base);	         /* start of brk */
Addr VG_(brk_limit);	         /* current brk */

Addr VG_(shadow_base);	         /* tool's shadow memory */
Addr VG_(shadow_end);

Addr VG_(valgrind_base);	 /* valgrind's address range */

// Note that VG_(valgrind_last) names the last byte of the section, whereas
// the VG_(*_end) vars name the byte one past the end of the section.
Addr VG_(valgrind_last);

struct vki_rlimit VG_(client_rlimit_data);
struct vki_rlimit VG_(client_rlimit_stack);

/* stage1 (main) executable */
static Int vgexecfd = -1;

/* client executable */
Int  VG_(clexecfd) = -1;

/* Path to library directory */
const Char *VG_(libdir) = VG_LIBDIR;

/* our argc/argv */
static Int  vg_argc;
static Char **vg_argv;

/* The master thread the one which will be responsible for mopping
   everything up at exit.  Normally it is tid 1, since that's the
   first thread created, but it may be something else after a
   fork(). */
ThreadId VG_(master_tid) = VG_INVALID_THREADID;

/* Application-visible file descriptor limits */
Int VG_(fd_soft_limit) = -1;
Int VG_(fd_hard_limit) = -1;

/* As deduced from sp_at_startup, the client's argc, argv[] and
   envp[] as extracted from the client's stack at startup-time. */
Int    VG_(client_argc);
Char** VG_(client_argv);
Char** VG_(client_envp);


/* ---------------------------------------------------------------------
   Running stuff                            
   ------------------------------------------------------------------ */

/* 64-bit counter for the number of basic blocks done. */
ULong VG_(bbs_done) = 0;

/* Tell the logging mechanism whether we are logging to a file
   descriptor or a socket descriptor. */
Bool VG_(logging_to_socket) = False;


/*====================================================================*/
/*=== Counters, for profiling purposes only                        ===*/
/*====================================================================*/

/* Counts pertaining to internal sanity checking. */
static UInt sanity_fast_count = 0;
static UInt sanity_slow_count = 0;

static void print_all_stats ( void )
{
   // Translation stats
   VG_(print_tt_tc_stats)();
   VG_(message)(Vg_DebugMsg,
      " dispatch: %llu jumps (bb entries).", VG_(bbs_done) );

   // Scheduler stats
   VG_(print_scheduler_stats)();

   VG_(message)(Vg_DebugMsg, 
                "   sanity: %d cheap, %d expensive checks.",
                sanity_fast_count, sanity_slow_count );

   VG_(print_ExeContext_stats)();

   // Memory stats
   if (VG_(clo_verbosity) > 2) {
      VG_(message)(Vg_DebugMsg, "");
      VG_(message)(Vg_DebugMsg, 
         "------ Valgrind's internal memory use stats follow ------" );
      VG_(sanity_check_malloc_all)();
      VG_(print_all_arena_stats)();
      VG_(message)(Vg_DebugMsg, "");
      //VG_(print_shadow_stats)();
   }
}


/*====================================================================*/
/*=== Miscellaneous global functions                               ===*/
/*====================================================================*/

static Int ptrace_setregs(Int pid, ThreadId tid)
{
   return VGA_(ptrace_setregs_from_tst)(pid, &VG_(threads)[tid].arch);
}

/* Start debugger and get it to attach to this process.  Called if the
   user requests this service after an error has been shown, so she can
   poke around and look at parameters, memory, etc.  You can't
   meaningfully get the debugger to continue the program, though; to
   continue, quit the debugger.  */
void VG_(start_debugger) ( ThreadId tid )
{
   Int pid;

   if ((pid = fork()) == 0) {
      ptrace(PTRACE_TRACEME, 0, NULL, NULL);
      VG_(kill)(VG_(getpid)(), VKI_SIGSTOP);

   } else if (pid > 0) {
      Int status;
      Int res;

      if ((res = VG_(waitpid)(pid, &status, 0)) == pid &&
          WIFSTOPPED(status) && WSTOPSIG(status) == SIGSTOP &&
          ptrace_setregs(pid, tid) == 0 &&
          kill(pid, SIGSTOP) == 0 &&
          ptrace(PTRACE_DETACH, pid, NULL, 0) == 0) {
         Char pidbuf[15];
         Char file[30];
         Char buf[100];
         Char *bufptr;
         Char *cmdptr;
         
         VG_(sprintf)(pidbuf, "%d", pid);
         VG_(sprintf)(file, "/proc/%d/fd/%d", pid, VG_(clexecfd));
 
         bufptr = buf;
         cmdptr = VG_(clo_db_command);
         
         while (*cmdptr) {
            switch (*cmdptr) {
               case '%':
                  switch (*++cmdptr) {
                     case 'f':
                        VG_(memcpy)(bufptr, file, VG_(strlen)(file));
                        bufptr += VG_(strlen)(file);
                        cmdptr++;
                        break;
                  case 'p':
                     VG_(memcpy)(bufptr, pidbuf, VG_(strlen)(pidbuf));
                     bufptr += VG_(strlen)(pidbuf);
                     cmdptr++;
                     break;
                  default:
                     *bufptr++ = *cmdptr++;
                     break;
                  }
                  break;
               default:
                  *bufptr++ = *cmdptr++;
                  break;
            }
         }
         
         *bufptr++ = '\0';
  
         VG_(message)(Vg_UserMsg, "starting debugger with cmd: %s", buf);
         res = VG_(system)(buf);
         if (res == 0) {      
            VG_(message)(Vg_UserMsg, "");
            VG_(message)(Vg_UserMsg, 
                         "Debugger has detached.  Valgrind regains control.  We continue.");
         } else {
            VG_(message)(Vg_UserMsg, "Apparently failed!");
            VG_(message)(Vg_UserMsg, "");
         }
      }

      VG_(kill)(pid, VKI_SIGKILL);
      VG_(waitpid)(pid, &status, 0);
   }
}


/* Print some helpful-ish text about unimplemented things, and give
   up. */
void VG_(unimplemented) ( Char* msg )
{
   VG_(message)(Vg_UserMsg, "");
   VG_(message)(Vg_UserMsg, 
      "Valgrind detected that your program requires");
   VG_(message)(Vg_UserMsg, 
      "the following unimplemented functionality:");
   VG_(message)(Vg_UserMsg, "   %s", msg);
   VG_(message)(Vg_UserMsg,
      "This may be because the functionality is hard to implement,");
   VG_(message)(Vg_UserMsg,
      "or because no reasonable program would behave this way,");
   VG_(message)(Vg_UserMsg,
      "or because nobody has yet needed it.  In any case, let us know at");
   VG_(message)(Vg_UserMsg,
      "%s and/or try to work around the problem, if you can.", VG_BUGS_TO);
   VG_(message)(Vg_UserMsg,
      "");
   VG_(message)(Vg_UserMsg,
      "Valgrind has to exit now.  Sorry.  Bye!");
   VG_(message)(Vg_UserMsg,
      "");
   VG_(pp_sched_status)();
   VG_(exit)(1);
}

/* Get the simulated stack pointer */
Addr VG_(get_SP) ( ThreadId tid )
{
   return STACK_PTR( VG_(threads)[tid].arch );
}

Addr VG_(get_IP) ( ThreadId tid )
{
   return INSTR_PTR( VG_(threads)[tid].arch );
}


/*====================================================================*/
/*=== Check we were launched by stage 1                            ===*/
/*====================================================================*/

/* Look for our AUXV table */
static int scan_auxv(void* init_sp)
{
   const struct ume_auxv *auxv = find_auxv((UWord*)init_sp);
   int padfile = -1, found = 0;

   for (; auxv->a_type != AT_NULL; auxv++)
      switch(auxv->a_type) {
      case AT_UME_PADFD:
	 padfile = auxv->u.a_val;
	 found |= 1;
	 break;

      case AT_UME_EXECFD:
	 vgexecfd = auxv->u.a_val;
	 found |= 2;
	 break;

      case AT_PHDR:
         VG_(valgrind_base) = PGROUNDDN(auxv->u.a_val);
         break;
      }

   if ( found != (1|2) ) {
      fprintf(stderr, "valgrind: stage2 must be launched by stage1\n");
      exit(127);
   }
   vg_assert(padfile >= 0);
   return padfile;
}


/*====================================================================*/
/*=== Address space determination                                  ===*/
/*====================================================================*/

extern char _start[];

static void layout_remaining_space(Addr argc_addr, float ratio)
{
   Int   ires;
   void* vres;
   Addr  client_size, shadow_size;

   // VG_(valgrind_base) should have been set by scan_auxv, but if not,
   // this is a workable approximation
   if (VG_(valgrind_base) == 0) {
      VG_(valgrind_base) = PGROUNDDN(&_start);
   }

   VG_(valgrind_last)  = ROUNDUP(argc_addr, 0x10000) - 1; // stack

   // This gives the client the largest possible address space while
   // taking into account the tool's shadow needs.
   client_size         = ROUNDDN((VG_(valgrind_base)-REDZONE_SIZE) / (1.+ratio),
                         CLIENT_SIZE_MULTIPLE);
   VG_(client_base)    = 0;
   VG_(client_end)     = VG_(client_base) + client_size;
   /* where !FIXED mmap goes */
   VG_(client_mapbase) = VG_(client_base) +
         PGROUNDDN((Addr)(client_size * CLIENT_HEAP_PROPORTION));

   VG_(shadow_base)    = VG_(client_end) + REDZONE_SIZE;
   VG_(shadow_end)     = VG_(valgrind_base);
   shadow_size         = VG_(shadow_end) - VG_(shadow_base);

#define SEGSIZE(a,b) ((VG_(b) - VG_(a))/(1024*1024))

   if (0)
      VG_(printf)(
         "client_base        %p (%dMB)\n"
         "client_mapbase     %p (%dMB)\n"
         "client_end         %p (%dMB)\n"
         "shadow_base        %p (%dMB)\n"
         "shadow_end         %p\n"
         "valgrind_base      %p (%dMB)\n"
         "valgrind_last      %p\n",
         VG_(client_base),       SEGSIZE(client_base,       client_mapbase),
         VG_(client_mapbase),    SEGSIZE(client_mapbase,    client_end),
         VG_(client_end),        SEGSIZE(client_end,        shadow_base),
         VG_(shadow_base),       SEGSIZE(shadow_base,       shadow_end),
         VG_(shadow_end),
         VG_(valgrind_base),     SEGSIZE(valgrind_base,     valgrind_last),
         VG_(valgrind_last)
      );

#undef SEGSIZE

   // Ban redzone
   vres = mmap((void *)VG_(client_end), REDZONE_SIZE, PROT_NONE,
               MAP_FIXED|MAP_ANON|MAP_PRIVATE|MAP_NORESERVE, -1, 0);
   vg_assert((void*)-1 != vres);

   // Make client hole
   ires = munmap((void*)VG_(client_base), client_size);
   vg_assert(0 == ires);

   // Map shadow memory.
   // Initially all inaccessible, incrementally initialized as it is used
   if (shadow_size != 0) {
      vres = mmap((char *)VG_(shadow_base), shadow_size, PROT_NONE,
                  MAP_PRIVATE|MAP_ANON|MAP_FIXED|MAP_NORESERVE, -1, 0);
      if ((void*)-1 == vres) {
         fprintf(stderr, 
          "valgrind: Could not allocate address space (%p bytes)\n"
          "valgrind:   for shadow memory\n"
          "valgrind: Possible causes:\n"
          "valgrind: - For some systems (especially under RedHat 8), Valgrind\n"
          "valgrind:   needs at least 1.5GB swap space.\n"
          "valgrind: - Or, your virtual memory size may be limited (check\n"
          "valgrind:   with 'ulimit -v').\n"
          "valgrind: - Or, your system may use a kernel that provides only a\n" 
          "valgrind:   too-small (eg. 2GB) user address space.\n"
          , (void*)shadow_size
         ); 
         exit(1);
      }
   }
}

/*====================================================================*/
/*=== Command line setup                                           ===*/
/*====================================================================*/

/* Nb: malloc'd memory never freed -- kept throughout like argv, envp */
static char* get_file_clo(char* dir)
{
#  define FLEN 512
   Int fd, n;
   struct stat s1;
   char* f_clo = NULL;
   char filename[FLEN];

   snprintf(filename, FLEN, "%s/.valgrindrc", ( NULL == dir ? "" : dir ) );
   fd = VG_(open)(filename, 0, VKI_S_IRUSR);
   if ( fd > 0 ) {
      if ( 0 == fstat(fd, &s1) ) {
         f_clo = malloc(s1.st_size+1);
         vg_assert(f_clo);
         n = read(fd, f_clo, s1.st_size);
         if (n == -1) n = 0;
         f_clo[n] = '\0';
      }
      close(fd);
   }
   return f_clo;
#  undef FLEN
}

static Int count_args(char* s)
{
   Int n = 0;
   if (s) {
      char* cp = s;
      while (True) {
         // We have alternating sequences: blanks, non-blanks, blanks...
         // count the non-blanks sequences.
         while ( VG_(isspace)(*cp) )    cp++;
         if    ( !*cp )                 break;
         n++;
         while ( !VG_(isspace)(*cp) && *cp ) cp++;
      }
   }
   return n;
}

/* add args out of environment, skipping multiple spaces and -- args */
static char** copy_args( char* s, char** to )
{
   if (s) {
      char* cp = s;
      while (True) {
         // We have alternating sequences: blanks, non-blanks, blanks...
         // copy the non-blanks sequences, and add terminating '\0'
         while ( VG_(isspace)(*cp) )    cp++;
         if    ( !*cp )                 break;
         *to++ = cp;
         while ( !VG_(isspace)(*cp) && *cp ) cp++;
         if ( *cp ) *cp++ = '\0';            // terminate if necessary
         if (VG_STREQ(to[-1], "--")) to--;   // undo any '--' arg
      }
   }
   return to;
}

// Augment command line with arguments from environment and .valgrindrc
// files.
static void augment_command_line(Int* vg_argc_inout, char*** vg_argv_inout)
{
   int    vg_argc0 = *vg_argc_inout;
   char** vg_argv0 = *vg_argv_inout;

   char*  env_clo = getenv(VALGRINDOPTS);
   char*  f1_clo  = get_file_clo( getenv("HOME") );
   char*  f2_clo  = get_file_clo(".");

   /* copy any extra args from file or environment, if present */
   if ( (env_clo && *env_clo) || (f1_clo && *f1_clo) || (f2_clo && *f2_clo) ) {
      /* ' ' separated extra options */
      char **from;
      char **to;
      int orig_arg_count, env_arg_count, f1_arg_count, f2_arg_count;

      for ( orig_arg_count = 0; vg_argv0[orig_arg_count]; orig_arg_count++ );

      env_arg_count = count_args(env_clo);
      f1_arg_count  = count_args(f1_clo);
      f2_arg_count  = count_args(f2_clo);

      if (0)
	 printf("extra-argc=%d %d %d\n",
		env_arg_count, f1_arg_count, f2_arg_count);

      /* +2: +1 for null-termination, +1 for added '--' */
      from     = vg_argv0;
      vg_argv0 = malloc( (orig_arg_count + env_arg_count + f1_arg_count 
                          + f2_arg_count + 2) * sizeof(char **));
      vg_assert(vg_argv0);
      to      = vg_argv0;

      /* copy argv[0] */
      *to++ = *from++;

      /* Copy extra args from env var and file, in the order: ~/.valgrindrc,
       * $VALGRIND_OPTS, ./.valgrindrc -- more local options are put later
       * to override less local ones. */
      to = copy_args(f1_clo,  to);
      to = copy_args(env_clo, to);
      to = copy_args(f2_clo,  to);

      /* copy original arguments, stopping at command or -- */
      while (*from) {
	 if (**from != '-')
	    break;
	 if (VG_STREQ(*from, "--")) {
	    from++;		/* skip -- */
	    break;
	 }
	 *to++ = *from++;
      }

      /* add -- */
      *to++ = "--";

      vg_argc0 = to - vg_argv0;

      /* copy rest of original command line, then NULL */
      while (*from) *to++ = *from++;
      *to = NULL;
   }

   *vg_argc_inout = vg_argc0;
   *vg_argv_inout = vg_argv0;
}

#define VG_CLO_SEP   '\01'

static void get_command_line( int argc, char** argv,
                              Int* vg_argc_out, Char*** vg_argv_out, 
                                                char*** cl_argv_out )
{
   int    vg_argc0;
   char** vg_argv0;
   char** cl_argv;
   char*  env_clo = getenv(VALGRINDCLO);

   if (env_clo != NULL && *env_clo != '\0') {
      char *cp;
      char **cpp;

      /* OK, VALGRINDCLO is set, which means we must be a child of another
         Valgrind process using --trace-children, so we're getting all our
         arguments from VALGRINDCLO, and the entire command line belongs to
         the client (including argv[0]) */
      vg_argc0 = 1;		/* argv[0] */
      for (cp = env_clo; *cp; cp++)
	 if (*cp == VG_CLO_SEP)
	    vg_argc0++;

      vg_argv0 = malloc(sizeof(char **) * (vg_argc0 + 1));
      vg_assert(vg_argv0);

      cpp = vg_argv0;

      *cpp++ = "valgrind";	/* nominal argv[0] */
      *cpp++ = env_clo;

      // Replace the VG_CLO_SEP args separator with '\0'
      for (cp = env_clo; *cp; cp++) {
	 if (*cp == VG_CLO_SEP) {
	    *cp++ = '\0';	/* chop it up in place */
	    *cpp++ = cp;
	 }
      }
      *cpp = NULL;
      cl_argv = argv;

   } else {
      Bool noaugment = False;

      /* Count the arguments on the command line. */
      vg_argv0 = argv;

      for (vg_argc0 = 1; vg_argc0 < argc; vg_argc0++) {
         Char* arg = argv[vg_argc0];
         if (arg[0] != '-') /* exe name */
	    break;
	 if (VG_STREQ(arg, "--")) { /* dummy arg */
	    vg_argc0++;
	    break;
	 }
         VG_BOOL_CLO(arg, "--command-line-only", noaugment)
      }
      cl_argv = &argv[vg_argc0];

      /* Get extra args from VALGRIND_OPTS and .valgrindrc files.
         Note we don't do this if getting args from VALGRINDCLO, as 
         those extra args will already be present in VALGRINDCLO.
         (We also don't do it when --command-line-only=yes.) */
      if (!noaugment)
	 augment_command_line(&vg_argc0, &vg_argv0);
   }

   if (0) {
      Int i;
      for (i = 0; i < vg_argc0; i++)
         printf("vg_argv0[%d]=\"%s\"\n", i, vg_argv0[i]);
   }

   *vg_argc_out =         vg_argc0;
   *vg_argv_out = (Char**)vg_argv0;
   *cl_argv_out =         cl_argv;
}


/*====================================================================*/
/*=== Environment and stack setup                                  ===*/
/*====================================================================*/

/* Scan a colon-separated list, and call a function on each element.
   The string must be mutable, because we insert a temporary '\0', but
   the string will end up unmodified.  (*func) should return True if it
   doesn't need to see any more.

   This routine will return True if (*func) returns True and False if
   it reaches the end of the list without that happening.
*/
static Bool scan_colsep(char *colsep, Bool (*func)(const char *))
{
   char *cp, *entry;
   int end;

   if (colsep == NULL ||
       *colsep == '\0')
      return False;

   entry = cp = colsep;

   do {
      end = (*cp == '\0');

      if (*cp == ':' || *cp == '\0') {
	 char save = *cp;

	 *cp = '\0';
	 if ((*func)(entry)) {
            *cp = save;
	    return True;
         }
	 *cp = save;
	 entry = cp+1;
      }
      cp++;
   } while(!end);

   return False;
}

/* Prepare the client's environment.  This is basically a copy of our
   environment, except:
     LD_PRELOAD=$VALGRINDLIB/vg_inject.so:($VALGRINDLIB/vgpreload_TOOL.so:)?$LD_PRELOAD

   If this is missing, then it is added.

   Yummy.  String hacking in C.

   If this needs to handle any more variables it should be hacked
   into something table driven.
 */
static char **fix_environment(char **origenv, const char *preload)
{
   static const char inject_so[]          = "vg_inject.so";
   static const char ld_preload[]         = "LD_PRELOAD=";
   static const char valgrind_clo[]       = VALGRINDCLO "=";
   static const int  ld_preload_len       = sizeof(ld_preload)-1;
   static const int  valgrind_clo_len     = sizeof(valgrind_clo)-1;
   int ld_preload_done       = 0;
   char *inject_path;
   int   inject_path_len;
   int vgliblen = strlen(VG_(libdir));
   char **cpp;
   char **ret;
   int envc;
   const int preloadlen = (preload == NULL) ? 0 : strlen(preload);

   /* Find the vg_inject.so; also make room for the tool preload
      library */
   inject_path_len = sizeof(inject_so) + vgliblen + preloadlen + 16;
   inject_path = malloc(inject_path_len);
   vg_assert(inject_path);

   if (preload)
      snprintf(inject_path, inject_path_len, "%s/%s:%s", 
	       VG_(libdir), inject_so, preload);
   else
      snprintf(inject_path, inject_path_len, "%s/%s", 
	       VG_(libdir), inject_so);
   
   /* Count the original size of the env */
   envc = 0;			/* trailing NULL */
   for (cpp = origenv; cpp && *cpp; cpp++)
      envc++;

   /* Allocate a new space */
   ret = malloc(sizeof(char *) * (envc+1+1)); /* 1 new entry + NULL */
   vg_assert(ret);

   /* copy it over */
   for (cpp = ret; *origenv; )
      *cpp++ = *origenv++;
   *cpp = NULL;
   
   vg_assert(envc == (cpp - ret));

   /* Walk over the new environment, mashing as we go */
   for (cpp = ret; cpp && *cpp; cpp++) {
      if (memcmp(*cpp, ld_preload, ld_preload_len) == 0) {
	 int len = strlen(*cpp) + inject_path_len;
	 char *cp = malloc(len);
         vg_assert(cp);

	 snprintf(cp, len, "%s%s:%s",
		  ld_preload, inject_path, (*cpp)+ld_preload_len);

	 *cpp = cp;
	 
	 ld_preload_done = 1;
      } else if (memcmp(*cpp, valgrind_clo, valgrind_clo_len) == 0) {
	 *cpp = "";
      }
   }

   /* Add the missing bits */
   if (!ld_preload_done) {
      int len = ld_preload_len + inject_path_len;
      char *cp = malloc(len);
      vg_assert(cp);
      
      snprintf(cp, len, "%s%s",
	       ld_preload, inject_path);
      
      ret[envc++] = cp;
   }

   free(inject_path);
   ret[envc] = NULL;

   return ret;
}

extern char **environ;		/* our environment */

/* Add a string onto the string table, and return its address */
static char *copy_str(char **tab, const char *str)
{
   char *cp = *tab;
   char *orig = cp;

   while(*str)
      *cp++ = *str++;
   *cp++ = '\0';

   if (0)
      printf("copied %p \"%s\" len %lld\n", orig, orig, (Long)(cp-orig));

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
   lower address  +-----------------+ <- sp
                  | undefined       |
		  :                 :
 */
static Addr setup_client_stack(void* init_sp,
                               char **orig_argv, char **orig_envp, 
			       const struct exeinfo *info,
                               UInt** client_auxv)
{
   void* res;
   char **cpp;
   char *strtab;		/* string table */
   char *stringbase;
   Addr *ptr;
   struct ume_auxv *auxv;
   const struct ume_auxv *orig_auxv;
   const struct ume_auxv *cauxv;
   unsigned stringsize;		/* total size of strings in bytes */
   unsigned auxsize;		/* total size of auxv in bytes */
   int argc;			/* total argc */
   int envc;			/* total number of env vars */
   unsigned stacksize;		/* total client stack size */
   Addr cl_esp;	                /* client stack base (initial esp) */

   /* use our own auxv as a prototype */
   orig_auxv = find_auxv(init_sp);

   /* ==================== compute sizes ==================== */

   /* first of all, work out how big the client stack will be */
   stringsize = 0;

   /* paste on the extra args if the loader needs them (ie, the #! 
      interpreter and its argument) */
   argc = 0;
   if (info->interp_name != NULL) {
      argc++;
      stringsize += strlen(info->interp_name) + 1;
   }
   if (info->interp_args != NULL) {
      argc++;
      stringsize += strlen(info->interp_args) + 1;
   }

   /* now scan the args we're given... */
   for (cpp = orig_argv; *cpp; cpp++) {
      argc++;
      stringsize += strlen(*cpp) + 1;
   }
   
   /* ...and the environment */
   envc = 0;
   for (cpp = orig_envp; cpp && *cpp; cpp++) {
      envc++;
      stringsize += strlen(*cpp) + 1;
   }

   /* now, how big is the auxv? */
   auxsize = sizeof(*auxv);	/* there's always at least one entry: AT_NULL */
   for (cauxv = orig_auxv; cauxv->a_type != AT_NULL; cauxv++) {
      if (cauxv->a_type == AT_PLATFORM)
	 stringsize += strlen(cauxv->u.a_ptr) + 1;
      auxsize += sizeof(*cauxv);
   }

   /* OK, now we know how big the client stack is */
   stacksize =
      sizeof(Word) +			/* argc */
      sizeof(char **)*argc +		/* argv */
      sizeof(char **) +			/* terminal NULL */
      sizeof(char **)*envc +		/* envp */
      sizeof(char **) +			/* terminal NULL */
      auxsize +				/* auxv */
      ROUNDUP(stringsize, sizeof(Word)) +/* strings (aligned) */
      VKI_PAGE_SIZE;			/* page for trampoline code */

   if (0) VG_(printf)("stacksize = %d\n", stacksize);

   // decide where stack goes!
   VG_(clstk_end) = VG_(client_end);

   VG_(client_trampoline_code) = VG_(clstk_end) - VKI_PAGE_SIZE;

   /* cl_esp is the client's stack pointer */
   cl_esp = VG_(clstk_end) - stacksize;
   cl_esp = ROUNDDN(cl_esp, 16); /* make stack 16 byte aligned */

   /* base of the string table (aligned) */
   stringbase = strtab = (char *)(VG_(client_trampoline_code) 
                         - ROUNDUP(stringsize, sizeof(int)));

   VG_(clstk_base) = PGROUNDDN(cl_esp);

   if (0)
      printf("stringsize=%d auxsize=%d stacksize=%d\n"
             "clstk_base %p\n"
             "clstk_end  %p\n",
	     stringsize, auxsize, stacksize,
             (void*)VG_(clstk_base), (void*)VG_(clstk_end));

   /* ==================== allocate space ==================== */

   /* allocate a stack - mmap enough space for the stack */
   res = mmap((void *)PGROUNDDN(cl_esp), VG_(clstk_end) - PGROUNDDN(cl_esp),
	      PROT_READ | PROT_WRITE | PROT_EXEC, 
	      MAP_PRIVATE | MAP_ANON | MAP_FIXED, -1, 0);
   vg_assert((void*)-1 != res); 

   /* ==================== copy client stack ==================== */

   ptr = (Addr*)cl_esp;

   /* --- argc --- */
   *ptr++ = argc;		/* client argc */

   /* --- argv --- */
   if (info->interp_name) {
      *ptr++ = (Addr)copy_str(&strtab, info->interp_name);
      free(info->interp_name);
   }
   if (info->interp_args) {
      *ptr++ = (Addr)copy_str(&strtab, info->interp_args);
      free(info->interp_args);
   }
   for (cpp = orig_argv; *cpp; ptr++, cpp++) {
      *ptr = (Addr)copy_str(&strtab, *cpp);
   }
   *ptr++ = 0;

   /* --- envp --- */
   VG_(client_envp) = (Char **)ptr;
   for (cpp = orig_envp; cpp && *cpp; ptr++, cpp++)
      *ptr = (Addr)copy_str(&strtab, *cpp);
   *ptr++ = 0;

   /* --- auxv --- */
   auxv = (struct ume_auxv *)ptr;
   *client_auxv = (UInt *)auxv;

   for (; orig_auxv->a_type != AT_NULL; auxv++, orig_auxv++) {
      /* copy the entry... */
      *auxv = *orig_auxv;

      /* ...and fix up the copy */
      switch(auxv->a_type) {
      case AT_PHDR:
	 if (info->phdr == 0)
	    auxv->a_type = AT_IGNORE;
	 else
	    auxv->u.a_val = info->phdr;
	 break;

      case AT_PHNUM:
	 if (info->phdr == 0)
	    auxv->a_type = AT_IGNORE;
	 else
	    auxv->u.a_val = info->phnum;
	 break;

      case AT_BASE:
	 auxv->u.a_val = info->interp_base;
	 break;

      case AT_PLATFORM:		/* points to a platform description string */
	 auxv->u.a_ptr = copy_str(&strtab, orig_auxv->u.a_ptr);
	 break;

      case AT_ENTRY:
	 auxv->u.a_val = info->entry;
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
	    need LD_PRELOAD to work for the client, we
	    set AT_SECURE to 0. */
	 auxv->u.a_val = 0;
	 break;

      case AT_SYSINFO:
	 /* Leave this unmolested for now, but we'll update it later
	    when we set up the client trampoline code page */
	 break;

      case AT_SYSINFO_EHDR:
	 /* Trash this, because we don't reproduce it */
	 auxv->a_type = AT_IGNORE;
	 break;

      default:
	 /* stomp out anything we don't know about */
	 if (0)
	    printf("stomping auxv entry %lld\n", (ULong)auxv->a_type);
	 auxv->a_type = AT_IGNORE;
	 break;
	 
      }
   }
   *auxv = *orig_auxv;
   vg_assert(auxv->a_type == AT_NULL);

// XXX: what architectures is this necessary for?  x86 yes, PPC no, others ?
// Perhaps a per-arch VGA_NEEDS_TRAMPOLINE constant is necessary?
#if defined(VGP_x86_linux) || defined(VGP_amd64_linux)
   /* --- trampoline page --- */
   VG_(memcpy)( (void *)VG_(client_trampoline_code),
                &VG_(trampoline_code_start), VG_(trampoline_code_length) );
#endif

   vg_assert((strtab-stringbase) == stringsize);

   /* We know the initial ESP is pointing at argc/argv */
   VG_(client_argc) = *(Int*)cl_esp;
   VG_(client_argv) = (Char**)(cl_esp + sizeof(HWord));

   if (0) VG_(printf)("startup SP = %p\n", cl_esp);
   return cl_esp;
}

/*====================================================================*/
/*=== Find executable                                              ===*/
/*====================================================================*/

static const char* executable_name;

static Bool match_executable(const char *entry) {
   char buf[strlen(entry) + strlen(executable_name) + 2];

   /* empty PATH element means . */
   if (*entry == '\0')
      entry = ".";

   snprintf(buf, sizeof(buf), "%s/%s", entry, executable_name);
   
   if (access(buf, R_OK|X_OK) == 0) {
      executable_name = strdup(buf);
      vg_assert(NULL != executable_name);
      return True;
   }
   return False;
}

static const char* find_executable(const char* exec)
{
   vg_assert(NULL != exec);
   executable_name = exec;
   if (strchr(executable_name, '/') == NULL) {
      /* no '/' - we need to search the path */
      char *path = getenv("PATH");
      scan_colsep(path, match_executable);
   }
   return executable_name;
}


/*====================================================================*/
/*=== Loading tools                                                ===*/
/*====================================================================*/

static void list_tools(void)
{
   DIR *dir = opendir(VG_(libdir));
   struct dirent *de;
   int first = 1;

   if (dir == NULL) {
      fprintf(stderr, "Can't open %s: %s (installation problem?)\n",
              VG_(libdir), strerror(errno));
      return;
   }

   while ((de = readdir(dir)) != NULL) {
      int len = strlen(de->d_name);

      /* look for vgtool_TOOL.so names */
      if (len > (7+1+3) &&   /* "vgtool_" + at least 1-char toolname + ".so" */
         strncmp(de->d_name, "vgtool_", 7) == 0 &&
         VG_STREQ(de->d_name + len - 3, ".so")) {
         if (first) {
            fprintf(stderr, "Available tools:\n");
            first = 0;
         }
         de->d_name[len-3] = '\0';
         fprintf(stderr, "\t%s\n", de->d_name+7);
      }
   }

   closedir(dir);

   if (first)
      fprintf(stderr, "No tools available in \"%s\" (installation problem?)\n",
             VG_(libdir));
}


/* Find and load a tool, and check it looks ok.  Also looks to see if there's 
 * a matching vgpreload_*.so file, and returns its name in *preloadpath. */
static void load_tool( const char *toolname,
                       ToolInfo** toolinfo_out, char **preloadpath_out )
{
   Bool      ok;
   int       len = strlen(VG_(libdir)) + strlen(toolname) + 16;
   char      buf[len];
   void*     handle;
   ToolInfo* toolinfo;
   char*     preloadpath = NULL;

   // XXX: allowing full paths for --tool option -- does it make sense?
   // Doesn't allow for vgpreload_<tool>.so.

   if (strchr(toolname, '/') != 0) {
      /* toolname contains '/', and so must be a pathname */
      handle = dlopen(toolname, RTLD_NOW);
   } else {
      /* just try in the libdir */
      snprintf(buf, len, "%s/vgtool_%s.so", VG_(libdir), toolname);
      handle = dlopen(buf, RTLD_NOW);

      if (handle != NULL) {
	 snprintf(buf, len, "%s/vgpreload_%s.so", VG_(libdir), toolname);
	 if (access(buf, R_OK) == 0) {
	    preloadpath = strdup(buf);
            vg_assert(NULL != preloadpath);
         }
      }
   }

   ok = (NULL != handle);
   if (!ok) {
      fprintf(stderr, "Can't open tool \"%s\": %s\n", toolname, dlerror());
      goto bad_load;
   }

   toolinfo = dlsym(handle, "vgPlain_tool_info");
   ok = (NULL != toolinfo);
   if (!ok) {
      fprintf(stderr, "Tool \"%s\" doesn't define its ToolInfo - "
                      "add VG_DETERMINE_INTERFACE_VERSION?\n", toolname);
      goto bad_load;
   }

   ok = (toolinfo->sizeof_ToolInfo == sizeof(*toolinfo) &&
         toolinfo->interface_version == VG_CORE_INTERFACE_VERSION &&
         toolinfo->tl_pre_clo_init != NULL);
   if (!ok) { 
      fprintf(stderr, "Error:\n"
              "  Tool and core interface versions do not match.\n"
              "  Interface version used by core is: %d (size %d)\n"
              "  Interface version used by tool is: %d (size %d)\n"
              "  The version numbers must match.\n",
              VG_CORE_INTERFACE_VERSION, 
              (Int)sizeof(*toolinfo),
              toolinfo->interface_version,
              toolinfo->sizeof_ToolInfo);
      fprintf(stderr, "  You need to at least recompile, and possibly update,\n");
      if (VG_CORE_INTERFACE_VERSION > toolinfo->interface_version)
         fprintf(stderr, "  your tool to work with this version of Valgrind.\n");
      else
         fprintf(stderr, "  your version of Valgrind to work with this tool.\n");
      goto bad_load;
   }

   vg_assert(NULL != toolinfo);
   *toolinfo_out    = toolinfo;
   *preloadpath_out = preloadpath;
   return;


 bad_load:
   if (handle != NULL)
      dlclose(handle);

   fprintf(stderr, "valgrind: couldn't load tool\n");
   list_tools();
   exit(127);
}


/*====================================================================*/
/*=== Command line errors                                          ===*/
/*====================================================================*/

static void revert_to_stderr ( void )
{
   vg_assert( !VG_(logging_to_socket) );
   VG_(clo_log_fd) = 2; /* stderr */
}

void VG_(bad_option) ( Char* opt )
{
   revert_to_stderr();
   VG_(printf)("valgrind: Bad option '%s'; aborting.\n", opt);
   VG_(printf)("valgrind: Use --help for more information.\n");
   VG_(exit)(1);
}

static void missing_prog ( void  )
{
   revert_to_stderr();
   VG_(printf)("valgrind: no program specified\n");
   VG_(printf)("valgrind: Use --help for more information.\n");
   VG_(exit)(1);
}

static void config_error ( Char* msg )
{
   revert_to_stderr();
   VG_(printf)("valgrind: Startup or configuration error:\n   %s\n", msg);
   VG_(printf)("valgrind: Unable to start up properly.  Giving up.\n");
   VG_(exit)(1);
}


/*====================================================================*/
/*=== Loading the client                                           ===*/
/*====================================================================*/

static void load_client(char* cl_argv[], const char* exec, Int need_help,
                 /*out*/struct exeinfo* info, /*out*/Addr* client_eip)
{
   // If they didn't specify an executable with --exec, and didn't specify 
   // --help, then use client argv[0] (searching $PATH if necessary).
   if (NULL == exec && !need_help) {
      if (cl_argv[0] == NULL || 
          ( NULL == (exec = find_executable(cl_argv[0])) ) )
      {
         missing_prog();
      }
   }

   info->map_base = VG_(client_mapbase);
   info->exe_base = VG_(client_base);
   info->exe_end  = VG_(client_end);
   info->argv     = cl_argv;

   if (need_help) {
      VG_(clexecfd) = -1;
      // Totally zero 'info' before continuing.
      VG_(memset)(info, 0, sizeof(*info));
   } else {
      Int ret;
      VG_(clexecfd) = VG_(open)(exec, VKI_O_RDONLY, VKI_S_IRUSR);
      ret = do_exec(exec, info);
      if (ret != 0) {
         fprintf(stderr, "valgrind: do_exec(%s) failed: %s\n",
                         exec, strerror(ret));
         exit(127);
      }
   }

   /* Copy necessary bits of 'info' that were filled in */
   *client_eip = info->init_eip;
   VG_(brk_base) = VG_(brk_limit) = info->brkbase;
}

/*====================================================================*/
/*=== Address space unpadding                                      ===*/
/*====================================================================*/

typedef struct {
   char*        killpad_start;
   char*        killpad_end;
   struct stat* killpad_padstat;
} killpad_extra;

static int killpad(char *segstart, char *segend, const char *perm, off_t off, 
                   int maj, int min, int ino, void* ex)
{
   killpad_extra* extra = ex;
   void *b, *e;
   int res;

   vg_assert(NULL != extra->killpad_padstat);

   if (extra->killpad_padstat->st_dev != makedev(maj, min) || 
       extra->killpad_padstat->st_ino != ino)
      return 1;
   
   if (segend <= extra->killpad_start || segstart >= extra->killpad_end)
      return 1;
   
   if (segstart <= extra->killpad_start)
      b = extra->killpad_start;
   else
      b = segstart;
   
   if (segend >= extra->killpad_end)
      e = extra->killpad_end;
   else
      e = segend;
   
   res = munmap(b, (char *)e-(char *)b);
   vg_assert(0 == res);
   
   return 1;
}

// Remove padding of 'padfile' from a range of address space.
static void as_unpad(void *start, void *end, int padfile)
{
   static struct stat padstat;
   killpad_extra extra;
   int res;

   vg_assert(padfile >= 0);
   
   res = fstat(padfile, &padstat);
   vg_assert(0 == res);
   extra.killpad_padstat = &padstat;
   extra.killpad_start   = start;
   extra.killpad_end     = end;
   foreach_map(killpad, &extra);
}

static void as_closepadfile(int padfile)
{
   int res = close(padfile);
   vg_assert(0 == res);
}

/*====================================================================*/
/*=== Command-line: variables, processing, etc                     ===*/
/*====================================================================*/

// See pub_{core,tool}_options.h for explanations of all these.

static void usage ( Bool debug_help )
{
   Char* usage1 = 
"usage: valgrind --tool=<toolname> [options] prog-and-args\n"
"\n"
"  common user options for all Valgrind tools, with defaults in [ ]:\n"
"    --tool=<name>             use the Valgrind tool named <name> [memcheck]\n"
"    -h --help                 show this message\n"
"    --help-debug              show this message, plus debugging options\n"
"    --version                 show version\n"
"    -q --quiet                run silently; only print error msgs\n"
"    -v --verbose              be more verbose, incl counts of errors\n"
"    --xml=yes                 show errors as XML\n"
"    --trace-children=no|yes   Valgrind-ise child processes? [no]\n"
"    --track-fds=no|yes        track open file descriptors? [no]\n"
"    --time-stamp=no|yes       add timestamps to log messages? [no]\n"
"\n"
"  uncommon user options for all Valgrind tools:\n"
"    --run-libc-freeres=no|yes free up glibc memory at exit? [yes]\n"
"    --weird-hacks=hack1,hack2,...  recognised hacks: lax-ioctls,ioctl-mmap [none]\n"
"    --pointercheck=no|yes     enforce client address space limits [yes]\n"
"    --support-elan3=no|yes    hacks for Quadrics Elan3 support [no]\n"
"    --show-emwarns=no|yes     show warnings about emulation limits? [no]\n"
"\n"
"  user options for Valgrind tools that report errors:\n"
"    --log-fd=<number>         log messages to file descriptor [2=stderr]\n"
"    --log-file=<file>         log messages to <file>.pid<pid>\n"
"    --log-file-exactly=<file> log messages to <file>\n"
"    --log-socket=ipaddr:port  log messages to socket ipaddr:port\n"
"    --demangle=no|yes         automatically demangle C++ names? [yes]\n"
"    --num-callers=<number>    show <num> callers in stack traces [12]\n"
"    --error-limit=no|yes      stop showing new errors if too many? [yes]\n"
"    --show-below-main=no|yes  continue stack traces below main() [no]\n"
"    --suppressions=<filename> suppress errors described in <filename>\n"
"    --gen-suppressions=no|yes|all    print suppressions for errors? [no]\n"
"    --db-attach=no|yes        start debugger when errors detected? [no]\n"
"    --db-command=<command>    command to start debugger [gdb -nw %%f %%p]\n"
"    --input-fd=<number>       file descriptor for input [0=stdin]\n"
"    --max-stackframe=<number> assume stack switch for SP changes larger\n"
"                              than <number> bytes [2000000]\n"
"\n";

   Char* usage2 = 
"\n"
"  debugging options for all Valgrind tools:\n"
"    --sanity-level=<number>   level of sanity checking to do [1]\n"
"    --single-step=no|yes      translate each instr separately? [no]\n"
"    --optimise=no|yes         improve intermediate code? [yes]\n"
"    --profile=no|yes          profile? (tool must be built for it) [no]\n"
"    --branchpred=yes|no       generate branch prediction hints [no]\n"
"    --trace-flags=<XXXXXXXX>   show generated code? (X = 0|1) [00000000]\n"
"    --profile-flags=<XXXXXXXX> ditto, but for profiling (X = 0|1) [00000000]\n"
"    --trace-notbelow=<number>    only show BBs above <number> [0]\n"
"    --trace-syscalls=no|yes   show all system calls? [no]\n"
"    --trace-signals=no|yes    show signal handling details? [no]\n"
"    --trace-symtab=no|yes     show symbol table details? [no]\n"
"    --trace-cfi=no|yes        show call-frame-info details? [no]\n"
"    --trace-sched=no|yes      show thread scheduler details? [no]\n"
"    --wait-for-gdb=yes|no     pause on startup to wait for gdb attach\n"
#if 0
"    --model-pthreads=yes|no   model the pthreads library [no]\n"
#endif
"    --command-line-only=no|yes  only use command line options [no]\n"
"\n"
"    --vex-iropt-verbosity             0 .. 9 [0]\n"
"    --vex-iropt-level                 0 .. 2 [2]\n"
"    --vex-iropt-precise-memory-exns   [no]\n"
"    --vex-iropt-unroll-thresh         0 .. 400 [120]\n"
"    --vex-guest-max-insns             1 .. 100 [50]\n"
"    --vex-guest-chase-thresh          0 .. 99  [10]\n"
"\n"
"    --trace-flags and --profile-flags values (omit the middle space):\n"
"       1000 0000   show conversion into IR\n"
"       0100 0000   show after initial opt\n"
"       0010 0000   show after instrumentation\n"
"       0001 0000   show after second opt\n"
"       0000 1000   show after tree building\n"
"       0000 0100   show selecting insns\n"
"       0000 0010   show after reg-alloc\n"
"       0000 0001   show final assembly\n"
"\n"
"  debugging options for Valgrind tools that report errors\n"
"    --dump-error=<number>     show translation for basic block associated\n"
"                              with <number>'th error context [0=show none]\n"
"\n";

   Char* usage3 =
"\n"
"  Extra options read from ~/.valgrindrc, $VALGRIND_OPTS, ./.valgrindrc\n"
"\n"
"  Valgrind is Copyright (C) 2000-2005 Julian Seward et al.\n"
"  and licensed under the GNU General Public License, version 2.\n"
"  Bug reports, feedback, admiration, abuse, etc, to: %s.\n"
"\n"
"  Tools are copyright and licensed by their authors.  See each\n"
"  tool's start-up message for more information.\n"
"\n";

   // Ensure the message goes to stdout
   VG_(clo_log_fd) = 1;
   vg_assert( !VG_(logging_to_socket) );

   VG_(printf)(usage1);
   if (VG_(details).name) {
      VG_(printf)("  user options for %s:\n", VG_(details).name);
      if (VG_(needs).command_line_options)
	 VG_TDICT_CALL(tool_print_usage);
      else
	 VG_(printf)("    (none)\n");
   }
   if (debug_help) {
      VG_(printf)(usage2);

      if (VG_(details).name) {
         VG_(printf)("  debugging options for %s:\n", VG_(details).name);
      
         if (VG_(needs).command_line_options)
            VG_TDICT_CALL(tool_print_debug_usage);
         else
            VG_(printf)("    (none)\n");
      }
   }
   VG_(printf)(usage3, VG_BUGS_TO);
   VG_(exit)(0);
}

static void pre_process_cmd_line_options
      ( Int* need_help, const char** tool, const char** exec )
{
   UInt i;

   LibVEX_default_VexControl(& VG_(clo_vex_control));

   /* parse the options we have (only the options we care about now) */
   for (i = 1; i < vg_argc; i++) {

      if (strcmp(vg_argv[i], "--version") == 0) {
         printf("valgrind-" VERSION "\n");
         exit(0);

      } else if (VG_CLO_STREQ(vg_argv[i], "--help") ||
                 VG_CLO_STREQ(vg_argv[i], "-h")) {
         *need_help = 1;

      } else if (VG_CLO_STREQ(vg_argv[i], "--help-debug")) {
         *need_help = 2;

      } else if (VG_CLO_STREQN(7, vg_argv[i], "--tool=")) {
	 *tool = &vg_argv[i][7];
	 
      } else if (VG_CLO_STREQN(7, vg_argv[i], "--exec=")) {
	 *exec = &vg_argv[i][7];
      }
   }
}

static void process_cmd_line_options( UInt* client_auxv, const char* toolname )
{
   Int  i, eventually_log_fd;
   Int  toolname_len = VG_(strlen)(toolname);
   enum {
      VgLogTo_Fd,
      VgLogTo_File,
      VgLogTo_FileExactly,
      VgLogTo_Socket
   } log_to = VgLogTo_Fd;   // Where is logging output to be sent?

   /* log to stderr by default, but usage message goes to stdout */
   eventually_log_fd = 2; 

   /* Check for sane path in ./configure --prefix=... */
   if (VG_LIBDIR[0] != '/') 
     config_error("Please use absolute paths in "
                  "./configure --prefix=... or --libdir=...");

// XXX: what architectures is this necessary for?  x86 yes, PPC no, others ?
#if defined(VGP_x86_linux)
   {
      UInt* auxp;
      for (auxp = client_auxv; auxp[0] != AT_NULL; auxp += 2) {
         switch(auxp[0]) {
         case AT_SYSINFO:
            auxp[1] = (Int)(VG_(client_trampoline_code) + VG_(tramp_syscall_offset));
            break;
         }
      } 
   }
#endif

   for (i = 1; i < vg_argc; i++) {

      Char* arg = vg_argv[i];
      Char* colon = arg;

      /* Look for a colon in the switch name */
      while (*colon && *colon != ':' && *colon != '=')
         colon++;

      /* Look for matching "--toolname:foo" */
      if (*colon == ':') {
         if (VG_CLO_STREQN(2,            arg,                "--") && 
             VG_CLO_STREQN(toolname_len, arg+2,              toolname) &&
             VG_CLO_STREQN(1,            arg+2+toolname_len, ":"))
         {
            // prefix matches, convert "--toolname:foo" to "--foo"
            if (0)
               VG_(printf)("tool-specific arg: %s\n", arg);
            arg = strdup(arg + toolname_len + 1);
            arg[0] = '-';
            arg[1] = '-';

         } else {
            // prefix doesn't match, skip to next arg
            continue;
         }
      }
      
      /* Ignore these options - they've already been handled */
      if (VG_CLO_STREQN( 7, arg, "--tool="))              goto skip_arg;
      if (VG_CLO_STREQN( 7, arg, "--exec="))              goto skip_arg;
      if (VG_CLO_STREQN(20, arg, "--command-line-only=")) goto skip_arg;

      if (     VG_CLO_STREQ(arg, "--"))                  goto skip_arg;

      else if (VG_CLO_STREQ(arg, "-v") ||
               VG_CLO_STREQ(arg, "--verbose"))
         VG_(clo_verbosity)++;

      else if (VG_CLO_STREQ(arg, "-q") ||
               VG_CLO_STREQ(arg, "--quiet"))
         VG_(clo_verbosity)--;

      else if (VG_CLO_STREQ(arg, "-d")) {
         /* do nothing */
      }

      else VG_BOOL_CLO(arg, "--xml",              VG_(clo_xml))
      else VG_BOOL_CLO(arg, "--branchpred",       VG_(clo_branchpred))
      else VG_BOOL_CLO(arg, "--db-attach",        VG_(clo_db_attach))
      else VG_BOOL_CLO(arg, "--demangle",         VG_(clo_demangle))
      else VG_BOOL_CLO(arg, "--error-limit",      VG_(clo_error_limit))
      else VG_BOOL_CLO(arg, "--pointercheck",     VG_(clo_pointercheck))
      else VG_BOOL_CLO(arg, "--support-elan3",    VG_(clo_support_elan3))
      else VG_BOOL_CLO(arg, "--show-emwarns",     VG_(clo_show_emwarns))
      else VG_NUM_CLO (arg, "--max-stackframe",   VG_(clo_max_stackframe))
      else VG_BOOL_CLO(arg, "--profile",          VG_(clo_profile))
      else VG_BOOL_CLO(arg, "--run-libc-freeres", VG_(clo_run_libc_freeres))
      else VG_BOOL_CLO(arg, "--show-below-main",  VG_(clo_show_below_main))
      else VG_BOOL_CLO(arg, "--time-stamp",       VG_(clo_time_stamp))
      else VG_BOOL_CLO(arg, "--track-fds",        VG_(clo_track_fds))
      else VG_BOOL_CLO(arg, "--trace-children",   VG_(clo_trace_children))
      else VG_BOOL_CLO(arg, "--trace-sched",      VG_(clo_trace_sched))
      else VG_BOOL_CLO(arg, "--trace-signals",    VG_(clo_trace_signals))
      else VG_BOOL_CLO(arg, "--trace-symtab",     VG_(clo_trace_symtab))
      else VG_BOOL_CLO(arg, "--trace-cfi",        VG_(clo_trace_cfi))
      else VG_BOOL_CLO(arg, "--trace-redir",      VG_(clo_trace_redir))
      else VG_BOOL_CLO(arg, "--trace-syscalls",   VG_(clo_trace_syscalls))
      else VG_BOOL_CLO(arg, "--trace-pthreads",   VG_(clo_trace_pthreads))
      else VG_BOOL_CLO(arg, "--wait-for-gdb",     VG_(clo_wait_for_gdb))
      else VG_BOOL_CLO(arg, "--model-pthreads",   VG_(clo_model_pthreads))

      else VG_STR_CLO (arg, "--db-command",       VG_(clo_db_command))
      else VG_STR_CLO (arg, "--weird-hacks",      VG_(clo_weird_hacks))

      else VG_NUM_CLO (arg, "--dump-error",       VG_(clo_dump_error))
      else VG_NUM_CLO (arg, "--input-fd",         VG_(clo_input_fd))
      else VG_NUM_CLO (arg, "--sanity-level",     VG_(clo_sanity_level))
      else VG_BNUM_CLO(arg, "--num-callers",      VG_(clo_backtrace_size), 1,
                                                  VG_DEEPEST_BACKTRACE)

      else VG_BNUM_CLO(arg, "--vex-iropt-verbosity",
                       VG_(clo_vex_control).iropt_verbosity, 0, 10)
      else VG_BNUM_CLO(arg, "--vex-iropt-level",
                       VG_(clo_vex_control).iropt_level, 0, 2)
      else VG_BOOL_CLO(arg, "--vex-iropt-precise-memory-exns",
                       VG_(clo_vex_control).iropt_precise_memory_exns)
      else VG_BNUM_CLO(arg, "--vex-iropt-unroll-thresh",
                       VG_(clo_vex_control).iropt_unroll_thresh, 0, 400)
      else VG_BNUM_CLO(arg, "--vex-guest-max-insns",
                       VG_(clo_vex_control).guest_max_insns, 1, 100)
      else VG_BNUM_CLO(arg, "--vex-guest-chase-thresh",
                       VG_(clo_vex_control).guest_chase_thresh, 0, 99)

      else if (VG_CLO_STREQN(9,  arg, "--log-fd=")) {
         log_to            = VgLogTo_Fd;
         VG_(clo_log_name) = NULL;
         eventually_log_fd = (Int)VG_(atoll)(&arg[9]);
      }

      else if (VG_CLO_STREQN(11, arg, "--log-file=")) {
         log_to            = VgLogTo_File;
         VG_(clo_log_name) = &arg[11];
      }

      else if (VG_CLO_STREQN(19, arg, "--log-file-exactly=")) {
         log_to            = VgLogTo_FileExactly;
         VG_(clo_log_name) = &arg[19];
      }

      else if (VG_CLO_STREQN(13, arg, "--log-socket=")) {
         log_to            = VgLogTo_Socket;
         VG_(clo_log_name) = &arg[13];
      }

      else if (VG_CLO_STREQN(15, arg, "--suppressions=")) {
         if (VG_(clo_n_suppressions) >= VG_CLO_MAX_SFILES) {
            VG_(message)(Vg_UserMsg, "Too many suppression files specified.");
            VG_(message)(Vg_UserMsg, 
                         "Increase VG_CLO_MAX_SFILES and recompile.");
            VG_(bad_option)(arg);
         }
         VG_(clo_suppressions)[VG_(clo_n_suppressions)] = &arg[15];
         VG_(clo_n_suppressions)++;
      }

      /* "stuvwxyz" --> stuvwxyz (binary) */
      else if (VG_CLO_STREQN(14, arg, "--trace-flags=")) {
         Int j;
         char* opt = & arg[14];
   
         if (8 != VG_(strlen)(opt)) {
            VG_(message)(Vg_UserMsg, 
                         "--trace-flags argument must have 8 digits");
            VG_(bad_option)(arg);
         }
         for (j = 0; j < 8; j++) {
            if      ('0' == opt[j]) { /* do nothing */ }
            else if ('1' == opt[j]) VG_(clo_trace_flags) |= (1 << (7-j));
            else {
               VG_(message)(Vg_UserMsg, "--trace-flags argument can only "
                                        "contain 0s and 1s");
               VG_(bad_option)(arg);
            }
         }
      }

      /* "stuvwxyz" --> stuvwxyz (binary) */
      else if (VG_CLO_STREQN(16, arg, "--profile-flags=")) {
         Int j;
         char* opt = & arg[16];
   
         if (8 != VG_(strlen)(opt)) {
            VG_(message)(Vg_UserMsg, 
                         "--profile-flags argument must have 8 digits");
            VG_(bad_option)(arg);
         }
         for (j = 0; j < 8; j++) {
            if      ('0' == opt[j]) { /* do nothing */ }
            else if ('1' == opt[j]) VG_(clo_profile_flags) |= (1 << (7-j));
            else {
               VG_(message)(Vg_UserMsg, "--profile-flags argument can only "
                                        "contain 0s and 1s");
               VG_(bad_option)(arg);
            }
         }
      }

      else VG_NUM_CLO (arg, "--trace-notbelow",   VG_(clo_trace_notbelow))

      else if (VG_CLO_STREQ(arg, "--gen-suppressions=no"))
         VG_(clo_gen_suppressions) = 0;
      else if (VG_CLO_STREQ(arg, "--gen-suppressions=yes"))
         VG_(clo_gen_suppressions) = 1;
      else if (VG_CLO_STREQ(arg, "--gen-suppressions=all"))
         VG_(clo_gen_suppressions) = 2;

      else if ( ! VG_(needs).command_line_options
             || ! VG_TDICT_CALL(tool_process_cmd_line_option, arg) ) {
         VG_(bad_option)(arg);
      }
    skip_arg:
      if (arg != vg_argv[i])
         free(arg);
   }

   /* Make VEX control parameters sane */

   if (VG_(clo_vex_control).guest_chase_thresh
       >= VG_(clo_vex_control).guest_max_insns)
      VG_(clo_vex_control).guest_chase_thresh
         = VG_(clo_vex_control).guest_max_insns - 1;

   if (VG_(clo_vex_control).guest_chase_thresh < 0)
      VG_(clo_vex_control).guest_chase_thresh = 0;

   /* Check various option values */

   if (VG_(clo_verbosity) < 0)
      VG_(clo_verbosity) = 0;

   if (VG_(clo_db_attach) && VG_(clo_trace_children)) {
      VG_(message)(Vg_UserMsg, "");
      VG_(message)(Vg_UserMsg, 
         "--db-attach=yes conflicts with --trace-children=yes");
      VG_(message)(Vg_UserMsg, 
         "Please choose one or the other, but not both.");
      VG_(bad_option)("--db-attach=yes and --trace-children=yes");
   }

   if (VG_(clo_gen_suppressions) > 0 && 
       !VG_(needs).core_errors && !VG_(needs).tool_errors) {
      VG_(message)(Vg_UserMsg, 
                   "Can't use --gen-suppressions= with this tool,");
      VG_(message)(Vg_UserMsg, 
                   "as it doesn't generate errors.");
      VG_(bad_option)("--gen-suppressions=");
   }

   /* If we've been asked to emit XML, mash around various other
      options so as to constrain the output somewhat, and to remove
      any need for user input during the run. */
   if (VG_(clo_xml)) {
      /* Disable suppression generation (requires user input) */
      VG_(clo_gen_suppressions) = 0;
      /* Disable attaching to GDB (requires user input) */
      VG_(clo_db_attach) = False;
      /* Set a known verbosity level */
      VG_(clo_verbosity) = 1;
      /* Disable error limits (this might be a bad idea!) */
      VG_(clo_error_limit) = False;
      /* Disable emulation warnings */
      VG_(clo_show_emwarns) = False;
      /* Disable waiting for GDB to debug Valgrind */
      VG_(clo_wait_for_gdb) = False;
      /* No file-descriptor leak checking yet */
      VG_(clo_track_fds) = False;
      /* Also, we want to set options for the leak checker, but that
         will have to be done in Memcheck's flag-handling code, not
         here. */
   }

   /* All non-logging-related options have been checked.  If the logging
      option specified is ok, we can switch to it, as we know we won't
      have to generate any other command-line-related error messages.
      (So far we should be still attached to stderr, so we can show on
      the terminal any problems to do with processing command line
      opts.)
   
      So set up logging now.  After this is done, VG_(clo_log_fd)
      should be connected to whatever sink has been selected, and we
      indiscriminately chuck stuff into it without worrying what the
      nature of it is.  Oh the wonder of Unix streams. */

   vg_assert(VG_(clo_log_fd) == 2 /* stderr */);
   vg_assert(VG_(logging_to_socket) == False);

   switch (log_to) {

      case VgLogTo_Fd: 
         vg_assert(VG_(clo_log_name) == NULL);
         VG_(clo_log_fd) = eventually_log_fd;
         break;

      case VgLogTo_File: {
         Char logfilename[1000];
	 Int seq = 0;
	 Int pid = VG_(getpid)();

         vg_assert(VG_(clo_log_name) != NULL);
         vg_assert(VG_(strlen)(VG_(clo_log_name)) <= 900); /* paranoia */

	 for (;;) {
	    if (seq == 0)
	       VG_(sprintf)(logfilename, "%s.pid%d",
			    VG_(clo_log_name), pid );
	    else
	       VG_(sprintf)(logfilename, "%s.pid%d.%d",
			    VG_(clo_log_name), pid, seq );
	    seq++;

            // EXCL: it will fail with EEXIST if the file already exists.
	    eventually_log_fd 
	       = VG_(open)(logfilename, 
			   VKI_O_CREAT|VKI_O_WRONLY|VKI_O_EXCL|VKI_O_TRUNC, 
			   VKI_S_IRUSR|VKI_S_IWUSR);
	    if (eventually_log_fd >= 0) {
	       VG_(clo_log_fd) = VG_(safe_fd)(eventually_log_fd);
	       break; /* for (;;) */
	    } else {
               // If the file already existed, we try the next name.  If it
               // was some other file error, we give up.
	       if (eventually_log_fd != -VKI_EEXIST) {
		  VG_(message)(Vg_UserMsg, 
			       "Can't create/open log file '%s.pid%d'; giving up!", 
			       VG_(clo_log_name), pid);
		  VG_(bad_option)(
		     "--log-file=<file> (didn't work out for some reason.)");
                  /*NOTREACHED*/
	       }
	    }
	 }
         break; /* switch (VG_(clo_log_to)) */
      }

      case VgLogTo_FileExactly: {
         vg_assert(VG_(clo_log_name) != NULL);
         vg_assert(VG_(strlen)(VG_(clo_log_name)) <= 900); /* paranoia */

         eventually_log_fd 
            = VG_(open)(VG_(clo_log_name),
                        VKI_O_CREAT|VKI_O_WRONLY|VKI_O_TRUNC, 
                        VKI_S_IRUSR|VKI_S_IWUSR);
         if (eventually_log_fd >= 0) {
            VG_(clo_log_fd) = VG_(safe_fd)(eventually_log_fd);
         } else {
            VG_(message)(Vg_UserMsg, 
                         "Can't create/open log file '%s'; giving up!", 
                         VG_(clo_log_name));
            VG_(bad_option)(
               "--log-file-exactly=<file> (didn't work out for some reason.)");
            /*NOTREACHED*/
	 }
         break; /* switch (VG_(clo_log_to)) */
      }

      case VgLogTo_Socket: {
         vg_assert(VG_(clo_log_name) != NULL);
         vg_assert(VG_(strlen)(VG_(clo_log_name)) <= 900); /* paranoia */
         eventually_log_fd = VG_(connect_via_socket)( VG_(clo_log_name) );
         if (eventually_log_fd == -1) {
            VG_(message)(Vg_UserMsg, 
               "Invalid --log-socket=ipaddr or --log-socket=ipaddr:port spec"); 
            VG_(message)(Vg_UserMsg, 
               "of '%s'; giving up!", VG_(clo_log_name) );
            VG_(bad_option)(
               "--log-socket=");
            /*NOTREACHED*/
	 }
         if (eventually_log_fd == -2) {
            VG_(message)(Vg_UserMsg, 
               "valgrind: failed to connect to logging server '%s'.",
               VG_(clo_log_name) ); 
            VG_(message)(Vg_UserMsg, 
                "Log messages will sent to stderr instead." );
            VG_(message)(Vg_UserMsg, 
                "" );
            /* We don't change anything here. */
            vg_assert(VG_(clo_log_fd) == 2);
	 } else {
            vg_assert(eventually_log_fd > 0);
            VG_(clo_log_fd) = eventually_log_fd;
            VG_(logging_to_socket) = True;
         }
         break;
      }
   }


   /* Check that the requested tool actually supports XML output. */
   if (VG_(clo_xml) && 0 != VG_(strcmp)(toolname, "memcheck")) {
      VG_(clo_xml) = False;
      VG_(message)(Vg_UserMsg, 
         "Currently only Memcheck supports XML output."); 
      VG_(bad_option)("--xml=yes");
      /*NOTREACHED*/
   }

   // Move log_fd into the safe range, so it doesn't conflict with any app fds.
   // XXX: this is more or less duplicating the behaviour of the calls to
   // VG_(safe_fd)() above, although this does not close the original fd.
   // Perhaps the VG_(safe_fd)() calls above should be removed, and this
   // code should be replaced with a call to VG_(safe_fd)().   --njn
   eventually_log_fd = VG_(fcntl)(VG_(clo_log_fd), VKI_F_DUPFD, VG_(fd_hard_limit));
   if (eventually_log_fd < 0)
      VG_(message)(Vg_UserMsg, "valgrind: failed to move logfile fd into safe range");
   else {
      VG_(clo_log_fd) = eventually_log_fd;
      VG_(fcntl)(VG_(clo_log_fd), VKI_F_SETFD, VKI_FD_CLOEXEC);
   }

   /* Ok, the logging sink is running now.  Print a suitable preamble.
      If logging to file or a socket, write details of parent PID and
      command line args, to help people trying to interpret the
      results of a run which encompasses multiple processes. */

   if (VG_(clo_xml)) {
      VG_(message)(Vg_UserMsg, "");
      VG_(message)(Vg_UserMsg, "<valgrindoutput>");
      VG_(message)(Vg_UserMsg, "");
      VG_(message)(Vg_UserMsg, "<protocolversion>1</protocolversion>");
      VG_(message)(Vg_UserMsg, "");
   }

   HChar* xpre  = VG_(clo_xml) ? "<preamble>" : "";
   HChar* xpost = VG_(clo_xml) ? "</preamble>" : "";

   if (VG_(clo_verbosity > 0)) {
      /* Tool details */
      VG_(message)(Vg_UserMsg, "%s%s%s%s, %s.%s",
                   xpre,
                   VG_(details).name, 
                   NULL == VG_(details).version ? "" : "-",
                   NULL == VG_(details).version 
                      ? (Char*)"" : VG_(details).version,
                   VG_(details).description,
                   xpost);
      VG_(message)(Vg_UserMsg, "%s%s%s", 
                               xpre, VG_(details).copyright_author, xpost);

      /* Core details */
      VG_(message)(Vg_UserMsg,
         "%sUsing LibVEX rev %s, a library for dynamic binary translation.%s",
         xpre, LibVEX_Version(), xpost );
      VG_(message)(Vg_UserMsg, 
         "%sCopyright (C) 2004-2005, and GNU GPL'd, by OpenWorks LLP.%s",
         xpre, xpost );
      VG_(message)(Vg_UserMsg,
         "%sUsing valgrind-%s, a dynamic binary instrumentation framework.%s",
         xpre, VERSION, xpost);
      VG_(message)(Vg_UserMsg, 
         "%sCopyright (C) 2000-2005, and GNU GPL'd, by Julian Seward et al.%s",
         xpre, xpost );
   }

   if (VG_(clo_verbosity) > 0 && log_to != VgLogTo_Fd) {
      VG_(message)(Vg_UserMsg, "");
      VG_(message)(Vg_UserMsg, 
         "My PID = %d, parent PID = %d.  Prog and args are:",
         VG_(getpid)(), VG_(getppid)() );
      for (i = 0; i < VG_(client_argc); i++) 
         VG_(message)(Vg_UserMsg, "   %s", VG_(client_argv)[i]);
   }
   else
   if (VG_(clo_xml)) {
      VG_(message)(Vg_UserMsg, "");
      VG_(message)(Vg_UserMsg, "<pid>%d</pid>", VG_(getpid)());
      VG_(message)(Vg_UserMsg, "<ppid>%d</ppid>", VG_(getppid)());
      VG_(message)(Vg_UserMsg, "");
      VG_(message)(Vg_UserMsg, "<argv>");   
      for (i = 0; i < VG_(client_argc); i++) 
         VG_(message)(Vg_UserMsg, "  <arg>%s</arg>", VG_(client_argv)[i]);
      VG_(message)(Vg_UserMsg, "</argv>");   
   }

   if (VG_(clo_verbosity) > 1) {
      Int fd;
      if (log_to != VgLogTo_Fd)
         VG_(message)(Vg_DebugMsg, "");
      VG_(message)(Vg_DebugMsg, "Valgrind library directory: %s", VG_(libdir));
      VG_(message)(Vg_DebugMsg, "Command line");
      for (i = 0; i < VG_(client_argc); i++)
         VG_(message)(Vg_DebugMsg, "   %s", VG_(client_argv)[i]);

      VG_(message)(Vg_DebugMsg, "Startup, with flags:");
      for (i = 1; i < vg_argc; i++) {
         VG_(message)(Vg_DebugMsg, "   %s", vg_argv[i]);
      }

      VG_(message)(Vg_DebugMsg, "Contents of /proc/version:");
      fd = VG_(open) ( "/proc/version", VKI_O_RDONLY, 0 );
      if (fd < 0) {
         VG_(message)(Vg_DebugMsg, "  can't open /proc/version");
      } else {
#        define BUF_LEN    256
         Char version_buf[BUF_LEN];
         Int n = VG_(read) ( fd, version_buf, BUF_LEN );
         vg_assert(n <= BUF_LEN);
         if (n > 0) {
            version_buf[n-1] = '\0';
            VG_(message)(Vg_DebugMsg, "  %s", version_buf);
         } else {
            VG_(message)(Vg_DebugMsg, "  (empty?)");
         }
         VG_(close)(fd);
#        undef BUF_LEN
      }
   }

   if (VG_(clo_n_suppressions) < VG_CLO_MAX_SFILES-1 &&
       (VG_(needs).core_errors || VG_(needs).tool_errors)) {
      /* If there are no suppression files specified and the tool
	 needs one, load the default */
      static const Char default_supp[] = "default.supp";
      Int len = VG_(strlen)(VG_(libdir)) + 1 + sizeof(default_supp);
      Char *buf = VG_(arena_malloc)(VG_AR_CORE, len);
      VG_(sprintf)(buf, "%s/%s", VG_(libdir), default_supp);
      VG_(clo_suppressions)[VG_(clo_n_suppressions)] = buf;
      VG_(clo_n_suppressions)++;
   }
}

// Build the string for VALGRINDCLO.
Char* VG_(build_child_VALGRINDCLO)( Char* exename )
{
   /* If we're tracing the children, then we need to start it
      with our starter+arguments, which are copied into VALGRINDCLO,
      except the --exec= option is changed if present.
   */
   Int i;
   Char *exec;
   Char *cp;
   Char *optvar;
   Int  optlen, execlen;

   // All these allocated blocks are not free - because we're either
   // going to exec, or panic when we fail.

   // Create --exec= option: "--exec=<exename>"
   exec = VG_(arena_malloc)(VG_AR_CORE, 
                            VG_(strlen)( exename ) + 7/*--exec=*/ + 1/*\0*/);
   vg_assert(NULL != exec);
   VG_(sprintf)(exec, "--exec=%s", exename);

   // Allocate space for optvar (may overestimate by counting --exec twice,
   // no matter)
   optlen = 1;
   for (i = 0; i < vg_argc; i++)
      optlen += VG_(strlen)(vg_argv[i]) + 1;
   optlen += VG_(strlen)(exec)+1;
   optvar = VG_(arena_malloc)(VG_AR_CORE, optlen);

   // Copy all valgrind args except the old --exec (if present)
   // VG_CLO_SEP is the separator.
   cp = optvar;
   for (i = 1; i < vg_argc; i++) {
      Char *arg = vg_argv[i];
      
      if (VG_(memcmp)(arg, "--exec=", 7) == 0) {
         // don't copy existing --exec= arg
      } else if (VG_(strcmp)(arg, "--") == 0) {
         // stop at "--"
         break;
      } else {
         // copy non "--exec" arg
         Int len = VG_(strlen)(arg);
         VG_(memcpy)(cp, arg, len);
         cp += len;
         *cp++ = VG_CLO_SEP;
      }
   }
   // Add the new --exec= option
   execlen = VG_(strlen)(exec);
   VG_(memcpy)(cp, exec, execlen);
   cp += execlen;
   *cp++ = VG_CLO_SEP;

   *cp = '\0';

   return optvar;
}

// Build "/proc/self/fd/<execfd>".
Char* VG_(build_child_exename)( void )
{
   Char* exename = VG_(arena_malloc)(VG_AR_CORE, 64);
   vg_assert(NULL != exename);
   VG_(sprintf)(exename, "/proc/self/fd/%d", vgexecfd);
   return exename;
}


/*====================================================================*/
/*=== File descriptor setup                                        ===*/
/*====================================================================*/

static void setup_file_descriptors(void)
{
   struct vki_rlimit rl;

   /* Get the current file descriptor limits. */
   if (VG_(getrlimit)(VKI_RLIMIT_NOFILE, &rl) < 0) {
      rl.rlim_cur = 1024;
      rl.rlim_max = 1024;
   }

   /* Work out where to move the soft limit to. */
   if (rl.rlim_cur + N_RESERVED_FDS <= rl.rlim_max) {
      rl.rlim_cur = rl.rlim_cur + N_RESERVED_FDS;
   } else {
      rl.rlim_cur = rl.rlim_max;
   }

   /* Reserve some file descriptors for our use. */
   VG_(fd_soft_limit) = rl.rlim_cur - N_RESERVED_FDS;
   VG_(fd_hard_limit) = rl.rlim_cur - N_RESERVED_FDS;

   /* Update the soft limit. */
   VG_(setrlimit)(VKI_RLIMIT_NOFILE, &rl);

   if (vgexecfd != -1)
      vgexecfd = VG_(safe_fd)( vgexecfd );
   if (VG_(clexecfd) != -1)
      VG_(clexecfd) = VG_(safe_fd)( VG_(clexecfd) );
}

/*====================================================================*/
/*===  Initialise program data/text, etc.                          ===*/
/*====================================================================*/

static void build_valgrind_map_callback ( Addr start, SizeT size, UInt prot,
					  UInt dev, UInt ino, ULong foffset, 
					  const UChar* filename )
{
   /* Only record valgrind mappings for now, without loading any
      symbols.  This is so we know where the free space is before we
      start allocating more memory (note: heap is OK, it's just mmap
      which is the problem here). */
   if (start >= VG_(client_end) && start < VG_(valgrind_last)) {
      VG_(debugLog)(2, "main",
                    "valgrind-seg: %p-%p prot 0x%x file=%s\n",
                    (void*)start, (void*)(start+size), prot, filename);
      VG_(map_file_segment)(start, size, prot,
                            SF_MMAP|SF_NOSYMS|SF_VALGRIND,
                            dev, ino, foffset, filename);
      /* update VG_(valgrind_last) if it looks wrong */
      if (start+size > VG_(valgrind_last))
         VG_(valgrind_last) = start+size-1;
   }
}

// Global var used to pass local data to callback
Addr sp_at_startup___global_arg = 0;

/* 
   This second pass adds in client mappings, and loads symbol tables
   for all interesting mappings.  The trouble is that things can
   change as we go, because we're calling the Tool to track memory as
   we find it.

   So for Valgrind mappings, we don't replace any mappings which
   aren't still identical (which will include the .so mappings, so we
   will load their symtabs)>
 */
static void build_segment_map_callback ( Addr start, SizeT size, UInt prot,
					 UInt dev, UInt ino, ULong foffset,
					 const UChar* filename )
{
   UInt flags;
   Bool is_stack_segment;
   Addr r_esp;

   is_stack_segment 
      = (start == VG_(clstk_base) && (start+size) == VG_(clstk_end));

   VG_(debugLog)(2, "main",
                 "any-seg: %p-%p prot 0x%x stack=%d file=%s\n",
                 (void*)start, (void*)(start+size), prot, is_stack_segment, 
                 filename);

   if (is_stack_segment)
      flags = SF_STACK | SF_GROWDOWN;
   else
      flags = SF_EXEC|SF_MMAP;

   if (filename != NULL)
      flags |= SF_FILE;

#if 0
   // This needs to be fixed properly.   jrs 20050307
   if (start >= VG_(client_end) && start < VG_(valgrind_last)) {
      Segment *s = VG_(find_segment_before)(start);

      /* We have to be a bit careful about inserting new mappings into
	 the Valgrind part of the address space.  We're actively
	 changing things as we parse these mappings, particularly in
	 shadow memory, and so we don't want to overwrite those
	 changes.  Therefore, we only insert/update a mapping if it is
	 mapped from a file or it exactly matches an existing mapping.

	 NOTE: we're only talking about the Segment list mapping
	 metadata; this doesn't actually mmap anything more. */
      if (filename || (s && s->addr == start && s->len == size)) {
	 flags |= SF_VALGRIND;
	 VG_(map_file_segment)(start, size, prot, flags, dev, ino, foffset, filename);
      } else {
	 /* assert range is already mapped */
	 vg_assert(VG_(is_addressable)(start, size, VKI_PROT_NONE));
      }
   } else
#endif
      VG_(map_file_segment)(start, size, prot, flags, dev, ino, foffset, filename);

   if (VG_(is_client_addr)(start) && VG_(is_client_addr)(start+size-1)) {
	   VG_TRACK( new_mem_startup, start, size,
		     !!(prot & VKI_PROT_READ), 
                     !!(prot & VKI_PROT_WRITE), 
                     !!(prot & VKI_PROT_EXEC));
   }

   /* If this is the stack segment mark all below %esp as noaccess. */
   r_esp = sp_at_startup___global_arg;
   vg_assert(0 != r_esp);
   if (is_stack_segment) {
      if (0) {
         VG_(message)(Vg_DebugMsg, "invalidating stack area: %p .. %p",
                      start,r_esp);
         VG_(message)(Vg_DebugMsg, "  validating stack area: %p .. %p",
                      r_esp, start+size);
      }
      VG_TRACK( die_mem_stack, start, r_esp-start );
      // what's this for?
      //VG_TRACK( post_mem_write, r_esp, (start+size)-r_esp );
   }
}


/*====================================================================*/
/*=== Sanity check machinery (permanently engaged)                 ===*/
/*====================================================================*/

/* A fast sanity check -- suitable for calling circa once per
   millisecond. */

void VG_(sanity_check_general) ( Bool force_expensive )
{
   ThreadId tid;

   VGP_PUSHCC(VgpCoreCheapSanity);

   if (VG_(clo_sanity_level) < 1) return;

   /* --- First do all the tests that we can do quickly. ---*/

   sanity_fast_count++;

   /* Check stuff pertaining to the memory check system. */

   /* Check that nobody has spuriously claimed that the first or
      last 16 pages of memory have become accessible [...] */
   if (VG_(needs).sanity_checks) {
      VGP_PUSHCC(VgpToolCheapSanity);
      vg_assert(VG_TDICT_CALL(tool_cheap_sanity_check));
      VGP_POPCC(VgpToolCheapSanity);
   }

   /* --- Now some more expensive checks. ---*/

   /* Once every 25 times, check some more expensive stuff. */
   if ( force_expensive
     || VG_(clo_sanity_level) > 1
     || (VG_(clo_sanity_level) == 1 && (sanity_fast_count % 25) == 0)) {

      VGP_PUSHCC(VgpCoreExpensiveSanity);
      sanity_slow_count++;

#     if 0
      { void zzzmemscan(void); zzzmemscan(); }
#     endif

      if ((sanity_fast_count % 250) == 0)
         VG_(sanity_check_tt_tc)("VG_(sanity_check_general)");

      if (VG_(needs).sanity_checks) {
          VGP_PUSHCC(VgpToolExpensiveSanity);
          vg_assert(VG_TDICT_CALL(tool_expensive_sanity_check));
          VGP_POPCC(VgpToolExpensiveSanity);
      }

      /* Check that Segments and /proc/self/maps match up */
      //vg_assert(VG_(sanity_check_memory)());

      /* Look for stack overruns.  Visit all threads. */
      for(tid = 1; tid < VG_N_THREADS; tid++) {
	 SSizeT remains;

	 if (VG_(threads)[tid].status == VgTs_Empty ||
	     VG_(threads)[tid].status == VgTs_Zombie)
	    continue;

	 remains = VGA_(stack_unused)(tid);
	 if (remains < VKI_PAGE_SIZE)
	    VG_(message)(Vg_DebugMsg, 
                         "WARNING: Thread %d is within %d bytes "
                         "of running out of stack!",
		         tid, remains);
      }

      /* 
      if ((sanity_fast_count % 500) == 0) VG_(mallocSanityCheckAll)(); 
      */
      VGP_POPCC(VgpCoreExpensiveSanity);
   }

   if (VG_(clo_sanity_level) > 1) {
      VGP_PUSHCC(VgpCoreExpensiveSanity);
      /* Check sanity of the low-level memory manager.  Note that bugs
         in the client's code can cause this to fail, so we don't do
         this check unless specially asked for.  And because it's
         potentially very expensive. */
      VG_(sanity_check_malloc_all)();
      VGP_POPCC(VgpCoreExpensiveSanity);
   }
   VGP_POPCC(VgpCoreCheapSanity);
}


/*====================================================================*/
/*=== main()                                                       ===*/
/*====================================================================*/

/*
  This code decides on the layout of the client and Valgrind address
  spaces, loads valgrind.so and the tool.so into the valgrind part,
  loads the client executable (and the dynamic linker, if necessary)
  into the client part, and calls into Valgrind proper.

  The code is careful not to allow spurious mappings to appear in the
  wrong parts of the address space.  In particular, to make sure
  dlopen puts things in the right place, it will pad out the forbidden
  chunks of address space so that dlopen is forced to put things where
  we want them.

  The memory map it creates is:

  client_base    +-------------------------+
                 | client address space    |
	         :                         :
	         :                         :
		 | client stack            |
  client_end     +-------------------------+
                 | redzone                 |
  shadow_base    +-------------------------+
                 |                         |
	         : shadow memory for tools :
	         | (may be 0 sized)        |
  shadow_end     +-------------------------+
  valgrind_base  +-------------------------+
                 | kickstart executable    |
                 | valgrind heap  vvvvvvvvv| (barely used)
                 -                         -
                 | valgrind .so files      |
		 | and mappings            |
                 -                         -
                 | valgrind stack ^^^^^^^^^|
  valgrind_last  +-------------------------+
		 : kernel                  :

  Nb: Before we can do general allocations with VG_(arena_malloc)() and
  VG_(mmap)(), we need to build the segment skip-list, so we know where
  we can put things.  However, building that structure requires
  allocating memory.  So we need to a bootstrapping process.  It's done
  by making VG_(arena_malloc)() have a special static superblock that's
  used for the first 1MB's worth of allocations.  This is enough to
  build the segment skip-list.
*/


/* This may be needed before m_mylibc is OK to run. */
static Int local_strcmp ( const HChar* s1, const HChar* s2 )
{
   while (True) {
      if (*s1 == 0 && *s2 == 0) return 0;
      if (*s1 == 0) return -1;
      if (*s2 == 0) return 1;

      if (*(UChar*)s1 < *(UChar*)s2) return -1;
      if (*(UChar*)s1 > *(UChar*)s2) return 1;

      s1++; s2++;
   }
}


int main(int argc, char **argv, char **envp)
{
   char **cl_argv;
   const char *tool = "memcheck";   // default to Memcheck
   const char *exec = NULL;
   char *preload;          /* tool-specific LD_PRELOAD .so */
   char **env;
   Int need_help = 0;      // 0 = no, 1 = --help, 2 = --help-debug
   struct exeinfo info;
   ToolInfo *toolinfo = NULL;
   Addr client_eip;
   Addr sp_at_startup;     /* client's SP at the point we gained control. */
   UInt * client_auxv;
   struct vki_rlimit zero = { 0, 0 };
   Int padfile, loglevel, i;

   //============================================================
   // Nb: startup is complex.  Prerequisites are shown at every step.
   //
   // *** Be very careful when messing with the order ***
   //============================================================

   //--------------------------------------------------------------
   // Start up the logging mechanism
   //   p: none
   //--------------------------------------------------------------
   /* Start the debugging-log system ASAP.  First find out how many 
      "-d"s were specified.  This is a pre-scan of the command line. */
   loglevel = 0;
   for (i = 1; i < argc; i++) {
     if (argv[i][0] != '-')
        break;
     if (0 == local_strcmp(argv[i], "--")) 
        break;
     if (0 == local_strcmp(argv[i], "-d")) 
        loglevel++;
   }

   /* ... and start the debug logger.  Now we can safely emit logging
      messages all through startup. */
   VG_(debugLog_startup)(loglevel, "Stage 2");

   //============================================================
   // Command line argument handling order:
   // * If --help/--help-debug are present, show usage message 
   //   (including the tool-specific usage)
   // * (If no --tool option given, default to Memcheck)
   // * Then, if client is missing, abort with error msg
   // * Then, if any cmdline args are bad, abort with error msg
   //============================================================

   // Get the current process datasize rlimit, and set it to zero.
   // This prevents any internal uses of brk() from having any effect.
   // We remember the old value so we can restore it on exec, so that
   // child processes will have a reasonable brk value.
   VG_(getrlimit)(VKI_RLIMIT_DATA, &VG_(client_rlimit_data));
   zero.rlim_max = VG_(client_rlimit_data).rlim_max;
   VG_(setrlimit)(VKI_RLIMIT_DATA, &zero);

   // Get the current process stack rlimit.
   VG_(getrlimit)(VKI_RLIMIT_STACK, &VG_(client_rlimit_stack));

   //--------------------------------------------------------------
   // Check we were launched by stage1
   //   p: none
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Doing scan_auxv()\n");
   {
   void* init_sp = argv - 1;
   padfile = scan_auxv(init_sp);
   }

   //--------------------------------------------------------------
   // Look for alternative libdir                                  
   //   p: none
   //--------------------------------------------------------------
   {  HChar *cp = getenv(VALGRINDLIB);
      if (cp != NULL)
	 VG_(libdir) = cp;
   }

   //--------------------------------------------------------------
   // Get valgrind args + client args (inc. from VALGRIND_OPTS/.valgrindrc).
   // Pre-process the command line.
   //   p: none
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Preprocess command line opts\n");
   get_command_line(argc, argv, &vg_argc, &vg_argv, &cl_argv);
   pre_process_cmd_line_options(&need_help, &tool, &exec);

   //==============================================================
   // Nb: once a tool is specified, the tool.so must be loaded even if 
   // they specified --help or didn't specify a client program.
   //==============================================================

   //--------------------------------------------------------------
   // With client padded out, map in tool
   //   p: set-libdir                     [for VG_(libdir)]
   //   p: pre_process_cmd_line_options() [for 'tool']
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Loading tool\n");
   load_tool(tool, &toolinfo, &preload);

   //==============================================================
   // Can use VG_(malloc)() and VG_(arena_malloc)() only after load_tool()
   // -- redzone size is now set.  This is checked by vg_malloc2.c.
   //==============================================================
   
   //--------------------------------------------------------------
   // Finalise address space layout
   //   p: load_tool()  [for 'toolinfo']
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Laying out remaining space\n");
   layout_remaining_space( (Addr) & argc, toolinfo->shadow_ratio );

   //--------------------------------------------------------------
   // Load client executable, finding in $PATH if necessary
   //   p: pre_process_cmd_line_options()  [for 'exec', 'need_help']
   //   p: layout_remaining_space          [so there's space]
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Loading client\n");
   load_client(cl_argv, exec, need_help, &info, &client_eip);

   //--------------------------------------------------------------
   // Everything in place, remove padding done by stage1
   //   p: layout_remaining_space()  [everything must be mapped in before now]  
   //   p: load_client()             [ditto] 
   //--------------------------------------------------------------
   as_unpad((void *)VG_(shadow_end), (void *)~0, padfile);
   as_closepadfile(padfile);  // no more padding

   //--------------------------------------------------------------
   // Set up client's environment
   //   p: set-libdir  [for VG_(libdir)]
   //   p: load_tool() [for 'preload']
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Setup client env\n");
   env = fix_environment(envp, preload);

   //--------------------------------------------------------------
   // Setup client stack, eip, and VG_(client_arg[cv])
   //   p: load_client()     [for 'info']
   //   p: fix_environment() [for 'env']
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Setup client stack\n");
   { 
   void* init_sp = argv - 1;

   sp_at_startup = setup_client_stack(init_sp, cl_argv, env, &info,
                                      &client_auxv);
   free(env);
   }

   VG_(debugLog)(2, "main",
                    "Client info: "
                    "entry=%p client esp=%p vg_argc=%d brkbase=%p\n",
                    (void*)client_eip, (void*)sp_at_startup, vg_argc, 
                    (void*)VG_(brk_base) );

   //==============================================================
   // Finished setting up operating environment.  Now initialise
   // Valgrind.  (This is where the old VG_(main)() started.)
   //==============================================================

   //--------------------------------------------------------------
   // setup file descriptors
   //   p: n/a
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Setup file descriptors\n");
   setup_file_descriptors();

   //--------------------------------------------------------------
   // Build segment map (Valgrind segments only)
   //   p: tl_pre_clo_init()  [to setup new_mem_startup tracker]
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Parse /proc/self/maps (round 1)\n");
   VG_(parse_procselfmaps) ( build_valgrind_map_callback );

   //==============================================================
   // Can use VG_(arena_malloc)() with non-CORE arena after segments set up
   //==============================================================

   //--------------------------------------------------------------
   // Init tool: pre_clo_init, process cmd line, post_clo_init
   //   p: setup_client_stack()      [for 'VG_(client_arg[cv]']
   //   p: load_tool()               [for 'toolinfo']
   //   p: setup_file_descriptors()  [for 'VG_(fd_xxx_limit)']
   //   p: parse_procselfmaps        [so VG segments are setup so tool can
   //                                 call VG_(malloc)]
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Initialise the tool\n");
   (*toolinfo->tl_pre_clo_init)();
   VG_(sanity_check_needs)();

   // If --tool and --help/--help-debug was given, now give the core+tool
   // help message
   if (need_help) {
      usage(/*--help-debug?*/2 == need_help);
   }
   process_cmd_line_options(client_auxv, tool);

   VG_TDICT_CALL(tool_post_clo_init);

   //--------------------------------------------------------------
   // Build segment map (all segments)
   //   p: shadow/redzone segments
   //   p: setup_client_stack()  [for 'sp_at_startup']
   //   p: init tool             [for 'new_mem_startup']
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Parse /proc/self/maps (round 2)\n");
   sp_at_startup___global_arg = sp_at_startup;
   VG_(parse_procselfmaps) ( build_segment_map_callback );  /* everything */
   sp_at_startup___global_arg = 0;
   
#if defined(VGP_x86_linux) || defined(VGP_amd64_linux)
   //--------------------------------------------------------------
   // Protect client trampoline page (which is also sysinfo stuff)
   //   p: segment stuff   [otherwise get seg faults...]
   //--------------------------------------------------------------
   {
      Segment *seg;
      VG_(mprotect)( (void *)VG_(client_trampoline_code),
		     VG_(trampoline_code_length), VKI_PROT_READ|VKI_PROT_EXEC );

      /* Make sure this segment isn't treated as stack */
      seg = VG_(find_segment)(VG_(client_trampoline_code));
      if (seg)
	 seg->flags &= ~(SF_STACK | SF_GROWDOWN);
   }
#endif

   //==============================================================
   // Can use VG_(map)() after segments set up
   //==============================================================

   //--------------------------------------------------------------
   // Allow GDB attach
   //   p: process_cmd_line_options()  [for VG_(clo_wait_for_gdb)]
   //--------------------------------------------------------------
   /* Hook to delay things long enough so we can get the pid and
      attach GDB in another shell. */
   if (VG_(clo_wait_for_gdb)) {
      VG_(debugLog)(1, "main", "Wait for GDB\n");
      VG_(printf)("pid=%d, entering delay loop\n", VG_(getpid)());
      /* jrs 20050206: I don't understand why this works on x86.  On
         amd64 the obvious analogues (jump *$rip or jump *$rcx) don't
         work. */
      /* do "jump *$eip" to skip this in gdb (x86) */
      //VG_(do_syscall0)(__NR_pause);
      { Long q; for (q = 0; q < 10ULL *1000*1000*1000; q++) ; }
   }

   //--------------------------------------------------------------
   // Search for file descriptors that are inherited from our parent
   //   p: process_cmd_line_options  [for VG_(clo_track_fds)]
   //--------------------------------------------------------------
   if (VG_(clo_track_fds)) {
      VG_(debugLog)(1, "main", "Init preopened fds\n");
      VG_(init_preopened_fds)();
   }

   //--------------------------------------------------------------
   // Initialise the scheduler
   //   p: setup_file_descriptors() [else VG_(safe_fd)() breaks]
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Initialise scheduler\n");
   VG_(scheduler_init)();

   //--------------------------------------------------------------
   // Initialise the pthread model
   //   p: ?
   //      load_client()          [for 'client_eip']
   //      setup_client_stack()   [for 'sp_at_startup']
   //      setup_scheduler()      [for the rest of state 1 stuff]
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Initialise thread 1's state\n");
   VGA_(init_thread1state)(client_eip, sp_at_startup, &VG_(threads)[1].arch );

   // Tell the tool that we just wrote to the registers.
   VG_TRACK( post_reg_write, Vg_CoreStartup, /*tid*/1, /*offset*/0,
             sizeof(VexGuestArchState));

   //--------------------------------------------------------------
   // Initialise the pthread model
   //   p: ?
   //--------------------------------------------------------------
   //if (VG_(clo_model_pthreads))
   //   VG_(pthread_init)();

   //--------------------------------------------------------------
   // Initialise the signal handling subsystem
   //   p: n/a
   //--------------------------------------------------------------
   // Nb: temporarily parks the saved blocking-mask in saved_sigmask.
   VG_(debugLog)(1, "main", "Initialise signal management\n");
   VG_(sigstartup_actions)();

   //--------------------------------------------------------------
   // Perhaps we're profiling Valgrind?
   //   p: process_cmd_line_options()  [for VG_(clo_profile)]
   //   p: others?
   //
   // XXX: this seems to be broken?   It always says the tool wasn't built
   // for profiling;  vg_profile.c's functions don't seem to be overriding
   // vg_dummy_profile.c's?
   //
   // XXX: want this as early as possible.  Looking for --profile
   // in pre_process_cmd_line_options() could get it earlier.
   //--------------------------------------------------------------
   if (VG_(clo_profile))
      VG_(init_profiling)();

   VGP_PUSHCC(VgpStartup);

   //--------------------------------------------------------------
   // Read suppression file
   //   p: process_cmd_line_options()  [for VG_(clo_suppressions)]
   //--------------------------------------------------------------
   if (VG_(needs).core_errors || VG_(needs).tool_errors) {
      VG_(debugLog)(1, "main", "Load suppressions\n");
      VG_(load_suppressions)();
   }

   //--------------------------------------------------------------
   // Initialise translation table and translation cache
   //   p: read_procselfmaps  [so the anonymous mmaps for the TT/TC
   //         aren't identified as part of the client, which would waste
   //         > 20M of virtual address space.]
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Initialise TT/TC\n");
   VG_(init_tt_tc)();

   //--------------------------------------------------------------
   // Read debug info to find glibc entry points to intercept
   //   p: parse_procselfmaps? [XXX for debug info?]
   //   p: init_tt_tc [so it can call VG_(search_transtab) safely]
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Initialise redirects\n");
   VG_(setup_code_redirect_table)();
   VGP_(setup_redirects)();

   //--------------------------------------------------------------
   // Verbosity message
   //   p: end_rdtsc_calibration [so startup message is printed first]
   //--------------------------------------------------------------
   if (VG_(clo_verbosity) == 1 && !VG_(clo_xml))
      VG_(message)(Vg_UserMsg, "For more details, rerun with: -v");
   if (VG_(clo_verbosity) > 0)
      VG_(message)(Vg_UserMsg, "");

   //--------------------------------------------------------------
   // Setup pointercheck
   //   p: process_cmd_line_options() [for VG_(clo_pointercheck)]
   //--------------------------------------------------------------
   if (VG_(clo_pointercheck))
      VG_(clo_pointercheck) = VGA_(setup_pointercheck)();

   //--------------------------------------------------------------
   // Run!
   //--------------------------------------------------------------
   VGP_POPCC(VgpStartup);

   vg_assert(VG_(master_tid) == 1);

   if (VG_(clo_xml)) {
      VG_(message)(Vg_UserMsg, "<status>RUNNING</status>");
      VG_(message)(Vg_UserMsg, "");
   }

   VG_(debugLog)(1, "main", "Running thread 1\n");
   VGA_(main_thread_wrapper)(1);

   abort();
}


/* Do everything which needs doing when the last thread exits */
void VG_(shutdown_actions)(ThreadId tid)
{
   vg_assert(tid == VG_(master_tid));
   vg_assert(VG_(is_running_thread)(tid));

   // Wait for all other threads to exit.
   VGA_(reap_threads)(tid);

   VG_(clo_model_pthreads) = False;

   // Clean the client up before the final report
   VGA_(final_tidyup)(tid);

   // OK, done
   VG_(exit_thread)(tid);

   /* should be no threads left */
   vg_assert(VG_(count_living_threads)() == 0);

   VG_(threads)[tid].status = VgTs_Empty;
   //--------------------------------------------------------------
   // Finalisation: cleanup, messages, etc.  Order no so important, only
   // affects what order the messages come.
   //--------------------------------------------------------------
   if (VG_(clo_verbosity) > 0)
      VG_(message)(Vg_UserMsg, "");

   if (VG_(clo_xml)) {
      VG_(message)(Vg_UserMsg, "<status>FINISHED</status>");
      VG_(message)(Vg_UserMsg, "");
   }

   /* Print out file descriptor summary and stats. */
   if (VG_(clo_track_fds))
      VG_(show_open_fds)();

   if (VG_(needs).core_errors || VG_(needs).tool_errors)
      VG_(show_all_errors)();

   VG_TDICT_CALL(tool_fini, 0/*exitcode*/);

   if (VG_(clo_xml)) {
      VG_(message)(Vg_UserMsg, "");
      VG_(message)(Vg_UserMsg, "</valgrindoutput>");
      VG_(message)(Vg_UserMsg, "");
   }

   VG_(sanity_check_general)( True /*include expensive checks*/ );

   if (VG_(clo_verbosity) > 1)
      print_all_stats();

   if (VG_(clo_profile))
      VG_(done_profiling)();
   if (VG_(clo_profile_flags) > 0)
      VG_(show_BB_profile)();

   /* Print Vex storage stats */
   if (0)
       LibVEX_ShowAllocStats();
}

/*--------------------------------------------------------------------*/
/*--- end                                                vg_main.c ---*/
/*--------------------------------------------------------------------*/
