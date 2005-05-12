/*-*- c -*- ----------------------------------------------------------*/
/*--- Header for lots of tool stuff.                        tool.h ---*/
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

#ifndef __TOOL_H
#define __TOOL_H

#include <stdarg.h>       /* ANSI varargs stuff  */

#include "basic_types.h"
#include "tool_asm.h"           /* asm stuff */
#include "tool_arch.h"          /* arch-specific tool stuff */
#include "vki.h"

#include "pub_tool_errormgr.h"      // needed for 'Error', 'Supp'
#include "pub_tool_execontext.h"    // needed for 'ExeContext'

#include "libvex.h"
#include "libvex_ir.h"

/*====================================================================*/
/*=== Build options and table sizes.                               ===*/
/*====================================================================*/

/* The maximum number of pthreads that we support.  This is
   deliberately not very high since our implementation of some of the
   scheduler algorithms is surely O(N) in the number of threads, since
   that's simple, at least.  And (in practice) we hope that most
   programs do not need many threads. */
#define VG_N_THREADS 100


/*====================================================================*/
/*=== Useful macros                                                ===*/
/*====================================================================*/

/* No, really.  I _am_ that strange. */
#define OINK(nnn) VG_(message)(Vg_DebugMsg, "OINK %d",nnn)

/* Path to all our library/aux files */
extern const Char *VG_(libdir);


/*====================================================================*/
/*=== Command-line options                                         ===*/
/*====================================================================*/

/* Use this for normal null-termination-style string comparison */
#define VG_STREQ(s1,s2) (s1 != NULL && s2 != NULL \
                         && VG_(strcmp)((s1),(s2))==0)

/* Use these for recognising tool command line options -- stops comparing
   once whitespace is reached. */
#define VG_CLO_STREQ(s1,s2)     (0==VG_(strcmp_ws)((s1),(s2)))
#define VG_CLO_STREQN(nn,s1,s2) (0==VG_(strncmp_ws)((s1),(s2),(nn)))

/* Higher-level command-line option recognisers;  use in if/else chains */

#define VG_BOOL_CLO(qq_arg, qq_option, qq_var) \
        if (VG_CLO_STREQ(qq_arg, qq_option"=yes")) { (qq_var) = True; } \
   else if (VG_CLO_STREQ(qq_arg, qq_option"=no"))  { (qq_var) = False; }

#define VG_STR_CLO(qq_arg, qq_option, qq_var) \
   if (VG_CLO_STREQN(VG_(strlen)(qq_option)+1, qq_arg, qq_option"=")) { \
      (qq_var) = &qq_arg[ VG_(strlen)(qq_option)+1 ]; \
   }

#define VG_NUM_CLO(qq_arg, qq_option, qq_var) \
   if (VG_CLO_STREQN(VG_(strlen)(qq_option)+1, qq_arg, qq_option"=")) { \
      (qq_var) = (Int)VG_(atoll)( &qq_arg[ VG_(strlen)(qq_option)+1 ] ); \
   }

/* Bounded integer arg */
#define VG_BNUM_CLO(qq_arg, qq_option, qq_var, qq_lo, qq_hi) \
   if (VG_CLO_STREQN(VG_(strlen)(qq_option)+1, qq_arg, qq_option"=")) { \
      (qq_var) = (Int)VG_(atoll)( &qq_arg[ VG_(strlen)(qq_option)+1 ] ); \
      if ((qq_var) < (qq_lo)) (qq_var) = (qq_lo); \
      if ((qq_var) > (qq_hi)) (qq_var) = (qq_hi); \
   }


/* Verbosity level: 0 = silent, 1 (default), > 1 = more verbose. */
extern Int   VG_(clo_verbosity);

/* Profile? */
extern Bool  VG_(clo_profile);

/* Call this if a recognised option was bad for some reason.
   Note: don't use it just because an option was unrecognised -- return 'False'
   from VG_(tdict).tool_process_cmd_line_option) to indicate that. */
extern void VG_(bad_option) ( Char* opt );

/* Client args */
extern Int    VG_(client_argc);
extern Char** VG_(client_argv);

/* Client environment.  Can be inspected with VG_(getenv)() */
extern Char** VG_(client_envp);


/*====================================================================*/
/*=== Printing messages for the user                               ===*/
/*====================================================================*/

/* Print a message prefixed by "??<pid>?? "; '?' depends on the VgMsgKind.
   Should be used for all user output. */

typedef
   enum { Vg_UserMsg,         /* '?' == '=' */
          Vg_DebugMsg,        /* '?' == '-' */
          Vg_DebugExtraMsg,   /* '?' == '+' */
          Vg_ClientMsg        /* '?' == '*' */
   }
   VgMsgKind;

/* Send a single-part message.  Appends a newline. */
extern UInt VG_(message)    ( VgMsgKind kind, const Char* format, ... );
extern UInt VG_(vmessage)   ( VgMsgKind kind, const Char* format, va_list vargs );


/*====================================================================*/
/*=== Profiling                                                    ===*/
/*====================================================================*/

/* Nb: VG_(register_profile_event)() relies on VgpUnc being the first one */
#define VGP_CORE_LIST \
   /* These ones depend on the core */                \
   VGP_PAIR(VgpUnc,         "unclassified"),          \
   VGP_PAIR(VgpStartup,     "startup"),               \
   VGP_PAIR(VgpRun,         "running"),               \
   VGP_PAIR(VgpSched,       "scheduler"),             \
   VGP_PAIR(VgpMalloc,      "low-lev malloc/free"),   \
   VGP_PAIR(VgpCliMalloc,   "client  malloc/free"),   \
   VGP_PAIR(VgpTranslate,   "translate-main"),        \
   VGP_PAIR(VgpToUCode,     "to-ucode"),              \
   VGP_PAIR(VgpFromUcode,   "from-ucode"),            \
   VGP_PAIR(VgpImprove,     "improve"),               \
   VGP_PAIR(VgpESPUpdate,   "ESP-update"),            \
   VGP_PAIR(VgpRegAlloc,    "reg-alloc"),             \
   VGP_PAIR(VgpLiveness,    "liveness-analysis"),     \
   VGP_PAIR(VgpDoLRU,       "do-lru"),                \
   VGP_PAIR(VgpSlowFindT,   "slow-search-transtab"),  \
   VGP_PAIR(VgpExeContext,  "exe-context"),           \
   VGP_PAIR(VgpReadSyms,    "read-syms"),             \
   VGP_PAIR(VgpSearchSyms,  "search-syms"),           \
   VGP_PAIR(VgpAddToT,      "add-to-transtab"),       \
   VGP_PAIR(VgpCoreSysWrap, "core-syscall-wrapper"),  \
   VGP_PAIR(VgpDemangle,    "demangle"),              \
   VGP_PAIR(VgpCoreCheapSanity,     "core-cheap-sanity"),     \
   VGP_PAIR(VgpCoreExpensiveSanity, "core-expensive-sanity"), \
   /* These ones depend on the tool */                \
   VGP_PAIR(VgpPreCloInit,  "pre-clo-init"),          \
   VGP_PAIR(VgpPostCloInit, "post-clo-init"),         \
   VGP_PAIR(VgpInstrument,  "instrument"),            \
   VGP_PAIR(VgpToolSysWrap, "tool-syscall-wrapper"),  \
   VGP_PAIR(VgpToolCheapSanity,     "tool-cheap-sanity"),     \
   VGP_PAIR(VgpToolExpensiveSanity, "tool-expensive-sanity"), \
   VGP_PAIR(VgpFini,        "fini")

#define VGP_PAIR(n,name) n
typedef enum { VGP_CORE_LIST } VgpCoreCC;
#undef  VGP_PAIR

/* When registering tool profiling events, ensure that the 'n' value is in
 * the range (VgpFini+1..) */
extern void VG_(register_profile_event) ( Int n, Char* name );

extern void VG_(pushcc) ( UInt cc );
extern void VG_(popcc)  ( UInt cc );

/* Define them only if they haven't already been defined by vg_profile.c */
#ifndef VGP_PUSHCC
#  define VGP_PUSHCC(x)
#endif
#ifndef VGP_POPCC
#  define VGP_POPCC(x)
#endif


/*====================================================================*/
/*=== Useful stuff to call from generated code                     ===*/
/*====================================================================*/

/* ------------------------------------------------------------------ */
/* General stuff */

/* Check if an address/whatever is aligned */
#define VG_IS_4_ALIGNED(aaa_p)      (0 == (((Addr)(aaa_p)) & 0x3))
#define VG_IS_8_ALIGNED(aaa_p)      (0 == (((Addr)(aaa_p)) & 0x7))
#define VG_IS_16_ALIGNED(aaa_p)     (0 == (((Addr)(aaa_p)) & 0xf))
#define VG_IS_WORD_ALIGNED(aaa_p)   (0 == (((Addr)(aaa_p)) & (sizeof(Addr)-1)))
#define VG_IS_PAGE_ALIGNED(aaa_p)   (0 == (((Addr)(aaa_p)) & (VKI_PAGE_SIZE-1)))


/* ------------------------------------------------------------------ */
/* Thread-related stuff */

/* Special magic value for an invalid ThreadId.  It corresponds to
   LinuxThreads using zero as the initial value for
   pthread_mutex_t.__m_owner and pthread_cond_t.__c_waiting. */
#define VG_INVALID_THREADID ((ThreadId)(0))

/* Get the TID of the thread which currently has the CPU. */
extern ThreadId VG_(get_running_tid) ( void );

/* Searches through all thread's stacks to see if any match.  Returns
   VG_INVALID_THREADID if none match. */
extern ThreadId VG_(first_matching_thread_stack)
                        ( Bool (*p) ( Addr stack_min, Addr stack_max, void* d ),
                          void* d );

/* Get parts of the client's state. */
extern Addr VG_(get_SP) ( ThreadId tid );
extern Addr VG_(get_IP) ( ThreadId tid );


/*====================================================================*/
/*=== Valgrind's version of libc                                   ===*/
/*====================================================================*/

/* Valgrind doesn't use libc at all, for good reasons (trust us).  So here
   are its own versions of C library functions, but with VG_ prefixes.  Note
   that the types of some are slightly different to the real ones.  Some
   additional useful functions are provided too; descriptions of how they
   work are given below. */

#if !defined(NULL)
#  define NULL ((void*)0)
#endif


/* ------------------------------------------------------------------ */
/* stdio.h
 *
 * Note that they all output to the file descriptor given by the
 * --log-fd/--log-file/--log-socket argument, which defaults to 2 (stderr).
 * Hence no need for VG_(fprintf)().
 */
extern UInt VG_(printf)  ( const char *format, ... );
extern UInt VG_(vprintf) ( const char *format, va_list vargs );
/* too noisy ...  __attribute__ ((format (printf, 1, 2))) ; */
extern UInt VG_(sprintf) ( Char* buf, const Char* format, ... );
extern UInt VG_(vsprintf)( Char* buf, const Char* format, va_list vargs );

extern Int  VG_(rename) ( Char* old_name, Char* new_name );

/* ------------------------------------------------------------------ */
/* stdlib.h */

/* terminate everything */
extern void VG_(exit)( Int status )
            __attribute__ ((__noreturn__));

/* Prints a panic message (a constant string), appends newline and bug
   reporting info, aborts. */
__attribute__ ((__noreturn__))
extern void  VG_(tool_panic) ( Char* str );

/* Looks up VG_(client_envp) */
extern Char* VG_(getenv) ( Char* name );

/* Get client resource limit*/
extern Int VG_(getrlimit) ( Int resource, struct vki_rlimit *rlim );

/* Set client resource limit*/
extern Int VG_(setrlimit) ( Int resource, const struct vki_rlimit *rlim );

/* Crude stand-in for the glibc system() call. */
extern Int   VG_(system) ( Char* cmd );

extern Long  VG_(atoll)  ( Char* str );

/* Like atoll(), but converts a number of base 16 */
extern Long  VG_(atoll16) ( Char* str );

/* Like atoll(), but converts a number of base 2..36 */
extern Long  VG_(atoll36) ( UInt base, Char* str );

/* Like qsort(), but does shell-sort.  The size==1/2/4 cases are specialised. */
extern void VG_(ssort)( void* base, SizeT nmemb, SizeT size,
                        Int (*compar)(void*, void*) );


/* ------------------------------------------------------------------ */
/* ctype.h */
extern Bool VG_(isspace) ( Char c );
extern Bool VG_(isdigit) ( Char c );
extern Char VG_(toupper) ( Char c );


/* ------------------------------------------------------------------ */
/* string.h */
extern Int   VG_(strlen)         ( const Char* str );
extern Char* VG_(strcat)         ( Char* dest, const Char* src );
extern Char* VG_(strncat)        ( Char* dest, const Char* src, Int n );
extern Char* VG_(strpbrk)        ( const Char* s, const Char* accept );
extern Char* VG_(strcpy)         ( Char* dest, const Char* src );
extern Char* VG_(strncpy)        ( Char* dest, const Char* src, Int ndest );
extern Int   VG_(strcmp)         ( const Char* s1, const Char* s2 );
extern Int   VG_(strncmp)        ( const Char* s1, const Char* s2, Int nmax );
extern Char* VG_(strstr)         ( const Char* haystack, Char* needle );
extern Char* VG_(strchr)         ( const Char* s, Char c );
extern Char* VG_(strrchr)        ( const Char* s, Char c );
extern Char* VG_(strdup)         ( const Char* s);
extern void* VG_(memcpy)         ( void *d, const void *s, Int sz );
extern void* VG_(memset)         ( void *s, Int c, Int sz );
extern Int   VG_(memcmp)         ( const void* s1, const void* s2, Int n );

/* Like strcmp() and strncmp(), but stop comparing at any whitespace. */
extern Int   VG_(strcmp_ws)      ( const Char* s1, const Char* s2 );
extern Int   VG_(strncmp_ws)     ( const Char* s1, const Char* s2, Int nmax );

/* Like strncpy(), but if 'src' is longer than 'ndest' inserts a '\0' as the
   last character. */
extern void  VG_(strncpy_safely) ( Char* dest, const Char* src, Int ndest );

/* Mini-regexp function.  Searches for 'pat' in 'str'.  Supports
 * meta-symbols '*' and '?'.  '\' escapes meta-symbols. */
extern Bool  VG_(string_match)   ( const Char* pat, const Char* str );


/* ------------------------------------------------------------------ */
/* math.h */
/* Returns the base-2 logarithm of x. */
extern Int VG_(log2) ( Int x );


/* ------------------------------------------------------------------ */
/* unistd.h, fcntl.h, sys/stat.h */
extern Int  VG_(getdents)( UInt fd, struct vki_dirent *dirp, UInt count );
extern Int  VG_(readlink)( Char* path, Char* buf, UInt bufsize );
extern Int  VG_(getpid)  ( void );
extern Int  VG_(getppid) ( void );
extern Int  VG_(getpgrp) ( void );
extern Int  VG_(gettid)	 ( void );
extern Int  VG_(setpgid) ( Int pid, Int pgrp );

extern Int  VG_(open)   ( const Char* pathname, Int flags, Int mode );
extern Int  VG_(read)   ( Int fd, void* buf, Int count);
extern Int  VG_(write)  ( Int fd, const void* buf, Int count);
extern OffT VG_(lseek)  ( Int fd, OffT offset, Int whence);
extern void VG_(close)  ( Int fd );

extern Int  VG_(pipe)   ( Int fd[2] );

/* Nb: VG_(rename)() declared in stdio.h section above */
extern Int  VG_(unlink) ( Char* file_name );
extern Int  VG_(stat)   ( Char* file_name, struct vki_stat* buf );
extern Int  VG_(fstat)  ( Int   fd,        struct vki_stat* buf );
extern Int  VG_(dup2)   ( Int oldfd, Int newfd );

extern Char* VG_(getcwd) ( Char* buf, SizeT size );

/* Easier to use than VG_(getcwd)() -- does the buffer fiddling itself.
   String put into 'cwd' is VG_(malloc)'d, and should be VG_(free)'d.
   Returns False if it fails.  Will fail if the pathname is > 65535 bytes. */
extern Bool VG_(getcwd_alloc) ( Char** cwd );

/* ------------------------------------------------------------------ */
/* assert.h */
/* Asserts permanently enabled -- no turning off with NDEBUG.  Hurrah! */

/* This odd definition lets us stringify VG_(x) function names to
   "vgPlain_x".  We need to do two macroexpansions to get the VG_ macro
   expanded before stringifying. */
#define VG_STRINGIFY_WRK(x)   #x
#define VG_STRINGIFY(x)       VG_STRINGIFY_WRK(x)

#define tl_assert(expr)                                                 \
  ((void) ((expr) ? 0 :                                                 \
           (VG_(assert_fail) (/*isCore?*/False, VG_STRINGIFY(expr),     \
                              __FILE__, __LINE__, __PRETTY_FUNCTION__,  \
                              ""),                                      \
                              0)))

#define tl_assert2(expr, format, args...)                               \
  ((void) ((expr) ? 0 :                                                 \
           (VG_(assert_fail) (/*isCore?*/False, VG_STRINGIFY(expr),     \
                              __FILE__, __LINE__, __PRETTY_FUNCTION__,  \
                              format, ##args),                          \
                              0)))

__attribute__ ((__noreturn__))
extern void VG_(assert_fail) ( Bool isCore, const Char* expr, const Char* file, 
                               Int line, const Char* fn, 
                               const Char* format, ... );

/* ------------------------------------------------------------------ */
/* Get memory by anonymous mmap. */
extern void* VG_(get_memory_from_mmap) ( SizeT nBytes, Char* who );

extern Bool VG_(is_client_addr) (Addr a);

extern Bool VG_(is_shadow_addr) (Addr a);
extern Addr VG_(get_shadow_size)(void);

extern void *VG_(shadow_alloc)(UInt size);

extern Bool VG_(is_addressable)(Addr p, SizeT sz, UInt prot);

/* Register an interest in apparently internal faults; used code which
   wanders around dangerous memory (ie, leakcheck).  The catcher is
   not expected to return. */
extern void VG_(set_fault_catcher)(void (*catcher)(Int sig, Addr addr));

/* initialize shadow pages in the range [p, p+sz) This calls
   init_shadow_page for each one.  It should be a lot more efficient
   for bulk-initializing shadow pages than faulting on each one. 
*/
extern void VG_(init_shadow_range)(Addr p, UInt sz, Bool call_init);

/* Calls into the core used by leak-checking */

/* Calls "add_rootrange" with each range of memory which looks like a
   plausible source of root pointers. */
extern void VG_(find_root_memory)(void (*add_rootrange)(Addr addr, SizeT sz));

/* Calls "mark_addr" with register values (which may or may not be pointers) */
extern void VG_(mark_from_registers)(void (*mark_addr)(Addr addr));

/* ------------------------------------------------------------------ */
/* signal.h.

   Note that these use the vk_ (kernel) structure
   definitions, which are different in places from those that glibc
   defines.  Since we're operating right at the kernel interface, glibc's view
   of the world is entirely irrelevant. */

/* --- Signal set ops --- */
extern Int  VG_(sigfillset)  ( vki_sigset_t* set );
extern Int  VG_(sigemptyset) ( vki_sigset_t* set );

extern Bool VG_(isfullsigset)  ( const vki_sigset_t* set );
extern Bool VG_(isemptysigset) ( const vki_sigset_t* set );
extern Bool VG_(iseqsigset)    ( const vki_sigset_t* set1,
                                 const vki_sigset_t* set2 );

extern Int  VG_(sigaddset)   ( vki_sigset_t* set, Int signum );
extern Int  VG_(sigdelset)   ( vki_sigset_t* set, Int signum );
extern Int  VG_(sigismember) ( const vki_sigset_t* set, Int signum );

extern void VG_(sigaddset_from_set) ( vki_sigset_t* dst, vki_sigset_t* src );
extern void VG_(sigdelset_from_set) ( vki_sigset_t* dst, vki_sigset_t* src );

/* --- Mess with the kernel's sig state --- */
extern Int VG_(sigprocmask) ( Int how, const vki_sigset_t* set,
                              vki_sigset_t* oldset );
extern Int VG_(sigaction)   ( Int signum,
                              const struct vki_sigaction* act,
                              struct vki_sigaction* oldact );

extern Int VG_(sigtimedwait)( const vki_sigset_t *, vki_siginfo_t *, 
			      const struct vki_timespec * );

extern Int VG_(signal)      ( Int signum, void (*sighandler)(Int) );
extern Int VG_(sigaltstack) ( const vki_stack_t* ss, vki_stack_t* oss );

extern Int VG_(kill)        ( Int pid, Int signo );
extern Int VG_(tkill)       ( ThreadId tid, Int signo );
extern Int VG_(sigpending)  ( vki_sigset_t* set );

extern Int VG_(waitpid)	    ( Int pid, Int *status, Int options );

/* ------------------------------------------------------------------ */
/* socket.h. */

extern Int VG_(getsockname) ( Int sd, struct vki_sockaddr *name, Int *namelen);
extern Int VG_(getpeername) ( Int sd, struct vki_sockaddr *name, Int *namelen);
extern Int VG_(getsockopt) ( Int sd, Int level, Int optname, void *optval,
                             Int *optlen);

/* ------------------------------------------------------------------ */
/* other, randomly useful functions */
extern UInt VG_(read_millisecond_timer) ( void );

extern Bool VG_(has_cpuid) ( void );

extern void VG_(cpuid) ( UInt eax,
                         UInt *eax_ret, UInt *ebx_ret,
                         UInt *ecx_ret, UInt *edx_ret );

/*====================================================================*/
/*=== Obtaining debug information                                  ===*/
/*====================================================================*/

/* Get the file/function/line number of the instruction at address
   'a'.  For these four, if debug info for the address is found, it
   copies the info into the buffer/UInt and returns True.  If not, it
   returns False and nothing is copied.  VG_(get_fnname) always
   demangles C++ function names.  VG_(get_fnname_w_offset) is the
   same, except it appends "+N" to symbol names to indicate offsets.  */
extern Bool VG_(get_filename) ( Addr a, Char* filename, Int n_filename );
extern Bool VG_(get_fnname)   ( Addr a, Char* fnname,   Int n_fnname   );
extern Bool VG_(get_linenum)  ( Addr a, UInt* linenum );
extern Bool VG_(get_fnname_w_offset)
                              ( Addr a, Char* fnname,   Int n_fnname   );

/* This one is more efficient if getting both filename and line number,
   because the two lookups are done together. */
extern Bool VG_(get_filename_linenum)
                              ( Addr a, Char* filename, Int n_filename,
                                        UInt* linenum );

/* Succeeds only if we find from debug info that 'a' is the address of the
   first instruction in a function -- as opposed to VG_(get_fnname) which
   succeeds if we find from debug info that 'a' is the address of any
   instruction in a function.  Use this to instrument the start of
   a particular function.  Nb: if an executable/shared object is stripped
   of its symbols, this function will not be able to recognise function
   entry points within it. */
extern Bool VG_(get_fnname_if_entry) ( Addr a, Char* fnname, Int n_fnname );

/* Succeeds if the address is within a shared object or the main executable.
   It doesn't matter if debug info is present or not. */
extern Bool VG_(get_objname)  ( Addr a, Char* objname,  Int n_objname  );

/* Puts into 'buf' info about the code address %eip:  the address, function
   name (if known) and filename/line number (if known), like this:

      0x4001BF05: realloc (vg_replace_malloc.c:339)

   'n_buf' gives length of 'buf'.  Returns 'buf'.
*/
extern Char* VG_(describe_IP)(Addr eip, Char* buf, Int n_buf);

/* Returns a string containing an expression for the given
   address. String is malloced with VG_(malloc)() */
Char *VG_(describe_addr)(ThreadId, Addr);

/* A way to get information about what segments are mapped */
typedef struct _SegInfo SegInfo;

/* Returns NULL if the SegInfo isn't found.  It doesn't matter if debug info
   is present or not. */
extern SegInfo* VG_(get_obj)  ( Addr a );

extern const SegInfo* VG_(next_seginfo)  ( const SegInfo *seg );
extern       Addr     VG_(seg_start)     ( const SegInfo *seg );
extern       SizeT    VG_(seg_size)      ( const SegInfo *seg );
extern const UChar*   VG_(seg_filename)  ( const SegInfo *seg );
extern       ULong    VG_(seg_sym_offset)( const SegInfo *seg );

typedef
   enum {
      Vg_SectUnknown,
      Vg_SectText,
      Vg_SectData,
      Vg_SectBSS,
      Vg_SectGOT,
      Vg_SectPLT
   }
   VgSectKind;

extern VgSectKind VG_(seg_sect_kind)(Addr);

/*====================================================================*/
/*=== Generic hash table                                           ===*/
/*====================================================================*/

/* Generic type for a separately-chained hash table.  Via a kind of dodgy
   C-as-C++ style inheritance, tools can extend the VgHashNode type, so long
   as the first two fields match the sizes of these two fields.  Requires
   a bit of casting by the tool. */
typedef
   struct _VgHashNode {
      struct _VgHashNode * next;
      UWord              key;
   }
   VgHashNode;

typedef
   VgHashNode**
   VgHashTable;

/* Make a new table.  Allocates the memory with VG_(calloc)(), so can be freed
 * with VG_(free)(). */
extern VgHashTable VG_(HT_construct) ( void );

/* Count the number of nodes in a table. */
extern Int VG_(HT_count_nodes) ( VgHashTable table );

/* Add a node to the table. */
extern void VG_(HT_add_node) ( VgHashTable t, VgHashNode* node );

/* Looks up a node in the hash table.  Also returns the address of the
   previous node's `next' pointer which allows it to be removed from the
   list later without having to look it up again.  */
extern VgHashNode* VG_(HT_get_node) ( VgHashTable t, UWord key,
                                    /*OUT*/VgHashNode*** next_ptr );

/* Allocates an array of pointers to all the shadow chunks of malloc'd
   blocks.  Must be freed with VG_(free)(). */
extern VgHashNode** VG_(HT_to_array) ( VgHashTable t, /*OUT*/ UInt* n_shadows );

/* Returns first node that matches predicate `p', or NULL if none do.
   Extra arguments can be implicitly passed to `p' using `d' which is an
   opaque pointer passed to `p' each time it is called. */
extern VgHashNode* VG_(HT_first_match) ( VgHashTable t,
                                         Bool (*p)(VgHashNode*, void*),
                                         void* d );

/* Applies a function f() once to each node.  Again, `d' can be used
   to pass extra information to the function. */
extern void VG_(HT_apply_to_all_nodes)( VgHashTable t,
                                        void (*f)(VgHashNode*, void*),
                                        void* d );

/* Destroy a table. */
extern void VG_(HT_destruct) ( VgHashTable t );


/*====================================================================*/
/*=== A generic skiplist                                           ===*/
/*====================================================================*/

/* 
   The idea here is that the skiplist puts its per-element data at the
   end of the structure.  When you initialize the skiplist, you tell
   it what structure your list elements are going to be.  Then you
   should allocate them with VG_(SkipNode_Alloc), which will allocate
   enough memory for the extra bits.
 */

typedef struct _SkipList SkipList;
typedef struct _SkipNode SkipNode;

typedef Int (*SkipCmp_t)(const void *key1, const void *key2);

struct _SkipList {
   const Short		arena;		/* allocation arena                        */
   const UShort		size;		/* structure size (not including SkipNode) */
   const UShort		keyoff;		/* key offset                              */
   const SkipCmp_t	cmp;		/* compare two keys                        */
	 Char *		(*strkey)(void *); /* stringify a key (for debugging)      */
         SkipNode	*head;		/* list head                               */
};

/* Use this macro to initialize your skiplist head.  The arguments are pretty self explanitory:
   _type is the type of your element structure
   _key is the field within that type which you want to use as the key
   _cmp is the comparison function for keys - it gets two typeof(_key) pointers as args
   _strkey is a function which can return a string of your key - it's only used for debugging
   _arena is the arena to use for allocation - -1 is the default
 */
#define VG_SKIPLIST_INIT(_type, _key, _cmp, _strkey, _arena)		\
	{								\
	   .arena       = _arena,					\
	   .size	= sizeof(_type),				\
	   .keyoff	= offsetof(_type, _key),			\
	   .cmp		= _cmp,						\
	   .strkey      = _strkey,					\
	   .head	= NULL,						\
	}

/* List operations:
   SkipList_Find_* search a list.  The 3 variants are:
      Before: returns a node which is <= key, or NULL if none
      Exact:  returns a node which is == key, or NULL if none
      After:  returns a node which is >= key, or NULL if none
   SkipList_Insert inserts a new element into the list.  Duplicates are
      forbidden.  The element must have been created with SkipList_Alloc!
   SkipList_Remove removes an element from the list and returns it.  It
      doesn't free the memory.
*/
extern void *VG_(SkipList_Find_Before)  (const SkipList *l, void *key);
extern void *VG_(SkipList_Find_Exact)   (const SkipList *l, void *key);
extern void *VG_(SkipList_Find_After)   (const SkipList *l, void *key);
extern void  VG_(SkipList_Insert)       (      SkipList *l, void *data);
extern void *VG_(SkipList_Remove)       (      SkipList *l, void *key);

/* Some useful standard comparisons */
extern Int  VG_(cmp_Addr)  (const void *a, const void *b);
extern Int  VG_(cmp_Int)   (const void *a, const void *b);
extern Int  VG_(cmp_UInt)  (const void *a, const void *b);
extern Int  VG_(cmp_string)(const void *a, const void *b);

/* Node (element) operations:
   SkipNode_Alloc: allocate memory for a new element on the list.  Must be
      used before an element can be inserted!  Returns NULL if not enough
      memory.
   SkipNode_Free: free memory allocated above
   SkipNode_First: return the first element on the list
   SkipNode_Next: return the next element after "data" on the list - 
      NULL for none

   You can iterate through a SkipList like this:

      for(x = VG_(SkipNode_First)(&list);	// or SkipList_Find
	  x != NULL;
	  x = VG_(SkipNode_Next)(&list, x)) { ... }
*/
extern void *VG_(SkipNode_Alloc) (const SkipList *l);
extern void  VG_(SkipNode_Free)  (const SkipList *l, void *p);
extern void *VG_(SkipNode_First) (const SkipList *l);
extern void *VG_(SkipNode_Next)  (const SkipList *l, void *data);


/*====================================================================*/
/*=== Functions for shadow registers                               ===*/
/*====================================================================*/

// For get/set, 'area' is where the asked-for shadow state will be copied
// into/from.
extern void VG_(get_shadow_regs_area) ( ThreadId tid, OffT guest_state_offset,
                                        SizeT size, UChar* area );
extern void VG_(set_shadow_regs_area) ( ThreadId tid, OffT guest_state_offset,
                                        SizeT size, const UChar* area );

#endif   /* __TOOL_H */


