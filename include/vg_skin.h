
/*--------------------------------------------------------------------*/
/*--- The only header your skin will ever need to #include...      ---*/
/*---                                                    vg_skin.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2003 Julian Seward 
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

#ifndef __VG_SKIN_H
#define __VG_SKIN_H

#include <stdarg.h>       /* ANSI varargs stuff  */
#include <setjmp.h>       /* for jmp_buf         */

#include "vg_constants_skin.h"


/*====================================================================*/
/*=== Build options and table sizes.                               ===*/
/*====================================================================*/

/* You should be able to change these options or sizes, recompile, and 
   still have a working system. */

/* The maximum number of pthreads that we support.  This is
   deliberately not very high since our implementation of some of the
   scheduler algorithms is surely O(N) in the number of threads, since
   that's simple, at least.  And (in practice) we hope that most
   programs do not need many threads. */
#define VG_N_THREADS 100

/* Maximum number of pthread keys available.  Again, we start low until
   the need for a higher number presents itself. */
#define VG_N_THREAD_KEYS 50

/* Total number of integer registers available for allocation -- all of
   them except %esp, %ebp.  %ebp permanently points at VG_(baseBlock).
   
   If you increase this you'll have to also change at least these:
     - VG_(rank_to_realreg)()
     - VG_(realreg_to_rank)()
     - ppRegsLiveness()
     - the RegsLive type (maybe -- RegsLive type must have more than
                          VG_MAX_REALREGS bits)

   You can decrease it, and performance will drop because more spills will
   occur.  If you decrease it too much, everything will fall over.
   
   Do not change this unless you really know what you are doing!  */
#define VG_MAX_REALREGS 6


/*====================================================================*/
/*=== Basic types, useful macros                                   ===*/
/*====================================================================*/

typedef unsigned char          UChar;
typedef unsigned short         UShort;
typedef unsigned int           UInt;
typedef unsigned long long int ULong;

typedef signed char            Char;
typedef signed short           Short;
typedef signed int             Int;
typedef signed long long int   Long;

typedef unsigned int           Addr;

typedef unsigned char          Bool;
#define False                  ((Bool)0)
#define True                   ((Bool)1)


#define mycat_wrk(aaa,bbb) aaa##bbb
#define mycat(aaa,bbb) mycat_wrk(aaa,bbb)

/* No, really.  I _am_ that strange. */
#define OINK(nnn) VG_(message)(Vg_DebugMsg, "OINK %d",nnn)

/* ---------------------------------------------------------------------
   Now the basic types are set up, we can haul in the kernel-interface
   definitions.
   ------------------------------------------------------------------ */

#include "vg_kerneliface.h"


/*====================================================================*/
/*=== Core/skin interface version                                  ===*/
/*====================================================================*/

/* The major version number indicates binary-incompatible changes to the
   interface;  if the core and skin major versions don't match, Valgrind
   will abort.  The minor version indicates binary-compatible changes.
*/
#define VG_CORE_INTERFACE_MAJOR_VERSION   3
#define VG_CORE_INTERFACE_MINOR_VERSION   0

extern const Int VG_(skin_interface_major_version);
extern const Int VG_(skin_interface_minor_version);

/* Every skin must include this macro somewhere, exactly once. */
#define VG_DETERMINE_INTERFACE_VERSION \
const Int VG_(skin_interface_major_version) = VG_CORE_INTERFACE_MAJOR_VERSION; \
const Int VG_(skin_interface_minor_version) = VG_CORE_INTERFACE_MINOR_VERSION;


/*====================================================================*/
/*=== Command-line options                                         ===*/
/*====================================================================*/

/* Use this for normal null-termination-style string comparison */
#define VG_STREQ(s1,s2) (s1 != NULL && s2 != NULL \
                         && VG_(strcmp)((s1),(s2))==0)

/* Use these for recognising skin command line options -- stops comparing
   once whitespace is reached. */
#  define VG_CLO_STREQ(s1,s2)     (0==VG_(strcmp_ws)((s1),(s2)))
#  define VG_CLO_STREQN(nn,s1,s2) (0==VG_(strncmp_ws)((s1),(s2),(nn)))

/* Verbosity level: 0 = silent, 1 (default), > 1 = more verbose. */
extern Int   VG_(clo_verbosity);

/* Profile? */
extern Bool  VG_(clo_profile);

/* Call this if a recognised option was bad for some reason.
   Note: don't use it just because an option was unrecognised -- return 'False'
   from SKN_(process_cmd_line_option) to indicate that. */
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
          Vg_DebugExtraMsg    /* '?' == '+' */
   }
   VgMsgKind;

/* Functions for building a message from multiple parts. */
extern void VG_(start_msg)  ( VgMsgKind kind );
extern void VG_(add_to_msg) ( Char* format, ... );
/* Ends and prints the message.  Appends a newline. */
extern void VG_(end_msg)    ( void );

/* Send a single-part message.  Appends a newline. */
extern void VG_(message)    ( VgMsgKind kind, Char* format, ... );


/*====================================================================*/
/*=== Profiling                                                    ===*/
/*====================================================================*/

/* Nb: VGP_(register_profile_event)() relies on VgpUnc being the first one */
#define VGP_CORE_LIST \
   /* These ones depend on the core */                \
   VGP_PAIR(VgpUnc,         "unclassified"),          \
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
   VGP_PAIR(VgpInitMem,     "init-memory"),           \
   VGP_PAIR(VgpExeContext,  "exe-context"),           \
   VGP_PAIR(VgpReadSyms,    "read-syms"),             \
   VGP_PAIR(VgpSearchSyms,  "search-syms"),           \
   VGP_PAIR(VgpAddToT,      "add-to-transtab"),       \
   VGP_PAIR(VgpCoreSysWrap, "core-syscall-wrapper"),  \
   VGP_PAIR(VgpDemangle,    "demangle"),              \
   VGP_PAIR(VgpCoreCheapSanity,     "core-cheap-sanity"),     \
   VGP_PAIR(VgpCoreExpensiveSanity, "core-expensive-sanity"), \
   /* These ones depend on the skin */                \
   VGP_PAIR(VgpPreCloInit,  "pre-clo-init"),          \
   VGP_PAIR(VgpPostCloInit, "post-clo-init"),         \
   VGP_PAIR(VgpInstrument,  "instrument"),            \
   VGP_PAIR(VgpSkinSysWrap, "skin-syscall-wrapper"),  \
   VGP_PAIR(VgpSkinCheapSanity,     "skin-cheap-sanity"),     \
   VGP_PAIR(VgpSkinExpensiveSanity, "skin-expensive-sanity"), \
   VGP_PAIR(VgpFini,        "fini")

#define VGP_PAIR(n,name) n
typedef enum { VGP_CORE_LIST } VgpCoreCC;
#undef  VGP_PAIR

/* When registering skin profiling events, ensure that the 'n' value is in
 * the range (VgpFini+1..) */
extern void VGP_(register_profile_event) ( Int n, Char* name );

extern void VGP_(pushcc) ( UInt cc );
extern void VGP_(popcc)  ( UInt cc );

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

/* 64-bit counter for the number of basic blocks done. */
extern ULong VG_(bbs_done);

/* Get the simulated %esp */
extern Addr VG_(get_stack_pointer) ( void );

/* Detect if an address is within Valgrind's stack, Valgrind's
   m_state_static, or the VG_(threads) array.  This is useful for
   memory leak detectors to rule out spurious pointers to a block. */
extern Bool VG_(within_stack)(Addr a);
extern Bool VG_(within_m_state_static_OR_threads)(Addr a);

/* Check if an address is 4-byte aligned */
#define IS_ALIGNED4_ADDR(aaa_p) (0 == (((UInt)(aaa_p)) & 3))
#define IS_ALIGNED8_ADDR(aaa_p) (0 == (((UInt)(aaa_p)) & 7))


/* ------------------------------------------------------------------ */
/* Thread-related stuff */

/* Special magic value for an invalid ThreadId.  It corresponds to
   LinuxThreads using zero as the initial value for
   pthread_mutex_t.__m_owner and pthread_cond_t.__c_waiting. */
#define VG_INVALID_THREADID ((ThreadId)(0))

/* ThreadIds are simply indices into the VG_(threads)[] array. */
typedef 
   UInt 
   ThreadId;

/* When looking for the current ThreadId, this is the safe option and
   probably the one you want.
  
   Details: Use this one from non-generated code, eg. from functions called
   on events like 'new_mem_heap'.  In such a case, the "current" thread is
   temporarily suspended as Valgrind's dispatcher is running.  This function
   is also suitable to be called from generated code (ie. from UCode, or a C
   function called directly from UCode).
   
   If you use VG_(get_current_tid)() from non-generated code, it will return
   0 signifying the invalid thread, which is probably not what you want. */
extern ThreadId VG_(get_current_or_recent_tid) ( void );

/* When looking for the current ThreadId, only use this one if you know what
   you are doing.
  
   Details: Use this one from generated code, eg. from C functions called
   from UCode.  (VG_(get_current_or_recent_tid)() is also suitable in that
   case.)  If you use this function from non-generated code, it will return
   0 signifying the invalid thread, which is probably not what you want. */
extern ThreadId VG_(get_current_tid)           ( void );

/* Searches through all thread's stacks to see if any match.  Returns
   VG_INVALID_THREADID if none match. */
extern ThreadId VG_(first_matching_thread_stack)
                        ( Bool (*p) ( Addr stack_min, Addr stack_max ));


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
 * --logfile-fd=N argument, which defaults to 2 (stderr).  Hence no
 * need for VG_(fprintf)().  
 */
extern UInt VG_(printf)  ( const char *format, ... );
/* too noisy ...  __attribute__ ((format (printf, 1, 2))) ; */
extern UInt VG_(sprintf) ( Char* buf, Char *format, ... );
extern UInt VG_(vprintf) ( void(*send)(Char), 
                           const Char *format, va_list vargs );

extern Int  VG_(rename) ( Char* old_name, Char* new_name );

/* ------------------------------------------------------------------ */
/* stdlib.h */

extern void* VG_(malloc)         ( Int nbytes );
extern void  VG_(free)           ( void* p );
extern void* VG_(calloc)         ( Int n, Int nbytes );
extern void* VG_(realloc)        ( void* p, Int size );
extern void* VG_(malloc_aligned) ( Int align_bytes, Int nbytes );

extern void  VG_(print_malloc_stats) ( void );


extern void  VG_(exit)( Int status )
             __attribute__ ((__noreturn__));
/* Prints a panic message (a constant string), appends newline and bug
   reporting info, aborts. */
__attribute__ ((__noreturn__))
extern void  VG_(skin_panic) ( Char* str );

/* Looks up VG_(client_envp) */
extern Char* VG_(getenv) ( Char* name );

/* Crude stand-in for the glibc system() call. */
extern Int   VG_(system) ( Char* cmd );

extern Long  VG_(atoll)  ( Char* str );

/* Like atoll(), but converts a number of base 2..36 */
extern Long  VG_(atoll36) ( UInt base, Char* str );


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
extern Bool  VG_(string_match)   ( Char* pat, Char* str );


/* ------------------------------------------------------------------ */
/* math.h */
/* Returns the base-2 logarithm of x. */
extern Int VG_(log2) ( Int x );


/* ------------------------------------------------------------------ */
/* unistd.h, fcntl.h, sys/stat.h */
extern Int  VG_(getpid)  ( void );
extern Int  VG_(getppid) ( void );

extern Int  VG_(open)   ( const Char* pathname, Int flags, Int mode );
extern Int  VG_(read)   ( Int fd, void* buf, Int count);
extern Int  VG_(write)  ( Int fd, void* buf, Int count);
extern void VG_(close)  ( Int fd );

/* Nb: VG_(rename)() declared in stdio.h section above */
extern Int  VG_(unlink) ( Char* file_name );
extern Int  VG_(stat)   ( Char* file_name, struct vki_stat* buf );

extern Char* VG_(getcwd) ( Char* buf, Int size );


/* ------------------------------------------------------------------ */
/* assert.h */
/* Asserts permanently enabled -- no turning off with NDEBUG.  Hurrah! */
#define VG__STRING(__str)  #__str

#define sk_assert(expr)                                               \
  ((void) ((expr) ? 0 :						      \
	   (VG_(skin_assert_fail) (VG__STRING(expr),	              \
			           __FILE__, __LINE__,                \
                                   __PRETTY_FUNCTION__), 0)))

__attribute__ ((__noreturn__))
extern void VG_(skin_assert_fail) ( const Char* expr, const Char* file, 
                                    Int line, const Char* fn );


/* ------------------------------------------------------------------ */
/* system/mman.h */
extern void* VG_(mmap)( void* start, UInt length, 
                        UInt prot, UInt flags, UInt fd, UInt offset );
extern Int  VG_(munmap)( void* start, Int length );

/* Get memory by anonymous mmap. */
extern void* VG_(get_memory_from_mmap) ( Int nBytes, Char* who );


/* ------------------------------------------------------------------ */
/* signal.h.  
  
   Note that these use the vk_ (kernel) structure
   definitions, which are different in places from those that glibc
   defines -- hence the 'k' prefix.  Since we're operating right at the
   kernel interface, glibc's view of the world is entirely irrelevant. */

/* --- Signal set ops --- */
extern Int  VG_(ksigfillset)  ( vki_ksigset_t* set );
extern Int  VG_(ksigemptyset) ( vki_ksigset_t* set );

extern Bool VG_(kisfullsigset)  ( vki_ksigset_t* set );
extern Bool VG_(kisemptysigset) ( vki_ksigset_t* set );

extern Int  VG_(ksigaddset)   ( vki_ksigset_t* set, Int signum );
extern Int  VG_(ksigdelset)   ( vki_ksigset_t* set, Int signum );
extern Int  VG_(ksigismember) ( vki_ksigset_t* set, Int signum );

extern void VG_(ksigaddset_from_set) ( vki_ksigset_t* dst, vki_ksigset_t* src );
extern void VG_(ksigdelset_from_set) ( vki_ksigset_t* dst, vki_ksigset_t* src );

/* --- Mess with the kernel's sig state --- */
extern Int VG_(ksigprocmask) ( Int how, const vki_ksigset_t* set, 
                                       vki_ksigset_t* oldset );
extern Int VG_(ksigaction)   ( Int signum,  
                               const vki_ksigaction* act,  
                               vki_ksigaction* oldact );

extern Int VG_(ksignal)      ( Int signum, void (*sighandler)(Int) );
extern Int VG_(ksigaltstack) ( const vki_kstack_t* ss, vki_kstack_t* oss );

extern Int VG_(kkill)        ( Int pid, Int signo );
extern Int VG_(ksigpending)  ( vki_ksigset_t* set );


/*====================================================================*/
/*=== UCode definition                                             ===*/
/*====================================================================*/

/* Tags which describe what operands are.  Must fit into 4 bits, which
   they clearly do. */
typedef
enum { TempReg  =0, /* virtual temp-reg */
       ArchReg  =1, /* simulated integer reg */
       ArchRegS =2, /* simulated segment reg */
       RealReg  =3, /* real machine's real reg */
       SpillNo  =4, /* spill slot location */
       Literal  =5, /* literal; .lit32 field has actual value */
       Lit16    =6, /* literal; .val[123] field has actual value */
       NoValue  =7  /* operand not in use */
     }
     Tag;

/* Invalid register numbers (can't be negative) */
#define INVALID_TEMPREG 999999999
#define INVALID_REALREG 999999999

/* Microinstruction opcodes. */
typedef
   enum {
      NOP,         /* Null op */

      LOCK,	   /* Indicate the existence of a LOCK prefix (functionally NOP) */

      /* Moving values around */
      GET,  PUT,   /* simulated register <--> TempReg */
      GETF, PUTF,  /* simulated %eflags  <--> TempReg */
      LOAD, STORE, /* memory  <--> TempReg            */
      MOV,         /* TempReg <--> TempReg            */
      CMOV,        /* Used for cmpxchg and cmov       */

      /* Arithmetic/logical ops */
      ADD, ADC, SUB, SBB,                 /* Add/subtract (w/wo carry)     */
      AND, OR,  XOR, NOT,                 /* Boolean ops                   */
      SHL, SHR, SAR, ROL, ROR, RCL, RCR,  /* Shift/rotate (w/wo carry)     */
      NEG,                                /* Negate                        */
      INC, DEC,                           /* Increment/decrement           */
      BSWAP,                              /* Big-endian <--> little-endian */
      CC2VAL,                             /* Condition code --> 0 or 1     */
      WIDEN,                              /* Signed or unsigned widening   */

      /* Conditional or unconditional jump  */
      JMP,         

      /* FPU ops */
      FPU,           /* Doesn't touch memory */
      FPU_R, FPU_W,  /* Reads/writes memory  */

      /* ------------ MMX ops ------------ */
      /* In this and the SSE encoding, bytes at higher addresses are
	 held in bits [7:0] in these 16-bit words.  I guess this means
	 it is a big-endian encoding. */

      /* 1 byte, no memrefs, no iregdefs, copy exactly to the
	 output.  Held in val1[7:0]. */
      MMX1,

      /* 2 bytes, no memrefs, no iregdefs, copy exactly to the
	 output.  Held in val1[15:0]. */
      MMX2,

      /* 3 bytes, no memrefs, no iregdefs, copy exactly to the
         output.  Held in val1[15:0] and val2[7:0]. */
      MMX3,

      /* 2 bytes, reads/writes mem.  Insns of the form
         bbbbbbbb:mod mmxreg r/m.
         Held in val1[15:0], and mod and rm are to be replaced
         at codegen time by a reference to the Temp/RealReg holding 
         the address.  Arg2 holds this Temp/Real Reg.
         Transfer is always at size 8.
      */
      MMX2_MemRd,
      MMX2_MemWr,

      /* 2 bytes, reads/writes an integer ("E") register.  Insns of the form
         bbbbbbbb:11 mmxreg ireg.
         Held in val1[15:0], and ireg is to be replaced
         at codegen time by a reference to the relevant RealReg.
         Transfer is always at size 4.  Arg2 holds this Temp/Real Reg.
      */
      MMX2_ERegRd,
      MMX2_ERegWr,

      /* ------------ SSE/SSE2 ops ------------ */
      /* In the following:

         a digit N indicates the next N bytes are to be copied exactly
         to the output.

         'a' indicates a mod-xmmreg-rm byte, where the mod-rm part is
         to be replaced at codegen time to a Temp/RealReg holding the
         address.

         'e' indicates a byte of the form '11 xmmreg ireg', where ireg
         is read or written, and is to be replaced at codegen time by
         a reference to the relevant RealReg.  'e' because it's the E
         reg in Intel encoding parlance.

         'g' indicates a byte of the form '11 ireg xmmreg', where ireg
         is read or written, and is to be replaced at codegen time by
         a reference to the relevant RealReg.  'g' because it's called
         G in Intel parlance. */

      /* 3 bytes, no memrefs, no iregdefs, copy exactly to the
         output.  Held in val1[15:0] and val2[7:0]. */
      SSE3,

      /* 3 bytes, reads/writes mem.  Insns of the form
         bbbbbbbb:bbbbbbbb:mod mmxreg r/m.
         Held in val1[15:0] and val2[7:0], and mod and rm are to be
         replaced at codegen time by a reference to the Temp/RealReg
         holding the address.  Arg3 holds this Temp/Real Reg.
         Transfer is usually, but not always, at size 16.  */
      SSE2a_MemRd,
      SSE2a_MemWr,

      /* 4 bytes, no memrefs, no iregdefs, copy exactly to the
         output.  Held in val1[15:0] and val2[15:0]. */
      SSE4,

      /* 4 bytes, reads/writes mem.  Insns of the form
         bbbbbbbb:bbbbbbbb:bbbbbbbb:mod mmxreg r/m.
         Held in val1[15:0] and val2[15:0], and mod and rm are to be
         replaced at codegen time by a reference to the Temp/RealReg
         holding the address.  Arg3 holds this Temp/Real Reg.
         Transfer is at stated size.  */
      SSE3a_MemRd,
      SSE3a_MemWr,

      /* 4 bytes, reads/writes mem.  Insns of the form
         bbbbbbbb:bbbbbbbb:mod mmxreg r/m:bbbbbbbb
         Held in val1[15:0] and val2[15:0], and mod and rm are to be
         replaced at codegen time by a reference to the Temp/RealReg
         holding the address.  Arg3 holds this Temp/Real Reg.
         Transfer is at stated size.  */
      SSE2a1_MemRd,
#if 0
      SSE2a1_MemWr,
#endif
      /* 4 bytes, writes an integer register.  Insns of the form
         bbbbbbbb:bbbbbbbb:bbbbbbbb:11 ireg bbb.
         Held in val1[15:0] and val2[15:0], and ireg is to be replaced
         at codegen time by a reference to the relevant RealReg.
         Transfer is always at size 4.  Arg3 holds this Temp/Real Reg.
      */
      SSE3g_RegWr,

      /* 5 bytes, writes an integer register.  Insns of the form
         bbbbbbbb:bbbbbbbb:bbbbbbbb: 11 ireg bbb :bbbbbbbb. Held in
         val1[15:0] and val2[15:0] and lit32[7:0], and ireg is to be
         replaced at codegen time by a reference to the relevant
         RealReg.  Transfer is always at size 4.  Arg3 holds this
         Temp/Real Reg.
      */
      SSE3g1_RegWr,

      /* 4 bytes, reads an integer register.  Insns of the form
         bbbbbbbb:bbbbbbbb:bbbbbbbb:11 bbb ireg.
         Held in val1[15:0] and val2[15:0], and ireg is to be replaced
         at codegen time by a reference to the relevant RealReg.
         Transfer is always at size 4.  Arg3 holds this Temp/Real Reg.
      */
      SSE3e_RegRd,
      SSE3e_RegWr, /* variant that writes Ereg, not reads it */

      /* 5 bytes, reads an integer register.  Insns of the form
         bbbbbbbb:bbbbbbbb:bbbbbbbb: 11 bbb ireg :bbbbbbbb. Held in
         val1[15:0] and val2[15:0] and lit32[7:0], and ireg is to be
         replaced at codegen time by a reference to the relevant
         RealReg.  Transfer is always at size 4.  Arg3 holds this
         Temp/Real Reg.
      */
      SSE3e1_RegRd,

      /* 4 bytes, reads memory, writes an integer register, but is
         nevertheless an SSE insn.  The insn is of the form
         bbbbbbbb:bbbbbbbb:bbbbbbbb:mod ireg rm where mod indicates
         memory (ie is not 11b) and ireg is the int reg written.  The
         first 4 bytes are held in lit32[31:0] since there is
         insufficient space elsewhere.  mod and rm are to be replaced
         at codegen time by a reference to the Temp/RealReg holding
         the address.  Arg1 holds this Temp/RealReg.  ireg is to be
         replaced at codegen time by a reference to the relevant
         RealReg in which the answer is to be written.  Arg2 holds
         this Temp/RealReg.  Transfer to the destination reg is always
         at size 4.  However the memory read can be at sizes 4 or 8
         and so this is what the sz field holds.  Note that the 4th
         byte of the instruction (the modrm byte) is redundant, but we
         store it anyway so as to be consistent with all other SSE
         uinstrs.
      */
      SSE3ag_MemRd_RegWr,

      /* 5 bytes, no memrefs, no iregdefs, copy exactly to the
         output.  Held in val1[15:0], val2[15:0] and val3[7:0]. */
      SSE5,
#if 0
      /* 5 bytes, reads/writes mem.  Insns of the form
         bbbbbbbb:bbbbbbbb:bbbbbbbb:mod mmxreg r/m:bbbbbbbb
         Held in val1[15:0], val2[15:0], lit32[7:0].  
         mod and rm are to be replaced at codegen time by a reference 
         to the Temp/RealReg holding the address.  Arg3 holds this 
         Temp/Real Reg.  Transfer is always at size 16.  */
      SSE3a1_MemRd,
      SSE3a1_MemWr,
#endif
      /* ------------------------ */

      /* Not strictly needed, but improve address calculation translations. */
      LEA1,  /* reg2 := const + reg1 */
      LEA2,  /* reg3 := const + reg1 + reg2 * 1,2,4 or 8 */

      /* Hack for x86 REP insns.  Jump to literal if TempReg/RealReg
         is zero. */
      JIFZ,

      /* Advance the simulated %eip by some small (< 128) number. */
      INCEIP,

      /* Dealing with segment registers */
      GETSEG, PUTSEG,   /* simulated segment register <--> TempReg */
      USESEG,           /* (LDT/GDT index, virtual addr) --> linear addr */

      /* Not for translating x86 calls -- only to call helpers */
      CALLM_S, CALLM_E, /* Mark start/end of CALLM push/pop sequence */
      PUSH, POP, CLEAR, /* Add/remove/zap args for helpers           */
      CALLM,            /* Call assembly-code helper                 */

      /* Not for translating x86 calls -- only to call C helper functions of
         up to three arguments (or two if the functions has a return value).
         Arguments and return value must be word-sized.  More arguments can
         be faked with global variables (eg. use VG_(set_global_var)()).

         Seven possibilities: 'arg[123]' show where args go, 'ret' shows
         where return value goes (if present).
        
         CCALL(-,    -,    -   )    void f(void)
         CCALL(arg1, -,    -   )    void f(UInt arg1)
         CCALL(arg1, arg2, -   )    void f(UInt arg1, UInt arg2)
         CCALL(arg1, arg2, arg3)    void f(UInt arg1, UInt arg2, UInt arg3)
         CCALL(-,    -,    ret )    UInt f(UInt)
         CCALL(arg1, -,    ret )    UInt f(UInt arg1)
         CCALL(arg1, arg2, ret )    UInt f(UInt arg1, UInt arg2) */
      CCALL,

      /* This opcode makes it easy for skins that extend UCode to do this to
         avoid opcode overlap:

           enum { EU_OP1 = DUMMY_FINAL_UOPCODE + 1, ... } 
   
         WARNING: Do not add new opcodes after this one!  They can be added
         before, though. */
      DUMMY_FINAL_UOPCODE
   }
   Opcode;


/* Condition codes, using the Intel encoding.  CondAlways is an extra. */
typedef
   enum {
      CondO      = 0,  /* overflow           */
      CondNO     = 1,  /* no overflow        */
      CondB      = 2,  /* below              */
      CondNB     = 3,  /* not below          */
      CondZ      = 4,  /* zero               */
      CondNZ     = 5,  /* not zero           */
      CondBE     = 6,  /* below or equal     */
      CondNBE    = 7,  /* not below or equal */
      CondS      = 8,  /* negative           */
      CondNS     = 9,  /* not negative       */
      CondP      = 10, /* parity even        */
      CondNP     = 11, /* not parity even    */
      CondL      = 12, /* jump less          */
      CondNL     = 13, /* not less           */
      CondLE     = 14, /* less or equal      */
      CondNLE    = 15, /* not less or equal  */
      CondAlways = 16  /* Jump always        */
   } 
   Condcode;


/* Descriptions of additional properties of *unconditional* jumps. */
typedef
   enum {
     JmpBoring=0,   /* boring unconditional jump */
     JmpCall=1,     /* jump due to an x86 call insn */
     JmpRet=2,      /* jump due to an x86 ret insn */
     JmpSyscall=3,  /* do a system call, then jump */
     JmpClientReq=4 /* do a client request, then jump */
   }
   JmpKind;


/* Flags.  User-level code can only read/write O(verflow), S(ign),
   Z(ero), A(ux-carry), C(arry), P(arity), and may also write
   D(irection).  That's a total of 7 flags.  A FlagSet is a bitset,
   thusly: 
      76543210
       DOSZACP
   and bit 7 must always be zero since it is unused.

   Note: these Flag? values are **not** the positions in the actual
   %eflags register.  */

typedef UChar FlagSet;

#define FlagD (1<<6)
#define FlagO (1<<5)
#define FlagS (1<<4)
#define FlagZ (1<<3)
#define FlagA (1<<2)
#define FlagC (1<<1)
#define FlagP (1<<0)

#define FlagsOSZACP (FlagO | FlagS | FlagZ | FlagA | FlagC | FlagP)
#define FlagsOSZAP  (FlagO | FlagS | FlagZ | FlagA |         FlagP)
#define FlagsOSZCP  (FlagO | FlagS | FlagZ |         FlagC | FlagP)
#define FlagsOSACP  (FlagO | FlagS |         FlagA | FlagC | FlagP)
#define FlagsSZACP  (        FlagS | FlagZ | FlagA | FlagC | FlagP)
#define FlagsSZAP   (        FlagS | FlagZ | FlagA |         FlagP)
#define FlagsZCP    (                FlagZ         | FlagC | FlagP)
#define FlagsOC     (FlagO |                         FlagC        )
#define FlagsAC     (                        FlagA | FlagC        )

#define FlagsALL    (FlagsOSZACP | FlagD)
#define FlagsEmpty  (FlagSet)0


/* flag positions in eflags */
#define EFlagC  (1 <<  0)       /* carry */
#define EFlagP  (1 <<  2)       /* parity */
#define EFlagA	(1 <<  4)	/* aux carry */
#define EFlagZ  (1 <<  6)       /* zero */
#define EFlagS  (1 <<  7)       /* sign */
#define EFlagD  (1 << 10)	/* direction */
#define EFlagO  (1 << 11)       /* overflow */

/* Liveness of general purpose registers, useful for code generation.
   Reg rank order 0..N-1 corresponds to bits 0..N-1, ie. first
   reg's liveness in bit 0, last reg's in bit N-1.  Note that
   these rankings don't match the Intel register ordering. */
typedef UInt RRegSet;

#define ALL_RREGS_DEAD      0                           /* 0000...00b */
#define ALL_RREGS_LIVE      ((1 << VG_MAX_REALREGS)-1)  /* 0011...11b */
#define UNIT_RREGSET(rank)  (1 << (rank))

#define IS_RREG_LIVE(rank,rregs_live) (rregs_live & UNIT_RREGSET(rank))
#define SET_RREG_LIVENESS(rank,rregs_live,b)       \
   do { RRegSet unit = UNIT_RREGSET(rank);         \
        if (b) rregs_live |= unit;                 \
        else   rregs_live &= ~unit;                \
   } while(0)


/* A Micro (u)-instruction. */
typedef
   struct {
      /* word 1 */
      UInt    lit32;      /* 32-bit literal */

      /* word 2 */
      UShort  val1;       /* first operand */
      UShort  val2;       /* second operand */

      /* word 3 */
      UShort  val3;       /* third operand */
      UChar   opcode;     /* opcode */
      UChar   size;       /* data transfer size */

      /* word 4 */
      FlagSet flags_r;    /* :: FlagSet */
      FlagSet flags_w;    /* :: FlagSet */
      UChar   tag1:4;     /* first  operand tag */
      UChar   tag2:4;     /* second operand tag */
      UChar   tag3:4;     /* third  operand tag */
      UChar   extra4b:4;  /* Spare field, used by WIDEN for src
                             -size, and by LEA2 for scale (1,2,4 or 8),
                             and by JMPs for original x86 instr size */

      /* word 5 */
      UChar   cond;            /* condition, for jumps */
      Bool    signed_widen:1;  /* signed or unsigned WIDEN ? */
      JmpKind jmpkind:3;       /* additional properties of unconditional JMP */

      /* Additional properties for UInstrs that call C functions:  
           - CCALL
           - PUT (when %ESP is the target)
           - possibly skin-specific UInstrs
      */
      UChar   argc:2;          /* Number of args, max 3 */
      UChar   regparms_n:2;    /* Number of args passed in registers */
      Bool    has_ret_val:1;   /* Function has return value? */

      /* RealReg liveness;  only sensical after reg alloc and liveness
         analysis done.  This info is a little bit arch-specific --
         VG_MAX_REALREGS can vary on different architectures.  Note that
         to use this information requires converting between register ranks
         and the Intel register numbers, using VG_(realreg_to_rank)()
         and/or VG_(rank_to_realreg)() */
      RRegSet regs_live_after:VG_MAX_REALREGS; 
   }
   UInstr;


typedef 
   struct _UCodeBlock
   UCodeBlock;

extern Int     VG_(get_num_instrs) (UCodeBlock* cb);
extern Int     VG_(get_num_temps)  (UCodeBlock* cb);

extern UInstr* VG_(get_instr)      (UCodeBlock* cb, Int i);
extern UInstr* VG_(get_last_instr) (UCodeBlock* cb);
   

/*====================================================================*/
/*=== Instrumenting UCode                                          ===*/
/*====================================================================*/

/* Maximum number of registers read or written by a single UInstruction. */
#define VG_MAX_REGS_USED   3

/* Find what this instruction does to its regs, useful for
   analysis/optimisation passes.  `tag' indicates whether we're considering
   TempRegs (pre-reg-alloc) or RealRegs (post-reg-alloc).  `regs' is filled
   with the affected register numbers, `isWrites' parallels it and indicates
   if the reg is read or written.  If a reg is read and written, it will
   appear twice in `regs'.  `regs' and `isWrites' must be able to fit
   VG_MAX_REGS_USED elements. */
extern Int VG_(get_reg_usage) ( UInstr* u, Tag tag, Int* regs, Bool* isWrites );


/* Used to register helper functions to be called from generated code.  A
   limited number of compact helpers can be registered;  the code generated
   to call them is slightly shorter -- so register the mostly frequently
   called helpers as compact. */
extern void VG_(register_compact_helper)    ( Addr a );
extern void VG_(register_noncompact_helper) ( Addr a );


/* ------------------------------------------------------------------ */
/* Virtual register allocation */

/* Get a new virtual register */
extern Int VG_(get_new_temp)   ( UCodeBlock* cb );

/* Get a new virtual shadow register */
extern Int VG_(get_new_shadow) ( UCodeBlock* cb );

/* Get a virtual register's corresponding virtual shadow register */
#define SHADOW(tempreg)  ((tempreg)+1)


/* ------------------------------------------------------------------ */
/* Low-level UInstr builders */
extern void VG_(new_NOP)     ( UInstr* u );
extern void VG_(new_UInstr0) ( UCodeBlock* cb, Opcode opcode, Int sz );
extern void VG_(new_UInstr1) ( UCodeBlock* cb, Opcode opcode, Int sz,
                               Tag tag1, UInt val1 );
extern void VG_(new_UInstr2) ( UCodeBlock* cb, Opcode opcode, Int sz,
                              Tag tag1, UInt val1,
                              Tag tag2, UInt val2 );
extern void VG_(new_UInstr3) ( UCodeBlock* cb, Opcode opcode, Int sz,
                              Tag tag1, UInt val1,
                              Tag tag2, UInt val2,
                              Tag tag3, UInt val3 );

/* Set read/write/undefined flags.  Undefined flags are treaten as written, 
   but it's worth keeping them logically distinct. */
extern void VG_(set_flag_fields)  ( UCodeBlock* cb, FlagSet fr, FlagSet fw,
                                    FlagSet fu);
extern void VG_(set_lit_field)    ( UCodeBlock* cb, UInt lit32 );
extern void VG_(set_ccall_fields) ( UCodeBlock* cb, Addr fn, UChar argc,
                                    UChar regparms_n, Bool has_ret_val );
extern void VG_(set_cond_field)   ( UCodeBlock* cb, Condcode code );

extern void VG_(copy_UInstr) ( UCodeBlock* cb, UInstr* instr );

extern Bool VG_(any_flag_use)( UInstr* u );

/* Macro versions of the above;  just shorter to type. */
#define uInstr0   VG_(new_UInstr0)
#define uInstr1   VG_(new_UInstr1)
#define uInstr2   VG_(new_UInstr2)
#define uInstr3   VG_(new_UInstr3)
#define uLiteral  VG_(set_lit_field)
#define uCCall    VG_(set_ccall_fields)
#define uCond     VG_(set_cond_field)
#define uFlagsRWU VG_(set_flag_fields)
#define newTemp   VG_(get_new_temp)
#define newShadow VG_(get_new_shadow)

/* Refer to `the last instruction stuffed in' (can be lvalue). */
#define LAST_UINSTR(cb) (cb)->instrs[(cb)->used-1]


/* ------------------------------------------------------------------ */
/* Higher-level UInstr sequence builders */
extern void VG_(call_helper_0_0) ( UCodeBlock* cb, Addr f);
extern void VG_(call_helper_1_0) ( UCodeBlock* cb, Addr f, UInt arg1,
                                   UInt regparms_n);
extern void VG_(call_helper_2_0) ( UCodeBlock* cb, Addr f, UInt arg1, UInt arg2,
                                   UInt regparms_n);

/* One way around the 3-arg C function limit is to pass args via global
 * variables... ugly, but it works.  This one puts a literal in there. */
extern void VG_(set_global_var) ( UCodeBlock* cb, Addr globvar_ptr, UInt val);

/* This one puts the contents of a TempReg in the global variable. */
extern void VG_(set_global_var_tempreg) ( UCodeBlock* cb, Addr globvar_ptr,
                                          UInt t_val);

/* ------------------------------------------------------------------ */
/* Allocating/freeing basic blocks of UCode */
extern UCodeBlock* VG_(setup_UCodeBlock) ( UCodeBlock* cb );
extern void        VG_(free_UCodeBlock)  ( UCodeBlock* cb );

/* ------------------------------------------------------------------ */
/* UCode pretty/ugly printing.  Probably only useful to call from a skin 
   if VG_(needs).extended_UCode == True. */

/* When True, all generated code is/should be printed. */
extern Bool  VG_(print_codegen);

/* Pretty/ugly printing functions */
extern void  VG_(pp_UCodeBlock)  ( UCodeBlock* cb, Char* title );
extern void  VG_(pp_UInstr)      ( Int instrNo, UInstr* u );
extern void  VG_(pp_UInstr_regs) ( Int instrNo, UInstr* u );
extern void  VG_(up_UInstr)      ( Int instrNo, UInstr* u );
extern Char* VG_(name_UOpcode)   ( Bool upper, Opcode opc );
extern Char* VG_(name_UCondcode) ( Condcode cond );
extern void  VG_(pp_UOperand)    ( UInstr* u, Int operandNo, 
                                   Int sz, Bool parens );

/* ------------------------------------------------------------------ */
/* Accessing archregs and their shadows */
extern UInt VG_(get_archreg)            ( UInt archreg );
extern UInt VG_(get_thread_archreg)     ( ThreadId tid, UInt archreg );

extern UInt VG_(get_shadow_archreg)     ( UInt archreg );
extern void VG_(set_shadow_archreg)     ( UInt archreg, UInt val );
extern void VG_(set_shadow_eflags)      ( UInt val );
extern Addr VG_(shadow_archreg_address) ( UInt archreg );

extern UInt VG_(get_thread_shadow_archreg) ( ThreadId tid, UInt archreg );
extern void VG_(set_thread_shadow_archreg) ( ThreadId tid, UInt archreg,
                                             UInt val );

/* ------------------------------------------------------------------ */
/* Offsets of addresses of helper functions.  A "helper" function is one
   which is called from generated code via CALLM. */

extern Int VGOFF_(helper_idiv_64_32);
extern Int VGOFF_(helper_div_64_32);
extern Int VGOFF_(helper_idiv_32_16);
extern Int VGOFF_(helper_div_32_16);
extern Int VGOFF_(helper_idiv_16_8);
extern Int VGOFF_(helper_div_16_8);

extern Int VGOFF_(helper_imul_32_64);
extern Int VGOFF_(helper_mul_32_64);
extern Int VGOFF_(helper_imul_16_32);
extern Int VGOFF_(helper_mul_16_32);
extern Int VGOFF_(helper_imul_8_16);
extern Int VGOFF_(helper_mul_8_16);

extern Int VGOFF_(helper_CLD);
extern Int VGOFF_(helper_STD);
extern Int VGOFF_(helper_get_dirflag);

extern Int VGOFF_(helper_CLC);
extern Int VGOFF_(helper_STC);

extern Int VGOFF_(helper_shldl);
extern Int VGOFF_(helper_shldw);
extern Int VGOFF_(helper_shrdl);
extern Int VGOFF_(helper_shrdw);

extern Int VGOFF_(helper_RDTSC);
extern Int VGOFF_(helper_CPUID);

extern Int VGOFF_(helper_bsf);
extern Int VGOFF_(helper_bsr);

extern Int VGOFF_(helper_fstsw_AX);
extern Int VGOFF_(helper_SAHF);
extern Int VGOFF_(helper_LAHF);
extern Int VGOFF_(helper_DAS);
extern Int VGOFF_(helper_DAA);


/*====================================================================*/
/*=== Generating x86 code from UCode                               ===*/
/*====================================================================*/

/* All this only necessary for skins with VG_(needs).extends_UCode == True. */

/* This is the Intel register encoding -- integer regs. */
#define R_EAX 0
#define R_ECX 1
#define R_EDX 2
#define R_EBX 3
#define R_ESP 4
#define R_EBP 5
#define R_ESI 6
#define R_EDI 7

#define R_AL (0+R_EAX)
#define R_CL (0+R_ECX)
#define R_DL (0+R_EDX)
#define R_BL (0+R_EBX)
#define R_AH (4+R_EAX)
#define R_CH (4+R_ECX)
#define R_DH (4+R_EDX)
#define R_BH (4+R_EBX)

/* This is the Intel register encoding -- segment regs. */
#define R_ES 0
#define R_CS 1
#define R_SS 2
#define R_DS 3
#define R_FS 4
#define R_GS 5

/* For pretty printing x86 code */
extern Char* VG_(name_of_mmx_gran) ( UChar gran );
extern Char* VG_(name_of_mmx_reg)  ( Int mmxreg );
extern Char* VG_(name_of_seg_reg)  ( Int sreg );
extern Char* VG_(name_of_int_reg)  ( Int size, Int reg );
extern Char  VG_(name_of_int_size) ( Int size );

/* Shorter macros for convenience */
#define nameIReg    VG_(name_of_int_reg)
#define nameISize   VG_(name_of_int_size)
#define nameSReg    VG_(name_of_seg_reg)
#define nameMMXReg  VG_(name_of_mmx_reg)
#define nameMMXGran VG_(name_of_mmx_gran)
#define nameXMMReg  VG_(name_of_xmm_reg)

/* Randomly useful things */
extern UInt  VG_(extend_s_8to32) ( UInt x );

/* Code emitters */
extern void VG_(emitB)    ( UInt b );
extern void VG_(emitW)    ( UInt w );
extern void VG_(emitL)    ( UInt l );
extern void VG_(new_emit) ( Bool upd_cc, FlagSet uses_flags, FlagSet sets_flags );

/* Finding offsets */
extern Int  VG_(helper_offset)       ( Addr a );
extern Int  VG_(shadow_reg_offset)   ( Int arch );
extern Int  VG_(shadow_flags_offset) ( void );

/* Convert reg ranks <-> Intel register ordering, for using register
   liveness information. */
extern Int VG_(realreg_to_rank) ( Int realreg );
extern Int VG_(rank_to_realreg) ( Int rank    );

/* Call a subroutine.  Does no argument passing, stack manipulations, etc. */
extern void VG_(synth_call) ( Bool ensure_shortform, Int word_offset, 
			      Bool upd_cc, FlagSet use_flags, FlagSet set_flags );

/* For calling C functions -- saves caller save regs, pushes args, calls,
   clears the stack, restores caller save regs.  `fn' must be registered in
   the baseBlock first.  Acceptable tags are RealReg and Literal.  Optimises
   things, eg. by not preserving non-live caller-save registers.

   WARNING:  a UInstr should *not* be translated with synth_ccall() followed
   by some other x86 assembly code;  this will invalidate the results of
   vg_realreg_liveness_analysis() and everything will fall over.  */
extern void VG_(synth_ccall) ( Addr fn, Int argc, Int regparms_n, UInt argv[],
                               Tag tagv[], Int ret_reg, 
                               RRegSet regs_live_before,
                               RRegSet regs_live_after );

/* Addressing modes */
extern void VG_(emit_amode_offregmem_reg)( Int off, Int regmem, Int reg );
extern void VG_(emit_amode_ereg_greg)    ( Int e_reg, Int g_reg );

/* v-size (4, or 2 with OSO) insn emitters */
extern void VG_(emit_movv_offregmem_reg) ( Int sz, Int off, Int areg, Int reg );
extern void VG_(emit_movv_reg_offregmem) ( Int sz, Int reg, Int off, Int areg );
extern void VG_(emit_movv_reg_reg)       ( Int sz, Int reg1, Int reg2 );
extern void VG_(emit_nonshiftopv_lit_reg)( Bool upd_cc, Int sz, Opcode opc, UInt lit,
                                           Int reg );
extern void VG_(emit_shiftopv_lit_reg)   ( Bool upd_cc, Int sz, Opcode opc, UInt lit,
                                           Int reg );
extern void VG_(emit_nonshiftopv_reg_reg)( Bool upd_cc, Int sz, Opcode opc,
                                           Int reg1, Int reg2 );
extern void VG_(emit_movv_lit_reg)       ( Int sz, UInt lit, Int reg );
extern void VG_(emit_unaryopv_reg)       ( Bool upd_cc, Int sz, Opcode opc, Int reg );
extern void VG_(emit_pushv_reg)          ( Int sz, Int reg );
extern void VG_(emit_popv_reg)           ( Int sz, Int reg );

extern void VG_(emit_pushl_lit32)        ( UInt int32 );
extern void VG_(emit_pushl_lit8)         ( Int lit8 );
extern void VG_(emit_cmpl_zero_reg)      ( Bool upd_cc, Int reg );
extern void VG_(emit_swapl_reg_EAX)      ( Int reg );
extern void VG_(emit_movv_lit_offregmem) ( Int sz, UInt lit, Int off,
                                           Int memreg );

/* b-size (1 byte) instruction emitters */
extern void VG_(emit_movb_lit_offregmem) ( UInt lit, Int off, Int memreg );
extern void VG_(emit_movb_reg_offregmem) ( Int reg, Int off, Int areg );
extern void VG_(emit_unaryopb_reg)       ( Bool upd_cc, Opcode opc, Int reg );
extern void VG_(emit_testb_lit_reg)      ( Bool upd_cc, UInt lit, Int reg );

/* zero-extended load emitters */
extern void VG_(emit_movzbl_offregmem_reg) ( Int off, Int regmem, Int reg );
extern void VG_(emit_movzwl_offregmem_reg) ( Int off, Int areg, Int reg );

/* misc instruction emitters */
extern void VG_(emit_call_reg)         ( Int reg );
extern void VG_(emit_add_lit_to_esp)   ( Int lit );
extern void VG_(emit_pushal)           ( void );
extern void VG_(emit_popal)            ( void );
extern void VG_(emit_AMD_prefetch_reg) ( Int reg );

/* jump emitters */
extern void VG_(init_target)	       ( Int *tgt );

extern void VG_(target_back)	       ( Int *tgt );
extern void VG_(target_forward)	       ( Int *tgt );
extern void VG_(emit_target_delta)     ( Int *tgt );

extern void VG_(emit_jcondshort_delta) ( Bool simd_cc, Condcode cond, Int delta );
extern void VG_(emit_jcondshort_target)( Bool simd_cc, Condcode cond, Int *tgt );


/*====================================================================*/
/*=== Execution contexts                                           ===*/
/*====================================================================*/

/* Generic resolution type used in a few different ways, such as deciding
   how closely to compare two errors for equality. */
typedef 
   enum { Vg_LowRes, Vg_MedRes, Vg_HighRes } 
   VgRes;

typedef
   struct _ExeContext
   ExeContext;

/* Compare two ExeContexts.  Number of callers considered depends on `res': 
     Vg_LowRes:  2 
     Vg_MedRes:  4 
     Vg_HighRes: all */
extern Bool VG_(eq_ExeContext) ( VgRes res,
                                 ExeContext* e1, ExeContext* e2 );

/* Print an ExeContext. */
extern void VG_(pp_ExeContext) ( ExeContext* );

/* Take a snapshot of the client's stack.  Search our collection of
   ExeContexts to see if we already have it, and if not, allocate a
   new one.  Either way, return a pointer to the context.  Context size
   controlled by --num-callers option.
   
   If called from generated code, use VG_(get_current_tid)() to get the
   current ThreadId.  If called from non-generated code, the current
   ThreadId should be passed in by the core. 
*/
extern ExeContext* VG_(get_ExeContext) ( ThreadId tid );

/* Get the nth EIP from the ExeContext.  0 is the EIP of the top function, 1
   is its caller, etc.  Returns 0 if there isn't one, or if n is greater
   than VG_(clo_backtrace_size), set by the --num-callers option. */
extern Addr VG_(get_EIP_from_ExeContext) ( ExeContext* e, UInt n );

/* Just grab the client's EIP, as a much smaller and cheaper
   indication of where they are.  Use is basically same as for
   VG_(get_ExeContext)() above. 
*/
extern Addr VG_(get_EIP)( ThreadId tid );

/* For skins needing more control over stack traces:  walks the stack to get
   %eips from the top stack frames for thread 'tid'.  Maximum of 'n_eips'
   addresses put into 'eips';  0 is the top of the stack, 1 is its caller,
   etc. */
extern UInt VG_(stack_snapshot) ( ThreadId tid, Addr* eips, UInt n_eips );

/* Does the same thing as VG_(pp_ExeContext)(), just with slightly
   different input. */
extern void VG_(mini_stack_dump) ( Addr eips[], UInt n_eips );


/*====================================================================*/
/*=== Error reporting                                              ===*/
/*====================================================================*/

/* ------------------------------------------------------------------ */
/* Suppressions describe errors which we want to suppress, ie, not 
   show the user, usually because it is caused by a problem in a library
   which we can't fix, replace or work around.  Suppressions are read from 
   a file at startup time.  This gives flexibility so that new
   suppressions can be added to the file as and when needed.
*/

typedef
   Int         /* Do not make this unsigned! */
   SuppKind;

/* The skin-relevant parts of a suppression are:
     kind:   what kind of suppression; must be in the range (0..)
     string: use is optional.  NULL by default.
     extra:  use is optional.  NULL by default.  void* so it's extensible.
*/
typedef
   struct _Supp
   Supp;

/* Useful in SK_(error_matches_suppression)() */
SuppKind VG_(get_supp_kind)   ( Supp* su );
Char*    VG_(get_supp_string) ( Supp* su );
void*    VG_(get_supp_extra)  ( Supp* su );

/* Must be used in VG_(recognised_suppression)() */
void VG_(set_supp_kind)   ( Supp* su, SuppKind suppkind );
/* May be used in VG_(read_extra_suppression_info)() */
void VG_(set_supp_string) ( Supp* su, Char* string );
void VG_(set_supp_extra)  ( Supp* su, void* extra );


/* ------------------------------------------------------------------ */
/* Error records contain enough info to generate an error report.  The idea
   is that (typically) the same few points in the program generate thousands
   of errors, and we don't want to spew out a fresh error message for each
   one.  Instead, we use these structures to common up duplicates.
*/

typedef
   Int         /* Do not make this unsigned! */
   ErrorKind;

/* The skin-relevant parts of an Error are:
     kind:   what kind of error; must be in the range (0..)
     addr:   use is optional.  0 by default.
     string: use is optional.  NULL by default.
     extra:  use is optional.  NULL by default.  void* so it's extensible.
*/
typedef
   struct _Error
   Error;

/* Useful in SK_(error_matches_suppression)(), SK_(pp_SkinError)(), etc */
ExeContext* VG_(get_error_where)   ( Error* err );
SuppKind    VG_(get_error_kind)    ( Error* err );
Addr        VG_(get_error_address) ( Error* err );
Char*       VG_(get_error_string)  ( Error* err );
void*       VG_(get_error_extra)   ( Error* err );

/* Call this when an error occurs.  It will be recorded if it hasn't been
   seen before.  If it has, the existing error record will have its count
   incremented.  
   
   'tid' can be found as for VG_(get_ExeContext)().  The `extra' field can
   be stack-allocated;  it will be copied by the core if needed (but it
   won't be copied if it's NULL).

   If no 'a', 's' or 'extra' of interest needs to be recorded, just use
   NULL for them.  */
extern void VG_(maybe_record_error) ( ThreadId tid, ErrorKind ekind, 
                                      Addr a, Char* s, void* extra );

/* Similar to VG_(maybe_record_error)(), except this one doesn't record the
   error -- useful for errors that can only happen once.  The errors can be
   suppressed, though.  Return value is True if it was suppressed.
   `print_error' dictates whether to print the error, which is a bit of a 
   hack that's useful sometimes if you just want to know if the error would
   be suppressed without possibly printing it.  `count_error' dictates 
   whether to add the error in the error total count (another mild hack). */
extern Bool VG_(unique_error) ( ThreadId tid, ErrorKind ekind,
                                Addr a, Char* s, void* extra,
                                ExeContext* where, Bool print_error,
                                Bool allow_GDB_attach, Bool count_error );

/* Gets a non-blank, non-comment line of at most nBuf chars from fd.
   Skips leading spaces on the line.  Returns True if EOF was hit instead. 
   Useful for reading in extra skin-specific suppression lines.  */
extern Bool VG_(get_line) ( Int fd, Char* buf, Int nBuf );


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
extern Char* VG_(describe_eip)(Addr eip, Char* buf, Int n_buf);

/* A way to get information about what segments are mapped */
typedef struct _SegInfo SegInfo;

/* Returns NULL if the SegInfo isn't found.  It doesn't matter if debug info
   is present or not. */
extern SegInfo* VG_(get_obj)  ( Addr a );

extern const SegInfo* VG_(next_seginfo)  ( const SegInfo *seg );
extern       Addr     VG_(seg_start)     ( const SegInfo *seg );
extern       UInt     VG_(seg_size)      ( const SegInfo *seg );
extern const UChar*   VG_(seg_filename)  ( const SegInfo *seg );
extern       UInt     VG_(seg_sym_offset)( const SegInfo *seg );

typedef
   enum {
      Vg_SectUnknown,
      Vg_SectText,
      Vg_SectData,
      Vg_SectBSS,
      Vg_SectGOT,
      Vg_SectPLT,
   }
   VgSectKind;

extern VgSectKind VG_(seg_sect_kind)(Addr);


/*====================================================================*/
/*=== Generic hash table                                           ===*/
/*====================================================================*/

/* Generic type for a separately-chained hash table.  Via a kind of dodgy
   C-as-C++ style inheritance, skins can extend the VgHashNode type, so long
   as the first two fields match the sizes of these two fields.  Requires
   a bit of casting by the skin. */
typedef
   struct _VgHashNode {
      struct _VgHashNode * next;
      UInt               key;
   }
   VgHashNode;

typedef
   VgHashNode**
   VgHashTable;

/* Make a new table. */
extern VgHashTable VG_(HT_construct) ( void );

/* Add a node to the table. */
extern void VG_(HT_add_node) ( VgHashTable t, VgHashNode* node );

/* Looks up a node in the hash table.  Also returns the address of the 
   previous node's `next' pointer which allows it to be removed from the
   list later without having to look it up again.  */
extern VgHashNode* VG_(HT_get_node) ( VgHashTable t, UInt key,
                                    /*OUT*/VgHashNode*** next_ptr );

/* Allocates a sorted array of pointers to all the shadow chunks of malloc'd
   blocks. */
extern VgHashNode** VG_(HT_to_sorted_array) ( VgHashTable t, 
                                              /*OUT*/ UInt* n_shadows );

/* Returns first node that matches predicate `p', or NULL if none do.
   Extra arguments can be implicitly passed to `p' using nested functions;
   see memcheck/mc_errcontext.c for an example. */
extern VgHashNode* VG_(HT_first_match) ( VgHashTable t,
                                         Bool (*p)(VgHashNode*) );

/* Applies a function f() once to each node.  Again, nested functions
   can be very useful. */
extern void VG_(HT_apply_to_all_nodes)( VgHashTable t, void (*f)(VgHashNode*) );

/* Destroy a table. */
extern void VG_(HT_destruct) ( VgHashTable t );


/*====================================================================*/
/*=== Functions for shadow registers                               ===*/
/*====================================================================*/

/* Nb: make sure the shadow_regs 'need' is set before using these! */

/* This one lets you override the shadow of the return value register for a
   syscall.  Call it from SK_(post_syscall)() (not SK_(pre_syscall)()!) to
   override the default shadow register value. */
extern void VG_(set_return_from_syscall_shadow) ( ThreadId tid, 
                                                  UInt ret_shadow );

/* This can be called from SK_(fini)() to find the shadow of the argument
   to exit(), ie. the shadow of the program's return value. */
extern UInt VG_(get_exit_status_shadow) ( void );


/*====================================================================*/
/*=== General stuff for replacing functions                        ===*/
/*====================================================================*/

/* Some skins need to replace the standard definitions of some functions. */

/* ------------------------------------------------------------------ */
/* General stuff, for replacing any functions */

/* Is the client running on the simulated CPU or the real one? 

   Nb: If it is, and you want to call a function to be run on the real CPU,
   use one of the VALGRIND_NON_SIMD_CALL[123] macros in valgrind.h to call it.

   Nb: don't forget the function parentheses when using this in a 
   condition... write this:

     if (VG_(is_running_on_simd_CPU)()) { ... }    // calls function

   not this:
     
     if (VG_(is_running_on_simd_CPU)) { ... }      // address of var!
*/
extern Bool VG_(is_running_on_simd_CPU) ( void ); 


/*====================================================================*/
/*=== Specific stuff for replacing malloc() and friends            ===*/
/*====================================================================*/

/* If a skin replaces malloc() et al, the easiest way to do so is to link
   with coregrind/vg_replace_malloc.c, and follow the following instructions.
   You can do it from scratch, though, if you enjoy that sort of thing. */

/* Arena size for valgrind's own malloc();  default value is 0, but can
   be overridden by skin -- but must be done so *statically*, eg:
  
     Int VG_(vg_malloc_redzone_szB) = 4;
  
   It can't be done from a function like SK_(pre_clo_init)().  So it can't,
   for example, be controlled with a command line option, unfortunately. */
extern UInt VG_(vg_malloc_redzone_szB);

/* If a skin links with vg_replace_malloc.c, the following functions will be
   called appropriately when malloc() et al are called. */
extern void* SK_(malloc)               ( Int n );
extern void* SK_(__builtin_new)        ( Int n );
extern void* SK_(__builtin_vec_new)    ( Int n );
extern void* SK_(memalign)             ( Int align, Int n );
extern void* SK_(calloc)               ( Int nmemb, Int n );
extern void  SK_(free)                 ( void* p );
extern void  SK_(__builtin_delete)     ( void* p );
extern void  SK_(__builtin_vec_delete) ( void* p );
extern void* SK_(realloc)              ( void* p, Int size );

/* Can be called from SK_(malloc) et al to do the actual alloc/freeing. */
extern void* VG_(cli_malloc) ( UInt align, Int nbytes ); 
extern void  VG_(cli_free)   ( void* p );

/* Check if an address is within a range, allowing for redzones at edges */
extern Bool VG_(addr_is_in_block)( Addr a, Addr start, UInt size );

/* ------------------------------------------------------------------ */
/* Some options that can be used by a skin if malloc() et al are replaced. 
   The skin should call the functions in the appropriate places to give
   control over these aspects of Valgrind's version of malloc(). */

/* Round malloc sizes upwards to integral number of words? default: NO */
extern Bool VG_(clo_sloppy_malloc);
/* DEBUG: print malloc details?  default: NO */
extern Bool VG_(clo_trace_malloc);
/* Minimum alignment in functions that don't specify alignment explicitly.
   default: 0, i.e. use default of the machine (== 4) */
extern Int  VG_(clo_alignment);

extern Bool VG_(replacement_malloc_process_cmd_line_option) ( Char* arg );
extern void VG_(replacement_malloc_print_usage)             ( void );
extern void VG_(replacement_malloc_print_debug_usage)       ( void );


/*====================================================================*/
/*=== Skin-specific stuff                                          ===*/
/*====================================================================*/

/* ------------------------------------------------------------------ */
/* Details */

/* Default value for avg_translations_sizeB (in bytes), indicating typical
   code expansion of about 6:1. */
#define VG_DEFAULT_TRANS_SIZEB   100

/* Information used in the startup message.  `name' also determines the
   string used for identifying suppressions in a suppression file as
   belonging to this skin.  `version' can be NULL, in which case (not
   surprisingly) no version info is printed; this mechanism is designed for
   skins distributed with Valgrind that share a version number with
   Valgrind.  Other skins not distributed as part of Valgrind should
   probably have their own version number.  */
extern void VG_(details_name)                  ( Char* name );
extern void VG_(details_version)               ( Char* version );
extern void VG_(details_description)           ( Char* description );
extern void VG_(details_copyright_author)      ( Char* copyright_author );

/* Average size of a translation, in bytes, so that the translation
   storage machinery can allocate memory appropriately.  Not critical,
   setting is optional. */ 
extern void VG_(details_avg_translation_sizeB) ( UInt size );

/* String printed if an `sk_assert' assertion fails or VG_(skin_panic)
   is called.  Should probably be an email address. */
extern void VG_(details_bug_reports_to)   ( Char* bug_reports_to );

/* ------------------------------------------------------------------ */
/* Needs */

/* Booleans that decide core behaviour, but don't require extra
   operations to be defined if `True' */

/* Should __libc_freeres() be run?  Bugs in it can crash the skin. */
extern void VG_(needs_libc_freeres) ( void );

/* Want to have errors detected by Valgrind's core reported?  Includes:
   - pthread API errors (many;  eg. unlocking a non-locked mutex)
   - invalid file descriptors to blocking syscalls read() and write()
   - bad signal numbers passed to sigaction()
   - attempt to install signal handler for SIGKILL or SIGSTOP */  
extern void VG_(needs_core_errors) ( void );

/* Booleans that indicate extra operations are defined;  if these are True,
   the corresponding template functions (given below) must be defined.  A
   lot like being a member of a type class. */

/* Want to report errors from skin?  This implies use of suppressions, too. */
extern void VG_(needs_skin_errors) ( void );

/* Is information kept about specific individual basic blocks?  (Eg. for
   cachegrind there are cost-centres for every instruction, stored at a
   basic block level.)  If so, it sometimes has to be discarded, because
   .so mmap/munmap-ping or self-modifying code (informed by the
   DISCARD_TRANSLATIONS user request) can cause one instruction address
   to be used for more than one instruction in one program run...  */
extern void VG_(needs_basic_block_discards) ( void );

/* Skin maintains information about each register? */
extern void VG_(needs_shadow_regs) ( void );

/* Skin defines its own command line options? */
extern void VG_(needs_command_line_options) ( void );

/* Skin defines its own client requests? */
extern void VG_(needs_client_requests) ( void );

/* Skin defines its own UInstrs? */
extern void VG_(needs_extended_UCode) ( void );

/* Skin does stuff before and/or after system calls? */
extern void VG_(needs_syscall_wrapper) ( void );

/* Are skin-state sanity checks performed? */
extern void VG_(needs_sanity_checks) ( void );

/* Do we need to see data symbols? */
extern void VG_(needs_data_syms) ( void );

/* ------------------------------------------------------------------ */
/* Core events to track */

/* Part of the core from which this call was made.  Useful for determining
   what kind of error message should be emitted. */
typedef 
   enum { Vg_CorePThread, Vg_CoreSignal, Vg_CoreSysCall, Vg_CoreTranslate }
   CorePart;

#define EV  extern void

/* Events happening in core to track.  To be notified, pass a callback
   function to the appropriate function.  To ignore an event, don't do
   anything (default is for events to be ignored). 
   
   Note that most events aren't passed a ThreadId.  To find out the ThreadId
   of the affected thread, use VG_(get_current_or_recent_tid)().  For the
   ones passed a ThreadId, use that instead, since
   VG_(get_current_or_recent_tid)() might not give the right ThreadId in
   that case.
*/


/* Memory events (Nb: to track heap allocation/freeing, a skin must replace
   malloc() et al.  See above how to do this.) */

/* These ones occur at startup, upon some signals, and upon some syscalls */
EV VG_(track_new_mem_startup) ( void (*f)(Addr a, UInt len, 
                                          Bool rr, Bool ww, Bool xx) );
EV VG_(track_new_mem_stack_signal)  ( void (*f)(Addr a, UInt len) );
EV VG_(track_new_mem_brk)     ( void (*f)(Addr a, UInt len) );
EV VG_(track_new_mem_mmap)    ( void (*f)(Addr a, UInt len,
                                          Bool rr, Bool ww, Bool xx) );

EV VG_(track_copy_mem_remap)  ( void (*f)(Addr from, Addr to, UInt len) );
EV VG_(track_change_mem_mprotect) ( void (*f)(Addr a, UInt len,
                                              Bool rr, Bool ww, Bool xx) );
EV VG_(track_die_mem_stack_signal)  ( void (*f)(Addr a, UInt len) );
EV VG_(track_die_mem_brk)     ( void (*f)(Addr a, UInt len) );
EV VG_(track_die_mem_munmap)  ( void (*f)(Addr a, UInt len) );


/* These ones are called when %esp changes.  A skin could track these itself
   (except for ban_mem_stack) but it's much easier to use the core's help.
  
   The specialised ones are called in preference to the general one, if they
   are defined.  These functions are called a lot if they are used, so
   specialising can optimise things significantly.  If any of the
   specialised cases are defined, the general case must be defined too. 
   
   Nb: they must all use the __attribute__((regparm(n))) attribute. */
EV VG_(track_new_mem_stack_4)  ( void (*f)(Addr new_ESP) );
EV VG_(track_new_mem_stack_8)  ( void (*f)(Addr new_ESP) );
EV VG_(track_new_mem_stack_12) ( void (*f)(Addr new_ESP) );
EV VG_(track_new_mem_stack_16) ( void (*f)(Addr new_ESP) );
EV VG_(track_new_mem_stack_32) ( void (*f)(Addr new_ESP) );
EV VG_(track_new_mem_stack)    ( void (*f)(Addr a, UInt len) );

EV VG_(track_die_mem_stack_4)  ( void (*f)(Addr die_ESP) );
EV VG_(track_die_mem_stack_8)  ( void (*f)(Addr die_ESP) );
EV VG_(track_die_mem_stack_12) ( void (*f)(Addr die_ESP) );
EV VG_(track_die_mem_stack_16) ( void (*f)(Addr die_ESP) );
EV VG_(track_die_mem_stack_32) ( void (*f)(Addr die_ESP) );
EV VG_(track_die_mem_stack)    ( void (*f)(Addr a, UInt len) );

/* Used for redzone at end of thread stacks */
EV VG_(track_ban_mem_stack)   ( void (*f)(Addr a, UInt len) );

/* These ones occur around syscalls, signal handling, etc */
EV VG_(track_pre_mem_read)    ( void (*f)(CorePart part, ThreadId tid,
                                          Char* s, Addr a, UInt size) );
EV VG_(track_pre_mem_read_asciiz) ( void (*f)(CorePart part, ThreadId tid,
                                              Char* s, Addr a) );
EV VG_(track_pre_mem_write)   ( void (*f)(CorePart part, ThreadId tid,
                                          Char* s, Addr a, UInt size) );
/* Not implemented yet -- have to add in lots of places, which is a
   pain.  Won't bother unless/until there's a need. */
/* EV VG_(track_post_mem_read)  ( void (*f)(ThreadId tid, Char* s, 
                                            Addr a, UInt size) ); */
EV VG_(track_post_mem_write) ( void (*f)(Addr a, UInt size) );


/* Register events -- if `shadow_regs' need is set, all should probably be
   used.  Use VG_(set_thread_shadow_archreg)() to set the shadow of the
   changed register. */

/* Use VG_(set_shadow_archreg)() to set the eight general purpose regs,
   and use VG_(set_shadow_eflags)() to set eflags. */
EV VG_(track_post_regs_write_init)  ( void (*f)() );    

/* Use VG_(set_thread_shadow_archreg)() to set the shadow regs for these 
   events. */
EV VG_(track_post_reg_write_syscall_return)    
                                    ( void (*f)(ThreadId tid, UInt reg) );
EV VG_(track_post_reg_write_deliver_signal)
                                    ( void (*f)(ThreadId tid, UInt reg) );
EV VG_(track_post_reg_write_pthread_return)
                                    ( void (*f)(ThreadId tid, UInt reg) );
EV VG_(track_post_reg_write_clientreq_return)
                                    ( void (*f)(ThreadId tid, UInt reg) );
   /* This one is called for malloc() et al if they are replaced by a skin. */
EV VG_(track_post_reg_write_clientcall_return)
                                    ( void (*f)(ThreadId tid, UInt reg,
                                                Addr called_function) );


/* Scheduler events (not exhaustive) */

EV VG_(track_thread_run) ( void (*f)(ThreadId tid) );

/* Thread events (not exhaustive) */

/* Called during thread create, before the new thread has run any
   instructions (or touched any memory). */
EV VG_(track_post_thread_create)( void (*f)(ThreadId tid, ThreadId child) );
/* Called once the joinee thread is terminated and the joining thread is
   about to resume. */
EV VG_(track_post_thread_join)  ( void (*f)(ThreadId joiner, ThreadId joinee) );

      
/* Mutex events (not exhaustive) */

/* Called before a thread can block while waiting for a mutex (called
   regardless of whether the thread will block or not). */
EV VG_(track_pre_mutex_lock)    ( void (*f)(ThreadId tid, 
                                          void* /*pthread_mutex_t* */ mutex) );
/* Called once the thread actually holds the mutex (always paired with
   pre_mutex_lock). */
EV VG_(track_post_mutex_lock)   ( void (*f)(ThreadId tid, 
                                          void* /*pthread_mutex_t* */ mutex) );
/* Called after a thread has released a mutex (no need for a corresponding
   pre_mutex_unlock, because unlocking can't block). */
EV VG_(track_post_mutex_unlock) ( void (*f)(ThreadId tid, 
                                          void* /*pthread_mutex_t* */ mutex) );


/* Signal events (not exhaustive) */

/* ... pre_send_signal, post_send_signal ... */

/* Called before a signal is delivered;  `alt_stack' indicates if it is
   delivered on an alternative stack. */
EV VG_(track_pre_deliver_signal)  ( void (*f)(ThreadId tid, Int sigNum,
                                             Bool alt_stack) );
/* Called after a signal is delivered.  Nb: unfortunately, if the signal
   handler longjmps, this won't be called. */
EV VG_(track_post_deliver_signal) ( void (*f)(ThreadId tid, Int sigNum ) );


/* Others... condition variables... */
/* ... */

#undef EV

/* ------------------------------------------------------------------ */
/* Template functions */

/* These are the parameterised functions in the core.  The default definitions
   are overridden by LD_PRELOADed skin version.  At the very least, a skin
   must define the fundamental template functions.  Depending on what needs
   are set, extra template functions will be used too.  Functions are
   grouped under the needs that govern their use. */


/* ------------------------------------------------------------------ */
/* Fundamental template functions */

/* Initialise skin.   Must do the following:
     - initialise the `details' struct, via the VG_(details_*)() functions
     - register any helpers called by generated code
  
   May do the following:
     - initialise the `needs' struct to indicate certain requirements, via
       the VG_(needs_*)() functions
     - initialise the `track' struct to indicate core events of interest, via
       the VG_(track_*)() functions
     - register any skin-specific profiling events
     - any other skin-specific initialisation
*/
extern void        SK_(pre_clo_init) ( void );

/* Do initialisation that can only be done after command line processing. */
extern void        SK_(post_clo_init)( void );

/* Instrument a basic block.  Must be a true function, ie. the same input
   always results in the same output, because basic blocks can be
   retranslated.  Unless you're doing something really strange...
   'orig_addr' is the address of the first instruction in the block. */
extern UCodeBlock* SK_(instrument)   ( UCodeBlock* cb, Addr orig_addr );

/* Finish up, print out any results, etc.  `exitcode' is program's exit
   code.  The shadow (if the `shadow_regs' need is set) can be found with
   VG_(get_shadow_archreg)(R_EBX), since %ebx holds the argument to the
   exit() syscall.  */
extern void        SK_(fini)         ( Int exitcode );


/* ------------------------------------------------------------------ */
/* VG_(needs).core_errors */

/* (none needed) */

/* ------------------------------------------------------------------ */
/* VG_(needs).skin_errors */

/* Identify if two errors are equal, or equal enough.  `res' indicates how
   close is "close enough".  `res' should be passed on as necessary, eg. if
   the Error's `extra' part contains an ExeContext, `res' should be
   passed to VG_(eq_ExeContext)() if the ExeContexts are considered.  Other
   than that, probably don't worry about it unless you have lots of very
   similar errors occurring.
 */
extern Bool SK_(eq_SkinError) ( VgRes res, Error* e1, Error* e2 );

/* Print error context. */
extern void SK_(pp_SkinError) ( Error* err );

/* Should fill in any details that could be postponed until after the
   decision whether to ignore the error (ie. details not affecting the
   result of SK_(eq_SkinError)()).  This saves time when errors are ignored.
   Yuk.

   Return value: must be the size of the `extra' part in bytes -- used by
   the core to make a copy.
*/
extern UInt SK_(update_extra) ( Error* err );

/* Return value indicates recognition.  If recognised, must set skind using
   VG_(set_supp_kind)(). */
extern Bool SK_(recognised_suppression) ( Char* name, Supp* su );

/* Read any extra info for this suppression kind.  Most likely for filling
   in the `extra' and `string' parts (with VG_(set_supp_{extra,string})())
   of a suppression if necessary.  Should return False if a syntax error 
   occurred, True otherwise. */
extern Bool SK_(read_extra_suppression_info) ( Int fd, Char* buf, Int nBuf,
                                               Supp* su );

/* This should just check the kinds match and maybe some stuff in the
   `string' and `extra' field if appropriate (using VG_(get_supp_*)() to
   get the relevant suppression parts). */
extern Bool SK_(error_matches_suppression) ( Error* err, Supp* su );

/* This should return the suppression name, for --gen-suppressions, or NULL
   if that error type cannot be suppressed.  This is the inverse of
   SK_(recognised_suppression)(). */
extern Char* SK_(get_error_name) ( Error* err );

/* This should print any extra info for the error, for --gen-suppressions,
   including the newline.  This is the inverse of
   SK_(read_extra_suppression_info)(). */
extern void SK_(print_extra_suppression_info) ( Error* err );


/* ------------------------------------------------------------------ */
/* VG_(needs).basic_block_discards */

/* Should discard any information that pertains to specific basic blocks
   or instructions within the address range given. */
extern void SK_(discard_basic_block_info) ( Addr a, UInt size );


/* ------------------------------------------------------------------ */
/* VG_(needs).shadow_regs */

/* No functions must be defined, but the post_reg[s]_write_* events should
   be tracked. */

/* ------------------------------------------------------------------ */
/* VG_(needs).command_line_options */

/* Return True if option was recognised.  Presumably sets some state to
   record the option as well. */
extern Bool SK_(process_cmd_line_option) ( Char* argv );

/* Print out command line usage for options for normal skin operation. */
extern void SK_(print_usage)             ( void );

/* Print out command line usage for options for debugging the skin. */
extern void SK_(print_debug_usage)       ( void );

/* ------------------------------------------------------------------ */
/* VG_(needs).client_requests */

/* If using client requests, the number of the first request should be equal
   to VG_USERREQ_SKIN_BASE('X','Y'), where 'X' and 'Y' form a suitable two
   character identification for the string.  The second and subsequent
   requests should follow. */

/* This function should use the VG_IS_SKIN_USERREQ macro (in
   include/valgrind.h) to first check if it's a request for this skin.  Then
   should handle it if it's recognised (and return True), or return False if
   not recognised.  arg_block[0] holds the request number, any further args
   from the request are in arg_block[1..].  'ret' is for the return value...
   it should probably be filled, if only with 0. */
extern Bool SK_(handle_client_request) ( ThreadId tid, UInt* arg_block,
                                         UInt *ret );


/* ------------------------------------------------------------------ */
/* VG_(needs).extends_UCode */

/* Useful to use in VG_(get_Xreg_usage)() */
#define VG_UINSTR_READS_REG(ono,regs,isWrites)  \
   { if (mycat(u->tag,ono) == tag)              \
        { regs[n]     = mycat(u->val,ono);      \
          isWrites[n] = False;                  \
          n++;                                  \
        }                                       \
   }
#define VG_UINSTR_WRITES_REG(ono,regs,isWrites) \
   { if (mycat(u->tag,ono) == tag)              \
        { regs[n]     = mycat(u->val,ono);      \
          isWrites[n] = True;                   \
          n++;                                  \
        }                                       \
   }

/* 'X' prefix indicates eXtended UCode. */
extern Int   SK_(get_Xreg_usage) ( UInstr* u, Tag tag, Int* regs,
                                   Bool* isWrites );
extern void  SK_(emit_XUInstr)   ( UInstr* u, RRegSet regs_live_before );
extern Bool  SK_(sane_XUInstr)   ( Bool beforeRA, Bool beforeLiveness,
                                   UInstr* u );
extern Char* SK_(name_XUOpcode)  ( Opcode opc );
extern void  SK_(pp_XUInstr)     ( UInstr* u );


/* ------------------------------------------------------------------ */
/* VG_(needs).syscall_wrapper */

/* If either of the pre_ functions malloc() something to return, the
 * corresponding post_ function had better free() it! 
 */ 
extern void* SK_( pre_syscall) ( ThreadId tid, UInt syscallno,
                                 Bool is_blocking );
extern void  SK_(post_syscall) ( ThreadId tid, UInt syscallno,
                                 void* pre_result, Int res,
                                 Bool is_blocking );


/* ---------------------------------------------------------------------
   VG_(needs).sanity_checks */

/* Can be useful for ensuring a skin's correctness.  SK_(cheap_sanity_check)
   is called very frequently;  SK_(expensive_sanity_check) is called less
   frequently and can be more involved. */
extern Bool SK_(cheap_sanity_check)     ( void );
extern Bool SK_(expensive_sanity_check) ( void );


#endif   /* NDEF __VG_SKIN_H */

/*--------------------------------------------------------------------*/
/*--- end                                                vg_skin.h ---*/
/*--------------------------------------------------------------------*/

