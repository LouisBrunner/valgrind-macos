
/*--------------------------------------------------------------------*/
/*--- The only header your skin will ever need to #include...      ---*/
/*---                                                    vg_skin.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2002 Julian Seward 
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
#define VG_N_THREADS 50

/* Maximum number of pthread keys available.  Again, we start low until
   the need for a higher number presents itself. */
#define VG_N_THREAD_KEYS 50

/* Total number of integer registers available for allocation -- all of
   them except %esp, %ebp.  %ebp permanently points at VG_(baseBlock).
   
   If you change this you'll have to also change at least these:
     - VG_(rank_to_realreg)()
     - VG_(realreg_to_rank)()
     - ppRegsLiveness()
     - the RegsLive type (maybe -- RegsLive type must have more than
                          VG_MAX_REALREGS bits)
   
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

#include "../coregrind/vg_kerneliface.h"


/*====================================================================*/
/*=== Command-line options                                         ===*/
/*====================================================================*/

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
   VGP_PAIR(VgpStack,       "adjust-stack"),          \
   VGP_PAIR(VgpTranslate,   "translate-main"),        \
   VGP_PAIR(VgpToUCode,     "to-ucode"),              \
   VGP_PAIR(VgpFromUcode,   "from-ucode"),            \
   VGP_PAIR(VgpImprove,     "improve"),               \
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

/* Get the simulated %esp */
extern Addr VG_(get_stack_pointer) ( void );

/* Detect if an address is within Valgrind's stack or Valgrind's
   m_state_static;  useful for memory leak detectors to tell if a block
   is used by Valgrind (and thus can be ignored). */
extern Bool VG_(within_stack)(Addr a);
extern Bool VG_(within_m_state_static)(Addr a);

/* Check if an address is 4-byte aligned */
#define IS_ALIGNED4_ADDR(aaa_p) (0 == (((UInt)(aaa_p)) & 3))


/* ------------------------------------------------------------------ */
/* Thread-related stuff */

/* Special magic value for an invalid ThreadId.  It corresponds to
   LinuxThreads using zero as the initial value for
   pthread_mutex_t.__m_owner and pthread_cond_t.__c_waiting. */
#define VG_INVALID_THREADID ((ThreadId)(0))

/* ThreadIds are simply indices into the vg_threads[] array. */
typedef 
   UInt 
   ThreadId;

/* struct _ThreadState defined elsewhere;  ThreadState is abstract as its
   definition is not important for skins. */
typedef
   struct _ThreadState
   ThreadState;

extern ThreadId     VG_(get_current_tid_1_if_root) ( void );
extern ThreadState* VG_(get_ThreadState)           ( ThreadId tid );


/*====================================================================*/
/*=== Valgrind's version of libc                                   ===*/
/*====================================================================*/

/* Valgrind doesn't use libc at all, for good reasons (trust us).  So here
   are its own versions of C library functions, but with VG_ prefixes.  Note
   that the types of some are slightly different to the real ones.  Some
   extra useful functions are provided too; descriptions of how they work
   are given below. */

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
extern void VG_(printf)  ( const char *format, ... );
/* too noisy ...  __attribute__ ((format (printf, 1, 2))) ; */
extern void VG_(sprintf) ( Char* buf, Char *format, ... );
extern void VG_(vprintf) ( void(*send)(Char), 
                           const Char *format, va_list vargs );

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
extern Int  VG_(getpid) ( void );

extern Int  VG_(open)   ( const Char* pathname, Int flags, Int mode );
extern Int  VG_(read)   ( Int fd, void* buf, Int count);
extern Int  VG_(write)  ( Int fd, void* buf, Int count);
extern void VG_(close)  ( Int fd );

extern Int  VG_(unlink) ( Char* file_name );
extern Int  VG_(stat)   ( Char* file_name, struct vki_stat* buf );


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
extern void VG_(skin_assert_fail) ( Char* expr, Char* file, 
                                    Int line, Char* fn );


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

      /* Not strictly needed, but improve address calculation translations. */
      LEA1,  /* reg2 := const + reg1 */
      LEA2,  /* reg3 := const + reg1 + reg2 * 1,2,4 or 8 */

      /* Hack for x86 REP insns.  Jump to literal if TempReg/RealReg is zero. */
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
*/
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


/* Expandable arrays of uinstrs. */
typedef 
   struct { 
      Int     used; 
      Int     size; 
      UInstr* instrs;
      Int     nextTemp;
   }
   UCodeBlock;


/*====================================================================*/
/*=== Instrumenting UCode                                          ===*/
/*====================================================================*/

/* A structure for communicating TempReg and RealReg uses of UInstrs. */
typedef
   struct {
      Int   num;
      Bool  isWrite;
   }
   RegUse;

/* Find what this instruction does to its regs.  Tag indicates whether we're
 * considering TempRegs (pre-reg-alloc) or RealRegs (post-reg-alloc).
 * Useful for analysis/optimisation passes. */
extern Int  VG_(get_reg_usage) ( UInstr* u, Tag tag, RegUse* arr );


/* ------------------------------------------------------------------ */
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

extern void VG_(set_flag_RW)      ( UInstr* u, FlagSet fr, FlagSet fw );
extern void VG_(set_lit_field)    ( UCodeBlock* cb, UInt lit32 );
extern void VG_(set_ccall_fields) ( UCodeBlock* cb, Addr fn, UChar argc,
                                    UChar regparms_n, Bool has_ret_val );

extern void VG_(copy_UInstr) ( UCodeBlock* cb, UInstr* instr );

extern Bool VG_(any_flag_use)( UInstr* u );

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
 * variables... ugly, but it works. */
extern void VG_(set_global_var) ( UCodeBlock* cb, Addr globvar_ptr, UInt val);

/* ------------------------------------------------------------------ */
/* Allocating/freeing basic blocks of UCode */
extern UCodeBlock* VG_(alloc_UCodeBlock) ( void );
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
extern void  VG_(pp_UOperand)    ( UInstr* u, Int operandNo, 
                                   Int sz, Bool parens );


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
extern Char* VG_(name_of_seg_reg)  ( Int sreg );
extern Char* VG_(name_of_int_reg)  ( Int size, Int reg );
extern Char  VG_(name_of_int_size) ( Int size );

/* Randomly useful things */
extern UInt  VG_(extend_s_8to32) ( UInt x );

/* Code emitters */
extern void VG_(emitB)    ( UInt b );
extern void VG_(emitW)    ( UInt w );
extern void VG_(emitL)    ( UInt l );
extern void VG_(new_emit) ( void );

/* Finding offsets */
extern Int  VG_(helper_offset)       ( Addr a );
extern Int  VG_(shadow_reg_offset)   ( Int arch );
extern Int  VG_(shadow_flags_offset) ( void );

/* Convert reg ranks <-> Intel register ordering, for using register
   liveness information. */
extern Int VG_(realreg_to_rank) ( Int realreg );
extern Int VG_(rank_to_realreg) ( Int rank    );

/* Call a subroutine.  Does no argument passing, stack manipulations, etc. */
extern void VG_(synth_call) ( Bool ensure_shortform, Int word_offset );

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
extern void VG_(emit_nonshiftopv_lit_reg)( Int sz, Opcode opc, UInt lit,
                                           Int reg );
extern void VG_(emit_shiftopv_lit_reg)   ( Int sz, Opcode opc, UInt lit,
                                           Int reg );
extern void VG_(emit_nonshiftopv_reg_reg)( Int sz, Opcode opc,
                                           Int reg1, Int reg2 );
extern void VG_(emit_movv_lit_reg)       ( Int sz, UInt lit, Int reg );
extern void VG_(emit_unaryopv_reg)       ( Int sz, Opcode opc, Int reg );
extern void VG_(emit_pushv_reg)          ( Int sz, Int reg );
extern void VG_(emit_popv_reg)           ( Int sz, Int reg );

extern void VG_(emit_pushl_lit32)        ( UInt int32 );
extern void VG_(emit_pushl_lit8)         ( Int lit8 );
extern void VG_(emit_cmpl_zero_reg)      ( Int reg );
extern void VG_(emit_swapl_reg_EAX)      ( Int reg );
extern void VG_(emit_movv_lit_offregmem) ( Int sz, UInt lit, Int off,
                                           Int memreg );

/* b-size (1 byte) instruction emitters */
extern void VG_(emit_movb_lit_offregmem) ( UInt lit, Int off, Int memreg );
extern void VG_(emit_movb_reg_offregmem) ( Int reg, Int off, Int areg );
extern void VG_(emit_unaryopb_reg)       ( Opcode opc, Int reg );
extern void VG_(emit_testb_lit_reg)      ( UInt lit, Int reg );

/* zero-extended load emitters */
extern void VG_(emit_movzbl_offregmem_reg) ( Int off, Int regmem, Int reg );
extern void VG_(emit_movzwl_offregmem_reg) ( Int off, Int areg, Int reg );

/* misc instruction emitters */
extern void VG_(emit_call_reg)         ( Int reg );
extern void VG_(emit_add_lit_to_esp)   ( Int lit );
extern void VG_(emit_jcondshort_delta) ( Condcode cond, Int delta );
extern void VG_(emit_pushal)           ( void );
extern void VG_(emit_popal)            ( void );
extern void VG_(emit_AMD_prefetch_reg) ( Int reg );


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
   new one.  Either way, return a pointer to the context. */
extern ExeContext* VG_(get_ExeContext) ( ThreadState *tst );


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

/* An extensible (via the 'extra' field) suppression record.  This holds
   the suppression details of interest to a skin.  Skins can use a normal
   enum (with element values in the normal range (0..)) for `skind'. 

   If VG_(needs).report_errors==True, for each suppression read in by core
   SKN_(recognised_suppression)() and SKN_(read_extra_suppression_info) will
   be called.  The `skind' field is filled in by the value returned in the
   argument of the first function;  the second function can fill in the
   `string' and `extra' fields if it wants. 
*/
typedef
   struct {
      /* What kind of suppression.  Must use the range (0..) */
      SuppKind skind;
      /* String -- use is optional.  NULL by default. */
      Char* string;
      /* Anything else -- use is optional.  NULL by default. */
      void* extra;
   }
   SkinSupp;


/* ------------------------------------------------------------------ */
/* Error records contain enough info to generate an error report.  The idea
   is that (typically) the same few points in the program generate thousands
   of errors, and we don't want to spew out a fresh error message for each
   one.  Instead, we use these structures to common up duplicates.
*/

typedef
   Int         /* Do not make this unsigned! */
   ErrorKind;

/* An extensible (via the 'extra' field) error record.  This holds
   the error details of interest to a skin.  Skins can use a normal
   enum (with element values in the normal range (0..)) for `ekind'. 

   When errors are found and recorded with VG_(maybe_record_error)(), all
   the skin must do is pass in the four parameters;  core will
   allocate/initialise the error record.
*/
typedef
   struct {
      /* Used by ALL.  Must be in the range (0..) */
      Int ekind;
      /* Used frequently */
      Addr addr;
      /* Used frequently */
      Char* string;
      /* For any skin-specific extras */
      void* extra;
   }
   SkinError;


/* ------------------------------------------------------------------ */
/* Call this when an error occurs.  It will be recorded if it hasn't been
   seen before.  If it has, the existing error record will have its count
   incremented.  
   
   If the error occurs in generated code, 'tst' should be NULL.  If the
   error occurs in non-generated code, 'tst' should be non-NULL.  The
   `extra' field can be stack-allocated;  it will be copied (using
   SKN_(dup_extra_and_update)()) if needed.  But it won't be copied
   if it's NULL.

   If no 'a', 's' or 'extra' of interest needs to be recorded, just use
   NULL for them.
*/
extern void VG_(maybe_record_error) ( ThreadState* tst, ErrorKind ekind, 
                                      Addr a, Char* s, void* extra );

/* Gets a non-blank, non-comment line of at most nBuf chars from fd.
   Skips leading spaces on the line.  Returns True if EOF was hit instead. 
   Useful for reading in extra skin-specific suppression lines.
*/
extern Bool VG_(get_line) ( Int fd, Char* buf, Int nBuf );


/*====================================================================*/
/*=== Obtaining debug information                                  ===*/
/*====================================================================*/

/* Get the file/function/line number of the instruction at address 'a'. 
   For these four, if debug info for the address is found, it copies the
   info into the buffer/UInt and returns True.  If not, it returns False and
   nothing is copied.  VG_(get_fnname) always demangles C++ function names.
*/
extern Bool VG_(get_filename) ( Addr a, Char* filename, Int n_filename );
extern Bool VG_(get_fnname)   ( Addr a, Char* fnname,   Int n_fnname   );
extern Bool VG_(get_linenum)  ( Addr a, UInt* linenum );

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
extern Bool VG_(get_fnname_if_entry) ( Addr a, Char* filename, Int n_filename );

/* Succeeds if the address is within a shared object or the main executable.
   It doesn't matter if debug info is present or not. */
extern Bool VG_(get_objname)  ( Addr a, Char* objname,  Int n_objname  );


/*====================================================================*/
/*=== Shadow chunks and block-finding                              ===*/
/*====================================================================*/

typedef
   enum { 
      Vg_AllocMalloc = 0,
      Vg_AllocNew    = 1,
      Vg_AllocNewVec = 2 
   }
   VgAllocKind;

/* Description of a malloc'd chunk.  skin_extra[] part can be used by
   the skin;  size of array is given by VG_(needs).sizeof_shadow_chunk. */
typedef 
   struct _ShadowChunk {
      struct _ShadowChunk* next;
      UInt          size : 30;      /* size requested                   */
      VgAllocKind   allockind : 2;  /* which wrapper did the allocation */
      Addr          data;           /* ptr to actual block              */
      UInt          skin_extra[0];  /* extra skin-specific info         */
   } 
   ShadowChunk;

/* Use this to free blocks if VG_(needs).alternative_free == True. 
   It frees the ShadowChunk and the malloc'd block it points to. */
extern void VG_(free_ShadowChunk) ( ShadowChunk* sc );

/* Makes an array of pointers to all the shadow chunks of malloc'd blocks */
extern ShadowChunk** VG_(get_malloc_shadows) ( /*OUT*/ UInt* n_shadows );

/* Determines if address 'a' is within the bounds of the block at start.
   Allows a little 'slop' round the edges. */
extern Bool VG_(addr_is_in_block) ( Addr a, Addr start, UInt size );

/* Searches through currently malloc'd blocks until a matching one is found.
   Returns NULL if none match.  Extra arguments can be implicitly passed to
   p using nested functions; see memcheck/mc_errcontext.c for an example. */
extern ShadowChunk* VG_(any_matching_mallocd_ShadowChunks) 
                        ( Bool (*p) ( ShadowChunk* ));

/* Searches through all thread's stacks to see if any match.  Returns
 * VG_INVALID_THREADID if none match. */
extern ThreadId VG_(any_matching_thread_stack)
                        ( Bool (*p) ( Addr stack_min, Addr stack_max ));

/*====================================================================*/
/*=== Skin-specific stuff                                          ===*/
/*====================================================================*/

/* ------------------------------------------------------------------ */
/* Details */
typedef
   struct {
      /* Information used in the startup message. `name' also determines the
         string used for identifying suppressions in a suppression file as
         belonging to this skin.  `version' can be NULL, in which case (not
         surprisingly) no version info is printed; this mechanism is
         designed for skins distributed with Valgrind that share a version
         number with Valgrind.  Other skins not distributed as part of
         Valgrind should probably have their own version number. */
      Char* name;
      Char* version;
      Char* description;
      Char* copyright_author;

      /* String printed if an `sk_assert' assertion fails or VG_(skin_panic)
         is called.  Should probably be an email address. */
      Char* bug_reports_to;
   }
   VgDetails;

extern VgDetails VG_(details);

/* ------------------------------------------------------------------ */
/* Needs */

/* If new fields are added to this type, update:
 *  - vg_main.c:initialisation of VG_(needs)
 *  - vg_main.c:sanity_check_needs()
 *
 * If the name of this type or any of its fields change, update:
 *  - dependent comments (just search for "VG_(needs)"). 
 */
typedef
   struct {
      /* Booleans that decide core behaviour, but don't require extra
         operations to be defined if `True' */

      /* Should __libc_freeres() be run?  Bugs in it can crash the skin. */
      Bool libc_freeres;

      /* Want to have errors detected by Valgrind's core reported?  Includes:
         - pthread API errors (many;  eg. unlocking a non-locked mutex)
         - silly arguments to malloc() et al (eg. negative size)
         - invalid file descriptors to blocking syscalls read() and write()
         - bad signal numbers passed to sigaction()
         - attempt to install signal handler for SIGKILL or SIGSTOP */  
      Bool core_errors;

      /* Booleans that indicate extra operations are defined;  if these are
         True, the corresponding template functions (given below) must be
         defined.  A lot like being a member of a type class. */

      /* Want to report errors from the skin?  This implies use of
         suppressions, too. */
      Bool skin_errors;

      /* Is information kept about specific individual basic blocks?  (Eg. for
         cachegrind there are cost-centres for every instruction, stored at a
         basic block level.)  If so, it sometimes has to be discarded, because
         .so mmap/munmap-ping or self-modifying code (informed by the
         DISCARD_TRANSLATIONS user request) can cause one instruction address
         to be used for more than one instruction in one program run...  */
      Bool basic_block_discards;

      /* Skin maintains information about each register? */
      Bool shadow_regs;

      /* Skin defines its own command line options? */
      Bool command_line_options;
      /* Skin defines its own client requests? */
      Bool client_requests;

      /* Skin defines its own UInstrs? */
      Bool extended_UCode;

      /* Skin does stuff before and/or after system calls? */
      Bool syscall_wrapper;

      /* Size, in words, of extra info about malloc'd blocks recorded by
         skin.  Be careful to get this right or you'll get seg faults! */
      UInt sizeof_shadow_block;

      /* Skin does free()s itself? */
      Bool alternative_free;

      /* Are skin-state sanity checks performed? */
      Bool sanity_checks;
   } 
   VgNeeds;

extern VgNeeds VG_(needs);


/* ------------------------------------------------------------------ */
/* Core events to track */

/* Part of the core from which this call was made.  Useful for determining
   what kind of error message should be emitted. */
typedef 
   enum { Vg_CorePThread, Vg_CoreSignal, Vg_CoreSysCall, Vg_CoreTranslate }
   CorePart;

/* Events happening in core to track.  To be notified, assign a function
   to the function pointer.  To ignore an event, don't do anything
   (default assignment is to NULL in which case the call is skipped). */
typedef
   struct {
      /* Memory events */
      void (*new_mem_startup)( Addr a, UInt len, Bool rr, Bool ww, Bool xx );
      void (*new_mem_heap)   ( Addr a, UInt len, Bool is_inited );
      void (*new_mem_stack)  ( Addr a, UInt len );
      void (*new_mem_stack_aligned) ( Addr a, UInt len );
      void (*new_mem_stack_signal)  ( Addr a, UInt len );
      void (*new_mem_brk)    ( Addr a, UInt len );
      void (*new_mem_mmap)   ( Addr a, UInt len, 
                               Bool nn, Bool rr, Bool ww, Bool xx );

      void (*copy_mem_heap)  ( Addr from, Addr to, UInt len );
      void (*copy_mem_remap) ( Addr from, Addr to, UInt len );
      void (*change_mem_mprotect) ( Addr a, UInt len,  
                                    Bool nn, Bool rr, Bool ww, Bool xx );
      
      /* Used on redzones around malloc'd blocks and at end of stack */
      void (*ban_mem_heap)   ( Addr a, UInt len );
      void (*ban_mem_stack)  ( Addr a, UInt len );

      void (*die_mem_heap)   ( Addr a, UInt len );
      void (*die_mem_stack)  ( Addr a, UInt len );
      void (*die_mem_stack_aligned) ( Addr a, UInt len );
      void (*die_mem_stack_signal)  ( Addr a, UInt len );
      void (*die_mem_brk)    ( Addr a, UInt len );
      void (*die_mem_munmap) ( Addr a, UInt len );

      void (*bad_free)        ( ThreadState* tst, Addr a );
      void (*mismatched_free) ( ThreadState* tst, Addr a );

      void (*pre_mem_read)   ( CorePart part, ThreadState* tst,
                               Char* s, Addr a, UInt size );
      void (*pre_mem_read_asciiz) ( CorePart part, ThreadState* tst,
                                    Char* s, Addr a );
      void (*pre_mem_write)  ( CorePart part, ThreadState* tst,
                               Char* s, Addr a, UInt size );
      /* Not implemented yet -- have to add in lots of places, which is a
         pain.  Won't bother unless/until there's a need. */
      /* void (*post_mem_read)  ( ThreadState* tst, Char* s, 
                                  Addr a, UInt size ); */
      void (*post_mem_write) ( Addr a, UInt size );


      /* Scheduler events (not exhaustive) */
      void (*thread_run) ( ThreadId tid );


      /* Mutex events (not exhaustive) */
      void (*post_mutex_lock)   ( ThreadId tid, 
                                  void* /*pthread_mutex_t* */ mutex );
      void (*post_mutex_unlock) ( ThreadId tid, 
                                  void* /*pthread_mutex_t* */ mutex );

      
      /* Others... thread, condition variable, signal events... */
      /* ... */
   }
   VgTrackEvents;

/* Declare the struct instance */
extern VgTrackEvents VG_(track_events);


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
     - initialise the `details' struct
     - register any helpers called by generated code
  
   May do the following:
     - initialise the `needs' struct to indicate certain requirements
     - initialise the `track' struct to indicate core events of interest
     - register any skin-specific profiling events
     - any other skin-specific initialisation
*/
extern void        SK_(pre_clo_init) ( VgDetails* details, VgNeeds* needs,
                                       VgTrackEvents* track );

/* Do initialisation that can only be done after command line processing. */
extern void        SK_(post_clo_init)( void );

/* Instrument a basic block.  Must be a true function, ie. the same input
   always results in the same output, because basic blocks can be
   retranslated.  Unless you're doing something really strange...
   'orig_addr' is the address of the first instruction in the block. */
extern UCodeBlock* SK_(instrument)   ( UCodeBlock* cb, Addr orig_addr );

/* Finish up, print out any results, etc. */
extern void        SK_(fini)         ( void );


/* ------------------------------------------------------------------ */
/* VG_(needs).report_errors */

/* Identify if two errors are equal, or equal enough.  `res' indicates how
   close is "close enough".  `res' should be passed on as necessary, eg. if
   the SkinError's extra field contains an ExeContext, `res' should be
   passed to VG_(eq_ExeContext)() if the ExeContexts are considered.  Other
   than that, probably don't worry about it unless you have lots of very
   similar errors occurring.
 */
extern Bool SK_(eq_SkinError) ( VgRes res,
                                SkinError* e1, SkinError* e2 );

/* Print error context.  The passed function pp_ExeContext() can be (and
   probably should be) used to print the location of the error. */
extern void SK_(pp_SkinError) ( SkinError* ec, void (*pp_ExeContext)(void) );

/* Copy the ec->extra part and replace ec->extra with the new copy.  This is
   necessary to move from a temporary stack copy to a permanent heap one.
  
   Then fill in any details that could be postponed until after the decision
   whether to ignore the error (ie. details not affecting the result of
   SK_(eq_SkinError)()).  This saves time when errors are ignored.
  
   Yuk.
*/
extern void SK_(dup_extra_and_update)(SkinError* ec);

/* Return value indicates recognition.  If recognised, type goes in `skind'. */
extern Bool SK_(recognised_suppression) ( Char* name, SuppKind *skind );

/* Read any extra info for this suppression kind.  For filling up the
   `string' and `extra' fields in a `SkinSupp' struct if necessary. */
extern Bool SK_(read_extra_suppression_info) ( Int fd, Char* buf, 
                                               Int nBuf, SkinSupp *s );

/* This should just check the kinds match and maybe some stuff in the
   'extra' field if appropriate */
extern Bool SK_(error_matches_suppression)(SkinError* ec, SkinSupp* su);


/* ------------------------------------------------------------------ */
/* VG_(needs).basic_block_discards */

/* Should discard any information that pertains to specific basic blocks
   or instructions within the address range given. */
extern void SK_(discard_basic_block_info) ( Addr a, UInt size );


/* ------------------------------------------------------------------ */
/* VG_(needs).shadow_regs */

/* Valid values for general registers and EFLAGS register, for initialising
   and updating registers when written in certain places in core. */
extern void SK_(written_shadow_regs_values) ( UInt* gen_reg, UInt* eflags );


/* ------------------------------------------------------------------ */
/* VG_(needs).command_line_options */

/* Return True if option was recognised.  Presumably sets some state to
   record the option as well. */
extern Bool SK_(process_cmd_line_option) ( Char* argv );

/* Print out command line usage for skin options */
extern Char* SK_(usage)                  ( void );


/* ------------------------------------------------------------------ */
/* VG_(needs).client_requests */

extern UInt SK_(handle_client_request) ( ThreadState* tst, UInt* arg_block );


/* ------------------------------------------------------------------ */
/* VG_(needs).extends_UCode */

/* Useful to use in VG_(get_Xreg_usage)() */
#define VG_UINSTR_READS_REG(ono)                \
   { if (mycat(u->tag,ono) == tag)              \
        { arr[n].num     = mycat(u->val,ono);   \
          arr[n].isWrite = False;               \
          n++;                                  \
        }                                       \
   }
#define VG_UINSTR_WRITES_REG(ono)               \
   { if (mycat(u->tag,ono) == tag)              \
        { arr[n].num     = mycat(u->val,ono);   \
          arr[n].isWrite = True;                \
          n++;                                  \
        }                                       \
   }

/* 'X' prefix indicates eXtended UCode. */
extern Int   SK_(get_Xreg_usage) ( UInstr* u, Tag tag, RegUse* arr );
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


/* ------------------------------------------------------------------ */
/* VG_(needs).sizeof_shadow_chunk (if > 0) */

extern void SK_(complete_shadow_chunk) ( ShadowChunk* sc, ThreadState* tst );


/* ------------------------------------------------------------------ */
/* VG_(needs).alternative_free */

extern void SK_(alt_free) ( ShadowChunk* sc, ThreadState* tst );


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

