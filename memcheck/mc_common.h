
/*--------------------------------------------------------------------*/
/*--- Code that is shared between MemCheck and AddrCheck.          ---*/
/*---                                                  mc_common.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of MemCheck, a heavyweight Valgrind skin for
   detecting memory errors, and AddrCheck, a lightweight Valgrind skin 
   for detecting memory errors.

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

#ifndef __MC_COMMON_H
#define __MC_COMMON_H

#include "vg_skin.h"
#include "mc_constants.h"

/*------------------------------------------------------------*/
/*--- Errors and suppressions                              ---*/
/*------------------------------------------------------------*/

/* The classification of a faulting address. */
typedef 
   enum { Undescribed,  /* as-yet unclassified */
          Stack, 
          Unknown,      /* classification yielded nothing useful */
          Freed, Mallocd, 
          UserG  /* in a user-defined block; Addrcheck & Memcheck only */
   }
   AddrKind;

/* Records info about a faulting address. */
typedef
   struct {
      /* ALL */
      AddrKind akind;
      /* Freed, Mallocd */
      Int blksize;
      /* Freed, Mallocd */
      Int rwoffset;
      /* Freed, Mallocd */
      ExeContext* lastchange;
      /* Stack */
      ThreadId stack_tid;
      /* True if is just-below %esp -- could be a gcc bug. */
      Bool maybe_gcc;
   }
   AddrInfo;

typedef 
   enum { 
      /* Bad syscall params */
      ParamSupp,
      /* Memory errors in core (pthread ops, signal handling) */
      CoreMemSupp,
      /* Use of invalid values of given size (MemCheck only) */
      Value0Supp, Value1Supp, Value2Supp, Value4Supp, Value8Supp, 
      /* Invalid read/write attempt at given size */
      Addr1Supp, Addr2Supp, Addr4Supp, Addr8Supp,
      /* Invalid or mismatching free */
      FreeSupp,
      /* Something to be suppressed in a leak check. */
      LeakSupp
   } 
   MemCheckSuppKind;

/* What kind of error it is. */
typedef 
   enum { ValueErr,     /* Memcheck only */
          CoreMemErr,
          AddrErr, 
          ParamErr, UserErr,  /* behaves like an anonymous ParamErr */
          FreeErr, FreeMismatchErr
   }
   MemCheckErrorKind;

/* What kind of memory access is involved in the error? */
typedef
   enum { ReadAxs, WriteAxs, ExecAxs }
   AxsKind;

/* Extra context for memory errors */
typedef
   struct {
      /* AddrErr */
      AxsKind axskind;
      /* AddrErr, ValueErr */
      Int size;
      /* AddrErr, FreeErr, FreeMismatchErr, ParamErr, UserErr */
      AddrInfo addrinfo;
      /* ParamErr, UserErr, CoreMemErr */
      Bool isWrite;
   }
   MemCheckError;

/*------------------------------------------------------------*/
/*--- Profiling of skins and memory events                 ---*/
/*------------------------------------------------------------*/

typedef 
   enum { 
      VgpCheckMem = VgpFini+1,
      VgpSetMem,
      VgpESPAdj
   } 
   VgpSkinCC;

/* Define to collect detailed performance info. */
/* #define VG_PROFILE_MEMORY */

#ifdef VG_PROFILE_MEMORY
#  define N_PROF_EVENTS 150

extern UInt MC_(event_ctr)[N_PROF_EVENTS];

#  define PROF_EVENT(ev)                                  \
   do { sk_assert((ev) >= 0 && (ev) < N_PROF_EVENTS);   \
        MC_(event_ctr)[ev]++;                           \
   } while (False);

#else

#  define PROF_EVENT(ev) /* */

#endif   /* VG_PROFILE_MEMORY */

/*------------------------------------------------------------*/
/*--- V and A bits                                         ---*/
/*------------------------------------------------------------*/

#define IS_DISTINGUISHED_SM(smap) \
   ((smap) == &distinguished_secondary_map)

#define ENSURE_MAPPABLE(addr,caller)                                   \
   do {                                                                \
      if (IS_DISTINGUISHED_SM(primary_map[(addr) >> 16])) {       \
         primary_map[(addr) >> 16] = alloc_secondary_map(caller); \
         /* VG_(printf)("new 2map because of %p\n", addr); */          \
      }                                                                \
   } while(0)

#define BITARR_SET(aaa_p,iii_p)                         \
   do {                                                 \
      UInt   iii = (UInt)iii_p;                         \
      UChar* aaa = (UChar*)aaa_p;                       \
      aaa[iii >> 3] |= (1 << (iii & 7));                \
   } while (0)

#define BITARR_CLEAR(aaa_p,iii_p)                       \
   do {                                                 \
      UInt   iii = (UInt)iii_p;                         \
      UChar* aaa = (UChar*)aaa_p;                       \
      aaa[iii >> 3] &= ~(1 << (iii & 7));               \
   } while (0)

#define BITARR_TEST(aaa_p,iii_p)                        \
      (0 != (((UChar*)aaa_p)[ ((UInt)iii_p) >> 3 ]      \
               & (1 << (((UInt)iii_p) & 7))))           \


#define VGM_BIT_VALID      0
#define VGM_BIT_INVALID    1

#define VGM_NIBBLE_VALID   0
#define VGM_NIBBLE_INVALID 0xF

#define VGM_BYTE_VALID     0
#define VGM_BYTE_INVALID   0xFF

#define VGM_WORD_VALID     0
#define VGM_WORD_INVALID   0xFFFFFFFF

#define VGM_EFLAGS_VALID   0xFFFFFFFE
#define VGM_EFLAGS_INVALID 0xFFFFFFFF     /* not used */

/*------------------------------------------------------------*/
/*--- Command line options + defaults                      ---*/
/*------------------------------------------------------------*/

/* Most are shared between MemCheck and AddrCheck, the last two are
   MemCheck only (but here anyway for simplicity) */

/* Allow loads from partially-valid addresses?  default: YES */
extern Bool MC_(clo_partial_loads_ok);

/* Max volume of the freed blocks queue. */
extern Int MC_(clo_freelist_vol);

/* Do leak check at exit?  default: NO */
extern Bool MC_(clo_leak_check);

/* How closely should we compare ExeContexts in leak records? default: 2 */
extern VgRes MC_(clo_leak_resolution);

/* In leak check, show reachable-but-not-freed blocks?  default: NO */
extern Bool MC_(clo_show_reachable);

/* Assume accesses immediately below %esp are due to gcc-2.96 bugs.
 * default: NO*/
extern Bool MC_(clo_workaround_gcc296_bugs);

/* DEBUG: clean up instrumented code?  default: YES */
extern Bool MC_(clo_cleanup);

/* When instrumenting, omit some checks if tell-tale literals for
   inlined strlen() are visible in the basic block.  default: YES */
extern Bool MC_(clo_avoid_strlen_errors);

extern Bool MC_(process_common_cmd_line_option)(Char* arg);


/*------------------------------------------------------------*/
/*--- Functions                                            ---*/
/*------------------------------------------------------------*/

extern void        MC_(set_where) ( ShadowChunk* sc, ExeContext* ec );
extern ExeContext *MC_(get_where) ( ShadowChunk* sc );

extern void MC_(pp_AddrInfo) ( Addr a, AddrInfo* ai );

extern void MC_(clear_MemCheckError) ( MemCheckError* err_extra );

extern void MC_(record_address_error)     ( Addr a, Int size, Bool isWrite );
extern void MC_(record_core_mem_error)    ( ThreadState* tst, Bool isWrite,
                                            Char* s );
extern void MC_(record_param_error)       ( ThreadState* tst, Addr a,   
                                            Bool isWriteLack, Char* msg );
extern void MC_(record_jump_error)        ( ThreadState* tst, Addr a );
extern void MC_(record_free_error)        ( ThreadState* tst, Addr a );
extern void MC_(record_freemismatch_error)( ThreadState* tst, Addr a );

extern void MC_(init_prof_mem) ( void );
extern void MC_(done_prof_mem) ( void );

extern Int          MC_(count_freelist)  ( void ) __attribute__ ((unused));
extern void         MC_(freelist_sanity) ( void ) __attribute__ ((unused));
extern ShadowChunk* MC_(any_matching_freed_ShadowChunks) 
                           ( Bool (*p)(ShadowChunk*) );

extern __attribute__((regparm(1))) void MC_(new_mem_stack_4)  ( Addr old_ESP );
extern __attribute__((regparm(1))) void MC_(die_mem_stack_4)  ( Addr old_ESP );
extern __attribute__((regparm(1))) void MC_(new_mem_stack_8)  ( Addr old_ESP );
extern __attribute__((regparm(1))) void MC_(die_mem_stack_8)  ( Addr old_ESP );
extern __attribute__((regparm(1))) void MC_(new_mem_stack_12) ( Addr old_ESP );
extern __attribute__((regparm(1))) void MC_(die_mem_stack_12) ( Addr old_ESP );
extern __attribute__((regparm(1))) void MC_(new_mem_stack_16) ( Addr old_ESP );
extern __attribute__((regparm(1))) void MC_(die_mem_stack_16) ( Addr old_ESP );
extern __attribute__((regparm(1))) void MC_(new_mem_stack_32) ( Addr old_ESP );
extern __attribute__((regparm(1))) void MC_(die_mem_stack_32) ( Addr old_ESP );
extern                             void MC_(die_mem_stack) ( Addr a, UInt len );
extern                             void MC_(new_mem_stack) ( Addr a, UInt len );


/*------------------------------------------------------------*/
/*--- Stack pointer adjustment                             ---*/
/*------------------------------------------------------------*/

/* Some noble preprocessor abuse, to enable Memcheck and Addrcheck to
   share this code, but not call the same functions.

   Note that this code is executed very frequently and must be highly
   optimised, which is why I resort to the preprocessor to achieve the
   factoring, rather than eg. using function pointers.
*/

#define ESP_UPDATE_HANDLERS(ALIGNED4_NEW,  ALIGNED4_DIE,                      \
                            ALIGNED8_NEW,  ALIGNED8_DIE,                      \
                            UNALIGNED_NEW, UNALIGNED_DIE)                     \
                                                                              \
void __attribute__((regparm(1))) MC_(new_mem_stack_4)(Addr new_ESP)           \
{                                                                             \
   PROF_EVENT(110);                                                           \
   if (IS_ALIGNED4_ADDR(new_ESP)) {                                           \
      ALIGNED4_NEW  ( new_ESP );                                              \
   } else {                                                                   \
      UNALIGNED_NEW ( new_ESP, 4 );                                           \
   }                                                                          \
}                                                                             \
                                                                              \
void __attribute__((regparm(1))) MC_(die_mem_stack_4)(Addr new_ESP)           \
{                                                                             \
   PROF_EVENT(120);                                                           \
   if (IS_ALIGNED4_ADDR(new_ESP)) {                                           \
      ALIGNED4_DIE  ( new_ESP-4 );                                            \
   } else {                                                                   \
      UNALIGNED_DIE ( new_ESP-4, 4 );                                         \
   }                                                                          \
}                                                                             \
                                                                              \
void __attribute__((regparm(1))) MC_(new_mem_stack_8)(Addr new_ESP)           \
{                                                                             \
   PROF_EVENT(111);                                                           \
   if (IS_ALIGNED8_ADDR(new_ESP)) {                                           \
      ALIGNED8_NEW  ( new_ESP );                                              \
   } else if (IS_ALIGNED4_ADDR(new_ESP)) {                                    \
      ALIGNED4_NEW  ( new_ESP   );                                            \
      ALIGNED4_NEW  ( new_ESP+4 );                                            \
   } else {                                                                   \
      UNALIGNED_NEW ( new_ESP, 8 );                                           \
   }                                                                          \
}                                                                             \
                                                                              \
void __attribute__((regparm(1))) MC_(die_mem_stack_8)(Addr new_ESP)           \
{                                                                             \
   PROF_EVENT(121);                                                           \
   if (IS_ALIGNED8_ADDR(new_ESP)) {                                           \
      ALIGNED8_DIE  ( new_ESP-8 );                                            \
   } else if (IS_ALIGNED4_ADDR(new_ESP)) {                                    \
      ALIGNED4_DIE  ( new_ESP-8 );                                            \
      ALIGNED4_DIE  ( new_ESP-4 );                                            \
   } else {                                                                   \
      UNALIGNED_DIE ( new_ESP-8, 8 );                                         \
   }                                                                          \
}                                                                             \
                                                                              \
void __attribute__((regparm(1))) MC_(new_mem_stack_12)(Addr new_ESP)          \
{                                                                             \
   PROF_EVENT(112);                                                           \
   if (IS_ALIGNED8_ADDR(new_ESP)) {                                           \
      ALIGNED8_NEW  ( new_ESP   );                                            \
      ALIGNED4_NEW  ( new_ESP+8 );                                            \
   } else if (IS_ALIGNED4_ADDR(new_ESP)) {                                    \
      ALIGNED4_NEW  ( new_ESP   );                                            \
      ALIGNED8_NEW  ( new_ESP+4 );                                            \
   } else {                                                                   \
      UNALIGNED_NEW ( new_ESP, 12 );                                          \
   }                                                                          \
}                                                                             \
                                                                              \
void __attribute__((regparm(1))) MC_(die_mem_stack_12)(Addr new_ESP)          \
{                                                                             \
   PROF_EVENT(122);                                                           \
   /* Note the -12 in the test */                                             \
   if (IS_ALIGNED8_ADDR(new_ESP-12)) {                                        \
      ALIGNED8_DIE  ( new_ESP-12 );                                           \
      ALIGNED4_DIE  ( new_ESP-4  );                                           \
   } else if (IS_ALIGNED4_ADDR(new_ESP)) {                                    \
      ALIGNED4_DIE  ( new_ESP-12 );                                           \
      ALIGNED8_DIE  ( new_ESP-8  );                                           \
   } else {                                                                   \
      UNALIGNED_DIE ( new_ESP-12, 12 );                                       \
   }                                                                          \
}                                                                             \
                                                                              \
void __attribute__((regparm(1))) MC_(new_mem_stack_16)(Addr new_ESP)          \
{                                                                             \
   PROF_EVENT(113);                                                           \
   if (IS_ALIGNED8_ADDR(new_ESP)) {                                           \
      ALIGNED8_NEW  ( new_ESP   );                                            \
      ALIGNED8_NEW  ( new_ESP+8 );                                            \
   } else if (IS_ALIGNED4_ADDR(new_ESP)) {                                    \
      ALIGNED4_NEW  ( new_ESP    );                                           \
      ALIGNED8_NEW  ( new_ESP+4  );                                           \
      ALIGNED4_NEW  ( new_ESP+12 );                                           \
   } else {                                                                   \
      UNALIGNED_NEW ( new_ESP, 16 );                                          \
   }                                                                          \
}                                                                             \
                                                                              \
void __attribute__((regparm(1))) MC_(die_mem_stack_16)(Addr new_ESP)          \
{                                                                             \
   PROF_EVENT(123);                                                           \
   if (IS_ALIGNED8_ADDR(new_ESP)) {                                           \
      ALIGNED8_DIE  ( new_ESP-16 );                                           \
      ALIGNED8_DIE  ( new_ESP-8  );                                           \
   } else if (IS_ALIGNED4_ADDR(new_ESP)) {                                    \
      ALIGNED4_DIE  ( new_ESP-16 );                                           \
      ALIGNED8_DIE  ( new_ESP-12 );                                           \
      ALIGNED4_DIE  ( new_ESP-4  );                                           \
   } else {                                                                   \
      UNALIGNED_DIE ( new_ESP-16, 16 );                                       \
   }                                                                          \
}                                                                             \
                                                                              \
void __attribute__((regparm(1))) MC_(new_mem_stack_32)(Addr new_ESP)          \
{                                                                             \
   PROF_EVENT(114);                                                           \
   if (IS_ALIGNED8_ADDR(new_ESP)) {                                           \
      ALIGNED8_NEW  ( new_ESP    );                                           \
      ALIGNED8_NEW  ( new_ESP+8  );                                           \
      ALIGNED8_NEW  ( new_ESP+16 );                                           \
      ALIGNED8_NEW  ( new_ESP+24 );                                           \
   } else if (IS_ALIGNED4_ADDR(new_ESP)) {                                    \
      ALIGNED4_NEW  ( new_ESP    );                                           \
      ALIGNED8_NEW  ( new_ESP+4  );                                           \
      ALIGNED8_NEW  ( new_ESP+12 );                                           \
      ALIGNED8_NEW  ( new_ESP+20 );                                           \
      ALIGNED4_NEW  ( new_ESP+28 );                                           \
   } else {                                                                   \
      UNALIGNED_NEW ( new_ESP, 32 );                                          \
   }                                                                          \
}                                                                             \
                                                                              \
void __attribute__((regparm(1))) MC_(die_mem_stack_32)(Addr new_ESP)          \
{                                                                             \
   PROF_EVENT(124);                                                           \
   if (IS_ALIGNED8_ADDR(new_ESP)) {                                           \
      ALIGNED8_DIE  ( new_ESP-32 );                                           \
      ALIGNED8_DIE  ( new_ESP-24 );                                           \
      ALIGNED8_DIE  ( new_ESP-16 );                                           \
      ALIGNED8_DIE  ( new_ESP- 8 );                                           \
   } else if (IS_ALIGNED4_ADDR(new_ESP)) {                                    \
      ALIGNED4_DIE  ( new_ESP-32 );                                           \
      ALIGNED8_DIE  ( new_ESP-28 );                                           \
      ALIGNED8_DIE  ( new_ESP-20 );                                           \
      ALIGNED8_DIE  ( new_ESP-12 );                                           \
      ALIGNED4_DIE  ( new_ESP-4  );                                           \
   } else {                                                                   \
      UNALIGNED_DIE ( new_ESP-32, 32 );                                       \
   }                                                                          \
}                                                                             \
                                                                              \
void MC_(new_mem_stack) ( Addr a, UInt len )                                  \
{                                                                             \
   PROF_EVENT(115);                                                           \
   UNALIGNED_NEW ( a, len );                                                  \
}                                                                             \
                                                                              \
void MC_(die_mem_stack) ( Addr a, UInt len )                                  \
{                                                                             \
   PROF_EVENT(125);                                                           \
   UNALIGNED_DIE ( a, len );                                                  \
}

#endif   /* __MC_COMMON_H */

/*--------------------------------------------------------------------*/
/*--- end                                              mc_common.h ---*/
/*--------------------------------------------------------------------*/
