
/*--------------------------------------------------------------------*/
/*--- Declarations shared between MemCheck and AddrCheck.          ---*/
/*---                                                 mac_shared.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of MemCheck, a heavyweight Valgrind tool for
   detecting memory errors, and AddrCheck, a lightweight Valgrind tool 
   for detecting memory errors.

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

/* Note: This header contains the declarations shared between
   Addrcheck and Memcheck, and is #included by both. */

#ifndef __MAC_SHARED_H
#define __MAC_SHARED_H

#include "tool.h"

#define MAC_(str)    VGAPPEND(vgMAC_,str)

/*------------------------------------------------------------*/
/*--- Errors and suppressions                              ---*/
/*------------------------------------------------------------*/

/* The classification of a faulting address. */
typedef 
   enum { 
      Undescribed,   // as-yet unclassified
      Stack, 
      Unknown,       // classification yielded nothing useful
      Freed, Mallocd, 
      UserG,         // in a user-defined block
      Mempool,       // in a mempool
      Register,      // in a register;  for Param errors only
   }
   AddrKind;

/* Records info about a faulting address. */
typedef
   struct {                   // Used by:
      AddrKind akind;         //   ALL
      SizeT blksize;          //   Freed, Mallocd
      OffT rwoffset;          //   Freed, Mallocd
      ExeContext* lastchange; //   Freed, Mallocd
      ThreadId stack_tid;     //   Stack
      const Char *desc;	      //   UserG
      Bool maybe_gcc;         // True if just below %esp -- could be a gcc bug.
   }
   AddrInfo;

typedef 
   enum { 
      ParamSupp,     // Bad syscall params
      CoreMemSupp,   // Memory errors in core (pthread ops, signal handling)

      // Use of invalid values of given size (MemCheck only)
      Value0Supp, Value1Supp, Value2Supp, Value4Supp, Value8Supp, Value16Supp,

      // Invalid read/write attempt at given size
      Addr1Supp, Addr2Supp, Addr4Supp, Addr8Supp, Addr16Supp,

      FreeSupp,      // Invalid or mismatching free
      OverlapSupp,   // Overlapping blocks in memcpy(), strcpy(), etc
      LeakSupp,      // Something to be suppressed in a leak check.
      MempoolSupp,   // Memory pool suppression.
   } 
   MAC_SuppKind;

/* What kind of error it is. */
typedef 
   enum { ValueErr,     /* Memcheck only */
          CoreMemErr,
          AddrErr, 
          ParamErr, UserErr,  /* behaves like an anonymous ParamErr */
          FreeErr, FreeMismatchErr,
          OverlapErr,
          LeakErr,
          IllegalMempoolErr,
   }
   MAC_ErrorKind;

/* What kind of memory access is involved in the error? */
typedef
   enum { ReadAxs, WriteAxs, ExecAxs }
   AxsKind;

/* Extra context for memory errors */
typedef
   struct {                // Used by:
      AxsKind axskind;     //   AddrErr
      Int size;            //   AddrErr, ValueErr
      AddrInfo addrinfo;   //   {Addr,Free,FreeMismatch,Param,User}Err
      Bool isUnaddr;       //   {CoreMem,Param,User}Err
   }
   MAC_Error;

/* Extra info for overlap errors */
typedef
   struct {
      Addr src;
      Addr dst;
      Int  len;   // -1 if unused
   }
   OverlapExtra;

/* For malloc()/new/new[] vs. free()/delete/delete[] mismatch checking. */
typedef
   enum {
      MAC_AllocMalloc = 0,
      MAC_AllocNew    = 1,
      MAC_AllocNewVec = 2,
      MAC_AllocCustom = 3
   }
   MAC_AllocKind;
   
/* Nb: first two fields must match core's VgHashNode. */
typedef
   struct _MAC_Chunk {
      struct _MAC_Chunk* next;
      Addr          data;           // ptr to actual block
      SizeT         size : (sizeof(UWord)*8)-2; // size requested; 30 or 62 bits
      MAC_AllocKind allockind : 2;  // which wrapper did the allocation
      ExeContext*   where;          // where it was allocated
   }
   MAC_Chunk;

/* Memory pool.  Nb: first two fields must match core's VgHashNode. */
typedef
   struct _MAC_Mempool {
      struct _MAC_Mempool* next;
      Addr          pool;           // pool identifier
      SizeT         rzB;            // pool red-zone size
      Bool          is_zeroed;      // allocations from this pool are zeroed
      VgHashTable   chunks;         // chunks associated with this pool
   }
   MAC_Mempool;


/*------------------------------------------------------------*/
/*--- Profiling of tools and memory events                 ---*/
/*------------------------------------------------------------*/

typedef 
   enum { 
      VgpCheckMem = VgpFini+1,
      VgpSetMem,
      VgpESPAdj
   } 
   VgpToolCC;

/* Define to collect detailed performance info. */
/* #define MAC_PROFILE_MEMORY */

#ifdef MAC_PROFILE_MEMORY
#  define N_PROF_EVENTS 150

extern UInt MAC_(event_ctr)[N_PROF_EVENTS];

#  define PROF_EVENT(ev)                                 \
   do { tl_assert((ev) >= 0 && (ev) < N_PROF_EVENTS);    \
        MAC_(event_ctr)[ev]++;                           \
   } while (False);

#else

#  define PROF_EVENT(ev) /* */

#endif   /* MAC_PROFILE_MEMORY */

/*------------------------------------------------------------*/
/*--- V and A bits                                         ---*/
/*------------------------------------------------------------*/

/* expand 1 bit -> 8 */
#define BIT_EXPAND(b)	((~(((UChar)(b) & 1) - 1)) & 0xFF)

#define SECONDARY_SHIFT	16
#define SECONDARY_SIZE	(1 << SECONDARY_SHIFT)
#define SECONDARY_MASK	(SECONDARY_SIZE - 1)

#define PRIMARY_SIZE	(1 << (32 - SECONDARY_SHIFT))

#define SM_OFF(addr)	((addr) & SECONDARY_MASK)
#define PM_IDX(addr)	((addr) >> SECONDARY_SHIFT)

#define IS_DISTINGUISHED_SM(smap)		   \
   ((smap) >= &distinguished_secondary_maps[0] &&  \
    (smap) < &distinguished_secondary_maps[N_SECONDARY_MAPS])

#define IS_DISTINGUISHED(addr)	(IS_DISTINGUISHED_SM(primary_map[PM_IDX(addr)]))

#define ENSURE_MAPPABLE(addr,caller)                              \
   do {                                                           \
      if (IS_DISTINGUISHED(addr)) {				  \
	 primary_map[PM_IDX(addr)] = alloc_secondary_map(caller, primary_map[PM_IDX(addr)]); \
         /* VG_(printf)("new 2map because of %p\n", addr); */     \
      }                                                           \
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

#define VGM_WORD64_VALID     0x0ULL
#define VGM_WORD64_INVALID   0xFFFFFFFFFFFFFFFFULL

/*------------------------------------------------------------*/
/*--- Command line options + defaults                      ---*/
/*------------------------------------------------------------*/

/* Memcheck defines a couple more. */

/* Allow loads from partially-valid addresses?  default: YES */
extern Bool MAC_(clo_partial_loads_ok);

/* Max volume of the freed blocks queue. */
extern Int MAC_(clo_freelist_vol);

/* Do leak check at exit?  default: NO */
typedef
   enum {
      LC_Off,
      LC_Summary,
      LC_Full,
   }
   LeakCheckMode;

extern LeakCheckMode MAC_(clo_leak_check);

/* How closely should we compare ExeContexts in leak records? default: 2 */
extern VgRes MAC_(clo_leak_resolution);

/* In leak check, show reachable-but-not-freed blocks?  default: NO */
extern Bool MAC_(clo_show_reachable);

/* Assume accesses immediately below %esp are due to gcc-2.96 bugs.
 * default: NO*/
extern Bool MAC_(clo_workaround_gcc296_bugs);

extern Bool MAC_(process_common_cmd_line_option) ( Char* arg );
extern void MAC_(print_common_usage)             ( void );
extern void MAC_(print_common_debug_usage)       ( void );


/*------------------------------------------------------------*/
/*--- Variables                                            ---*/
/*------------------------------------------------------------*/

/* For tracking malloc'd blocks */
extern VgHashTable MAC_(malloc_list);

/* For tracking memory pools. */
extern VgHashTable MAC_(mempool_list);

/* Function pointers for the two tools to track interesting events. */
extern void (*MAC_(new_mem_heap)) ( Addr a, SizeT len, Bool is_inited );
extern void (*MAC_(ban_mem_heap)) ( Addr a, SizeT len );
extern void (*MAC_(die_mem_heap)) ( Addr a, SizeT len );
extern void (*MAC_(copy_mem_heap))( Addr from, Addr to, SizeT len );

/* Function pointers for internal sanity checking. */
extern Bool (*MAC_(check_noaccess))( Addr a, SizeT len, Addr* bad_addr );

/* Used in describe_addr() */
extern Bool (*MAC_(describe_addr_supp))    ( Addr a, AddrInfo* ai );

/* For VALGRIND_COUNT_LEAKS client request */
extern Int MAC_(bytes_leaked);
extern Int MAC_(bytes_indirect);
extern Int MAC_(bytes_dubious);
extern Int MAC_(bytes_reachable);
extern Int MAC_(bytes_suppressed);

/*------------------------------------------------------------*/
/*--- Functions                                            ---*/
/*------------------------------------------------------------*/

extern void MAC_(pp_AddrInfo) ( Addr a, AddrInfo* ai );

extern void MAC_(clear_MAC_Error)          ( MAC_Error* err_extra );

extern Bool MAC_(shared_recognised_suppression) ( Char* name, Supp* su );

extern void* MAC_(new_block) ( ThreadId tid,
                               Addr p, SizeT size, SizeT align, UInt rzB,
                               Bool is_zeroed, MAC_AllocKind kind,
                               VgHashTable table);

extern void MAC_(handle_free) ( ThreadId tid,
                                Addr p, UInt rzB, MAC_AllocKind kind );

extern void MAC_(create_mempool)(Addr pool, UInt rzB, Bool is_zeroed);

extern void MAC_(destroy_mempool)(Addr pool);

extern void MAC_(mempool_alloc)(ThreadId tid, 
                                Addr pool, Addr addr, SizeT size);

extern void MAC_(mempool_free)(Addr pool, Addr addr);

extern void MAC_(record_address_error)     ( ThreadId tid, Addr a,
                                             Int size, Bool isWrite );
extern void MAC_(record_core_mem_error)    ( ThreadId tid, Bool isUnaddr,
                                             Char* s );
extern void MAC_(record_param_error)       ( ThreadId tid, Addr a, Bool isReg,
                                             Bool isUnaddr, Char* msg );
extern void MAC_(record_jump_error)        ( ThreadId tid, Addr a );
extern void MAC_(record_free_error)        ( ThreadId tid, Addr a );
extern void MAC_(record_freemismatch_error)( ThreadId tid, Addr a );
extern void MAC_(record_overlap_error)     ( ThreadId tid, 
                                             Char* function, OverlapExtra* oe );
extern void MAC_(record_illegal_mempool_error) ( ThreadId tid, Addr pool );

extern void MAC_(pp_shared_Error)          ( Error* err);

extern MAC_Chunk* MAC_(first_matching_freed_MAC_Chunk)( Bool (*p)(MAC_Chunk*, void*), void* d );

extern void MAC_(common_pre_clo_init) ( void );
extern void MAC_(common_fini)         ( void (*leak_check)(LeakCheckMode mode) );

extern Bool MAC_(handle_common_client_requests) ( ThreadId tid, 
                                                  UWord* arg_block, UWord* ret );

extern void MAC_(print_malloc_stats) ( void );

/* For leak checking */
extern void MAC_(pp_LeakError)(void* vl, UInt n_this_record, 
                                         UInt n_total_records); 
                           
extern void MAC_(do_detect_memory_leaks) (
          LeakCheckMode mode,
          Bool (*is_valid_64k_chunk) ( UInt ),
          Bool (*is_valid_address)   ( Addr )
       );

extern REGPARM(1) void MAC_(new_mem_stack_4)  ( Addr old_ESP );
extern REGPARM(1) void MAC_(die_mem_stack_4)  ( Addr old_ESP );
extern REGPARM(1) void MAC_(new_mem_stack_8)  ( Addr old_ESP );
extern REGPARM(1) void MAC_(die_mem_stack_8)  ( Addr old_ESP );
extern REGPARM(1) void MAC_(new_mem_stack_12) ( Addr old_ESP );
extern REGPARM(1) void MAC_(die_mem_stack_12) ( Addr old_ESP );
extern REGPARM(1) void MAC_(new_mem_stack_16) ( Addr old_ESP );
extern REGPARM(1) void MAC_(die_mem_stack_16) ( Addr old_ESP );
extern REGPARM(1) void MAC_(new_mem_stack_32) ( Addr old_ESP );
extern REGPARM(1) void MAC_(die_mem_stack_32) ( Addr old_ESP );
extern            void MAC_(die_mem_stack) ( Addr a, SizeT len);
extern            void MAC_(new_mem_stack) ( Addr a, SizeT len);


/*------------------------------------------------------------*/
/*--- Stack pointer adjustment                             ---*/
/*------------------------------------------------------------*/

/* Some noble preprocessor abuse, to enable Memcheck and Addrcheck to
   share this code, but call different functions.

   Note that this code is executed very frequently and must be highly
   optimised, which is why I resort to the preprocessor to achieve the
   factoring, rather than eg. using function pointers.
*/

#define ESP_UPDATE_HANDLERS(ALIGNED4_NEW,  ALIGNED4_DIE,                      \
                            ALIGNED8_NEW,  ALIGNED8_DIE,                      \
                            UNALIGNED_NEW, UNALIGNED_DIE)                     \
                                                                              \
void REGPARM(1) MAC_(new_mem_stack_4)(Addr new_ESP)                           \
{                                                                             \
   PROF_EVENT(110);                                                           \
   if (IS_4_ALIGNED(new_ESP)) {                                               \
      ALIGNED4_NEW  ( new_ESP );                                              \
   } else {                                                                   \
      UNALIGNED_NEW ( new_ESP, 4 );                                           \
   }                                                                          \
}                                                                             \
                                                                              \
void REGPARM(1) MAC_(die_mem_stack_4)(Addr new_ESP)                           \
{                                                                             \
   PROF_EVENT(120);                                                           \
   if (IS_4_ALIGNED(new_ESP)) {                                               \
      ALIGNED4_DIE  ( new_ESP-4 );                                            \
   } else {                                                                   \
      UNALIGNED_DIE ( new_ESP-4, 4 );                                         \
   }                                                                          \
}                                                                             \
                                                                              \
void REGPARM(1) MAC_(new_mem_stack_8)(Addr new_ESP)                           \
{                                                                             \
   PROF_EVENT(111);                                                           \
   if (IS_8_ALIGNED(new_ESP)) {                                               \
      ALIGNED8_NEW  ( new_ESP );                                              \
   } else if (IS_4_ALIGNED(new_ESP)) {                                        \
      ALIGNED4_NEW  ( new_ESP   );                                            \
      ALIGNED4_NEW  ( new_ESP+4 );                                            \
   } else {                                                                   \
      UNALIGNED_NEW ( new_ESP, 8 );                                           \
   }                                                                          \
}                                                                             \
                                                                              \
void REGPARM(1) MAC_(die_mem_stack_8)(Addr new_ESP)                           \
{                                                                             \
   PROF_EVENT(121);                                                           \
   if (IS_8_ALIGNED(new_ESP)) {                                               \
      ALIGNED8_DIE  ( new_ESP-8 );                                            \
   } else if (IS_4_ALIGNED(new_ESP)) {                                        \
      ALIGNED4_DIE  ( new_ESP-8 );                                            \
      ALIGNED4_DIE  ( new_ESP-4 );                                            \
   } else {                                                                   \
      UNALIGNED_DIE ( new_ESP-8, 8 );                                         \
   }                                                                          \
}                                                                             \
                                                                              \
void REGPARM(1) MAC_(new_mem_stack_12)(Addr new_ESP)                          \
{                                                                             \
   PROF_EVENT(112);                                                           \
   if (IS_8_ALIGNED(new_ESP)) {                                               \
      ALIGNED8_NEW  ( new_ESP   );                                            \
      ALIGNED4_NEW  ( new_ESP+8 );                                            \
   } else if (IS_4_ALIGNED(new_ESP)) {                                        \
      ALIGNED4_NEW  ( new_ESP   );                                            \
      ALIGNED8_NEW  ( new_ESP+4 );                                            \
   } else {                                                                   \
      UNALIGNED_NEW ( new_ESP, 12 );                                          \
   }                                                                          \
}                                                                             \
                                                                              \
void REGPARM(1) MAC_(die_mem_stack_12)(Addr new_ESP)                          \
{                                                                             \
   PROF_EVENT(122);                                                           \
   /* Note the -12 in the test */                                             \
   if (IS_8_ALIGNED(new_ESP-12)) {                                            \
      ALIGNED8_DIE  ( new_ESP-12 );                                           \
      ALIGNED4_DIE  ( new_ESP-4  );                                           \
   } else if (IS_4_ALIGNED(new_ESP)) {                                        \
      ALIGNED4_DIE  ( new_ESP-12 );                                           \
      ALIGNED8_DIE  ( new_ESP-8  );                                           \
   } else {                                                                   \
      UNALIGNED_DIE ( new_ESP-12, 12 );                                       \
   }                                                                          \
}                                                                             \
                                                                              \
void REGPARM(1) MAC_(new_mem_stack_16)(Addr new_ESP)                          \
{                                                                             \
   PROF_EVENT(113);                                                           \
   if (IS_8_ALIGNED(new_ESP)) {                                               \
      ALIGNED8_NEW  ( new_ESP   );                                            \
      ALIGNED8_NEW  ( new_ESP+8 );                                            \
   } else if (IS_4_ALIGNED(new_ESP)) {                                        \
      ALIGNED4_NEW  ( new_ESP    );                                           \
      ALIGNED8_NEW  ( new_ESP+4  );                                           \
      ALIGNED4_NEW  ( new_ESP+12 );                                           \
   } else {                                                                   \
      UNALIGNED_NEW ( new_ESP, 16 );                                          \
   }                                                                          \
}                                                                             \
                                                                              \
void REGPARM(1) MAC_(die_mem_stack_16)(Addr new_ESP)                          \
{                                                                             \
   PROF_EVENT(123);                                                           \
   if (IS_8_ALIGNED(new_ESP)) {                                               \
      ALIGNED8_DIE  ( new_ESP-16 );                                           \
      ALIGNED8_DIE  ( new_ESP-8  );                                           \
   } else if (IS_4_ALIGNED(new_ESP)) {                                        \
      ALIGNED4_DIE  ( new_ESP-16 );                                           \
      ALIGNED8_DIE  ( new_ESP-12 );                                           \
      ALIGNED4_DIE  ( new_ESP-4  );                                           \
   } else {                                                                   \
      UNALIGNED_DIE ( new_ESP-16, 16 );                                       \
   }                                                                          \
}                                                                             \
                                                                              \
void REGPARM(1) MAC_(new_mem_stack_32)(Addr new_ESP)                          \
{                                                                             \
   PROF_EVENT(114);                                                           \
   if (IS_8_ALIGNED(new_ESP)) {                                               \
      ALIGNED8_NEW  ( new_ESP    );                                           \
      ALIGNED8_NEW  ( new_ESP+8  );                                           \
      ALIGNED8_NEW  ( new_ESP+16 );                                           \
      ALIGNED8_NEW  ( new_ESP+24 );                                           \
   } else if (IS_4_ALIGNED(new_ESP)) {                                        \
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
void REGPARM(1) MAC_(die_mem_stack_32)(Addr new_ESP)                          \
{                                                                             \
   PROF_EVENT(124);                                                           \
   if (IS_8_ALIGNED(new_ESP)) {                                               \
      ALIGNED8_DIE  ( new_ESP-32 );                                           \
      ALIGNED8_DIE  ( new_ESP-24 );                                           \
      ALIGNED8_DIE  ( new_ESP-16 );                                           \
      ALIGNED8_DIE  ( new_ESP- 8 );                                           \
   } else if (IS_4_ALIGNED(new_ESP)) {                                        \
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
void MAC_(new_mem_stack) ( Addr a, SizeT len )                                \
{                                                                             \
   PROF_EVENT(115);                                                           \
   UNALIGNED_NEW ( a, len );                                                  \
}                                                                             \
                                                                              \
void MAC_(die_mem_stack) ( Addr a, SizeT len )                                \
{                                                                             \
   PROF_EVENT(125);                                                           \
   UNALIGNED_DIE ( a, len );                                                  \
}

#endif   /* __MAC_SHARED_H */

/*--------------------------------------------------------------------*/
/*--- end                                             mac_shared.h ---*/
/*--------------------------------------------------------------------*/
