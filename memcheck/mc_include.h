
/*--------------------------------------------------------------------*/
/*--- A header file for all parts of the MemCheck tool.            ---*/
/*---                                                 mc_include.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of MemCheck, a heavyweight Valgrind tool for
   detecting memory errors.

   Copyright (C) 2000-2007 Julian Seward 
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

#ifndef __MC_INCLUDE_H
#define __MC_INCLUDE_H

#define MC_(str)    VGAPPEND(vgMemCheck_,str)

/*------------------------------------------------------------*/
/*--- Tracking the heap                                    ---*/
/*------------------------------------------------------------*/

/* We want at least a 16B redzone on client heap blocks for Memcheck */
#define MC_MALLOC_REDZONE_SZB    16

/* For malloc()/new/new[] vs. free()/delete/delete[] mismatch checking. */
typedef
   enum {
      MC_AllocMalloc = 0,
      MC_AllocNew    = 1,
      MC_AllocNewVec = 2,
      MC_AllocCustom = 3
   }
   MC_AllocKind;
   
/* Nb: first two fields must match core's VgHashNode. */
typedef
   struct _MC_Chunk {
      struct _MC_Chunk* next;
      Addr         data;            // ptr to actual block
      SizeT        szB : (sizeof(UWord)*8)-2; // size requested; 30 or 62 bits
      MC_AllocKind allockind : 2;   // which wrapper did the allocation
      ExeContext*  where;           // where it was allocated
   }
   MC_Chunk;

/* Memory pool.  Nb: first two fields must match core's VgHashNode. */
typedef
   struct _MC_Mempool {
      struct _MC_Mempool* next;
      Addr          pool;           // pool identifier
      SizeT         rzB;            // pool red-zone size
      Bool          is_zeroed;      // allocations from this pool are zeroed
      VgHashTable   chunks;         // chunks associated with this pool
   }
   MC_Mempool;


extern void* MC_(new_block)  ( ThreadId tid,
                               Addr p, SizeT size, SizeT align, UInt rzB,
                               Bool is_zeroed, MC_AllocKind kind,
                               VgHashTable table);
extern void MC_(handle_free) ( ThreadId tid,
                                Addr p, UInt rzB, MC_AllocKind kind );

extern void MC_(create_mempool)  ( Addr pool, UInt rzB, Bool is_zeroed );
extern void MC_(destroy_mempool) ( Addr pool );
extern void MC_(mempool_alloc)   ( ThreadId tid, Addr pool,
                                   Addr addr, SizeT size );
extern void MC_(mempool_free)    ( Addr pool, Addr addr );
extern void MC_(mempool_trim)    ( Addr pool, Addr addr, SizeT size );
extern void MC_(move_mempool)    ( Addr poolA, Addr poolB );
extern void MC_(mempool_change)  ( Addr pool, Addr addrA, Addr addrB, SizeT size );
extern Bool MC_(mempool_exists)  ( Addr pool );

extern MC_Chunk* MC_(get_freed_list_head)( void );

/* For tracking malloc'd blocks */
extern VgHashTable MC_(malloc_list);

/* For tracking memory pools. */
extern VgHashTable MC_(mempool_list);

/* Shadow memory functions */
extern Bool MC_(check_mem_is_noaccess)( Addr a, SizeT len, Addr* bad_addr );
extern void MC_(make_mem_noaccess) ( Addr a, SizeT len );
extern void MC_(make_mem_undefined)( Addr a, SizeT len );
extern void MC_(make_mem_defined)  ( Addr a, SizeT len );
extern void MC_(copy_address_range_state) ( Addr src, Addr dst, SizeT len );

extern void MC_(print_malloc_stats) ( void );

extern void* MC_(malloc)               ( ThreadId tid, SizeT n );
extern void* MC_(__builtin_new)        ( ThreadId tid, SizeT n );
extern void* MC_(__builtin_vec_new)    ( ThreadId tid, SizeT n );
extern void* MC_(memalign)             ( ThreadId tid, SizeT align, SizeT n );
extern void* MC_(calloc)               ( ThreadId tid, SizeT nmemb, SizeT size1 );
extern void  MC_(free)                 ( ThreadId tid, void* p );
extern void  MC_(__builtin_delete)     ( ThreadId tid, void* p );
extern void  MC_(__builtin_vec_delete) ( ThreadId tid, void* p );
extern void* MC_(realloc)              ( ThreadId tid, void* p, SizeT new_size );


/*------------------------------------------------------------*/
/*--- Profiling of memory events                           ---*/
/*------------------------------------------------------------*/

/* Define to collect detailed performance info. */
/* #define MC_PROFILE_MEMORY */

#ifdef MC_PROFILE_MEMORY
#  define N_PROF_EVENTS 500

extern UInt   MC_(event_ctr)[N_PROF_EVENTS];
extern HChar* MC_(event_ctr_name)[N_PROF_EVENTS];

#  define PROF_EVENT(ev, name)                                \
   do { tl_assert((ev) >= 0 && (ev) < N_PROF_EVENTS);         \
        /* crude and inaccurate check to ensure the same */   \
        /* event isn't being used with > 1 name */            \
        if (MC_(event_ctr_name)[ev])                         \
           tl_assert(name == MC_(event_ctr_name)[ev]);       \
        MC_(event_ctr)[ev]++;                                \
        MC_(event_ctr_name)[ev] = (name);                    \
   } while (False);

#else

#  define PROF_EVENT(ev, name) /* */

#endif   /* MC_PROFILE_MEMORY */


/*------------------------------------------------------------*/
/*--- V and A bits (Victoria & Albert ?)                   ---*/
/*------------------------------------------------------------*/

/* The number of entries in the primary map can be altered.  However
   we hardwire the assumption that each secondary map covers precisely
   64k of address space. */
#define SM_SIZE 65536            /* DO NOT CHANGE */
#define SM_MASK (SM_SIZE-1)      /* DO NOT CHANGE */

#define V_BIT_DEFINED         0
#define V_BIT_UNDEFINED       1

#define V_BITS8_DEFINED       0
#define V_BITS8_UNDEFINED     0xFF

#define V_BITS16_DEFINED      0
#define V_BITS16_UNDEFINED    0xFFFF

#define V_BITS32_DEFINED      0
#define V_BITS32_UNDEFINED    0xFFFFFFFF

#define V_BITS64_DEFINED      0ULL
#define V_BITS64_UNDEFINED    0xFFFFFFFFFFFFFFFFULL


/*------------------------------------------------------------*/
/*--- Leak checking                                        ---*/
/*------------------------------------------------------------*/

/* A block is either 
   -- Proper-ly reached; a pointer to its start has been found
   -- Interior-ly reached; only an interior pointer to it has been found
   -- Unreached; so far, no pointers to any part of it have been found. 
   -- IndirectLeak; leaked, but referred to by another leaked block
*/
typedef 
   enum { 
      Unreached    =0, 
      IndirectLeak =1,
      Interior     =2, 
      Proper       =3
  }
  Reachedness;

/* For VALGRIND_COUNT_LEAKS client request */
extern SizeT MC_(bytes_leaked);
extern SizeT MC_(bytes_indirect);
extern SizeT MC_(bytes_dubious);
extern SizeT MC_(bytes_reachable);
extern SizeT MC_(bytes_suppressed);

typedef
   enum {
      LC_Off,
      LC_Summary,
      LC_Full,
   }
   LeakCheckMode;

/* A block record, used for generating err msgs. */
typedef
   struct _LossRecord {
      struct _LossRecord* next;
      /* Where these lost blocks were allocated. */
      ExeContext*  allocated_at;
      /* Their reachability. */
      Reachedness  loss_mode;
      /* Number of blocks and total # bytes involved. */
      SizeT        total_bytes;
      SizeT        indirect_bytes;
      UInt         num_blocks;
   }
   LossRecord;

extern void MC_(do_detect_memory_leaks) (
          ThreadId tid, LeakCheckMode mode,
          Bool (*is_within_valid_secondary) ( Addr ),
          Bool (*is_valid_aligned_word)     ( Addr )
       );

extern void MC_(pp_LeakError)(UInt n_this_record, UInt n_total_records,
                              LossRecord* l);
                          

/*------------------------------------------------------------*/
/*--- Errors and suppressions                              ---*/
/*------------------------------------------------------------*/

extern void MC_(record_free_error)            ( ThreadId tid, Addr a ); 
extern void MC_(record_illegal_mempool_error) ( ThreadId tid, Addr a );
extern void MC_(record_freemismatch_error)    ( ThreadId tid, MC_Chunk* mc );
extern Bool MC_(record_leak_error)            ( ThreadId tid,
                                                UInt n_this_record,
                                                UInt n_total_records,
                                                LossRecord* lossRecord,
                                                Bool print_record );

/*------------------------------------------------------------*/
/*--- Command line options + defaults                      ---*/
/*------------------------------------------------------------*/

/* Allow loads from partially-valid addresses?  default: YES */
extern Bool MC_(clo_partial_loads_ok);

/* Max volume of the freed blocks queue. */
extern Int MC_(clo_freelist_vol);

/* Do leak check at exit?  default: NO */
extern LeakCheckMode MC_(clo_leak_check);

/* How closely should we compare ExeContexts in leak records? default: 2 */
extern VgRes MC_(clo_leak_resolution);

/* In leak check, show reachable-but-not-freed blocks?  default: NO */
extern Bool MC_(clo_show_reachable);

/* Assume accesses immediately below %esp are due to gcc-2.96 bugs.
 * default: NO */
extern Bool MC_(clo_workaround_gcc296_bugs);

/* Do undefined value checking? "No" gives Addrcheck-style behaviour, ie.
 * faster but fewer errors found.  Note that although Addrcheck had 1 bit
 * per byte overhead vs the old Memcheck's 9 bits per byte, with this mode
 * and compressed V bits, no memory is saved with this mode -- it's still
 * 2 bits per byte overhead.  This is a little wasteful -- it could be done
 * with 1 bit per byte -- but lets us reuse the many shadow memory access
 * functions.  Note also that in this mode the secondary V bit table is
 * never used.
 *
 * default: YES */
extern Bool MC_(clo_undef_value_errors);


/*------------------------------------------------------------*/
/*--- Instrumentation                                      ---*/
/*------------------------------------------------------------*/

/* Functions defined in mc_main.c */
extern VG_REGPARM(1) void MC_(helperc_complain_undef) ( HWord );
extern void MC_(helperc_value_check8_fail) ( void );
extern void MC_(helperc_value_check4_fail) ( void );
extern void MC_(helperc_value_check1_fail) ( void );
extern void MC_(helperc_value_check0_fail) ( void );

extern VG_REGPARM(1) void MC_(helperc_STOREV64be) ( Addr, ULong );
extern VG_REGPARM(1) void MC_(helperc_STOREV64le) ( Addr, ULong );
extern VG_REGPARM(2) void MC_(helperc_STOREV32be) ( Addr, UWord );
extern VG_REGPARM(2) void MC_(helperc_STOREV32le) ( Addr, UWord );
extern VG_REGPARM(2) void MC_(helperc_STOREV16be) ( Addr, UWord );
extern VG_REGPARM(2) void MC_(helperc_STOREV16le) ( Addr, UWord );
extern VG_REGPARM(2) void MC_(helperc_STOREV8)   ( Addr, UWord );

extern VG_REGPARM(1) ULong MC_(helperc_LOADV64be) ( Addr );
extern VG_REGPARM(1) ULong MC_(helperc_LOADV64le) ( Addr );
extern VG_REGPARM(1) UWord MC_(helperc_LOADV32be) ( Addr );
extern VG_REGPARM(1) UWord MC_(helperc_LOADV32le) ( Addr );
extern VG_REGPARM(1) UWord MC_(helperc_LOADV16be) ( Addr );
extern VG_REGPARM(1) UWord MC_(helperc_LOADV16le) ( Addr );
extern VG_REGPARM(1) UWord MC_(helperc_LOADV8)    ( Addr );

extern void MC_(helperc_MAKE_STACK_UNINIT) ( Addr base, UWord len );

/* Functions defined in mc_translate.c */
extern
IRSB* MC_(instrument) ( VgCallbackClosure* closure,
                        IRSB* bb_in, 
                        VexGuestLayout* layout, 
                        VexGuestExtents* vge,
                        IRType gWordTy, IRType hWordTy );

#endif /* ndef __MC_INCLUDE_H */

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/

