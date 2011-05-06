
/*--------------------------------------------------------------------*/
/*--- A header file for all parts of the MemCheck tool.            ---*/
/*---                                                 mc_include.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of MemCheck, a heavyweight Valgrind tool for
   detecting memory errors.

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

#ifndef __MC_INCLUDE_H
#define __MC_INCLUDE_H

#define MC_(str)    VGAPPEND(vgMemCheck_,str)


/* This is a private header file for use only within the
   memcheck/ directory. */

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
   
/* This describes a heap block. Nb: first two fields must match core's
 * VgHashNode. */
typedef
   struct _MC_Chunk {
      struct _MC_Chunk* next;
      Addr         data;            // Address of the actual block.
      SizeT        szB : (sizeof(SizeT)*8)-2; // Size requested; 30 or 62 bits.
      MC_AllocKind allockind : 2;   // Which operation did the allocation.
      ExeContext*  where;           // Where it was allocated.
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


void* MC_(new_block)  ( ThreadId tid,
                        Addr p, SizeT size, SizeT align,
                        Bool is_zeroed, MC_AllocKind kind,
                        VgHashTable table);
void MC_(handle_free) ( ThreadId tid,
                        Addr p, UInt rzB, MC_AllocKind kind );

void MC_(create_mempool)  ( Addr pool, UInt rzB, Bool is_zeroed );
void MC_(destroy_mempool) ( Addr pool );
void MC_(mempool_alloc)   ( ThreadId tid, Addr pool,
                            Addr addr, SizeT size );
void MC_(mempool_free)    ( Addr pool, Addr addr );
void MC_(mempool_trim)    ( Addr pool, Addr addr, SizeT size );
void MC_(move_mempool)    ( Addr poolA, Addr poolB );
void MC_(mempool_change)  ( Addr pool, Addr addrA, Addr addrB, SizeT size );
Bool MC_(mempool_exists)  ( Addr pool );

MC_Chunk* MC_(get_freed_list_head)( void );

/* For tracking malloc'd blocks.  Nb: it's quite important that it's a
   VgHashTable, because VgHashTable allows duplicate keys without complaint.
   This can occur if a user marks a malloc() block as also a custom block with
   MALLOCLIKE_BLOCK. */
extern VgHashTable MC_(malloc_list);

/* For tracking memory pools. */
extern VgHashTable MC_(mempool_list);

/* Shadow memory functions */
Bool MC_(check_mem_is_noaccess)( Addr a, SizeT len, Addr* bad_addr );
void MC_(make_mem_noaccess)        ( Addr a, SizeT len );
void MC_(make_mem_undefined_w_otag)( Addr a, SizeT len, UInt otag );
void MC_(make_mem_defined)         ( Addr a, SizeT len );
void MC_(copy_address_range_state) ( Addr src, Addr dst, SizeT len );

void MC_(print_malloc_stats) ( void );

void* MC_(malloc)               ( ThreadId tid, SizeT n );
void* MC_(__builtin_new)        ( ThreadId tid, SizeT n );
void* MC_(__builtin_vec_new)    ( ThreadId tid, SizeT n );
void* MC_(memalign)             ( ThreadId tid, SizeT align, SizeT n );
void* MC_(calloc)               ( ThreadId tid, SizeT nmemb, SizeT size1 );
void  MC_(free)                 ( ThreadId tid, void* p );
void  MC_(__builtin_delete)     ( ThreadId tid, void* p );
void  MC_(__builtin_vec_delete) ( ThreadId tid, void* p );
void* MC_(realloc)              ( ThreadId tid, void* p, SizeT new_size );
SizeT MC_(malloc_usable_size)   ( ThreadId tid, void* p );

void MC_(handle_resizeInPlace)(ThreadId tid, Addr p,
                               SizeT oldSizeB, SizeT newSizeB, SizeT rzB);


/*------------------------------------------------------------*/
/*--- Origin tracking translate-time support               ---*/
/*------------------------------------------------------------*/

/* See detailed comments in mc_machine.c. */
Int MC_(get_otrack_shadow_offset) ( Int offset, Int szB );
IRType MC_(get_otrack_reg_array_equiv_int_type) ( IRRegArray* arr );

/* Constants which are used as the lowest 2 bits in origin tags.
   
   An origin tag comprises an upper 30-bit ECU field and a lower 2-bit
   'kind' field.  The ECU field is a number given out by m_execontext
   and has a 1-1 mapping with ExeContext*s.  An ECU can be used
   directly as an origin tag (otag), but in fact we want to put
   additional information 'kind' field to indicate roughly where the
   tag came from.  This helps print more understandable error messages
   for the user -- it has no other purpose.

   Hence the following 2-bit constants are needed for 'kind' field. 

   To summarise:

   * Both ECUs and origin tags are represented as 32-bit words

   * m_execontext and the core-tool interface deal purely in ECUs.
     They have no knowledge of origin tags - that is a purely
     Memcheck-internal matter.

   * all valid ECUs have the lowest 2 bits zero and at least
     one of the upper 30 bits nonzero (see VG_(is_plausible_ECU))

   * to convert from an ECU to an otag, OR in one of the MC_OKIND_
     constants below

   * to convert an otag back to an ECU, AND it with ~3
*/

#define MC_OKIND_UNKNOWN  0  /* unknown origin */
#define MC_OKIND_HEAP     1  /* this is a heap origin */
#define MC_OKIND_STACK    2  /* this is a stack origin */
#define MC_OKIND_USER     3  /* arises from user-supplied client req */


/*------------------------------------------------------------*/
/*--- Profiling of memory events                           ---*/
/*------------------------------------------------------------*/

/* Define to collect detailed performance info. */
/* #define MC_PROFILE_MEMORY */

#ifdef MC_PROFILE_MEMORY
#  define N_PROF_EVENTS 500

UInt   MC_(event_ctr)[N_PROF_EVENTS];
HChar* MC_(event_ctr_name)[N_PROF_EVENTS];

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

typedef 
   enum { 
      // Nb: the order is important -- it dictates the order of loss records
      // of equal sizes.
      Reachable    =0,  // Definitely reachable from root-set.
      Possible     =1,  // Possibly reachable from root-set;  involves at
                        //   least one interior-pointer along the way.
      IndirectLeak =2,  // Leaked, but reachable from another leaked block
                        //   (be it Unreached or IndirectLeak).
      Unreached    =3,  // Not reached, ie. leaked. 
                        //   (At best, only reachable from itself via a cycle.)
  }
  Reachedness;

/* For VALGRIND_COUNT_LEAKS client request */
extern SizeT MC_(bytes_leaked);
extern SizeT MC_(bytes_indirect);
extern SizeT MC_(bytes_dubious);
extern SizeT MC_(bytes_reachable);
extern SizeT MC_(bytes_suppressed);

/* For VALGRIND_COUNT_LEAK_BLOCKS client request */
extern SizeT MC_(blocks_leaked);
extern SizeT MC_(blocks_indirect);
extern SizeT MC_(blocks_dubious);
extern SizeT MC_(blocks_reachable);
extern SizeT MC_(blocks_suppressed);

typedef
   enum {
      LC_Off,
      LC_Summary,
      LC_Full,
   }
   LeakCheckMode;

/* When a LossRecord is put into an OSet, these elements represent the key. */
typedef
   struct _LossRecordKey {
      Reachedness  state;        // LC_Extra.state value shared by all blocks.
      ExeContext*  allocated_at; // Where they were allocated.
   } 
   LossRecordKey;

/* A loss record, used for generating err msgs.  Multiple leaked blocks can be
 * merged into a single loss record if they have the same state and similar
 * enough allocation points (controlled by --leak-resolution). */
typedef
   struct _LossRecord {
      LossRecordKey key;  // Key, when used in an OSet.
      SizeT szB;          // Sum of all MC_Chunk.szB values.
      SizeT indirect_szB; // Sum of all LC_Extra.indirect_szB values.
      UInt  num_blocks;   // Number of blocks represented by the record.
   }
   LossRecord;

void MC_(detect_memory_leaks) ( ThreadId tid, LeakCheckMode mode );

Bool MC_(is_valid_aligned_word)     ( Addr a );
Bool MC_(is_within_valid_secondary) ( Addr a );

void MC_(pp_LeakError)(UInt n_this_record, UInt n_total_records,
                       LossRecord* l);
                          

/*------------------------------------------------------------*/
/*--- Errors and suppressions                              ---*/
/*------------------------------------------------------------*/

/* Did we show to the user, any errors for which an uninitialised
   value origin could have been collected (but wasn't) ?  If yes,
   then, at the end of the run, print a 1 line message advising that a
   rerun with --track-origins=yes might help. */
extern Bool MC_(any_value_errors);

/* Standard functions for error and suppressions as required by the
   core/tool iface */
Bool MC_(eq_Error)           ( VgRes res, Error* e1, Error* e2 );
void MC_(before_pp_Error)    ( Error* err );
void MC_(pp_Error)           ( Error* err );
UInt MC_(update_Error_extra) ( Error* err );

Bool MC_(is_recognised_suppression) ( Char* name, Supp* su );

Bool MC_(read_extra_suppression_info) ( Int fd, Char** buf,
                                        SizeT* nBuf, Supp *su );

Bool MC_(error_matches_suppression) ( Error* err, Supp* su );

Bool MC_(get_extra_suppression_info) ( Error* err,
                                       /*OUT*/Char* buf, Int nBuf );

Char* MC_(get_error_name) ( Error* err );

/* Recording of errors */
void MC_(record_address_error) ( ThreadId tid, Addr a, Int szB,
                                 Bool isWrite );
void MC_(record_cond_error)    ( ThreadId tid, UInt otag );
void MC_(record_value_error)   ( ThreadId tid, Int szB, UInt otag );
void MC_(record_jump_error)    ( ThreadId tid, Addr a );

void MC_(record_free_error)            ( ThreadId tid, Addr a ); 
void MC_(record_illegal_mempool_error) ( ThreadId tid, Addr a );
void MC_(record_freemismatch_error)    ( ThreadId tid, MC_Chunk* mc );

void MC_(record_overlap_error)  ( ThreadId tid, Char* function,
                                  Addr src, Addr dst, SizeT szB );
void MC_(record_core_mem_error) ( ThreadId tid, Char* msg );
void MC_(record_regparam_error) ( ThreadId tid, Char* msg, UInt otag );
void MC_(record_memparam_error) ( ThreadId tid, Addr a, 
                                  Bool isAddrErr, Char* msg, UInt otag );
void MC_(record_user_error)     ( ThreadId tid, Addr a,
                                  Bool isAddrErr, UInt otag );

Bool MC_(record_leak_error)     ( ThreadId tid,
                                  UInt n_this_record,
                                  UInt n_total_records,
                                  LossRecord* lossRecord,
                                  Bool print_record,
                                  Bool count_error );

/* prints a description of address a */
void MC_(pp_describe_addr) (Addr a);

/* Is this address in a user-specified "ignored range" ? */
Bool MC_(in_ignored_range) ( Addr a );


/*------------------------------------------------------------*/
/*--- Client blocks                                        ---*/
/*------------------------------------------------------------*/

/* Describes a client block.  See mc_main.c.  An unused block has
   start == size == 0.  */
typedef
   struct {
      Addr        start;
      SizeT       size;
      ExeContext* where;
      Char*       desc;
   } 
   CGenBlock;

/* Get access to the client block array. */
void MC_(get_ClientBlock_array)( /*OUT*/CGenBlock** blocks,
                                 /*OUT*/UWord* nBlocks );


/*------------------------------------------------------------*/
/*--- Command line options + defaults                      ---*/
/*------------------------------------------------------------*/

/* Allow loads from partially-valid addresses?  default: YES */
extern Bool MC_(clo_partial_loads_ok);

/* Max volume of the freed blocks queue. */
extern Long MC_(clo_freelist_vol);

/* Do leak check at exit?  default: NO */
extern LeakCheckMode MC_(clo_leak_check);

/* How closely should we compare ExeContexts in leak records? default: 2 */
extern VgRes MC_(clo_leak_resolution);

/* In leak check, show reachable-but-not-freed blocks?  default: NO */
extern Bool MC_(clo_show_reachable);

/* In leak check, show possibly-lost blocks?  default: YES */
extern Bool MC_(clo_show_possibly_lost);

/* Assume accesses immediately below %esp are due to gcc-2.96 bugs.
 * default: NO */
extern Bool MC_(clo_workaround_gcc296_bugs);

/* Fill malloc-d/free-d client blocks with a specific value?  -1 if
   not, else 0x00 .. 0xFF indicating the fill value to use.  Can be
   useful for causing programs with bad heap corruption to fail in
   more repeatable ways.  Note that malloc-filled and free-filled
   areas are still undefined and noaccess respectively.  This merely
   causes them to contain the specified values. */
extern Int MC_(clo_malloc_fill);
extern Int MC_(clo_free_fill);

/* Indicates the level of instrumentation/checking done by Memcheck.

   1 = No undefined value checking, Addrcheck-style behaviour only:
       only address checking is done.  This is faster but finds fewer
       errors.  Note that although Addrcheck had 1 bit per byte
       overhead vs the old Memcheck's 9 bits per byte, with this mode
       and compressed V bits, no memory is saved with this mode --
       it's still 2 bits per byte overhead.  This is a little wasteful
       -- it could be done with 1 bit per byte -- but lets us reuse
       the many shadow memory access functions.  Note that in this
       mode neither the secondary V bit table nor the origin-tag cache
       are used.

   2 = Address checking and Undefined value checking are performed,
       but origins are not tracked.  So the origin-tag cache is not
       used in this mode.  This setting is the default and corresponds
       to the "normal" Memcheck behaviour that has shipped for years.

   3 = Address checking, undefined value checking, and origins for
       undefined values are tracked.

   The default is 2.
*/
extern Int MC_(clo_mc_level);


/*------------------------------------------------------------*/
/*--- Instrumentation                                      ---*/
/*------------------------------------------------------------*/

/* Functions defined in mc_main.c */

/* For the fail_w_o functions, the UWord arg is actually the 32-bit
   origin tag and should really be UInt, but to be simple and safe
   considering it's called from generated code, just claim it to be a
   UWord. */
VG_REGPARM(2) void MC_(helperc_value_checkN_fail_w_o) ( HWord, UWord );
VG_REGPARM(1) void MC_(helperc_value_check8_fail_w_o) ( UWord );
VG_REGPARM(1) void MC_(helperc_value_check4_fail_w_o) ( UWord );
VG_REGPARM(1) void MC_(helperc_value_check1_fail_w_o) ( UWord );
VG_REGPARM(1) void MC_(helperc_value_check0_fail_w_o) ( UWord );

/* And call these ones instead to report an uninitialised value error
   but with no origin available. */
VG_REGPARM(1) void MC_(helperc_value_checkN_fail_no_o) ( HWord );
VG_REGPARM(0) void MC_(helperc_value_check8_fail_no_o) ( void );
VG_REGPARM(0) void MC_(helperc_value_check4_fail_no_o) ( void );
VG_REGPARM(0) void MC_(helperc_value_check1_fail_no_o) ( void );
VG_REGPARM(0) void MC_(helperc_value_check0_fail_no_o) ( void );

/* V-bits load/store helpers */
VG_REGPARM(1) void MC_(helperc_STOREV64be) ( Addr, ULong );
VG_REGPARM(1) void MC_(helperc_STOREV64le) ( Addr, ULong );
VG_REGPARM(2) void MC_(helperc_STOREV32be) ( Addr, UWord );
VG_REGPARM(2) void MC_(helperc_STOREV32le) ( Addr, UWord );
VG_REGPARM(2) void MC_(helperc_STOREV16be) ( Addr, UWord );
VG_REGPARM(2) void MC_(helperc_STOREV16le) ( Addr, UWord );
VG_REGPARM(2) void MC_(helperc_STOREV8)   ( Addr, UWord );

VG_REGPARM(1) ULong MC_(helperc_LOADV64be) ( Addr );
VG_REGPARM(1) ULong MC_(helperc_LOADV64le) ( Addr );
VG_REGPARM(1) UWord MC_(helperc_LOADV32be) ( Addr );
VG_REGPARM(1) UWord MC_(helperc_LOADV32le) ( Addr );
VG_REGPARM(1) UWord MC_(helperc_LOADV16be) ( Addr );
VG_REGPARM(1) UWord MC_(helperc_LOADV16le) ( Addr );
VG_REGPARM(1) UWord MC_(helperc_LOADV8)    ( Addr );

void MC_(helperc_MAKE_STACK_UNINIT) ( Addr base, UWord len,
                                                 Addr nia );

/* Origin tag load/store helpers */
VG_REGPARM(2) void  MC_(helperc_b_store1) ( Addr a, UWord d32 );
VG_REGPARM(2) void  MC_(helperc_b_store2) ( Addr a, UWord d32 );
VG_REGPARM(2) void  MC_(helperc_b_store4) ( Addr a, UWord d32 );
VG_REGPARM(2) void  MC_(helperc_b_store8) ( Addr a, UWord d32 );
VG_REGPARM(2) void  MC_(helperc_b_store16)( Addr a, UWord d32 );
VG_REGPARM(1) UWord MC_(helperc_b_load1) ( Addr a );
VG_REGPARM(1) UWord MC_(helperc_b_load2) ( Addr a );
VG_REGPARM(1) UWord MC_(helperc_b_load4) ( Addr a );
VG_REGPARM(1) UWord MC_(helperc_b_load8) ( Addr a );
VG_REGPARM(1) UWord MC_(helperc_b_load16)( Addr a );

/* Functions defined in mc_translate.c */
IRSB* MC_(instrument) ( VgCallbackClosure* closure,
                        IRSB* bb_in, 
                        VexGuestLayout* layout, 
                        VexGuestExtents* vge,
                        IRType gWordTy, IRType hWordTy );

IRSB* MC_(final_tidy) ( IRSB* );

#endif /* ndef __MC_INCLUDE_H */

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
