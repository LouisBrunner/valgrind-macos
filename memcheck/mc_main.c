
/*--------------------------------------------------------------------*/
/*--- MemCheck: Maintain bitmaps of memory, tracking the           ---*/
/*--- accessibility (A) and validity (V) status of each byte.      ---*/
/*---                                                    mc_main.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of MemCheck, a heavyweight Valgrind tool for
   detecting memory errors.

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

#include "mc_include.h"
#include "memcheck.h"   /* for client requests */
//#include "vg_profile.c"

/* Define to debug the mem audit system. */
/* #define VG_DEBUG_MEMORY */

#define DEBUG(fmt, args...) //VG_(printf)(fmt, ## args)

/*------------------------------------------------------------*/
/*--- Low-level support for memory checking.               ---*/
/*------------------------------------------------------------*/

/* All reads and writes are checked against a memory map, which
   records the state of all memory in the process.  The memory map is
   organised like this:

   The top 16 bits of an address are used to index into a top-level
   map table, containing 65536 entries.  Each entry is a pointer to a
   second-level map, which records the accesibililty and validity
   permissions for the 65536 bytes indexed by the lower 16 bits of the
   address.  Each byte is represented by nine bits, one indicating
   accessibility, the other eight validity.  So each second-level map
   contains 73728 bytes.  This two-level arrangement conveniently
   divides the 4G address space into 64k lumps, each size 64k bytes.

   All entries in the primary (top-level) map must point to a valid
   secondary (second-level) map.  Since most of the 4G of address
   space will not be in use -- ie, not mapped at all -- there is a
   distinguished secondary map, which indicates `not addressible and
   not valid' writeable for all bytes.  Entries in the primary map for
   which the entire 64k is not in use at all point at this
   distinguished map.

   [...] lots of stuff deleted due to out of date-ness

   As a final optimisation, the alignment and address checks for
   4-byte loads and stores are combined in a neat way.  The primary
   map is extended to have 262144 entries (2^18), rather than 2^16.
   The top 3/4 of these entries are permanently set to the
   distinguished secondary map.  For a 4-byte load/store, the
   top-level map is indexed not with (addr >> 16) but instead f(addr),
   where

    f( XXXX XXXX XXXX XXXX ____ ____ ____ __YZ )
        = ____ ____ ____ __YZ XXXX XXXX XXXX XXXX  or 
        = ____ ____ ____ __ZY XXXX XXXX XXXX XXXX

   ie the lowest two bits are placed above the 16 high address bits.
   If either of these two bits are nonzero, the address is misaligned;
   this will select a secondary map from the upper 3/4 of the primary
   map.  Because this is always the distinguished secondary map, a
   (bogus) address check failure will result.  The failure handling
   code can then figure out whether this is a genuine addr check
   failure or whether it is a possibly-legitimate access at a
   misaligned address.  
*/


/*------------------------------------------------------------*/
/*--- Function declarations.                               ---*/
/*------------------------------------------------------------*/

static ULong mc_rd_V8_SLOWLY ( Addr a );
static UInt  mc_rd_V4_SLOWLY ( Addr a );
static UInt  mc_rd_V2_SLOWLY ( Addr a );
static UInt  mc_rd_V1_SLOWLY ( Addr a );

static void mc_wr_V8_SLOWLY ( Addr a, ULong vbytes );
static void mc_wr_V4_SLOWLY ( Addr a, UInt vbytes );
static void mc_wr_V2_SLOWLY ( Addr a, UInt vbytes );
static void mc_wr_V1_SLOWLY ( Addr a, UInt vbytes );

/*------------------------------------------------------------*/
/*--- Data defns.                                          ---*/
/*------------------------------------------------------------*/

typedef 
   struct {
      UChar abits[8192];
      UChar vbyte[65536];
   }
   SecMap;

static SecMap* primary_map[ /*65536*/ 262144 ];
static SecMap  distinguished_secondary_map;

static void init_shadow_memory ( void )
{
   Int i;

   for (i = 0; i < 8192; i++)             /* Invalid address */
      distinguished_secondary_map.abits[i] = VGM_BYTE_INVALID; 
   for (i = 0; i < 65536; i++)            /* Invalid Value */
      distinguished_secondary_map.vbyte[i] = VGM_BYTE_INVALID; 

   /* These entries gradually get overwritten as the used address
      space expands. */
   for (i = 0; i < 65536; i++)
      primary_map[i] = &distinguished_secondary_map;

   /* These ones should never change; it's a bug in Valgrind if they do. */
   for (i = 65536; i < 262144; i++)
      primary_map[i] = &distinguished_secondary_map;
}

/*------------------------------------------------------------*/
/*--- Basic bitmap management, reading and writing.        ---*/
/*------------------------------------------------------------*/

/* Allocate and initialise a secondary map. */

static SecMap* alloc_secondary_map ( __attribute__ ((unused)) 
                                     Char* caller )
{
   SecMap* map;
   UInt  i;
   PROF_EVENT(10);

   /* Mark all bytes as invalid access and invalid value. */
   map = (SecMap *)VG_(shadow_alloc)(sizeof(SecMap));

   for (i = 0; i < 8192; i++)
      map->abits[i] = VGM_BYTE_INVALID; /* Invalid address */
   for (i = 0; i < 65536; i++)
      map->vbyte[i] = VGM_BYTE_INVALID; /* Invalid Value */

   /* VG_(printf)("ALLOC_2MAP(%s)\n", caller ); */
   return map;
}


/* Basic reading/writing of the bitmaps, for byte-sized accesses. */

static __inline__ UChar get_abit ( Addr a )
{
   SecMap* sm     = primary_map[a >> 16];
   UInt    sm_off = a & 0xFFFF;
   PROF_EVENT(20);
#  if 0
      if (IS_DISTINGUISHED_SM(sm))
         VG_(message)(Vg_DebugMsg, 
                      "accessed distinguished 2ndary (A)map! 0x%x\n", a);
#  endif
   return BITARR_TEST(sm->abits, sm_off) 
             ? VGM_BIT_INVALID : VGM_BIT_VALID;
}

static __inline__ UChar get_vbyte ( Addr a )
{
   SecMap* sm     = primary_map[a >> 16];
   UInt    sm_off = a & 0xFFFF;
   PROF_EVENT(21);
#  if 0
      if (IS_DISTINGUISHED_SM(sm))
         VG_(message)(Vg_DebugMsg, 
                      "accessed distinguished 2ndary (V)map! 0x%x\n", a);
#  endif
   return sm->vbyte[sm_off];
}

static /* __inline__ */ void set_abit ( Addr a, UChar abit )
{
   SecMap* sm;
   UInt    sm_off;
   PROF_EVENT(22);
   ENSURE_MAPPABLE(a, "set_abit");
   sm     = primary_map[a >> 16];
   sm_off = a & 0xFFFF;
   if (abit) 
      BITARR_SET(sm->abits, sm_off);
   else
      BITARR_CLEAR(sm->abits, sm_off);
}

static __inline__ void set_vbyte ( Addr a, UChar vbyte )
{
   SecMap* sm;
   UInt    sm_off;
   PROF_EVENT(23);
   ENSURE_MAPPABLE(a, "set_vbyte");
   sm     = primary_map[a >> 16];
   sm_off = a & 0xFFFF;
   sm->vbyte[sm_off] = vbyte;
}


/* Reading/writing of the bitmaps, for aligned word-sized accesses. */

static __inline__ UChar get_abits4_ALIGNED ( Addr a )
{
   SecMap* sm;
   UInt    sm_off;
   UChar   abits8;
   PROF_EVENT(24);
#  ifdef VG_DEBUG_MEMORY
   tl_assert(IS_4_ALIGNED(a));
#  endif
   sm     = primary_map[a >> 16];
   sm_off = a & 0xFFFF;
   abits8 = sm->abits[sm_off >> 3];
   abits8 >>= (a & 4 /* 100b */);   /* a & 4 is either 0 or 4 */
   abits8 &= 0x0F;
   return abits8;
}

static UInt __inline__ get_vbytes4_ALIGNED ( Addr a )
{
   SecMap* sm     = primary_map[a >> 16];
   UInt    sm_off = a & 0xFFFF;
   PROF_EVENT(25);
#  ifdef VG_DEBUG_MEMORY
   tl_assert(IS_4_ALIGNED(a));
#  endif
   return ((UInt*)(sm->vbyte))[sm_off >> 2];
}


static void __inline__ set_vbytes4_ALIGNED ( Addr a, UInt vbytes )
{
   SecMap* sm;
   UInt    sm_off;
   ENSURE_MAPPABLE(a, "set_vbytes4_ALIGNED");
   sm     = primary_map[a >> 16];
   sm_off = a & 0xFFFF;
   PROF_EVENT(23);
#  ifdef VG_DEBUG_MEMORY
   tl_assert(IS_4_ALIGNED(a));
#  endif
   ((UInt*)(sm->vbyte))[sm_off >> 2] = vbytes;
}


/*------------------------------------------------------------*/
/*--- Setting permissions over address ranges.             ---*/
/*------------------------------------------------------------*/

static void set_address_range_perms ( Addr a, SizeT len, 
                                      UInt example_a_bit,
                                      UInt example_v_bit )
{
   UChar   vbyte, abyte8;
   UInt    vword4, sm_off;
   SecMap* sm;

   PROF_EVENT(30);

   if (len == 0)
      return;

   if (VG_(clo_verbosity) > 0) {
      if (len > 100 * 1000 * 1000) {
         VG_(message)(Vg_UserMsg, 
                      "Warning: set address range perms: "
                      "large range %u, a %d, v %d",
                      len, example_a_bit, example_v_bit );
      }
   }

   VGP_PUSHCC(VgpSetMem);

   /* Requests to change permissions of huge address ranges may
      indicate bugs in our machinery.  30,000,000 is arbitrary, but so
      far all legitimate requests have fallen beneath that size. */
   /* 4 Mar 02: this is just stupid; get rid of it. */
   /* tl_assert(len < 30000000); */

   /* Check the permissions make sense. */
   tl_assert(example_a_bit == VGM_BIT_VALID 
             || example_a_bit == VGM_BIT_INVALID);
   tl_assert(example_v_bit == VGM_BIT_VALID 
             || example_v_bit == VGM_BIT_INVALID);
   if (example_a_bit == VGM_BIT_INVALID)
      tl_assert(example_v_bit == VGM_BIT_INVALID);

   /* The validity bits to write. */
   vbyte = example_v_bit==VGM_BIT_VALID 
              ? VGM_BYTE_VALID : VGM_BYTE_INVALID;

   /* In order that we can charge through the address space at 8
      bytes/main-loop iteration, make up some perms. */
   abyte8 = (example_a_bit << 7)
            | (example_a_bit << 6)
            | (example_a_bit << 5)
            | (example_a_bit << 4)
            | (example_a_bit << 3)
            | (example_a_bit << 2)
            | (example_a_bit << 1)
            | (example_a_bit << 0);
   vword4 = (vbyte << 24) | (vbyte << 16) | (vbyte << 8) | vbyte;

#  ifdef VG_DEBUG_MEMORY
   /* Do it ... */
   while (True) {
      PROF_EVENT(31);
      if (len == 0) break;
      set_abit ( a, example_a_bit );
      set_vbyte ( a, vbyte );
      a++;
      len--;
   }

#  else
   /* Slowly do parts preceding 8-byte alignment. */
   while (True) {
      PROF_EVENT(31);
      if (len == 0) break;
      if ((a % 8) == 0) break;
      set_abit ( a, example_a_bit );
      set_vbyte ( a, vbyte );
      a++;
      len--;
   }   

   if (len == 0) {
      VGP_POPCC(VgpSetMem);
      return;
   }
   tl_assert((a % 8) == 0 && len > 0);

   /* Once aligned, go fast. */
   while (True) {
      PROF_EVENT(32);
      if (len < 8) break;
      ENSURE_MAPPABLE(a, "set_address_range_perms(fast)");
      sm = primary_map[a >> 16];
      sm_off = a & 0xFFFF;
      sm->abits[sm_off >> 3] = abyte8;
      ((UInt*)(sm->vbyte))[(sm_off >> 2) + 0] = vword4;
      ((UInt*)(sm->vbyte))[(sm_off >> 2) + 1] = vword4;
      a += 8;
      len -= 8;
   }

   if (len == 0) {
      VGP_POPCC(VgpSetMem);
      return;
   }
   tl_assert((a % 8) == 0 && len > 0 && len < 8);

   /* Finish the upper fragment. */
   while (True) {
      PROF_EVENT(33);
      if (len == 0) break;
      set_abit ( a, example_a_bit );
      set_vbyte ( a, vbyte );
      a++;
      len--;
   }   
#  endif

   /* Check that zero page and highest page have not been written to
      -- this could happen with buggy syscall wrappers.  Today
      (2001-04-26) had precisely such a problem with __NR_setitimer. */
   tl_assert(TL_(cheap_sanity_check)());
   VGP_POPCC(VgpSetMem);
}

/* Set permissions for address ranges ... */

static void mc_make_noaccess ( Addr a, SizeT len )
{
   PROF_EVENT(35);
   DEBUG("mc_make_noaccess(%p, %llu)\n", a, (ULong)len);
   set_address_range_perms ( a, len, VGM_BIT_INVALID, VGM_BIT_INVALID );
}

static void mc_make_writable ( Addr a, SizeT len )
{
   PROF_EVENT(36);
   DEBUG("mc_make_writable(%p, %llu)\n", a, (ULong)len);
   set_address_range_perms ( a, len, VGM_BIT_VALID, VGM_BIT_INVALID );
}

static void mc_make_readable ( Addr a, SizeT len )
{
   PROF_EVENT(37);
   DEBUG("mc_make_readable(%p, %llu)\n", a, (ULong)len);
   set_address_range_perms ( a, len, VGM_BIT_VALID, VGM_BIT_VALID );
}

static __inline__
void make_aligned_word_writable(Addr a)
{
   SecMap* sm;
   UInt    sm_off;
   UChar   mask;

   VGP_PUSHCC(VgpESPAdj);
   ENSURE_MAPPABLE(a, "make_aligned_word_writable");
   sm     = primary_map[a >> 16];
   sm_off = a & 0xFFFF;
   ((UInt*)(sm->vbyte))[sm_off >> 2] = VGM_WORD_INVALID;
   mask = 0x0F;
   mask <<= (a & 4 /* 100b */);   /* a & 4 is either 0 or 4 */
   /* mask now contains 1s where we wish to make address bits invalid (0s). */
   sm->abits[sm_off >> 3] &= ~mask;
   VGP_POPCC(VgpESPAdj);
}

static __inline__
void make_aligned_word_noaccess(Addr a)
{
   SecMap* sm;
   UInt    sm_off;
   UChar   mask;

   VGP_PUSHCC(VgpESPAdj);
   ENSURE_MAPPABLE(a, "make_aligned_word_noaccess");
   sm     = primary_map[a >> 16];
   sm_off = a & 0xFFFF;
   ((UInt*)(sm->vbyte))[sm_off >> 2] = VGM_WORD_INVALID;
   mask = 0x0F;
   mask <<= (a & 4 /* 100b */);   /* a & 4 is either 0 or 4 */
   /* mask now contains 1s where we wish to make address bits invalid (1s). */
   sm->abits[sm_off >> 3] |= mask;
   VGP_POPCC(VgpESPAdj);
}

/* Nb: by "aligned" here we mean 8-byte aligned */
static __inline__
void make_aligned_doubleword_writable(Addr a)
{
   SecMap* sm;
   UInt    sm_off;

   VGP_PUSHCC(VgpESPAdj);
   ENSURE_MAPPABLE(a, "make_aligned_doubleword_writable");
   sm = primary_map[a >> 16];
   sm_off = a & 0xFFFF;
   sm->abits[sm_off >> 3] = VGM_BYTE_VALID;
   ((UInt*)(sm->vbyte))[(sm_off >> 2) + 0] = VGM_WORD_INVALID;
   ((UInt*)(sm->vbyte))[(sm_off >> 2) + 1] = VGM_WORD_INVALID;
   VGP_POPCC(VgpESPAdj);
}

static __inline__
void make_aligned_doubleword_noaccess(Addr a)
{
   SecMap* sm;
   UInt    sm_off;

   VGP_PUSHCC(VgpESPAdj);
   ENSURE_MAPPABLE(a, "make_aligned_doubleword_noaccess");
   sm = primary_map[a >> 16];
   sm_off = a & 0xFFFF;
   sm->abits[sm_off >> 3] = VGM_BYTE_INVALID;
   ((UInt*)(sm->vbyte))[(sm_off >> 2) + 0] = VGM_WORD_INVALID;
   ((UInt*)(sm->vbyte))[(sm_off >> 2) + 1] = VGM_WORD_INVALID;
   VGP_POPCC(VgpESPAdj);
}

/* The %esp update handling functions */
ESP_UPDATE_HANDLERS ( make_aligned_word_writable,
                      make_aligned_word_noaccess,
                      make_aligned_doubleword_writable,
                      make_aligned_doubleword_noaccess,
                      mc_make_writable,
                      mc_make_noaccess 
                    );

/* Block-copy permissions (needed for implementing realloc()). */
static void mc_copy_address_range_state ( Addr src, Addr dst, SizeT len )
{
   SizeT i;

   DEBUG("mc_copy_address_range_state\n");

   PROF_EVENT(40);
   for (i = 0; i < len; i++) {
      UChar abit  = get_abit ( src+i );
      UChar vbyte = get_vbyte ( src+i );
      PROF_EVENT(41);
      set_abit ( dst+i, abit );
      set_vbyte ( dst+i, vbyte );
   }
}

/*------------------------------------------------------------*/
/*--- Checking memory                                      ---*/
/*------------------------------------------------------------*/

/* Check permissions for address range.  If inadequate permissions
   exist, *bad_addr is set to the offending address, so the caller can
   know what it is. */

/* Returns True if [a .. a+len) is not addressible.  Otherwise,
   returns False, and if bad_addr is non-NULL, sets *bad_addr to
   indicate the lowest failing address.  Functions below are
   similar. */
static Bool mc_check_noaccess ( Addr a, SizeT len, Addr* bad_addr )
{
   SizeT i;
   UChar abit;
   PROF_EVENT(42);
   for (i = 0; i < len; i++) {
      PROF_EVENT(43);
      abit = get_abit(a);
      if (abit == VGM_BIT_VALID) {
         if (bad_addr != NULL) *bad_addr = a;
         return False;
      }
      a++;
   }
   return True;
}

static Bool mc_check_writable ( Addr a, SizeT len, Addr* bad_addr )
{
   SizeT i;
   UChar abit;
   PROF_EVENT(42);
   for (i = 0; i < len; i++) {
      PROF_EVENT(43);
      abit = get_abit(a);
      if (abit == VGM_BIT_INVALID) {
         if (bad_addr != NULL) *bad_addr = a;
         return False;
      }
      a++;
   }
   return True;
}

typedef enum {
   MC_Ok = 5, MC_AddrErr = 6, MC_ValueErr = 7
} MC_ReadResult;

static MC_ReadResult mc_check_readable ( Addr a, SizeT len, Addr* bad_addr )
{
   SizeT i;
   UChar abit;
   UChar vbyte;

   PROF_EVENT(44);
   DEBUG("mc_check_readable\n");
   for (i = 0; i < len; i++) {
      abit  = get_abit(a);
      vbyte = get_vbyte(a);
      PROF_EVENT(45);
      // Report addressability errors in preference to definedness errors
      // by checking the A bits first.
      if (abit != VGM_BIT_VALID) {
         if (bad_addr != NULL) *bad_addr = a;
         return MC_AddrErr;
      }
      if (vbyte != VGM_BYTE_VALID) {
         if (bad_addr != NULL) *bad_addr = a;
         return MC_ValueErr;
      }
      a++;
   }
   return MC_Ok;
}


/* Check a zero-terminated ascii string.  Tricky -- don't want to
   examine the actual bytes, to find the end, until we're sure it is
   safe to do so. */

static Bool mc_check_readable_asciiz ( Addr a, Addr* bad_addr )
{
   UChar abit;
   UChar vbyte;
   PROF_EVENT(46);
   DEBUG("mc_check_readable_asciiz\n");
   while (True) {
      PROF_EVENT(47);
      abit  = get_abit(a);
      vbyte = get_vbyte(a);
      // As in mc_check_readable(), check A bits first
      if (abit != VGM_BIT_VALID) {
         if (bad_addr != NULL) *bad_addr = a;
         return MC_AddrErr;
      }
      if (vbyte != VGM_BYTE_VALID) {
         if (bad_addr != NULL) *bad_addr = a;
         return MC_ValueErr;
      }
      /* Ok, a is safe to read. */
      if (* ((UChar*)a) == 0) return MC_Ok;
      a++;
   }
}


/*------------------------------------------------------------*/
/*--- Memory event handlers                                ---*/
/*------------------------------------------------------------*/

static
void mc_check_is_writable ( CorePart part, ThreadId tid, Char* s,
                            Addr base, SizeT size )
{
   Bool ok;
   Addr bad_addr;

   VGP_PUSHCC(VgpCheckMem);

   /* VG_(message)(Vg_DebugMsg,"check is writable: %x .. %x",
                               base,base+size-1); */
   ok = mc_check_writable ( base, size, &bad_addr );
   if (!ok) {
      switch (part) {
      case Vg_CoreSysCall:
         MAC_(record_param_error) ( tid, bad_addr, /*isReg*/False,
                                    /*isUnaddr*/True, s );
         break;

      case Vg_CorePThread:
      case Vg_CoreSignal:
         MAC_(record_core_mem_error)( tid, /*isUnaddr*/True, s );
         break;

      default:
         VG_(tool_panic)("mc_check_is_writable: unexpected CorePart");
      }
   }

   VGP_POPCC(VgpCheckMem);
}

static
void mc_check_is_readable ( CorePart part, ThreadId tid, Char* s,
                            Addr base, SizeT size )
{     
   Addr bad_addr;
   MC_ReadResult res;

   VGP_PUSHCC(VgpCheckMem);
   
   /* VG_(message)(Vg_DebugMsg,"check is readable: %x .. %x",
                               base,base+size-1); */
   res = mc_check_readable ( base, size, &bad_addr );
   if (MC_Ok != res) {
      Bool isUnaddr = ( MC_AddrErr == res ? True : False );
      
      switch (part) {
      case Vg_CoreSysCall:
         MAC_(record_param_error) ( tid, bad_addr, /*isReg*/False,
                                    isUnaddr, s );
         break;
      
      case Vg_CorePThread:
         MAC_(record_core_mem_error)( tid, isUnaddr, s );
         break;

      /* If we're being asked to jump to a silly address, record an error 
         message before potentially crashing the entire system. */
      case Vg_CoreTranslate:
         MAC_(record_jump_error)( tid, bad_addr );
         break;

      default:
         VG_(tool_panic)("mc_check_is_readable: unexpected CorePart");
      }
   }
   VGP_POPCC(VgpCheckMem);
}

static
void mc_check_is_readable_asciiz ( CorePart part, ThreadId tid,
                                   Char* s, Addr str )
{
   MC_ReadResult res;
   Addr bad_addr;
   /* VG_(message)(Vg_DebugMsg,"check is readable asciiz: 0x%x",str); */

   VGP_PUSHCC(VgpCheckMem);

   tl_assert(part == Vg_CoreSysCall);
   res = mc_check_readable_asciiz ( (Addr)str, &bad_addr );
   if (MC_Ok != res) {
      Bool isUnaddr = ( MC_AddrErr == res ? True : False );
      MAC_(record_param_error) ( tid, bad_addr, /*isReg*/False, isUnaddr, s );
   }

   VGP_POPCC(VgpCheckMem);
}


static
void mc_new_mem_startup( Addr a, SizeT len, Bool rr, Bool ww, Bool xx )
{
   /* Ignore the permissions, just make it readable.  Seems to work... */
   DEBUG("mc_new_mem_startup(%p, %llu, rr=%u, ww=%u, xx=%u)\n",
         a,(ULong)len,rr,ww,xx);
   mc_make_readable(a, len);
}

static
void mc_new_mem_heap ( Addr a, SizeT len, Bool is_inited )
{
   if (is_inited) {
      mc_make_readable(a, len);
   } else {
      mc_make_writable(a, len);
   }
}

static
void mc_set_perms (Addr a, SizeT len, Bool rr, Bool ww, Bool xx)
{
   DEBUG("mc_set_perms(%p, %llu, rr=%u ww=%u, xx=%u)\n",
         a, (ULong)len, rr, ww, xx);
   if      (rr) mc_make_readable(a, len);
   else if (ww) mc_make_writable(a, len);
   else         mc_make_noaccess(a, len);
}

static
void mc_post_mem_write(CorePart part, ThreadId tid, Addr a, SizeT len)
{
   mc_make_readable(a, len);
}

/*------------------------------------------------------------*/
/*--- Register event handlers                              ---*/
/*------------------------------------------------------------*/

// When a reg is written, mark the corresponding shadow reg bytes as valid.
static void mc_post_reg_write(CorePart part, ThreadId tid, OffT offset,
                              SizeT size)
{
   UChar area[size];
   VG_(memset)(area, VGM_BYTE_VALID, size);
   VG_(set_shadow_regs_area)( tid, offset, size, area );
}

static void mc_post_reg_write_clientcall(ThreadId tid, OffT offset, SizeT size,
                                         Addr f)
{
   mc_post_reg_write(/*dummy*/0, tid, offset, size);
}

static void mc_pre_reg_read(CorePart part, ThreadId tid, Char* s, OffT offset,
                            SizeT size)
{
   UWord mask;
   UWord sh_reg_contents;
   
   // XXX: the only one at the moment
   tl_assert(Vg_CoreSysCall == part);

   switch (size) {
   case 4:  mask = 0xffffffff; break;
   case 2:  mask = 0xffff;     break;
   case 1:  mask = 0xff;       break;
   default: VG_(tool_panic)("Unhandled size in mc_pre_reg_read");
   }

   VG_(get_shadow_regs_area)( tid, offset, size, (UChar*)&sh_reg_contents );
   if ( VGM_WORD_VALID != (mask & sh_reg_contents) )
      MAC_(record_param_error) ( tid, 0, /*isReg*/True, /*isUnaddr*/False, s );
}

/*------------------------------------------------------------*/
/*--- Functions called directly from generated code.       ---*/
/*------------------------------------------------------------*/

static __inline__ UInt rotateRight16 ( UInt x )
{
   /* Amazingly, gcc turns this into a single rotate insn. */
   return (x >> 16) | (x << 16);
}


static __inline__ UInt shiftRight16 ( UInt x )
{
   return x >> 16;
}


/* Read/write 1/2/4/8 sized V bytes, and emit an address error if
   needed. */

/* MC_(helperc_{LD,ST}V{1,2,4,8}) handle the common case fast.
   Under all other circumstances, it defers to the relevant _SLOWLY
   function, which can handle all situations.
*/

/* ------------------------ Size = 8 ------------------------ */

REGPARM(1)
ULong MC_(helperc_LOADV8) ( Addr a )
{
#  ifdef VG_DEBUG_MEMORY
   return mc_rd_V8_SLOWLY(a);
#  else
   if (IS_4_ALIGNED(a)) {
      UInt    sec_no = shiftRight16(a) & 0xFFFF;
      SecMap* sm     = primary_map[sec_no];
      UInt    a_off  = (a & 0xFFFF) >> 3;
      UChar   abits  = sm->abits[a_off];
      if (abits == VGM_BYTE_VALID) {
         /* a is 8-aligned, mapped, and addressible. */
         UInt v_off = a & 0xFFFF;
         /* LITTLE-ENDIAN */
         UInt vLo   = ((UInt*)(sm->vbyte))[ (v_off >> 2) ];
         UInt vHi   = ((UInt*)(sm->vbyte))[ (v_off >> 2) + 1 ];
         return ( ((ULong)vHi) << 32 ) | ((ULong)vLo);
      } else {
         return mc_rd_V8_SLOWLY(a);
      }
   }
   else
   if (IS_4_ALIGNED(a)) {
      /* LITTLE-ENDIAN */
      UInt vLo =  MC_(helperc_LOADV4)(a+0);
      UInt vHi =  MC_(helperc_LOADV4)(a+4);
      return ( ((ULong)vHi) << 32 ) | ((ULong)vLo);
   }
   else
      return mc_rd_V8_SLOWLY(a);
#  endif
}

REGPARM(1)
void MC_(helperc_STOREV8) ( Addr a, ULong vbytes )
{
#  ifdef VG_DEBUG_MEMORY
   mc_wr_V8_SLOWLY(a, vbytes);
#  else
   if (IS_4_ALIGNED(a)) {
      UInt    sec_no = shiftRight16(a) & 0xFFFF;
      SecMap* sm     = primary_map[sec_no];
      UInt    a_off  = (a & 0xFFFF) >> 3;
      UChar   abits  = sm->abits[a_off];
      if (abits == VGM_BYTE_VALID) {
         /* a is 8-aligned, mapped, and addressible. */
         UInt v_off = a & 0xFFFF;
         UInt vHi = (UInt)(vbytes >> 32);
         UInt vLo = (UInt)vbytes;
         /* LITTLE-ENDIAN */
         ((UInt*)(sm->vbyte))[ (v_off >> 2) ]     = vLo;
         ((UInt*)(sm->vbyte))[ (v_off >> 2) + 1 ] = vHi;
      } else {
         mc_wr_V8_SLOWLY(a, vbytes);
      }
      return;
   }
   else
   if (IS_4_ALIGNED(a)) {
      UInt vHi = (UInt)(vbytes >> 32);
      UInt vLo = (UInt)vbytes;
      /* LITTLE-ENDIAN */
      MC_(helperc_STOREV4)(a+0, vLo);
      MC_(helperc_STOREV4)(a+4, vHi);
      return;
   }
   else
      mc_wr_V8_SLOWLY(a, vbytes);
#  endif
}

/* ------------------------ Size = 4 ------------------------ */

REGPARM(1)
UInt MC_(helperc_LOADV4) ( Addr a )
{
#  ifdef VG_DEBUG_MEMORY
   return mc_rd_V4_SLOWLY(a);
#  else
   UInt    sec_no = rotateRight16(a) & 0x3FFFF;
   SecMap* sm     = primary_map[sec_no];
   UInt    a_off  = (a & 0xFFFF) >> 3;
   UChar   abits  = sm->abits[a_off];
   abits >>= (a & 4);
   abits &= 15;
   PROF_EVENT(60);
   if (abits == VGM_NIBBLE_VALID) {
      /* Handle common case quickly: a is suitably aligned, is mapped,
         and is addressible. */
      UInt v_off = a & 0xFFFF;
      return ((UInt*)(sm->vbyte))[ v_off >> 2 ];
   } else {
      /* Slow but general case. */
      return mc_rd_V4_SLOWLY(a);
   }
#  endif
}

REGPARM(2)
void MC_(helperc_STOREV4) ( Addr a, UInt vbytes )
{
#  ifdef VG_DEBUG_MEMORY
   mc_wr_V4_SLOWLY(a, vbytes);
#  else
   UInt    sec_no = rotateRight16(a) & 0x3FFFF;
   SecMap* sm     = primary_map[sec_no];
   UInt    a_off  = (a & 0xFFFF) >> 3;
   UChar   abits  = sm->abits[a_off];
   abits >>= (a & 4);
   abits &= 15;
   PROF_EVENT(61);
   if (abits == VGM_NIBBLE_VALID) {
      /* Handle common case quickly: a is suitably aligned, is mapped,
         and is addressible. */
      UInt v_off = a & 0xFFFF;
      ((UInt*)(sm->vbyte))[ v_off >> 2 ] = vbytes;
   } else {
      /* Slow but general case. */
      mc_wr_V4_SLOWLY(a, vbytes);
   }
#  endif
}

/* ------------------------ Size = 2 ------------------------ */

REGPARM(1)
UInt MC_(helperc_LOADV2) ( Addr a )
{
#  ifdef VG_DEBUG_MEMORY
   return mc_rd_V2_SLOWLY(a);
#  else
   UInt    sec_no = rotateRight16(a) & 0x1FFFF;
   SecMap* sm     = primary_map[sec_no];
   UInt    a_off  = (a & 0xFFFF) >> 3;
   PROF_EVENT(62);
   if (sm->abits[a_off] == VGM_BYTE_VALID) {
      /* Handle common case quickly. */
      UInt v_off = a & 0xFFFF;
      return 0xFFFF0000 
             |  
             (UInt)( ((UShort*)(sm->vbyte))[ v_off >> 1 ] );
   } else {
      /* Slow but general case. */
      return mc_rd_V2_SLOWLY(a);
   }
#  endif
}

REGPARM(2)
void MC_(helperc_STOREV2) ( Addr a, UInt vbytes )
{
#  ifdef VG_DEBUG_MEMORY
   mc_wr_V2_SLOWLY(a, vbytes);
#  else
   UInt    sec_no = rotateRight16(a) & 0x1FFFF;
   SecMap* sm     = primary_map[sec_no];
   UInt    a_off  = (a & 0xFFFF) >> 3;
   PROF_EVENT(63);
   if (sm->abits[a_off] == VGM_BYTE_VALID) {
      /* Handle common case quickly. */
      UInt v_off = a & 0xFFFF;
      ((UShort*)(sm->vbyte))[ v_off >> 1 ] = vbytes & 0x0000FFFF;
   } else {
      /* Slow but general case. */
      mc_wr_V2_SLOWLY(a, vbytes);
   }
#  endif
}

/* ------------------------ Size = 1 ------------------------ */

REGPARM(1)
UInt MC_(helperc_LOADV1) ( Addr a )
{
#  ifdef VG_DEBUG_MEMORY
   return mc_rd_V1_SLOWLY(a);
#  else
   UInt    sec_no = shiftRight16(a);
   SecMap* sm     = primary_map[sec_no];
   UInt    a_off  = (a & 0xFFFF) >> 3;
   PROF_EVENT(64);
   if (sm->abits[a_off] == VGM_BYTE_VALID) {
      /* Handle common case quickly. */
      UInt v_off = a & 0xFFFF;
      return 0xFFFFFF00
             |
             (UInt)( ((UChar*)(sm->vbyte))[ v_off ] );
   } else {
      /* Slow but general case. */
      return mc_rd_V1_SLOWLY(a);
   }
#  endif
}

REGPARM(2)
void MC_(helperc_STOREV1) ( Addr a, UInt vbytes )
{
#  ifdef VG_DEBUG_MEMORY
   mc_wr_V1_SLOWLY(a, vbytes);
#  else
   UInt    sec_no = shiftRight16(a);
   SecMap* sm     = primary_map[sec_no];
   UInt    a_off  = (a & 0xFFFF) >> 3;
   PROF_EVENT(65);
   if (sm->abits[a_off] == VGM_BYTE_VALID) {
      /* Handle common case quickly. */
      UInt v_off = a & 0xFFFF;
      ((UChar*)(sm->vbyte))[ v_off ] = vbytes & 0x000000FF;
   } else {
      /* Slow but general case. */
      mc_wr_V1_SLOWLY(a, vbytes);
   }
#  endif
}


/*------------------------------------------------------------*/
/*--- Fallback functions to handle cases that the above    ---*/
/*--- VG_(helperc_{LD,ST}V{1,2,4,8}) can't manage.         ---*/
/*------------------------------------------------------------*/

/* ------------------------ Size = 8 ------------------------ */

static ULong mc_rd_V8_SLOWLY ( Addr a )
{
   Bool a0ok, a1ok, a2ok, a3ok, a4ok, a5ok, a6ok, a7ok;
   UInt vb0,  vb1,  vb2,  vb3,  vb4,  vb5,  vb6,  vb7;

   PROF_EVENT(70);

   /* First establish independently the addressibility of the 4 bytes
      involved. */
   a0ok = get_abit(a+0) == VGM_BIT_VALID;
   a1ok = get_abit(a+1) == VGM_BIT_VALID;
   a2ok = get_abit(a+2) == VGM_BIT_VALID;
   a3ok = get_abit(a+3) == VGM_BIT_VALID;
   a4ok = get_abit(a+4) == VGM_BIT_VALID;
   a5ok = get_abit(a+5) == VGM_BIT_VALID;
   a6ok = get_abit(a+6) == VGM_BIT_VALID;
   a7ok = get_abit(a+7) == VGM_BIT_VALID;

   /* Also get the validity bytes for the address. */
   vb0 = (UInt)get_vbyte(a+0);
   vb1 = (UInt)get_vbyte(a+1);
   vb2 = (UInt)get_vbyte(a+2);
   vb3 = (UInt)get_vbyte(a+3);
   vb4 = (UInt)get_vbyte(a+4);
   vb5 = (UInt)get_vbyte(a+5);
   vb6 = (UInt)get_vbyte(a+6);
   vb7 = (UInt)get_vbyte(a+7);

   /* Now distinguish 3 cases */

   /* Case 1: the address is completely valid, so:
      - no addressing error
      - return V bytes as read from memory
   */
   if (a0ok && a1ok && a2ok && a3ok && a4ok && a5ok && a6ok && a7ok) {
      ULong vw = VGM_WORD64_INVALID;
      vw <<= 8; vw |= vb7;
      vw <<= 8; vw |= vb6;
      vw <<= 8; vw |= vb5;
      vw <<= 8; vw |= vb4;
      vw <<= 8; vw |= vb3;
      vw <<= 8; vw |= vb2;
      vw <<= 8; vw |= vb1;
      vw <<= 8; vw |= vb0;
      return vw;
   }

   /* Case 2: the address is completely invalid.  
      - emit addressing error
      - return V word indicating validity.  
      This sounds strange, but if we make loads from invalid addresses 
      give invalid data, we also risk producing a number of confusing
      undefined-value errors later, which confuses the fact that the
      error arose in the first place from an invalid address. 
   */
   /* VG_(printf)("%p (%d %d %d %d)\n", a, a0ok, a1ok, a2ok, a3ok); */
   if (!MAC_(clo_partial_loads_ok) 
       || ((a & 7) != 0)
       || (!a0ok && !a1ok && !a2ok && !a3ok && !a4ok && !a5ok && !a6ok && !a7ok)) {
      MAC_(record_address_error)( VG_(get_current_tid)(), a, 8, False );
      return VGM_WORD64_VALID;
   }

   /* Case 3: the address is partially valid.  
      - no addressing error
      - returned V word is invalid where the address is invalid, 
        and contains V bytes from memory otherwise. 
      Case 3 is only allowed if MC_(clo_partial_loads_ok) is True
      (which is the default), and the address is 4-aligned.  
      If not, Case 2 will have applied.
   */
   tl_assert(MAC_(clo_partial_loads_ok));
   {
      ULong vw = VGM_WORD64_INVALID;
      vw <<= 8; vw |= (a7ok ? vb7 : VGM_BYTE_INVALID);
      vw <<= 8; vw |= (a6ok ? vb6 : VGM_BYTE_INVALID);
      vw <<= 8; vw |= (a5ok ? vb5 : VGM_BYTE_INVALID);
      vw <<= 8; vw |= (a4ok ? vb4 : VGM_BYTE_INVALID);
      vw <<= 8; vw |= (a3ok ? vb3 : VGM_BYTE_INVALID);
      vw <<= 8; vw |= (a2ok ? vb2 : VGM_BYTE_INVALID);
      vw <<= 8; vw |= (a1ok ? vb1 : VGM_BYTE_INVALID);
      vw <<= 8; vw |= (a0ok ? vb0 : VGM_BYTE_INVALID);
      return vw;
   }
}

static void mc_wr_V8_SLOWLY ( Addr a, ULong vbytes )
{
   /* Check the address for validity. */
   Bool aerr = False;
   PROF_EVENT(71);

   if (get_abit(a+0) != VGM_BIT_VALID) aerr = True;
   if (get_abit(a+1) != VGM_BIT_VALID) aerr = True;
   if (get_abit(a+2) != VGM_BIT_VALID) aerr = True;
   if (get_abit(a+3) != VGM_BIT_VALID) aerr = True;
   if (get_abit(a+4) != VGM_BIT_VALID) aerr = True;
   if (get_abit(a+5) != VGM_BIT_VALID) aerr = True;
   if (get_abit(a+6) != VGM_BIT_VALID) aerr = True;
   if (get_abit(a+7) != VGM_BIT_VALID) aerr = True;

   /* Store the V bytes, remembering to do it little-endian-ly. */
   set_vbyte( a+0, vbytes & 0x000000FF ); vbytes >>= 8;
   set_vbyte( a+1, vbytes & 0x000000FF ); vbytes >>= 8;
   set_vbyte( a+2, vbytes & 0x000000FF ); vbytes >>= 8;
   set_vbyte( a+3, vbytes & 0x000000FF ); vbytes >>= 8;
   set_vbyte( a+4, vbytes & 0x000000FF ); vbytes >>= 8;
   set_vbyte( a+5, vbytes & 0x000000FF ); vbytes >>= 8;
   set_vbyte( a+6, vbytes & 0x000000FF ); vbytes >>= 8;
   set_vbyte( a+7, vbytes & 0x000000FF );

   /* If an address error has happened, report it. */
   if (aerr)
      MAC_(record_address_error)( VG_(get_current_tid)(), a, 8, True );
}

/* ------------------------ Size = 4 ------------------------ */

static UInt mc_rd_V4_SLOWLY ( Addr a )
{
   Bool a0ok, a1ok, a2ok, a3ok;
   UInt vb0, vb1, vb2, vb3;

   PROF_EVENT(70);

   /* First establish independently the addressibility of the 4 bytes
      involved. */
   a0ok = get_abit(a+0) == VGM_BIT_VALID;
   a1ok = get_abit(a+1) == VGM_BIT_VALID;
   a2ok = get_abit(a+2) == VGM_BIT_VALID;
   a3ok = get_abit(a+3) == VGM_BIT_VALID;

   /* Also get the validity bytes for the address. */
   vb0 = (UInt)get_vbyte(a+0);
   vb1 = (UInt)get_vbyte(a+1);
   vb2 = (UInt)get_vbyte(a+2);
   vb3 = (UInt)get_vbyte(a+3);

   /* Now distinguish 3 cases */

   /* Case 1: the address is completely valid, so:
      - no addressing error
      - return V bytes as read from memory
   */
   if (a0ok && a1ok && a2ok && a3ok) {
      UInt vw = VGM_WORD_INVALID;
      vw <<= 8; vw |= vb3;
      vw <<= 8; vw |= vb2;
      vw <<= 8; vw |= vb1;
      vw <<= 8; vw |= vb0;
      return vw;
   }

   /* Case 2: the address is completely invalid.  
      - emit addressing error
      - return V word indicating validity.  
      This sounds strange, but if we make loads from invalid addresses 
      give invalid data, we also risk producing a number of confusing
      undefined-value errors later, which confuses the fact that the
      error arose in the first place from an invalid address. 
   */
   /* VG_(printf)("%p (%d %d %d %d)\n", a, a0ok, a1ok, a2ok, a3ok); */
   if (!MAC_(clo_partial_loads_ok) 
       || ((a & 3) != 0)
       || (!a0ok && !a1ok && !a2ok && !a3ok)) {
      MAC_(record_address_error)( VG_(get_current_tid)(), a, 4, False );
      return (VGM_BYTE_VALID << 24) | (VGM_BYTE_VALID << 16) 
             | (VGM_BYTE_VALID << 8) | VGM_BYTE_VALID;
   }

   /* Case 3: the address is partially valid.  
      - no addressing error
      - returned V word is invalid where the address is invalid, 
        and contains V bytes from memory otherwise. 
      Case 3 is only allowed if MC_(clo_partial_loads_ok) is True
      (which is the default), and the address is 4-aligned.  
      If not, Case 2 will have applied.
   */
   tl_assert(MAC_(clo_partial_loads_ok));
   {
      UInt vw = VGM_WORD_INVALID;
      vw <<= 8; vw |= (a3ok ? vb3 : VGM_BYTE_INVALID);
      vw <<= 8; vw |= (a2ok ? vb2 : VGM_BYTE_INVALID);
      vw <<= 8; vw |= (a1ok ? vb1 : VGM_BYTE_INVALID);
      vw <<= 8; vw |= (a0ok ? vb0 : VGM_BYTE_INVALID);
      return vw;
   }
}

static void mc_wr_V4_SLOWLY ( Addr a, UInt vbytes )
{
   /* Check the address for validity. */
   Bool aerr = False;
   PROF_EVENT(71);

   if (get_abit(a+0) != VGM_BIT_VALID) aerr = True;
   if (get_abit(a+1) != VGM_BIT_VALID) aerr = True;
   if (get_abit(a+2) != VGM_BIT_VALID) aerr = True;
   if (get_abit(a+3) != VGM_BIT_VALID) aerr = True;

   /* Store the V bytes, remembering to do it little-endian-ly. */
   set_vbyte( a+0, vbytes & 0x000000FF ); vbytes >>= 8;
   set_vbyte( a+1, vbytes & 0x000000FF ); vbytes >>= 8;
   set_vbyte( a+2, vbytes & 0x000000FF ); vbytes >>= 8;
   set_vbyte( a+3, vbytes & 0x000000FF );

   /* If an address error has happened, report it. */
   if (aerr)
      MAC_(record_address_error)( VG_(get_current_tid)(), a, 4, True );
}

/* ------------------------ Size = 2 ------------------------ */

static UInt mc_rd_V2_SLOWLY ( Addr a )
{
   /* Check the address for validity. */
   UInt vw   = VGM_WORD_INVALID;
   Bool aerr = False;
   PROF_EVENT(72);

   if (get_abit(a+0) != VGM_BIT_VALID) aerr = True;
   if (get_abit(a+1) != VGM_BIT_VALID) aerr = True;

   /* Fetch the V bytes, remembering to do it little-endian-ly. */
   vw <<= 8; vw |= (UInt)get_vbyte(a+1);
   vw <<= 8; vw |= (UInt)get_vbyte(a+0);

   /* If an address error has happened, report it. */
   if (aerr) {
      MAC_(record_address_error)( VG_(get_current_tid)(), a, 2, False );
      vw = (VGM_BYTE_INVALID << 24) | (VGM_BYTE_INVALID << 16) 
           | (VGM_BYTE_VALID << 8) | (VGM_BYTE_VALID);
   }
   return vw;   
}

static void mc_wr_V2_SLOWLY ( Addr a, UInt vbytes )
{
   /* Check the address for validity. */
   Bool aerr = False;
   PROF_EVENT(73);

   if (get_abit(a+0) != VGM_BIT_VALID) aerr = True;
   if (get_abit(a+1) != VGM_BIT_VALID) aerr = True;

   /* Store the V bytes, remembering to do it little-endian-ly. */
   set_vbyte( a+0, vbytes & 0x000000FF ); vbytes >>= 8;
   set_vbyte( a+1, vbytes & 0x000000FF );

   /* If an address error has happened, report it. */
   if (aerr)
      MAC_(record_address_error)( VG_(get_current_tid)(), a, 2, True );
}

/* ------------------------ Size = 1 ------------------------ */

static UInt mc_rd_V1_SLOWLY ( Addr a )
{
   /* Check the address for validity. */
   UInt vw   = VGM_WORD_INVALID;
   Bool aerr = False;
   PROF_EVENT(74);

   if (get_abit(a+0) != VGM_BIT_VALID) aerr = True;

   /* Fetch the V byte. */
   vw <<= 8; vw |= (UInt)get_vbyte(a+0);

   /* If an address error has happened, report it. */
   if (aerr) {
      MAC_(record_address_error)( VG_(get_current_tid)(), a, 1, False );
      vw = (VGM_BYTE_INVALID << 24) | (VGM_BYTE_INVALID << 16) 
           | (VGM_BYTE_INVALID << 8) | (VGM_BYTE_VALID);
   }
   return vw;   
}

static void mc_wr_V1_SLOWLY ( Addr a, UInt vbytes )
{
   /* Check the address for validity. */
   Bool aerr = False;
   PROF_EVENT(75);
   if (get_abit(a+0) != VGM_BIT_VALID) aerr = True;

   /* Store the V bytes, remembering to do it little-endian-ly. */
   set_vbyte( a+0, vbytes & 0x000000FF );

   /* If an address error has happened, report it. */
   if (aerr)
      MAC_(record_address_error)( VG_(get_current_tid)(), a, 1, True );
}


/* ---------------------------------------------------------------------
   Called from generated code, or from the assembly helpers.
   Handlers for value check failures.
   ------------------------------------------------------------------ */

void MC_(helperc_value_check0_fail) ( void )
{
   MC_(record_value_error) ( VG_(get_current_tid)(), 0 );
}

void MC_(helperc_value_check1_fail) ( void )
{
   MC_(record_value_error) ( VG_(get_current_tid)(), 1 );
}

void MC_(helperc_value_check2_fail) ( void )
{
   MC_(record_value_error) ( VG_(get_current_tid)(), 2 );
}

void MC_(helperc_value_check4_fail) ( void )
{
   MC_(record_value_error) ( VG_(get_current_tid)(), 4 );
}

REGPARM(1) void MC_(helperc_complain_undef) ( HWord sz )
{
   MC_(record_value_error) ( VG_(get_current_tid)(), (Int)sz );
}


/*------------------------------------------------------------*/
/*--- Metadata get/set functions, for client requests.     ---*/
/*------------------------------------------------------------*/

/* Copy Vbits for src into vbits. Returns: 1 == OK, 2 == alignment
   error, 3 == addressing error. */
static Int mc_get_or_set_vbits_for_client ( 
   ThreadId tid,
   Addr dataV, 
   Addr vbitsV, 
   SizeT size, 
   Bool setting /* True <=> set vbits,  False <=> get vbits */ 
)
{
   Bool addressibleD = True;
   Bool addressibleV = True;
   UInt* data  = (UInt*)dataV;
   UInt* vbits = (UInt*)vbitsV;
   SizeT szW   = size / 4; /* sigh */
   SizeT i;
   UInt* dataP  = NULL; /* bogus init to keep gcc happy */
   UInt* vbitsP = NULL; /* ditto */

   /* Check alignment of args. */
   if (!(IS_4_ALIGNED(data) && IS_4_ALIGNED(vbits)))
      return 2;
   if ((size & 3) != 0)
      return 2;
  
   /* Check that arrays are addressible. */
   for (i = 0; i < szW; i++) {
      dataP  = &data[i];
      vbitsP = &vbits[i];
      if (get_abits4_ALIGNED((Addr)dataP) != VGM_NIBBLE_VALID) {
         addressibleD = False;
         break;
      }
      if (get_abits4_ALIGNED((Addr)vbitsP) != VGM_NIBBLE_VALID) {
         addressibleV = False;
         break;
      }
   }
   if (!addressibleD) {
      MAC_(record_address_error)( tid, (Addr)dataP, 4, 
                                  setting ? True : False );
      return 3;
   }
   if (!addressibleV) {
      MAC_(record_address_error)( tid, (Addr)vbitsP, 4, 
                                  setting ? False : True );
      return 3;
   }
 
   /* Do the copy */
   if (setting) {
      /* setting */
      for (i = 0; i < szW; i++) {
         if (get_vbytes4_ALIGNED( (Addr)&vbits[i] ) != VGM_WORD_VALID)
            MC_(record_value_error)(tid, 4);
         set_vbytes4_ALIGNED( (Addr)&data[i], vbits[i] );
      }
   } else {
      /* getting */
      for (i = 0; i < szW; i++) {
         vbits[i] = get_vbytes4_ALIGNED( (Addr)&data[i] );
         set_vbytes4_ALIGNED( (Addr)&vbits[i], VGM_WORD_VALID );
      }
   }

   return 1;
}


/*------------------------------------------------------------*/
/*--- Detecting leaked (unreachable) malloc'd blocks.      ---*/
/*------------------------------------------------------------*/

/* For the memory leak detector, say whether an entire 64k chunk of
   address space is possibly in use, or not.  If in doubt return
   True.
*/
static
Bool mc_is_valid_64k_chunk ( UInt chunk_number )
{
   tl_assert(chunk_number >= 0 && chunk_number < 65536);
   if (IS_DISTINGUISHED_SM(primary_map[chunk_number])) {
      /* Definitely not in use. */
      return False;
   } else {
      return True;
   }
}


/* For the memory leak detector, say whether or not a given word
   address is to be regarded as valid. */
static
Bool mc_is_valid_address ( Addr a )
{
   UInt vbytes;
   UChar abits;
   tl_assert(IS_4_ALIGNED(a));
   abits  = get_abits4_ALIGNED(a);
   vbytes = get_vbytes4_ALIGNED(a);
   if (abits == VGM_NIBBLE_VALID && vbytes == VGM_WORD_VALID) {
      return True;
   } else {
      return False;
   }
}


/* Leak detector for this tool.  We don't actually do anything, merely
   run the generic leak detector with suitable parameters for this
   tool. */
static void mc_detect_memory_leaks ( ThreadId tid )
{
   MAC_(do_detect_memory_leaks) ( 
      tid, mc_is_valid_64k_chunk, mc_is_valid_address );
}


/* ---------------------------------------------------------------------
   Sanity check machinery (permanently engaged).
   ------------------------------------------------------------------ */

Bool TL_(cheap_sanity_check) ( void )
{
   /* nothing useful we can rapidly check */
   return True;
}

Bool TL_(expensive_sanity_check) ( void )
{
   Int i;

   /* Make sure nobody changed the distinguished secondary. */
   for (i = 0; i < 8192; i++)
      if (distinguished_secondary_map.abits[i] != VGM_BYTE_INVALID)
         return False;

   for (i = 0; i < 65536; i++)
      if (distinguished_secondary_map.vbyte[i] != VGM_BYTE_INVALID)
         return False;

   /* Make sure that the upper 3/4 of the primary map hasn't
      been messed with. */
   for (i = 65536; i < 262144; i++)
      if (primary_map[i] != & distinguished_secondary_map)
         return False;

   return True;
}
      
/*------------------------------------------------------------*/
/*--- Command line args                                    ---*/
/*------------------------------------------------------------*/

Bool  MC_(clo_avoid_strlen_errors)    = True;

Bool TL_(process_cmd_line_option)(Char* arg)
{
        VG_BOOL_CLO("--avoid-strlen-errors", MC_(clo_avoid_strlen_errors))
   else
      return MAC_(process_common_cmd_line_option)(arg);

   return True;
}

void TL_(print_usage)(void)
{  
   MAC_(print_common_usage)();
   VG_(printf)(
"    --avoid-strlen-errors=no|yes  suppress errs from inlined strlen [yes]\n"
   );
}

void TL_(print_debug_usage)(void)
{  
   MAC_(print_common_debug_usage)();
   VG_(printf)(
"    --cleanup=no|yes          improve after instrumentation? [yes]\n"
   );
}

/*------------------------------------------------------------*/
/*--- Client requests                                      ---*/
/*------------------------------------------------------------*/

/* Client block management:
  
   This is managed as an expanding array of client block descriptors.
   Indices of live descriptors are issued to the client, so it can ask
   to free them later.  Therefore we cannot slide live entries down
   over dead ones.  Instead we must use free/inuse flags and scan for
   an empty slot at allocation time.  This in turn means allocation is
   relatively expensive, so we hope this does not happen too often. 
*/

typedef
   enum { CG_NotInUse, CG_NoAccess, CG_Writable, CG_Readable }
   CGenBlockKind;

typedef
   struct {
      Addr          start;
      SizeT         size;
      ExeContext*   where;
      CGenBlockKind kind;
   } 
   CGenBlock;

/* This subsystem is self-initialising. */
static UInt       vg_cgb_size = 0;
static UInt       vg_cgb_used = 0;
static CGenBlock* vg_cgbs     = NULL;

/* Stats for this subsystem. */
static UInt vg_cgb_used_MAX = 0;   /* Max in use. */
static UInt vg_cgb_allocs   = 0;   /* Number of allocs. */
static UInt vg_cgb_discards = 0;   /* Number of discards. */
static UInt vg_cgb_search   = 0;   /* Number of searches. */


static
Int vg_alloc_client_block ( void )
{
   UInt       i, sz_new;
   CGenBlock* cgbs_new;

   vg_cgb_allocs++;

   for (i = 0; i < vg_cgb_used; i++) {
      vg_cgb_search++;
      if (vg_cgbs[i].kind == CG_NotInUse)
         return i;
   }

   /* Not found.  Try to allocate one at the end. */
   if (vg_cgb_used < vg_cgb_size) {
      vg_cgb_used++;
      return vg_cgb_used-1;
   }

   /* Ok, we have to allocate a new one. */
   tl_assert(vg_cgb_used == vg_cgb_size);
   sz_new = (vg_cgbs == NULL) ? 10 : (2 * vg_cgb_size);

   cgbs_new = VG_(malloc)( sz_new * sizeof(CGenBlock) );
   for (i = 0; i < vg_cgb_used; i++) 
      cgbs_new[i] = vg_cgbs[i];

   if (vg_cgbs != NULL)
      VG_(free)( vg_cgbs );
   vg_cgbs = cgbs_new;

   vg_cgb_size = sz_new;
   vg_cgb_used++;
   if (vg_cgb_used > vg_cgb_used_MAX)
      vg_cgb_used_MAX = vg_cgb_used;
   return vg_cgb_used-1;
}


static void show_client_block_stats ( void )
{
   VG_(message)(Vg_DebugMsg, 
      "general CBs: %d allocs, %d discards, %d maxinuse, %d search",
      vg_cgb_allocs, vg_cgb_discards, vg_cgb_used_MAX, vg_cgb_search 
   );
}

static Bool find_addr(VgHashNode* sh_ch, void* ap)
{
  MAC_Chunk *m = (MAC_Chunk*)sh_ch;
  Addr a = *(Addr*)ap;

  return VG_(addr_is_in_block)(a, m->data, m->size);
}

static Bool client_perm_maybe_describe( Addr a, AddrInfo* ai )
{
   UInt i;
   /* VG_(printf)("try to identify %d\n", a); */

   /* Perhaps it's a general block ? */
   for (i = 0; i < vg_cgb_used; i++) {
      if (vg_cgbs[i].kind == CG_NotInUse) 
         continue;
      if (VG_(addr_is_in_block)(a, vg_cgbs[i].start, vg_cgbs[i].size)) {
         MAC_Mempool **d, *mp;

         /* OK - maybe it's a mempool, too? */
         mp = (MAC_Mempool*)VG_(HT_get_node)(MAC_(mempool_list),
                                             (UWord)vg_cgbs[i].start,
                                             (void*)&d);
         if(mp != NULL) {
            if(mp->chunks != NULL) {
               MAC_Chunk *mc;

               mc = (MAC_Chunk*)VG_(HT_first_match)(mp->chunks, find_addr, &a);
               if(mc != NULL) {
                  ai->akind = UserG;
                  ai->blksize = mc->size;
                  ai->rwoffset = (Int)(a) - (Int)mc->data;
                  ai->lastchange = mc->where;
                  return True;
               }
            }
            ai->akind = Mempool;
            ai->blksize = vg_cgbs[i].size;
            ai->rwoffset  = (Int)(a) - (Int)(vg_cgbs[i].start);
            ai->lastchange = vg_cgbs[i].where;
            return True;
         }
         ai->akind = UserG;
         ai->blksize = vg_cgbs[i].size;
         ai->rwoffset  = (Int)(a) - (Int)(vg_cgbs[i].start);
         ai->lastchange = vg_cgbs[i].where;
         return True;
      }
   }
   return False;
}

Bool TL_(handle_client_request) ( ThreadId tid, UWord* arg, UWord* ret )
{
   Int   i;
   Bool  ok;
   Addr  bad_addr;

   if (!VG_IS_TOOL_USERREQ('M','C',arg[0])
    && VG_USERREQ__MALLOCLIKE_BLOCK != arg[0]
    && VG_USERREQ__FREELIKE_BLOCK   != arg[0]
    && VG_USERREQ__CREATE_MEMPOOL   != arg[0]
    && VG_USERREQ__DESTROY_MEMPOOL  != arg[0]
    && VG_USERREQ__MEMPOOL_ALLOC    != arg[0]
    && VG_USERREQ__MEMPOOL_FREE     != arg[0])
      return False;

   switch (arg[0]) {
      case VG_USERREQ__CHECK_WRITABLE: /* check writable */
         ok = mc_check_writable ( arg[1], arg[2], &bad_addr );
         if (!ok)
            MC_(record_user_error) ( tid, bad_addr, /*isWrite*/True,
                                     /*isUnaddr*/True );
         *ret = ok ? (UWord)NULL : bad_addr;
	 break;

      case VG_USERREQ__CHECK_READABLE: { /* check readable */
         MC_ReadResult res;
         res = mc_check_readable ( arg[1], arg[2], &bad_addr );
         if (MC_AddrErr == res)
            MC_(record_user_error) ( tid, bad_addr, /*isWrite*/False,
                                     /*isUnaddr*/True );
         else if (MC_ValueErr == res)
            MC_(record_user_error) ( tid, bad_addr, /*isWrite*/False,
                                     /*isUnaddr*/False );
         *ret = ( res==MC_Ok ? (UWord)NULL : bad_addr );
	 break;
      }

      case VG_USERREQ__DO_LEAK_CHECK:
         mc_detect_memory_leaks(tid);
	 *ret = 0; /* return value is meaningless */
	 break;

      case VG_USERREQ__MAKE_NOACCESS: /* make no access */
         i = vg_alloc_client_block();
         /* VG_(printf)("allocated %d %p\n", i, vg_cgbs); */
         vg_cgbs[i].kind  = CG_NoAccess;
         vg_cgbs[i].start = arg[1];
         vg_cgbs[i].size  = arg[2];
         vg_cgbs[i].where = VG_(get_ExeContext) ( tid );
         mc_make_noaccess ( arg[1], arg[2] );
	 *ret = i;
	 break;

      case VG_USERREQ__MAKE_WRITABLE: /* make writable */
         i = vg_alloc_client_block();
         vg_cgbs[i].kind  = CG_Writable;
         vg_cgbs[i].start = arg[1];
         vg_cgbs[i].size  = arg[2];
         vg_cgbs[i].where = VG_(get_ExeContext) ( tid );
         mc_make_writable ( arg[1], arg[2] );
         *ret = i;
	 break;

      case VG_USERREQ__MAKE_READABLE: /* make readable */
         i = vg_alloc_client_block();
         vg_cgbs[i].kind  = CG_Readable;
         vg_cgbs[i].start = arg[1];
         vg_cgbs[i].size  = arg[2];
         vg_cgbs[i].where = VG_(get_ExeContext) ( tid );
         mc_make_readable ( arg[1], arg[2] );
	 *ret = i;
         break;

      case VG_USERREQ__DISCARD: /* discard */
         if (vg_cgbs == NULL 
             || arg[2] >= vg_cgb_used || vg_cgbs[arg[2]].kind == CG_NotInUse)
            return 1;
         tl_assert(arg[2] >= 0 && arg[2] < vg_cgb_used);
         vg_cgbs[arg[2]].kind = CG_NotInUse;
         vg_cgb_discards++;
	 *ret = 0;
	 break;

      case VG_USERREQ__GET_VBITS:
         /* Returns: 1 == OK, 2 == alignment error, 3 == addressing
            error. */
         /* VG_(printf)("get_vbits %p %p %d\n", arg[1], arg[2], arg[3] ); */
         *ret = mc_get_or_set_vbits_for_client
                   ( tid, arg[1], arg[2], arg[3], False /* get them */ );
         break;

      case VG_USERREQ__SET_VBITS:
         /* Returns: 1 == OK, 2 == alignment error, 3 == addressing
            error. */
         /* VG_(printf)("set_vbits %p %p %d\n", arg[1], arg[2], arg[3] ); */
         *ret = mc_get_or_set_vbits_for_client
                   ( tid, arg[1], arg[2], arg[3], True /* set them */ );
         break;

      default:
         if (MAC_(handle_common_client_requests)(tid, arg, ret )) {
            return True;
         } else {
            VG_(message)(Vg_UserMsg, 
                         "Warning: unknown memcheck client request code %llx",
                         (ULong)arg[0]);
            return False;
         }
   }
   return True;
}

/*------------------------------------------------------------*/
/*--- Setup                                                ---*/
/*------------------------------------------------------------*/

void TL_(pre_clo_init)(void)
{
   VG_(details_name)            ("Memcheck");
   VG_(details_version)         (NULL);
   VG_(details_description)     ("a memory error detector");
   VG_(details_copyright_author)(
      "Copyright (C) 2002-2004, and GNU GPL'd, by Julian Seward et al.");
   VG_(details_bug_reports_to)  (VG_BUGS_TO);
   VG_(details_avg_translation_sizeB) ( 370 );

   VG_(needs_core_errors)         ();
   VG_(needs_tool_errors)         ();
   VG_(needs_libc_freeres)        ();
   VG_(needs_command_line_options)();
   VG_(needs_client_requests)     ();
   VG_(needs_sanity_checks)       ();
   VG_(needs_shadow_memory)       ();

   MAC_( new_mem_heap)             = & mc_new_mem_heap;
   MAC_( ban_mem_heap)             = & mc_make_noaccess;
   MAC_(copy_mem_heap)             = & mc_copy_address_range_state;
   MAC_( die_mem_heap)             = & mc_make_noaccess;
   MAC_(check_noaccess)            = & mc_check_noaccess;

   VG_(init_new_mem_startup)      ( & mc_new_mem_startup );
   VG_(init_new_mem_stack_signal) ( & mc_make_writable );
   VG_(init_new_mem_brk)          ( & mc_make_writable );
   VG_(init_new_mem_mmap)         ( & mc_set_perms );
   
   VG_(init_copy_mem_remap)       ( & mc_copy_address_range_state );
   VG_(init_change_mem_mprotect)  ( & mc_set_perms );
      
   VG_(init_die_mem_stack_signal) ( & mc_make_noaccess ); 
   VG_(init_die_mem_brk)          ( & mc_make_noaccess );
   VG_(init_die_mem_munmap)       ( & mc_make_noaccess ); 

   VG_(init_new_mem_stack_4)      ( & MAC_(new_mem_stack_4)  );
   VG_(init_new_mem_stack_8)      ( & MAC_(new_mem_stack_8)  );
   VG_(init_new_mem_stack_12)     ( & MAC_(new_mem_stack_12) );
   VG_(init_new_mem_stack_16)     ( & MAC_(new_mem_stack_16) );
   VG_(init_new_mem_stack_32)     ( & MAC_(new_mem_stack_32) );
   VG_(init_new_mem_stack)        ( & MAC_(new_mem_stack)    );

   VG_(init_die_mem_stack_4)      ( & MAC_(die_mem_stack_4)  );
   VG_(init_die_mem_stack_8)      ( & MAC_(die_mem_stack_8)  );
   VG_(init_die_mem_stack_12)     ( & MAC_(die_mem_stack_12) );
   VG_(init_die_mem_stack_16)     ( & MAC_(die_mem_stack_16) );
   VG_(init_die_mem_stack_32)     ( & MAC_(die_mem_stack_32) );
   VG_(init_die_mem_stack)        ( & MAC_(die_mem_stack)    );
   
   VG_(init_ban_mem_stack)        ( & mc_make_noaccess );

   VG_(init_pre_mem_read)         ( & mc_check_is_readable );
   VG_(init_pre_mem_read_asciiz)  ( & mc_check_is_readable_asciiz );
   VG_(init_pre_mem_write)        ( & mc_check_is_writable );
   VG_(init_post_mem_write)       ( & mc_post_mem_write );

   VG_(init_pre_reg_read)         ( & mc_pre_reg_read );

   VG_(init_post_reg_write)                   ( & mc_post_reg_write );
   VG_(init_post_reg_write_clientcall_return) ( & mc_post_reg_write_clientcall );

   VGP_(register_profile_event) ( VgpSetMem,   "set-mem-perms" );
   VGP_(register_profile_event) ( VgpCheckMem, "check-mem-perms" );
   VGP_(register_profile_event) ( VgpESPAdj,   "adjust-ESP" );

   /* Additional block description for VG_(describe_addr)() */
   MAC_(describe_addr_supp) = client_perm_maybe_describe;

   init_shadow_memory();
   MAC_(common_pre_clo_init)();
}

void TL_(post_clo_init) ( void )
{
}

void TL_(fini) ( Int exitcode )
{
   MAC_(common_fini)( mc_detect_memory_leaks );
   
   if (0) {
      VG_(message)(Vg_DebugMsg, 
        "------ Valgrind's client block stats follow ---------------" );
      show_client_block_stats();
   }
}

VG_DETERMINE_INTERFACE_VERSION(TL_(pre_clo_init), 9./8)

/*--------------------------------------------------------------------*/
/*--- end                                                mc_main.c ---*/
/*--------------------------------------------------------------------*/
