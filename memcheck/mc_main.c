
/*--------------------------------------------------------------------*/
/*--- MemCheck: Maintain bitmaps of memory, tracking the           ---*/
/*--- accessibility (A) and validity (V) status of each byte.      ---*/
/*---                                                    mc_main.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of MemCheck, a heavyweight Valgrind tool for
   detecting memory errors.

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

#include "mc_include.h"
#include "memcheck.h"   /* for client requests */
//#include "vg_profile.c"

VG_DETERMINE_INTERFACE_VERSION

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

static UInt mc_rd_V4_SLOWLY ( Addr a );
static UInt mc_rd_V2_SLOWLY ( Addr a );
static UInt mc_rd_V1_SLOWLY ( Addr a );
static void mc_wr_V4_SLOWLY ( Addr a, UInt vbytes );
static void mc_wr_V2_SLOWLY ( Addr a, UInt vbytes );
static void mc_wr_V1_SLOWLY ( Addr a, UInt vbytes );
static void mc_fpu_read_check_SLOWLY ( Addr addr, Int size );
static void mc_fpu_write_check_SLOWLY ( Addr addr, Int size );

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

   /* It just happens that a SecMap occupies exactly 18 pages --
      although this isn't important, so the following assert is
      spurious. */
   sk_assert(0 == (sizeof(SecMap) % VKI_BYTES_PER_PAGE));
   map = VG_(get_memory_from_mmap)( sizeof(SecMap), caller );

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
   sk_assert(IS_ALIGNED4_ADDR(a));
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
   sk_assert(IS_ALIGNED4_ADDR(a));
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
   sk_assert(IS_ALIGNED4_ADDR(a));
#  endif
   ((UInt*)(sm->vbyte))[sm_off >> 2] = vbytes;
}


/*------------------------------------------------------------*/
/*--- Setting permissions over address ranges.             ---*/
/*------------------------------------------------------------*/

static void set_address_range_perms ( Addr a, UInt len, 
                                      UInt example_a_bit,
                                      UInt example_v_bit )
{
   UChar   vbyte, abyte8;
   UInt    vword4, sm_off;
   SecMap* sm;

   PROF_EVENT(30);

   if (len == 0)
      return;

   if (len > 100 * 1000 * 1000) {
      VG_(message)(Vg_UserMsg, 
                   "Warning: set address range perms: "
                   "large range %u, a %d, v %d",
                   len, example_a_bit, example_v_bit );
   }

   VGP_PUSHCC(VgpSetMem);

   /* Requests to change permissions of huge address ranges may
      indicate bugs in our machinery.  30,000,000 is arbitrary, but so
      far all legitimate requests have fallen beneath that size. */
   /* 4 Mar 02: this is just stupid; get rid of it. */
   /* sk_assert(len < 30000000); */

   /* Check the permissions make sense. */
   sk_assert(example_a_bit == VGM_BIT_VALID 
             || example_a_bit == VGM_BIT_INVALID);
   sk_assert(example_v_bit == VGM_BIT_VALID 
             || example_v_bit == VGM_BIT_INVALID);
   if (example_a_bit == VGM_BIT_INVALID)
      sk_assert(example_v_bit == VGM_BIT_INVALID);

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
   sk_assert((a % 8) == 0 && len > 0);

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
   sk_assert((a % 8) == 0 && len > 0 && len < 8);

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
   sk_assert(SK_(cheap_sanity_check)());
   VGP_POPCC(VgpSetMem);
}

/* Set permissions for address ranges ... */

void MC_(make_noaccess) ( Addr a, UInt len )
{
   PROF_EVENT(35);
   DEBUG("MC_(make_noaccess)(%p, %x)\n", a, len);
   set_address_range_perms ( a, len, VGM_BIT_INVALID, VGM_BIT_INVALID );
}

void MC_(make_writable) ( Addr a, UInt len )
{
   PROF_EVENT(36);
   DEBUG("MC_(make_writable)(%p, %x)\n", a, len);
   set_address_range_perms ( a, len, VGM_BIT_VALID, VGM_BIT_INVALID );
}

void MC_(make_readable) ( Addr a, UInt len )
{
   PROF_EVENT(37);
   DEBUG("MC_(make_readable)(%p, 0x%x)\n", a, len);
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
                      MC_(make_writable),
                      MC_(make_noaccess) 
                    );

/* Block-copy permissions (needed for implementing realloc()). */
static void mc_copy_address_range_state ( Addr src, Addr dst, UInt len )
{
   UInt i;

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


/* Check permissions for address range.  If inadequate permissions
   exist, *bad_addr is set to the offending address, so the caller can
   know what it is. */

/* Returns True if [a .. a+len) is not addressible.  Otherwise,
   returns False, and if bad_addr is non-NULL, sets *bad_addr to
   indicate the lowest failing address.  Functions below are
   similar. */
Bool MC_(check_noaccess) ( Addr a, UInt len, Addr* bad_addr )
{
   UInt  i;
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

Bool MC_(check_writable) ( Addr a, UInt len, Addr* bad_addr )
{
   UInt  i;
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

Bool MC_(check_readable) ( Addr a, UInt len, Addr* bad_addr )
{
   UInt  i;
   UChar abit;
   UChar vbyte;

   PROF_EVENT(44);
   DEBUG("MC_(check_readable)\n");
   for (i = 0; i < len; i++) {
      abit  = get_abit(a);
      vbyte = get_vbyte(a);
      PROF_EVENT(45);
      if (abit != VGM_BIT_VALID || vbyte != VGM_BYTE_VALID) {
         if (bad_addr != NULL) *bad_addr = a;
         return False;
      }
      a++;
   }
   return True;
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
      if (abit != VGM_BIT_VALID || vbyte != VGM_BYTE_VALID) {
         if (bad_addr != NULL) *bad_addr = a;
         return False;
      }
      /* Ok, a is safe to read. */
      if (* ((UChar*)a) == 0) return True;
      a++;
   }
}


/*------------------------------------------------------------*/
/*--- Memory event handlers                                ---*/
/*------------------------------------------------------------*/

static
void mc_check_is_writable ( CorePart part, ThreadId tid, Char* s,
                            Addr base, UInt size )
{
   Bool ok;
   Addr bad_addr;

   VGP_PUSHCC(VgpCheckMem);

   /* VG_(message)(Vg_DebugMsg,"check is writable: %x .. %x",
                               base,base+size-1); */
   ok = MC_(check_writable) ( base, size, &bad_addr );
   if (!ok) {
      switch (part) {
      case Vg_CoreSysCall:
         MAC_(record_param_error) ( tid, bad_addr, /*isWrite =*/True, s );
         break;

      case Vg_CorePThread:
      case Vg_CoreSignal:
         MAC_(record_core_mem_error)( tid, /*isWrite=*/True, s );
         break;

      default:
         VG_(skin_panic)("mc_check_is_writable: unexpected CorePart");
      }
   }

   VGP_POPCC(VgpCheckMem);
}

static
void mc_check_is_readable ( CorePart part, ThreadId tid, Char* s,
                            Addr base, UInt size )
{     
   Bool ok;
   Addr bad_addr;

   VGP_PUSHCC(VgpCheckMem);
   
   /* VG_(message)(Vg_DebugMsg,"check is readable: %x .. %x",
                               base,base+size-1); */
   ok = MC_(check_readable) ( base, size, &bad_addr );
   if (!ok) {
      switch (part) {
      case Vg_CoreSysCall:
         MAC_(record_param_error) ( tid, bad_addr, /*isWrite =*/False, s );
         break;
      
      case Vg_CorePThread:
         MAC_(record_core_mem_error)( tid, /*isWrite=*/False, s );
         break;

      /* If we're being asked to jump to a silly address, record an error 
         message before potentially crashing the entire system. */
      case Vg_CoreTranslate:
         MAC_(record_jump_error)( tid, bad_addr );
         break;

      default:
         VG_(skin_panic)("mc_check_is_readable: unexpected CorePart");
      }
   }
   VGP_POPCC(VgpCheckMem);
}

static
void mc_check_is_readable_asciiz ( CorePart part, ThreadId tid,
                                   Char* s, Addr str )
{
   Bool ok = True;
   Addr bad_addr;
   /* VG_(message)(Vg_DebugMsg,"check is readable asciiz: 0x%x",str); */

   VGP_PUSHCC(VgpCheckMem);

   sk_assert(part == Vg_CoreSysCall);
   ok = mc_check_readable_asciiz ( (Addr)str, &bad_addr );
   if (!ok) {
      MAC_(record_param_error) ( tid, bad_addr, /*is_writable =*/False, s );
   }

   VGP_POPCC(VgpCheckMem);
}


static
void mc_new_mem_startup( Addr a, UInt len, Bool rr, Bool ww, Bool xx )
{
   /* Ignore the permissions, just make it readable.  Seems to work... */
   DEBUG("mc_new_mem_startup(%p, %u, rr=%u, ww=%u, xx=%u)\n", a,len,rr,ww,xx);
   MC_(make_readable)(a, len);
}

static
void mc_new_mem_heap ( Addr a, UInt len, Bool is_inited )
{
   if (is_inited) {
      MC_(make_readable)(a, len);
   } else {
      MC_(make_writable)(a, len);
   }
}

static
void mc_set_perms (Addr a, UInt len, Bool rr, Bool ww, Bool xx)
{
   DEBUG("mc_set_perms(%p, %u, rr=%u ww=%u, xx=%u)\n", a, len, rr, ww, xx);
   if      (rr) MC_(make_readable)(a, len);
   else if (ww) MC_(make_writable)(a, len);
   else         MC_(make_noaccess)(a, len);
}


/*------------------------------------------------------------*/
/*--- Register event handlers                              ---*/
/*------------------------------------------------------------*/

static void mc_post_regs_write_init ( void )
{
   UInt i;
   for (i = R_EAX; i <= R_EDI; i++)
      VG_(set_shadow_archreg)( i, VGM_WORD_VALID );
   VG_(set_shadow_eflags)( VGM_EFLAGS_VALID );
}

static void mc_post_reg_write(ThreadId tid, UInt reg)
{
   VG_(set_thread_shadow_archreg)( tid, reg, VGM_WORD_VALID );
}

static void mc_post_reg_write_clientcall(ThreadId tid, UInt reg, Addr f )
{
   VG_(set_thread_shadow_archreg)( tid, reg, VGM_WORD_VALID );
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


/* Read/write 1/2/4 sized V bytes, and emit an address error if
   needed. */

/* VG_(helperc_{LD,ST}V{1,2,4}) handle the common case fast.
   Under all other circumstances, it defers to the relevant _SLOWLY
   function, which can handle all situations.
*/
__attribute__ ((regparm(1)))
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

__attribute__ ((regparm(2)))
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

__attribute__ ((regparm(1)))
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

__attribute__ ((regparm(2)))
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

__attribute__ ((regparm(1)))
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

__attribute__ ((regparm(2)))
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
/*--- VG_(helperc_{LD,ST}V{1,2,4}) can't manage.           ---*/
/*------------------------------------------------------------*/

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
   sk_assert(MAC_(clo_partial_loads_ok));
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


/* ---------------------------------------------------------------------
   FPU load and store checks, called from generated code.
   ------------------------------------------------------------------ */

__attribute__ ((regparm(2)))
void MC_(fpu_read_check) ( Addr addr, Int size )
{
   /* Ensure the read area is both addressible and valid (ie,
      readable).  If there's an address error, don't report a value
      error too; but if there isn't an address error, check for a
      value error. 

      Try to be reasonably fast on the common case; wimp out and defer
      to mc_fpu_read_check_SLOWLY for everything else.  */

   SecMap* sm;
   UInt    sm_off, v_off, a_off;
   Addr    addr4;

   PROF_EVENT(80);

#  ifdef VG_DEBUG_MEMORY
   mc_fpu_read_check_SLOWLY ( addr, size );
#  else

   if (size == 4) {
      if (!IS_ALIGNED4_ADDR(addr)) goto slow4;
      PROF_EVENT(81);
      /* Properly aligned. */
      sm     = primary_map[addr >> 16];
      sm_off = addr & 0xFFFF;
      a_off  = sm_off >> 3;
      if (sm->abits[a_off] != VGM_BYTE_VALID) goto slow4;
      /* Properly aligned and addressible. */
      v_off = addr & 0xFFFF;
      if (((UInt*)(sm->vbyte))[ v_off >> 2 ] != VGM_WORD_VALID) 
         goto slow4;
      /* Properly aligned, addressible and with valid data. */
      return;
     slow4:
      mc_fpu_read_check_SLOWLY ( addr, 4 );
      return;
   }

   if (size == 8) {
      if (!IS_ALIGNED4_ADDR(addr)) goto slow8;
      PROF_EVENT(82);
      /* Properly aligned.  Do it in two halves. */
      addr4 = addr + 4;
      /* First half. */
      sm     = primary_map[addr >> 16];
      sm_off = addr & 0xFFFF;
      a_off  = sm_off >> 3;
      if (sm->abits[a_off] != VGM_BYTE_VALID) goto slow8;
      /* First half properly aligned and addressible. */
      v_off = addr & 0xFFFF;
      if (((UInt*)(sm->vbyte))[ v_off >> 2 ] != VGM_WORD_VALID) 
         goto slow8;
      /* Second half. */
      sm     = primary_map[addr4 >> 16];
      sm_off = addr4 & 0xFFFF;
      a_off  = sm_off >> 3;
      if (sm->abits[a_off] != VGM_BYTE_VALID) goto slow8;
      /* Second half properly aligned and addressible. */
      v_off = addr4 & 0xFFFF;
      if (((UInt*)(sm->vbyte))[ v_off >> 2 ] != VGM_WORD_VALID) 
         goto slow8;
      /* Both halves properly aligned, addressible and with valid
         data. */
      return;
     slow8:
      mc_fpu_read_check_SLOWLY ( addr, 8 );
      return;
   }

   /* Can't be bothered to huff'n'puff to make these (allegedly) rare
      cases go quickly.  */
   if (size == 2) {
      PROF_EVENT(83);
      mc_fpu_read_check_SLOWLY ( addr, 2 );
      return;
   }

   if (size == 16 /*SSE*/ 
       || size == 10 || size == 28 || size == 108) {
      PROF_EVENT(84);
      mc_fpu_read_check_SLOWLY ( addr, size );
      return;
   }

   VG_(printf)("size is %d\n", size);
   VG_(skin_panic)("MC_(fpu_read_check): unhandled size");
#  endif
}


__attribute__ ((regparm(2)))
void MC_(fpu_write_check) ( Addr addr, Int size )
{
   /* Ensure the written area is addressible, and moan if otherwise.
      If it is addressible, make it valid, otherwise invalid. 
   */

   SecMap* sm;
   UInt    sm_off, v_off, a_off;
   Addr    addr4;

   PROF_EVENT(85);

#  ifdef VG_DEBUG_MEMORY
   mc_fpu_write_check_SLOWLY ( addr, size );
#  else

   if (size == 4) {
      if (!IS_ALIGNED4_ADDR(addr)) goto slow4;
      PROF_EVENT(86);
      /* Properly aligned. */
      sm     = primary_map[addr >> 16];
      sm_off = addr & 0xFFFF;
      a_off  = sm_off >> 3;
      if (sm->abits[a_off] != VGM_BYTE_VALID) goto slow4;
      /* Properly aligned and addressible.  Make valid. */
      v_off = addr & 0xFFFF;
      ((UInt*)(sm->vbyte))[ v_off >> 2 ] = VGM_WORD_VALID;
      return;
     slow4:
      mc_fpu_write_check_SLOWLY ( addr, 4 );
      return;
   }

   if (size == 8) {
      if (!IS_ALIGNED4_ADDR(addr)) goto slow8;
      PROF_EVENT(87);
      /* Properly aligned.  Do it in two halves. */
      addr4 = addr + 4;
      /* First half. */
      sm     = primary_map[addr >> 16];
      sm_off = addr & 0xFFFF;
      a_off  = sm_off >> 3;
      if (sm->abits[a_off] != VGM_BYTE_VALID) goto slow8;
      /* First half properly aligned and addressible.  Make valid. */
      v_off = addr & 0xFFFF;
      ((UInt*)(sm->vbyte))[ v_off >> 2 ] = VGM_WORD_VALID;
      /* Second half. */
      sm     = primary_map[addr4 >> 16];
      sm_off = addr4 & 0xFFFF;
      a_off  = sm_off >> 3;
      if (sm->abits[a_off] != VGM_BYTE_VALID) goto slow8;
      /* Second half properly aligned and addressible. */
      v_off = addr4 & 0xFFFF;
      ((UInt*)(sm->vbyte))[ v_off >> 2 ] = VGM_WORD_VALID;
      /* Properly aligned, addressible and with valid data. */
      return;
     slow8:
      mc_fpu_write_check_SLOWLY ( addr, 8 );
      return;
   }

   /* Can't be bothered to huff'n'puff to make these (allegedly) rare
      cases go quickly.  */
   if (size == 2) {
      PROF_EVENT(88);
      mc_fpu_write_check_SLOWLY ( addr, 2 );
      return;
   }

   if (size == 16 /*SSE*/ 
       || size == 10 || size == 28 || size == 108) {
      PROF_EVENT(89);
      mc_fpu_write_check_SLOWLY ( addr, size );
      return;
   }

   VG_(printf)("size is %d\n", size);
   VG_(skin_panic)("MC_(fpu_write_check): unhandled size");
#  endif
}


/* ---------------------------------------------------------------------
   Slow, general cases for FPU load and store checks.
   ------------------------------------------------------------------ */

/* Generic version.  Test for both addr and value errors, but if
   there's an addr error, don't report a value error even if it
   exists. */

void mc_fpu_read_check_SLOWLY ( Addr addr, Int size )
{
   Int  i;
   Bool aerr = False;
   Bool verr = False;
   PROF_EVENT(90);
   for (i = 0; i < size; i++) {
      PROF_EVENT(91);
      if (get_abit(addr+i) != VGM_BIT_VALID)
         aerr = True;
      if (get_vbyte(addr+i) != VGM_BYTE_VALID)
         verr = True;
   }

   if (aerr) {
      MAC_(record_address_error)( VG_(get_current_tid)(), addr, size, False );
   } else {
     if (verr)
        MC_(record_value_error)( VG_(get_current_tid)(), size );
   }
}


/* Generic version.  Test for addr errors.  Valid addresses are
   given valid values, and invalid addresses invalid values. */

void mc_fpu_write_check_SLOWLY ( Addr addr, Int size )
{
   Int  i;
   Addr a_here;
   Bool a_ok;
   Bool aerr = False;
   PROF_EVENT(92);
   for (i = 0; i < size; i++) {
      PROF_EVENT(93);
      a_here = addr+i;
      a_ok = get_abit(a_here) == VGM_BIT_VALID;
      if (a_ok) {
	set_vbyte(a_here, VGM_BYTE_VALID);
      } else {
	set_vbyte(a_here, VGM_BYTE_INVALID);
        aerr = True;
      }
   }
   if (aerr) {
      MAC_(record_address_error)( VG_(get_current_tid)(), addr, size, True );
   }
}


/*------------------------------------------------------------*/
/*--- Metadata get/set functions, for client requests.     ---*/
/*------------------------------------------------------------*/

/* Copy Vbits for src into vbits. Returns: 1 == OK, 2 == alignment
   error, 3 == addressing error. */
Int MC_(get_or_set_vbits_for_client) ( 
   ThreadId tid,
   Addr dataV, 
   Addr vbitsV, 
   UInt size, 
   Bool setting /* True <=> set vbits,  False <=> get vbits */ 
)
{
   Bool addressibleD = True;
   Bool addressibleV = True;
   UInt* data  = (UInt*)dataV;
   UInt* vbits = (UInt*)vbitsV;
   UInt  szW   = size / 4; /* sigh */
   UInt  i;
   UInt* dataP  = NULL; /* bogus init to keep gcc happy */
   UInt* vbitsP = NULL; /* ditto */

   /* Check alignment of args. */
   if (!(IS_ALIGNED4_ADDR(data) && IS_ALIGNED4_ADDR(vbits)))
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
   sk_assert(chunk_number >= 0 && chunk_number < 65536);
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
   sk_assert(IS_ALIGNED4_ADDR(a));
   abits  = get_abits4_ALIGNED(a);
   vbytes = get_vbytes4_ALIGNED(a);
   if (abits == VGM_NIBBLE_VALID && vbytes == VGM_WORD_VALID) {
      return True;
   } else {
      return False;
   }
}


/* Leak detector for this skin.  We don't actually do anything, merely
   run the generic leak detector with suitable parameters for this
   skin. */
void MC_(detect_memory_leaks) ( void )
{
   MAC_(do_detect_memory_leaks) ( mc_is_valid_64k_chunk, mc_is_valid_address );
}


/* ---------------------------------------------------------------------
   Sanity check machinery (permanently engaged).
   ------------------------------------------------------------------ */

/* Check that nobody has spuriously claimed that the first or last 16
   pages (64 KB) of address space have become accessible.  Failure of
   the following do not per se indicate an internal consistency
   problem, but they are so likely to that we really want to know
   about it if so. */

Bool SK_(cheap_sanity_check) ( void )
{
   if (IS_DISTINGUISHED_SM(primary_map[0])
       /* kludge: kernel drops a page up at top of address range for
          magic "optimized syscalls", so we can no longer check the
          highest page */
       /* && IS_DISTINGUISHED_SM(primary_map[65535]) */
      )
      return True;
   else
      return False;
}

Bool SK_(expensive_sanity_check) ( void )
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
      
/* ---------------------------------------------------------------------
   Debugging machinery (turn on to debug).  Something of a mess.
   ------------------------------------------------------------------ */

#if 0
/* Print the value tags on the 8 integer registers & flag reg. */

static void uint_to_bits ( UInt x, Char* str )
{
   Int i;
   Int w = 0;
   /* str must point to a space of at least 36 bytes. */
   for (i = 31; i >= 0; i--) {
      str[w++] = (x & ( ((UInt)1) << i)) ? '1' : '0';
      if (i == 24 || i == 16 || i == 8)
         str[w++] = ' ';
   }
   str[w++] = 0;
   sk_assert(w == 36);
}

/* Caution!  Not vthread-safe; looks in VG_(baseBlock), not the thread
   state table. */

static void vg_show_reg_tags ( void )
{
   Char buf1[36];
   Char buf2[36];
   UInt z_eax, z_ebx, z_ecx, z_edx, 
        z_esi, z_edi, z_ebp, z_esp, z_eflags;

   z_eax    = VG_(baseBlock)[VGOFF_(sh_eax)];
   z_ebx    = VG_(baseBlock)[VGOFF_(sh_ebx)];
   z_ecx    = VG_(baseBlock)[VGOFF_(sh_ecx)];
   z_edx    = VG_(baseBlock)[VGOFF_(sh_edx)];
   z_esi    = VG_(baseBlock)[VGOFF_(sh_esi)];
   z_edi    = VG_(baseBlock)[VGOFF_(sh_edi)];
   z_ebp    = VG_(baseBlock)[VGOFF_(sh_ebp)];
   z_esp    = VG_(baseBlock)[VGOFF_(sh_esp)];
   z_eflags = VG_(baseBlock)[VGOFF_(sh_eflags)];
   
   uint_to_bits(z_eflags, buf1);
   VG_(message)(Vg_DebugMsg, "efl %s\n", buf1);

   uint_to_bits(z_eax, buf1);
   uint_to_bits(z_ebx, buf2);
   VG_(message)(Vg_DebugMsg, "eax %s   ebx %s\n", buf1, buf2);

   uint_to_bits(z_ecx, buf1);
   uint_to_bits(z_edx, buf2);
   VG_(message)(Vg_DebugMsg, "ecx %s   edx %s\n", buf1, buf2);

   uint_to_bits(z_esi, buf1);
   uint_to_bits(z_edi, buf2);
   VG_(message)(Vg_DebugMsg, "esi %s   edi %s\n", buf1, buf2);

   uint_to_bits(z_ebp, buf1);
   uint_to_bits(z_esp, buf2);
   VG_(message)(Vg_DebugMsg, "ebp %s   esp %s\n", buf1, buf2);
}


/* For debugging only.  Scan the address space and touch all allegedly
   addressible words.  Useful for establishing where Valgrind's idea of
   addressibility has diverged from what the kernel believes. */

static 
void zzzmemscan_notify_word ( Addr a, UInt w )
{
}

void zzzmemscan ( void )
{
   Int n_notifies
      = VG_(scan_all_valid_memory)( zzzmemscan_notify_word );
   VG_(printf)("zzzmemscan: n_bytes = %d\n", 4 * n_notifies );
}
#endif




#if 0
static Int zzz = 0;

void show_bb ( Addr eip_next )
{
   VG_(printf)("[%4d] ", zzz);
   vg_show_reg_tags( &VG_(m_shadow );
   VG_(translate) ( eip_next, NULL, NULL, NULL );
}
#endif /* 0 */


/*------------------------------------------------------------*/
/*--- Command line args                                    ---*/
/*------------------------------------------------------------*/

Bool  MC_(clo_avoid_strlen_errors)    = True;
Bool  MC_(clo_cleanup)                = True;

Bool SK_(process_cmd_line_option)(Char* arg)
{
   if (VG_CLO_STREQ(arg, "--avoid-strlen-errors=yes"))
      MC_(clo_avoid_strlen_errors) = True;
   else if (VG_CLO_STREQ(arg, "--avoid-strlen-errors=no"))
      MC_(clo_avoid_strlen_errors) = False;

   else if (VG_CLO_STREQ(arg, "--cleanup=yes"))
      MC_(clo_cleanup) = True;
   else if (VG_CLO_STREQ(arg, "--cleanup=no"))
      MC_(clo_cleanup) = False;

   else
      return MAC_(process_common_cmd_line_option)(arg);

   return True;
}

void SK_(print_usage)(void)
{  
   MAC_(print_common_usage)();
   VG_(printf)(
"    --avoid-strlen-errors=no|yes  suppress errs from inlined strlen [yes]\n"
   );
}

void SK_(print_debug_usage)(void)
{  
   MAC_(print_common_debug_usage)();
   VG_(printf)(
"    --cleanup=no|yes          improve after instrumentation? [yes]\n"
   );
}


/*------------------------------------------------------------*/
/*--- Setup                                                ---*/
/*------------------------------------------------------------*/

void SK_(pre_clo_init)(void)
{
   VG_(details_name)            ("Memcheck");
   VG_(details_version)         (NULL);
   VG_(details_description)     ("a memory error detector");
   VG_(details_copyright_author)(
      "Copyright (C) 2002-2003, and GNU GPL'd, by Julian Seward.");
   VG_(details_bug_reports_to)  ("jseward@acm.org");
   VG_(details_avg_translation_sizeB) ( 228 );

   VG_(needs_core_errors)         ();
   VG_(needs_skin_errors)         ();
   VG_(needs_libc_freeres)        ();
   VG_(needs_shadow_regs)         ();
   VG_(needs_command_line_options)();
   VG_(needs_client_requests)     ();
   VG_(needs_extended_UCode)      ();
   VG_(needs_syscall_wrapper)     ();
   VG_(needs_sanity_checks)       ();

   MAC_( new_mem_heap)             = & mc_new_mem_heap;
   MAC_( ban_mem_heap)             = & MC_(make_noaccess);
   MAC_(copy_mem_heap)             = & mc_copy_address_range_state;
   MAC_( die_mem_heap)             = & MC_(make_noaccess);
   MAC_(check_noaccess)            = & MC_(check_noaccess);

   VG_(track_new_mem_startup)      ( & mc_new_mem_startup );
   VG_(track_new_mem_stack_signal) ( & MC_(make_writable) );
   VG_(track_new_mem_brk)          ( & MC_(make_writable) );
   VG_(track_new_mem_mmap)         ( & mc_set_perms );
   
   VG_(track_copy_mem_remap)       ( & mc_copy_address_range_state );
   VG_(track_change_mem_mprotect)  ( & mc_set_perms );
      
   VG_(track_die_mem_stack_signal) ( & MC_(make_noaccess) ); 
   VG_(track_die_mem_brk)          ( & MC_(make_noaccess) );
   VG_(track_die_mem_munmap)       ( & MC_(make_noaccess) ); 

   VG_(track_new_mem_stack_4)      ( & MAC_(new_mem_stack_4)  );
   VG_(track_new_mem_stack_8)      ( & MAC_(new_mem_stack_8)  );
   VG_(track_new_mem_stack_12)     ( & MAC_(new_mem_stack_12) );
   VG_(track_new_mem_stack_16)     ( & MAC_(new_mem_stack_16) );
   VG_(track_new_mem_stack_32)     ( & MAC_(new_mem_stack_32) );
   VG_(track_new_mem_stack)        ( & MAC_(new_mem_stack)    );

   VG_(track_die_mem_stack_4)      ( & MAC_(die_mem_stack_4)  );
   VG_(track_die_mem_stack_8)      ( & MAC_(die_mem_stack_8)  );
   VG_(track_die_mem_stack_12)     ( & MAC_(die_mem_stack_12) );
   VG_(track_die_mem_stack_16)     ( & MAC_(die_mem_stack_16) );
   VG_(track_die_mem_stack_32)     ( & MAC_(die_mem_stack_32) );
   VG_(track_die_mem_stack)        ( & MAC_(die_mem_stack)    );
   
   VG_(track_ban_mem_stack)        ( & MC_(make_noaccess) );

   VG_(track_pre_mem_read)         ( & mc_check_is_readable );
   VG_(track_pre_mem_read_asciiz)  ( & mc_check_is_readable_asciiz );
   VG_(track_pre_mem_write)        ( & mc_check_is_writable );
   VG_(track_post_mem_write)       ( & MC_(make_readable) );

   VG_(track_post_regs_write_init)             ( & mc_post_regs_write_init );
   VG_(track_post_reg_write_syscall_return)    ( & mc_post_reg_write );
   VG_(track_post_reg_write_deliver_signal)    ( & mc_post_reg_write );
   VG_(track_post_reg_write_pthread_return)    ( & mc_post_reg_write );
   VG_(track_post_reg_write_clientreq_return)  ( & mc_post_reg_write );
   VG_(track_post_reg_write_clientcall_return) ( & mc_post_reg_write_clientcall );

   /* Three compact slots taken up by stack memory helpers */
   VG_(register_compact_helper)((Addr) & MC_(helper_value_check4_fail));
   VG_(register_compact_helper)((Addr) & MC_(helper_value_check0_fail));
   VG_(register_compact_helper)((Addr) & MC_(helper_value_check2_fail));
   VG_(register_compact_helper)((Addr) & MC_(helperc_STOREV4));
   VG_(register_compact_helper)((Addr) & MC_(helperc_LOADV4));

   /* These two made non-compact because 2-byte transactions are rare. */
   VG_(register_noncompact_helper)((Addr) & MC_(helperc_STOREV2));
   VG_(register_noncompact_helper)((Addr) & MC_(helperc_STOREV1));
   VG_(register_noncompact_helper)((Addr) & MC_(helperc_LOADV2));
   VG_(register_noncompact_helper)((Addr) & MC_(helperc_LOADV1));
   VG_(register_noncompact_helper)((Addr) & MC_(fpu_write_check));
   VG_(register_noncompact_helper)((Addr) & MC_(fpu_read_check));
   VG_(register_noncompact_helper)((Addr) & MC_(helper_value_check1_fail));

   VGP_(register_profile_event) ( VgpSetMem,   "set-mem-perms" );
   VGP_(register_profile_event) ( VgpCheckMem, "check-mem-perms" );
   VGP_(register_profile_event) ( VgpESPAdj,   "adjust-ESP" );

   /* Additional block description for VG_(describe_addr)() */
   MAC_(describe_addr_supp) = MC_(client_perm_maybe_describe);

   init_shadow_memory();
   MAC_(common_pre_clo_init)();
}

void SK_(post_clo_init) ( void )
{
}

void SK_(fini) ( Int exitcode )
{
   MAC_(common_fini)( MC_(detect_memory_leaks) );
   
   if (0) {
      VG_(message)(Vg_DebugMsg, 
        "------ Valgrind's client block stats follow ---------------" );
      MC_(show_client_block_stats)();
   }
}

/*--------------------------------------------------------------------*/
/*--- end                                                mc_main.c ---*/
/*--------------------------------------------------------------------*/
