
/*--------------------------------------------------------------------*/
/*--- The AddrCheck skin: like MemCheck, but only does address     ---*/
/*--- checking.  No definedness checking.                          ---*/
/*---                                                    ac_main.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of AddrCheck, a lightweight Valgrind skin for
   detecting memory errors.

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

#include "ac_include.h"
//#include "vg_profile.c"

/*------------------------------------------------------------*/
/*--- Defns                                                ---*/
/*------------------------------------------------------------*/

/* These many bytes below %ESP are considered addressible if we're
   doing the --workaround-gcc296-bugs hack. */
#define VG_GCC296_BUG_STACK_SLOP 1024


typedef 
   enum { 
      /* Bad syscall params */
      ParamSupp,
      /* Memory errors in core (pthread ops, signal handling) */
      CoreMemSupp,
      /* Invalid read/write attempt at given size */
      Addr1Supp, Addr2Supp, Addr4Supp, Addr8Supp,
      /* Invalid or mismatching free */
      FreeSupp
   } 
   AddrCheckSuppKind;

/* What kind of error it is. */
typedef 
   enum { CoreMemErr,
          AddrErr, 
          ParamErr, UserErr,  /* behaves like an anonymous ParamErr */
          FreeErr, FreeMismatchErr
   }
   AddrCheckErrorKind;

/* What kind of memory access is involved in the error? */
typedef
   enum { ReadAxs, WriteAxs, ExecAxs }
   AxsKind;

/* Extra context for memory errors */
typedef
   struct {
      /* AddrErr */
      AxsKind axskind;
      /* AddrErr */
      Int size;
      /* AddrErr, FreeErr, FreeMismatchErr, ParamErr, UserErr */
      AcAddrInfo addrinfo;
      /* ParamErr, UserErr, CoreMemErr */
      Bool isWrite;
   }
   AddrCheckError;

/*------------------------------------------------------------*/
/*--- Comparing and printing errors                        ---*/
/*------------------------------------------------------------*/

static __inline__
void clear_AcAddrInfo ( AcAddrInfo* ai )
{
   ai->akind      = Unknown;
   ai->blksize    = 0;
   ai->rwoffset   = 0;
   ai->lastchange = NULL;
   ai->stack_tid  = VG_INVALID_THREADID;
   ai->maybe_gcc  = False;
}

static __inline__
void clear_AddrCheckError ( AddrCheckError* err_extra )
{
   err_extra->axskind   = ReadAxs;
   err_extra->size      = 0;
   clear_AcAddrInfo ( &err_extra->addrinfo );
   err_extra->isWrite   = False;
}

__attribute__((unused))
static Bool eq_AcAddrInfo ( VgRes res, AcAddrInfo* ai1, AcAddrInfo* ai2 )
{
   if (ai1->akind != Undescribed 
       && ai2->akind != Undescribed
       && ai1->akind != ai2->akind) 
      return False;
   if (ai1->akind == Freed || ai1->akind == Mallocd) {
      if (ai1->blksize != ai2->blksize)
         return False;
      if (!VG_(eq_ExeContext)(res, ai1->lastchange, ai2->lastchange))
         return False;
   }
   return True;
}

/* Compare error contexts, to detect duplicates.  Note that if they
   are otherwise the same, the faulting addrs and associated rwoffsets
   are allowed to be different.  */

Bool SK_(eq_SkinError) ( VgRes res,
                         SkinError* e1, SkinError* e2 )
{
   AddrCheckError* e1_extra = e1->extra;
   AddrCheckError* e2_extra = e2->extra;
   
   switch (e1->ekind) {
      case CoreMemErr:
         if (e1_extra->isWrite != e2_extra->isWrite)   return False;
         if (e2->ekind != CoreMemErr)                  return False; 
         if (e1->string == e2->string)                 return True;
         if (0 == VG_(strcmp)(e1->string, e2->string)) return True;
         return False;

      case UserErr:
      case ParamErr:
         if (e1_extra->isWrite != e2_extra->isWrite)
            return False;
         if (e1->ekind == ParamErr 
             && 0 != VG_(strcmp)(e1->string, e2->string))
            return False;
         return True;

      case FreeErr:
      case FreeMismatchErr:
         /* JRS 2002-Aug-26: comparing addrs seems overkill and can
            cause excessive duplication of errors.  Not even AddrErr
            below does that.  So don't compare either the .addr field
            or the .addrinfo fields. */
         /* if (e1->addr != e2->addr) return False; */
         /* if (!eq_AcAddrInfo(res, &e1_extra->addrinfo, &e2_extra->addrinfo)) 
               return False;
         */
         return True;

      case AddrErr:
         /* if (e1_extra->axskind != e2_extra->axskind) return False; */
         if (e1_extra->size != e2_extra->size) return False;
         /*
         if (!eq_AcAddrInfo(res, &e1_extra->addrinfo, &e2_extra->addrinfo)) 
            return False;
         */
         return True;

      default: 
         VG_(printf)("Error:\n  unknown AddrCheck error code %d\n", e1->ekind);
         VG_(skin_panic)("unknown error code in SK_(eq_SkinError)");
   }
}

static void pp_AcAddrInfo ( Addr a, AcAddrInfo* ai )
{
   switch (ai->akind) {
      case Stack: 
         VG_(message)(Vg_UserMsg, 
                      "   Address 0x%x is on thread %d's stack", 
                      a, ai->stack_tid);
         break;
      case Unknown:
         if (ai->maybe_gcc) {
            VG_(message)(Vg_UserMsg, 
               "   Address 0x%x is just below %%esp.  Possibly a bug in GCC/G++",
               a);
            VG_(message)(Vg_UserMsg, 
               "   v 2.96 or 3.0.X.  To suppress, use: --workaround-gcc296-bugs=yes");
	 } else {
            VG_(message)(Vg_UserMsg, 
               "   Address 0x%x is not stack'd, malloc'd or free'd", a);
         }
         break;
      case Freed: case Mallocd: {
         UInt delta;
         UChar* relative;
         if (ai->rwoffset < 0) {
            delta    = (UInt)(- ai->rwoffset);
            relative = "before";
         } else if (ai->rwoffset >= ai->blksize) {
            delta    = ai->rwoffset - ai->blksize;
            relative = "after";
         } else {
            delta    = ai->rwoffset;
            relative = "inside";
         }
         {
            VG_(message)(Vg_UserMsg, 
               "   Address 0x%x is %d bytes %s a block of size %d %s",
               a, delta, relative, 
               ai->blksize,
               ai->akind==Mallocd ? "alloc'd" 
                  : ai->akind==Freed ? "free'd" 
                                     : "client-defined");
         }
         VG_(pp_ExeContext)(ai->lastchange);
         break;
      }
      default:
         VG_(skin_panic)("pp_AcAddrInfo");
   }
}

void SK_(pp_SkinError) ( SkinError* err, void (*pp_ExeContext)(void) )
{
   AddrCheckError* err_extra = err->extra;

   switch (err->ekind) {
      case CoreMemErr:
         if (err_extra->isWrite) {
            VG_(message)(Vg_UserMsg, 
               "%s contains unaddressable byte(s)", err->string );
         } else {
            VG_(message)(Vg_UserMsg, 
               "%s contains unaddressable byte(s)", err->string );
         }
         pp_ExeContext();
         break;
      
      case AddrErr:
         switch (err_extra->axskind) {
            case ReadAxs:
            case WriteAxs:
               /* These two aren't actually differentiated ever. */
               VG_(message)(Vg_UserMsg, "Invalid memory access of size %d", 
                                        err_extra->size ); 
               break;
            case ExecAxs:
               VG_(message)(Vg_UserMsg, "Jump to the invalid address "
                                        "stated on the next line");
               break;
            default: 
               VG_(skin_panic)("pp_SkinError(axskind)");
         }
         pp_ExeContext();
         pp_AcAddrInfo(err->addr, &err_extra->addrinfo);
         break;

      case FreeErr:
         VG_(message)(Vg_UserMsg,"Invalid free() / delete / delete[]");
         /* fall through */
      case FreeMismatchErr:
         if (err->ekind == FreeMismatchErr)
            VG_(message)(Vg_UserMsg, 
                         "Mismatched free() / delete / delete []");
         pp_ExeContext();
         pp_AcAddrInfo(err->addr, &err_extra->addrinfo);
         break;

      case ParamErr:
         if (err_extra->isWrite) {
            VG_(message)(Vg_UserMsg, 
               "Syscall param %s contains unaddressable byte(s)",
                err->string );
         } else {
            VG_(message)(Vg_UserMsg, 
                "Syscall param %s contains uninitialised or "
                "unaddressable byte(s)",
            err->string);
         }
         pp_ExeContext();
         pp_AcAddrInfo(err->addr, &err_extra->addrinfo);
         break;

      case UserErr:
         if (err_extra->isWrite) {
            VG_(message)(Vg_UserMsg, 
               "Unaddressable byte(s) found during client check request");
         } else {
            VG_(message)(Vg_UserMsg, 
               "Uninitialised or "
               "unaddressable byte(s) found during client check request");
         }
         pp_ExeContext();
         pp_AcAddrInfo(err->addr, &err_extra->addrinfo);
         break;

      default: 
         VG_(printf)("Error:\n  unknown AddrCheck error code %d\n", err->ekind);
         VG_(skin_panic)("unknown error code in SK_(pp_SkinError)");
   }
}

/*------------------------------------------------------------*/
/*--- Recording errors                                     ---*/
/*------------------------------------------------------------*/

/* Describe an address as best you can, for error messages,
   putting the result in ai. */

static void describe_addr ( Addr a, AcAddrInfo* ai )
{
   ShadowChunk* sc;
   ThreadId     tid;

   /* Nested functions, yeah.  Need the lexical scoping of 'a'. */ 

   /* Closure for searching thread stacks */
   Bool addr_is_in_bounds(Addr stack_min, Addr stack_max)
   {
      return (stack_min <= a && a <= stack_max);
   }
   /* Closure for searching malloc'd and free'd lists */
   Bool addr_is_in_block(ShadowChunk *sh_ch)
   {
      return VG_(addr_is_in_block) ( a, sh_ch->data, sh_ch->size );
   }
   /* Perhaps it's on a thread's stack? */
   tid = VG_(any_matching_thread_stack)(addr_is_in_bounds);
   if (tid != VG_INVALID_THREADID) {
      ai->akind     = Stack;
      ai->stack_tid = tid;
      return;
   }
   /* Search for a recently freed block which might bracket it. */
   sc = SK_(any_matching_freed_ShadowChunks)(addr_is_in_block);
   if (NULL != sc) {
      ai->akind      = Freed;
      ai->blksize    = sc->size;
      ai->rwoffset   = (Int)(a) - (Int)(sc->data);
      ai->lastchange = (ExeContext*)sc->skin_extra[0];
      return;
   }
   /* Search for a currently malloc'd block which might bracket it. */
   sc = VG_(any_matching_mallocd_ShadowChunks)(addr_is_in_block);
   if (NULL != sc) {
      ai->akind      = Mallocd;
      ai->blksize    = sc->size;
      ai->rwoffset   = (Int)(a) - (Int)(sc->data);
      ai->lastchange = (ExeContext*)sc->skin_extra[0];
      return;
   } 
   /* Clueless ... */
   ai->akind = Unknown;
   return;
}


/* Creates a copy of the err_extra, updates the copy with address info if
   necessary, sticks the copy into the SkinError. */
void SK_(dup_extra_and_update)(SkinError* err)
{
   AddrCheckError* err_extra;

   err_extra  = VG_(malloc)(sizeof(AddrCheckError));
   *err_extra = *((AddrCheckError*)err->extra);

   if (err_extra->addrinfo.akind == Undescribed)
      describe_addr ( err->addr, &(err_extra->addrinfo) );

   err->extra = err_extra;
}

/* Is this address within some small distance below %ESP?  Used only
   for the --workaround-gcc296-bugs kludge. */
Bool VG_(is_just_below_ESP)( Addr esp, Addr aa )
{
   if ((UInt)esp > (UInt)aa
       && ((UInt)esp - (UInt)aa) <= VG_GCC296_BUG_STACK_SLOP)
      return True;
   else
      return False;
}

static
void sk_record_address_error ( Addr a, Int size, Bool isWrite )
{
   AddrCheckError err_extra;
   Bool           just_below_esp;

   just_below_esp 
      = VG_(is_just_below_ESP)( VG_(get_stack_pointer)(), a );

   /* If this is caused by an access immediately below %ESP, and the
      user asks nicely, we just ignore it. */
   if (SK_(clo_workaround_gcc296_bugs) && just_below_esp)
      return;

   clear_AddrCheckError( &err_extra );
   err_extra.axskind = isWrite ? WriteAxs : ReadAxs;
   err_extra.size    = size;
   err_extra.addrinfo.akind     = Undescribed;
   err_extra.addrinfo.maybe_gcc = just_below_esp;
   VG_(maybe_record_error)( NULL, AddrErr, a, /*s*/NULL, &err_extra );
}

/* These ones are called from non-generated code */

/* This is for memory errors in pthread functions, as opposed to pthread API
   errors which are found by the core. */
void SK_(record_core_mem_error) ( ThreadState* tst, Bool isWrite, Char* msg )
{
   AddrCheckError err_extra;

   clear_AddrCheckError( &err_extra );
   err_extra.isWrite = isWrite;
   VG_(maybe_record_error)( tst, CoreMemErr, /*addr*/0, msg, &err_extra );
}

void SK_(record_param_error) ( ThreadState* tst, Addr a, Bool isWrite, 
                               Char* msg )
{
   AddrCheckError err_extra;

   sk_assert(NULL != tst);
   clear_AddrCheckError( &err_extra );
   err_extra.addrinfo.akind = Undescribed;
   err_extra.isWrite = isWrite;
   VG_(maybe_record_error)( tst, ParamErr, a, msg, &err_extra );
}

void SK_(record_jump_error) ( ThreadState* tst, Addr a )
{
   AddrCheckError err_extra;

   sk_assert(NULL != tst);

   clear_AddrCheckError( &err_extra );
   err_extra.axskind = ExecAxs;
   err_extra.addrinfo.akind = Undescribed;
   VG_(maybe_record_error)( tst, AddrErr, a, /*s*/NULL, &err_extra );
}

void SK_(record_free_error) ( ThreadState* tst, Addr a ) 
{
   AddrCheckError err_extra;

   sk_assert(NULL != tst);

   clear_AddrCheckError( &err_extra );
   err_extra.addrinfo.akind = Undescribed;
   VG_(maybe_record_error)( tst, FreeErr, a, /*s*/NULL, &err_extra );
}

void SK_(record_freemismatch_error) ( ThreadState* tst, Addr a )
{
   AddrCheckError err_extra;

   sk_assert(NULL != tst);

   clear_AddrCheckError( &err_extra );
   err_extra.addrinfo.akind = Undescribed;
   VG_(maybe_record_error)( tst, FreeMismatchErr, a, /*s*/NULL, &err_extra );
}

void SK_(record_user_error) ( ThreadState* tst, Addr a, Bool isWrite )
{
   AddrCheckError err_extra;

   sk_assert(NULL != tst);

   clear_AddrCheckError( &err_extra );
   err_extra.addrinfo.akind = Undescribed;
   err_extra.isWrite        = isWrite;
   VG_(maybe_record_error)( tst, UserErr, a, /*s*/NULL, &err_extra );
}


/*------------------------------------------------------------*/
/*--- Suppressions                                         ---*/
/*------------------------------------------------------------*/

#define STREQ(s1,s2) (s1 != NULL && s2 != NULL \
                      && VG_(strcmp)((s1),(s2))==0)

Bool SK_(recognised_suppression) ( Char* name, SuppKind *skind )
{
   if      (STREQ(name, "Param"))   *skind = ParamSupp;
   else if (STREQ(name, "CoreMem")) *skind = CoreMemSupp;
   else if (STREQ(name, "Addr1"))   *skind = Addr1Supp;
   else if (STREQ(name, "Addr2"))   *skind = Addr2Supp;
   else if (STREQ(name, "Addr4"))   *skind = Addr4Supp;
   else if (STREQ(name, "Addr8"))   *skind = Addr8Supp;
   else if (STREQ(name, "Free"))    *skind = FreeSupp;
   else 
      return False;

   return True;
}

Bool SK_(read_extra_suppression_info) ( Int fd, Char* buf, Int nBuf, 
                                         SkinSupp *s )
{
   Bool eof;

   if (s->skind == ParamSupp) {
      eof = VG_(get_line) ( fd, buf, nBuf );
      if (eof) return False;
      s->string = VG_(strdup)(buf);
   }
   return True;
}

extern Bool SK_(error_matches_suppression)(SkinError* err, SkinSupp* su)
{
   UInt su_size;
   AddrCheckError* err_extra = err->extra;

   switch (su->skind) {
      case ParamSupp:
         return (err->ekind == ParamErr && STREQ(su->string, err->string));

      case CoreMemSupp:
         return (err->ekind == CoreMemErr && STREQ(su->string, err->string));

      case Addr1Supp: su_size = 1; goto addr_case;
      case Addr2Supp: su_size = 2; goto addr_case;
      case Addr4Supp: su_size = 4; goto addr_case;
      case Addr8Supp: su_size = 8; goto addr_case;
      addr_case:
         return (err->ekind == AddrErr && err_extra->size != su_size);

      case FreeSupp:
         return (err->ekind == FreeErr || err->ekind == FreeMismatchErr);

      default:
         VG_(printf)("Error:\n"
                     "  unknown AddrCheck suppression type %d\n", su->skind);
         VG_(skin_panic)("unknown suppression type in "
                         "SK_(error_matches_suppression)");
   }
}

#  undef STREQ


/*--------------------------------------------------------------------*/
/*--- Part of the AddrCheck skin: Maintain bitmaps of memory,      ---*/
/*--- tracking the accessibility (A) each byte.                    ---*/
/*--------------------------------------------------------------------*/

#define DEBUG(fmt, args...) //VG_(printf)(fmt, ## args)

/*------------------------------------------------------------*/
/*--- Command line options                                 ---*/
/*------------------------------------------------------------*/

Bool  SK_(clo_partial_loads_ok)       = True;
Int   SK_(clo_freelist_vol)           = 1000000;
Bool  SK_(clo_leak_check)             = False;
VgRes SK_(clo_leak_resolution)        = Vg_LowRes;
Bool  SK_(clo_show_reachable)         = False;
Bool  SK_(clo_workaround_gcc296_bugs) = False;
Bool  SK_(clo_cleanup)                = True;

/*------------------------------------------------------------*/
/*--- Profiling events                                     ---*/
/*------------------------------------------------------------*/

typedef 
   enum { 
      VgpCheckMem = VgpFini+1,
      VgpSetMem
   } 
   VgpSkinCC;

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
   address.  Each byte is represented by one bit, indicating
   accessibility.  So each second-level map contains 8192 bytes.  This
   two-level arrangement conveniently divides the 4G address space
   into 64k lumps, each size 64k bytes.

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
   misaligned address.  */


/*------------------------------------------------------------*/
/*--- Crude profiling machinery.                           ---*/
/*------------------------------------------------------------*/

#ifdef VG_PROFILE_MEMORY

#define N_PROF_EVENTS 150

static UInt event_ctr[N_PROF_EVENTS];

static void init_prof_mem ( void )
{
   Int i;
   for (i = 0; i < N_PROF_EVENTS; i++)
      event_ctr[i] = 0;
}

static void done_prof_mem ( void )
{
   Int i;
   for (i = 0; i < N_PROF_EVENTS; i++) {
      if ((i % 10) == 0) 
         VG_(printf)("\n");
      if (event_ctr[i] > 0)
         VG_(printf)( "prof mem event %2d: %d\n", i, event_ctr[i] );
   }
   VG_(printf)("\n");
}

#define PROF_EVENT(ev)                                  \
   do { sk_assert((ev) >= 0 && (ev) < N_PROF_EVENTS);   \
        event_ctr[ev]++;                                \
   } while (False);

#else

static void init_prof_mem ( void ) { }
static void done_prof_mem ( void ) { }

#define PROF_EVENT(ev) /* */

#endif

/* Event index.  If just the name of the fn is given, this means the
   number of calls to the fn.  Otherwise it is the specified event.

   10   alloc_secondary_map

   20   get_abit
   21   get_vbyte
   22   set_abit
   23   set_vbyte
   24   get_abits4_ALIGNED
   25   get_vbytes4_ALIGNED

   30   set_address_range_perms
   31   set_address_range_perms(lower byte loop)
   32   set_address_range_perms(quadword loop)
   33   set_address_range_perms(upper byte loop)
   
   35   make_noaccess
   36   make_writable
   37   make_readable

   40   copy_address_range_state
   41   copy_address_range_state(byte loop)
   42   check_writable
   43   check_writable(byte loop)
   44   check_readable
   45   check_readable(byte loop)
   46   check_readable_asciiz
   47   check_readable_asciiz(byte loop)

   50   make_aligned_word_NOACCESS
   51   make_aligned_word_WRITABLE

   60   helperc_LOADV4
   61   helperc_STOREV4
   62   helperc_LOADV2
   63   helperc_STOREV2
   64   helperc_LOADV1
   65   helperc_STOREV1

   70   rim_rd_V4_SLOWLY
   71   rim_wr_V4_SLOWLY
   72   rim_rd_V2_SLOWLY
   73   rim_wr_V2_SLOWLY
   74   rim_rd_V1_SLOWLY
   75   rim_wr_V1_SLOWLY

   80   fpu_read
   81   fpu_read aligned 4
   82   fpu_read aligned 8
   83   fpu_read 2
   84   fpu_read 10

   85   fpu_write
   86   fpu_write aligned 4
   87   fpu_write aligned 8
   88   fpu_write 2
   89   fpu_write 10

   90   fpu_read_check_SLOWLY
   91   fpu_read_check_SLOWLY(byte loop)
   92   fpu_write_check_SLOWLY
   93   fpu_write_check_SLOWLY(byte loop)

   100  is_plausible_stack_addr
   101  handle_esp_assignment
   102  handle_esp_assignment(-4)
   103  handle_esp_assignment(+4)
   104  handle_esp_assignment(-12)
   105  handle_esp_assignment(-8)
   106  handle_esp_assignment(+16)
   107  handle_esp_assignment(+12)
   108  handle_esp_assignment(0)
   109  handle_esp_assignment(+8)
   110  handle_esp_assignment(-16)
   111  handle_esp_assignment(+20)
   112  handle_esp_assignment(-20)
   113  handle_esp_assignment(+24)
   114  handle_esp_assignment(-24)

   120  vg_handle_esp_assignment_SLOWLY
   121  vg_handle_esp_assignment_SLOWLY(normal; move down)
   122  vg_handle_esp_assignment_SLOWLY(normal; move up)
   123  vg_handle_esp_assignment_SLOWLY(normal)
   124  vg_handle_esp_assignment_SLOWLY(>= HUGE_DELTA)
*/

/*------------------------------------------------------------*/
/*--- Function declarations.                               ---*/
/*------------------------------------------------------------*/

static void vgmext_ACCESS4_SLOWLY ( Addr a );
static void vgmext_ACCESS2_SLOWLY ( Addr a );
static void vgmext_ACCESS1_SLOWLY ( Addr a );
static void fpu_ACCESS_check_SLOWLY ( Addr addr, Int size );

/*------------------------------------------------------------*/
/*--- Data defns.                                          ---*/
/*------------------------------------------------------------*/

typedef 
   struct {
      UChar abits[8192];
   }
   AcSecMap;

static AcSecMap* primary_map[ /*65536*/ 262144 ];
static AcSecMap  distinguished_secondary_map;

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


static void init_shadow_memory ( void )
{
   Int i;

   for (i = 0; i < 8192; i++)             /* Invalid address */
      distinguished_secondary_map.abits[i] = VGM_BYTE_INVALID; 

   /* These entries gradually get overwritten as the used address
      space expands. */
   for (i = 0; i < 65536; i++)
      primary_map[i] = &distinguished_secondary_map;

   /* These ones should never change; it's a bug in Valgrind if they do. */
   for (i = 65536; i < 262144; i++)
      primary_map[i] = &distinguished_secondary_map;
}

void SK_(post_clo_init) ( void )
{
}

void SK_(fini) ( void )
{
   VG_(print_malloc_stats)();

   if (VG_(clo_verbosity) == 1) {
      if (!SK_(clo_leak_check))
         VG_(message)(Vg_UserMsg, 
             "For a detailed leak analysis,  rerun with: --leak-check=yes");

      VG_(message)(Vg_UserMsg, 
                   "For counts of detected errors, rerun with: -v");
   }
   if (SK_(clo_leak_check)) SK_(detect_memory_leaks)();

   done_prof_mem();
}

/*------------------------------------------------------------*/
/*--- Basic bitmap management, reading and writing.        ---*/
/*------------------------------------------------------------*/

/* Allocate and initialise a secondary map. */

static AcSecMap* alloc_secondary_map ( __attribute__ ((unused)) 
                                       Char* caller )
{
   AcSecMap* map;
   UInt  i;
   PROF_EVENT(10);

   /* Mark all bytes as invalid access and invalid value. */

   /* It just happens that a AcSecMap occupies exactly 18 pages --
      although this isn't important, so the following assert is
      spurious. */
   sk_assert(0 == (sizeof(AcSecMap) % VKI_BYTES_PER_PAGE));
   map = VG_(get_memory_from_mmap)( sizeof(AcSecMap), caller );

   for (i = 0; i < 8192; i++)
      map->abits[i] = VGM_BYTE_INVALID; /* Invalid address */

   /* VG_(printf)("ALLOC_2MAP(%s)\n", caller ); */
   return map;
}


/* Basic reading/writing of the bitmaps, for byte-sized accesses. */

static __inline__ UChar get_abit ( Addr a )
{
   AcSecMap* sm     = primary_map[a >> 16];
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

static __inline__ void set_abit ( Addr a, UChar abit )
{
   AcSecMap* sm;
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


/* Reading/writing of the bitmaps, for aligned word-sized accesses. */

static __inline__ UChar get_abits4_ALIGNED ( Addr a )
{
   AcSecMap* sm;
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



/*------------------------------------------------------------*/
/*--- Setting permissions over address ranges.             ---*/
/*------------------------------------------------------------*/

static void set_address_range_perms ( Addr a, UInt len, 
                                      UInt example_a_bit )
{
   UChar     abyte8;
   UInt      sm_off;
   AcSecMap* sm;

   PROF_EVENT(30);

   if (len == 0)
      return;

   if (len > 100 * 1000 * 1000) {
      VG_(message)(Vg_UserMsg, 
                   "Warning: set address range perms: "
                   "large range %u, a %d",
                   len, example_a_bit );
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

void SK_(make_noaccess) ( Addr a, UInt len )
{
   PROF_EVENT(35);
   DEBUG("SK_(make_noaccess)(%p, %x)\n", a, len);
   set_address_range_perms ( a, len, VGM_BIT_INVALID );
}

void SK_(make_accessible) ( Addr a, UInt len )
{
   PROF_EVENT(36);
   DEBUG("SK_(make_accessible)(%p, %x)\n", a, len);
   set_address_range_perms ( a, len, VGM_BIT_VALID );
}

/* Block-copy permissions (needed for implementing realloc()). */

static void copy_address_range_state ( Addr src, Addr dst, UInt len )
{
   UInt i;

   DEBUG("copy_address_range_state\n");

   PROF_EVENT(40);
   for (i = 0; i < len; i++) {
      UChar abit  = get_abit ( src+i );
      PROF_EVENT(41);
      set_abit ( dst+i, abit );
   }
}


/* Check permissions for address range.  If inadequate permissions
   exist, *bad_addr is set to the offending address, so the caller can
   know what it is. */

Bool SK_(check_writable) ( Addr a, UInt len, Addr* bad_addr )
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

Bool SK_(check_readable) ( Addr a, UInt len, Addr* bad_addr )
{
   UInt  i;
   UChar abit;

   PROF_EVENT(44);
   DEBUG("SK_(check_readable)\n");
   for (i = 0; i < len; i++) {
      abit  = get_abit(a);
      PROF_EVENT(45);
      if (abit != VGM_BIT_VALID) {
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

Bool SK_(check_readable_asciiz) ( Addr a, Addr* bad_addr )
{
   UChar abit;
   PROF_EVENT(46);
   DEBUG("SK_(check_readable_asciiz)\n");
   while (True) {
      PROF_EVENT(47);
      abit  = get_abit(a);
      if (abit != VGM_BIT_VALID) {
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

/* Setting permissions for aligned words.  This supports fast stack
   operations. */

static void make_noaccess_aligned ( Addr a, UInt len )
{
   AcSecMap* sm;
   UInt    sm_off;
   UChar   mask;
   Addr    a_past_end = a + len;

   VGP_PUSHCC(VgpSetMem);

   PROF_EVENT(50);
#  ifdef VG_DEBUG_MEMORY
   sk_assert(IS_ALIGNED4_ADDR(a));
   sk_assert(IS_ALIGNED4_ADDR(len));
#  endif

   for ( ; a < a_past_end; a += 4) {
      ENSURE_MAPPABLE(a, "make_noaccess_aligned");
      sm     = primary_map[a >> 16];
      sm_off = a & 0xFFFF;
      mask = 0x0F;
      mask <<= (a & 4 /* 100b */);   /* a & 4 is either 0 or 4 */
      /* mask now contains 1s where we wish to make address bits
         invalid (1s). */
      sm->abits[sm_off >> 3] |= mask;
   }
   VGP_POPCC(VgpSetMem);
}

static void make_writable_aligned ( Addr a, UInt len )
{
   AcSecMap* sm;
   UInt    sm_off;
   UChar   mask;
   Addr    a_past_end = a + len;

   VGP_PUSHCC(VgpSetMem);

   PROF_EVENT(51);
#  ifdef VG_DEBUG_MEMORY
   sk_assert(IS_ALIGNED4_ADDR(a));
   sk_assert(IS_ALIGNED4_ADDR(len));
#  endif

   for ( ; a < a_past_end; a += 4) {
      ENSURE_MAPPABLE(a, "make_writable_aligned");
      sm     = primary_map[a >> 16];
      sm_off = a & 0xFFFF;
      mask = 0x0F;
      mask <<= (a & 4 /* 100b */);   /* a & 4 is either 0 or 4 */
      /* mask now contains 1s where we wish to make address bits
         invalid (0s). */
      sm->abits[sm_off >> 3] &= ~mask;
   }
   VGP_POPCC(VgpSetMem);
}


static
void check_is_writable ( CorePart part, ThreadState* tst,
                         Char* s, UInt base, UInt size )
{
   Bool ok;
   Addr bad_addr;

   VGP_PUSHCC(VgpCheckMem);

   /* VG_(message)(Vg_DebugMsg,"check is writable: %x .. %x",
                               base,base+size-1); */
   ok = SK_(check_writable) ( base, size, &bad_addr );
   if (!ok) {
      switch (part) {
      case Vg_CoreSysCall:
         SK_(record_param_error) ( tst, bad_addr, /*isWrite =*/True, s );
         break;

      case Vg_CorePThread:
      case Vg_CoreSignal:
         SK_(record_core_mem_error)( tst, /*isWrite=*/True, s );
         break;

      default:
         VG_(skin_panic)("check_is_readable: Unknown or unexpected CorePart");
      }
   }

   VGP_POPCC(VgpCheckMem);
}

static
void check_is_readable ( CorePart part, ThreadState* tst,
                         Char* s, UInt base, UInt size )
{     
   Bool ok;
   Addr bad_addr;

   VGP_PUSHCC(VgpCheckMem);
   
   /* VG_(message)(Vg_DebugMsg,"check is readable: %x .. %x",
                               base,base+size-1); */
   ok = SK_(check_readable) ( base, size, &bad_addr );
   if (!ok) {
      switch (part) {
      case Vg_CoreSysCall:
         SK_(record_param_error) ( tst, bad_addr, /*isWrite =*/False, s );
         break;
      
      case Vg_CorePThread:
         SK_(record_core_mem_error)( tst, /*isWrite=*/False, s );
         break;

      /* If we're being asked to jump to a silly address, record an error 
         message before potentially crashing the entire system. */
      case Vg_CoreTranslate:
         SK_(record_jump_error)( tst, bad_addr );
         break;

      default:
         VG_(skin_panic)("check_is_readable: Unknown or unexpected CorePart");
      }
   }
   VGP_POPCC(VgpCheckMem);
}

static
void check_is_readable_asciiz ( CorePart part, ThreadState* tst,
                                Char* s, UInt str )
{
   Bool ok = True;
   Addr bad_addr;
   /* VG_(message)(Vg_DebugMsg,"check is readable asciiz: 0x%x",str); */

   VGP_PUSHCC(VgpCheckMem);

   sk_assert(part == Vg_CoreSysCall);
   ok = SK_(check_readable_asciiz) ( (Addr)str, &bad_addr );
   if (!ok) {
      SK_(record_param_error) ( tst, bad_addr, /*is_writable =*/False, s );
   }

   VGP_POPCC(VgpCheckMem);
}

static
void addrcheck_new_mem_startup( Addr a, UInt len, Bool rr, Bool ww, Bool xx )
{
   /* Ignore the permissions, just make it readable.  Seems to work... */
   DEBUG("new_mem_startup(%p, %u, rr=%u, ww=%u, xx=%u)\n", a,len,rr,ww,xx);
   SK_(make_accessible)(a, len);
}

static
void addrcheck_new_mem_heap ( Addr a, UInt len, Bool is_inited )
{
   SK_(make_accessible)(a, len);
}

static
void addrcheck_set_perms (Addr a, UInt len, 
                         Bool nn, Bool rr, Bool ww, Bool xx)
{
   DEBUG("addrcheck_set_perms(%p, %u, nn=%u, rr=%u ww=%u, xx=%u)\n",
                              a, len, nn, rr, ww, xx);
   if (rr || ww || xx) {
      SK_(make_accessible)(a, len);
   } else {
      SK_(make_noaccess)(a, len);
   }
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

/* SK_(helperc_ACCESS{1,2,4}) handle the common case fast.
   Under all other circumstances, it defers to the relevant _SLOWLY
   function, which can handle all situations.
*/
__attribute__ ((regparm(1)))
void SK_(helperc_ACCESS4) ( Addr a )
{
#  ifdef VG_DEBUG_MEMORY
   return vgmext_ACCESS4_SLOWLY(a);
#  else
   UInt    sec_no = rotateRight16(a) & 0x3FFFF;
   AcSecMap* sm     = primary_map[sec_no];
   UInt    a_off  = (a & 0xFFFF) >> 3;
   UChar   abits  = sm->abits[a_off];
   abits >>= (a & 4);
   abits &= 15;
   PROF_EVENT(60);
   if (abits == VGM_NIBBLE_VALID) {
      /* Handle common case quickly: a is suitably aligned, is mapped,
         and is addressible.  So just return. */
      return;
   } else {
      /* Slow but general case. */
      vgmext_ACCESS4_SLOWLY(a);
   }
#  endif
}

__attribute__ ((regparm(1)))
void SK_(helperc_ACCESS2) ( Addr a )
{
#  ifdef VG_DEBUG_MEMORY
   return vgmext_ACCESS2_SLOWLY(a);
#  else
   UInt    sec_no = rotateRight16(a) & 0x1FFFF;
   AcSecMap* sm     = primary_map[sec_no];
   UInt    a_off  = (a & 0xFFFF) >> 3;
   PROF_EVENT(62);
   if (sm->abits[a_off] == VGM_BYTE_VALID) {
      /* Handle common case quickly. */
      return;
   } else {
      /* Slow but general case. */
      vgmext_ACCESS2_SLOWLY(a);
   }
#  endif
}

__attribute__ ((regparm(1)))
void SK_(helperc_ACCESS1) ( Addr a )
{
#  ifdef VG_DEBUG_MEMORY
   return vgmext_ACCESS1_SLOWLY(a);
#  else
   UInt    sec_no = shiftRight16(a);
   AcSecMap* sm   = primary_map[sec_no];
   UInt    a_off  = (a & 0xFFFF) >> 3;
   PROF_EVENT(64);
   if (sm->abits[a_off] == VGM_BYTE_VALID) {
      /* Handle common case quickly. */
      return;
   } else {
      /* Slow but general case. */
      vgmext_ACCESS1_SLOWLY(a);
   }
#  endif
}


/*------------------------------------------------------------*/
/*--- Fallback functions to handle cases that the above    ---*/
/*--- VG_(helperc_ACCESS{1,2,4}) can't manage.             ---*/
/*------------------------------------------------------------*/

static void vgmext_ACCESS4_SLOWLY ( Addr a )
{
   Bool a0ok, a1ok, a2ok, a3ok;

   PROF_EVENT(70);

   /* First establish independently the addressibility of the 4 bytes
      involved. */
   a0ok = get_abit(a+0) == VGM_BIT_VALID;
   a1ok = get_abit(a+1) == VGM_BIT_VALID;
   a2ok = get_abit(a+2) == VGM_BIT_VALID;
   a3ok = get_abit(a+3) == VGM_BIT_VALID;

   /* Now distinguish 3 cases */

   /* Case 1: the address is completely valid, so:
      - no addressing error
   */
   if (a0ok && a1ok && a2ok && a3ok) {
      return;
   }

   /* Case 2: the address is completely invalid.  
      - emit addressing error
   */
   /* VG_(printf)("%p (%d %d %d %d)\n", a, a0ok, a1ok, a2ok, a3ok); */
   if (!SK_(clo_partial_loads_ok) 
       || ((a & 3) != 0)
       || (!a0ok && !a1ok && !a2ok && !a3ok)) {
      sk_record_address_error( a, 4, False );
      return;
   }

   /* Case 3: the address is partially valid.  
      - no addressing error
      Case 3 is only allowed if SK_(clo_partial_loads_ok) is True
      (which is the default), and the address is 4-aligned.  
      If not, Case 2 will have applied.
   */
   sk_assert(SK_(clo_partial_loads_ok));
   {
      return;
   }
}

static void vgmext_ACCESS2_SLOWLY ( Addr a )
{
   /* Check the address for validity. */
   Bool aerr = False;
   PROF_EVENT(72);

   if (get_abit(a+0) != VGM_BIT_VALID) aerr = True;
   if (get_abit(a+1) != VGM_BIT_VALID) aerr = True;

   /* If an address error has happened, report it. */
   if (aerr) {
      sk_record_address_error( a, 2, False );
   }
}

static void vgmext_ACCESS1_SLOWLY ( Addr a )
{
   /* Check the address for validity. */
   Bool aerr = False;
   PROF_EVENT(74);

   if (get_abit(a+0) != VGM_BIT_VALID) aerr = True;

   /* If an address error has happened, report it. */
   if (aerr) {
      sk_record_address_error( a, 1, False );
   }
}


/* ---------------------------------------------------------------------
   FPU load and store checks, called from generated code.
   ------------------------------------------------------------------ */

__attribute__ ((regparm(2)))
void SK_(fpu_ACCESS_check) ( Addr addr, Int size )
{
   /* Ensure the read area is both addressible and valid (ie,
      readable).  If there's an address error, don't report a value
      error too; but if there isn't an address error, check for a
      value error. 

      Try to be reasonably fast on the common case; wimp out and defer
      to fpu_ACCESS_check_SLOWLY for everything else.  */

   AcSecMap* sm;
   UInt    sm_off, a_off;
   Addr    addr4;

   PROF_EVENT(80);

#  ifdef VG_DEBUG_MEMORY
   fpu_ACCESS_check_SLOWLY ( addr, size );
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
      return;
     slow4:
      fpu_ACCESS_check_SLOWLY ( addr, 4 );
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
      /* Second half. */
      sm     = primary_map[addr4 >> 16];
      sm_off = addr4 & 0xFFFF;
      a_off  = sm_off >> 3;
      if (sm->abits[a_off] != VGM_BYTE_VALID) goto slow8;
      /* Second half properly aligned and addressible. */
      /* Both halves properly aligned and addressible. */
      return;
     slow8:
      fpu_ACCESS_check_SLOWLY ( addr, 8 );
      return;
   }

   /* Can't be bothered to huff'n'puff to make these (allegedly) rare
      cases go quickly.  */
   if (size == 2) {
      PROF_EVENT(83);
      fpu_ACCESS_check_SLOWLY ( addr, 2 );
      return;
   }

   if (size == 10) {
      PROF_EVENT(84);
      fpu_ACCESS_check_SLOWLY ( addr, 10 );
      return;
   }

   if (size == 28 || size == 108) {
      PROF_EVENT(84); /* XXX assign correct event number */
      fpu_ACCESS_check_SLOWLY ( addr, size );
      return;
   }

   VG_(printf)("size is %d\n", size);
   VG_(skin_panic)("fpu_ACCESS_check: unhandled size");
#  endif
}


/* ---------------------------------------------------------------------
   Slow, general cases for FPU access checks.
   ------------------------------------------------------------------ */

void fpu_ACCESS_check_SLOWLY ( Addr addr, Int size )
{
   Int  i;
   Bool aerr = False;
   PROF_EVENT(90);
   for (i = 0; i < size; i++) {
      PROF_EVENT(91);
      if (get_abit(addr+i) != VGM_BIT_VALID)
         aerr = True;
   }

   if (aerr) {
      sk_record_address_error( addr, size, False );
   }
}


/*------------------------------------------------------------*/
/*--- Shadow chunks info                                   ---*/
/*------------------------------------------------------------*/

static __inline__
void set_where( ShadowChunk* sc, ExeContext* ec )
{
   sc->skin_extra[0] = (UInt)ec;
}

static __inline__
ExeContext *get_where( ShadowChunk* sc )
{
   return (ExeContext*)sc->skin_extra[0];
}

void SK_(complete_shadow_chunk) ( ShadowChunk* sc, ThreadState* tst )
{
   set_where( sc, VG_(get_ExeContext) ( tst ) );
}

/*------------------------------------------------------------*/
/*--- Postponing free()ing                                 ---*/
/*------------------------------------------------------------*/

/* Holds blocks after freeing. */
static ShadowChunk* vg_freed_list_start   = NULL;
static ShadowChunk* vg_freed_list_end     = NULL;
static Int          vg_freed_list_volume  = 0;

static __attribute__ ((unused))
       Int count_freelist ( void )
{
   ShadowChunk* sc;
   Int n = 0;
   for (sc = vg_freed_list_start; sc != NULL; sc = sc->next)
      n++;
   return n;
}

static __attribute__ ((unused))
       void freelist_sanity ( void )
{
   ShadowChunk* sc;
   Int n = 0;
   /* VG_(printf)("freelist sanity\n"); */
   for (sc = vg_freed_list_start; sc != NULL; sc = sc->next)
      n += sc->size;
   sk_assert(n == vg_freed_list_volume);
}

/* Put a shadow chunk on the freed blocks queue, possibly freeing up
   some of the oldest blocks in the queue at the same time. */
static void add_to_freed_queue ( ShadowChunk* sc )
{
   ShadowChunk* sc1;

   /* Put it at the end of the freed list */
   if (vg_freed_list_end == NULL) {
      sk_assert(vg_freed_list_start == NULL);
      vg_freed_list_end = vg_freed_list_start = sc;
      vg_freed_list_volume = sc->size;
   } else {    
      sk_assert(vg_freed_list_end->next == NULL);
      vg_freed_list_end->next = sc;
      vg_freed_list_end = sc;
      vg_freed_list_volume += sc->size;
   }
   sc->next = NULL;

   /* Release enough of the oldest blocks to bring the free queue
      volume below vg_clo_freelist_vol. */
   
   while (vg_freed_list_volume > SK_(clo_freelist_vol)) {
      /* freelist_sanity(); */
      sk_assert(vg_freed_list_start != NULL);
      sk_assert(vg_freed_list_end != NULL);

      sc1 = vg_freed_list_start;
      vg_freed_list_volume -= sc1->size;
      /* VG_(printf)("volume now %d\n", vg_freed_list_volume); */
      sk_assert(vg_freed_list_volume >= 0);

      if (vg_freed_list_start == vg_freed_list_end) {
         vg_freed_list_start = vg_freed_list_end = NULL;
      } else {
         vg_freed_list_start = sc1->next;
      }
      sc1->next = NULL; /* just paranoia */
      VG_(free_ShadowChunk) ( sc1 );
   }
}

/* Return the first shadow chunk satisfying the predicate p. */
ShadowChunk* SK_(any_matching_freed_ShadowChunks)
                        ( Bool (*p) ( ShadowChunk* ))
{
   ShadowChunk* sc;

   /* No point looking through freed blocks if we're not keeping
      them around for a while... */
   for (sc = vg_freed_list_start; sc != NULL; sc = sc->next)
      if (p(sc))
         return sc;

   return NULL;
}

void SK_(alt_free) ( ShadowChunk* sc, ThreadState* tst )
{
   /* Record where freed */
   set_where( sc, VG_(get_ExeContext) ( tst ) );

   /* Put it out of harm's way for a while. */
   add_to_freed_queue ( sc );
}


/*------------------------------------------------------------*/
/*--- Our instrumenter                                     ---*/
/*------------------------------------------------------------*/

#define uInstr1   VG_(new_UInstr1)
#define uInstr2   VG_(new_UInstr2)
#define uLiteral  VG_(set_lit_field)
#define uCCall    VG_(set_ccall_fields)
#define newTemp   VG_(get_new_temp)

UCodeBlock* SK_(instrument)(UCodeBlock* cb_in, Addr orig_addr)
{
/* Use this rather than eg. -1 because it's a UInt. */
#define INVALID_DATA_SIZE   999999

   UCodeBlock* cb;
   Int         i;
   UInstr*     u_in;
   Int         t_addr, t_size;

   cb = VG_(alloc_UCodeBlock)();
   cb->nextTemp = cb_in->nextTemp;

   for (i = 0; i < cb_in->used; i++) {

      t_addr = t_size = INVALID_TEMPREG;
      u_in = &cb_in->instrs[i];

      switch (u_in->opcode) {
         case NOP:  case CALLM_E:  case CALLM_S:
            break;

         /* For memory-ref instrs, copy the data_addr into a temporary to be
          * passed to the cachesim_* helper at the end of the instruction.
          */
         case LOAD: 
            t_addr = u_in->val1; 
            goto do_LOAD_or_STORE;
         case STORE: t_addr = u_in->val2;
            goto do_LOAD_or_STORE;
           do_LOAD_or_STORE:
            uInstr1(cb, CCALL, 0, TempReg, t_addr);
            switch (u_in->size) {
               case 4: uCCall(cb, (Addr)&SK_(helperc_ACCESS4), 1, 1, False );
                  break;
               case 2: uCCall(cb, (Addr)&SK_(helperc_ACCESS2), 1, 1, False );
                  break;
               case 1: uCCall(cb, (Addr)&SK_(helperc_ACCESS1), 1, 1, False );
                  break;
               default: 
                  VG_(skin_panic)("addrcheck::SK_(instrument):LOAD/STORE");
            }
            VG_(copy_UInstr)(cb, u_in);
            break;

         case FPU_R:
         case FPU_W:
            t_addr = u_in->val2;
            t_size = newTemp(cb);
	    uInstr2(cb, MOV, 4, Literal, 0, TempReg, t_size);
	    uLiteral(cb, u_in->size);
            uInstr2(cb, CCALL, 0, TempReg, t_addr, TempReg, t_size);
            uCCall(cb, (Addr)&SK_(fpu_ACCESS_check), 2, 2, False );
            VG_(copy_UInstr)(cb, u_in);
            break;

         default:
            VG_(copy_UInstr)(cb, u_in);
            break;
      }
   }

   VG_(free_UCodeBlock)(cb_in);
   return cb;
}



/*------------------------------------------------------------*/
/*--- Low-level address-space scanning, for the leak       ---*/
/*--- detector.                                            ---*/
/*------------------------------------------------------------*/

static 
jmp_buf memscan_jmpbuf;

static
void vg_scan_all_valid_memory_sighandler ( Int sigNo )
{
   __builtin_longjmp(memscan_jmpbuf, 1);
}

/* Safely (avoiding SIGSEGV / SIGBUS) scan the entire valid address
   space and pass the addresses and values of all addressible,
   defined, aligned words to notify_word.  This is the basis for the
   leak detector.  Returns the number of calls made to notify_word.  */
UInt VG_(scan_all_valid_memory) ( void (*notify_word)( Addr, UInt ) )
{
   /* All volatile, because some gccs seem paranoid about longjmp(). */
   volatile UInt res, numPages, page, primaryMapNo, nWordsNotified;
   volatile Addr pageBase, addr;
   volatile AcSecMap* sm;
   volatile UChar abits;
   volatile UInt page_first_word;

   vki_ksigaction sigbus_saved;
   vki_ksigaction sigbus_new;
   vki_ksigaction sigsegv_saved;
   vki_ksigaction sigsegv_new;
   vki_ksigset_t  blockmask_saved;
   vki_ksigset_t  unblockmask_new;

   /* Temporarily install a new sigsegv and sigbus handler, and make
      sure SIGBUS, SIGSEGV and SIGTERM are unblocked.  (Perhaps the
      first two can never be blocked anyway?)  */

   sigbus_new.ksa_handler = vg_scan_all_valid_memory_sighandler;
   sigbus_new.ksa_flags = VKI_SA_ONSTACK | VKI_SA_RESTART;
   sigbus_new.ksa_restorer = NULL;
   res = VG_(ksigemptyset)( &sigbus_new.ksa_mask );
   sk_assert(res == 0);

   sigsegv_new.ksa_handler = vg_scan_all_valid_memory_sighandler;
   sigsegv_new.ksa_flags = VKI_SA_ONSTACK | VKI_SA_RESTART;
   sigsegv_new.ksa_restorer = NULL;
   res = VG_(ksigemptyset)( &sigsegv_new.ksa_mask );
   sk_assert(res == 0+0);

   res =  VG_(ksigemptyset)( &unblockmask_new );
   res |= VG_(ksigaddset)( &unblockmask_new, VKI_SIGBUS );
   res |= VG_(ksigaddset)( &unblockmask_new, VKI_SIGSEGV );
   res |= VG_(ksigaddset)( &unblockmask_new, VKI_SIGTERM );
   sk_assert(res == 0+0+0);

   res = VG_(ksigaction)( VKI_SIGBUS, &sigbus_new, &sigbus_saved );
   sk_assert(res == 0+0+0+0);

   res = VG_(ksigaction)( VKI_SIGSEGV, &sigsegv_new, &sigsegv_saved );
   sk_assert(res == 0+0+0+0+0);

   res = VG_(ksigprocmask)( VKI_SIG_UNBLOCK, &unblockmask_new, &blockmask_saved );
   sk_assert(res == 0+0+0+0+0+0);

   /* The signal handlers are installed.  Actually do the memory scan. */
   numPages = 1 << (32-VKI_BYTES_PER_PAGE_BITS);
   sk_assert(numPages == 1048576);
   sk_assert(4096 == (1 << VKI_BYTES_PER_PAGE_BITS));

   nWordsNotified = 0;

   for (page = 0; page < numPages; page++) {
      pageBase = page << VKI_BYTES_PER_PAGE_BITS;
      primaryMapNo = pageBase >> 16;
      sm = primary_map[primaryMapNo];
      if (IS_DISTINGUISHED_SM(sm)) continue;
      if (__builtin_setjmp(memscan_jmpbuf) == 0) {
         /* try this ... */
         page_first_word = * (volatile UInt*)pageBase;
         /* we get here if we didn't get a fault */
         /* Scan the page */
         for (addr = pageBase; addr < pageBase+VKI_BYTES_PER_PAGE; addr += 4) {
            abits  = get_abits4_ALIGNED(addr);
            if (abits == VGM_NIBBLE_VALID) {
               nWordsNotified++;
               notify_word ( addr, *(UInt*)addr );
	    }
         }
      } else {
         /* We get here if reading the first word of the page caused a
            fault, which in turn caused the signal handler to longjmp.
            Ignore this page. */
         if (0)
         VG_(printf)(
            "vg_scan_all_valid_memory_sighandler: ignoring page at %p\n",
            (void*)pageBase 
         );
      }
   }

   /* Restore signal state to whatever it was before. */
   res = VG_(ksigaction)( VKI_SIGBUS, &sigbus_saved, NULL );
   sk_assert(res == 0 +0);

   res = VG_(ksigaction)( VKI_SIGSEGV, &sigsegv_saved, NULL );
   sk_assert(res == 0 +0 +0);

   res = VG_(ksigprocmask)( VKI_SIG_SETMASK, &blockmask_saved, NULL );
   sk_assert(res == 0 +0 +0 +0);

   return nWordsNotified;
}


/*------------------------------------------------------------*/
/*--- Detecting leaked (unreachable) malloc'd blocks.      ---*/
/*------------------------------------------------------------*/

/* A block is either 
   -- Proper-ly reached; a pointer to its start has been found
   -- Interior-ly reached; only an interior pointer to it has been found
   -- Unreached; so far, no pointers to any part of it have been found. 
*/
typedef 
   enum { Unreached, Interior, Proper } 
   Reachedness;

/* A block record, used for generating err msgs. */
typedef
   struct _LossRecord {
      struct _LossRecord* next;
      /* Where these lost blocks were allocated. */
      ExeContext*  allocated_at;
      /* Their reachability. */
      Reachedness  loss_mode;
      /* Number of blocks and total # bytes involved. */
      UInt         total_bytes;
      UInt         num_blocks;
   }
   LossRecord;


/* Find the i such that ptr points at or inside the block described by
   shadows[i].  Return -1 if none found.  This assumes that shadows[]
   has been sorted on the ->data field. */

#ifdef VG_DEBUG_LEAKCHECK
/* Used to sanity-check the fast binary-search mechanism. */
static Int find_shadow_for_OLD ( Addr          ptr, 
                                 ShadowChunk** shadows,
                                 Int           n_shadows )

{
   Int  i;
   Addr a_lo, a_hi;
   PROF_EVENT(70);
   for (i = 0; i < n_shadows; i++) {
      PROF_EVENT(71);
      a_lo = shadows[i]->data;
      a_hi = ((Addr)shadows[i]->data) + shadows[i]->size - 1;
      if (a_lo <= ptr && ptr <= a_hi)
         return i;
   }
   return -1;
}
#endif


static Int find_shadow_for ( Addr          ptr, 
                             ShadowChunk** shadows,
                             Int           n_shadows )
{
   Addr a_mid_lo, a_mid_hi;
   Int lo, mid, hi, retVal;
   PROF_EVENT(70);
   /* VG_(printf)("find shadow for %p = ", ptr); */
   retVal = -1;
   lo = 0;
   hi = n_shadows-1;
   while (True) {
      PROF_EVENT(71);

      /* invariant: current unsearched space is from lo to hi,
         inclusive. */
      if (lo > hi) break; /* not found */

      mid      = (lo + hi) / 2;
      a_mid_lo = shadows[mid]->data;
      a_mid_hi = ((Addr)shadows[mid]->data) + shadows[mid]->size - 1;

      if (ptr < a_mid_lo) {
         hi = mid-1;
         continue;
      } 
      if (ptr > a_mid_hi) {
         lo = mid+1;
         continue;
      }
      sk_assert(ptr >= a_mid_lo && ptr <= a_mid_hi);
      retVal = mid;
      break;
   }

#  ifdef VG_DEBUG_LEAKCHECK
   sk_assert(retVal == find_shadow_for_OLD ( ptr, shadows, n_shadows ));
#  endif
   /* VG_(printf)("%d\n", retVal); */
   return retVal;
}



static void sort_malloc_shadows ( ShadowChunk** shadows, UInt n_shadows )
{
   Int   incs[14] = { 1, 4, 13, 40, 121, 364, 1093, 3280,
                      9841, 29524, 88573, 265720,
                      797161, 2391484 };
   Int          lo = 0;
   Int          hi = n_shadows-1;
   Int          i, j, h, bigN, hp;
   ShadowChunk* v;

   PROF_EVENT(72);
   bigN = hi - lo + 1; if (bigN < 2) return;
   hp = 0; while (incs[hp] < bigN) hp++; hp--;

   for (; hp >= 0; hp--) {
      PROF_EVENT(73);
      h = incs[hp];
      i = lo + h;
      while (1) {
         PROF_EVENT(74);
         if (i > hi) break;
         v = shadows[i];
         j = i;
         while (shadows[j-h]->data > v->data) {
            PROF_EVENT(75);
            shadows[j] = shadows[j-h];
            j = j - h;
            if (j <= (lo + h - 1)) break;
         }
         shadows[j] = v;
         i++;
      }
   }
}

/* Globals, for the callback used by SK_(detect_memory_leaks). */

static ShadowChunk** vglc_shadows;
static Int           vglc_n_shadows;
static Reachedness*  vglc_reachedness;
static Addr          vglc_min_mallocd_addr;
static Addr          vglc_max_mallocd_addr;

static 
void vg_detect_memory_leaks_notify_addr ( Addr a, UInt word_at_a )
{
   Int  sh_no;
   Addr ptr;

   /* Rule out some known causes of bogus pointers.  Mostly these do
      not cause much trouble because only a few false pointers can
      ever lurk in these places.  This mainly stops it reporting that
      blocks are still reachable in stupid test programs like this

         int main (void) { char* a = malloc(100); return 0; }

      which people seem inordinately fond of writing, for some reason.  

      Note that this is a complete kludge.  It would be better to
      ignore any addresses corresponding to valgrind.so's .bss and
      .data segments, but I cannot think of a reliable way to identify
      where the .bss segment has been put.  If you can, drop me a
      line.  
   */
   if (VG_(within_stack)(a))                return;
   if (VG_(within_m_state_static)(a))       return;
   if (a == (Addr)(&vglc_min_mallocd_addr)) return;
   if (a == (Addr)(&vglc_max_mallocd_addr)) return;

   /* OK, let's get on and do something Useful for a change. */

   ptr = (Addr)word_at_a;
   if (ptr >= vglc_min_mallocd_addr && ptr <= vglc_max_mallocd_addr) {
      /* Might be legitimate; we'll have to investigate further. */
      sh_no = find_shadow_for ( ptr, vglc_shadows, vglc_n_shadows );
      if (sh_no != -1) {
         /* Found a block at/into which ptr points. */
         sk_assert(sh_no >= 0 && sh_no < vglc_n_shadows);
         sk_assert(ptr < vglc_shadows[sh_no]->data 
                         + vglc_shadows[sh_no]->size);
         /* Decide whether Proper-ly or Interior-ly reached. */
         if (ptr == vglc_shadows[sh_no]->data) {
            if (0) VG_(printf)("pointer at %p to %p\n", a, word_at_a );
            vglc_reachedness[sh_no] = Proper;
         } else {
            if (vglc_reachedness[sh_no] == Unreached)
               vglc_reachedness[sh_no] = Interior;
         }
      }
   }
}


void SK_(detect_memory_leaks) ( void )
{
   Int    i;
   Int    blocks_leaked, bytes_leaked;
   Int    blocks_dubious, bytes_dubious;
   Int    blocks_reachable, bytes_reachable;
   Int    n_lossrecords;
   UInt   bytes_notified;
   
   LossRecord*  errlist;
   LossRecord*  p;

   PROF_EVENT(76);

   /* VG_(get_malloc_shadows) allocates storage for shadows */
   vglc_shadows = VG_(get_malloc_shadows)( &vglc_n_shadows );
   if (vglc_n_shadows == 0) {
      sk_assert(vglc_shadows == NULL);
      VG_(message)(Vg_UserMsg, 
                   "No malloc'd blocks -- no leaks are possible.\n");
      return;
   }

   VG_(message)(Vg_UserMsg, 
                "searching for pointers to %d not-freed blocks.", 
                vglc_n_shadows );
   sort_malloc_shadows ( vglc_shadows, vglc_n_shadows );

   /* Sanity check; assert that the blocks are now in order and that
      they don't overlap. */
   for (i = 0; i < vglc_n_shadows-1; i++) {
      sk_assert( ((Addr)vglc_shadows[i]->data)
                 < ((Addr)vglc_shadows[i+1]->data) );
      sk_assert( ((Addr)vglc_shadows[i]->data) + vglc_shadows[i]->size
                 < ((Addr)vglc_shadows[i+1]->data) );
   }

   vglc_min_mallocd_addr = ((Addr)vglc_shadows[0]->data);
   vglc_max_mallocd_addr = ((Addr)vglc_shadows[vglc_n_shadows-1]->data)
                         + vglc_shadows[vglc_n_shadows-1]->size - 1;

   vglc_reachedness 
      = VG_(malloc)( vglc_n_shadows * sizeof(Reachedness) );
   for (i = 0; i < vglc_n_shadows; i++)
      vglc_reachedness[i] = Unreached;

   /* Do the scan of memory. */
   bytes_notified
       = VG_(scan_all_valid_memory)( &vg_detect_memory_leaks_notify_addr )
         * VKI_BYTES_PER_WORD;

   VG_(message)(Vg_UserMsg, "checked %d bytes.", bytes_notified);

   blocks_leaked    = bytes_leaked    = 0;
   blocks_dubious   = bytes_dubious   = 0;
   blocks_reachable = bytes_reachable = 0;

   for (i = 0; i < vglc_n_shadows; i++) {
      if (vglc_reachedness[i] == Unreached) {
         blocks_leaked++;
         bytes_leaked += vglc_shadows[i]->size;
      }
      else if (vglc_reachedness[i] == Interior) {
         blocks_dubious++;
         bytes_dubious += vglc_shadows[i]->size;
      }
      else if (vglc_reachedness[i] == Proper) {
         blocks_reachable++;
         bytes_reachable += vglc_shadows[i]->size;
      }
   }

   VG_(message)(Vg_UserMsg, "");
   VG_(message)(Vg_UserMsg, "definitely lost: %d bytes in %d blocks.", 
                            bytes_leaked, blocks_leaked );
   VG_(message)(Vg_UserMsg, "possibly lost:   %d bytes in %d blocks.", 
                            bytes_dubious, blocks_dubious );
   VG_(message)(Vg_UserMsg, "still reachable: %d bytes in %d blocks.", 
                            bytes_reachable, blocks_reachable );


   /* Common up the lost blocks so we can print sensible error
      messages. */

   n_lossrecords = 0;
   errlist       = NULL;
   for (i = 0; i < vglc_n_shadows; i++) {
     
      /* 'where' stored in 'skin_extra' field */
      ExeContext* where = get_where ( vglc_shadows[i] );

      for (p = errlist; p != NULL; p = p->next) {
         if (p->loss_mode == vglc_reachedness[i]
             && VG_(eq_ExeContext) ( SK_(clo_leak_resolution),
                                     p->allocated_at, 
                                     where) ) {
            break;
	 }
      }
      if (p != NULL) {
         p->num_blocks  ++;
         p->total_bytes += vglc_shadows[i]->size;
      } else {
         n_lossrecords ++;
         p = VG_(malloc)(sizeof(LossRecord));
         p->loss_mode    = vglc_reachedness[i];
         p->allocated_at = where;
         p->total_bytes  = vglc_shadows[i]->size;
         p->num_blocks   = 1;
         p->next         = errlist;
         errlist         = p;
      }
   }
   
   for (i = 0; i < n_lossrecords; i++) {
      LossRecord* p_min = NULL;
      UInt        n_min = 0xFFFFFFFF;
      for (p = errlist; p != NULL; p = p->next) {
         if (p->num_blocks > 0 && p->total_bytes < n_min) {
            n_min = p->total_bytes;
            p_min = p;
         }
      }
      sk_assert(p_min != NULL);

      if ( (!SK_(clo_show_reachable)) && p_min->loss_mode == Proper) {
         p_min->num_blocks = 0;
         continue;
      }

      VG_(message)(Vg_UserMsg, "");
      VG_(message)(
         Vg_UserMsg,
         "%d bytes in %d blocks are %s in loss record %d of %d",
         p_min->total_bytes, p_min->num_blocks,
         p_min->loss_mode==Unreached ? "definitely lost" :
            (p_min->loss_mode==Interior ? "possibly lost"
                                        : "still reachable"),
         i+1, n_lossrecords
      );
      VG_(pp_ExeContext)(p_min->allocated_at);
      p_min->num_blocks = 0;
   }

   VG_(message)(Vg_UserMsg, "");
   VG_(message)(Vg_UserMsg, "LEAK SUMMARY:");
   VG_(message)(Vg_UserMsg, "   definitely lost: %d bytes in %d blocks.", 
                            bytes_leaked, blocks_leaked );
   VG_(message)(Vg_UserMsg, "   possibly lost:   %d bytes in %d blocks.", 
                            bytes_dubious, blocks_dubious );
   VG_(message)(Vg_UserMsg, "   still reachable: %d bytes in %d blocks.", 
                            bytes_reachable, blocks_reachable );
   if (!SK_(clo_show_reachable)) {
      VG_(message)(Vg_UserMsg, 
         "Reachable blocks (those to which a pointer was found) are not shown.");
      VG_(message)(Vg_UserMsg, 
         "To see them, rerun with: --show-reachable=yes");
   }
   VG_(message)(Vg_UserMsg, "");

   VG_(free) ( vglc_shadows );
   VG_(free) ( vglc_reachedness );
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
   if (IS_DISTINGUISHED_SM(primary_map[0]) && 
       IS_DISTINGUISHED_SM(primary_map[65535]))
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
   VG_(message)(Vg_DebugMsg, "efl %\n", buf1);

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
/*--- Syscall wrappers                                     ---*/
/*------------------------------------------------------------*/

void* SK_(pre_syscall)  ( ThreadId tid, UInt syscallno, Bool isBlocking )
{
   Int sane = SK_(cheap_sanity_check)();
   return (void*)sane;
}

void  SK_(post_syscall) ( ThreadId tid, UInt syscallno,
                           void* pre_result, Int res, Bool isBlocking )
{
   Int  sane_before_call = (Int)pre_result;
   Bool sane_after_call  = SK_(cheap_sanity_check)();

   if ((Int)sane_before_call && (!sane_after_call)) {
      VG_(message)(Vg_DebugMsg, "post-syscall: ");
      VG_(message)(Vg_DebugMsg,
                   "probable sanity check failure for syscall number %d\n",
                   syscallno );
      VG_(skin_panic)("aborting due to the above ... bye!");
   }
}


/*------------------------------------------------------------*/
/*--- Setup                                                ---*/
/*------------------------------------------------------------*/

void SK_(written_shadow_regs_values)( UInt* gen_reg_value, UInt* eflags_value )
{
   *gen_reg_value = VGM_WORD_VALID;
   *eflags_value  = VGM_EFLAGS_VALID;
}

Bool SK_(process_cmd_line_option)(Char* arg)
{
#  define STREQ(s1,s2)     (0==VG_(strcmp_ws)((s1),(s2)))
#  define STREQN(nn,s1,s2) (0==VG_(strncmp_ws)((s1),(s2),(nn)))

   if      (STREQ(arg, "--partial-loads-ok=yes"))
      SK_(clo_partial_loads_ok) = True;
   else if (STREQ(arg, "--partial-loads-ok=no"))
      SK_(clo_partial_loads_ok) = False;

   else if (STREQN(15, arg, "--freelist-vol=")) {
      SK_(clo_freelist_vol) = (Int)VG_(atoll)(&arg[15]);
      if (SK_(clo_freelist_vol) < 0) SK_(clo_freelist_vol) = 0;
   }

   else if (STREQ(arg, "--leak-check=yes"))
      SK_(clo_leak_check) = True;
   else if (STREQ(arg, "--leak-check=no"))
      SK_(clo_leak_check) = False;

   else if (STREQ(arg, "--leak-resolution=low"))
      SK_(clo_leak_resolution) = Vg_LowRes;
   else if (STREQ(arg, "--leak-resolution=med"))
      SK_(clo_leak_resolution) = Vg_MedRes;
   else if (STREQ(arg, "--leak-resolution=high"))
      SK_(clo_leak_resolution) = Vg_HighRes;
   
   else if (STREQ(arg, "--show-reachable=yes"))
      SK_(clo_show_reachable) = True;
   else if (STREQ(arg, "--show-reachable=no"))
      SK_(clo_show_reachable) = False;

   else if (STREQ(arg, "--workaround-gcc296-bugs=yes"))
      SK_(clo_workaround_gcc296_bugs) = True;
   else if (STREQ(arg, "--workaround-gcc296-bugs=no"))
      SK_(clo_workaround_gcc296_bugs) = False;

   else if (STREQ(arg, "--cleanup=yes"))
      SK_(clo_cleanup) = True;
   else if (STREQ(arg, "--cleanup=no"))
      SK_(clo_cleanup) = False;

   else
      return False;

   return True;

#undef STREQ
#undef STREQN
}

Char* SK_(usage)(void)
{  
   return  
"    --partial-loads-ok=no|yes too hard to explain here; see manual [yes]\n"
"    --freelist-vol=<number>   volume of freed blocks queue [1000000]\n"
"    --leak-check=no|yes       search for memory leaks at exit? [no]\n"
"    --leak-resolution=low|med|high\n"
"                              amount of bt merging in leak check [low]\n"
"    --show-reachable=no|yes   show reachable blocks in leak check? [no]\n"
"    --workaround-gcc296-bugs=no|yes  self explanatory [no]\n"
"    --check-addrVs=no|yes     experimental lighterweight checking? [yes]\n"
"                              yes == Valgrind's original behaviour\n"
"\n"
"    --cleanup=no|yes          improve after instrumentation? [yes]\n";
}


/*------------------------------------------------------------*/
/*--- Setup                                                ---*/
/*------------------------------------------------------------*/

void SK_(pre_clo_init)(VgDetails* details, VgNeeds* needs, VgTrackEvents* track)
{
   details->name             = "addrcheck";
   details->version          = NULL;
   details->description      = "a fine-grained address checker";
   details->copyright_author =
      "Copyright (C) 2002, and GNU GPL'd, by Julian Seward.";
   details->bug_reports_to   = "jseward@acm.org";

   needs->core_errors          = True;
   needs->skin_errors          = True;
   needs->libc_freeres         = True;
   needs->sizeof_shadow_block  = 1;
   needs->basic_block_discards = False;
   needs->shadow_regs          = False;
   needs->command_line_options = True;
   needs->client_requests      = True;
   needs->extended_UCode       = False;
   needs->syscall_wrapper      = True;
   needs->alternative_free     = True;
   needs->sanity_checks        = True;

   track->new_mem_startup       = & addrcheck_new_mem_startup;
   track->new_mem_heap          = & addrcheck_new_mem_heap;
   track->new_mem_stack         = & SK_(make_accessible);
   track->new_mem_stack_aligned = & make_writable_aligned;
   track->new_mem_stack_signal  = & SK_(make_accessible);
   track->new_mem_brk           = & SK_(make_accessible);
   track->new_mem_mmap          = & addrcheck_set_perms;
   
   track->copy_mem_heap         = & copy_address_range_state;
   track->copy_mem_remap        = & copy_address_range_state;
   track->change_mem_mprotect   = & addrcheck_set_perms;
      
   track->ban_mem_heap          = & SK_(make_noaccess);
   track->ban_mem_stack         = & SK_(make_noaccess);

   track->die_mem_heap          = & SK_(make_noaccess);
   track->die_mem_stack         = & SK_(make_noaccess);
   track->die_mem_stack_aligned = & make_noaccess_aligned; 
   track->die_mem_stack_signal  = & SK_(make_noaccess); 
   track->die_mem_brk           = & SK_(make_noaccess);
   track->die_mem_munmap        = & SK_(make_noaccess); 

   track->bad_free              = & SK_(record_free_error);
   track->mismatched_free       = & SK_(record_freemismatch_error);

   track->pre_mem_read          = & check_is_readable;
   track->pre_mem_read_asciiz   = & check_is_readable_asciiz;
   track->pre_mem_write         = & check_is_writable;
   track->post_mem_write        = & SK_(make_accessible);

   VG_(register_compact_helper)((Addr) & SK_(helperc_ACCESS4));
   VG_(register_compact_helper)((Addr) & SK_(helperc_ACCESS2));
   VG_(register_compact_helper)((Addr) & SK_(helperc_ACCESS1));
   VG_(register_compact_helper)((Addr) & SK_(fpu_ACCESS_check));

   VGP_(register_profile_event) ( VgpSetMem,   "set-mem-perms" );
   VGP_(register_profile_event) ( VgpCheckMem, "check-mem-perms" );

   init_shadow_memory();
   init_prof_mem();
}

/*--------------------------------------------------------------------*/
/*--- end                                                ac_main.c ---*/
/*--------------------------------------------------------------------*/
