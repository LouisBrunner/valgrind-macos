
/*--------------------------------------------------------------------*/
/*--- Code that is shared between MemCheck and AddrCheck.          ---*/
/*---                                                  mc_common.c ---*/
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


#include "mc_common.h"

/*------------------------------------------------------------*/
/*--- Defns                                                ---*/
/*------------------------------------------------------------*/

/* These many bytes below %ESP are considered addressible if we're
   doing the --workaround-gcc296-bugs hack. */
#define VG_GCC296_BUG_STACK_SLOP 1024

/*------------------------------------------------------------*/
/*--- Command line options                                 ---*/
/*------------------------------------------------------------*/

Bool  MC_(clo_partial_loads_ok)       = True;
Int   MC_(clo_freelist_vol)           = 1000000;
Bool  MC_(clo_leak_check)             = False;
VgRes MC_(clo_leak_resolution)        = Vg_LowRes;
Bool  MC_(clo_show_reachable)         = False;
Bool  MC_(clo_workaround_gcc296_bugs) = False;
Bool  MC_(clo_cleanup)                = True;
Bool  MC_(clo_avoid_strlen_errors)    = True;

Bool MC_(process_common_cmd_line_option)(Char* arg)
{
#  define STREQ(s1,s2)     (0==VG_(strcmp_ws)((s1),(s2)))
#  define STREQN(nn,s1,s2) (0==VG_(strncmp_ws)((s1),(s2),(nn)))

   if      (STREQ(arg, "--partial-loads-ok=yes"))
      MC_(clo_partial_loads_ok) = True;
   else if (STREQ(arg, "--partial-loads-ok=no"))
      MC_(clo_partial_loads_ok) = False;

   else if (STREQN(15, arg, "--freelist-vol=")) {
      MC_(clo_freelist_vol) = (Int)VG_(atoll)(&arg[15]);
      if (MC_(clo_freelist_vol) < 0) MC_(clo_freelist_vol) = 0;
   }

   else if (STREQ(arg, "--leak-check=yes"))
      MC_(clo_leak_check) = True;
   else if (STREQ(arg, "--leak-check=no"))
      MC_(clo_leak_check) = False;

   else if (STREQ(arg, "--leak-resolution=low"))
      MC_(clo_leak_resolution) = Vg_LowRes;
   else if (STREQ(arg, "--leak-resolution=med"))
      MC_(clo_leak_resolution) = Vg_MedRes;
   else if (STREQ(arg, "--leak-resolution=high"))
      MC_(clo_leak_resolution) = Vg_HighRes;
   
   else if (STREQ(arg, "--show-reachable=yes"))
      MC_(clo_show_reachable) = True;
   else if (STREQ(arg, "--show-reachable=no"))
      MC_(clo_show_reachable) = False;

   else if (STREQ(arg, "--workaround-gcc296-bugs=yes"))
      MC_(clo_workaround_gcc296_bugs) = True;
   else if (STREQ(arg, "--workaround-gcc296-bugs=no"))
      MC_(clo_workaround_gcc296_bugs) = False;

   else if (STREQ(arg, "--cleanup=yes"))
      MC_(clo_cleanup) = True;
   else if (STREQ(arg, "--cleanup=no"))
      MC_(clo_cleanup) = False;

   else
      return False;

   return True;

#undef STREQ
#undef STREQN
}

/*------------------------------------------------------------*/
/*--- Comparing and printing errors                        ---*/
/*------------------------------------------------------------*/

static __inline__
void clear_AddrInfo ( AddrInfo* ai )
{
   ai->akind      = Unknown;
   ai->blksize    = 0;
   ai->rwoffset   = 0;
   ai->lastchange = NULL;
   ai->stack_tid  = VG_INVALID_THREADID;
   ai->maybe_gcc  = False;
}

void MC_(clear_MemCheckError) ( MemCheckError* err_extra )
{
   err_extra->axskind   = ReadAxs;
   err_extra->size      = 0;
   clear_AddrInfo ( &err_extra->addrinfo );
   err_extra->isWrite   = False;
}

__attribute__ ((unused))
static Bool eq_AddrInfo ( VgRes res, AddrInfo* ai1, AddrInfo* ai2 )
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

Bool SK_(eq_SkinError) ( VgRes res, Error* e1, Error* e2 )
{
   MemCheckError* e1_extra = VG_(get_error_extra)(e1);
   MemCheckError* e2_extra = VG_(get_error_extra)(e2);

   /* Guaranteed by calling function */
   sk_assert(VG_(get_error_kind)(e1) == VG_(get_error_kind)(e2));
   
   switch (VG_(get_error_kind)(e1)) {
      case CoreMemErr: {
         Char *e1s, *e2s;
         if (e1_extra->isWrite != e2_extra->isWrite)   return False;
         e1s = VG_(get_error_string)(e1);
         e2s = VG_(get_error_string)(e2);
         if (e1s == e2s)                               return True;
         if (0 == VG_(strcmp)(e1s, e2s))               return True;
         return False;
      }

      case UserErr:
      case ParamErr:
         if (e1_extra->isWrite != e2_extra->isWrite)           return False;
         if (VG_(get_error_kind)(e1) == ParamErr 
             && 0 != VG_(strcmp)(VG_(get_error_string)(e1),
                                 VG_(get_error_string)(e2)))   return False;
         return True;

      case FreeErr:
      case FreeMismatchErr:
         /* JRS 2002-Aug-26: comparing addrs seems overkill and can
            cause excessive duplication of errors.  Not even AddrErr
            below does that.  So don't compare either the .addr field
            or the .addrinfo fields. */
         /* if (e1->addr != e2->addr) return False; */
         /* if (!eq_AddrInfo(res, &e1_extra->addrinfo, &e2_extra->addrinfo)) 
               return False;
         */
         return True;

      case AddrErr:
         /* if (e1_extra->axskind != e2_extra->axskind) return False; */
         if (e1_extra->size != e2_extra->size) return False;
         /*
         if (!eq_AddrInfo(res, &e1_extra->addrinfo, &e2_extra->addrinfo)) 
            return False;
         */
         return True;

      case ValueErr:
         if (e1_extra->size != e2_extra->size) return False;
         return True;

      default: 
         VG_(printf)("Error:\n  unknown error code %d\n",
                     VG_(get_error_kind)(e1));
         VG_(skin_panic)("unknown error code in SK_(eq_SkinError)");
   }
}

void MC_(pp_AddrInfo) ( Addr a, AddrInfo* ai )
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
      case Freed: case Mallocd: case UserG: {
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
         VG_(message)(Vg_UserMsg, 
            "   Address 0x%x is %d bytes %s a block of size %d %s",
            a, delta, relative, 
            ai->blksize,
            ai->akind==Mallocd ? "alloc'd" 
               : ai->akind==Freed ? "free'd" 
                                  : "client-defined");
         VG_(pp_ExeContext)(ai->lastchange);
         break;
      }
      default:
         VG_(skin_panic)("MC_(pp_AddrInfo)");
   }
}

/*------------------------------------------------------------*/
/*--- Recording errors                                     ---*/
/*------------------------------------------------------------*/

/* Is this address within some small distance below %ESP?  Used only
   for the --workaround-gcc296-bugs kludge. */
static Bool is_just_below_ESP( Addr esp, Addr aa )
{
   if ((UInt)esp > (UInt)aa
       && ((UInt)esp - (UInt)aa) <= VG_GCC296_BUG_STACK_SLOP)
      return True;
   else
      return False;
}

/* This one called from generated code. */

void MC_(record_address_error) ( Addr a, Int size, Bool isWrite )
{
   MemCheckError err_extra;
   Bool          just_below_esp;

   just_below_esp = is_just_below_ESP( VG_(get_stack_pointer)(), a );

   /* If this is caused by an access immediately below %ESP, and the
      user asks nicely, we just ignore it. */
   if (MC_(clo_workaround_gcc296_bugs) && just_below_esp)
      return;

   MC_(clear_MemCheckError)( &err_extra );
   err_extra.axskind = isWrite ? WriteAxs : ReadAxs;
   err_extra.size    = size;
   err_extra.addrinfo.akind     = Undescribed;
   err_extra.addrinfo.maybe_gcc = just_below_esp;
   VG_(maybe_record_error)( NULL, AddrErr, a, /*s*/NULL, &err_extra );
}

/* These ones are called from non-generated code */

/* This is for memory errors in pthread functions, as opposed to pthread API
   errors which are found by the core. */
void MC_(record_core_mem_error) ( ThreadState* tst, Bool isWrite, Char* msg )
{
   MemCheckError err_extra;

   MC_(clear_MemCheckError)( &err_extra );
   err_extra.isWrite = isWrite;
   VG_(maybe_record_error)( tst, CoreMemErr, /*addr*/0, msg, &err_extra );
}

void MC_(record_param_error) ( ThreadState* tst, Addr a, Bool isWrite, 
                               Char* msg )
{
   MemCheckError err_extra;

   sk_assert(NULL != tst);
   MC_(clear_MemCheckError)( &err_extra );
   err_extra.addrinfo.akind = Undescribed;
   err_extra.isWrite = isWrite;
   VG_(maybe_record_error)( tst, ParamErr, a, msg, &err_extra );
}

void MC_(record_jump_error) ( ThreadState* tst, Addr a )
{
   MemCheckError err_extra;

   sk_assert(NULL != tst);

   MC_(clear_MemCheckError)( &err_extra );
   err_extra.axskind = ExecAxs;
   err_extra.addrinfo.akind = Undescribed;
   VG_(maybe_record_error)( tst, AddrErr, a, /*s*/NULL, &err_extra );
}

void MC_(record_free_error) ( ThreadState* tst, Addr a ) 
{
   MemCheckError err_extra;

   sk_assert(NULL != tst);

   MC_(clear_MemCheckError)( &err_extra );
   err_extra.addrinfo.akind = Undescribed;
   VG_(maybe_record_error)( tst, FreeErr, a, /*s*/NULL, &err_extra );
}

void MC_(record_freemismatch_error) ( ThreadState* tst, Addr a )
{
   MemCheckError err_extra;

   sk_assert(NULL != tst);

   MC_(clear_MemCheckError)( &err_extra );
   err_extra.addrinfo.akind = Undescribed;
   VG_(maybe_record_error)( tst, FreeMismatchErr, a, /*s*/NULL, &err_extra );
}

/*------------------------------------------------------------*/
/*--- Suppressions                                         ---*/
/*------------------------------------------------------------*/

Bool SK_(read_extra_suppression_info) ( Int fd, Char* buf, Int nBuf, Supp *su )
{
   Bool eof;

   if (VG_(get_supp_kind)(su) == ParamSupp) {
      eof = VG_(get_line) ( fd, buf, nBuf );
      if (eof) return False;
      VG_(set_supp_string)(su, VG_(strdup)(buf));
   }
   return True;
}

#define STREQ(s1,s2) (s1 != NULL && s2 != NULL \
                      && VG_(strcmp)((s1),(s2))==0)

Bool SK_(error_matches_suppression)(Error* err, Supp* su)
{
   UInt su_size;
   MemCheckError* err_extra = VG_(get_error_extra)(err);
   ErrorKind      ekind     = VG_(get_error_kind )(err);

   switch (VG_(get_supp_kind)(su)) {
      case ParamSupp:
         return (ekind == ParamErr 
              && STREQ(VG_(get_error_string)(err), VG_(get_supp_string)(su)));

      case CoreMemSupp:
         return (ekind == CoreMemErr
              && STREQ(VG_(get_error_string)(err), VG_(get_supp_string)(su)));

      case Value0Supp: su_size = 0; goto value_case;
      case Value1Supp: su_size = 1; goto value_case;
      case Value2Supp: su_size = 2; goto value_case;
      case Value4Supp: su_size = 4; goto value_case;
      case Value8Supp: su_size = 8; goto value_case;
      value_case:
         return (ekind == ValueErr && err_extra->size == su_size);

      case Addr1Supp: su_size = 1; goto addr_case;
      case Addr2Supp: su_size = 2; goto addr_case;
      case Addr4Supp: su_size = 4; goto addr_case;
      case Addr8Supp: su_size = 8; goto addr_case;
      addr_case:
         return (ekind == AddrErr && err_extra->size == su_size);

      case FreeSupp:
         return (ekind == FreeErr || ekind == FreeMismatchErr);

      case LeakSupp:
         return False; /* Doesn't match any normal error */

      default:
         VG_(printf)("Error:\n"
                     "  unknown suppression type %d\n",
                     VG_(get_supp_kind)(su));
         VG_(skin_panic)("unknown suppression type in "
                         "SK_(error_matches_suppression)");
   }
}

#  undef STREQ

/*------------------------------------------------------------*/
/*--- Crude profiling machinery.                           ---*/
/*------------------------------------------------------------*/

/* Event index.  If just the name of the fn is given, this means the
   number of calls to the fn.  Otherwise it is the specified event.
   Ones marked 'M' are MemCheck only.  Ones marked 'A' are AddrCheck only.
   The rest are shared.

   10   alloc_secondary_map

   20   get_abit
M  21   get_vbyte
   22   set_abit
M  23   set_vbyte
   24   get_abits4_ALIGNED
M  25   get_vbytes4_ALIGNED       

   30   set_address_range_perms
   31   set_address_range_perms(lower byte loop)
   32   set_address_range_perms(quadword loop)
   33   set_address_range_perms(upper byte loop)
   
   35   make_noaccess
   36   make_writable
   37   make_readable
A  38   make_accessible

   40   copy_address_range_state
   41   copy_address_range_state(byte loop)
   42   check_writable
   43   check_writable(byte loop)
   44   check_readable
   45   check_readable(byte loop)
   46   check_readable_asciiz
   47   check_readable_asciiz(byte loop)
A  48   check_accessible
A  49   check_accessible(byte loop)

   50   make_noaccess_aligned
   51   make_writable_aligned

M  60   helperc_LOADV4
M  61   helperc_STOREV4
M  62   helperc_LOADV2
M  63   helperc_STOREV2
M  64   helperc_LOADV1
M  65   helperc_STOREV1

A  66   helperc_ACCESS4
A  67   helperc_ACCESS2
A  68   helperc_ACCESS1

M  70   rim_rd_V4_SLOWLY
M  71   rim_wr_V4_SLOWLY
M  72   rim_rd_V2_SLOWLY
M  73   rim_wr_V2_SLOWLY
M  74   rim_rd_V1_SLOWLY
M  75   rim_wr_V1_SLOWLY

A  76   ACCESS4_SLOWLY
A  77   ACCESS2_SLOWLY
A  78   ACCESS1_SLOWLY

   80   fpu_read
   81   fpu_read aligned 4
   82   fpu_read aligned 8
   83   fpu_read 2
   84   fpu_read 10/28/108

M  85   fpu_write
M  86   fpu_write aligned 4
M  87   fpu_write aligned 8
M  88   fpu_write 2
M  89   fpu_write 10/28/108

   90   fpu_access
   91   fpu_access aligned 4
   92   fpu_access aligned 8
   93   fpu_access 2
   94   fpu_access 10/28/108

   100  fpu_access_check_SLOWLY
   101  fpu_access_check_SLOWLY(byte loop)

   110  new_mem_stack_4
   111  new_mem_stack_8
   112  new_mem_stack_12
   113  new_mem_stack_16
   114  new_mem_stack_32
   115  new_mem_stack

   120  die_mem_stack_4
   121  die_mem_stack_8
   122  die_mem_stack_12
   123  die_mem_stack_16
   124  die_mem_stack_32
   125  die_mem_stack
*/

#ifdef VG_PROFILE_MEMORY

UInt MC_(event_ctr)[N_PROF_EVENTS];

void MC_(init_prof_mem) ( void )
{
   Int i;
   for (i = 0; i < N_PROF_EVENTS; i++)
      MC_(event_ctr)[i] = 0;
}

void MC_(done_prof_mem) ( void )
{
   Int i;
   for (i = 0; i < N_PROF_EVENTS; i++) {
      if ((i % 10) == 0) 
         VG_(printf)("\n");
      if (MC_(event_ctr)[i] > 0)
         VG_(printf)( "prof mem event %2d: %d\n", i, MC_(event_ctr)[i] );
   }
   VG_(printf)("\n");
}

#else

void MC_(init_prof_mem) ( void ) { }
void MC_(done_prof_mem) ( void ) { }

#endif

/*------------------------------------------------------------*/
/*--- Shadow chunks info                                   ---*/
/*------------------------------------------------------------*/

void MC_(set_where)( ShadowChunk* sc, ExeContext* ec )
{
   VG_(set_sc_extra)( sc, 0, (UInt)ec );
}

ExeContext *MC_(get_where)( ShadowChunk* sc )
{
   return (ExeContext*)VG_(get_sc_extra)(sc, 0);
}

void SK_(complete_shadow_chunk) ( ShadowChunk* sc, ThreadState* tst )
{
   VG_(set_sc_extra) ( sc, 0, (UInt)VG_(get_ExeContext)(tst) );
}


/*------------------------------------------------------------*/
/*--- Postponing free()ing                                 ---*/
/*------------------------------------------------------------*/

/* Holds blocks after freeing. */
static ShadowChunk* freed_list_start  = NULL;
static ShadowChunk* freed_list_end    = NULL;
static Int          freed_list_volume = 0;

__attribute__ ((unused))
Int MC_(count_freelist) ( void )
{
   ShadowChunk* sc;
   Int n = 0;
   for (sc = freed_list_start; sc != NULL; sc = VG_(get_sc_next)(sc))
      n++;
   return n;
}

__attribute__ ((unused))
void MC_(freelist_sanity) ( void )
{
   ShadowChunk* sc;
   Int n = 0;
   /* VG_(printf)("freelist sanity\n"); */
   for (sc = freed_list_start; sc != NULL; sc = VG_(get_sc_next)(sc))
      n += VG_(get_sc_size)(sc);
   sk_assert(n == freed_list_volume);
}

/* Put a shadow chunk on the freed blocks queue, possibly freeing up
   some of the oldest blocks in the queue at the same time. */
static void add_to_freed_queue ( ShadowChunk* sc )
{
   ShadowChunk* sc1;

   /* Put it at the end of the freed list */
   if (freed_list_end == NULL) {
      sk_assert(freed_list_start == NULL);
      freed_list_end = freed_list_start = sc;
      freed_list_volume = VG_(get_sc_size)(sc);
   } else {    
      sk_assert(VG_(get_sc_next)(freed_list_end) == NULL);
      VG_(set_sc_next)(freed_list_end, sc);
      freed_list_end = sc;
      freed_list_volume += VG_(get_sc_size)(sc);
   }
   VG_(set_sc_next)(sc, NULL);

   /* Release enough of the oldest blocks to bring the free queue
      volume below vg_clo_freelist_vol. */
   
   while (freed_list_volume > MC_(clo_freelist_vol)) {
      /* freelist_sanity(); */
      sk_assert(freed_list_start != NULL);
      sk_assert(freed_list_end != NULL);

      sc1 = freed_list_start;
      freed_list_volume -= VG_(get_sc_size)(sc1);
      /* VG_(printf)("volume now %d\n", freed_list_volume); */
      sk_assert(freed_list_volume >= 0);

      if (freed_list_start == freed_list_end) {
         freed_list_start = freed_list_end = NULL;
      } else {
         freed_list_start = VG_(get_sc_next)(sc1);
      }
      VG_(set_sc_next)(sc1, NULL); /* just paranoia */
      VG_(free_ShadowChunk) ( sc1 );
   }
}

/* Return the first shadow chunk satisfying the predicate p. */
ShadowChunk* MC_(any_matching_freed_ShadowChunks) ( Bool (*p)(ShadowChunk*) )
{
   ShadowChunk* sc;

   /* No point looking through freed blocks if we're not keeping
      them around for a while... */
   for (sc = freed_list_start; sc != NULL; sc = VG_(get_sc_next)(sc))
      if (p(sc))
         return sc;

   return NULL;
}

void SK_(alt_free) ( ShadowChunk* sc, ThreadState* tst )
{
   /* Record where freed */
   MC_(set_where)( sc, VG_(get_ExeContext) ( tst ) );

   /* Put it out of harm's way for a while. */
   add_to_freed_queue ( sc );
}

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

/*--------------------------------------------------------------------*/
/*--- end                                              mc_common.c ---*/
/*--------------------------------------------------------------------*/
