
/*--------------------------------------------------------------------*/
/*--- Code that is shared between MemCheck and AddrCheck.          ---*/
/*---                                                  mac_needs.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of MemCheck, a heavyweight Valgrind tool for
   detecting memory errors, and AddrCheck, a lightweight Valgrind tool 
   for detecting memory errors.

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


#include "mac_shared.h"

#include "memcheck.h"   /* for VG_USERREQ__* */

/*------------------------------------------------------------*/
/*--- Defns                                                ---*/
/*------------------------------------------------------------*/

/* These many bytes below %ESP are considered addressible if we're
   doing the --workaround-gcc296-bugs hack. */
#define VG_GCC296_BUG_STACK_SLOP 1024

/*------------------------------------------------------------*/
/*--- Command line options                                 ---*/
/*------------------------------------------------------------*/

Bool  MAC_(clo_partial_loads_ok)       = True;
Int   MAC_(clo_freelist_vol)           = 1000000;
Bool  MAC_(clo_leak_check)             = False;
VgRes MAC_(clo_leak_resolution)        = Vg_LowRes;
Bool  MAC_(clo_show_reachable)         = False;
Bool  MAC_(clo_workaround_gcc296_bugs) = False;

Bool MAC_(process_common_cmd_line_option)(Char* arg)
{
        VG_BOOL_CLO("--leak-check",            MAC_(clo_leak_check))
   else VG_BOOL_CLO("--partial-loads-ok",      MAC_(clo_partial_loads_ok))
   else VG_BOOL_CLO("--show-reachable",        MAC_(clo_show_reachable))
   else VG_BOOL_CLO("--workaround-gcc296-bugs",MAC_(clo_workaround_gcc296_bugs))
   
   else VG_BNUM_CLO("--freelist-vol",  MAC_(clo_freelist_vol), 0, 1000000000)
   
   else if (VG_CLO_STREQ(arg, "--leak-resolution=low"))
      MAC_(clo_leak_resolution) = Vg_LowRes;
   else if (VG_CLO_STREQ(arg, "--leak-resolution=med"))
      MAC_(clo_leak_resolution) = Vg_MedRes;
   else if (VG_CLO_STREQ(arg, "--leak-resolution=high"))
      MAC_(clo_leak_resolution) = Vg_HighRes;

   else
      return VG_(replacement_malloc_process_cmd_line_option)(arg);

   return True;
}

void MAC_(print_common_usage)(void)
{
   VG_(printf)(
"    --partial-loads-ok=no|yes too hard to explain here; see manual [yes]\n"
"    --freelist-vol=<number>   volume of freed blocks queue [1000000]\n"
"    --leak-check=no|yes       search for memory leaks at exit? [no]\n"
"    --leak-resolution=low|med|high  how much bt merging in leak check [low]\n"
"    --show-reachable=no|yes   show reachable blocks in leak check? [no]\n"
"    --workaround-gcc296-bugs=no|yes  self explanatory [no]\n"
   );
   VG_(replacement_malloc_print_usage)();
}

void MAC_(print_common_debug_usage)(void)
{
   VG_(replacement_malloc_print_debug_usage)();
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

void MAC_(clear_MAC_Error) ( MAC_Error* err_extra )
{
   err_extra->axskind   = ReadAxs;
   err_extra->size      = 0;
   clear_AddrInfo ( &err_extra->addrinfo );
   err_extra->isUnaddr  = True;
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

Bool TL_(eq_Error) ( VgRes res, Error* e1, Error* e2 )
{
   MAC_Error* e1_extra = VG_(get_error_extra)(e1);
   MAC_Error* e2_extra = VG_(get_error_extra)(e2);

   /* Guaranteed by calling function */
   tl_assert(VG_(get_error_kind)(e1) == VG_(get_error_kind)(e2));
   
   switch (VG_(get_error_kind)(e1)) {
      case CoreMemErr: {
         Char *e1s, *e2s;
         if (e1_extra->isUnaddr != e2_extra->isUnaddr) return False;
         e1s = VG_(get_error_string)(e1);
         e2s = VG_(get_error_string)(e2);
         if (e1s == e2s)                               return True;
         if (0 == VG_(strcmp)(e1s, e2s))               return True;
         return False;
      }

      case UserErr:
      case ParamErr:
         if (e1_extra->isUnaddr != e2_extra->isUnaddr)         return False;
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

      case OverlapErr:
         return True;

      case LeakErr:
         VG_(tool_panic)("Shouldn't get LeakErr in TL_(eq_Error),\n"
                         "since it's handled with VG_(unique_error)()!");

      case IllegalMempoolErr:
         return True;

      default: 
         VG_(printf)("Error:\n  unknown error code %d\n",
                     VG_(get_error_kind)(e1));
         VG_(tool_panic)("unknown error code in TL_(eq_Error)");
   }
}

void MAC_(pp_AddrInfo) ( Addr a, AddrInfo* ai )
{
   switch (ai->akind) {
      case Stack: 
         VG_(message)(Vg_UserMsg, 
                      " Address 0x%x is on thread %d's stack", 
                      a, ai->stack_tid);
         break;
      case Unknown:
         if (ai->maybe_gcc) {
            VG_(message)(Vg_UserMsg, 
               " Address 0x%x is just below %%esp.  Possibly a bug in GCC/G++",
               a);
            VG_(message)(Vg_UserMsg, 
               "  v 2.96 or 3.0.X.  To suppress, use: --workaround-gcc296-bugs=yes");
	 } else {
            VG_(message)(Vg_UserMsg, 
               " Address 0x%x is not stack'd, malloc'd or (recently) free'd",a);
         }
         break;
      case Freed: case Mallocd: case UserG: case Mempool: {
         SizeT delta;
         UChar* relative;
         UChar* kind;
         if (ai->akind == Mempool) {
            kind = "mempool";
         } else {
            kind = "block";
         }
         if (ai->rwoffset < 0) {
            delta    = (SizeT)(- ai->rwoffset);
            relative = "before";
         } else if (ai->rwoffset >= ai->blksize) {
            delta    = ai->rwoffset - ai->blksize;
            relative = "after";
         } else {
            delta    = ai->rwoffset;
            relative = "inside";
         }
         VG_(message)(Vg_UserMsg, 
            " Address 0x%x is %llu bytes %s a %s of size %d %s",
            a, (ULong)delta, relative, kind,
            ai->blksize,
            ai->akind==Mallocd ? "alloc'd" 
               : ai->akind==Freed ? "free'd" 
                                  : "client-defined");
         VG_(pp_ExeContext)(ai->lastchange);
         break;
      }
      case Register:
         // print nothing
         tl_assert(0 == a);
         break;
      default:
         VG_(tool_panic)("MAC_(pp_AddrInfo)");
   }
}

/* This prints out the message for the error types where Memcheck and
   Addrcheck have identical messages */
void MAC_(pp_shared_Error) ( Error* err )
{
   MAC_Error* err_extra = VG_(get_error_extra)(err);

   switch (VG_(get_error_kind)(err)) {
      case FreeErr:
         VG_(message)(Vg_UserMsg, "Invalid free() / delete / delete[]");
         /* fall through */
      case FreeMismatchErr:
         if (VG_(get_error_kind)(err) == FreeMismatchErr)
            VG_(message)(Vg_UserMsg, 
                         "Mismatched free() / delete / delete []");
         VG_(pp_ExeContext)( VG_(get_error_where)(err) );
         MAC_(pp_AddrInfo)(VG_(get_error_address)(err), &err_extra->addrinfo);
         break;

      case AddrErr:
         switch (err_extra->axskind) {
            case ReadAxs:
               VG_(message)(Vg_UserMsg, "Invalid read of size %d", 
                                        err_extra->size ); 
               break;
            case WriteAxs:
               VG_(message)(Vg_UserMsg, "Invalid write of size %d", 
                                        err_extra->size ); 
               break;
            case ExecAxs:
               VG_(message)(Vg_UserMsg, "Jump to the invalid address "
                                        "stated on the next line");
               break;
            default: 
               VG_(tool_panic)("TL_(pp_shared_Error)(axskind)");
         }
         VG_(pp_ExeContext)( VG_(get_error_where)(err) );
         MAC_(pp_AddrInfo)(VG_(get_error_address)(err), &err_extra->addrinfo);
         break;

      case OverlapErr: {
         OverlapExtra* ov_extra = (OverlapExtra*)VG_(get_error_extra)(err);
         if (ov_extra->len == -1)
            VG_(message)(Vg_UserMsg,
                         "Source and destination overlap in %s(%p, %p)",
                         VG_(get_error_string)(err),
                         ov_extra->dst, ov_extra->src);
         else
            VG_(message)(Vg_UserMsg,
                         "Source and destination overlap in %s(%p, %p, %d)",
                         VG_(get_error_string)(err),
                         ov_extra->dst, ov_extra->src, ov_extra->len);
         VG_(pp_ExeContext)( VG_(get_error_where)(err) );
         break;
      }
      case LeakErr: {
         /* Totally abusing the types of these spare fields... oh well. */
         UInt n_this_record   = (UWord)VG_(get_error_address)(err);
         UInt n_total_records = (UWord)VG_(get_error_string) (err);

         MAC_(pp_LeakError)(err_extra, n_this_record, n_total_records);
         break;
      }

      case IllegalMempoolErr:
         VG_(message)(Vg_UserMsg, "Illegal memory pool address");
         VG_(pp_ExeContext)( VG_(get_error_where)(err) );
         MAC_(pp_AddrInfo)(VG_(get_error_address)(err), &err_extra->addrinfo);
         break;

      default: 
         VG_(printf)("Error:\n  unknown Memcheck/Addrcheck error code %d\n",
                     VG_(get_error_kind)(err));
         VG_(tool_panic)("unknown error code in MAC_(pp_shared_Error)");
   }
}

/*------------------------------------------------------------*/
/*--- Recording errors                                     ---*/
/*------------------------------------------------------------*/

/* Additional description function for describe_addr();  used by
   MemCheck for user blocks, which Addrcheck doesn't support. */
Bool (*MAC_(describe_addr_supp)) ( Addr a, AddrInfo* ai ) = NULL;

/* Callback for searching thread stacks */
static Bool addr_is_in_bounds(Addr stack_min, Addr stack_max, void *ap)
{
   Addr a = *(Addr *)ap;
  
   return (stack_min <= a && a <= stack_max);
}

/* Callback for searching free'd list */
static Bool addr_is_in_MAC_Chunk(MAC_Chunk* mc, void *ap)
{
   Addr a = *(Addr *)ap;
  
   return VG_(addr_is_in_block)( a, mc->data, mc->size );
}

/* Callback for searching malloc'd lists */
static Bool addr_is_in_HashNode(VgHashNode* sh_ch, void *ap)
{
   return addr_is_in_MAC_Chunk( (MAC_Chunk*)sh_ch, ap );
}

/* Describe an address as best you can, for error messages,
   putting the result in ai. */
static void describe_addr ( Addr a, AddrInfo* ai )
{
   MAC_Chunk* sc;
   ThreadId   tid;

   /* Perhaps it's a user-def'd block ?  (only check if requested, though) */
   if (NULL != MAC_(describe_addr_supp)) {
      if (MAC_(describe_addr_supp)( a, ai ))
         return;
   }
   /* Perhaps it's on a thread's stack? */
   tid = VG_(first_matching_thread_stack)(addr_is_in_bounds, &a);
   if (tid != VG_INVALID_THREADID) {
      ai->akind     = Stack;
      ai->stack_tid = tid;
      return;
   }
   /* Search for a recently freed block which might bracket it. */
   sc = MAC_(first_matching_freed_MAC_Chunk)(addr_is_in_MAC_Chunk, &a);
   if (NULL != sc) {
      ai->akind      = Freed;
      ai->blksize    = sc->size;
      ai->rwoffset   = (Int)a - (Int)sc->data;
      ai->lastchange = sc->where;
      return;
   }
   /* Search for a currently malloc'd block which might bracket it. */
   sc = (MAC_Chunk*)VG_(HT_first_match)(MAC_(malloc_list), addr_is_in_HashNode, &a);
   if (NULL != sc) {
      ai->akind      = Mallocd;
      ai->blksize    = sc->size;
      ai->rwoffset   = (Int)(a) - (Int)sc->data;
      ai->lastchange = sc->where;
      return;
   }
   /* Clueless ... */
   ai->akind = Unknown;
   return;
}

/* Is this address within some small distance below %ESP?  Used only
   for the --workaround-gcc296-bugs kludge. */
static Bool is_just_below_ESP( Addr esp, Addr aa )
{
   if (esp > aa && (esp - aa) <= VG_GCC296_BUG_STACK_SLOP)
      return True;
   else
      return False;
}

/* This one called from generated code and non-generated code. */

void MAC_(record_address_error) ( ThreadId tid, Addr a, Int size,
                                  Bool isWrite )
{
   MAC_Error err_extra;
   Bool      just_below_esp;

   just_below_esp = is_just_below_ESP( 
                       VG_(get_stack_pointer)(tid),
                       a 
                    );

   /* If this is caused by an access immediately below %ESP, and the
      user asks nicely, we just ignore it. */
   if (MAC_(clo_workaround_gcc296_bugs) && just_below_esp)
      return;

   MAC_(clear_MAC_Error)( &err_extra );
   err_extra.axskind = isWrite ? WriteAxs : ReadAxs;
   err_extra.size    = size;
   err_extra.addrinfo.akind     = Undescribed;
   err_extra.addrinfo.maybe_gcc = just_below_esp;
   VG_(maybe_record_error)( tid, AddrErr, a, /*s*/NULL, &err_extra );
}

/* These ones are called from non-generated code */

/* This is for memory errors in pthread functions, as opposed to pthread API
   errors which are found by the core. */
void MAC_(record_core_mem_error) ( ThreadId tid, Bool isUnaddr, Char* msg )
{
   MAC_Error err_extra;

   MAC_(clear_MAC_Error)( &err_extra );
   err_extra.isUnaddr = isUnaddr;
   VG_(maybe_record_error)( tid, CoreMemErr, /*addr*/0, msg, &err_extra );
}

void MAC_(record_param_error) ( ThreadId tid, Addr a, Bool isReg,
                                Bool isUnaddr, Char* msg )
{
   MAC_Error err_extra;

   tl_assert(VG_INVALID_THREADID != tid);
   MAC_(clear_MAC_Error)( &err_extra );
   err_extra.addrinfo.akind = ( isReg ? Register : Undescribed );
   err_extra.isUnaddr = isUnaddr;
   VG_(maybe_record_error)( tid, ParamErr, a, msg, &err_extra );
}

void MAC_(record_jump_error) ( ThreadId tid, Addr a )
{
   MAC_Error err_extra;

   tl_assert(VG_INVALID_THREADID != tid);
   MAC_(clear_MAC_Error)( &err_extra );
   err_extra.axskind = ExecAxs;
   err_extra.size    = 1;     // size only used for suppressions
   err_extra.addrinfo.akind = Undescribed;
   VG_(maybe_record_error)( tid, AddrErr, a, /*s*/NULL, &err_extra );
}

void MAC_(record_free_error) ( ThreadId tid, Addr a ) 
{
   MAC_Error err_extra;

   tl_assert(VG_INVALID_THREADID != tid);
   MAC_(clear_MAC_Error)( &err_extra );
   err_extra.addrinfo.akind = Undescribed;
   VG_(maybe_record_error)( tid, FreeErr, a, /*s*/NULL, &err_extra );
}

void MAC_(record_illegal_mempool_error) ( ThreadId tid, Addr a ) 
{
   MAC_Error err_extra;

   tl_assert(VG_INVALID_THREADID != tid);
   MAC_(clear_MAC_Error)( &err_extra );
   err_extra.addrinfo.akind = Undescribed;
   VG_(maybe_record_error)( tid, IllegalMempoolErr, a, /*s*/NULL, &err_extra );
}

void MAC_(record_freemismatch_error) ( ThreadId tid, Addr a )
{
   MAC_Error err_extra;

   tl_assert(VG_INVALID_THREADID != tid);
   MAC_(clear_MAC_Error)( &err_extra );
   err_extra.addrinfo.akind = Undescribed;
   VG_(maybe_record_error)( tid, FreeMismatchErr, a, /*s*/NULL, &err_extra );
}

void MAC_(record_overlap_error) ( ThreadId tid, 
                                  Char* function, OverlapExtra* ov_extra )
{
   VG_(maybe_record_error)( 
      tid, OverlapErr, /*addr*/0, /*s*/function, ov_extra );
}


/* Updates the copy with address info if necessary (but not for all errors). */
UInt TL_(update_extra)( Error* err )
{
   switch (VG_(get_error_kind)(err)) {
   case ValueErr:
   case CoreMemErr:
   case AddrErr: 
   case ParamErr:
   case UserErr:
   case FreeErr:
   case IllegalMempoolErr:
   case FreeMismatchErr: {
      MAC_Error* extra = VG_(get_error_extra)(err);
      if (extra != NULL && Undescribed == extra->addrinfo.akind) {
         describe_addr ( VG_(get_error_address)(err), &(extra->addrinfo) );
      }
      return sizeof(MAC_Error);
   }
   /* Don't need to return the correct size -- LeakErrs are always shown with
      VG_(unique_error)() so they're not copied anyway. */
   case LeakErr:     return 0;
   case OverlapErr:  return sizeof(OverlapExtra);
   default: VG_(tool_panic)("update_extra: bad errkind");
   }
}


/*------------------------------------------------------------*/
/*--- Suppressions                                         ---*/
/*------------------------------------------------------------*/

Bool MAC_(shared_recognised_suppression) ( Char* name, Supp* su )
{
   SuppKind skind;

   if      (VG_STREQ(name, "Param"))   skind = ParamSupp;
   else if (VG_STREQ(name, "CoreMem")) skind = CoreMemSupp;
   else if (VG_STREQ(name, "Addr1"))   skind = Addr1Supp;
   else if (VG_STREQ(name, "Addr2"))   skind = Addr2Supp;
   else if (VG_STREQ(name, "Addr4"))   skind = Addr4Supp;
   else if (VG_STREQ(name, "Addr8"))   skind = Addr8Supp;
   else if (VG_STREQ(name, "Addr16"))  skind = Addr16Supp;
   else if (VG_STREQ(name, "Free"))    skind = FreeSupp;
   else if (VG_STREQ(name, "Leak"))    skind = LeakSupp;
   else if (VG_STREQ(name, "Overlap")) skind = OverlapSupp;
   else if (VG_STREQ(name, "Mempool")) skind = MempoolSupp;
   else
      return False;

   VG_(set_supp_kind)(su, skind);
   return True;
}

Bool TL_(read_extra_suppression_info) ( Int fd, Char* buf, Int nBuf, Supp *su )
{
   Bool eof;

   if (VG_(get_supp_kind)(su) == ParamSupp) {
      eof = VG_(get_line) ( fd, buf, nBuf );
      if (eof) return False;
      VG_(set_supp_string)(su, VG_(strdup)(buf));
   }
   return True;
}

Bool TL_(error_matches_suppression)(Error* err, Supp* su)
{
   Int        su_size;
   MAC_Error* err_extra = VG_(get_error_extra)(err);
   ErrorKind  ekind     = VG_(get_error_kind )(err);

   switch (VG_(get_supp_kind)(su)) {
      case ParamSupp:
         return (ekind == ParamErr 
              && VG_STREQ(VG_(get_error_string)(err), 
                          VG_(get_supp_string)(su)));

      case CoreMemSupp:
         return (ekind == CoreMemErr
              && VG_STREQ(VG_(get_error_string)(err),
                          VG_(get_supp_string)(su)));

      case Value0Supp: su_size = 0; goto value_case;
      case Value1Supp: su_size = 1; goto value_case;
      case Value2Supp: su_size = 2; goto value_case;
      case Value4Supp: su_size = 4; goto value_case;
      case Value8Supp: su_size = 8; goto value_case;
      case Value16Supp:su_size =16; goto value_case;
      value_case:
         return (ekind == ValueErr && err_extra->size == su_size);

      case Addr1Supp: su_size = 1; goto addr_case;
      case Addr2Supp: su_size = 2; goto addr_case;
      case Addr4Supp: su_size = 4; goto addr_case;
      case Addr8Supp: su_size = 8; goto addr_case;
      case Addr16Supp:su_size =16; goto addr_case;
      addr_case:
         return (ekind == AddrErr && err_extra->size == su_size);

      case FreeSupp:
         return (ekind == FreeErr || ekind == FreeMismatchErr);

      case OverlapSupp:
         return (ekind = OverlapErr);

      case LeakSupp:
         return (ekind == LeakErr);

      case MempoolSupp:
         return (ekind == IllegalMempoolErr);

      default:
         VG_(printf)("Error:\n"
                     "  unknown suppression type %d\n",
                     VG_(get_supp_kind)(su));
         VG_(tool_panic)("unknown suppression type in "
                         "TL_(error_matches_suppression)");
   }
}

Char* TL_(get_error_name) ( Error* err )
{
   Char* s;
   switch (VG_(get_error_kind)(err)) {
   case ParamErr:           return "Param";
   case UserErr:            return NULL;  /* Can't suppress User errors */
   case FreeMismatchErr:    return "Free";
   case IllegalMempoolErr:  return "Mempool";
   case FreeErr:            return "Free";
   case AddrErr:            
      switch ( ((MAC_Error*)VG_(get_error_extra)(err))->size ) {
      case 1:               return "Addr1";
      case 2:               return "Addr2";
      case 4:               return "Addr4";
      case 8:               return "Addr8";
      case 16:              return "Addr16";
      default:              VG_(tool_panic)("unexpected size for Addr");
      }
     
   case ValueErr:
      switch ( ((MAC_Error*)VG_(get_error_extra)(err))->size ) {
      case 0:               return "Cond";
      case 1:               return "Value1";
      case 2:               return "Value2";
      case 4:               return "Value4";
      case 8:               return "Value8";
      case 16:              return "Value16";
      default:              VG_(tool_panic)("unexpected size for Value");
      }
   case CoreMemErr:         return "CoreMem";
   case OverlapErr:         return "Overlap";
   case LeakErr:            return "Leak";
   default:                 VG_(tool_panic)("get_error_name: unexpected type");
   }
   VG_(printf)(s);
}

void TL_(print_extra_suppression_info) ( Error* err )
{
   if (ParamErr == VG_(get_error_kind)(err)) {
      VG_(printf)("   %s\n", VG_(get_error_string)(err));
   }
}

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
   84   fpu_read 10/28/108/512

M  85   fpu_write
M  86   fpu_write aligned 4
M  87   fpu_write aligned 8
M  88   fpu_write 2
M  89   fpu_write 10/28/108/512

   90   fpu_access
   91   fpu_access aligned 4
   92   fpu_access aligned 8
   93   fpu_access 2
   94   fpu_access 10/28/108/512

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

#ifdef MAC_PROFILE_MEMORY

UInt MAC_(event_ctr)[N_PROF_EVENTS];

static void init_prof_mem ( void )
{
   Int i;
   for (i = 0; i < N_PROF_EVENTS; i++)
      MAC_(event_ctr)[i] = 0;
}

static void done_prof_mem ( void )
{
   Int i;
   for (i = 0; i < N_PROF_EVENTS; i++) {
      if ((i % 10) == 0) 
         VG_(printf)("\n");
      if (MAC_(event_ctr)[i] > 0)
         VG_(printf)( "prof mem event %2d: %d\n", i, MAC_(event_ctr)[i] );
   }
   VG_(printf)("\n");
}

#else

static void init_prof_mem ( void ) { }
static void done_prof_mem ( void ) { }

#endif

/*------------------------------------------------------------*/
/*--- Common initialisation + finalisation                 ---*/
/*------------------------------------------------------------*/

void MAC_(common_pre_clo_init)(void)
{
   MAC_(malloc_list) = VG_(HT_construct)();
   MAC_(mempool_list) = VG_(HT_construct)();
   init_prof_mem();
}

void MAC_(common_fini)(void (*leak_check)(ThreadId))
{
   MAC_(print_malloc_stats)();

   if (VG_(clo_verbosity) == 1) {
      if (!MAC_(clo_leak_check))
         VG_(message)(Vg_UserMsg, 
             "For a detailed leak analysis,  rerun with: --leak-check=yes");

      VG_(message)(Vg_UserMsg, 
                   "For counts of detected errors, rerun with: -v");
   }
   if (MAC_(clo_leak_check)) 
      leak_check( 1/*bogus ThreadID*/ );

   done_prof_mem();
}

/*------------------------------------------------------------*/
/*--- Common client request handling                       ---*/
/*------------------------------------------------------------*/

Bool MAC_(handle_common_client_requests)(ThreadId tid, UWord* arg, UWord* ret )
{
   Char* err  = 
         "The client requests VALGRIND_MALLOCLIKE_BLOCK and\n"
      "   VALGRIND_FREELIKE_BLOCK have moved.  Please recompile your\n"
      "   program to incorporate the updates in the Valgrind header files.\n"
      "   You shouldn't need to change the text of your program at all.\n"
      "   Everything should then work as before.  Sorry for the bother.\n";
   
   switch (arg[0]) {
   case VG_USERREQ__COUNT_LEAKS: { /* count leaked bytes */
      UWord** argp = (UWord**)arg;
      // MAC_(bytes_leaked) et al were set by the last leak check (or zero
      // if no prior leak checks performed).
      *argp[1] = MAC_(bytes_leaked);
      *argp[2] = MAC_(bytes_dubious);
      *argp[3] = MAC_(bytes_reachable);
      *argp[4] = MAC_(bytes_suppressed);
      *ret = 0;
      return True;
   }
   case VG_USERREQ__MALLOCLIKE_BLOCK__OLD_DO_NOT_USE:
   case VG_USERREQ__FREELIKE_BLOCK__OLD_DO_NOT_USE:
      VG_(tool_panic)(err);

   case VG_USERREQ__MALLOCLIKE_BLOCK: {
      Addr p         = (Addr)arg[1];
      SizeT sizeB    =       arg[2];
      UInt rzB       =       arg[3];
      Bool is_zeroed = (Bool)arg[4];

      MAC_(new_block) ( tid, p, sizeB, /*ignored*/0, rzB, is_zeroed, 
                        MAC_AllocCustom, MAC_(malloc_list) );
      return True;
   }
   case VG_USERREQ__FREELIKE_BLOCK: {
      Addr p         = (Addr)arg[1];
      UInt rzB       =       arg[2];

      MAC_(handle_free) ( tid, p, rzB, MAC_AllocCustom );
      return True;
   }

   case _VG_USERREQ__MEMCHECK_GET_RECORD_OVERLAP:
      *ret = (Addr)MAC_(record_overlap_error);
      return True;

   case VG_USERREQ__CREATE_MEMPOOL: {
      Addr pool      = (Addr)arg[1];
      UInt rzB       =       arg[2];
      Bool is_zeroed = (Bool)arg[3];

      MAC_(create_mempool) ( pool, rzB, is_zeroed );
      return True;
   }

   case VG_USERREQ__DESTROY_MEMPOOL: {
      Addr pool      = (Addr)arg[1];

      MAC_(destroy_mempool) ( pool );
      return True;
   }

   case VG_USERREQ__MEMPOOL_ALLOC: {
      Addr pool      = (Addr)arg[1];
      Addr addr      = (Addr)arg[2];
      UInt size      =       arg[3];

      MAC_(mempool_alloc) ( tid, pool, addr, size );
      return True;
   }

   case VG_USERREQ__MEMPOOL_FREE: {
      Addr pool      = (Addr)arg[1];
      Addr addr      = (Addr)arg[2];

      MAC_(mempool_free) ( pool, addr );
      return True;
   }

   default:
      return False;
   }
}

/*--------------------------------------------------------------------*/
/*--- end                                              mac_needs.c ---*/
/*--------------------------------------------------------------------*/
