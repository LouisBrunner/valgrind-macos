
/*--------------------------------------------------------------------*/
/*--- Management, printing, etc, of errors and suppressions.       ---*/
/*---                                                  mc_errors.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of MemCheck, a heavyweight Valgrind tool for
   detecting memory errors.

   Copyright (C) 2000-2013 Julian Seward 
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

#include "pub_tool_basics.h"
#include "pub_tool_gdbserver.h"
#include "pub_tool_poolalloc.h"     // For mc_include.h
#include "pub_tool_hashtable.h"     // For mc_include.h
#include "pub_tool_libcbase.h"
#include "pub_tool_libcassert.h"
#include "pub_tool_libcprint.h"
#include "pub_tool_machine.h"
#include "pub_tool_mallocfree.h"
#include "pub_tool_options.h"
#include "pub_tool_replacemalloc.h"
#include "pub_tool_tooliface.h"
#include "pub_tool_threadstate.h"
#include "pub_tool_debuginfo.h"     // VG_(get_dataname_and_offset)
#include "pub_tool_xarray.h"
#include "pub_tool_addrinfo.h"

#include "mc_include.h"


/*------------------------------------------------------------*/
/*--- Error types                                          ---*/
/*------------------------------------------------------------*/

/* See comment in mc_include.h */
Bool MC_(any_value_errors) = False;


/* ------------------ Errors ----------------------- */

/* What kind of error it is. */
typedef 
   enum { 
      Err_Value,
      Err_Cond,
      Err_CoreMem,
      Err_Addr, 
      Err_Jump, 
      Err_RegParam,
      Err_MemParam,
      Err_User,
      Err_Free,
      Err_FreeMismatch,
      Err_Overlap,
      Err_Leak,
      Err_IllegalMempool,
   }
   MC_ErrorTag;


typedef struct _MC_Error MC_Error;

struct _MC_Error {
   // Nb: we don't need the tag here, as it's stored in the Error type! Yuk.
   //MC_ErrorTag tag;

   union {
      // Use of an undefined value:
      // - as a pointer in a load or store
      // - as a jump target
      struct {
         SizeT szB;   // size of value in bytes
         // Origin info
         UInt        otag;      // origin tag
         ExeContext* origin_ec; // filled in later
      } Value;

      // Use of an undefined value in a conditional branch or move.
      struct {
         // Origin info
         UInt        otag;      // origin tag
         ExeContext* origin_ec; // filled in later
      } Cond;

      // Addressability error in core (signal-handling) operation.
      // It would be good to get rid of this error kind, merge it with
      // another one somehow.
      struct {
      } CoreMem;

      // Use of an unaddressable memory location in a load or store.
      struct {
         Bool     isWrite;    // read or write?
         SizeT    szB;        // not used for exec (jump) errors
         Bool     maybe_gcc;  // True if just below %esp -- could be a gcc bug
         AddrInfo ai;
      } Addr;

      // Jump to an unaddressable memory location.
      struct {
         AddrInfo ai;
      } Jump;

      // System call register input contains undefined bytes.
      struct {
         // Origin info
         UInt        otag;      // origin tag
         ExeContext* origin_ec; // filled in later
      } RegParam;

      // System call memory input contains undefined/unaddressable bytes
      struct {
         Bool     isAddrErr;  // Addressability or definedness error?
         AddrInfo ai;
         // Origin info
         UInt        otag;      // origin tag
         ExeContext* origin_ec; // filled in later
      } MemParam;

      // Problem found from a client request like CHECK_MEM_IS_ADDRESSABLE.
      struct {
         Bool     isAddrErr;  // Addressability or definedness error?
         AddrInfo ai;
         // Origin info
         UInt        otag;      // origin tag
         ExeContext* origin_ec; // filled in later
      } User;

      // Program tried to free() something that's not a heap block (this
      // covers double-frees). */
      struct {
         AddrInfo ai;
      } Free;

      // Program allocates heap block with one function
      // (malloc/new/new[]/custom) and deallocates with not the matching one.
      struct {
         AddrInfo ai;
      } FreeMismatch;

      // Call to strcpy, memcpy, etc, with overlapping blocks.
      struct {
         Addr  src;   // Source block
         Addr  dst;   // Destination block
         SizeT szB;   // Size in bytes;  0 if unused.
      } Overlap;

      // A memory leak.
      struct {
         UInt        n_this_record;
         UInt        n_total_records;
         LossRecord* lr;
      } Leak;

      // A memory pool error.
      struct {
         AddrInfo ai;
      } IllegalMempool;

   } Err;
};


/*------------------------------------------------------------*/
/*--- Printing errors                                      ---*/
/*------------------------------------------------------------*/

/* This is the "this error is due to be printed shortly; so have a
   look at it any print any preamble you want" function.  Which, in
   Memcheck, we don't use.  Hence a no-op.
*/
void MC_(before_pp_Error) ( Error* err ) {
}

/* Do a printf-style operation on either the XML or normal output
   channel, depending on the setting of VG_(clo_xml).
*/
static void emit_WRK ( const HChar* format, va_list vargs )
{
   if (VG_(clo_xml)) {
      VG_(vprintf_xml)(format, vargs);
   } else {
      VG_(vmessage)(Vg_UserMsg, format, vargs);
   }
}
static void emit ( const HChar* format, ... ) PRINTF_CHECK(1, 2);
static void emit ( const HChar* format, ... )
{
   va_list vargs;
   va_start(vargs, format);
   emit_WRK(format, vargs);
   va_end(vargs);
}


static const HChar* str_leak_lossmode ( Reachedness lossmode )
{
   const HChar *loss = "?";
   switch (lossmode) {
      case Unreached:    loss = "definitely lost"; break;
      case IndirectLeak: loss = "indirectly lost"; break;
      case Possible:     loss = "possibly lost"; break;
      case Reachable:    loss = "still reachable"; break;
   }
   return loss;
}

static const HChar* xml_leak_kind ( Reachedness lossmode )
{
   const HChar *loss = "?";
   switch (lossmode) {
      case Unreached:    loss = "Leak_DefinitelyLost"; break;
      case IndirectLeak: loss = "Leak_IndirectlyLost"; break;
      case Possible:     loss = "Leak_PossiblyLost"; break;
      case Reachable:    loss = "Leak_StillReachable"; break;
   }
   return loss;
}

Bool MC_(parse_leak_kinds) ( const HChar* str0, UInt* lks )
{
   return VG_(parse_enum_set)("reachable,possible,indirect,definite",
                              str0, lks);
}

static const HChar* pp_Reachedness_for_leak_kinds(Reachedness r)
{
   switch(r) {
   case Reachable:    return "reachable";
   case Possible:     return "possible";
   case IndirectLeak: return "indirect";
   case Unreached:    return "definite";
   default:           tl_assert(0);
   }
}

static void mc_pp_origin ( ExeContext* ec, UInt okind )
{
   const HChar* src = NULL;
   tl_assert(ec);

   switch (okind) {
      case MC_OKIND_STACK:   src = " by a stack allocation"; break;
      case MC_OKIND_HEAP:    src = " by a heap allocation"; break;
      case MC_OKIND_USER:    src = " by a client request"; break;
      case MC_OKIND_UNKNOWN: src = ""; break;
   }
   tl_assert(src); /* guards against invalid 'okind' */

   if (VG_(clo_xml)) {
      emit( "  <auxwhat>Uninitialised value was created%s</auxwhat>\n",
            src);
      VG_(pp_ExeContext)( ec );
   } else {
      emit( " Uninitialised value was created%s\n", src);
      VG_(pp_ExeContext)( ec );
   }
}

HChar * MC_(snprintf_delta) (HChar * buf, Int size, 
                             SizeT current_val, SizeT old_val, 
                             LeakCheckDeltaMode delta_mode)
{
   if (delta_mode == LCD_Any)
      buf[0] = '\0';
   else if (current_val >= old_val)
      VG_(snprintf) (buf, size, " (+%'lu)", current_val - old_val);
   else
      VG_(snprintf) (buf, size, " (-%'lu)", old_val - current_val);

   return buf;
}

static void pp_LossRecord(UInt n_this_record, UInt n_total_records,
                          LossRecord* lr, Bool xml)
{
   // char arrays to produce the indication of increase/decrease in case
   // of delta_mode != LCD_Any
   HChar d_bytes[20];
   HChar d_direct_bytes[20];
   HChar d_indirect_bytes[20];
   HChar d_num_blocks[20];

   MC_(snprintf_delta) (d_bytes, 20, 
                        lr->szB + lr->indirect_szB, 
                        lr->old_szB + lr->old_indirect_szB,
                        MC_(detect_memory_leaks_last_delta_mode));
   MC_(snprintf_delta) (d_direct_bytes, 20,
                        lr->szB,
                        lr->old_szB,
                        MC_(detect_memory_leaks_last_delta_mode));
   MC_(snprintf_delta) (d_indirect_bytes, 20,
                        lr->indirect_szB,
                        lr->old_indirect_szB,
                        MC_(detect_memory_leaks_last_delta_mode));
   MC_(snprintf_delta) (d_num_blocks, 20,
                        (SizeT) lr->num_blocks,
                        (SizeT) lr->old_num_blocks,
                        MC_(detect_memory_leaks_last_delta_mode));

   if (xml) {
      emit("  <kind>%s</kind>\n", xml_leak_kind(lr->key.state));
      if (lr->indirect_szB > 0) {
         emit( "  <xwhat>\n" );
         emit( "    <text>%'lu%s (%'lu%s direct, %'lu%s indirect) bytes "
               "in %'u%s blocks"
               " are %s in loss record %'u of %'u</text>\n",
               lr->szB + lr->indirect_szB, d_bytes,
               lr->szB, d_direct_bytes,
               lr->indirect_szB, d_indirect_bytes,
               lr->num_blocks, d_num_blocks,
               str_leak_lossmode(lr->key.state),
               n_this_record, n_total_records );
         // Nb: don't put commas in these XML numbers 
         emit( "    <leakedbytes>%lu</leakedbytes>\n",
               lr->szB + lr->indirect_szB );
         emit( "    <leakedblocks>%u</leakedblocks>\n", lr->num_blocks );
         emit( "  </xwhat>\n" );
      } else {
         emit( "  <xwhat>\n" );
         emit( "    <text>%'lu%s bytes in %'u%s blocks"
               " are %s in loss record %'u of %'u</text>\n",
               lr->szB, d_direct_bytes,
               lr->num_blocks, d_num_blocks,
               str_leak_lossmode(lr->key.state), 
               n_this_record, n_total_records );
         emit( "    <leakedbytes>%ld</leakedbytes>\n", lr->szB);
         emit( "    <leakedblocks>%d</leakedblocks>\n", lr->num_blocks);
         emit( "  </xwhat>\n" );
      }
      VG_(pp_ExeContext)(lr->key.allocated_at);
   } else { /* ! if (xml) */
      if (lr->indirect_szB > 0) {
         emit(
            "%'lu%s (%'lu%s direct, %'lu%s indirect) bytes in %'u%s blocks"
            " are %s in loss record %'u of %'u\n",
            lr->szB + lr->indirect_szB, d_bytes,
            lr->szB, d_direct_bytes,
            lr->indirect_szB, d_indirect_bytes,
            lr->num_blocks, d_num_blocks,
            str_leak_lossmode(lr->key.state),
            n_this_record, n_total_records
         );
      } else {
         emit(
            "%'lu%s bytes in %'u%s blocks are %s in loss record %'u of %'u\n",
            lr->szB, d_direct_bytes,
            lr->num_blocks, d_num_blocks,
            str_leak_lossmode(lr->key.state),
            n_this_record, n_total_records
         );
      }
      VG_(pp_ExeContext)(lr->key.allocated_at);
   } /* if (xml) */
}

void MC_(pp_LossRecord)(UInt n_this_record, UInt n_total_records,
                        LossRecord* l)
{
   pp_LossRecord (n_this_record, n_total_records, l, /* xml */ False);
}

void MC_(pp_Error) ( Error* err )
{
   const Bool xml  = VG_(clo_xml); /* a shorthand */
   MC_Error* extra = VG_(get_error_extra)(err);

   switch (VG_(get_error_kind)(err)) {
      case Err_CoreMem:
         /* What the hell *is* a CoreMemError? jrs 2005-May-18 */
         /* As of 2006-Dec-14, it's caused by unaddressable bytes in a
            signal handler frame.  --njn */
         // JRS 17 May 09: None of our regtests exercise this; hence AFAIK
         // the following code is untested.  Bad.
         if (xml) {
            emit( "  <kind>CoreMemError</kind>\n" );
            emit( "  <what>%pS contains unaddressable byte(s)</what>\n",
                  VG_(get_error_string)(err));
            VG_(pp_ExeContext)( VG_(get_error_where)(err) );
         } else {
            emit( "%s contains unaddressable byte(s)\n",
                  VG_(get_error_string)(err));
            VG_(pp_ExeContext)( VG_(get_error_where)(err) );
         }
         break;
      
      case Err_Value:
         MC_(any_value_errors) = True;
         if (xml) {
            emit( "  <kind>UninitValue</kind>\n" );
            emit( "  <what>Use of uninitialised value of size %ld</what>\n",
                  extra->Err.Value.szB );
            VG_(pp_ExeContext)( VG_(get_error_where)(err) );
            if (extra->Err.Value.origin_ec)
               mc_pp_origin( extra->Err.Value.origin_ec,
                            extra->Err.Value.otag & 3 );
         } else {
            /* Could also show extra->Err.Cond.otag if debugging origin
               tracking */
            emit( "Use of uninitialised value of size %ld\n",
                  extra->Err.Value.szB );
            VG_(pp_ExeContext)( VG_(get_error_where)(err) );
            if (extra->Err.Value.origin_ec)
               mc_pp_origin( extra->Err.Value.origin_ec,
                            extra->Err.Value.otag & 3 );
         }
         break;

      case Err_Cond:
         MC_(any_value_errors) = True;
         if (xml) {
            emit( "  <kind>UninitCondition</kind>\n" );
            emit( "  <what>Conditional jump or move depends"
                  " on uninitialised value(s)</what>\n" );
            VG_(pp_ExeContext)( VG_(get_error_where)(err) );
            if (extra->Err.Cond.origin_ec)
               mc_pp_origin( extra->Err.Cond.origin_ec,
                             extra->Err.Cond.otag & 3 );
         } else {
            /* Could also show extra->Err.Cond.otag if debugging origin
               tracking */
            emit( "Conditional jump or move depends"
                  " on uninitialised value(s)\n" );
            VG_(pp_ExeContext)( VG_(get_error_where)(err) );
            if (extra->Err.Cond.origin_ec)
               mc_pp_origin( extra->Err.Cond.origin_ec,
                             extra->Err.Cond.otag & 3 );
         }
         break;

      case Err_RegParam:
         MC_(any_value_errors) = True;
         if (xml) {
            emit( "  <kind>SyscallParam</kind>\n" );
            emit( "  <what>Syscall param %pS contains "
                  "uninitialised byte(s)</what>\n",
                  VG_(get_error_string)(err) );
            VG_(pp_ExeContext)( VG_(get_error_where)(err) );
            if (extra->Err.RegParam.origin_ec)
               mc_pp_origin( extra->Err.RegParam.origin_ec,
                             extra->Err.RegParam.otag & 3 );
         } else {
            emit( "Syscall param %s contains uninitialised byte(s)\n",
                  VG_(get_error_string)(err) );
            VG_(pp_ExeContext)( VG_(get_error_where)(err) );
            if (extra->Err.RegParam.origin_ec)
               mc_pp_origin( extra->Err.RegParam.origin_ec,
                             extra->Err.RegParam.otag & 3 );
         }
         break;

      case Err_MemParam:
         if (!extra->Err.MemParam.isAddrErr)
            MC_(any_value_errors) = True;
         if (xml) {
            emit( "  <kind>SyscallParam</kind>\n" );
            emit( "  <what>Syscall param %pS points to %s byte(s)</what>\n",
                  VG_(get_error_string)(err),
                  extra->Err.MemParam.isAddrErr 
                     ? "unaddressable" : "uninitialised" );
            VG_(pp_ExeContext)( VG_(get_error_where)(err) );
            VG_(pp_addrinfo_mc)(VG_(get_error_address)(err),
                                &extra->Err.MemParam.ai, False);
            if (extra->Err.MemParam.origin_ec 
                && !extra->Err.MemParam.isAddrErr)
               mc_pp_origin( extra->Err.MemParam.origin_ec,
                             extra->Err.MemParam.otag & 3 );
         } else {
            emit( "Syscall param %s points to %s byte(s)\n",
                  VG_(get_error_string)(err),
                  extra->Err.MemParam.isAddrErr 
                     ? "unaddressable" : "uninitialised" );
            VG_(pp_ExeContext)( VG_(get_error_where)(err) );
            VG_(pp_addrinfo_mc)(VG_(get_error_address)(err),
                                &extra->Err.MemParam.ai, False);
            if (extra->Err.MemParam.origin_ec 
                && !extra->Err.MemParam.isAddrErr)
               mc_pp_origin( extra->Err.MemParam.origin_ec,
                             extra->Err.MemParam.otag & 3 );
         }
         break;

      case Err_User:
         if (!extra->Err.User.isAddrErr)
            MC_(any_value_errors) = True;
         if (xml) { 
            emit( "  <kind>ClientCheck</kind>\n" );
            emit( "  <what>%s byte(s) found "
                  "during client check request</what>\n", 
                   extra->Err.User.isAddrErr
                      ? "Unaddressable" : "Uninitialised" );
            VG_(pp_ExeContext)( VG_(get_error_where)(err) );
            VG_(pp_addrinfo_mc)(VG_(get_error_address)(err), &extra->Err.User.ai,
                                False);
            if (extra->Err.User.origin_ec && !extra->Err.User.isAddrErr)
               mc_pp_origin( extra->Err.User.origin_ec,
                             extra->Err.User.otag & 3 );
         } else {
            emit( "%s byte(s) found during client check request\n", 
                   extra->Err.User.isAddrErr
                      ? "Unaddressable" : "Uninitialised" );
            VG_(pp_ExeContext)( VG_(get_error_where)(err) );
            VG_(pp_addrinfo_mc)(VG_(get_error_address)(err), &extra->Err.User.ai,
                                False);
            if (extra->Err.User.origin_ec && !extra->Err.User.isAddrErr)
               mc_pp_origin( extra->Err.User.origin_ec,
                             extra->Err.User.otag & 3 );
         }
         break;

      case Err_Free:
         if (xml) {
            emit( "  <kind>InvalidFree</kind>\n" );
            emit( "  <what>Invalid free() / delete / delete[]"
                  " / realloc()</what>\n" );
            VG_(pp_ExeContext)( VG_(get_error_where)(err) );
            VG_(pp_addrinfo_mc)( VG_(get_error_address)(err),
                                 &extra->Err.Free.ai, False );
         } else {
            emit( "Invalid free() / delete / delete[] / realloc()\n" );
            VG_(pp_ExeContext)( VG_(get_error_where)(err) );
            VG_(pp_addrinfo_mc)( VG_(get_error_address)(err),
                                 &extra->Err.Free.ai, False );
         }
         break;

      case Err_FreeMismatch:
         if (xml) {
            emit( "  <kind>MismatchedFree</kind>\n" );
            emit( "  <what>Mismatched free() / delete / delete []</what>\n" );
            VG_(pp_ExeContext)( VG_(get_error_where)(err) );
            VG_(pp_addrinfo_mc)(VG_(get_error_address)(err),
                                &extra->Err.FreeMismatch.ai, False);
         } else {
            emit( "Mismatched free() / delete / delete []\n" );
            VG_(pp_ExeContext)( VG_(get_error_where)(err) );
            VG_(pp_addrinfo_mc)(VG_(get_error_address)(err),
                                &extra->Err.FreeMismatch.ai, False);
         }
         break;

      case Err_Addr:
         if (xml) {
            emit( "  <kind>Invalid%s</kind>\n",
                  extra->Err.Addr.isWrite ? "Write" : "Read"  );
            emit( "  <what>Invalid %s of size %ld</what>\n",
                  extra->Err.Addr.isWrite ? "write" : "read",
                  extra->Err.Addr.szB );
            VG_(pp_ExeContext)( VG_(get_error_where)(err) );
            VG_(pp_addrinfo_mc)( VG_(get_error_address)(err),
                                 &extra->Err.Addr.ai,
                                 extra->Err.Addr.maybe_gcc );
         } else {
            emit( "Invalid %s of size %ld\n",
                  extra->Err.Addr.isWrite ? "write" : "read",
                  extra->Err.Addr.szB );
            VG_(pp_ExeContext)( VG_(get_error_where)(err) );

            VG_(pp_addrinfo_mc)( VG_(get_error_address)(err),
                                 &extra->Err.Addr.ai,
                                 extra->Err.Addr.maybe_gcc );
         }
         break;

      case Err_Jump:
         if (xml) {
            emit( "  <kind>InvalidJump</kind>\n" );
            emit( "  <what>Jump to the invalid address stated "
                  "on the next line</what>\n" );
            VG_(pp_ExeContext)( VG_(get_error_where)(err) );
            VG_(pp_addrinfo_mc)( VG_(get_error_address)(err), &extra->Err.Jump.ai,
                                 False );
         } else {
            emit( "Jump to the invalid address stated on the next line\n" );
            VG_(pp_ExeContext)( VG_(get_error_where)(err) );
            VG_(pp_addrinfo_mc)( VG_(get_error_address)(err), &extra->Err.Jump.ai,
                                 False );
         }
         break;

      case Err_Overlap:
         if (xml) {
            emit( "  <kind>Overlap</kind>\n" );
            if (extra->Err.Overlap.szB == 0) {
               emit( "  <what>Source and destination overlap "
                     "in %pS(%#lx, %#lx)\n</what>\n",
                     VG_(get_error_string)(err),
                     extra->Err.Overlap.dst, extra->Err.Overlap.src );
            } else {
               emit( "  <what>Source and destination overlap "
                     "in %s(%#lx, %#lx, %lu)</what>\n",
                     VG_(get_error_string)(err),
                     extra->Err.Overlap.dst, extra->Err.Overlap.src,
                     extra->Err.Overlap.szB );
            }
            VG_(pp_ExeContext)( VG_(get_error_where)(err) );
         } else {
            if (extra->Err.Overlap.szB == 0) {
               emit( "Source and destination overlap in %pS(%#lx, %#lx)\n",
                     VG_(get_error_string)(err),
                     extra->Err.Overlap.dst, extra->Err.Overlap.src );
            } else {
               emit( "Source and destination overlap in %s(%#lx, %#lx, %lu)\n",
                     VG_(get_error_string)(err),
                     extra->Err.Overlap.dst, extra->Err.Overlap.src,
                     extra->Err.Overlap.szB );
            }
            VG_(pp_ExeContext)( VG_(get_error_where)(err) );
         }
         break;

      case Err_IllegalMempool:
         // JRS 17 May 09: None of our regtests exercise this; hence AFAIK
         // the following code is untested.  Bad.
         if (xml) {
            emit( "  <kind>InvalidMemPool</kind>\n" );
            emit( "  <what>Illegal memory pool address</what>\n" );
            VG_(pp_ExeContext)( VG_(get_error_where)(err) );
            VG_(pp_addrinfo_mc)( VG_(get_error_address)(err),
                                 &extra->Err.IllegalMempool.ai, False );
         } else {
            emit( "Illegal memory pool address\n" );
            VG_(pp_ExeContext)( VG_(get_error_where)(err) );
            VG_(pp_addrinfo_mc)( VG_(get_error_address)(err),
                                 &extra->Err.IllegalMempool.ai, False );
         }
         break;

      case Err_Leak: {
         UInt        n_this_record   = extra->Err.Leak.n_this_record;
         UInt        n_total_records = extra->Err.Leak.n_total_records;
         LossRecord* lr              = extra->Err.Leak.lr;
         pp_LossRecord (n_this_record, n_total_records, lr, xml);
         break;
      }

      default: 
         VG_(printf)("Error:\n  unknown Memcheck error code %d\n",
                     VG_(get_error_kind)(err));
         VG_(tool_panic)("unknown error code in mc_pp_Error)");
   }
}

/*------------------------------------------------------------*/
/*--- Recording errors                                     ---*/
/*------------------------------------------------------------*/

/* These many bytes below %ESP are considered addressible if we're
   doing the --workaround-gcc296-bugs hack. */
#define VG_GCC296_BUG_STACK_SLOP 1024

/* Is this address within some small distance below %ESP?  Used only
   for the --workaround-gcc296-bugs kludge. */
static Bool is_just_below_ESP( Addr esp, Addr aa )
{
   esp -= VG_STACK_REDZONE_SZB;
   if (esp > aa && (esp - aa) <= VG_GCC296_BUG_STACK_SLOP)
      return True;
   else
      return False;
}

/* --- Called from generated and non-generated code --- */

void MC_(record_address_error) ( ThreadId tid, Addr a, Int szB,
                                 Bool isWrite )
{
   MC_Error extra;
   Bool     just_below_esp;

   if (MC_(in_ignored_range)(a)) 
      return;

   if (VG_(is_watched)( (isWrite ? write_watchpoint : read_watchpoint), a, szB))
      return;

   just_below_esp = is_just_below_ESP( VG_(get_SP)(tid), a );

   /* If this is caused by an access immediately below %ESP, and the
      user asks nicely, we just ignore it. */
   if (MC_(clo_workaround_gcc296_bugs) && just_below_esp)
      return;

   extra.Err.Addr.isWrite   = isWrite;
   extra.Err.Addr.szB       = szB;
   extra.Err.Addr.maybe_gcc = just_below_esp;
   extra.Err.Addr.ai.tag    = Addr_Undescribed;
   VG_(maybe_record_error)( tid, Err_Addr, a, /*s*/NULL, &extra );
}

void MC_(record_value_error) ( ThreadId tid, Int szB, UInt otag )
{
   MC_Error extra;
   tl_assert( MC_(clo_mc_level) >= 2 );
   if (otag > 0)
      tl_assert( MC_(clo_mc_level) == 3 );
   extra.Err.Value.szB       = szB;
   extra.Err.Value.otag      = otag;
   extra.Err.Value.origin_ec = NULL;  /* Filled in later */
   VG_(maybe_record_error)( tid, Err_Value, /*addr*/0, /*s*/NULL, &extra );
}

void MC_(record_cond_error) ( ThreadId tid, UInt otag )
{
   MC_Error extra;
   tl_assert( MC_(clo_mc_level) >= 2 );
   if (otag > 0)
      tl_assert( MC_(clo_mc_level) == 3 );
   extra.Err.Cond.otag      = otag;
   extra.Err.Cond.origin_ec = NULL;  /* Filled in later */
   VG_(maybe_record_error)( tid, Err_Cond, /*addr*/0, /*s*/NULL, &extra );
}

/* --- Called from non-generated code --- */

/* This is for memory errors in signal-related memory. */
void MC_(record_core_mem_error) ( ThreadId tid, const HChar* msg )
{
   VG_(maybe_record_error)( tid, Err_CoreMem, /*addr*/0, msg, /*extra*/NULL );
}

void MC_(record_regparam_error) ( ThreadId tid, const HChar* msg, UInt otag )
{
   MC_Error extra;
   tl_assert(VG_INVALID_THREADID != tid);
   if (otag > 0)
      tl_assert( MC_(clo_mc_level) == 3 );
   extra.Err.RegParam.otag      = otag;
   extra.Err.RegParam.origin_ec = NULL;  /* Filled in later */
   VG_(maybe_record_error)( tid, Err_RegParam, /*addr*/0, msg, &extra );
}

void MC_(record_memparam_error) ( ThreadId tid, Addr a, 
                                  Bool isAddrErr, const HChar* msg, UInt otag )
{
   MC_Error extra;
   tl_assert(VG_INVALID_THREADID != tid);
   if (!isAddrErr) 
      tl_assert( MC_(clo_mc_level) >= 2 );
   if (otag != 0) {
      tl_assert( MC_(clo_mc_level) == 3 );
      tl_assert( !isAddrErr );
   }
   extra.Err.MemParam.isAddrErr = isAddrErr;
   extra.Err.MemParam.ai.tag    = Addr_Undescribed;
   extra.Err.MemParam.otag      = otag;
   extra.Err.MemParam.origin_ec = NULL;  /* Filled in later */
   VG_(maybe_record_error)( tid, Err_MemParam, a, msg, &extra );
}

void MC_(record_jump_error) ( ThreadId tid, Addr a )
{
   MC_Error extra;
   tl_assert(VG_INVALID_THREADID != tid);
   extra.Err.Jump.ai.tag = Addr_Undescribed;
   VG_(maybe_record_error)( tid, Err_Jump, a, /*s*/NULL, &extra );
}

void MC_(record_free_error) ( ThreadId tid, Addr a ) 
{
   MC_Error extra;
   tl_assert(VG_INVALID_THREADID != tid);
   extra.Err.Free.ai.tag = Addr_Undescribed;
   VG_(maybe_record_error)( tid, Err_Free, a, /*s*/NULL, &extra );
}

void MC_(record_freemismatch_error) ( ThreadId tid, MC_Chunk* mc )
{
   MC_Error extra;
   AddrInfo* ai = &extra.Err.FreeMismatch.ai;
   tl_assert(VG_INVALID_THREADID != tid);
   ai->tag = Addr_Block;
   ai->Addr.Block.block_kind = Block_Mallocd;  // Nb: Not 'Block_Freed'
   ai->Addr.Block.block_desc = "block";
   ai->Addr.Block.block_szB  = mc->szB;
   ai->Addr.Block.rwoffset   = 0;
   ai->Addr.Block.allocated_at = MC_(allocated_at) (mc);
   ai->Addr.Block.freed_at = MC_(freed_at) (mc);
   VG_(maybe_record_error)( tid, Err_FreeMismatch, mc->data, /*s*/NULL,
                            &extra );
}

void MC_(record_illegal_mempool_error) ( ThreadId tid, Addr a ) 
{
   MC_Error extra;
   tl_assert(VG_INVALID_THREADID != tid);
   extra.Err.IllegalMempool.ai.tag = Addr_Undescribed;
   VG_(maybe_record_error)( tid, Err_IllegalMempool, a, /*s*/NULL, &extra );
}

void MC_(record_overlap_error) ( ThreadId tid, const HChar* function,
                                 Addr src, Addr dst, SizeT szB )
{
   MC_Error extra;
   tl_assert(VG_INVALID_THREADID != tid);
   extra.Err.Overlap.src = src;
   extra.Err.Overlap.dst = dst;
   extra.Err.Overlap.szB = szB;
   VG_(maybe_record_error)( 
      tid, Err_Overlap, /*addr*/0, /*s*/function, &extra );
}

Bool MC_(record_leak_error) ( ThreadId tid, UInt n_this_record,
                              UInt n_total_records, LossRecord* lr,
                              Bool print_record, Bool count_error )
{
   MC_Error extra;
   extra.Err.Leak.n_this_record   = n_this_record;
   extra.Err.Leak.n_total_records = n_total_records;
   extra.Err.Leak.lr              = lr;
   return
   VG_(unique_error) ( tid, Err_Leak, /*Addr*/0, /*s*/NULL, &extra,
                       lr->key.allocated_at, print_record,
                       /*allow_GDB_attach*/False, count_error );
}

void MC_(record_user_error) ( ThreadId tid, Addr a,
                              Bool isAddrErr, UInt otag )
{
   MC_Error extra;
   if (otag != 0) {
      tl_assert(!isAddrErr);
      tl_assert( MC_(clo_mc_level) == 3 );
   }
   if (!isAddrErr) {
      tl_assert( MC_(clo_mc_level) >= 2 );
   }
   tl_assert(VG_INVALID_THREADID != tid);
   extra.Err.User.isAddrErr = isAddrErr;
   extra.Err.User.ai.tag    = Addr_Undescribed;
   extra.Err.User.otag      = otag;
   extra.Err.User.origin_ec = NULL;  /* Filled in later */
   VG_(maybe_record_error)( tid, Err_User, a, /*s*/NULL, &extra );
}

/*------------------------------------------------------------*/
/*--- Other error operations                               ---*/
/*------------------------------------------------------------*/

/* Compare error contexts, to detect duplicates.  Note that if they
   are otherwise the same, the faulting addrs and associated rwoffsets
   are allowed to be different.  */
Bool MC_(eq_Error) ( VgRes res, Error* e1, Error* e2 )
{
   MC_Error* extra1 = VG_(get_error_extra)(e1);
   MC_Error* extra2 = VG_(get_error_extra)(e2);

   /* Guaranteed by calling function */
   tl_assert(VG_(get_error_kind)(e1) == VG_(get_error_kind)(e2));
   
   switch (VG_(get_error_kind)(e1)) {
      case Err_CoreMem: {
         const HChar *e1s, *e2s;
         e1s = VG_(get_error_string)(e1);
         e2s = VG_(get_error_string)(e2);
         if (e1s == e2s)                   return True;
         if (VG_STREQ(e1s, e2s))           return True;
         return False;
      }

      case Err_RegParam:
         return VG_STREQ(VG_(get_error_string)(e1), VG_(get_error_string)(e2));

      // Perhaps we should also check the addrinfo.akinds for equality.
      // That would result in more error reports, but only in cases where
      // a register contains uninitialised bytes and points to memory
      // containing uninitialised bytes.  Currently, the 2nd of those to be
      // detected won't be reported.  That is (nearly?) always the memory
      // error, which is good.
      case Err_MemParam:
         if (!VG_STREQ(VG_(get_error_string)(e1),
                       VG_(get_error_string)(e2))) return False;
         // fall through
      case Err_User:
         return ( extra1->Err.User.isAddrErr == extra2->Err.User.isAddrErr
                ? True : False );

      case Err_Free:
      case Err_FreeMismatch:
      case Err_Jump:
      case Err_IllegalMempool:
      case Err_Overlap:
      case Err_Cond:
         return True;

      case Err_Addr:
         return ( extra1->Err.Addr.szB == extra2->Err.Addr.szB
                ? True : False );

      case Err_Value:
         return ( extra1->Err.Value.szB == extra2->Err.Value.szB
                ? True : False );

      case Err_Leak:
         VG_(tool_panic)("Shouldn't get Err_Leak in mc_eq_Error,\n"
                         "since it's handled with VG_(unique_error)()!");

      default: 
         VG_(printf)("Error:\n  unknown error code %d\n",
                     VG_(get_error_kind)(e1));
         VG_(tool_panic)("unknown error code in mc_eq_Error");
   }
}

/* Functions used when searching MC_Chunk lists */
static
Bool addr_is_in_MC_Chunk_default_REDZONE_SZB(MC_Chunk* mc, Addr a)
{
   return VG_(addr_is_in_block)( a, mc->data, mc->szB,
                                 MC_(Malloc_Redzone_SzB) );
}
static
Bool addr_is_in_MC_Chunk_with_REDZONE_SZB(MC_Chunk* mc, Addr a, SizeT rzB)
{
   return VG_(addr_is_in_block)( a, mc->data, mc->szB,
                                 rzB );
}

// Forward declarations
static Bool client_block_maybe_describe( Addr a, AddrInfo* ai );
static Bool mempool_block_maybe_describe( Addr a, AddrInfo* ai );


/* Describe an address as best you can, for error messages,
   putting the result in ai. */
static void describe_addr ( Addr a, /*OUT*/AddrInfo* ai )
{
   MC_Chunk*  mc;

   tl_assert(Addr_Undescribed == ai->tag);

   /* -- Perhaps it's a user-named block? -- */
   if (client_block_maybe_describe( a, ai )) {
      return;
   }
   /* -- Perhaps it's in mempool block? -- */
   if (mempool_block_maybe_describe( a, ai )) {
      return;
   }
   /* Blocks allocated by memcheck malloc functions are either
      on the recently freed list or on the malloc-ed list.
      Custom blocks can be on both : a recently freed block might
      have been just re-allocated.
      So, first search the malloc-ed block, as the most recent
      block is the probable cause of error.
      We however detect and report that this is a recently re-allocated
      block. */
   /* -- Search for a currently malloc'd block which might bracket it. -- */
   VG_(HT_ResetIter)(MC_(malloc_list));
   while ( (mc = VG_(HT_Next)(MC_(malloc_list))) ) {
      if (addr_is_in_MC_Chunk_default_REDZONE_SZB(mc, a)) {
         ai->tag = Addr_Block;
         ai->Addr.Block.block_kind = Block_Mallocd;
         if (MC_(get_freed_block_bracketting)( a ))
            ai->Addr.Block.block_desc = "recently re-allocated block";
         else
            ai->Addr.Block.block_desc = "block";
         ai->Addr.Block.block_szB  = mc->szB;
         ai->Addr.Block.rwoffset   = (Word)a - (Word)mc->data;
         ai->Addr.Block.allocated_at = MC_(allocated_at)(mc);
         ai->Addr.Block.freed_at = MC_(freed_at)(mc);
         return;
      }
   }
   /* -- Search for a recently freed block which might bracket it. -- */
   mc = MC_(get_freed_block_bracketting)( a );
   if (mc) {
      ai->tag = Addr_Block;
      ai->Addr.Block.block_kind = Block_Freed;
      ai->Addr.Block.block_desc = "block";
      ai->Addr.Block.block_szB  = mc->szB;
      ai->Addr.Block.rwoffset   = (Word)a - (Word)mc->data;
      ai->Addr.Block.allocated_at = MC_(allocated_at)(mc);
      ai->Addr.Block.freed_at = MC_(freed_at)(mc);
      return;
   }

   /* No block found. Search a non-heap block description. */
   VG_(describe_addr) (a, ai);
}

void MC_(pp_describe_addr) ( Addr a )
{
   AddrInfo ai;

   ai.tag = Addr_Undescribed;
   describe_addr (a, &ai);
   VG_(pp_addrinfo_mc) (a, &ai, /* maybe_gcc */ False);
}

/* Fill in *origin_ec as specified by otag, or NULL it out if otag
   does not refer to a known origin. */
static void update_origin ( /*OUT*/ExeContext** origin_ec,
                            UInt otag )
{
   UInt ecu = otag & ~3;
   *origin_ec = NULL;
   if (VG_(is_plausible_ECU)(ecu)) {
      *origin_ec = VG_(get_ExeContext_from_ECU)( ecu );
   }
}

/* Updates the copy with address info if necessary (but not for all errors). */
UInt MC_(update_Error_extra)( Error* err )
{
   MC_Error* extra = VG_(get_error_extra)(err);

   switch (VG_(get_error_kind)(err)) {
   // These ones don't have addresses associated with them, and so don't
   // need any updating.
   case Err_CoreMem:
   //case Err_Value:
   //case Err_Cond:
   case Err_Overlap:
   // For Err_Leaks the returned size does not matter -- they are always
   // shown with VG_(unique_error)() so they 'extra' not copied.  But
   // we make it consistent with the others.
   case Err_Leak:
      return sizeof(MC_Error);

   // For value errors, get the ExeContext corresponding to the
   // origin tag.  Note that it is a kludge to assume that 
   // a length-1 trace indicates a stack origin.  FIXME.
   case Err_Value:
      update_origin( &extra->Err.Value.origin_ec,
                     extra->Err.Value.otag );
      return sizeof(MC_Error);
   case Err_Cond:
      update_origin( &extra->Err.Cond.origin_ec,
                     extra->Err.Cond.otag );
      return sizeof(MC_Error);
   case Err_RegParam:
      update_origin( &extra->Err.RegParam.origin_ec,
                     extra->Err.RegParam.otag );
      return sizeof(MC_Error);

   // These ones always involve a memory address.
   case Err_Addr:
      describe_addr ( VG_(get_error_address)(err),
                      &extra->Err.Addr.ai );
      return sizeof(MC_Error);
   case Err_MemParam:
      describe_addr ( VG_(get_error_address)(err),
                      &extra->Err.MemParam.ai );
      update_origin( &extra->Err.MemParam.origin_ec,
                     extra->Err.MemParam.otag );
      return sizeof(MC_Error);
   case Err_Jump:
      describe_addr ( VG_(get_error_address)(err),
                      &extra->Err.Jump.ai );
      return sizeof(MC_Error);
   case Err_User:
      describe_addr ( VG_(get_error_address)(err),
                      &extra->Err.User.ai );
      update_origin( &extra->Err.User.origin_ec,
                     extra->Err.User.otag );
      return sizeof(MC_Error);
   case Err_Free:
      describe_addr ( VG_(get_error_address)(err),
                      &extra->Err.Free.ai );
      return sizeof(MC_Error);
   case Err_IllegalMempool:
      describe_addr ( VG_(get_error_address)(err),
                      &extra->Err.IllegalMempool.ai );
      return sizeof(MC_Error);

   // Err_FreeMismatches have already had their address described;  this is
   // possible because we have the MC_Chunk on hand when the error is
   // detected.  However, the address may be part of a user block, and if so
   // we override the pre-determined description with a user block one.
   case Err_FreeMismatch: {
      tl_assert(extra && Block_Mallocd ==
                extra->Err.FreeMismatch.ai.Addr.Block.block_kind);
      (void)client_block_maybe_describe( VG_(get_error_address)(err), 
                                        &extra->Err.FreeMismatch.ai );
      return sizeof(MC_Error);
   }

   default: VG_(tool_panic)("mc_update_extra: bad errkind");
   }
}


static Bool client_block_maybe_describe( Addr a,
                                         /*OUT*/AddrInfo* ai )
{
   UWord      i;
   CGenBlock* cgbs = NULL;
   UWord      cgb_used = 0;

   MC_(get_ClientBlock_array)( &cgbs, &cgb_used );
   if (cgbs == NULL)
      tl_assert(cgb_used == 0);

   /* Perhaps it's a general block ? */
   for (i = 0; i < cgb_used; i++) {
      if (cgbs[i].start == 0 && cgbs[i].size == 0) 
         continue;
      // Use zero as the redzone for client blocks.
      if (VG_(addr_is_in_block)(a, cgbs[i].start, cgbs[i].size, 0)) {
         ai->tag = Addr_Block;
         ai->Addr.Block.block_kind = Block_UserG;
         ai->Addr.Block.block_desc = cgbs[i].desc;
         ai->Addr.Block.block_szB  = cgbs[i].size;
         ai->Addr.Block.rwoffset   = (Word)(a) - (Word)(cgbs[i].start);
         ai->Addr.Block.allocated_at = cgbs[i].where;
         ai->Addr.Block.freed_at = VG_(null_ExeContext)();;
         return True;
      }
   }
   return False;
}


static Bool mempool_block_maybe_describe( Addr a,
                                          /*OUT*/AddrInfo* ai )
{
   MC_Mempool* mp;
   tl_assert( MC_(mempool_list) );

   VG_(HT_ResetIter)( MC_(mempool_list) );
   while ( (mp = VG_(HT_Next)(MC_(mempool_list))) ) {
      if (mp->chunks != NULL) {
         MC_Chunk* mc;
         VG_(HT_ResetIter)(mp->chunks);
         while ( (mc = VG_(HT_Next)(mp->chunks)) ) {
            if (addr_is_in_MC_Chunk_with_REDZONE_SZB(mc, a, mp->rzB)) {
               ai->tag = Addr_Block;
               ai->Addr.Block.block_kind = Block_MempoolChunk;
               ai->Addr.Block.block_desc = "block";
               ai->Addr.Block.block_szB  = mc->szB;
               ai->Addr.Block.rwoffset   = (Word)a - (Word)mc->data;
               ai->Addr.Block.allocated_at = MC_(allocated_at)(mc);
               ai->Addr.Block.freed_at = MC_(freed_at)(mc);
               return True;
            }
         }
      }
   }
   return False;
}


/*------------------------------------------------------------*/
/*--- Suppressions                                         ---*/
/*------------------------------------------------------------*/

typedef 
   enum { 
      ParamSupp,     // Bad syscall params
      UserSupp,      // Errors arising from client-request checks
      CoreMemSupp,   // Memory errors in core (pthread ops, signal handling)

      // Undefined value errors of given size
      Value1Supp, Value2Supp, Value4Supp, Value8Supp, Value16Supp,

      // Undefined value error in conditional.
      CondSupp,

      // Unaddressable read/write attempt at given size
      Addr1Supp, Addr2Supp, Addr4Supp, Addr8Supp, Addr16Supp,

      JumpSupp,      // Jump to unaddressable target
      FreeSupp,      // Invalid or mismatching free
      OverlapSupp,   // Overlapping blocks in memcpy(), strcpy(), etc
      LeakSupp,      // Something to be suppressed in a leak check.
      MempoolSupp,   // Memory pool suppression.
   } 
   MC_SuppKind;

Bool MC_(is_recognised_suppression) ( const HChar* name, Supp* su )
{
   SuppKind skind;

   if      (VG_STREQ(name, "Param"))   skind = ParamSupp;
   else if (VG_STREQ(name, "User"))    skind = UserSupp;
   else if (VG_STREQ(name, "CoreMem")) skind = CoreMemSupp;
   else if (VG_STREQ(name, "Addr1"))   skind = Addr1Supp;
   else if (VG_STREQ(name, "Addr2"))   skind = Addr2Supp;
   else if (VG_STREQ(name, "Addr4"))   skind = Addr4Supp;
   else if (VG_STREQ(name, "Addr8"))   skind = Addr8Supp;
   else if (VG_STREQ(name, "Addr16"))  skind = Addr16Supp;
   else if (VG_STREQ(name, "Jump"))    skind = JumpSupp;
   else if (VG_STREQ(name, "Free"))    skind = FreeSupp;
   else if (VG_STREQ(name, "Leak"))    skind = LeakSupp;
   else if (VG_STREQ(name, "Overlap")) skind = OverlapSupp;
   else if (VG_STREQ(name, "Mempool")) skind = MempoolSupp;
   else if (VG_STREQ(name, "Cond"))    skind = CondSupp;
   else if (VG_STREQ(name, "Value0"))  skind = CondSupp; /* backwards compat */
   else if (VG_STREQ(name, "Value1"))  skind = Value1Supp;
   else if (VG_STREQ(name, "Value2"))  skind = Value2Supp;
   else if (VG_STREQ(name, "Value4"))  skind = Value4Supp;
   else if (VG_STREQ(name, "Value8"))  skind = Value8Supp;
   else if (VG_STREQ(name, "Value16")) skind = Value16Supp;
   else 
      return False;

   VG_(set_supp_kind)(su, skind);
   return True;
}

typedef struct _MC_LeakSuppExtra MC_LeakSuppExtra;

struct _MC_LeakSuppExtra {
   UInt match_leak_kinds;

   /* Maintains nr of blocks and bytes suppressed with this suppression
      during the leak search identified by leak_search_gen.
      blocks_suppressed and bytes_suppressed are reset to 0 when
      used the first time during a leak search. */
   SizeT blocks_suppressed;
   SizeT bytes_suppressed;
   UInt  leak_search_gen;
};

Bool MC_(read_extra_suppression_info) ( Int fd, HChar** bufpp,
                                        SizeT* nBufp, Int* lineno, Supp *su )
{
   Bool eof;
   Int i;

   if (VG_(get_supp_kind)(su) == ParamSupp) {
      eof = VG_(get_line) ( fd, bufpp, nBufp, lineno );
      if (eof) return False;
      VG_(set_supp_string)(su, VG_(strdup)("mc.resi.1", *bufpp));
   } else if (VG_(get_supp_kind)(su) == LeakSupp) {
      // We might have the optional match-leak-kinds line
      MC_LeakSuppExtra* lse;
      lse = VG_(malloc)("mc.resi.2", sizeof(MC_LeakSuppExtra));
      lse->match_leak_kinds = RallS;
      lse->blocks_suppressed = 0;
      lse->bytes_suppressed = 0;
      lse->leak_search_gen = 0;
      VG_(set_supp_extra)(su, lse); // By default, all kinds will match.
      eof = VG_(get_line) ( fd, bufpp, nBufp, lineno );
      if (eof) return True; // old LeakSupp style, no match-leak-kinds line.
      if (0 == VG_(strncmp)(*bufpp, "match-leak-kinds:", 17)) {
         i = 17;
         while ((*bufpp)[i] && VG_(isspace((*bufpp)[i])))
            i++;
         if (!MC_(parse_leak_kinds)((*bufpp)+i, &lse->match_leak_kinds)) {
            return False;
         }
      } else {
         return False; // unknown extra line.
      }
   }
   return True;
}

Bool MC_(error_matches_suppression) ( Error* err, Supp* su )
{
   Int       su_szB;
   MC_Error* extra = VG_(get_error_extra)(err);
   ErrorKind ekind = VG_(get_error_kind )(err);

   switch (VG_(get_supp_kind)(su)) {
      case ParamSupp:
         return ((ekind == Err_RegParam || ekind == Err_MemParam)
              && VG_STREQ(VG_(get_error_string)(err), 
                          VG_(get_supp_string)(su)));

      case UserSupp:
         return (ekind == Err_User);

      case CoreMemSupp:
         return (ekind == Err_CoreMem
              && VG_STREQ(VG_(get_error_string)(err),
                          VG_(get_supp_string)(su)));

      case Value1Supp: su_szB = 1; goto value_case;
      case Value2Supp: su_szB = 2; goto value_case;
      case Value4Supp: su_szB = 4; goto value_case;
      case Value8Supp: su_szB = 8; goto value_case;
      case Value16Supp:su_szB =16; goto value_case;
      value_case:
         return (ekind == Err_Value && extra->Err.Value.szB == su_szB);

      case CondSupp:
         return (ekind == Err_Cond);

      case Addr1Supp: su_szB = 1; goto addr_case;
      case Addr2Supp: su_szB = 2; goto addr_case;
      case Addr4Supp: su_szB = 4; goto addr_case;
      case Addr8Supp: su_szB = 8; goto addr_case;
      case Addr16Supp:su_szB =16; goto addr_case;
      addr_case:
         return (ekind == Err_Addr && extra->Err.Addr.szB == su_szB);

      case JumpSupp:
         return (ekind == Err_Jump);

      case FreeSupp:
         return (ekind == Err_Free || ekind == Err_FreeMismatch);

      case OverlapSupp:
         return (ekind == Err_Overlap);

      case LeakSupp:
         if (ekind == Err_Leak) {
            MC_LeakSuppExtra* lse = (MC_LeakSuppExtra*) VG_(get_supp_extra)(su);
            if (lse->leak_search_gen != MC_(leak_search_gen)) {
               // First time we see this suppression during this leak search.
               // => reset the counters to 0.
               lse->blocks_suppressed = 0;
               lse->bytes_suppressed = 0;
               lse->leak_search_gen = MC_(leak_search_gen);
            }
            return RiS(extra->Err.Leak.lr->key.state, lse->match_leak_kinds);
         } else
            return False;

      case MempoolSupp:
         return (ekind == Err_IllegalMempool);

      default:
         VG_(printf)("Error:\n"
                     "  unknown suppression type %d\n",
                     VG_(get_supp_kind)(su));
         VG_(tool_panic)("unknown suppression type in "
                         "MC_(error_matches_suppression)");
   }
}

const HChar* MC_(get_error_name) ( Error* err )
{
   switch (VG_(get_error_kind)(err)) {
   case Err_RegParam:       return "Param";
   case Err_MemParam:       return "Param";
   case Err_User:           return "User";
   case Err_FreeMismatch:   return "Free";
   case Err_IllegalMempool: return "Mempool";
   case Err_Free:           return "Free";
   case Err_Jump:           return "Jump";
   case Err_CoreMem:        return "CoreMem";
   case Err_Overlap:        return "Overlap";
   case Err_Leak:           return "Leak";
   case Err_Cond:           return "Cond";
   case Err_Addr: {
      MC_Error* extra = VG_(get_error_extra)(err);
      switch ( extra->Err.Addr.szB ) {
      case 1:               return "Addr1";
      case 2:               return "Addr2";
      case 4:               return "Addr4";
      case 8:               return "Addr8";
      case 16:              return "Addr16";
      default:              VG_(tool_panic)("unexpected size for Addr");
      }
   }
   case Err_Value: {
      MC_Error* extra = VG_(get_error_extra)(err);
      switch ( extra->Err.Value.szB ) {
      case 1:               return "Value1";
      case 2:               return "Value2";
      case 4:               return "Value4";
      case 8:               return "Value8";
      case 16:              return "Value16";
      default:              VG_(tool_panic)("unexpected size for Value");
      }
   }
   default:                 VG_(tool_panic)("get_error_name: unexpected type");
   }
}

Bool MC_(get_extra_suppression_info) ( Error* err,
                                       /*OUT*/HChar* buf, Int nBuf )
{
   ErrorKind ekind = VG_(get_error_kind )(err);
   tl_assert(buf);
   tl_assert(nBuf >= 16); // stay sane
   if (Err_RegParam == ekind || Err_MemParam == ekind) {
      const HChar* errstr = VG_(get_error_string)(err);
      tl_assert(errstr);
      VG_(snprintf)(buf, nBuf-1, "%s", errstr);
      return True;
   } else if (Err_Leak == ekind) {
      MC_Error* extra = VG_(get_error_extra)(err);
      VG_(snprintf)
         (buf, nBuf-1, "match-leak-kinds: %s",
          pp_Reachedness_for_leak_kinds(extra->Err.Leak.lr->key.state));
      return True;
   } else {
      return False;
   }
}

Bool MC_(print_extra_suppression_use) ( Supp *su,
                                        /*OUT*/HChar *buf, Int nBuf )
{
   if (VG_(get_supp_kind)(su) == LeakSupp) {
      MC_LeakSuppExtra *lse = (MC_LeakSuppExtra*) VG_(get_supp_extra) (su);

      if (lse->leak_search_gen == MC_(leak_search_gen)
          && lse->blocks_suppressed > 0) {
         VG_(snprintf) (buf, nBuf-1, 
                        "suppressed: %'lu bytes in %'lu blocks",
                        lse->bytes_suppressed,
                        lse->blocks_suppressed);
         return True;
      } else
         return False;
   } else
      return False;
}

void MC_(update_extra_suppression_use) ( Error* err, Supp* su)
{
   if (VG_(get_supp_kind)(su) == LeakSupp) {
      MC_LeakSuppExtra *lse = (MC_LeakSuppExtra*) VG_(get_supp_extra) (su);
      MC_Error* extra = VG_(get_error_extra)(err);

      tl_assert (lse->leak_search_gen = MC_(leak_search_gen));
      lse->blocks_suppressed += extra->Err.Leak.lr->num_blocks;
      lse->bytes_suppressed 
         += extra->Err.Leak.lr->szB + extra->Err.Leak.lr->indirect_szB;
   }
}

/*--------------------------------------------------------------------*/
/*--- end                                              mc_errors.c ---*/
/*--------------------------------------------------------------------*/
