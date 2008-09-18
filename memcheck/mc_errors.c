
/*--------------------------------------------------------------------*/
/*--- Management, printing, etc, of errors and suppressions.       ---*/
/*---                                                  mc_errors.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of MemCheck, a heavyweight Valgrind tool for
   detecting memory errors.

   Copyright (C) 2000-2008 Julian Seward 
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

#include "mc_include.h"


/*------------------------------------------------------------*/
/*--- Error types                                          ---*/
/*------------------------------------------------------------*/

/* See comment in mc_include.h */
Bool MC_(any_value_errors) = False;


// Different kinds of blocks.
typedef enum {
   Block_Mallocd = 111,
   Block_Freed,
   Block_Mempool,
   Block_MempoolChunk,
   Block_UserG
} BlockKind;

/* ------------------ Addresses -------------------- */

/* The classification of a faulting address. */
typedef 
   enum { 
      Addr_Undescribed, // as-yet unclassified
      Addr_Unknown,     // classification yielded nothing useful
      Addr_Block,       // in malloc'd/free'd block
      Addr_Stack,       // on a thread's stack       
      Addr_DataSym,     // in a global data sym
      Addr_Variable,    // variable described by the debug info
      Addr_SectKind     // last-ditch classification attempt
   }
   AddrTag;

typedef
   struct _AddrInfo
   AddrInfo;

struct _AddrInfo {
   AddrTag tag;
   union {
      // As-yet unclassified.
      struct { } Undescribed;

      // On a stack.
      struct {
         ThreadId tid;        // Which thread's stack?
      } Stack;

      // This covers heap blocks (normal and from mempools) and user-defined
      // blocks.
      struct {
         BlockKind   block_kind;
         Char*       block_desc;    // "block", "mempool" or user-defined
         SizeT       block_szB;
         OffT        rwoffset;
         ExeContext* lastchange;
      } Block;

      // In a global .data symbol.  This holds the first 63 chars of
      // the variable's (zero terminated), plus an offset.
      struct {
         Char name[128];
         OffT offset;
      } DataSym;

      // Is described by Dwarf debug info.  Arbitrary strings.  Must
      // be the same length.
      struct {
         Char descr1[96];
         Char descr2[96];
      } Variable;

      // Could only narrow it down to be the PLT/GOT/etc of a given
      // object.  Better than nothing, perhaps.
      struct {
         Char       objname[128];
         VgSectKind kind;
      } SectKind;

      // Classification yielded nothing useful.
      struct { } Unknown;

   } Addr;
};

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
         Addr src;   // Source block
         Addr dst;   // Destination block
         Int  szB;   // Size in bytes;  0 if unused.
      } Overlap;

      // A memory leak.
      struct {
         UInt        n_this_record;
         UInt        n_total_records;
         LossRecord* lossRecord;
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

static void mc_pp_AddrInfo ( Addr a, AddrInfo* ai, Bool maybe_gcc )
{
   HChar* xpre  = VG_(clo_xml) ? "  <auxwhat>" : " ";
   HChar* xpost = VG_(clo_xml) ? "</auxwhat>"  : "";

   switch (ai->tag) {
      case Addr_Unknown:
         if (maybe_gcc) {
            VG_(message)(Vg_UserMsg, 
               "%sAddress 0x%llx is just below the stack ptr.  "
               "To suppress, use: --workaround-gcc296-bugs=yes%s",
               xpre, (ULong)a, xpost
            );
	 } else {
            VG_(message)(Vg_UserMsg, 
               "%sAddress 0x%llx "
               "is not stack'd, malloc'd or (recently) free'd%s",
               xpre, (ULong)a, xpost);
         }
         break;

      case Addr_Stack: 
         VG_(message)(Vg_UserMsg, 
                      "%sAddress 0x%llx is on thread %d's stack%s", 
                      xpre, (ULong)a, ai->Addr.Stack.tid, xpost);
         break;

      case Addr_Block: {
         SizeT block_szB  = ai->Addr.Block.block_szB;
         OffT  rwoffset   = ai->Addr.Block.rwoffset;
         SizeT delta;
         const Char* relative;

         if (rwoffset < 0) {
            delta    = (SizeT)(-rwoffset);
            relative = "before";
         } else if (rwoffset >= block_szB) {
            delta    = rwoffset - block_szB;
            relative = "after";
         } else {
            delta    = rwoffset;
            relative = "inside";
         }
         VG_(message)(Vg_UserMsg, 
            "%sAddress 0x%lx is %'lu bytes %s a %s of size %'lu %s%s",
            xpre,
            a, delta, relative, ai->Addr.Block.block_desc,
            block_szB,
            ai->Addr.Block.block_kind==Block_Mallocd ? "alloc'd" 
            : ai->Addr.Block.block_kind==Block_Freed ? "free'd" 
                                                     : "client-defined",
            xpost);
         VG_(pp_ExeContext)(ai->Addr.Block.lastchange);
         break;
      }

      case Addr_DataSym:
         VG_(message_no_f_c)(Vg_UserMsg,
                             "%sAddress 0x%llx is %llu bytes "
                             "inside data symbol \"%t\"%s",
                             xpre,
                             (ULong)a,
                             (ULong)ai->Addr.DataSym.offset,
                             ai->Addr.DataSym.name,
                             xpost);
         break;

      case Addr_Variable:
         if (ai->Addr.Variable.descr1[0] != '\0')
            VG_(message)(Vg_UserMsg, "%s%s%s",
                         xpre, ai->Addr.Variable.descr1, xpost);
         if (ai->Addr.Variable.descr2[0] != '\0')
            VG_(message)(Vg_UserMsg, "%s%s%s",
                         xpre, ai->Addr.Variable.descr2, xpost);
         break;

      case Addr_SectKind:
         VG_(message_no_f_c)(Vg_UserMsg,
                             "%sAddress 0x%llx is in the %t segment of %t%s",
                             xpre,
                             (ULong)a,
                             VG_(pp_SectKind)(ai->Addr.SectKind.kind),
                             ai->Addr.SectKind.objname,
                             xpost);
         break;

      default:
         VG_(tool_panic)("mc_pp_AddrInfo");
   }
}

static const HChar* str_leak_lossmode ( Reachedness lossmode )
{
   const HChar *loss = "?";
   switch (lossmode) {
      case Unreached:    loss = "definitely lost"; break;
      case IndirectLeak: loss = "indirectly lost"; break;
      case Interior:     loss = "possibly lost"; break;
      case Proper:       loss = "still reachable"; break;
   }
   return loss;
}

static const HChar* xml_leak_kind ( Reachedness lossmode )
{
   const HChar *loss = "?";
   switch (lossmode) {
      case Unreached:    loss = "Leak_DefinitelyLost"; break;
      case IndirectLeak: loss = "Leak_IndirectlyLost"; break;
      case Interior:     loss = "Leak_PossiblyLost"; break;
      case Proper:       loss = "Leak_StillReachable"; break;
   }
   return loss;
}

static void mc_pp_msg( Char* xml_name, Error* err, const HChar* format, ... )
{
   HChar* xpre  = VG_(clo_xml) ? "  <what>" : "";
   HChar* xpost = VG_(clo_xml) ? "</what>"  : "";
   Char buf[256];
   va_list vargs;

   if (VG_(clo_xml))
      VG_(message)(Vg_UserMsg, "  <kind>%s</kind>", xml_name);
   // Stick xpre and xpost on the front and back of the format string.
   VG_(snprintf)(buf, 256, "%s%s%s", xpre, format, xpost);
   va_start(vargs, format);
   VG_(vmessage) ( Vg_UserMsg, buf, vargs );
   va_end(vargs);
   VG_(pp_ExeContext)( VG_(get_error_where)(err) );
}

static void mc_pp_origin ( ExeContext* ec, UInt okind )
{
   HChar* src   = NULL;
   HChar* xpre  = VG_(clo_xml) ? "  <what>" : " ";
   HChar* xpost = VG_(clo_xml) ? "</what>"  : "";
   tl_assert(ec);

   switch (okind) {
      case MC_OKIND_STACK:   src = " by a stack allocation"; break;
      case MC_OKIND_HEAP:    src = " by a heap allocation"; break;
      case MC_OKIND_USER:    src = " by a client request"; break;
      case MC_OKIND_UNKNOWN: src = ""; break;
   }
   tl_assert(src); /* guards against invalid 'okind' */

   if (VG_(clo_xml)) {
      VG_(message)(Vg_UserMsg, "  <origin>");
   }

   VG_(message)(Vg_UserMsg, "%sUninitialised value was created%s%s",
                            xpre, src, xpost);
   VG_(pp_ExeContext)( ec );
   if (VG_(clo_xml)) {
      VG_(message)(Vg_UserMsg, "  </origin>");
   }
}

void MC_(pp_Error) ( Error* err )
{
   MC_Error* extra = VG_(get_error_extra)(err);

   switch (VG_(get_error_kind)(err)) {
      case Err_CoreMem: {
         /* What the hell *is* a CoreMemError? jrs 2005-May-18 */
         /* As of 2006-Dec-14, it's caused by unaddressable bytes in a
            signal handler frame.  --njn */
         mc_pp_msg("CoreMemError", err,
                   "%s contains unaddressable byte(s)", 
                   VG_(get_error_string)(err));
         break;
      } 
      
      case Err_Value:
         MC_(any_value_errors) = True;
         if (1 || extra->Err.Value.otag == 0) {
            mc_pp_msg("UninitValue", err,
                      "Use of uninitialised value of size %d",
                      extra->Err.Value.szB);
         } else {
            mc_pp_msg("UninitValue", err,
                      "Use of uninitialised value of size %d (otag %u)",
                      extra->Err.Value.szB, extra->Err.Value.otag);
         }
         if (extra->Err.Value.origin_ec)
            mc_pp_origin( extra->Err.Value.origin_ec,
                          extra->Err.Value.otag & 3 );
         break;

      case Err_Cond:
         MC_(any_value_errors) = True;
         if (1 || extra->Err.Cond.otag == 0) {
            mc_pp_msg("UninitCondition", err,
                      "Conditional jump or move depends"
                      " on uninitialised value(s)");
         } else {
            mc_pp_msg("UninitCondition", err,
                      "Conditional jump or move depends"
                      " on uninitialised value(s) (otag %u)",
                      extra->Err.Cond.otag);
         }
         if (extra->Err.Cond.origin_ec)
            mc_pp_origin( extra->Err.Cond.origin_ec,
                          extra->Err.Cond.otag & 3 );
         break;

      case Err_RegParam:
         MC_(any_value_errors) = True;
         mc_pp_msg("SyscallParam", err,
                   "Syscall param %s contains uninitialised byte(s)",
                   VG_(get_error_string)(err));
         if (extra->Err.RegParam.origin_ec)
            mc_pp_origin( extra->Err.RegParam.origin_ec,
                          extra->Err.RegParam.otag & 3 );
         break;

      case Err_MemParam:
         if (!extra->Err.MemParam.isAddrErr)
            MC_(any_value_errors) = True;
         mc_pp_msg("SyscallParam", err,
                   "Syscall param %s points to %s byte(s)",
                   VG_(get_error_string)(err),
                   ( extra->Err.MemParam.isAddrErr 
                     ? "unaddressable" : "uninitialised" ));
         mc_pp_AddrInfo(VG_(get_error_address)(err),
                        &extra->Err.MemParam.ai, False);
         if (extra->Err.MemParam.origin_ec && !extra->Err.MemParam.isAddrErr)
            mc_pp_origin( extra->Err.MemParam.origin_ec,
                          extra->Err.MemParam.otag & 3 );
         break;

      case Err_User:
         if (!extra->Err.User.isAddrErr)
            MC_(any_value_errors) = True;
         mc_pp_msg("ClientCheck", err,
                   "%s byte(s) found during client check request", 
                   ( extra->Err.User.isAddrErr
                     ? "Unaddressable" : "Uninitialised" ));
         mc_pp_AddrInfo(VG_(get_error_address)(err), &extra->Err.User.ai,
                        False);
         if (extra->Err.User.origin_ec && !extra->Err.User.isAddrErr)
            mc_pp_origin( extra->Err.User.origin_ec,
                          extra->Err.User.otag & 3 );
         break;

      case Err_Free:
         mc_pp_msg("InvalidFree", err,
                   "Invalid free() / delete / delete[]");
         mc_pp_AddrInfo(VG_(get_error_address)(err),
                        &extra->Err.Free.ai, False);
         break;

      case Err_FreeMismatch:
         mc_pp_msg("MismatchedFree", err,
                   "Mismatched free() / delete / delete []");
         mc_pp_AddrInfo(VG_(get_error_address)(err),
                        &extra->Err.FreeMismatch.ai, False);
         break;

      case Err_Addr:
         if (extra->Err.Addr.isWrite) {
            mc_pp_msg("InvalidWrite", err,
                      "Invalid write of size %d", 
                      extra->Err.Addr.szB); 
         } else {
            mc_pp_msg("InvalidRead", err,
                      "Invalid read of size %d", 
                      extra->Err.Addr.szB); 
         }
         mc_pp_AddrInfo(VG_(get_error_address)(err), &extra->Err.Addr.ai,
                        extra->Err.Addr.maybe_gcc);
         break;

      case Err_Jump:
         mc_pp_msg("InvalidJump", err,
                   "Jump to the invalid address stated on the next line");
         mc_pp_AddrInfo(VG_(get_error_address)(err), &extra->Err.Jump.ai,
                        False);
         break;

      case Err_Overlap:
         if (extra->Err.Overlap.szB == 0)
            mc_pp_msg("Overlap", err,
                      "Source and destination overlap in %s(%p, %p)",
                      VG_(get_error_string)(err),
                      extra->Err.Overlap.dst, extra->Err.Overlap.src);
         else
            mc_pp_msg("Overlap", err,
                      "Source and destination overlap in %s(%p, %p, %d)",
                      VG_(get_error_string)(err),
                      extra->Err.Overlap.dst, extra->Err.Overlap.src,
                      extra->Err.Overlap.szB);
         break;

      case Err_IllegalMempool:
         mc_pp_msg("InvalidMemPool", err,
                   "Illegal memory pool address");
         mc_pp_AddrInfo(VG_(get_error_address)(err),
                        &extra->Err.IllegalMempool.ai, False);
         break;

      case Err_Leak: {
         HChar*      xpre  = VG_(clo_xml) ? "  <what>" : "";
         HChar*      xpost = VG_(clo_xml) ? "</what>"  : "";
         UInt        n_this_record   = extra->Err.Leak.n_this_record;
         UInt        n_total_records = extra->Err.Leak.n_total_records;
         LossRecord* l               = extra->Err.Leak.lossRecord;

         if (VG_(clo_xml)) {
            VG_(message_no_f_c)(Vg_UserMsg, "  <kind>%t</kind>",
                                xml_leak_kind(l->loss_mode));
         } else {
            VG_(message)(Vg_UserMsg, "");
         }

         if (l->indirect_bytes) {
            VG_(message)(Vg_UserMsg, 
               "%s%'lu (%'lu direct, %'lu indirect) bytes in %'u blocks"
               " are %s in loss record %'u of %'u%s",
               xpre,
               l->total_bytes + l->indirect_bytes, 
               l->total_bytes, l->indirect_bytes, l->num_blocks,
               str_leak_lossmode(l->loss_mode), n_this_record, n_total_records,
               xpost
            );
            if (VG_(clo_xml)) {
               // Nb: don't put commas in these XML numbers 
               VG_(message)(Vg_UserMsg, "  <leakedbytes>%lu</leakedbytes>", 
                                        l->total_bytes + l->indirect_bytes);
               VG_(message)(Vg_UserMsg, "  <leakedblocks>%u</leakedblocks>", 
                                        l->num_blocks);
            }
         } else {
            VG_(message)(
               Vg_UserMsg, 
               "%s%'lu bytes in %'u blocks are %s in loss record %'u of %'u%s",
               xpre,
               l->total_bytes, l->num_blocks,
               str_leak_lossmode(l->loss_mode), n_this_record, n_total_records,
               xpost
            );
            if (VG_(clo_xml)) {
               VG_(message)(Vg_UserMsg, "  <leakedbytes>%ld</leakedbytes>",
                                        l->total_bytes);
               VG_(message)(Vg_UserMsg, "  <leakedblocks>%d</leakedblocks>", 
                                        l->num_blocks);
            }
         }
         VG_(pp_ExeContext)(l->allocated_at);
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

#  if defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5)
   /* AIX zero-page handling.  On AIX, reads from page zero are,
      bizarrely enough, legitimate.  Writes to page zero aren't,
      though.  Since memcheck can't distinguish reads from writes, the
      best we can do is to 'act normal' and mark the A bits in the
      normal way as noaccess, but then hide any reads from that page
      that get reported here. */
   if ((!isWrite) && a >= 0 && a < 4096 && a+szB <= 4096) 
      return;

   /* Appalling AIX hack.  It suppresses reads done by glink
      fragments.  Getting rid of this would require figuring out
      somehow where the referenced data areas are (and their
      sizes). */
   if ((!isWrite) && szB == sizeof(Word)) { 
      UInt i1, i2;
      UInt* pc = (UInt*)VG_(get_IP)(tid);
      if (sizeof(Word) == 4) {
         i1 = 0x800c0000; /* lwz r0,0(r12) */
         i2 = 0x804c0004; /* lwz r2,4(r12) */
      } else {
         i1 = 0xe80c0000; /* ld  r0,0(r12) */
         i2 = 0xe84c0008; /* ld  r2,8(r12) */
      }
      if (pc[0] == i1 && pc[1] == i2) return;
      if (pc[0] == i2 && pc[-1] == i1) return;
   }
#  endif

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

/* This is for memory errors in pthread functions, as opposed to pthread API
   errors which are found by the core. */
void MC_(record_core_mem_error) ( ThreadId tid, Bool isAddrErr, Char* msg )
{
   VG_(maybe_record_error)( tid, Err_CoreMem, /*addr*/0, msg, /*extra*/NULL );
}

void MC_(record_regparam_error) ( ThreadId tid, Char* msg, UInt otag )
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
                                  Bool isAddrErr, Char* msg, UInt otag )
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
   ai->Addr.Block.lastchange = mc->where;
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

void MC_(record_overlap_error) ( ThreadId tid, Char* function,
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
                              UInt n_total_records, LossRecord* lossRecord,
                              Bool print_record )
{
   MC_Error extra;
   extra.Err.Leak.n_this_record   = n_this_record;
   extra.Err.Leak.n_total_records = n_total_records;
   extra.Err.Leak.lossRecord      = lossRecord;
   return
   VG_(unique_error) ( tid, Err_Leak, /*Addr*/0, /*s*/NULL, &extra,
                       lossRecord->allocated_at, print_record,
                       /*allow_GDB_attach*/False, /*count_error*/False );
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
         Char *e1s, *e2s;
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

/* Function used when searching MC_Chunk lists */
static Bool addr_is_in_MC_Chunk(MC_Chunk* mc, Addr a)
{
   // Nb: this is not quite right!  It assumes that the heap block has
   // a redzone of size MC_MALLOC_REDZONE_SZB.  That's true for malloc'd
   // blocks, but not necessarily true for custom-alloc'd blocks.  So
   // in some cases this could result in an incorrect description (eg.
   // saying "12 bytes after block A" when really it's within block B.
   // Fixing would require adding redzone size to MC_Chunks, though.
   return VG_(addr_is_in_block)( a, mc->data, mc->szB,
                                 MC_MALLOC_REDZONE_SZB );
}

// Forward declaration
static Bool client_block_maybe_describe( Addr a, AddrInfo* ai );


/* Describe an address as best you can, for error messages,
   putting the result in ai. */
static void describe_addr ( Addr a, /*OUT*/AddrInfo* ai )
{
   MC_Chunk*  mc;
   ThreadId   tid;
   Addr       stack_min, stack_max;
   VgSectKind sect;

   tl_assert(Addr_Undescribed == ai->tag);

   /* Perhaps it's a user-def'd block? */
   if (client_block_maybe_describe( a, ai )) {
      return;
   }
   /* Search for a recently freed block which might bracket it. */
   mc = MC_(get_freed_list_head)();
   while (mc) {
      if (addr_is_in_MC_Chunk(mc, a)) {
         ai->tag = Addr_Block;
         ai->Addr.Block.block_kind = Block_Freed;
         ai->Addr.Block.block_desc = "block";
         ai->Addr.Block.block_szB  = mc->szB;
         ai->Addr.Block.rwoffset   = (Word)a - (Word)mc->data;
         ai->Addr.Block.lastchange = mc->where;
         return;
      }
      mc = mc->next; 
   }
   /* Search for a currently malloc'd block which might bracket it. */
   VG_(HT_ResetIter)(MC_(malloc_list));
   while ( (mc = VG_(HT_Next)(MC_(malloc_list))) ) {
      if (addr_is_in_MC_Chunk(mc, a)) {
         ai->tag = Addr_Block;
         ai->Addr.Block.block_kind = Block_Mallocd;
         ai->Addr.Block.block_desc = "block";
         ai->Addr.Block.block_szB  = mc->szB;
         ai->Addr.Block.rwoffset   = (Word)a - (Word)mc->data;
         ai->Addr.Block.lastchange = mc->where;
         return;
      }
   }
   /* Perhaps the variable type/location data describes it? */
   tl_assert(sizeof(ai->Addr.Variable.descr1) 
             == sizeof(ai->Addr.Variable.descr2));
   VG_(memset)( &ai->Addr.Variable.descr1, 
                0, sizeof(ai->Addr.Variable.descr1));
   VG_(memset)( &ai->Addr.Variable.descr2, 
                0, sizeof(ai->Addr.Variable.descr2));
   if (VG_(get_data_description)(
             &ai->Addr.Variable.descr1[0],
             &ai->Addr.Variable.descr2[0],
             sizeof(ai->Addr.Variable.descr1)-1, 
             a )) {
      ai->tag = Addr_Variable;
      tl_assert( ai->Addr.Variable.descr1
                    [ sizeof(ai->Addr.Variable.descr1)-1 ] == 0);
      tl_assert( ai->Addr.Variable.descr2
                    [ sizeof(ai->Addr.Variable.descr2)-1 ] == 0);
      return;
   }
   /* Have a look at the low level data symbols - perhaps it's in
      there. */
   VG_(memset)( &ai->Addr.DataSym.name,
                0, sizeof(ai->Addr.DataSym.name));
   if (VG_(get_datasym_and_offset)(
             a, &ai->Addr.DataSym.name[0],
             sizeof(ai->Addr.DataSym.name)-1,
             &ai->Addr.DataSym.offset )) {
      ai->tag = Addr_DataSym;
      tl_assert( ai->Addr.DataSym.name
                    [ sizeof(ai->Addr.DataSym.name)-1 ] == 0);
      return;
   }
   /* Perhaps it's on a thread's stack? */
   VG_(thread_stack_reset_iter)(&tid);
   while ( VG_(thread_stack_next)(&tid, &stack_min, &stack_max) ) {
      if (stack_min - VG_STACK_REDZONE_SZB <= a && a <= stack_max) {
         ai->tag            = Addr_Stack;
         ai->Addr.Stack.tid = tid;
         return;
      }
   }
   /* last ditch attempt at classification */
   tl_assert( sizeof(ai->Addr.SectKind.objname) > 4 );
   VG_(memset)( &ai->Addr.SectKind.objname, 
                0, sizeof(ai->Addr.SectKind.objname));
   VG_(strcpy)( ai->Addr.SectKind.objname, "???" );
   sect = VG_(seginfo_sect_kind)( &ai->Addr.SectKind.objname[0],
                                  sizeof(ai->Addr.SectKind.objname)-1, a);
   if (sect != Vg_SectUnknown) {
      ai->tag = Addr_SectKind;
      ai->Addr.SectKind.kind = sect;
      tl_assert( ai->Addr.SectKind.objname
                    [ sizeof(ai->Addr.SectKind.objname)-1 ] == 0);
      return;
   }
   /* Clueless ... */
   ai->tag = Addr_Unknown;
   return;
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

// FIXME: does this perhaps want to live somewhere else
// in this file?
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
         /* OK - maybe it's a mempool, too? */
         MC_Mempool* mp = VG_(HT_lookup)(MC_(mempool_list),
                                          (UWord)cgbs[i].start);
         if (mp != NULL) {
            if (mp->chunks != NULL) {
               MC_Chunk* mc;
               VG_(HT_ResetIter)(mp->chunks);
               while ( (mc = VG_(HT_Next)(mp->chunks)) ) {
                  if (addr_is_in_MC_Chunk(mc, a)) {
                     ai->tag = Addr_Block;
                     ai->Addr.Block.block_kind = Block_MempoolChunk;
                     ai->Addr.Block.block_desc = "block";
                     ai->Addr.Block.block_szB  = mc->szB;
                     ai->Addr.Block.rwoffset   = (Word)a - (Word)mc->data;
                     ai->Addr.Block.lastchange = mc->where;
                     return True;
                  }
               }
            }
            ai->tag = Addr_Block;
            ai->Addr.Block.block_kind = Block_Mempool;
            ai->Addr.Block.block_desc = "mempool";
            ai->Addr.Block.block_szB  = cgbs[i].size;
            ai->Addr.Block.rwoffset   = (Word)(a) - (Word)(cgbs[i].start);
            ai->Addr.Block.lastchange = cgbs[i].where;
            return True;
         }
         ai->tag = Addr_Block;
         ai->Addr.Block.block_kind = Block_UserG;
         ai->Addr.Block.block_desc = cgbs[i].desc;
         ai->Addr.Block.block_szB  = cgbs[i].size;
         ai->Addr.Block.rwoffset   = (Word)(a) - (Word)(cgbs[i].start);
         ai->Addr.Block.lastchange = cgbs[i].where;
         return True;
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

Bool MC_(is_recognised_suppression) ( Char* name, Supp* su )
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

Bool MC_(read_extra_suppression_info) ( Int fd, Char* buf,
                                        Int nBuf, Supp *su )
{
   Bool eof;

   if (VG_(get_supp_kind)(su) == ParamSupp) {
      eof = VG_(get_line) ( fd, buf, nBuf );
      if (eof) return False;
      VG_(set_supp_string)(su, VG_(strdup)("mc.resi.1", buf));
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
         return (ekind == Err_Leak);

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

Char* MC_(get_error_name) ( Error* err )
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

void MC_(print_extra_suppression_info) ( Error* err )
{
   ErrorKind ekind = VG_(get_error_kind )(err);
   if (Err_RegParam == ekind || Err_MemParam == ekind) {
      VG_(printf)("   %s\n", VG_(get_error_string)(err));
   }
}


/*--------------------------------------------------------------------*/
/*--- end                                              mc_errors.c ---*/
/*--------------------------------------------------------------------*/
