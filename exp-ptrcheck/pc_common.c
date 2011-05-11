
/*--------------------------------------------------------------------*/
/*--- Ptrcheck: a pointer-use checker.                             ---*/
/*--- Provides stuff shared between sg_ and h_ subtools.           ---*/
/*---                                                  pc_common.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Ptrcheck, a Valgrind tool for checking pointer
   use in programs.

   Copyright (C) 2008-2010 OpenWorks Ltd
      info@open-works.co.uk

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

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

#include "pub_tool_basics.h"
#include "pub_tool_libcbase.h"
#include "pub_tool_libcprint.h"
#include "pub_tool_xarray.h"
#include "pub_tool_mallocfree.h"
#include "pub_tool_libcassert.h"
#include "pub_tool_options.h"
#include "pub_tool_replacemalloc.h"
#include "pub_tool_execontext.h"
#include "pub_tool_tooliface.h"    // CorePart
#include "pub_tool_threadstate.h"  // VG_(get_running_tid)
#include "pub_tool_debuginfo.h"

#include "pc_common.h"   // self, & Seg

#include "h_main.h"      // NONPTR, BOTTOM, UNKNOWN


//////////////////////////////////////////////////////////////
//                                                          //
// Command line options                                     //
//                                                          //
//////////////////////////////////////////////////////////////

Bool h_clo_partial_loads_ok  = True;   /* user visible */
/* Bool h_clo_lossage_check     = False; */ /* dev flag only */
Bool sg_clo_enable_sg_checks = True;   /* user visible */

Bool pc_process_cmd_line_options(Char* arg)
{
        if VG_BOOL_CLO(arg, "--partial-loads-ok", h_clo_partial_loads_ok) {}
   /* else if VG_BOOL_CLO(arg, "--lossage-check",    h_clo_lossage_check) {} */
   else if VG_BOOL_CLO(arg, "--enable-sg-checks", sg_clo_enable_sg_checks) {}
   else
      return VG_(replacement_malloc_process_cmd_line_option)(arg);

   return True;
}

void pc_print_usage(void)
{
   VG_(printf)(
   "    --partial-loads-ok=no|yes  same as for Memcheck [yes]\n"
   "    --enable-sg-checks=no|yes  enable stack & global array checking? [yes]\n"
   );
}

void pc_print_debug_usage(void)
{
   VG_(printf)(
"    (none)\n"
//"    --lossage-check=no|yes    gather stats for quality control [no]\n"
   );
}



//////////////////////////////////////////////////////////////
//                                                          //
// Error management -- storage                              //
//                                                          //
//////////////////////////////////////////////////////////////

/* What kind of error it is. */
typedef
   enum {
      XE_SorG=1202, // sg: stack or global array inconsistency
      XE_Heap,      // h: mismatched ptr/addr segments on load/store
      XE_Arith,     // h: bad arithmetic between two segment pointers
      XE_SysParam   // h: block straddling >1 segment passed to syscall
   }
   XErrorTag;

typedef
   enum {
      XS_SorG=2021,
      XS_Heap,
      XS_Arith,
      XS_SysParam
   }
   XSuppTag;

typedef
   struct {
      XErrorTag tag;
      union {
         struct {
            Addr   addr;
            SSizeT sszB;  /* -ve is write, +ve is read */
            HChar  expect[128];
            HChar  actual[128];
            HChar  delta[32]; // text showing relation to expected
         } SorG;
         struct {
            Addr     addr;
            SSizeT   sszB;  /* -ve is write, +ve is read */
            Seg*     vseg;
            XArray*  descr1; /* XArray* of HChar */
            XArray*  descr2; /* XArray* of HChar */
            Char     datasym[96];
            PtrdiffT datasymoff;
         } Heap;
         struct {
            Seg* seg1;
            Seg* seg2;
            const HChar* opname; // user-understandable text name
         } Arith;
         struct {
            CorePart part;
            Addr lo;
            Addr hi;
            Seg* seglo;
            Seg* seghi;
         } SysParam;
      } XE;
   }
   XError;


void sg_record_error_SorG ( ThreadId tid,
                            Addr addr, SSizeT sszB,
                            HChar* expect, HChar* actual, HChar* delta )
{
   XError xe;
   VG_(memset)(&xe, 0, sizeof(xe));
   xe.tag = XE_SorG;
   xe.XE.SorG.addr = addr;
   xe.XE.SorG.sszB = sszB;
   VG_(strncpy)( &xe.XE.SorG.expect[0],
                 expect, sizeof(xe.XE.SorG.expect) );
   VG_(strncpy)( &xe.XE.SorG.actual[0],
                 actual, sizeof(xe.XE.SorG.actual) );
   VG_(strncpy)( &xe.XE.SorG.delta[0],
                 delta, sizeof(xe.XE.SorG.delta) );
   xe.XE.SorG.expect[ sizeof(xe.XE.SorG.expect)-1 ] = 0;
   xe.XE.SorG.actual[ sizeof(xe.XE.SorG.actual)-1 ] = 0;
   xe.XE.SorG.delta[ sizeof(xe.XE.SorG.delta)-1 ] = 0;
   VG_(maybe_record_error)( tid, XE_SorG, 0, NULL, &xe );
}

void h_record_heap_error( Addr a, SizeT size, Seg* vseg, Bool is_write )
{
   XError xe;
   tl_assert(size > 0);
   VG_(memset)(&xe, 0, sizeof(xe));
   xe.tag = XE_Heap;
   xe.XE.Heap.addr = a;
   xe.XE.Heap.sszB = is_write ? -size : size;
   xe.XE.Heap.vseg = vseg;
   VG_(maybe_record_error)( VG_(get_running_tid)(), XE_Heap,
                            /*a*/0, /*str*/NULL, /*extra*/(void*)&xe);
}

void h_record_arith_error( Seg* seg1, Seg* seg2, HChar* opname )
{
   XError xe;
   VG_(memset)(&xe, 0, sizeof(xe));
   xe.tag = XE_Arith;
   xe.XE.Arith.seg1   = seg1;
   xe.XE.Arith.seg2   = seg2;
   xe.XE.Arith.opname = opname;
   VG_(maybe_record_error)( VG_(get_running_tid)(), XE_Arith,
                            /*a*/0, /*str*/NULL, /*extra*/(void*)&xe);
}

void h_record_sysparam_error( ThreadId tid, CorePart part, Char* s,
                              Addr lo, Addr hi, Seg* seglo, Seg* seghi )
{
   XError xe;
   VG_(memset)(&xe, 0, sizeof(xe));
   xe.tag = XE_SysParam;
   xe.XE.SysParam.part = part;
   xe.XE.SysParam.lo = lo;
   xe.XE.SysParam.hi = hi;
   xe.XE.SysParam.seglo = seglo;
   xe.XE.SysParam.seghi = seghi;
   VG_(maybe_record_error)( tid, XE_SysParam, /*a*/(Addr)0, /*str*/s,
                            /*extra*/(void*)&xe);
}


Bool pc_eq_Error ( VgRes res, Error* e1, Error* e2 )
{
   XError *xe1, *xe2;
   tl_assert(VG_(get_error_kind)(e1) == VG_(get_error_kind)(e2));
   //tl_assert(VG_(get_error_string)(e1) == NULL);
   //tl_assert(VG_(get_error_string)(e2) == NULL);

   xe1 = (XError*)VG_(get_error_extra)(e1);
   xe2 = (XError*)VG_(get_error_extra)(e2);
   tl_assert(xe1);
   tl_assert(xe2);

   if (xe1->tag != xe2->tag)
      return False;

   switch (xe1->tag) {
      case XE_SorG:
         return //xe1->XE.SorG.addr == xe2->XE.SorG.addr
                //&& 
                xe1->XE.SorG.sszB == xe2->XE.SorG.sszB
                && 0 == VG_(strncmp)( &xe1->XE.SorG.expect[0],
                                      &xe2->XE.SorG.expect[0],
                                      sizeof(xe1->XE.SorG.expect) ) 
                && 0 == VG_(strncmp)( &xe1->XE.SorG.actual[0],
                                      &xe2->XE.SorG.actual[0],
                                      sizeof(xe1->XE.SorG.actual) );
      case XE_Heap:
      case XE_Arith:
      case XE_SysParam:
         return True;
      default:
         VG_(tool_panic)("eq_Error: unrecognised error kind");
   }
}


//////////////////////////////////////////////////////////////
//                                                          //
// Error management -- printing                             //
//                                                          //
//////////////////////////////////////////////////////////////

/* This is the "this error is due to be printed shortly; so have a
   look at it any print any preamble you want" function.  Which, in
   Ptrcheck, we don't use.  Hence a no-op.
*/
void pc_before_pp_Error ( Error* err ) {
}

/* Do a printf-style operation on either the XML or normal output
   channel, depending on the setting of VG_(clo_xml).
*/
static void emit_WRK ( HChar* format, va_list vargs )
{
   if (VG_(clo_xml)) {
      VG_(vprintf_xml)(format, vargs);
   } else {
      VG_(vmessage)(Vg_UserMsg, format, vargs);
   }
}
static void emit ( HChar* format, ... ) PRINTF_CHECK(1, 2);
static void emit ( HChar* format, ... )
{
   va_list vargs;
   va_start(vargs, format);
   emit_WRK(format, vargs);
   va_end(vargs);
}
static void emiN ( HChar* format, ... ) /* With NO FORMAT CHECK */
{
   va_list vargs;
   va_start(vargs, format);
   emit_WRK(format, vargs);
   va_end(vargs);
}


static Char* readwrite(SSizeT sszB)
{
   return ( sszB < 0 ? "write" : "read" );
}

static Word Word__abs ( Word w ) {
   return w < 0 ? -w : w;
}

void pc_pp_Error ( Error* err )
{
   const Bool xml = VG_(clo_xml); /* a shorthand, that's all */

   XError *xe = (XError*)VG_(get_error_extra)(err);
   tl_assert(xe);

   switch (VG_(get_error_kind)(err)) {

   //----------------------------------------------------------
   case XE_SorG:

      if (xml) {

         emit( "  <kind>SorG</kind>\n");
         emit( "  <what>Invalid %s of size %ld</what>\n",
               xe->XE.SorG.sszB < 0 ? "write" : "read",
               Word__abs(xe->XE.SorG.sszB) );
         VG_(pp_ExeContext)( VG_(get_error_where)(err) );
   
         emit( "  <auxwhat>Address %#lx expected vs actual:</auxwhat>\n",
               xe->XE.SorG.addr );
         emiN( "  <auxwhat>Expected: %t</auxwhat>\n",
               &xe->XE.SorG.expect[0] );
         emiN( "  <auxwhat>Actual:   %t</auxwhat>\n", 
               &xe->XE.SorG.actual[0] );

      } else {

         emit( "Invalid %s of size %ld\n", 
               xe->XE.SorG.sszB < 0 ? "write" : "read",
               Word__abs(xe->XE.SorG.sszB) );
         VG_(pp_ExeContext)( VG_(get_error_where)(err) );
   
         emit( " Address %#lx expected vs actual:\n", xe->XE.SorG.addr );
         emit( " Expected: %s\n", &xe->XE.SorG.expect[0] );
         emit( " Actual:   %s\n", &xe->XE.SorG.actual[0] );
         if (xe->XE.SorG.delta[0] != 0)
            emit(" Actual:   is %s Expected\n", &xe->XE.SorG.delta[0]);
      }
      break;

   //----------------------------------------------------------
   case XE_Heap: {
      Char *place, *legit, *how_invalid;
      Addr a    = xe->XE.Heap.addr;
      Seg* vseg = xe->XE.Heap.vseg;

      tl_assert(is_known_segment(vseg) || NONPTR == vseg);

      if (NONPTR == vseg) {
         // Access via a non-pointer

         if (xml) {

            emit( "  <kind>Heap</kind>\n");
            emit( "  <what>Invalid %s of size %ld</what>\n",
                  readwrite(xe->XE.Heap.sszB),
                  Word__abs(xe->XE.Heap.sszB) );
            VG_(pp_ExeContext)( VG_(get_error_where)(err) );
   
            emit( "  <auxwhat>Address %#lx is not derived from "
                  "any known block</auxwhat>\n", a );

         } else {

            emit( "Invalid %s of size %ld\n",
                  readwrite(xe->XE.Heap.sszB),
                  Word__abs(xe->XE.Heap.sszB) );
            VG_(pp_ExeContext)( VG_(get_error_where)(err) );
   
            emit( " Address %#lx is not derived from "
                  "any known block\n", a );

         }

      } else {
         // Access via a pointer, but outside its range.
         Int cmp;
         UWord miss_size;
         Seg__cmp(vseg, a, &cmp, &miss_size);
         if      (cmp  < 0) place = "before";
         else if (cmp == 0) place = "inside";
         else               place = "after";
         how_invalid = ( ( Seg__is_freed(vseg) && 0 != cmp )
                       ? "Doubly-invalid" : "Invalid" );
         legit = ( Seg__is_freed(vseg) ? "once-" : "" );

         if (xml) {

            emit( "  <kind>Heap</kind>\n");
            emit( "  <what>%s %s of size %ld</what>\n",
                  how_invalid,
                  readwrite(xe->XE.Heap.sszB),
                  Word__abs(xe->XE.Heap.sszB) );
            VG_(pp_ExeContext)( VG_(get_error_where)(err) );
   
            emit( "  <auxwhat>Address %#lx is %lu bytes %s "
                     "the accessing pointer's</auxwhat>\n",
                  a, miss_size, place );
            emit( "  <auxwhat>%slegitimate range, "
                     "a block of size %lu %s</auxwhat>\n",
                  legit, Seg__size(vseg),
                  Seg__is_freed(vseg) ? "free'd" : "alloc'd" );
            VG_(pp_ExeContext)(Seg__where(vseg));

         } else {

            emit( "%s %s of size %ld\n",
                  how_invalid,
                  readwrite(xe->XE.Heap.sszB),
                  Word__abs(xe->XE.Heap.sszB) );
            VG_(pp_ExeContext)( VG_(get_error_where)(err) );
   
            emit( " Address %#lx is %lu bytes %s the accessing pointer's\n",
                  a, miss_size, place );
            emit( " %slegitimate range, a block of size %lu %s\n",
                  legit, Seg__size(vseg),
                  Seg__is_freed(vseg) ? "free'd" : "alloc'd" );
            VG_(pp_ExeContext)(Seg__where(vseg));

         }
      }

      /* If we have a better description of the address, show it.
         Note that in XML mode, it will already by nicely wrapped up
         in tags, either <auxwhat> or <xauxwhat>, so we can just emit
         it verbatim. */
      if (xml) {

         if (xe->XE.Heap.descr1)
            emiN( "  %t\n",
                  (HChar*)VG_(indexXA)( xe->XE.Heap.descr1, 0 ) );
         if (xe->XE.Heap.descr2)
            emiN( "  %t\n",
                  (HChar*)VG_(indexXA)( xe->XE.Heap.descr2, 0 ) );
         if (xe->XE.Heap.datasym[0] != 0)
            emiN( "  <auxwhat>Address 0x%llx is %llu bytes "
                  "inside data symbol \"%t\"</auxwhat>\n",
                  (ULong)xe->XE.Heap.addr,
                  (ULong)xe->XE.Heap.datasymoff,
                  xe->XE.Heap.datasym );

      } else {

         if (xe->XE.Heap.descr1)
            emit( " %s\n",
                  (HChar*)VG_(indexXA)( xe->XE.Heap.descr1, 0 ) );
         if (xe->XE.Heap.descr2)
            emit( " %s\n",
                  (HChar*)VG_(indexXA)( xe->XE.Heap.descr2, 0 ) ); 
         if (xe->XE.Heap.datasym[0] != 0)
            emit( " Address 0x%llx is %llu bytes "
                  "inside data symbol \"%s\"\n",
                  (ULong)xe->XE.Heap.addr,
                  (ULong)xe->XE.Heap.datasymoff,
                  xe->XE.Heap.datasym );

      }
      break;
   }

   //----------------------------------------------------------
   case XE_Arith: {
      Seg*   seg1   = xe->XE.Arith.seg1;
      Seg*   seg2   = xe->XE.Arith.seg2;
      Char*  which;

      tl_assert(BOTTOM != seg1);
      tl_assert(BOTTOM != seg2 && UNKNOWN != seg2);

      if (xml) {

         emit( "  <kind>Arith</kind>\n");
         emit( "  <what>Invalid arguments to %s</what>\n",
               xe->XE.Arith.opname );
         VG_(pp_ExeContext)( VG_(get_error_where)(err) );
   
         if (seg1 != seg2) {
            if (NONPTR == seg1) {
               emit( "  <auxwhat>First arg not a pointer</auxwhat>\n" );
            } else if (UNKNOWN == seg1) {
               emit( "  <auxwhat>First arg may be a pointer</auxwhat>\n" );
            } else {
               emit( "  <auxwhat>First arg derived from address %#lx of "
                     "%lu-byte block alloc'd</auxwhat>\n",
                     Seg__addr(seg1), Seg__size(seg1) );
               VG_(pp_ExeContext)(Seg__where(seg1));
            }
            which = "Second arg";
         } else {
            which = "Both args";
         }
         if (NONPTR == seg2) {
            emit( "  <auxwhat>%s not a pointer</auxwhat>\n", which );
         } else {
            emit( "  <auxwhat>%s derived from address %#lx of "
                  "%lu-byte block alloc'd</auxwhat>\n",
                  which, Seg__addr(seg2), Seg__size(seg2) );
            VG_(pp_ExeContext)(Seg__where(seg2));
         }

      } else {

         emit( "Invalid arguments to %s\n",
               xe->XE.Arith.opname );
         VG_(pp_ExeContext)( VG_(get_error_where)(err) );
   
         if (seg1 != seg2) {
            if (NONPTR == seg1) {
               emit( " First arg not a pointer\n" );
            } else if (UNKNOWN == seg1) {
               emit( " First arg may be a pointer\n" );
            } else {
               emit( " First arg derived from address %#lx of "
                     "%lu-byte block alloc'd\n",
                     Seg__addr(seg1), Seg__size(seg1) );
               VG_(pp_ExeContext)(Seg__where(seg1));
            }
            which = "Second arg";
         } else {
            which = "Both args";
         }
         if (NONPTR == seg2) {
            emit( " %s not a pointer\n", which );
         } else {
            emit( " %s derived from address %#lx of "
                  "%lu-byte block alloc'd\n",
                  which, Seg__addr(seg2), Seg__size(seg2) );
            VG_(pp_ExeContext)(Seg__where(seg2));
         }

      }

      break;
   }

   //----------------------------------------------------------
   case XE_SysParam: {
      Addr  lo    = xe->XE.SysParam.lo;
      Addr  hi    = xe->XE.SysParam.hi;
      Seg*  seglo = xe->XE.SysParam.seglo;
      Seg*  seghi = xe->XE.SysParam.seghi;
      Char* s     = VG_(get_error_string) (err);
      Char* what;

      tl_assert(BOTTOM != seglo && BOTTOM != seghi);

      if      (Vg_CoreSysCall == xe->XE.SysParam.part) 
                 what = "Syscall param ";
      else    VG_(tool_panic)("bad CorePart");

      if (seglo == seghi) {
         // freed block
         tl_assert(is_known_segment(seglo));
         tl_assert(Seg__is_freed(seglo)); // XXX what if it's now recycled?

         if (xml) {

            emit( "  <kind>SysParam</kind>\n");
            emit( "  <what>%s%s contains unaddressable byte(s)</what>\n",
                  what, s );
            VG_(pp_ExeContext)( VG_(get_error_where)(err) );
   
            emit( "  <auxwhat>Address %#lx is %ld bytes inside a "
                  "%ld-byte block free'd</auxwhat>\n",
                  lo, lo-Seg__addr(seglo), Seg__size(seglo) );
            VG_(pp_ExeContext)(Seg__where(seglo));

         } else {

            emit( " %s%s contains unaddressable byte(s)\n",
                  what, s );
            VG_(pp_ExeContext)( VG_(get_error_where)(err) );
   
            emit( " Address %#lx is %ld bytes inside a "
                  "%ld-byte block free'd\n",
                  lo, lo-Seg__addr(seglo), Seg__size(seglo) );
            VG_(pp_ExeContext)(Seg__where(seglo));

         }

      } else {
         // mismatch

         if (xml) {

            emit( "  <kind>SysParam</kind>\n");
            emit( "  <what>%s%s is non-contiguous</what>\n",
                  what, s );
            VG_(pp_ExeContext)( VG_(get_error_where)(err) );
   
            if (UNKNOWN == seglo) {
               emit( "  <auxwhat>First byte is "
                        "not inside a known block</auxwhat>\n" );
            } else {
               emit( "  <auxwhat>First byte (%#lx) is %ld bytes inside a "
                     "%ld-byte block alloc'd</auxwhat>\n",
                     lo, lo-Seg__addr(seglo), Seg__size(seglo) );
               VG_(pp_ExeContext)(Seg__where(seglo));
            }
   
            if (UNKNOWN == seghi) {
               emit( "  <auxwhat>Last byte is "
                        "not inside a known block</auxwhat>\n" );
            } else {
               emit( "  <auxwhat>Last byte (%#lx) is %ld bytes inside a "
                     "%ld-byte block alloc'd</auxwhat>\n",
                     hi, hi-Seg__addr(seghi), Seg__size(seghi) );
               VG_(pp_ExeContext)(Seg__where(seghi));
            }

         } else {

            emit( "%s%s is non-contiguous\n",
                  what, s );
            VG_(pp_ExeContext)( VG_(get_error_where)(err) );
   
            if (UNKNOWN == seglo) {
               emit( " First byte is not inside a known block\n" );
            } else {
               emit( " First byte (%#lx) is %ld bytes inside a "
                     "%ld-byte block alloc'd\n",
                     lo, lo-Seg__addr(seglo), Seg__size(seglo) );
               VG_(pp_ExeContext)(Seg__where(seglo));
            }
   
            if (UNKNOWN == seghi) {
               emit( " Last byte is not inside a known block\n" );
            } else {
               emit( " Last byte (%#lx) is %ld bytes inside a "
                     "%ld-byte block alloc'd\n",
                     hi, hi-Seg__addr(seghi), Seg__size(seghi) );
               VG_(pp_ExeContext)(Seg__where(seghi));
            }

         }

      }
      break;
   }

   default:
      VG_(tool_panic)("pp_Error: unrecognised error kind");
   }
}


UInt pc_update_Error_extra ( Error* err )
{
   XError *xe = (XError*)VG_(get_error_extra)(err);
   tl_assert(xe);
   switch (xe->tag) {
      case XE_SorG:
         break;
      case XE_Heap: {
         Bool have_descr;

         tl_assert(sizeof(xe->XE.Heap.datasym) > 0);
         xe->XE.Heap.datasymoff = 0;
         xe->XE.Heap.datasym[0] = 0;

         tl_assert(!xe->XE.Heap.descr1);
         tl_assert(!xe->XE.Heap.descr2);

         xe->XE.Heap.descr1
            = VG_(newXA)( VG_(malloc), "pc.update_extra.Heap.descr1",
                          VG_(free), sizeof(HChar) );
         xe->XE.Heap.descr2
            = VG_(newXA)( VG_(malloc), "pc.update_extra.Heap.descr1",
                          VG_(free), sizeof(HChar) );

         VG_(memset)(&xe->XE.Heap.datasym, 0, sizeof(xe->XE.Heap.datasym));
         xe->XE.Heap.datasymoff = 0;

         have_descr
            = VG_(get_data_description)( xe->XE.Heap.descr1,
                                         xe->XE.Heap.descr2,
                                         xe->XE.Heap.addr );

         /* If there's nothing in descr1/2, free it.  Why is it safe to
            to VG_(indexXA) at zero here?  Because
            VG_(get_data_description) guarantees to zero terminate
            descr1/2 regardless of the outcome of the call.  So there's
            always at least one element in each XA after the call.
         */
         if (0 == VG_(strlen)( VG_(indexXA)( xe->XE.Heap.descr1, 0 ))
             || !have_descr) {
            VG_(deleteXA)( xe->XE.Heap.descr1 );
            xe->XE.Heap.descr1 = NULL;
         }
         if (0 == VG_(strlen)( VG_(indexXA)( xe->XE.Heap.descr2, 0 ))
             || !have_descr) {
            VG_(deleteXA)( xe->XE.Heap.descr2 );
            xe->XE.Heap.descr2 = NULL;
         }

         /* If Dwarf3 info produced nothing useful, see at least if
            we can fish something useful out of the ELF symbol info. */
         if (!have_descr) {
            if (VG_(get_datasym_and_offset)(
                   xe->XE.Heap.addr, &xe->XE.Heap.datasym[0],
                   sizeof(xe->XE.Heap.datasym)-1,
                   &xe->XE.Heap.datasymoff )
               ) {
               tl_assert(xe->XE.Heap.datasym[sizeof(xe->XE.Heap.datasym)-1] 
                         == 0);
            }
         }
         break;
      }
      case XE_Arith:
         break;
      case XE_SysParam:
         break;
      default:
         VG_(tool_panic)("update_extra");
   }
   return sizeof(XError);
}

Bool pc_is_recognised_suppression ( Char* name, Supp *su )
{
   SuppKind skind;

   if      (VG_STREQ(name, "SorG"))     skind = XS_SorG;
   else if (VG_STREQ(name, "Heap"))     skind = XS_Heap;
   else if (VG_STREQ(name, "Arith"))    skind = XS_Arith;
   else if (VG_STREQ(name, "SysParam")) skind = XS_SysParam;
   else
      return False;

   VG_(set_supp_kind)(su, skind);
   return True;
}

Bool pc_read_extra_suppression_info ( Int fd, Char** bufpp, 
                                      SizeT* nBufp, Supp* su )
{
   Bool eof;
   if (VG_(get_supp_kind)(su) == XS_SysParam) {
      eof = VG_(get_line) ( fd, bufpp, nBufp, NULL );
      if (eof) return False;
      VG_(set_supp_string)(su, VG_(strdup)("pc.common.presi.1", *bufpp));
   }
   return True;
}

Bool pc_error_matches_suppression (Error* err, Supp* su)
{
   ErrorKind ekind = VG_(get_error_kind)(err);
   switch (VG_(get_supp_kind)(su)) {
      case XS_SorG:     return ekind == XE_SorG;
      case XS_Heap:     return ekind == XE_Heap;
      case XS_Arith:    return ekind == XE_Arith;
      case XS_SysParam: return ekind == XE_SysParam;
      default:
         VG_(printf)("Error:\n"
                     "  unknown suppression type %d\n",
                     VG_(get_supp_kind)(su));
         VG_(tool_panic)("unknown suppression type in "
                         "pc_error_matches_suppression");
   }
}

Char* pc_get_error_name ( Error* err )
{
   XError *xe = (XError*)VG_(get_error_extra)(err);
   tl_assert(xe);
   switch (xe->tag) {
      case XE_SorG:     return "SorG";
      case XE_Heap:     return "Heap";
      case XE_Arith:    return "Arith";
      case XE_SysParam: return "SysParam";
      default:          VG_(tool_panic)("get_error_name: unexpected type");
   }
}

Bool pc_get_extra_suppression_info ( Error* err,
                                     /*OUT*/Char* buf, Int nBuf )
{
   ErrorKind ekind = VG_(get_error_kind )(err);
   tl_assert(buf);
   tl_assert(nBuf >= 16); // stay sane
   if (XE_SysParam == ekind) {
      Char* errstr = VG_(get_error_string)(err);
      tl_assert(errstr);
      VG_(snprintf)(buf, nBuf-1, "%s", errstr);
      return True;
   } else {
      return False;
   }
}


/*--------------------------------------------------------------------*/
/*--- end                                              pc_common.c ---*/
/*--------------------------------------------------------------------*/
