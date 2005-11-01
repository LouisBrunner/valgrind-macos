
/*--------------------------------------------------------------------*/
/*--- An example Valgrind tool.                                    ---*/
/*---                                                    lk_main.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Lackey, an example Valgrind tool that does
   some simple program measurement.

   Copyright (C) 2002-2005 Nicholas Nethercote
      njn@valgrind.org

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
#include "pub_tool_tooliface.h"
#include "pub_tool_libcassert.h"
#include "pub_tool_libcprint.h"
#include "pub_tool_debuginfo.h"
#include "pub_tool_libcbase.h"
#include "pub_tool_options.h"


/* The name of the function of which the number of calls is to be
 * counted, with default. Override with command line option
 * --fnname. */
static Char* lk_clo_fnname = "_dl_runtime_resolve";

/* If true, show statistics about loads, stores and alu ops. Set
 * with command line option --detailed-counts. */
static Bool lk_clo_detailed_counts = False;

/***********************************************************************
 * Implement the needs_command_line_options for Valgrind.
 **********************************************************************/

static Bool lk_process_cmd_line_option(Char* arg)
{
   VG_STR_CLO(arg, "--fnname", lk_clo_fnname)
   else VG_BOOL_CLO(arg, "--detailed-counts", lk_clo_detailed_counts)
   else
      return False;
   
   tl_assert(lk_clo_fnname);
   tl_assert(lk_clo_fnname[0]);
   return True;
}

static void lk_print_usage(void)
{  
   VG_(printf)(
"    --fnname=<name>           count calls to <name> [_dl_runtime_resolve]\n"
"    --detailed-counts=no|yes  count loads, stores and alu ops [no]\n"
   );
}

static void lk_print_debug_usage(void)
{  
}

/***********************************************************************
 * Data and helpers related to the default operation of Lackey.
 **********************************************************************/

/* Nb: use ULongs because the numbers can get very big */
static ULong n_func_calls    = 0;
static ULong n_BBs_entered   = 0;
static ULong n_BBs_completed = 0;
static ULong n_IRStmts      = 0;
static ULong n_guest_instrs  = 0;
static ULong n_Jccs          = 0;
static ULong n_Jccs_untaken  = 0;

__attribute__((unused))
static void add_one_func_call(void)
{
   n_func_calls++;
}

static void add_one_BB_entered(void)
{
   n_BBs_entered++;
}

static void add_one_BB_completed(void)
{
   n_BBs_completed++;
}

__attribute__((unused))
static void add_one_IRStmt(void)
{
   n_IRStmts++;
}

__attribute__((unused))
static void add_one_guest_instr(void)
{
   n_guest_instrs++;
}

static void add_one_Jcc(void)
{
   n_Jccs++;
}

static void add_one_Jcc_untaken(void)
{
   n_Jccs_untaken++;
}

/***********************************************************************
 * Data and helpers related to --detailed-counts.
 **********************************************************************/

/* --- Operations --- */

typedef enum { OpLoad=0, OpStore=1, OpAlu=2 } Op;

#define N_OPS 3


/* --- Types --- */

#define N_TYPES 9

static Int type2index ( IRType ty )
{
   switch (ty) {
      case Ity_I1:      return 0;
      case Ity_I8:      return 1;
      case Ity_I16:     return 2;
      case Ity_I32:     return 3;
      case Ity_I64:     return 4;
      case Ity_I128:    return 5;
      case Ity_F32:     return 6;
      case Ity_F64:     return 7;
      case Ity_V128:    return 8;
      default: tl_assert(0); break;
   }
}

static HChar* nameOfTypeIndex ( IRType ty )
{
   switch (ty) {
      case 0: return "I1";   break;
      case 1: return "I8";   break;
      case 2: return "I16";  break;
      case 3: return "I32";  break;
      case 4: return "I64";  break;
      case 5: return "I128"; break;
      case 6: return "F32";  break;
      case 7: return "F64";  break;
      case 8: return "V128"; break;
      default: tl_assert(0); break;
   }
}


/* --- Counts --- */

static ULong detailCounts[N_OPS][N_TYPES];

/* The helper that is called from the instrumented code. */
static VG_REGPARM(1)
void increment_detail(ULong* detail)
{
   (*detail)++;
}

/* A helper that adds the instrumentation for a detail. */
static void instrument_detail(IRBB* bb, Op op, IRType type)
{
   IRDirty* di;
   IRExpr** argv;
   const UInt typeIx = type2index(type);

   tl_assert(op < N_OPS);
   tl_assert(typeIx < N_TYPES);

   argv = mkIRExprVec_1( mkIRExpr_HWord( (HWord)&detailCounts[op][typeIx] ) );
   di = unsafeIRDirty_0_N( 1, "increment_detail", &increment_detail, argv);
   addStmtToIRBB( bb, IRStmt_Dirty(di) );
}

/* Summarize and print the details. */

static void print_details ( void )
{
   Int typeIx;
   VG_(message)(Vg_UserMsg,
                "   Type        Loads       Stores       AluOps");
   VG_(message)(Vg_UserMsg,
                "   -------------------------------------------");
   for (typeIx = 0; typeIx < N_TYPES; typeIx++) {
      VG_(message)(Vg_UserMsg,
                   "   %4s %,12llu %,12llu %,12llu", 
                   nameOfTypeIndex( typeIx ),
                   detailCounts[OpLoad ][typeIx],
                   detailCounts[OpStore][typeIx],
                   detailCounts[OpAlu  ][typeIx]
      );
   }
}


/***********************************************************************
 * Implement the basic_tool_funcs for Valgrind.
 **********************************************************************/

static void lk_post_clo_init(void)
{
   Int op, tyIx;

   for (op = 0; op < N_OPS; op++)
      for (tyIx = 0; tyIx < N_TYPES; tyIx++)
         detailCounts[op][tyIx] = 0;
}

static
IRBB* lk_instrument( IRBB* bb_in, VexGuestLayout* layout, 
                     Addr64 orig_addr_noredir, VexGuestExtents* vge,
                     IRType gWordTy, IRType hWordTy )
{
   IRDirty* di;
   Int      i;
   IRBB*    bb;
   Char     fnname[100];
   IRType   type;

   if (gWordTy != hWordTy) {
      /* We don't currently support this case. */
      VG_(tool_panic)("host/guest word size mismatch");
   }

   /* Set up BB */
   bb           = emptyIRBB();
   bb->tyenv    = dopyIRTypeEnv(bb_in->tyenv);
   bb->next     = dopyIRExpr(bb_in->next);
   bb->jumpkind = bb_in->jumpkind;

   // Copy verbatim any IR preamble preceding the first IMark
   i = 0;
   while (i < bb_in->stmts_used && bb_in->stmts[i]->tag != Ist_IMark) {
      addStmtToIRBB( bb, bb_in->stmts[i] );
      i++;
   }

   /* Count this basic block. */
   di = unsafeIRDirty_0_N( 0, "add_one_BB_entered", &add_one_BB_entered,
                              mkIRExprVec_0() );
   addStmtToIRBB( bb, IRStmt_Dirty(di) );

   for (/*use current i*/; i < bb_in->stmts_used; i++) {
      IRStmt* st = bb_in->stmts[i];
      if (!st || st->tag == Ist_NoOp) continue;

      /* Count one VEX statement. */
      di = unsafeIRDirty_0_N( 0, "add_one_IRStmt", &add_one_IRStmt, 
                                 mkIRExprVec_0() );
      addStmtToIRBB( bb, IRStmt_Dirty(di) );
      
      switch (st->tag) {
         case Ist_IMark:
            /* Count guest instruction. */
            di = unsafeIRDirty_0_N( 0, "add_one_guest_instr",
                                       &add_one_guest_instr, 
                                       mkIRExprVec_0() );
            addStmtToIRBB( bb, IRStmt_Dirty(di) );
            
            /* An unconditional branch to a known destination in the
             * guest's instructions can be represented, in the IRBB to
             * instrument, by the VEX statements that are the
             * translation of that known destination. This feature is
             * called 'BB chasing' and can be influenced by command
             * line option --vex-guest-chase-thresh.
             *
             * To get an accurate count of the calls to a specific
             * function, taking BB chasing into account, we need to
             * check for each guest instruction (Ist_IMark) if it is
             * the entry point of a function.
             */
            tl_assert(lk_clo_fnname);
            tl_assert(lk_clo_fnname[0]);
            if (VG_(get_fnname_if_entry)(st->Ist.IMark.addr, 
                                         fnname, sizeof(fnname))
                && 0 == VG_(strcmp)(fnname, lk_clo_fnname)) {
               di = unsafeIRDirty_0_N( 0, "add_one_func_call", 
                                          &add_one_func_call, 
                                          mkIRExprVec_0() );
               addStmtToIRBB( bb, IRStmt_Dirty(di) );
            }
            addStmtToIRBB( bb, st );
            break;

         case Ist_Exit:
            /* Count Jcc */
            di = unsafeIRDirty_0_N( 0, "add_one_Jcc", &add_one_Jcc, 
                                       mkIRExprVec_0() );
            addStmtToIRBB( bb, IRStmt_Dirty(di) );

            addStmtToIRBB( bb, st );

            /* Count non-taken Jcc */
            di = unsafeIRDirty_0_N( 0, "add_one_Jcc_untaken", 
                                       &add_one_Jcc_untaken, mkIRExprVec_0() );
            addStmtToIRBB( bb, IRStmt_Dirty(di) );
            break;

         /* Someone on the users list asked for something like this
          * just the other day (Christian Stimming, "Fast profiling in
          * valgrind?", 25 Oct).  Personally I think it'd be a
          * valuable addition.
          *   
          * Not hard to do either: for stores, examine Ist_Store, and
          * use typeOfIRExpr(bb->tyenv, st->Ist.Store.data) to get the
          * store type.  For loads and ALU ops, you only need to look
          * at Ist_Tmp cases where the Ist.Tmp.data is either Iex_Load
          * or Iex_{Unop,Binop}.  All statements you will ever
          * encounter will satisfy isFlatIRStmt which essentially
          * constrains them to being flat SSA-style.
          */
         case Ist_Store:
            if (lk_clo_detailed_counts) {
               type = typeOfIRExpr(bb->tyenv, st->Ist.Store.data);
               tl_assert(type != Ity_INVALID);
               instrument_detail( bb, OpStore, type );
            }
            addStmtToIRBB( bb, st );
            break;

         case Ist_Tmp:
            if (lk_clo_detailed_counts) {
               IRExpr* expr = st->Ist.Tmp.data;
               type = typeOfIRExpr(bb->tyenv, expr);
               tl_assert(type != Ity_INVALID);
               switch (expr->tag) {
                  case Iex_Load:
                     instrument_detail( bb, OpLoad, type );
                     break;
                  case Iex_Unop:
                  case Iex_Binop:
                  case Iex_Mux0X:
                     instrument_detail( bb, OpAlu, type );
                     break;
                  default:
                     break;
               }
            }
            addStmtToIRBB( bb, st );
            break;

         default:
            addStmtToIRBB( bb, st );
      }
   }

   /* Count this basic block. */
   di = unsafeIRDirty_0_N( 0, "add_one_BB_completed", 
                              &add_one_BB_completed, mkIRExprVec_0() );
   addStmtToIRBB( bb, IRStmt_Dirty(di) );

   return bb;
}

static void lk_fini(Int exitcode)
{
   char percentify_buf[4]; /* Two digits, '%' and 0. */
   const int percentify_size = sizeof(percentify_buf);
   const int percentify_decs = 0;
   
   tl_assert(lk_clo_fnname);
   tl_assert(lk_clo_fnname[0]);
   VG_(message)(Vg_UserMsg,
      "Counted %,llu calls to %s()", n_func_calls, lk_clo_fnname);

   VG_(message)(Vg_UserMsg, "");
   VG_(message)(Vg_UserMsg, "Jccs:");
   VG_(message)(Vg_UserMsg, "  total:         %,llu", n_Jccs);
   VG_(percentify)((n_Jccs - n_Jccs_untaken), (n_Jccs ? n_Jccs : 1),
      percentify_decs, percentify_size, percentify_buf);
   VG_(message)(Vg_UserMsg, "  taken:         %,llu (%s)", 
      (n_Jccs - n_Jccs_untaken), percentify_buf);
   
   VG_(message)(Vg_UserMsg, "");
   VG_(message)(Vg_UserMsg, "Executed:");
   VG_(message)(Vg_UserMsg, "  BBs entered:   %,llu", n_BBs_entered);
   VG_(message)(Vg_UserMsg, "  BBs completed: %,llu", n_BBs_completed);
   VG_(message)(Vg_UserMsg, "  guest instrs:  %,llu", n_guest_instrs);
   VG_(message)(Vg_UserMsg, "  IRStmts:       %,llu", n_IRStmts);
   
   VG_(message)(Vg_UserMsg, "");
   VG_(message)(Vg_UserMsg, "Ratios:");
   tl_assert(n_BBs_entered); // Paranoia time.
   VG_(message)(Vg_UserMsg, "  guest instrs : BB entered  = %3u : 10",
      10 * n_guest_instrs / n_BBs_entered);
   VG_(message)(Vg_UserMsg, "       IRStmts : BB entered  = %3u : 10",
      10 * n_IRStmts / n_BBs_entered);
   tl_assert(n_guest_instrs); // Paranoia time.
   VG_(message)(Vg_UserMsg, "       IRStmts : guest instr = %3u : 10",
      10 * n_IRStmts / n_guest_instrs);

   if (lk_clo_detailed_counts) {
      VG_(message)(Vg_UserMsg, "");
      VG_(message)(Vg_UserMsg, "IR-level counts by type:");
      print_details();
   }

   VG_(message)(Vg_UserMsg, "");
   VG_(message)(Vg_UserMsg, "Exit code:       %d", exitcode);
}

static void lk_pre_clo_init(void)
{
   VG_(details_name)            ("Lackey");
   VG_(details_version)         (NULL);
   VG_(details_description)     ("an example Valgrind tool");
   VG_(details_copyright_author)(
      "Copyright (C) 2002-2005, and GNU GPL'd, by Nicholas Nethercote.");
   VG_(details_bug_reports_to)  (VG_BUGS_TO);
   VG_(details_avg_translation_sizeB) ( 175 );

   VG_(basic_tool_funcs)          (lk_post_clo_init,
                                   lk_instrument,
                                   lk_fini);
   VG_(needs_command_line_options)(lk_process_cmd_line_option,
                                   lk_print_usage,
                                   lk_print_debug_usage);
}

VG_DETERMINE_INTERFACE_VERSION(lk_pre_clo_init)

/*--------------------------------------------------------------------*/
/*--- end                                                lk_main.c ---*/
/*--------------------------------------------------------------------*/
