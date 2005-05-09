
/*--------------------------------------------------------------------*/
/*--- Simple tool for counting UInstrs, using a C helper.          ---*/
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

#include "tool.h"

/* Nb: use ULongs because the numbers can get very big */
static ULong n_dlrr_calls   = 0;
static ULong n_BBs          = 0;
static ULong n_UInstrs      = 0;
static ULong n_guest_instrs = 0;
static ULong n_Jccs         = 0;
static ULong n_Jccs_untaken = 0;

static void add_one_dlrr_call(void)
{
   n_dlrr_calls++;
}

/* See comment above lk_instrument for reason why n_machine_instrs is
   incremented here. */
static void add_one_BB(void)
{
   n_BBs++;
   n_guest_instrs++;
}

static void add_one_UInstr(void)
{
   n_UInstrs++;
}

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

static void lk_post_clo_init(void)
{
}

/* Note: machine instructions are marked by an INCEIP at the end of each one,
   except for the final one in the basic block which ends in an
   unconditional JMP.  Sometimes the final unconditional JMP is preceded by
   a conditional JMP (Jcc), and thus it isn't reached.  Eg:

      <code a>
      INCEIP ...

      <code b>
      Jcc ...
      JMP ...     (will not be reached if Jcc succeeds)

   If we simplemindedly added calls to add_one_guest_instr() before INCEIPs
   and unconditional JMPs, we'd sometimes miss the final call (when a
   preceding conditional JMP succeeds), underestimating the machine instruction
   count.

      <code a>
      call add_one_guest_instr()
      INCEIP ...

      <code b>
      Jcc ...
      call add_one_guest_instr()
      JMP ...

   Instead we add a call before each INCEIP, and also one at the start of the
   block, but not one at the end, viz:

      call add_one_guest_instr()

      <code a>
      call add_one_guest_instr()
      INCEIP ...

      <code b>
      Jcc ...
      JMP ...

   Which gives us the right answer.  And just to avoid two C calls, we fold
   the basic-block-beginning call in with add_one_BB().  Phew.
*/ 
static IRBB* lk_instrument(IRBB* bb_in, VexGuestLayout* layout, 
                           IRType gWordTy, IRType hWordTy )
{
   IRDirty* di;
   Int      i;
   IRBB*    bb;

   if (gWordTy != hWordTy) {
      /* We don't currently support this case. */
      VG_(tool_panic)("host/guest word size mismatch");
   }

   /* Set up BB */
   bb           = emptyIRBB();
   bb->tyenv    = dopyIRTypeEnv(bb_in->tyenv);
   bb->next     = dopyIRExpr(bb_in->next);
   bb->jumpkind = bb_in->jumpkind;

#if 0
   /* We need to know the entry point for this bb to do this.  In any
      case it's pretty meaningless in the presence of bb chasing since
      we may enter this function part way through an IRBB. */
   /* Count call to dlrr(), if this BB is dlrr()'s entry point */
   if (VG_(get_fnname_if_entry)(orig_addr, fnname, 100) &&
       0 == VG_(strcmp)(fnname, "_dl_runtime_resolve")) 
   {
      addStmtToIRBB( 
         bb,
         IRStmt_Dirty(
            unsafeIRDirty_0_N( "add_one_dlrr_call", mkIRExprVec_0() )
      ));
   }
#endif

   /* Count this basic block */
   di = unsafeIRDirty_0_N( 0, "add_one_BB", &add_one_BB, mkIRExprVec_0() );
   addStmtToIRBB( bb, IRStmt_Dirty(di) );

   for (i = 0; i < bb_in->stmts_used; i++) {
      IRStmt* st = bb_in->stmts[i];
      if (!st) continue;

      switch (st->tag) {
         case Ist_Exit:
            /* Count Jcc */
            addStmtToIRBB( 
               bb,
               IRStmt_Dirty(
                  unsafeIRDirty_0_N( 0, "add_one_Jcc", &add_one_Jcc, 
                                        mkIRExprVec_0() )
            ));
            addStmtToIRBB( bb, dopyIRStmt(st) );
            /* Count non-taken Jcc */
            addStmtToIRBB( 
               bb,
               IRStmt_Dirty(
                  unsafeIRDirty_0_N( 0, "add_one_Jcc_untaken", &add_one_Jcc_untaken, 
                                        mkIRExprVec_0() )
            ));
            break;

         default:
            addStmtToIRBB( bb, st );
      }
   }

   return bb;
   

#if 0
   UCodeBlock* cb;
   Int         i;
   UInstr*     u;
   Char        fnname[100];

   cb = VG_(setup_UCodeBlock)(cb_in);

   /* Count basic block */
   VG_(call_helper_0_0)(cb, (Addr) & add_one_BB);

   for (i = 0; i < VG_(get_num_instrs)(cb_in); i++) {
      u = VG_(get_instr)(cb_in, i);

      switch (u->opcode) {
         case NOP: case LOCK: case CALLM_S: case CALLM_E:
            break;
   
         case INCEIP:
            /* Count x86 instr */
            VG_(call_helper_0_0)(cb, (Addr) & add_one_x86_instr);
            VG_(copy_UInstr)(cb, u);
            break;

         case JMP:
            if (u->cond != CondAlways) {
               /* Count Jcc */
               VG_(call_helper_0_0)(cb, (Addr) & add_one_Jcc);
               VG_(copy_UInstr)(cb, u);
               /* Count non-taken Jcc */
               VG_(call_helper_0_0)(cb, (Addr) & add_one_Jcc_untaken);
            } else {
               VG_(copy_UInstr)(cb, u);
            }
            break;

         default:
            /* Count UInstr */
            VG_(call_helper_0_0)(cb, (Addr) & add_one_UInstr);
            VG_(copy_UInstr)(cb, u);
            break;
      }
   }

   VG_(free_UCodeBlock)(cb_in);
   return cb;
#endif
}

static void lk_fini(Int exitcode)
{
    VG_(message)(Vg_UserMsg,
                 "Counted %d calls to _dl_runtime_resolve()", n_dlrr_calls);

    VG_(message)(Vg_UserMsg, "");
    VG_(message)(Vg_UserMsg, "Executed:");
    VG_(message)(Vg_UserMsg, "  BBs:          %u", n_BBs);
    VG_(message)(Vg_UserMsg, "  guest instrs: %u", n_guest_instrs);
    VG_(message)(Vg_UserMsg, "  UInstrs:      %u", n_UInstrs);

    VG_(message)(Vg_UserMsg, "");
    VG_(message)(Vg_UserMsg, "Jccs:");
    VG_(message)(Vg_UserMsg, "  total:       %u", n_Jccs);
    VG_(message)(Vg_UserMsg, "  %% taken:     %u%%",
                             (n_Jccs - n_Jccs_untaken)*100 / 
                             (n_Jccs ? n_Jccs : 1));

    VG_(message)(Vg_UserMsg, "");
    VG_(message)(Vg_UserMsg, "Ratios:");
    VG_(message)(Vg_UserMsg, "  guest instrs : BB      = %3u : 10",
                             10 * n_guest_instrs / n_BBs);
    VG_(message)(Vg_UserMsg, "     UInstrs : BB        = %3u : 10",
                             10 * n_UInstrs / n_BBs);
    VG_(message)(Vg_UserMsg, "     UInstrs : x86_instr = %3u : 10",
                             10 * n_UInstrs / n_guest_instrs);

    VG_(message)(Vg_UserMsg, "");
    VG_(message)(Vg_UserMsg, "Exit code:     %d", exitcode);
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
}

VG_DETERMINE_INTERFACE_VERSION(lk_pre_clo_init, 0)

/*--------------------------------------------------------------------*/
/*--- end                                                lk_main.c ---*/
/*--------------------------------------------------------------------*/

