
/*--------------------------------------------------------------------*/
/*--- Simple skin for counting UInstrs, using a C helper.          ---*/
/*---                                                    lk_main.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Lackey, an example Valgrind skin that does
   some simple program measurement.

   Copyright (C) 2002 Nicholas Nethercote
      njn25@cam.ac.uk

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

#include "vg_skin.h"

//#define uInstr0   VG_(new_UInstr0)
//#define uLiteral  VG_(set_lit_field)

/* Nb: use ULongs because the numbers can get very big */
static ULong n_dlrr_calls   = 0;
static ULong n_BBs          = 0;
static ULong n_UInstrs      = 0;
static ULong n_x86_instrs   = 0;
static ULong n_Jccs         = 0;
static ULong n_Jccs_untaken = 0;

static void add_one_dlrr_call(void)
{
   n_dlrr_calls++;
}

/* See comment above SK_(instrument) for reason why n_x86_instrs is
   incremented here. */
static void add_one_BB(void)
{
   n_BBs++;
   n_x86_instrs++;
}

static void add_one_UInstr(void)
{
   n_UInstrs++;
}

static void add_one_x86_instr(void)
{
   n_x86_instrs++;
}

static void add_one_Jcc(void)
{
   n_Jccs++;
}

static void add_one_Jcc_untaken(void)
{
   n_Jccs_untaken++;
}

void SK_(pre_clo_init)(VgNeeds* needs, VgTrackEvents* not_used)
{
   needs->name        = "lackey";
   needs->description = "a UInstr counter";
   needs->description = "njn25@cam.ac.uk";

   VG_(register_compact_helper)((Addr) & add_one_dlrr_call);
   VG_(register_compact_helper)((Addr) & add_one_BB);
   VG_(register_compact_helper)((Addr) & add_one_x86_instr);
   VG_(register_compact_helper)((Addr) & add_one_UInstr);
   VG_(register_compact_helper)((Addr) & add_one_Jcc);
   VG_(register_compact_helper)((Addr) & add_one_Jcc_untaken);
}

void SK_(post_clo_init)(void)
{
}

/* Note: x86 instructions are marked by an INCEIP at the end of each one,
   except for the final one in the basic block which ends in an
   unconditional JMP.  Sometimes the final unconditional JMP is preceded by
   a conditional JMP (Jcc), and thus it isn't reached.  Eg:

      <code a>
      INCEIP ...

      <code b>
      Jcc ...
      JMP ...     (will not be reached if Jcc succeeds)

   If we simplemindedly added calls to add_one_x86_instr() before INCEIPs
   and unconditional JMPs, we'd sometimes miss the final call (when a
   preceding conditional JMP succeeds), underestimating the x86 instruction
   count.

      <code a>
      call add_one_x86_instr()
      INCEIP ...

      <code b>
      Jcc ...
      call add_one_x86_instr()
      JMP ...

   Instead we add a call before each INCEIP, and also one at the start of the
   block, but not one at the end, viz:

      call add_one_x86_instr()

      <code a>
      call add_one_x86_instr()
      INCEIP ...

      <code b>
      Jcc ...
      JMP ...

   Which gives us the right answer.  And just to avoid two C calls, we fold
   the basic-block-beginning call in with add_one_BB().  Phew.
*/ 
UCodeBlock* SK_(instrument)(UCodeBlock* cb_in, Addr orig_addr)
{
   UCodeBlock* cb;
   Int         i;
   UInstr*     u;
   Char        fnname[100];

   cb = VG_(alloc_UCodeBlock)();
   cb->nextTemp = cb_in->nextTemp;

   /* Count call to dlrr(), if this BB is dlrr()'s entry point */
   if (VG_(get_fnname_if_entry)(orig_addr, fnname, 100) &&
       0 == VG_(strcmp)(fnname, "_dl_runtime_resolve")) 
   {
      VG_(call_helper_0_0)(cb, (Addr) & add_one_dlrr_call);
   }

   /* Count basic block */
   VG_(call_helper_0_0)(cb, (Addr) & add_one_BB);

   for (i = 0; i < cb_in->used; i++) {
      u = &cb_in->instrs[i];

      switch (u->opcode) {
         case NOP: case CALLM_S: case CALLM_E:
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
}

void SK_(fini)(void)
{
    VG_(message)(Vg_UserMsg,
                 "Counted %d calls to _dl_runtime_resolve()", n_dlrr_calls);

    VG_(message)(Vg_UserMsg, "");
    VG_(message)(Vg_UserMsg, "Executed:");
    VG_(message)(Vg_UserMsg, "  BBs:         %u", n_BBs);
    VG_(message)(Vg_UserMsg, "  x86 instrs:  %u", n_x86_instrs);
    VG_(message)(Vg_UserMsg, "  UInstrs:     %u", n_UInstrs);

    VG_(message)(Vg_UserMsg, "");
    VG_(message)(Vg_UserMsg, "Jccs:");
    VG_(message)(Vg_UserMsg, "  total:       %u", n_Jccs);
    VG_(message)(Vg_UserMsg, "  %% taken:     %u%%",
                             (n_Jccs - n_Jccs_untaken)*100 / n_Jccs);

    VG_(message)(Vg_UserMsg, "");
    VG_(message)(Vg_UserMsg, "Ratios:");
    VG_(message)(Vg_UserMsg, "  x86 instrs : BB        = %3u : 10",
                             10 * n_x86_instrs / n_BBs);
    VG_(message)(Vg_UserMsg, "     UInstrs : BB        = %3u : 10",
                             10 * n_UInstrs / n_BBs);
    VG_(message)(Vg_UserMsg, "     UInstrs : x86_instr = %3u : 10",
                             10 * n_UInstrs / n_x86_instrs);

}

/*--------------------------------------------------------------------*/
/*--- end                                                lk_main.c ---*/
/*--------------------------------------------------------------------*/

