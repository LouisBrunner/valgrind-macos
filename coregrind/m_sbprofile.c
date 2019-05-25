
/*--------------------------------------------------------------------*/
/*--- For printing superblock profiles               m_sbprofile.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2012-2017 Mozilla Foundation

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

/* Contributed by Julian Seward <jseward@acm.org> */

#include "pub_core_basics.h"
#include "pub_core_transtab.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcassert.h"
#include "pub_core_debuginfo.h"
#include "pub_core_translate.h"
#include "pub_core_options.h"
#include "pub_core_sbprofile.h"    // self

/*====================================================================*/
/*=== SB profiling                                                 ===*/
/*====================================================================*/

static UInt n_profiles = 0;

static 
void show_SB_profile ( const SBProfEntry tops[], UInt n_tops,
                       ULong score_total, ULong ecs_done )
{
   ULong score_cumul, score_cumul_saved, score_here;
   Int   r; /* must be signed */

   HChar ecs_txt[50];    // large enough
   if (ecs_done > 0) {
      VG_(sprintf)(ecs_txt, "%'llu ecs done", ecs_done);
   } else {
      VG_(strcpy)(ecs_txt, "for the entire run");
   }

   vg_assert(VG_(clo_profyle_sbs));

   VG_(printf)("\n");
   VG_(printf)("<<<---<<<---<<<---<<<---<<<---<<<---<<<---"
               "<<<---<<<---<<<---<<<---<<<---<<<\n");
   VG_(printf)("<<<---<<<---<<<---<<<---<<<---<<<---<<<---"
               "<<<---<<<---<<<---<<<---<<<---<<<\n");
   VG_(printf)("\n");
   VG_(printf)("<<< BEGIN SB Profile #%u (%s)\n",
               ++n_profiles, ecs_txt);
   VG_(printf)("<<<\n");
   VG_(printf)("\n");

   VG_(printf)("Total score = %'llu\n\n", score_total);

   // FIXME JRS EPOCH 28 July 2017: this is probably not right in general
   DiEpoch cur_ep = VG_(current_DiEpoch)();

   /* Print an initial per-block summary. */
   VG_(printf)("rank  ---cumulative---      -----self-----\n");
   score_cumul = 0;
   for (r = 0; r < n_tops; r++) {
      if (tops[r].addr == 0)
         continue;
      if (tops[r].score == 0)
         continue;

      const HChar *name;
      VG_(get_fnname_w_offset)(cur_ep, tops[r].addr, &name);

      score_here = tops[r].score;
      score_cumul += score_here;

      /* Careful: do not divide by zero. score_total == 0 implies
         score_cumul == 0 and also score_here == 0. */
      Double percent_cumul =
         score_total == 0 ? 100.0 : score_cumul * 100.0 / score_total;
      Double percent_here =
         score_total == 0 ? 100.0 : score_here * 100.0 / score_total;
        
      VG_(printf)("%3d: (%9llu %5.2f%%)   %9llu %5.2f%%      0x%lx %s\n",
                  r,
                  score_cumul, percent_cumul,
                  score_here,  percent_here, tops[r].addr, name);
   }
   score_cumul_saved = score_cumul;

   if (VG_(clo_profyle_flags) > 0) {

      /* Show the details, if requested. */
      VG_(printf)("\n");
      VG_(printf)("-----------------------------"
                  "------------------------------\n");
      VG_(printf)("--- SB Profile (SB details)  "
                  "                           ---\n");
      VG_(printf)("-----------------------------"
                  "------------------------------\n");
      VG_(printf)("\n");

      score_cumul = 0;
      for (r = 0; r < n_tops; r++) {
         if (tops[r].addr == 0)
            continue;
         if (tops[r].score == 0)
            continue;

         const HChar *name;
         VG_(get_fnname_w_offset)(cur_ep, tops[r].addr, &name);

         score_here = tops[r].score;
         score_cumul += score_here;

         /* Careful: do not divide by zero. score_total == 0 implies
            score_cumul == 0 and also score_here == 0. */
         Double percent_cumul =
           score_total == 0 ? 100.0 : score_cumul * 100.0 / score_total;
         Double percent_here =
           score_total == 0 ? 100.0 : score_here * 100.0 / score_total;

         VG_(printf)("\n");
         VG_(printf)("=-=-=-=-=-=-=-=-=-=-=-=-=-= begin SB rank %d "
                     "=-=-=-=-=-=-=-=-=-=-=-=-=-=\n\n", r);
         VG_(printf)("%3d: (%9llu %5.2f%%)   %9llu %5.2f%%      0x%lx %s\n",
                     r,
                     score_cumul, percent_cumul,
                     score_here,  percent_here, tops[r].addr, name );
         VG_(printf)("\n");
         VG_(discard_translations)(tops[r].addr, 1, "bb profile");
         VG_(translate)(0, tops[r].addr, True, VG_(clo_profyle_flags), 0, True);
         VG_(printf)("=-=-=-=-=-=-=-=-=-=-=-=-=-=  end SB rank %d  "
                     "=-=-=-=-=-=-=-=-=-=-=-=-=-=\n\n", r);
      }

      /* Print a final per-block summary, in reverse order, for the
         convenience of people reading up from the end. */
      score_cumul = score_cumul_saved;
      for (r = n_tops-1; r >= 0; r--) {
         if (tops[r].addr == 0)
            continue;
         if (tops[r].score == 0)
            continue;

         const HChar *name;
         VG_(get_fnname_w_offset)(cur_ep, tops[r].addr, &name);

         score_here = tops[r].score;

         /* Careful: do not divide by zero. score_total == 0 implies
            score_cumul == 0 and also score_here == 0. */
         Double percent_cumul =
           score_total == 0 ? 100.0 : score_cumul * 100.0 / score_total;
         Double percent_here =
           score_total == 0 ? 100.0 : score_here * 100.0 / score_total;

         VG_(printf)("%3d: (%9llu %5.2f%%)   %9llu %5.2f%%      0x%lx %s\n",
                     r,
                     score_cumul, percent_cumul,
                     score_here,  percent_here, tops[r].addr, name );
         score_cumul -= score_here;
      }
      VG_(printf)("rank  ---cumulative---      -----self-----\n");

   }

   VG_(printf)("\n");
   VG_(printf)(">>>\n");
   VG_(printf)(">>> END SB Profile #%u (%s)\n",
               n_profiles, ecs_txt);
   VG_(printf)(">>>\n");
   VG_(printf)(">>>--->>>--->>>--->>>--->>>--->>>--->>>---"
               ">>>--->>>--->>>--->>>--->>>--->>>\n");
   VG_(printf)(">>>--->>>--->>>--->>>--->>>--->>>--->>>---"
               ">>>--->>>--->>>--->>>--->>>--->>>\n");
   VG_(printf)("\n");
}


/* Get and print a profile.  Also, zero out the counters so that if we
   call it again later, the second call will only show new work done
   since the first call.  ecs_done == 0 is taken to mean this is a
   run-end profile. */
void VG_(get_and_show_SB_profile) ( ULong ecs_done )
{
   /* The number of blocks to show for a end-of-run profile */
#  define N_MAX_END 200
   /* The number of blocks to show for a mid-run profile. */
#  define N_MAX_INTERVAL 20
   vg_assert(N_MAX_INTERVAL <= N_MAX_END);
   SBProfEntry tops[N_MAX_END];
   Int nToShow = ecs_done == 0  ? N_MAX_END  : N_MAX_INTERVAL;
   ULong score_total = VG_(get_SB_profile)(tops, nToShow);
   show_SB_profile(tops, nToShow, score_total, ecs_done);
#  undef N_MAX_END
#  undef N_MAX_INTERVAL
}


/*--------------------------------------------------------------------*/
/*--- end                                            m_sbprofile.c ---*/
/*--------------------------------------------------------------------*/
