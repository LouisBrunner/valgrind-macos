
/*--------------------------------------------------------------------*/
/*--- Take snapshots of client stacks.              m_stacktrace.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2017 Julian Seward 
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
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_threadstate.h"
#include "pub_core_debuginfo.h"     // XXX: circular dependency
#include "pub_core_aspacemgr.h"     // For VG_(is_addressable)()
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"
#include "pub_core_machine.h"
#include "pub_core_options.h"
#include "pub_core_stacks.h"        // VG_(stack_limits)
#include "pub_core_stacktrace.h"
#include "pub_core_syswrap.h"       // VG_(is_in_syscall)
#include "pub_core_xarray.h"
#include "pub_core_clientstate.h"   // VG_(client__dl_sysinfo_int80)
#include "pub_core_trampoline.h"
#include "config.h"


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- BEGIN platform-dependent unwinder worker functions   ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

/* Take a snapshot of the client's stack, putting up to 'max_n_ips'
   IPs into 'ips'.  In order to be thread-safe, we pass in the
   thread's IP SP, FP if that's meaningful, and LR if that's
   meaningful.  Returns number of IPs put in 'ips'.

   If you know what the thread ID for this stack is, send that as the
   first parameter, else send zero.  This helps generate better stack
   traces on ppc64-linux and has no effect on other platforms.
*/

/* Do frame merging in the _i frames in _ips array of recursive cycles
   of up to _nframes.  The merge is done during stack unwinding
   (i.e. in platform specific unwinders) to collect as many
   "interesting" stack traces as possible. */
#define RECURSIVE_MERGE(_nframes,_ips,_i) if (UNLIKELY(_nframes > 0)) \
do {                                                                  \
   Int dist;                                                          \
   for (dist = 1; dist <= _nframes && dist < (Int)_i; dist++) {       \
      if (_ips[_i-1] == _ips[_i-1-dist]) {                            \
         _i = _i - dist;                                              \
         break;                                                       \
      }                                                               \
   }                                                                  \
} while (0)

/* Note about calculation of fp_min : fp_min is the lowest address
   which can be accessed during unwinding. This is SP - VG_STACK_REDZONE_SZB.
   On most platforms, this will be equal to SP (as VG_STACK_REDZONE_SZB
   is 0). However, on some platforms (e.g. amd64), there is an accessible
   redzone below the SP. Some CFI unwind info are generated, taking this
   into account. As an example, the following is a CFI unwind info on
   amd64 found for a 'retq' instruction:
[0x400f7e .. 0x400f7e]: let cfa=oldSP+8 in RA=*(cfa+-8) SP=cfa+0 BP=*(cfa+-16)
  0x400f7e: retq
  As you can see, the previous BP is found 16 bytes below the cfa, which
  is the oldSP+8. So, effectively, the BP is found 8 bytes below the SP.
  The fp_min must take this into account, otherwise, VG_(use_CF_info) will
  not unwind the BP. */
   
/* ------------------------ x86 ------------------------- */

#if defined(VGP_x86_linux) || defined(VGP_x86_darwin) \
    || defined(VGP_x86_solaris) || defined(VGP_x86_freebsd)

#define N_FP_CF_VERIF 1021
// prime number so that size of fp_CF_verif is just below 4K or 8K
// Note that this prime nr differs from the one chosen in
// m_debuginfo/debuginfo.c for the cfsi cache : in case we have
// a collision here between two IPs, we expect to not (often) have the
// same collision in the cfsi cache (and vice-versa).

// unwinding with fp chain is ok:
#define FPUNWIND 0
// there is no CFI info for this IP:
#define NOINFO   1
// Unwind with FP is not ok, must use CF unwind:
#define CFUNWIND 2

static Addr fp_CF_verif_cache [N_FP_CF_VERIF];

/* An unwind done by following the fp chain technique can be incorrect
   as not all frames are respecting the standard bp/sp ABI.
   The CF information is now generated by default by gcc
   (as part of the dwarf info). However, unwinding using CF information
   is significantly slower : a slowdown of 20% has been observed
   on an helgrind test case.
   So, by default, the unwinding will be done using the fp chain.
   But before accepting to unwind an IP with fp_chain, the result
   of the unwind will be checked with the CF information.
   This check can give 3 results:
     FPUNWIND (0): there is CF info, and it gives the same result as fp unwind.
       => it is assumed that future unwind for this IP can be done
          with the fast fp chain, without further CF checking
     NOINFO   (1): there is no CF info (so, fp unwind is the only do-able thing)
     CFUNWIND (2): there is CF info, but unwind result differs.
       => it is assumed that future unwind for this IP must be done
       with the CF info.
   Of course, if each fp unwind implies a check done with a CF unwind,
   it would just be slower => we cache the check result in an
   array of checked Addr.
   The check for an IP will be stored at
    fp_CF_verif_cache[IP % N_FP_CF_VERIF] as one of:
                     IP ^ FPUNWIND
                     IP ^ NOINFO
                     IP ^ CFUNWIND

   Note: we can re-use the last (ROUNDDOWN (log (N_FP_CF_VERIF))) bits
   to store the check result, as they are guaranteed to be non significant
   in the comparison between 2 IPs stored in fp_CF_verif_cache).
   In other words, if two IPs are only differing on the last 2 bits,
   then they will not land in the same cache bucket.
*/

/* cached result of VG_(FPO_info_present)(). Refreshed each time
   the fp_CF_verif_generation is different of the current debuginfo
   generation. */
static Bool FPO_info_present = False;

static UInt fp_CF_verif_generation = 0;
// Our cache has to be maintained in sync with the CFI cache.
// Each time the debuginfo is changed, its generation will be incremented.
// We will clear our cache when our saved generation differs from
// the debuginfo generation.

UInt VG_(get_StackTrace_wrk) ( ThreadId tid_if_known,
                               /*OUT*/Addr* ips, UInt max_n_ips,
                               /*OUT*/Addr* sps, /*OUT*/Addr* fps,
                               const UnwindStartRegs* startRegs,
                               Addr fp_max_orig )
{
   const Bool do_stats = False; // compute and output some stats regularly.
   static struct {
      UInt nr; // nr of stacktraces computed
      UInt nf; // nr of frames computed
      UInt Ca; // unwind for which cache indicates CFUnwind must be used.
      UInt FF; // unwind for which cache indicates FPUnwind can be used.
      UInt Cf; // unwind at end of stack+store CFUNWIND (xip not end of stack).
      UInt Fw; // unwind at end of stack+store FPUNWIND
      UInt FO; // unwind + store FPUNWIND
      UInt CF; // unwind + store CFUNWIND. Details below.
      UInt xi; UInt xs; UInt xb; // register(s) which caused a 'store CFUNWIND'.
      UInt Ck; // unwind fp invalid+store FPUNWIND
      UInt MS; // microsoft unwind
   } stats;
   
   const Bool   debug = False;
   //                 = VG_(debugLog_getLevel) () > 3;
   //                 = True;
   //                 = stats.nr >= 123456;
   const HChar* unwind_case; // used when debug is True.
   // Debugging this function is not straightforward.
   // Here is the easiest way I have found:
   // 1. Change the above to True.
   // 2. Start your program under Valgrind with --tool=none --vgdb-error=0
   // 3. Use GDB/vgdb to put a breakpoint where you want to debug the stacktrace
   // 4. Continue till breakpoint is encountered
   // 5. From GDB, use 'monitor v.info scheduler' and examine the unwind traces.
   //    You might have to do twice 'monitor v.info scheduler' to see
   //    the effect of caching the results of the verification.
   //    You can also modify the debug dynamically using by using
   //    'monitor v.set debuglog 4.

   Int   i;
   Addr  fp_max;
   UInt  n_found = 0;
   const Int cmrf = VG_(clo_merge_recursive_frames);

   vg_assert(sizeof(Addr) == sizeof(UWord));
   vg_assert(sizeof(Addr) == sizeof(void*));

   D3UnwindRegs fpverif_uregs; // result of CF unwind for a check reason.
   Addr xip_verified = 0; // xip for which we have calculated fpverif_uregs
   // 0 assigned to silence false positive -Wuninitialized warning
   // This is a false positive as xip_verified is assigned when
   // xip_verif > CFUNWIND and only used if xip_verif > CFUNWIND.

   D3UnwindRegs uregs;
   uregs.xip = (Addr)startRegs->r_pc;
   uregs.xsp = (Addr)startRegs->r_sp;
   uregs.xbp = startRegs->misc.X86.r_ebp;
   Addr fp_min = uregs.xsp - VG_STACK_REDZONE_SZB;

   /* Snaffle IPs from the client's stack into ips[0 .. max_n_ips-1],
      stopping when the trail goes cold, which we guess to be
      when FP is not a reasonable stack location. */

   // JRS 2002-sep-17: hack, to round up fp_max to the end of the
   // current page, at least.  Dunno if it helps.
   // NJN 2002-sep-17: seems to -- stack traces look like 1.0.X again
   fp_max = VG_PGROUNDUP(fp_max_orig);
   if (fp_max >= sizeof(Addr))
      fp_max -= sizeof(Addr);

   if (debug)
      VG_(printf)("max_n_ips=%u fp_min=0x%08lx fp_max_orig=0x08%lx, "
                  "fp_max=0x%08lx ip=0x%08lx fp=0x%08lx\n",
                  max_n_ips, fp_min, fp_max_orig, fp_max,
                  uregs.xip, uregs.xbp);

   /* Assertion broken before main() is reached in pthreaded programs;  the
    * offending stack traces only have one item.  --njn, 2002-aug-16 */
   /* vg_assert(fp_min <= fp_max);*/
   // On Darwin, this kicks in for pthread-related stack traces, so they're
   // only 1 entry long which is wrong.
#  if defined(VGO_linux)
   if (fp_min + 512 >= fp_max) {
      /* If the stack limits look bogus, don't poke around ... but
         don't bomb out either. */
#  elif defined(VGO_solaris) || defined(VGO_freebsd)
   if (fp_max == 0) {
      /* VG_(get_StackTrace)() can be called by tools very early when
         various tracing options are enabled. Don't proceed further
         if the stack limits look bogus.
       */
#  endif
#  if defined(VGO_linux) || defined(VGO_solaris) || defined(VGO_freebsd)
      if (sps) sps[0] = uregs.xsp;
      if (fps) fps[0] = uregs.xbp;
      ips[0] = uregs.xip;
      return 1;
   } 
#  endif

   if (UNLIKELY (fp_CF_verif_generation != VG_(debuginfo_generation)())) {
      fp_CF_verif_generation = VG_(debuginfo_generation)();
      VG_(memset)(&fp_CF_verif_cache, 0, sizeof(fp_CF_verif_cache));
      FPO_info_present = VG_(FPO_info_present)();
   }


   /* Loop unwinding the stack. Note that the IP value we get on
    * each pass (whether from CFI info or a stack frame) is a
    * return address so is actually after the calling instruction
    * in the calling function.
    *
    * Because of this we subtract one from the IP after each pass
    * of the loop so that we find the right CFI block on the next
    * pass - otherwise we can find the wrong CFI info if it happens
    * to change after the calling instruction and that will mean
    * that we will fail to unwind the next step.
    *
    * This most frequently happens at the end of a function when
    * a tail call occurs and we wind up using the CFI info for the
    * next function which is completely wrong.
    */
   if (sps) sps[0] = uregs.xsp;
   if (fps) fps[0] = uregs.xbp;
   ips[0] = uregs.xip;
   i = 1;
   if (do_stats) stats.nr++;

   // Does this apply to macOS 10.14 and earlier?
#  if defined(VGO_freebsd) && (FREEBSD_VERS < FREEBSD_13_0)
   if (VG_(is_valid_tid)(tid_if_known) &&
      VG_(is_in_syscall)(tid_if_known) &&
      i < max_n_ips) {
      /* On FreeBSD, all the system call stubs have no function
       * prolog.  So instead of top of the stack being a new
       * frame comprising a saved BP and a return address, we
       * just have the return address in the caller's frame.
       * Adjust for this by recording the return address.
       */
      if (debug)
         VG_(printf)("     in syscall, use XSP-1\n");
      ips[i] = *(Addr *)uregs.xsp - 1;
      if (sps) sps[i] = uregs.xsp;
      if (fps) fps[i] = uregs.xbp;
      i++;
   }
#  endif

   while (True) {

      if (i >= max_n_ips)
         break;

      UWord hash = uregs.xip % N_FP_CF_VERIF;
      Addr xip_verif = uregs.xip ^ fp_CF_verif_cache [hash];
      if (debug)
         VG_(printf)("     uregs.xip 0x%08lx xip_verif[0x%08lx]"
                     " xbp 0x%08lx xsp 0x%08lx\n",
                     uregs.xip, xip_verif,
                     uregs.xbp, uregs.xsp);
      // If xip is in cache, then xip_verif will be <= CFUNWIND.
      // Otherwise, if not in cache, xip_verif will be > CFUNWIND.

      /* Try to derive a new (ip,sp,fp) triple from the current set. */

      /* Do we have to do CFI unwinding ?
         We do CFI unwinding if one of the following condition holds:
         a. fp_CF_verif_cache contains xip but indicates CFUNWIND must
            be done (i.e. fp unwind check failed when we did the first
            unwind for this IP).
         b. fp_CF_verif_cache does not contain xip.
            We will try CFI unwinding in fpverif_uregs and compare with
            FP unwind result to insert xip in the cache with the correct
            indicator. */
      if (UNLIKELY(xip_verif >= CFUNWIND)) {
         if (xip_verif == CFUNWIND) {
            /* case a : do "real" cfi unwind */
            if ( VG_(use_CF_info)( &uregs, fp_min, fp_max ) ) {
               if (debug) unwind_case = "Ca";
               if (do_stats) stats.Ca++;
               goto unwind_done;
            }
            /* ??? cache indicates we have to do CFI unwind (so, we
             previously found CFI info, and failed the fp unwind
             check). Now, we just failed with CFI.  So, once we
             succeed, once we fail.  No idea what is going on =>
             cleanup the cache entry and fallover to fp unwind (this
             time). */
            fp_CF_verif_cache [hash] = 0;
            if (debug) VG_(printf)("     cache reset as CFI ok then nok\n");
            //??? stats
            xip_verif = NOINFO;
         } else {
            /* case b : do "verif" cfi unwind in fpverif_uregs */
            fpverif_uregs = uregs;
            xip_verified = uregs.xip;
            if ( !VG_(use_CF_info)( &fpverif_uregs, fp_min, fp_max ) ) {
               fp_CF_verif_cache [hash] = uregs.xip ^ NOINFO;
               if (debug) VG_(printf)("     cache NOINFO fpverif_uregs\n");
               xip_verif = NOINFO;
            }
         }
      }

      /* On x86, try the old-fashioned method of following the
         %ebp-chain.  This can be done if the fp_CF_verif_cache for xip
         indicate fp unwind is ok. This must be done if the cache indicates
         there is no info. This is also done to confirm what to put in the cache
         if xip was not in the cache. */
      /* This deals with frames resulting from functions which begin "pushl%
         ebp ; movl %esp, %ebp" which is the ABI-mandated preamble. */
      if (fp_min <= uregs.xbp &&
          uregs.xbp <= fp_max - 1 * sizeof(UWord)/*see comment below*/ &&
          VG_IS_4_ALIGNED(uregs.xbp))
      {
         Addr old_xsp;

         /* fp looks sane, so use it. */
         uregs.xip = (((UWord*)uregs.xbp)[1]);
         // We stop if we hit a zero (the traditional end-of-stack
         // marker) or a one -- these correspond to recorded IPs of 0 or -1.
         // The latter because r8818 (in this file) changes the meaning of
         // entries [1] and above in a stack trace, by subtracting 1 from
         // them.  Hence stacks that used to end with a zero value now end in
         // -1 and so we must detect that too.
         if (0 == uregs.xip || 1 == uregs.xip) {
            if (xip_verif > CFUNWIND) {
               // Check if we obtain the same result with fp unwind.
               // If same result, then mark xip as fp unwindable
               if (uregs.xip == fpverif_uregs.xip) {
                  fp_CF_verif_cache [hash] = xip_verified ^ FPUNWIND;
                  if (debug) VG_(printf)("     cache FPUNWIND 0\n");
                  unwind_case = "Fw";
                  if (do_stats) stats.Fw++;
                  break;
               } else {
                  fp_CF_verif_cache [hash] = xip_verified ^ CFUNWIND;
                  uregs = fpverif_uregs;
                  if (debug) VG_(printf)("     cache CFUNWIND 0\n");
                  unwind_case = "Cf";
                  if (do_stats) stats.Cf++;
                  goto unwind_done;
               }
            } else {
               // end of stack => out of the loop.
               break;
            }
         }

         old_xsp = uregs.xsp;
         uregs.xsp = uregs.xbp + sizeof(Addr) /*saved %ebp*/ 
                               + sizeof(Addr) /*ra*/;
         uregs.xbp = (((UWord*)uregs.xbp)[0]);
         if (xip_verif > CFUNWIND) {
            if (uregs.xip == fpverif_uregs.xip
                && uregs.xsp == fpverif_uregs.xsp
                && uregs.xbp == fpverif_uregs.xbp) {
               fp_CF_verif_cache [hash] = xip_verified ^ FPUNWIND;
               if (debug) VG_(printf)("     cache FPUNWIND >2\n");
               if (debug) unwind_case = "FO";
               if (do_stats) stats.FO++;
               if (old_xsp >= uregs.xsp) {
                  if (debug)
                    VG_(printf) ("     FO end of stack old_xsp %p >= xsp %p\n",
                                 (void*)old_xsp, (void*)uregs.xsp);
                  break;
               }
            } else {
               fp_CF_verif_cache [hash] = xip_verified ^ CFUNWIND;
               if (debug) VG_(printf)("     cache CFUNWIND >2\n");
               if (do_stats && uregs.xip != fpverif_uregs.xip) stats.xi++;
               if (do_stats && uregs.xsp != fpverif_uregs.xsp) stats.xs++;
               if (do_stats && uregs.xbp != fpverif_uregs.xbp) stats.xb++;
               uregs = fpverif_uregs;
               if (debug) unwind_case = "CF";
               if (do_stats) stats.CF++;
            }
         } else {
            if (debug) unwind_case = "FF";
            if (do_stats) stats.FF++;
            if (old_xsp >= uregs.xsp) {
               if (debug)
                  VG_(printf) ("     FF end of stack old_xsp %p >= xsp %p\n",
                               (void*)old_xsp, (void*)uregs.xsp);
               break;
            }
         }
         goto unwind_done;
      } else {
         // fp unwind has failed.
         // If we were checking the validity of the cfi unwinding,
         // we mark in the cache that the fp unwind cannot be done, and that
         // cfi unwind is desired.
         if (xip_verif > CFUNWIND) {
            // We know that fpverif_uregs contains valid information,
            // as a failed cf unwind would have put NOINFO in xip_verif.
            fp_CF_verif_cache [hash] = xip_verified ^ CFUNWIND;
            if (debug) VG_(printf)("     cache CFUNWIND as fp failed\n");
            uregs = fpverif_uregs;
            if (debug) unwind_case = "Ck";
            if (do_stats) stats.Ck++;
            goto unwind_done;
         }
         // xip_verif is FPUNWIND or NOINFO.
         // We failed the cfi unwind and/or the fp unwind.
         // => fallback to FPO info.
      }

      /* And, similarly, try for MSVC FPO unwind info. */
      if (FPO_info_present
          && VG_(use_FPO_info)( &uregs.xip, &uregs.xsp, &uregs.xbp,
                                VG_(current_DiEpoch)(),
                                fp_min, fp_max ) ) {
         if (debug) unwind_case = "MS";
         if (do_stats) stats.MS++;
         goto unwind_done;
      }

      /* No luck.  We have to give up. */
      break;

   unwind_done:
      /* Add a frame in ips/sps/fps */
      /* fp is %ebp.  sp is %esp.  ip is %eip. */
      if (0 == uregs.xip || 1 == uregs.xip) break;
      if (sps) sps[i] = uregs.xsp;
      if (fps) fps[i] = uregs.xbp;
      ips[i++] = uregs.xip - 1;
      /* -1: refer to calling insn, not the RA */
      if (debug)
         VG_(printf)("     ips%s[%d]=0x%08lx\n", unwind_case, i-1, ips[i-1]);
      uregs.xip = uregs.xip - 1;
      /* as per comment at the head of this loop */
      RECURSIVE_MERGE(cmrf,ips,i);
   }

   if (do_stats) stats.nf += i;
   if (do_stats && stats.nr % 10000 == 0) {
     VG_(printf)("nr %u nf %u "
                 "Ca %u FF %u "
                 "Cf %u "
                 "Fw %u FO %u "
                 "CF %u (xi %u xs %u xb %u) "
                 "Ck %u MS %u\n",
                 stats.nr, stats.nf,
                 stats.Ca, stats.FF, 
                 stats.Cf,
                 stats.Fw, stats.FO, 
                 stats.CF, stats.xi, stats.xs, stats.xb,
                 stats.Ck, stats.MS);
   }
   n_found = i;
   return n_found;
}

#undef N_FP_CF_VERIF
#undef FPUNWIND
#undef NOINFO
#undef CFUNWIND

#endif

/* ----------------------- amd64 ------------------------ */

#if defined(VGP_amd64_linux) || defined(VGP_amd64_darwin) \
    || defined(VGP_amd64_solaris) || defined(VGP_amd64_freebsd)

UInt VG_(get_StackTrace_wrk) ( ThreadId tid_if_known,
                               /*OUT*/Addr* ips, UInt max_n_ips,
                               /*OUT*/Addr* sps, /*OUT*/Addr* fps,
                               const UnwindStartRegs* startRegs,
                               Addr fp_max_orig )
{
   const Bool  debug = False;
   Int   i;
   Addr  fp_max;
   UInt  n_found = 0;
   const Int cmrf = VG_(clo_merge_recursive_frames);

   vg_assert(sizeof(Addr) == sizeof(UWord));
   vg_assert(sizeof(Addr) == sizeof(void*));

   D3UnwindRegs uregs;
   uregs.xip = startRegs->r_pc;
   uregs.xsp = startRegs->r_sp;
   uregs.xbp = startRegs->misc.AMD64.r_rbp;
   Addr fp_min = uregs.xsp - VG_STACK_REDZONE_SZB;

   /* Snaffle IPs from the client's stack into ips[0 .. max_n_ips-1],
      stopping when the trail goes cold, which we guess to be
      when FP is not a reasonable stack location. */

   // JRS 2002-sep-17: hack, to round up fp_max to the end of the
   // current page, at least.  Dunno if it helps.
   // NJN 2002-sep-17: seems to -- stack traces look like 1.0.X again
   fp_max = VG_PGROUNDUP(fp_max_orig);
   if (fp_max >= sizeof(Addr))
      fp_max -= sizeof(Addr);

   if (debug)
      VG_(printf)("max_n_ips=%u fp_min=0x%lx fp_max_orig=0x%lx, "
                  "fp_max=0x%lx ip=0x%lx fp=0x%lx\n",
                  max_n_ips, fp_min, fp_max_orig, fp_max,
                  uregs.xip, uregs.xbp);

   /* Assertion broken before main() is reached in pthreaded programs;  the
    * offending stack traces only have one item.  --njn, 2002-aug-16 */
   /* vg_assert(fp_min <= fp_max);*/
   // On Darwin, this kicks in for pthread-related stack traces, so they're
   // only 1 entry long which is wrong.
#  if defined(VGO_linux)
   if (fp_min + 256 >= fp_max) {
      /* If the stack limits look bogus, don't poke around ... but
         don't bomb out either. */
#  elif defined(VGO_solaris)
   if (fp_max == 0) {
      /* VG_(get_StackTrace)() can be called by tools very early when
         various tracing options are enabled. Don't proceed further
         if the stack limits look bogus.
       */
#  endif
#  if defined(VGO_linux) || defined(VGO_solaris)

      if (sps) sps[0] = uregs.xsp;
      if (fps) fps[0] = uregs.xbp;
      ips[0] = uregs.xip;
      return 1;
   } 
#  endif

   /* fp is %rbp.  sp is %rsp.  ip is %rip. */

   ips[0] = uregs.xip;
   if (sps) sps[0] = uregs.xsp;
   if (fps) fps[0] = uregs.xbp;
   i = 1;
   if (debug)
      VG_(printf)("     ipsS[%d]=%#08lx rbp %#08lx rsp %#08lx\n",
                  i-1, ips[i-1], uregs.xbp, uregs.xsp);

#  if defined(VGO_darwin) || (defined(VGO_freebsd) && (FREEBSD_VERS < FREEBSD_13_0))
   if (VG_(is_valid_tid)(tid_if_known) &&
      VG_(is_in_syscall)(tid_if_known) &&
      i < max_n_ips) {
      /* On Darwin and FreeBSD, all the system call stubs have no function
       * prolog.  So instead of top of the stack being a new
       * frame comprising a saved BP and a return address, we
       * just have the return address in the caller's frame.
       * Adjust for this by recording the return address.
       */
      if (debug)
         VG_(printf)("     in syscall, use XSP-1\n");
      ips[i] = *(Addr *)uregs.xsp - 1;
      if (sps) sps[i] = uregs.xsp;
      if (fps) fps[i] = uregs.xbp;
      i++;
   }
#  endif
       
   /* Loop unwinding the stack. Note that the IP value we get on
    * each pass (whether from CFI info or a stack frame) is a
    * return address so is actually after the calling instruction
    * in the calling function.
    *
    * Because of this we subtract one from the IP after each pass
    * of the loop so that we find the right CFI block on the next
    * pass - otherwise we can find the wrong CFI info if it happens
    * to change after the calling instruction and that will mean
    * that we will fail to unwind the next step.
    *
    * This most frequently happens at the end of a function when
    * a tail call occurs and we wind up using the CFI info for the
    * next function which is completely wrong.
    */
   while (True) {
      Addr old_xsp;

      if (i >= max_n_ips)
         break;

      old_xsp = uregs.xsp;

      /* Try to derive a new (ip,sp,fp) triple from the current set. */

      /* First off, see if there is any CFI info to hand which can
         be used. */
      if ( VG_(use_CF_info)( &uregs, fp_min, fp_max ) ) {
         if (0 == uregs.xip || 1 == uregs.xip) break;
         if (old_xsp >= uregs.xsp) {
            if (debug)
               VG_(printf) ("     CF end of stack old_xsp %p >= xsp %p\n",
                            (void*)old_xsp, (void*)uregs.xsp);
            break;
         }
         if (sps) sps[i] = uregs.xsp;
         if (fps) fps[i] = uregs.xbp;
         ips[i++] = uregs.xip - 1; /* -1: refer to calling insn, not the RA */
         if (debug)
            VG_(printf)("     ipsC[%d]=%#08lx rbp %#08lx rsp %#08lx\n",
                        i-1, ips[i-1], uregs.xbp, uregs.xsp);
         uregs.xip = uregs.xip - 1; /* as per comment at the head of this loop */
         RECURSIVE_MERGE(cmrf,ips,i);
         continue;
      }

      /* If VG_(use_CF_info) fails, it won't modify ip/sp/fp, so
         we can safely try the old-fashioned method. */
      /* This bit is supposed to deal with frames resulting from
         functions which begin "pushq %rbp ; movq %rsp, %rbp".
         Unfortunately, since we can't (easily) look at the insns at
         the start of the fn, like GDB does, there's no reliable way
         to tell.  Hence the hack of first trying out CFI, and if that
         fails, then use this as a fallback. */
      /* Note: re "- 1 * sizeof(UWord)", need to take account of the
         fact that we are prodding at & ((UWord*)fp)[1] and so need to
         adjust the limit check accordingly.  Omitting this has been
         observed to cause segfaults on rare occasions. */
      if (fp_min <= uregs.xbp && uregs.xbp <= fp_max - 1 * sizeof(UWord)) {
         /* fp looks sane, so use it. */
         uregs.xip = (((UWord*)uregs.xbp)[1]);
         if (0 == uregs.xip || 1 == uregs.xip) break;
         uregs.xsp = uregs.xbp + sizeof(Addr) /*saved %rbp*/ 
                               + sizeof(Addr) /*ra*/;
         if (old_xsp >= uregs.xsp) {
            if (debug)
               VG_(printf) ("     FF end of stack old_xsp %p >= xsp %p\n",
                            (void*)old_xsp, (void*)uregs.xsp);
            break;
         }
         uregs.xbp = (((UWord*)uregs.xbp)[0]);
         if (sps) sps[i] = uregs.xsp;
         if (fps) fps[i] = uregs.xbp;
         ips[i++] = uregs.xip - 1; /* -1: refer to calling insn, not the RA */
         if (debug)
            VG_(printf)("     ipsF[%d]=%#08lx rbp %#08lx rsp %#08lx\n",
                        i-1, ips[i-1], uregs.xbp, uregs.xsp);
         uregs.xip = uregs.xip - 1; /* as per comment at the head of this loop */
         RECURSIVE_MERGE(cmrf,ips,i);
         continue;
      }

      /* Last-ditch hack (evidently GDB does something similar).  We
         are in the middle of nowhere and we have a nonsense value for
         the frame pointer.  If the stack pointer is still valid,
         assume that what it points at is a return address.  Yes,
         desperate measures.  Could do better here:
         - check that the supposed return address is in
           an executable page
         - check that the supposed return address is just after a call insn
         - given those two checks, don't just consider *sp as the return 
           address; instead scan a likely section of stack (eg sp .. sp+256)
           and use suitable values found there.
      */
      if (fp_min <= uregs.xsp && uregs.xsp < fp_max) {
         uregs.xip = ((UWord*)uregs.xsp)[0];
         if (0 == uregs.xip || 1 == uregs.xip) break;
         if (sps) sps[i] = uregs.xsp;
         if (fps) fps[i] = uregs.xbp;
         ips[i++] = uregs.xip == 0 
                    ? 0 /* sp[0] == 0 ==> stuck at the bottom of a
                           thread stack */
                    : uregs.xip - 1;
                        /* -1: refer to calling insn, not the RA */
         if (debug)
            VG_(printf)("     ipsH[%d]=%#08lx\n", i-1, ips[i-1]);
         uregs.xip = uregs.xip - 1; /* as per comment at the head of this loop */
         uregs.xsp += 8;
         RECURSIVE_MERGE(cmrf,ips,i);
         continue;
      }

      /* No luck at all.  We have to give up. */
      break;
   }

   n_found = i;
   return n_found;
}

#endif

/* -----------------------ppc32/64 ---------------------- */

#if defined(VGP_ppc32_linux) || defined(VGP_ppc64be_linux) \
    || defined(VGP_ppc64le_linux)

UInt VG_(get_StackTrace_wrk) ( ThreadId tid_if_known,
                               /*OUT*/Addr* ips, UInt max_n_ips,
                               /*OUT*/Addr* sps, /*OUT*/Addr* fps,
                               const UnwindStartRegs* startRegs,
                               Addr fp_max_orig )
{
   Bool  lr_is_first_RA = False;
#  if defined(VG_PLAT_USES_PPCTOC) || defined(VGP_ppc64le_linux)
   Word redir_stack_size = 0;
   Word redirs_used      = 0;
#  endif
   const Int cmrf = VG_(clo_merge_recursive_frames);
   const DiEpoch cur_ep = VG_(current_DiEpoch)();

   Bool  debug = False;
   Int   i;
   Addr  fp_max;
   UInt  n_found = 0;

   vg_assert(sizeof(Addr) == sizeof(UWord));
   vg_assert(sizeof(Addr) == sizeof(void*));

   Addr ip = (Addr)startRegs->r_pc;
   Addr sp = (Addr)startRegs->r_sp;
   Addr fp = sp;
#  if defined(VGP_ppc32_linux)
   Addr lr = startRegs->misc.PPC32.r_lr;
#  elif defined(VGP_ppc64be_linux) || defined(VGP_ppc64le_linux)
   Addr lr = startRegs->misc.PPC64.r_lr;
#  endif
   Addr fp_min = sp - VG_STACK_REDZONE_SZB;

   /* Snaffle IPs from the client's stack into ips[0 .. max_n_ips-1],
      stopping when the trail goes cold, which we guess to be
      when FP is not a reasonable stack location. */

   // JRS 2002-sep-17: hack, to round up fp_max to the end of the
   // current page, at least.  Dunno if it helps.
   // NJN 2002-sep-17: seems to -- stack traces look like 1.0.X again
   fp_max = VG_PGROUNDUP(fp_max_orig);
   if (fp_max >= sizeof(Addr))
      fp_max -= sizeof(Addr);

   if (debug)
      VG_(printf)("max_n_ips=%u fp_min=0x%lx fp_max_orig=0x%lx, "
                  "fp_max=0x%lx ip=0x%lx fp=0x%lx\n",
		  max_n_ips, fp_min, fp_max_orig, fp_max, ip, fp);

   /* Assertion broken before main() is reached in pthreaded programs;  the
    * offending stack traces only have one item.  --njn, 2002-aug-16 */
   /* vg_assert(fp_min <= fp_max);*/
   if (fp_min + 512 >= fp_max) {
      /* If the stack limits look bogus, don't poke around ... but
         don't bomb out either. */
      if (sps) sps[0] = sp;
      if (fps) fps[0] = fp;
      ips[0] = ip;
      return 1;
   } 

   /* fp is %r1.  ip is %cia.  Note, ppc uses r1 as both the stack and
      frame pointers. */

#  if defined(VGP_ppc64be_linux) || defined(VGP_ppc64le_linux)
   redir_stack_size = VEX_GUEST_PPC64_REDIR_STACK_SIZE;
   redirs_used      = 0;
#  endif

#  if defined(VG_PLAT_USES_PPCTOC) || defined (VGP_ppc64le_linux)
   /* Deal with bogus LR values caused by function
      interception/wrapping on ppc-TOC platforms; see comment on
      similar code a few lines further down. */
   if (lr == (Addr)&VG_(ppctoc_magic_redirect_return_stub)
       && VG_(is_valid_tid)(tid_if_known)) {
      Word hsp = VG_(threads)[tid_if_known].arch.vex.guest_REDIR_SP;
      redirs_used++;
      if (hsp >= 1 && hsp < redir_stack_size)
         lr = VG_(threads)[tid_if_known]
                 .arch.vex.guest_REDIR_STACK[hsp-1];
   }
#  endif

   /* We have to determine whether or not LR currently holds this fn
      (call it F)'s return address.  It might not if F has previously
      called some other function, hence overwriting LR with a pointer
      to some part of F.  Hence if LR and IP point to the same
      function then we conclude LR does not hold this function's
      return address; instead the LR at entry must have been saved in
      the stack by F's prologue and so we must get it from there
      instead.  Note all this guff only applies to the innermost
      frame. */
   lr_is_first_RA = False;
   {
      const HChar *buf_lr, *buf_ip;
      /* The following conditional looks grossly inefficient and
         surely could be majorly improved, with not much effort. */
      if (VG_(get_fnname_raw) (cur_ep, lr, &buf_lr)) {
         HChar buf_lr_copy[VG_(strlen)(buf_lr) + 1];
         VG_(strcpy)(buf_lr_copy, buf_lr);
         if (VG_(get_fnname_raw) (cur_ep, ip, &buf_ip))
            if (VG_(strcmp)(buf_lr_copy, buf_ip))
               lr_is_first_RA = True;
      }
   }

   if (sps) sps[0] = fp; /* NB. not sp */
   if (fps) fps[0] = fp;
   ips[0] = ip;
   i = 1;

   if (fp_min <= fp && fp < fp_max-VG_WORDSIZE+1) {

      /* initial FP is sane; keep going */
      fp = (((UWord*)fp)[0]);

      while (True) {

        /* On ppc64-linux (ppc64-elf, really), the lr save
           slot is 2 words back from sp, whereas on ppc32-elf(?) it's
           only one word back. */
#        if defined(VG_PLAT_USES_PPCTOC) || defined(VGP_ppc64le_linux)
         const Int lr_offset = 2;
#        else
         const Int lr_offset = 1;
#        endif

         if (i >= max_n_ips)
            break;

         /* Try to derive a new (ip,fp) pair from the current set. */

         if (fp_min <= fp && fp <= fp_max - lr_offset * sizeof(UWord)) {
            /* fp looks sane, so use it. */

            if (i == 1 && lr_is_first_RA)
               ip = lr;
            else
               ip = (((UWord*)fp)[lr_offset]);

#           if defined(VG_PLAT_USES_PPCTOC) || defined(VGP_ppc64le_linux)
            /* Nasty hack to do with function replacement/wrapping on
               ppc64-linux.  If LR points to our magic return stub,
               then we are in a wrapped or intercepted function, in
               which LR has been messed with.  The original LR will
               have been pushed onto the thread's hidden REDIR stack
               one down from the top (top element is the saved R2) and
               so we should restore the value from there instead.
               Since nested redirections can and do happen, we keep
               track of the number of nested LRs used by the unwinding
               so far with 'redirs_used'. */
            if (ip == (Addr)&VG_(ppctoc_magic_redirect_return_stub)
                && VG_(is_valid_tid)(tid_if_known)) {
               Word hsp = VG_(threads)[tid_if_known]
                             .arch.vex.guest_REDIR_SP;
               hsp -= 2 * redirs_used;
               redirs_used ++;
               if (hsp >= 1 && hsp < redir_stack_size)
                  ip = VG_(threads)[tid_if_known]
                          .arch.vex.guest_REDIR_STACK[hsp-1];
            }
#           endif

            if (0 == ip || 1 == ip) break;
            if (sps) sps[i] = fp; /* NB. not sp */
            if (fps) fps[i] = fp;
            fp = (((UWord*)fp)[0]);
            ips[i++] = ip - 1; /* -1: refer to calling insn, not the RA */
            if (debug)
               VG_(printf)("     ipsF[%d]=%#08lx\n", i-1, ips[i-1]);
            ip = ip - 1; /* ip is probably dead at this point, but
                            play safe, a la x86/amd64 above.  See
                            extensive comments above. */
            RECURSIVE_MERGE(cmrf,ips,i);
            continue;
         }

         /* No luck there.  We have to give up. */
         break;
      }
   }

   n_found = i;
   return n_found;
}

#endif

/* ------------------------ arm ------------------------- */

#if defined(VGP_arm_linux)

static Bool in_same_fn ( Addr a1, Addr a2 )
{
   const HChar *buf_a1, *buf_a2;
   /* The following conditional looks grossly inefficient and
      surely could be majorly improved, with not much effort. */
   const DiEpoch cur_ep = VG_(current_DiEpoch)();
   if (VG_(get_fnname_raw) (cur_ep, a1, &buf_a1)) {
      HChar buf_a1_copy[VG_(strlen)(buf_a1) + 1];
      VG_(strcpy)(buf_a1_copy, buf_a1);
      if (VG_(get_fnname_raw) (cur_ep, a2, &buf_a2))
         if (VG_(strcmp)(buf_a1_copy, buf_a2))
            return True;
   }
   return False;
}

static Bool in_same_page ( Addr a1, Addr a2 ) {
   return (a1 & ~0xFFF) == (a2 & ~0xFFF);
}

static Addr abs_diff ( Addr a1, Addr a2 ) {
   return (Addr)(a1 > a2 ? a1 - a2 : a2 - a1);
}

static Bool has_XT_perms ( Addr a )
{
   NSegment const* seg = VG_(am_find_nsegment)(a);
   return seg && seg->hasX && seg->hasT;
}

static Bool looks_like_Thumb_call32 ( UShort w0, UShort w1 )
{
   if (0)
      VG_(printf)("isT32call %04x %04x\n", (UInt)w0, (UInt)w1);
   // BL  simm26 
   if ((w0 & 0xF800) == 0xF000 && (w1 & 0xC000) == 0xC000) return True;
   // BLX simm26
   if ((w0 & 0xF800) == 0xF000 && (w1 & 0xC000) == 0xC000) return True;
   return False;
}

static Bool looks_like_Thumb_call16 ( UShort w0 )
{
   return False;
}

static Bool looks_like_ARM_call ( UInt a0 )
{
   if (0)
      VG_(printf)("isA32call %08x\n", a0);
   // Leading E forces unconditional only -- fix
   if ((a0 & 0xFF000000) == 0xEB000000) return True;
   return False;
}

static Bool looks_like_RA ( Addr ra )
{
   /* 'ra' is a plausible return address if it points to
       an instruction after a call insn. */
   Bool isT = (ra & 1);
   if (isT) {
      // returning to Thumb code
      ra &= ~1;
      ra -= 4;
      if (has_XT_perms(ra)) {
         UShort w0 = *(UShort*)ra;
         UShort w1 = in_same_page(ra, ra+2) ? *(UShort*)(ra+2) : 0;
         if (looks_like_Thumb_call16(w1) || looks_like_Thumb_call32(w0,w1))
            return True;
      }
   } else {
      // ARM
      ra &= ~3;
      ra -= 4;
      if (has_XT_perms(ra)) {
         UInt a0 = *(UInt*)ra;
         if (looks_like_ARM_call(a0))
            return True;
      }
   }
   return False;
}

UInt VG_(get_StackTrace_wrk) ( ThreadId tid_if_known,
                               /*OUT*/Addr* ips, UInt max_n_ips,
                               /*OUT*/Addr* sps, /*OUT*/Addr* fps,
                               const UnwindStartRegs* startRegs,
                               Addr fp_max_orig )
{
   Bool  debug = False;
   Int   i;
   Addr  fp_max;
   UInt  n_found = 0;
   const Int cmrf = VG_(clo_merge_recursive_frames);

   vg_assert(sizeof(Addr) == sizeof(UWord));
   vg_assert(sizeof(Addr) == sizeof(void*));

   D3UnwindRegs uregs;
   uregs.r15 = startRegs->r_pc & 0xFFFFFFFE;
   uregs.r14 = startRegs->misc.ARM.r14;
   uregs.r13 = startRegs->r_sp;
   uregs.r12 = startRegs->misc.ARM.r12;
   uregs.r11 = startRegs->misc.ARM.r11;
   uregs.r7  = startRegs->misc.ARM.r7;
   Addr fp_min = uregs.r13 - VG_STACK_REDZONE_SZB;

   /* Snaffle IPs from the client's stack into ips[0 .. max_n_ips-1],
      stopping when the trail goes cold, which we guess to be
      when FP is not a reasonable stack location. */

   // JRS 2002-sep-17: hack, to round up fp_max to the end of the
   // current page, at least.  Dunno if it helps.
   // NJN 2002-sep-17: seems to -- stack traces look like 1.0.X again
   fp_max = VG_PGROUNDUP(fp_max_orig);
   if (fp_max >= sizeof(Addr))
      fp_max -= sizeof(Addr);

   if (debug)
      VG_(printf)("\nmax_n_ips=%u fp_min=0x%lx fp_max_orig=0x%lx, "
                  "fp_max=0x%lx r15=0x%lx r13=0x%lx\n",
                  max_n_ips, fp_min, fp_max_orig, fp_max,
                  uregs.r15, uregs.r13);

   /* Assertion broken before main() is reached in pthreaded programs;  the
    * offending stack traces only have one item.  --njn, 2002-aug-16 */
   /* vg_assert(fp_min <= fp_max);*/
   // On Darwin, this kicks in for pthread-related stack traces, so they're
   // only 1 entry long which is wrong.
   if (fp_min + 512 >= fp_max) {
      /* If the stack limits look bogus, don't poke around ... but
         don't bomb out either. */
      if (sps) sps[0] = uregs.r13;
      if (fps) fps[0] = 0;
      ips[0] = uregs.r15;
      return 1;
   } 

   /* */

   if (sps) sps[0] = uregs.r13;
   if (fps) fps[0] = 0;
   ips[0] = uregs.r15;
   i = 1;

   /* Loop unwinding the stack. */
   Bool do_stack_scan = False;

   /* First try the Official Way, using Dwarf CFI. */
   while (True) {
      if (debug) {
         VG_(printf)("i: %d, r15: 0x%lx, r13: 0x%lx\n",
                     i, uregs.r15, uregs.r13);
      }

      if (i >= max_n_ips)
         break;

      if (VG_(use_CF_info)( &uregs, fp_min, fp_max )) {
         if (sps) sps[i] = uregs.r13;
         if (fps) fps[i] = 0;
         ips[i++] = (uregs.r15 & 0xFFFFFFFE) - 1;
         if (debug)
            VG_(printf)("USING CFI: r15: 0x%lx, r13: 0x%lx\n",
                        uregs.r15, uregs.r13);
         uregs.r15 = (uregs.r15 & 0xFFFFFFFE) - 1;
         RECURSIVE_MERGE(cmrf,ips,i);
         continue;
      }

      /* No luck.  We have to give up. */
      do_stack_scan = True;
      break;
   }

   /* Now try Plan B (maybe) -- stack scanning.  This often gives
      pretty bad results, so this has to be enabled explicitly by the
      user. */
   if (do_stack_scan
       && i < max_n_ips && i < (Int)VG_(clo_unw_stack_scan_thresh)) {
      Int  nByStackScan = 0;
      Addr lr = uregs.r14;
      Addr sp = uregs.r13 & ~3;
      Addr pc = uregs.r15;
      // First see if LR contains
      // something that could be a valid return address.
      if (!in_same_fn(lr, pc) && looks_like_RA(lr)) {
         // take it only if 'cand' isn't obviously a duplicate
         // of the last found IP value
         Addr cand = (lr & 0xFFFFFFFE) - 1;
         if (abs_diff(cand, ips[i-1]) > 1) {
            if (sps) sps[i] = 0;
            if (fps) fps[i] = 0;
            ips[i++] = cand;
            RECURSIVE_MERGE(cmrf,ips,i);
            nByStackScan++;
         }
      }
      while (in_same_page(sp, uregs.r13)) {
         if (i >= max_n_ips)
            break;
         // we're in the same page; fairly safe to keep going
         UWord w = *(UWord*)(sp & ~0x3);
         if (looks_like_RA(w)) {
            Addr cand = (w & 0xFFFFFFFE) - 1;
            // take it only if 'cand' isn't obviously a duplicate
            // of the last found IP value
            if (abs_diff(cand, ips[i-1]) > 1) {
               if (sps) sps[i] = 0;
               if (fps) fps[i] = 0;
               ips[i++] = cand;
               RECURSIVE_MERGE(cmrf,ips,i);
               if (++nByStackScan >= VG_(clo_unw_stack_scan_frames)) break;
            }
         }
         sp += 4;
      }
   }

   n_found = i;
   return n_found;
}

#endif

/* ------------------------ arm64 ------------------------- */

#if defined(VGP_arm64_linux)

UInt VG_(get_StackTrace_wrk) ( ThreadId tid_if_known,
                               /*OUT*/Addr* ips, UInt max_n_ips,
                               /*OUT*/Addr* sps, /*OUT*/Addr* fps,
                               const UnwindStartRegs* startRegs,
                               Addr fp_max_orig )
{
   Bool  debug = False;
   Int   i;
   Addr  fp_max;
   UInt  n_found = 0;
   const Int cmrf = VG_(clo_merge_recursive_frames);

   vg_assert(sizeof(Addr) == sizeof(UWord));
   vg_assert(sizeof(Addr) == sizeof(void*));

   D3UnwindRegs uregs;
   uregs.pc = startRegs->r_pc;
   uregs.sp = startRegs->r_sp;
   uregs.x30 = startRegs->misc.ARM64.x30;
   uregs.x29 = startRegs->misc.ARM64.x29;
   Addr fp_min = uregs.sp - VG_STACK_REDZONE_SZB;

   /* Snaffle IPs from the client's stack into ips[0 .. max_n_ips-1],
      stopping when the trail goes cold, which we guess to be
      when FP is not a reasonable stack location. */

   // JRS 2002-sep-17: hack, to round up fp_max to the end of the
   // current page, at least.  Dunno if it helps.
   // NJN 2002-sep-17: seems to -- stack traces look like 1.0.X again
   fp_max = VG_PGROUNDUP(fp_max_orig);
   if (fp_max >= sizeof(Addr))
      fp_max -= sizeof(Addr);

   if (debug)
      VG_(printf)("\nmax_n_ips=%u fp_min=0x%lx fp_max_orig=0x%lx, "
                  "fp_max=0x%lx PC=0x%lx SP=0x%lx\n",
                  max_n_ips, fp_min, fp_max_orig, fp_max,
                  uregs.pc, uregs.sp);

   /* Assertion broken before main() is reached in pthreaded programs;  the
    * offending stack traces only have one item.  --njn, 2002-aug-16 */
   /* vg_assert(fp_min <= fp_max);*/
   // On Darwin, this kicks in for pthread-related stack traces, so they're
   // only 1 entry long which is wrong.
   if (fp_min + 512 >= fp_max) {
      /* If the stack limits look bogus, don't poke around ... but
         don't bomb out either. */
      if (sps) sps[0] = uregs.sp;
      if (fps) fps[0] = uregs.x29;
      ips[0] = uregs.pc;
      return 1;
   } 

   /* */

   if (sps) sps[0] = uregs.sp;
   if (fps) fps[0] = uregs.x29;
   ips[0] = uregs.pc;
   i = 1;

   /* Loop unwinding the stack, using CFI. */
   while (True) {
      if (debug) {
         VG_(printf)("i: %d, pc: 0x%lx, sp: 0x%lx\n",
                     i, uregs.pc, uregs.sp);
      }

      if (i >= max_n_ips)
         break;

      if (VG_(use_CF_info)( &uregs, fp_min, fp_max )) {
         if (sps) sps[i] = uregs.sp;
         if (fps) fps[i] = uregs.x29;
         ips[i++] = uregs.pc - 1;
         if (debug)
            VG_(printf)("USING CFI: pc: 0x%lx, sp: 0x%lx\n",
                        uregs.pc, uregs.sp);
         uregs.pc = uregs.pc - 1;
         RECURSIVE_MERGE(cmrf,ips,i);
         continue;
      }

      /* No luck.  We have to give up. */
      break;
   }

   n_found = i;
   return n_found;
}

#endif

/* ------------------------ s390x ------------------------- */

#if defined(VGP_s390x_linux)

UInt VG_(get_StackTrace_wrk) ( ThreadId tid_if_known,
                               /*OUT*/Addr* ips, UInt max_n_ips,
                               /*OUT*/Addr* sps, /*OUT*/Addr* fps,
                               const UnwindStartRegs* startRegs,
                               Addr fp_max_orig )
{
   Bool  debug = False;
   Int   i;
   Addr  fp_max;
   UInt  n_found = 0;
   const Int cmrf = VG_(clo_merge_recursive_frames);

   vg_assert(sizeof(Addr) == sizeof(UWord));
   vg_assert(sizeof(Addr) == sizeof(void*));

   D3UnwindRegs uregs;
   uregs.ia = startRegs->r_pc;
   uregs.sp = startRegs->r_sp;
   Addr fp_min = uregs.sp - VG_STACK_REDZONE_SZB;
   uregs.fp = startRegs->misc.S390X.r_fp;
   uregs.lr = startRegs->misc.S390X.r_lr;
   uregs.f0 = startRegs->misc.S390X.r_f0;
   uregs.f1 = startRegs->misc.S390X.r_f1;
   uregs.f2 = startRegs->misc.S390X.r_f2;
   uregs.f3 = startRegs->misc.S390X.r_f3;
   uregs.f4 = startRegs->misc.S390X.r_f4;
   uregs.f5 = startRegs->misc.S390X.r_f5;
   uregs.f6 = startRegs->misc.S390X.r_f6;
   uregs.f7 = startRegs->misc.S390X.r_f7;

   fp_max = VG_PGROUNDUP(fp_max_orig);
   if (fp_max >= sizeof(Addr))
      fp_max -= sizeof(Addr);

   if (debug)
      VG_(printf)("max_n_ips=%u fp_min=0x%lx fp_max_orig=0x%lx, "
                  "fp_max=0x%lx IA=0x%lx SP=0x%lx FP=0x%lx\n",
                  max_n_ips, fp_min, fp_max_orig, fp_max,
                  uregs.ia, uregs.sp,uregs.fp);

   /* The first frame is pretty obvious */
   ips[0] = uregs.ia;
   if (sps) sps[0] = uregs.sp;
   if (fps) fps[0] = uregs.fp;
   i = 1;

   /* for everything else we have to rely on the eh_frame. gcc defaults to
      not create a backchain and all the other  tools (like gdb) also have
      to use the CFI. */
   while (True) {
      if (i >= max_n_ips)
         break;

      if (VG_(use_CF_info)( &uregs, fp_min, fp_max )) {
         if (sps) sps[i] = uregs.sp;
         if (fps) fps[i] = uregs.fp;
         ips[i++] = uregs.ia - 1;
         uregs.ia = uregs.ia - 1;
         RECURSIVE_MERGE(cmrf,ips,i);
         continue;
      }
      /* A problem on the first frame? Lets assume it was a bad jump.
         We will use the link register and the current stack and frame
         pointers and see if we can use the CFI in the next round. */
      if (i == 1) {
         if (sps) {
            sps[i] = sps[0];
            uregs.sp = sps[0];
         }
         if (fps) {
            fps[i] = fps[0];
            uregs.fp = fps[0];
         }
         uregs.ia = uregs.lr - 1;
         ips[i++] = uregs.lr - 1;
         RECURSIVE_MERGE(cmrf,ips,i);
         continue;
      }

      /* No luck.  We have to give up. */
      break;
   }

   n_found = i;
   return n_found;
}

#endif

/* ------------------------ mips 32/64 ------------------------- */
#if defined(VGP_mips32_linux) || defined(VGP_mips64_linux) \
 || defined(VGP_nanomips_linux)
UInt VG_(get_StackTrace_wrk) ( ThreadId tid_if_known,
                               /*OUT*/Addr* ips, UInt max_n_ips,
                               /*OUT*/Addr* sps, /*OUT*/Addr* fps,
                               const UnwindStartRegs* startRegs,
                               Addr fp_max_orig )
{
   Bool  debug = False;
   Int   i;
   Addr  fp_max;
   UInt  n_found = 0;
   const Int cmrf = VG_(clo_merge_recursive_frames);

   vg_assert(sizeof(Addr) == sizeof(UWord));
   vg_assert(sizeof(Addr) == sizeof(void*));

   D3UnwindRegs uregs;
   uregs.pc = startRegs->r_pc;
   uregs.sp = startRegs->r_sp;
   Addr fp_min = uregs.sp - VG_STACK_REDZONE_SZB;

#if defined(VGP_mips32_linux) || defined(VGP_nanomips_linux)
   uregs.fp = startRegs->misc.MIPS32.r30;
   uregs.ra = startRegs->misc.MIPS32.r31;
#elif defined(VGP_mips64_linux)
   uregs.fp = startRegs->misc.MIPS64.r30;
   uregs.ra = startRegs->misc.MIPS64.r31;
#endif

   /* Snaffle IPs from the client's stack into ips[0 .. max_n_ips-1],
      stopping when the trail goes cold, which we guess to be
      when FP is not a reasonable stack location. */

   fp_max = VG_PGROUNDUP(fp_max_orig);
   if (fp_max >= sizeof(Addr))
      fp_max -= sizeof(Addr);

   if (debug)
      VG_(printf)("max_n_ips=%u fp_min=0x%lx fp_max_orig=0x%lx, "
                  "fp_max=0x%lx pc=0x%lx sp=0x%lx fp=0x%lx\n",
                  max_n_ips, fp_min, fp_max_orig, fp_max,
                  uregs.pc, uregs.sp, uregs.fp);

   if (sps) sps[0] = uregs.sp;
   if (fps) fps[0] = uregs.fp;
   ips[0] = uregs.pc;
   i = 1;

   /* Loop unwinding the stack. */

   while (True) {
      if (debug) {
         VG_(printf)("i: %d, pc: 0x%lx, sp: 0x%lx, ra: 0x%lx\n",
                     i, uregs.pc, uregs.sp, uregs.ra);
      }
      if (i >= max_n_ips)
         break;

      D3UnwindRegs uregs_copy = uregs;
      if (VG_(use_CF_info)( &uregs, fp_min, fp_max )) {
         if (debug)
            VG_(printf)("USING CFI: pc: 0x%lx, sp: 0x%lx, ra: 0x%lx\n",
                        uregs.pc, uregs.sp, uregs.ra);
         if (0 != uregs.pc && 1 != uregs.pc) {
            if (sps) sps[i] = uregs.sp;
            if (fps) fps[i] = uregs.fp;
            ips[i++] = uregs.pc - 4;
            uregs.pc = uregs.pc - 4;
            RECURSIVE_MERGE(cmrf,ips,i);
            continue;
         } else
            uregs = uregs_copy;
      }

      int seen_sp_adjust = 0;
      long frame_offset = 0;
      PtrdiffT offset;
      const DiEpoch cur_ep = VG_(current_DiEpoch)();
      if (VG_(get_inst_offset_in_function)(cur_ep, uregs.pc, &offset)) {
         Addr start_pc = uregs.pc - offset;
         Addr limit_pc = uregs.pc;
         Addr cur_pc;
         for (cur_pc = start_pc; cur_pc < limit_pc; cur_pc += 4) {
            unsigned long inst, high_word, low_word;
            unsigned long * cur_inst;
            /* Fetch the instruction.   */
            cur_inst = (unsigned long *)cur_pc;
            inst = *((UInt *) cur_inst);
            if(debug)
               VG_(printf)("cur_pc: 0x%lx, inst: 0x%lx\n", cur_pc, inst);

            /* Save some code by pre-extracting some useful fields.  */
            high_word = (inst >> 16) & 0xffff;
            low_word = inst & 0xffff;

            if (high_word == 0x27bd        /* addiu $sp,$sp,-i */
                || high_word == 0x23bd     /* addi $sp,$sp,-i */
                || high_word == 0x67bd) {  /* daddiu $sp,$sp,-i */
               if (low_word & 0x8000)	/* negative stack adjustment? */
                  frame_offset += 0x10000 - low_word;
               else
                  /* Exit loop if a positive stack adjustment is found, which
                     usually means that the stack cleanup code in the function
                     epilogue is reached.  */
               break;
            seen_sp_adjust = 1;
            }
         }
         if(debug)
            VG_(printf)("offset: 0x%ld\n", frame_offset);
      }
      if (seen_sp_adjust) {
         if (0 == uregs.pc || 1 == uregs.pc) break;
         if (uregs.pc == uregs.ra - 8) break;
         if (sps) {
            sps[i] = uregs.sp + frame_offset;
         }
         uregs.sp = uregs.sp + frame_offset;
         
         if (fps) {
            fps[i] = fps[0];
            uregs.fp = fps[0];
         }
         if (0 == uregs.ra || 1 == uregs.ra) break;
         uregs.pc = uregs.ra - 8;
         ips[i++] = uregs.ra - 8;
         RECURSIVE_MERGE(cmrf,ips,i);
         continue;
      }

      if (i == 1) {
         if (sps) {
            sps[i] = sps[0];
            uregs.sp = sps[0];
         }
         if (fps) {
            fps[i] = fps[0];
            uregs.fp = fps[0];
         }
         if (0 == uregs.ra || 1 == uregs.ra) break;
         uregs.pc = uregs.ra - 8;
         ips[i++] = uregs.ra - 8;
         RECURSIVE_MERGE(cmrf,ips,i);
         continue;
      }
      /* No luck.  We have to give up. */
      break;
   }

   n_found = i;
   return n_found;
}

#endif

/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- END platform-dependent unwinder worker functions     ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

/*------------------------------------------------------------*/
/*--- Exported functions.                                  ---*/
/*------------------------------------------------------------*/

UInt VG_(get_StackTrace_with_deltas)(
         ThreadId tid, 
         /*OUT*/StackTrace ips, UInt n_ips,
         /*OUT*/StackTrace sps,
         /*OUT*/StackTrace fps,
         Word first_ip_delta,
         Word first_sp_delta
      )
{
   /* Get the register values with which to start the unwind. */
   UnwindStartRegs startRegs;
   VG_(memset)( &startRegs, 0, sizeof(startRegs) );
   VG_(get_UnwindStartRegs)( &startRegs, tid );

   Addr stack_highest_byte = VG_(threads)[tid].client_stack_highest_byte;
   Addr stack_lowest_byte  = 0;

#  if defined(VGP_x86_linux)
   /* Nasty little hack to deal with syscalls - if libc is using its
      _dl_sysinfo_int80 function for syscalls (the TLS version does),
      then ip will always appear to be in that function when doing a
      syscall, not the actual libc function doing the syscall.  This
      check sees if IP is within that function, and pops the return
      address off the stack so that ip is placed within the library
      function calling the syscall.  This makes stack backtraces much
      more useful.

      The function is assumed to look like this (from glibc-2.3.6 sources):
         _dl_sysinfo_int80:
            int $0x80
            ret
      That is 3 (2+1) bytes long.  We could be more thorough and check
      the 3 bytes of the function are as expected, but I can't be
      bothered.
   */
   if (VG_(client__dl_sysinfo_int80) != 0 /* we know its address */
       && startRegs.r_pc >= VG_(client__dl_sysinfo_int80)
       && startRegs.r_pc < VG_(client__dl_sysinfo_int80)+3
       && VG_(am_is_valid_for_client)(startRegs.r_pc, sizeof(Addr),
                                      VKI_PROT_READ)) {
      startRegs.r_pc  = (ULong) *(Addr*)(UWord)startRegs.r_sp;
      startRegs.r_sp += (ULong) sizeof(Addr);
   }
#  endif

   /* See if we can get a better idea of the stack limits */
   VG_(stack_limits)( (Addr)startRegs.r_sp,
                      &stack_lowest_byte, &stack_highest_byte );

   /* Take into account the first_ip_delta and first_sp_delta. */
   startRegs.r_pc += (Long)first_ip_delta;
   startRegs.r_sp += (Long)first_sp_delta;

   if (0)
      VG_(printf)("tid %u: stack_highest=0x%08lx ip=0x%010llx "
                  "sp=0x%010llx\n",
                  tid, stack_highest_byte,
                  startRegs.r_pc, startRegs.r_sp);

   return VG_(get_StackTrace_wrk)(tid, ips, n_ips, 
                                       sps, fps,
                                       &startRegs,
                                       stack_highest_byte);
}

UInt VG_(get_StackTrace) ( ThreadId tid, 
                           /*OUT*/StackTrace ips, UInt max_n_ips,
                           /*OUT*/StackTrace sps,
                           /*OUT*/StackTrace fps,
                           Word first_ip_delta )
{
   return VG_(get_StackTrace_with_deltas) (tid,
                                           ips, max_n_ips,
                                           sps,
                                           fps,
                                           first_ip_delta,
                                           0 /* first_sp_delta */
                                           );
}

static void printIpDesc(UInt n, DiEpoch ep, Addr ip, void* uu_opaque)
{
   InlIPCursor *iipc = VG_(new_IIPC)(ep, ip);

   do {
      const HChar *buf = VG_(describe_IP)(ep, ip, iipc);
      if (VG_(clo_xml)) {
         VG_(printf_xml)("    %s\n", buf);
      } else {
         VG_(message)(Vg_UserMsg, "   %s %s\n", 
                      ( n == 0 ? "at" : "by" ), buf);
      }
      n++; 
      // Increase n to show "at" for only one level.
   } while (VG_(next_IIPC)(iipc));
   VG_(delete_IIPC)(iipc);
}

/* Print a StackTrace. */
void VG_(pp_StackTrace) ( DiEpoch ep, StackTrace ips, UInt n_ips )
{
   vg_assert( n_ips > 0 );

   if (VG_(clo_xml))
      VG_(printf_xml)("  <stack>\n");

   VG_(apply_StackTrace)( printIpDesc, NULL, ep, ips, n_ips );

   if (VG_(clo_xml))
      VG_(printf_xml)("  </stack>\n");
}

/* Get and immediately print a StackTrace. */
void VG_(get_and_pp_StackTrace) ( ThreadId tid, UInt max_n_ips )
{
   Addr ips[max_n_ips];
   UInt n_ips
      = VG_(get_StackTrace)(tid, ips, max_n_ips,
                            NULL/*array to dump SP values in*/,
                            NULL/*array to dump FP values in*/,
                            0/*first_ip_delta*/);
   VG_(pp_StackTrace)(VG_(current_DiEpoch)(), ips, n_ips);
}

void VG_(apply_StackTrace)(
        void(*action)(UInt n, DiEpoch ep, Addr ip, void* opaque),
        void* opaque,
        DiEpoch ep, StackTrace ips, UInt n_ips
     )
{
   Int i;

   vg_assert(n_ips > 0);
   if ( ! VG_(clo_show_below_main) ) {
      // Search (from the outer frame onwards) the appearance of "main"
      // or the last appearance of a below main function.
      // Then decrease n_ips so as to not call action for the below main
      for (i = n_ips - 1; i >= 0; i--) {
         Vg_FnNameKind kind = VG_(get_fnname_kind_from_IP)(ep, ips[i]);
         if (Vg_FnNameMain == kind || Vg_FnNameBelowMain == kind)
            n_ips = i + 1;
         if (Vg_FnNameMain == kind)
            break;
      }
   }

   for (i = 0; i < n_ips; i++)
      // Act on the ip
      action(i, ep, ips[i], opaque);
}


/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
