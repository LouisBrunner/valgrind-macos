/*--------------------------------------------------------------------*/
/*--- Cachegrind: cache configuration.                   cg-arch.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Cachegrind, a high-precision tracing profiler
   built with Valgrind.

   Copyright (C) 2011-2017 Nicholas Nethercote
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
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#include "pub_tool_basics.h"
#include "pub_tool_libcassert.h"
#include "pub_tool_libcbase.h"
#include "pub_tool_libcprint.h"
#include "pub_tool_options.h"
#include "pub_tool_machine.h"

#include "cg_arch.h"

static void configure_caches(cache_t* I1c, cache_t* D1c, cache_t* LLc,
                             Bool all_caches_clo_defined);

// Checks cache config is ok.  Returns NULL if ok, or a pointer to an error
// string otherwise.
static const HChar* check_cache(cache_t* cache)
{
   if (cache->line_size == 0)
   {
      return "Cache line size is zero.\n";
   }

   if (cache->assoc == 0)
   {
      return "Cache associativity is zero.\n";
   }

   // Simulator requires set count to be a power of two.
   if ((cache->size % (cache->line_size * cache->assoc) != 0) ||
       (-1 == VG_(log2)(cache->size/cache->line_size/cache->assoc)))
   {
      return "Cache set count is not a power of two.\n";
   }

   // Simulator requires line size to be a power of two.
   if (-1 == VG_(log2)(cache->line_size)) {
      return "Cache line size is not a power of two.\n";
   }

   // Then check line size >= 16 -- any smaller and a single instruction could
   // straddle three cache lines, which breaks a simulation assertion and is
   // stupid anyway.
   if (cache->line_size < MIN_LINE_SIZE) {
      return "Cache line size is too small.\n";
   }

   /* Then check cache size > line size (causes seg faults if not). */
   if (cache->size <= cache->line_size) {
      return "Cache size <= line size.\n";
   }

   /* Then check assoc <= (size / line size) (seg faults otherwise). */
   if (cache->assoc > (cache->size / cache->line_size)) {
      return "Cache associativity > (size / line size).\n";
   }

   return NULL;
}


static void parse_cache_opt ( cache_t* cache, const HChar* opt,
                              const HChar* optval )
{
   Long i1, i2, i3;
   HChar* endptr;
   const HChar* checkRes;

   // Option argument looks like "65536,2,64".  Extract them.
   i1 = VG_(strtoll10)(optval,   &endptr); if (*endptr != ',')  goto bad;
   i2 = VG_(strtoll10)(endptr+1, &endptr); if (*endptr != ',')  goto bad;
   i3 = VG_(strtoll10)(endptr+1, &endptr); if (*endptr != '\0') goto bad;

   // Check for overflow.
   cache->size      = (Int)i1;
   cache->assoc     = (Int)i2;
   cache->line_size = (Int)i3;
   if (cache->size      != i1) goto overflow;
   if (cache->assoc     != i2) goto overflow;
   if (cache->line_size != i3) goto overflow;

   checkRes = check_cache(cache);
   if (checkRes) {
      VG_(fmsg)("%s", checkRes);
      goto bad;
   }

   return;

  bad:
   VG_(fmsg_bad_option)(opt, "Bad argument '%s'\n", optval);

  overflow:
   VG_(fmsg_bad_option)(opt,
      "One of the cache parameters was too large and overflowed.\n");
}


Bool VG_(str_clo_cache_opt)(const HChar *arg,
                            cache_t* clo_I1c,
                            cache_t* clo_D1c,
                            cache_t* clo_LLc)
{
   const HChar* tmp_str;

   if      VG_STR_CLO(arg, "--I1", tmp_str) {
      parse_cache_opt(clo_I1c, arg, tmp_str);
      return True;
   } else if VG_STR_CLO(arg, "--D1", tmp_str) {
      parse_cache_opt(clo_D1c, arg, tmp_str);
      return True;
   } else if (VG_STR_CLO(arg, "--L2", tmp_str) || // for backwards compatibility
              VG_STR_CLO(arg, "--LL", tmp_str)) {
      parse_cache_opt(clo_LLc, arg, tmp_str);
      return True;
   } else
      return False;
}

static void umsg_cache_img(const HChar* desc, cache_t* c)
{
   VG_(umsg)("  %s: %'d B, %d-way, %d B lines\n", desc,
             c->size, c->assoc, c->line_size);
}

// Verifies if c is a valid cache.
// An invalid value causes an assert, unless clo_redefined is True.
static void check_cache_or_override(const HChar* desc, cache_t* c, Bool clo_redefined)
{
   const HChar* checkRes;

   checkRes = check_cache(c);
   if (checkRes) {
      VG_(umsg)("Auto-detected %s cache configuration not supported: %s",
                desc, checkRes);
      umsg_cache_img(desc, c);
      if (!clo_redefined) {
         VG_(umsg)("As it probably should be supported, please report a bug!\n");
         VG_(umsg)("Bypass this message by using option --%s=...\n", desc);
         tl_assert(0);
      }
   }
}


/* If the LL cache config isn't something the simulation functions
   can handle, try to adjust it so it is.  Caches are characterised
   by (total size T, line size L, associativity A), and then we
   have

     number of sets S = T / (L * A)

   The required constraints are:

   * L must be a power of 2, but it always is in practice, so
     no problem there

   * A can be any value >= 1

   * T can be any value, but ..

   * S must be a power of 2.

   That sometimes gives a problem.  For example, some Core iX based
   Intel CPUs have T = 12MB, A = 16, L = 64, which gives 12288
   sets.  Some AMD cpus have T = 5MB, A = 48, L = 64, which gives
   1706.667 sets (!).

   The "fix" is to force S down to the nearest power of two below its
   original value, and increase A proportionately, so as to keep the
   total cache size the same.  In fact to be safe we recalculate the
   cache size afterwards anyway, to guarantee that it divides exactly
   between the new number of sets.

   The "fix" is "justified" (cough, cough) by alleging that
   increases of associativity above about 4 have very little effect
   on the actual miss rate.  It would be far more inaccurate to
   fudge this by changing the size of the simulated cache --
   changing the associativity is a much better option.
*/

/* (Helper function) Returns the largest power of 2 that is <= |x|.
   Even works when |x| == 0. */
static UInt floor_power_of_2 ( UInt x )
{
   x = x | (x >> 1);
   x = x | (x >> 2);
   x = x | (x >> 4);
   x = x | (x >> 8);
   x = x | (x >> 16);
   return x - (x >> 1);
}

static void
maybe_tweak_LLc(cache_t *LLc)
{
  if (LLc->size == 0 || LLc->assoc == 0 || LLc->line_size == 0)
     return;

  tl_assert(LLc->size > 0 && LLc->assoc > 0 && LLc->line_size > 0);

  UInt old_size      = (UInt)LLc->size;
  UInt old_assoc     = (UInt)LLc->assoc;
  UInt old_line_size = (UInt)LLc->line_size;

  UInt new_size      = old_size;
  UInt new_assoc     = old_assoc;
  UInt new_line_size = old_line_size;

  UInt old_nSets = old_size / (old_assoc * old_line_size);
  if (old_nSets == 0) {
     /* This surely can't happen; but would cause chaos with the maths
      * below if it did.  Just give up if it does. */
     return;
  }

  if (-1 != VG_(log2_64)(old_nSets)) {
     /* The number of sets is already a power of 2.  Make sure that
        the size divides exactly between the sets.  Almost all of the
        time this will have no effect. */
     new_size = old_line_size * old_assoc * old_nSets;
  } else {
     /* The number of sets isn't a power of two.  Calculate some
        scale-down factor which causes the number of sets to become a
        power of two.  Then, increase the associativity by that
        factor.  Finally, re-calculate the total size so as to make
        sure it divides exactly between the sets. */
     UInt new_nSets = floor_power_of_2 ( old_nSets );
     tl_assert(new_nSets > 0 && new_nSets < old_nSets);
     Double factor = (Double)old_nSets / (Double)new_nSets;
     tl_assert(factor >= 1.0);

     new_assoc = (UInt)(0.5 + factor * (Double)old_assoc);
     tl_assert(new_assoc >= old_assoc);

     new_size = old_line_size * new_assoc * new_nSets;
  }
  
  tl_assert(new_line_size == old_line_size); /* we never change this */
  if (new_size == old_size && new_assoc == old_assoc)
     return;

  VG_(dmsg)("warning: "
            "specified LL cache: line_size %u  assoc %u  total_size %'u\n",
            old_line_size, old_assoc, old_size);
  VG_(dmsg)("warning: "
            "simulated LL cache: line_size %u  assoc %u  total_size %'u\n",\
            new_line_size, new_assoc, new_size);

  LLc->size      = new_size;
  LLc->assoc     = new_assoc;
  LLc->line_size = new_line_size;
}

void VG_(post_clo_init_configure_caches)(cache_t* I1c,
                                         cache_t* D1c,
                                         cache_t* LLc,
                                         cache_t* clo_I1c,
                                         cache_t* clo_D1c,
                                         cache_t* clo_LLc)
{
#define DEFINED(L)   (-1 != L->size  || -1 != L->assoc || -1 != L->line_size)

   // Count how many were defined on the command line.
   Bool all_caches_clo_defined =
      (DEFINED(clo_I1c) &&
       DEFINED(clo_D1c) &&
       DEFINED(clo_LLc));

   // Set the cache config (using auto-detection, if supported by the
   // architecture).
   configure_caches( I1c, D1c, LLc, all_caches_clo_defined );

   maybe_tweak_LLc( LLc );

   // Check the default/auto-detected values.
   // Allow the user to override invalid auto-detected caches
   // with command line.
   check_cache_or_override ("I1", I1c, DEFINED(clo_I1c));
   check_cache_or_override ("D1", D1c, DEFINED(clo_D1c));
   check_cache_or_override ("LL", LLc, DEFINED(clo_LLc));

   // Then replace with any defined on the command line.  (Already checked in
   // VG(str_clo_cache_opt)().)
   if (DEFINED(clo_I1c)) { *I1c = *clo_I1c; }
   if (DEFINED(clo_D1c)) { *D1c = *clo_D1c; }
   if (DEFINED(clo_LLc)) { *LLc = *clo_LLc; }

   if (VG_(clo_verbosity) >= 2) {
      VG_(umsg)("Cache configuration used:\n");
      umsg_cache_img ("I1", I1c);
      umsg_cache_img ("D1", D1c);
      umsg_cache_img ("LL", LLc);
   }
#undef DEFINED
}

void VG_(print_cache_clo_opts)(void)
{
   VG_(printf)(
"    --I1=<size>,<assoc>,<line_size>  set I1 cache manually\n"
"    --D1=<size>,<assoc>,<line_size>  set D1 cache manually\n"
"    --LL=<size>,<assoc>,<line_size>  set LL cache manually\n"
   );
}


// Traverse the cache info and return a cache of the given kind and level.
// Return NULL if no such cache exists.
static const VexCache *
locate_cache(const VexCacheInfo *ci, VexCacheKind kind, UInt level)
{
   const VexCache *c;

   for (c = ci->caches; c != ci->caches + ci->num_caches; ++c) {
      if (c->level == level && c->kind == kind) {
         return c;
      }
   }
   return NULL;  // not found
}


// Gives the auto-detected configuration of I1, D1 and LL caches.  They get
// overridden by any cache configurations specified on the command line.
static void
configure_caches(cache_t *I1c, cache_t *D1c, cache_t *LLc,
                 Bool all_caches_clo_defined)
{
   VexArchInfo vai;
   const VexCacheInfo *ci;
   const VexCache *i1, *d1, *ll;

   VG_(machine_get_VexArchInfo)(NULL, &vai);
   ci = &vai.hwcache_info;

   // Extract what we need
   i1 = locate_cache(ci, INSN_CACHE, 1);
   d1 = locate_cache(ci, DATA_CACHE, 1);
   ll = locate_cache(ci, UNIFIED_CACHE, ci->num_levels);

   if (ci->num_caches > 0 && ll == NULL) {
      VG_(dmsg)("warning: L2 cache not installed, ignore LL results.\n");
   }

   if (ll && ci->num_levels > 2) {
      VG_(dmsg)("warning: L%u cache found, using its data for the "
                "LL simulation.\n", ci->num_levels);
   }

   if (i1 && d1 && ll) {
      if (i1->is_trace_cache) {
         /* HACK ALERT: Instruction trace cache -- capacity is micro-ops based.
          * conversion to byte size is a total guess;  treat the 12K and 16K
          * cases the same since the cache byte size must be a power of two for
          * everything to work!.  Also guessing 32 bytes for the line size...
          */
         UInt adjusted_size, guessed_line_size = 32;

         if (i1->sizeB == 12 * 1024 || i1->sizeB == 16 * 1024) {
            adjusted_size = 16 * 1024;
         } else {
            adjusted_size = 32 * 1024;
         }
         VG_(dmsg)("warning: Pentium 4 with %u KB micro-op instruction trace cache\n",
                   i1->sizeB / 1024);
         VG_(dmsg)("         Simulating a %u KB I-cache with %u B lines\n",
                   adjusted_size / 1024, guessed_line_size);

         *I1c = (cache_t) { adjusted_size, i1->assoc, guessed_line_size };
      } else {
         *I1c = (cache_t) { i1->sizeB, i1->assoc, i1->line_sizeB };
      }
      *D1c = (cache_t) { d1->sizeB, d1->assoc, d1->line_sizeB };
      *LLc = (cache_t) { ll->sizeB, ll->assoc, ll->line_sizeB };

      return;
   }

   // Cache information could not be queried; choose some default
   // architecture specific default setting.

#if defined(VGA_ppc32)

   // Default cache configuration
   *I1c = (cache_t) {  65536, 2, 64 };
   *D1c = (cache_t) {  65536, 2, 64 };
   *LLc = (cache_t) { 262144, 8, 64 };

#elif defined(VGA_ppc64be) || defined(VGA_ppc64le)

   // Default cache configuration
   *I1c = (cache_t) {  65536, 2, 64 };
   *D1c = (cache_t) {  65536, 2, 64 };
   *LLc = (cache_t) { 262144, 8, 64 };

#elif defined(VGA_arm)

   // Set caches to default (for Cortex-A8 ?)
   *I1c = (cache_t) {  16384, 4, 64 };
   *D1c = (cache_t) {  16384, 4, 64 };
   *LLc = (cache_t) { 262144, 8, 64 };

#elif defined(VGA_arm64)

   // Copy the 32-bit ARM version until such time as we have
   // some real hardware to run on
   *I1c = (cache_t) {  16384, 4, 64 };
   *D1c = (cache_t) {  16384, 4, 64 };
   *LLc = (cache_t) { 262144, 8, 64 };

#elif defined(VGA_s390x)
   //
   // Here is the cache data from older machine models:
   //
   //           I1            D1      I/D L2
   // z900  256k/256/4    256k/256/4   16MB
   // z800  256k/256/4    256k/256/4    8MB
   // z990  256k/256/4    256k/256/4   32MB
   // z890  256k/256/4    256k/256/4   32MB
   // z9    256k/256/4    256k/256/4   40MB
   //
   // Sources:
   // (1) IBM System z9 109 Technical Introduction
   //     www.redbooks.ibm.com/redbooks/pdfs/sg246669.pdf
   // (2) The microarchitecture of the IBM eServer z900 processor
   //     IBM Journal of Research and Development
   //     Volume 46, Number 4/5, pp 381-395, July/September 2002
   // (3) The IBM eServer z990 microprocessor
   //     IBM Journal of Research and Development
   //     Volume 48, Number 3/4, pp 295-309, May/July 2004 
   // (4) Charles Webb, IBM
   //
   // L2 data is unfortunately incomplete. Otherwise, we could support
   // machines without the ECAG insn by looking at VEX_S390X_MODEL(hwcaps).

   // Default cache configuration is z10-EC  (Source: ECAG insn)
   *I1c = (cache_t) {    65536,  4, 256 };
   *D1c = (cache_t) {   131072,  8, 256 };
   *LLc = (cache_t) { 50331648, 24, 256 };

#elif defined(VGA_mips32) || defined(VGA_nanomips)

   // Set caches to default (for MIPS32-r2(mips 74kc))
   *I1c = (cache_t) {  32768, 4, 32 };
   *D1c = (cache_t) {  32768, 4, 32 };
   *LLc = (cache_t) { 524288, 8, 32 };

#elif defined(VGA_mips64)

   // Set caches to default (for MIPS64 - 5kc)
   *I1c = (cache_t) {  32768, 4, 32 };
   *D1c = (cache_t) {  32768, 4, 32 };
   *LLc = (cache_t) { 524288, 8, 32 };

#elif defined(VGA_x86) || defined(VGA_amd64)

   *I1c = (cache_t) {  65536, 2, 64 };
   *D1c = (cache_t) {  65536, 2, 64 };
   *LLc = (cache_t) { 262144, 8, 64 };

#else

#error "Unknown arch"

#endif

   if (!all_caches_clo_defined) {
      const HChar warning[] =
        "Warning: Cannot auto-detect cache config, using defaults.\n"
        "         Run with -v to see.\n";
      VG_(dmsg)("%s", warning);
   }
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
