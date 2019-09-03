/* -*- mode: C; c-basic-offset: 3; -*- */

/*--------------------------------------------------------------------*/
/*--- Cache-related stuff.                               m_cache.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2002-2017 Nicholas Nethercote
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

#include "pub_core_basics.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"
#include "pub_core_mallocfree.h"
#include "pub_core_machine.h"
#include "pub_core_debuglog.h"
#include "libvex.h"

#if defined(VGA_x86) || defined(VGA_amd64)

#include "pub_core_cpuid.h"

// All CPUID info taken from sandpile.org/ia32/cpuid.htm */
// Probably only works for Intel and AMD chips, and probably only for some of
// them.

static void
add_cache(VexCacheInfo *ci, VexCache cache)
{
   static UInt num_allocated = 0;

   if (ci->num_caches == num_allocated) {
      num_allocated += 6;
      ci->caches = VG_(realloc)("m_cache", ci->caches,
                                num_allocated * sizeof *ci->caches);
   }

   if (ci->num_levels < cache.level) ci->num_levels = cache.level;
   ci->caches[ci->num_caches++] = cache;
}

/* Convenience macros */
#define add_icache(level, size, assoc, linesize) \
   do { \
      add_cache(ci, \
                VEX_CACHE_INIT(INSN_CACHE, level, size, linesize, assoc)); \
   } while (0)

#define add_dcache(level, size, assoc, linesize) \
   do { \
      add_cache(ci, \
                VEX_CACHE_INIT(DATA_CACHE, level, size, linesize, assoc)); \
   } while (0)

#define add_ucache(level, size, assoc, linesize) \
   do { \
      add_cache(ci, \
                VEX_CACHE_INIT(UNIFIED_CACHE, level, size, linesize, assoc)); \
   } while (0)

#define add_itcache(level, size, assoc) \
   do { \
      VexCache c = \
          VEX_CACHE_INIT(INSN_CACHE, level, size, 0, assoc); \
      c.is_trace_cache = True; \
      add_cache(ci, c); \
   } while (0)

#define add_I1(size, assoc, linesize) add_icache(1, size, assoc, linesize)
#define add_D1(size, assoc, linesize) add_dcache(1, size, assoc, linesize)
#define add_U1(size, assoc, linesize) add_ucache(1, size, assoc, linesize)
#define add_I2(size, assoc, linesize) add_icache(2, size, assoc, linesize)
#define add_D2(size, assoc, linesize) add_dcache(2, size, assoc, linesize)
#define add_U2(size, assoc, linesize) add_ucache(2, size, assoc, linesize)
#define add_I3(size, assoc, linesize) add_icache(3, size, assoc, linesize)
#define add_D3(size, assoc, linesize) add_dcache(3, size, assoc, linesize)
#define add_U3(size, assoc, linesize) add_ucache(3, size, assoc, linesize)

#define add_I1T(size, assoc) \
   add_itcache(1, size, assoc)

/* Intel method is truly wretched.  We have to do an insane indexing into an
 * array of pre-defined configurations for various parts of the memory
 * hierarchy.
 * According to Intel Processor Identification, App Note 485.
 *
 * If a L3 cache is found, then data for it rather than the L2
 * is returned via *LLc.
 */
static Int
Intel_cache_info(Int level, VexCacheInfo *ci)
{
   UInt cpuid1_eax;
   UInt cpuid1_ignore;
   Int family;
   Int model;
   UChar info[16];
   Int   i, j, trials;

   if (level < 2) {
      VG_(debugLog)(1, "cache", "warning: CPUID level < 2 for Intel "
                    "processor (%d)\n", level);
      return -1;
   }

   /* family/model needed to distinguish code reuse (currently 0x49) */
   VG_(cpuid)(1, 0, &cpuid1_eax, &cpuid1_ignore,
	      &cpuid1_ignore, &cpuid1_ignore);
   family = (((cpuid1_eax >> 20) & 0xff) << 4) + ((cpuid1_eax >> 8) & 0xf);
   model =  (((cpuid1_eax >> 16) & 0xf) << 4) + ((cpuid1_eax >> 4) & 0xf);

   VG_(cpuid)(2, 0, (UInt*)&info[0], (UInt*)&info[4],
                    (UInt*)&info[8], (UInt*)&info[12]);
   trials  = info[0] - 1;   /* AL register - bits 0..7 of %eax */
   info[0] = 0x0;           /* reset AL */

   if (0 != trials) {
      VG_(debugLog)(1, "cache", "warning: non-zero CPUID trials for Intel "
                    "processor (%d)\n", trials);
      return -1;
   }

   ci->num_levels = 0;
   ci->num_caches = 0;
   ci->icaches_maintain_coherence = True;
   ci->caches = NULL;

   for (i = 0; i < 16; i++) {

      switch (info[i]) {

      case 0x0:       /* ignore zeros */
          break;

      /* TLB info, ignore */
      case 0x01: case 0x02: case 0x03: case 0x04: case 0x05:
      case 0x0b:
      case 0x4f: case 0x50: case 0x51: case 0x52: case 0x55:
      case 0x56: case 0x57: case 0x59:
      case 0x5a: case 0x5b: case 0x5c: case 0x5d:
      case 0x76:
      case 0xb0: case 0xb1: case 0xb2:
      case 0xb3: case 0xb4: case 0xba: case 0xc0:
      case 0xca:
          break;

      case 0x06: add_I1( 8, 4, 32); break;
      case 0x08: add_I1(16, 4, 32); break;
      case 0x09: add_I1(32, 4, 64); break;
      case 0x30: add_I1(32, 8, 64); break;

      case 0x0a: add_D1( 8, 2, 32); break;
      case 0x0c: add_D1(16, 4, 32); break;
      case 0x0d: add_D1(16, 4, 64); break;
      case 0x0e: add_D1(24, 6, 64); break;
      case 0x2c: add_D1(32, 8, 64); break;

      /* IA-64 info -- panic! */
      case 0x10: case 0x15: case 0x1a:
      case 0x88: case 0x89: case 0x8a: case 0x8d:
      case 0x90: case 0x96: case 0x9b:
         VG_(core_panic)("IA-64 cache detected?!");

      /* L3 cache info. */
      case 0x22: add_U3(512,    4, 64); break;
      case 0x23: add_U3(1024,   8, 64); break;
      case 0x25: add_U3(2048,   8, 64); break;
      case 0x29: add_U3(4096,   8, 64); break;
      case 0x46: add_U3(4096,   4, 64); break;
      case 0x47: add_U3(8192,   8, 64); break;
      case 0x4a: add_U3(6144,  12, 64); break;
      case 0x4b: add_U3(8192,  16, 64); break;
      case 0x4c: add_U3(12288, 12, 64); break;
      case 0x4d: add_U3(16384, 16, 64); break;
      case 0xd0: add_U3(512,    4, 64); break;
      case 0xd1: add_U3(1024,   4, 64); break;
      case 0xd2: add_U3(2048,   4, 64); break;
      case 0xd6: add_U3(1024,   8, 64); break;
      case 0xd7: add_U3(2048,   8, 64); break;
      case 0xd8: add_U3(4096,   8, 64); break;
      case 0xdc: add_U3(1536,  12, 64); break;
      case 0xdd: add_U3(3072,  12, 64); break;
      case 0xde: add_U3(6144,  12, 64); break;
      case 0xe2: add_U3(2048,  16, 64); break;
      case 0xe3: add_U3(4096,  16, 64); break;
      case 0xe4: add_U3(8192,  16, 64); break;
      case 0xea: add_U3(12288, 24, 64); break;
      case 0xeb: add_U3(18432, 24, 64); break;
      case 0xec: add_U3(24576, 24, 64); break;

      /* Described as "MLC" in Intel documentation */
      case 0x21: add_U2(256, 8, 64); break;

      /* These are sectored, whatever that means */
         // FIXME: I did not find these in the Intel docs
      case 0x39: add_U2(128, 4, 64); break;
      case 0x3c: add_U2(256, 4, 64); break;

      /* If a P6 core, this means "no L2 cache".
         If a P4 core, this means "no L3 cache".
         We don't know what core it is, so don't issue a warning.  To detect
         a missing L2 cache, we use 'L2_found'. */
      case 0x40:
          break;

      case 0x41: add_U2(  128,  4, 32); break;
      case 0x42: add_U2(  256,  4, 32); break;
      case 0x43: add_U2(  512,  4, 32); break;
      case 0x44: add_U2( 1024,  4, 32); break;
      case 0x45: add_U2( 2048,  4, 32); break;
      case 0x48: add_U2( 3072, 12, 64); break;
      case 0x4e: add_U2( 6144, 24, 64); break;
      case 0x49:
         if (family == 15 && model == 6) {
            /* On Xeon MP (family F, model 6), this is for L3 */
            add_U3(4096, 16, 64);
         } else {
	    add_U2(4096, 16, 64);
         }
         break;

      /* These are sectored, whatever that means */
      case 0x60: add_D1(16, 8, 64);  break;      /* sectored */
      case 0x66: add_D1( 8, 4, 64);  break;      /* sectored */
      case 0x67: add_D1(16, 4, 64);  break;      /* sectored */
      case 0x68: add_D1(32, 4, 64);  break;      /* sectored */

      /* HACK ALERT: Instruction trace cache -- capacity is micro-ops based.
       * conversion to byte size is a total guess;  treat the 12K and 16K
       * cases the same since the cache byte size must be a power of two for
       * everything to work!.  Also guessing 32 bytes for the line size...
       */
      case 0x70:    /* 12K micro-ops, 8-way */
         add_I1T(12, 8);
         break;
      case 0x71:    /* 16K micro-ops, 8-way */
         add_I1T(16, 8);
         break;
      case 0x72:    /* 32K micro-ops, 8-way */
         add_I1T(32, 8);
         break;

      /* not sectored, whatever that might mean */
      case 0x78: add_U2(1024, 4,  64);  break;

      /* These are sectored, whatever that means */
      case 0x79: add_U2( 128, 8,  64);  break;
      case 0x7a: add_U2( 256, 8,  64);  break;
      case 0x7b: add_U2( 512, 8,  64);  break;
      case 0x7c: add_U2(1024, 8,  64);  break;
      case 0x7d: add_U2(2048, 8,  64);  break;
      case 0x7e: add_U2( 256, 8, 128);  break;
      case 0x7f: add_U2( 512, 2,  64);  break;
      case 0x80: add_U2( 512, 8,  64);  break;
      case 0x81: add_U2( 128, 8,  32);  break;
      case 0x82: add_U2( 256, 8,  32);  break;
      case 0x83: add_U2( 512, 8,  32);  break;
      case 0x84: add_U2(1024, 8,  32);  break;
      case 0x85: add_U2(2048, 8,  32);  break;
      case 0x86: add_U2( 512, 4,  64);  break;
      case 0x87: add_U2(1024, 8,  64);  break;

      /* Ignore prefetch information */
      case 0xf0: case 0xf1:
         break;

      case 0xff:
         j = 0;
         VG_(cpuid)(4, j++, (UInt*)&info[0], (UInt*)&info[4],
                            (UInt*)&info[8], (UInt*)&info[12]);

         while ((info[0] & 0x1f) != 0) {
            UInt assoc = ((*(UInt *)&info[4] >> 22) & 0x3ff) + 1;
            UInt parts = ((*(UInt *)&info[4] >> 12) & 0x3ff) + 1;
            UInt line_size = (*(UInt *)&info[4] & 0x7ff) + 1;
            UInt sets = *(UInt *)&info[8] + 1;

            UInt size = assoc * parts * line_size * sets / 1024;

            switch ((info[0] & 0xe0) >> 5)
            {
            case 1:
               switch (info[0] & 0x1f)
               {
               case 1: add_D1(size, assoc, line_size); break;
               case 2: add_I1(size, assoc, line_size); break;
               case 3: add_U1(size, assoc, line_size); break;
               default:
                  VG_(debugLog)(1, "cache",
                                "warning: L1 cache of unknown type ignored\n");
                  break;
               }
               break;
            case 2:
               switch (info[0] & 0x1f)
               {
               case 1: add_D2(size, assoc, line_size); break;
               case 2: add_I2(size, assoc, line_size); break;
               case 3: add_U2(size, assoc, line_size); break;
               default:
                  VG_(debugLog)(1, "cache",
                                "warning: L2 cache of unknown type ignored\n");
                  break;
               }
               break;
            case 3:
               switch (info[0] & 0x1f)
               {
               case 1: add_D3(size, assoc, line_size); break;
               case 2: add_I3(size, assoc, line_size); break;
               case 3: add_U3(size, assoc, line_size); break;
               default:
                  VG_(debugLog)(1, "cache",
                                "warning: L3 cache of unknown type ignored\n");
                  break;
               }
               break;
            default:
               VG_(debugLog)(1, "cache", "warning: L%u cache ignored\n",
                             (info[0] & 0xe0) >> 5);
               break;
            }

            VG_(cpuid)(4, j++, (UInt*)&info[0], (UInt*)&info[4],
                               (UInt*)&info[8], (UInt*)&info[12]);
         }
         break;

      default:
         VG_(debugLog)(1, "cache",
                       "warning: Unknown Intel cache config value (0x%x), "
                       "ignoring\n", info[i]);
         break;
      }
   }

   return 0;
}

/* AMD method is straightforward, just extract appropriate bits from the
 * result registers.
 *
 * Bits, for D1 and I1:
 *  31..24  data L1 cache size in KBs
 *  23..16  data L1 cache associativity (FFh=full)
 *  15.. 8  data L1 cache lines per tag
 *   7.. 0  data L1 cache line size in bytes
 *
 * Bits, for L2:
 *  31..16  unified L2 cache size in KBs
 *  15..12  unified L2 cache associativity (0=off, FFh=full)
 *  11.. 8  unified L2 cache lines per tag
 *   7.. 0  unified L2 cache line size in bytes
 *
 * #3  The AMD K7 processor's L2 cache must be configured prior to relying
 *     upon this information. (Whatever that means -- njn)
 *
 * Also, according to Cyrille Chepelov, Duron stepping A0 processors (model
 * 0x630) have a bug and misreport their L2 size as 1KB (it's really 64KB),
 * so we detect that.
 *
 * Returns 0 on success, non-zero on failure.  As with the Intel code
 * above, if a L3 cache is found, then data for it rather than the L2
 * is returned via *LLc.
 */

/* A small helper */
static Int
decode_AMD_cache_L2_L3_assoc ( Int bits_15_12 )
{
   /* Decode a L2/L3 associativity indication.  It is encoded
      differently from the I1/D1 associativity.  Returns 1
      (direct-map) as a safe but suboptimal result for unknown
      encodings. */
   switch (bits_15_12 & 0xF) {
      case 1: return 1;    case 2: return 2;
      case 4: return 4;    case 6: return 8;
      case 8: return 16;   case 0xA: return 32;
      case 0xB: return 48; case 0xC: return 64;
      case 0xD: return 96; case 0xE: return 128;
      case 0xF: /* fully associative */
      case 0: /* L2/L3 cache or TLB is disabled */
      default:
        return 1;
   }
}

static Int
AMD_cache_info(VexCacheInfo *ci)
{
   UInt ext_level;
   UInt dummy, model;
   UInt I1i, D1i, L2i, L3i;
   UInt size, line_size, assoc;

   VG_(cpuid)(0x80000000, 0, &ext_level, &dummy, &dummy, &dummy);

   if (0 == (ext_level & 0x80000000) || ext_level < 0x80000006) {
      VG_(debugLog)(1, "cache", "warning: ext_level < 0x80000006 for AMD "
                    "processor (0x%x)\n", ext_level);
      return -1;
   }

   VG_(cpuid)(0x80000005, 0, &dummy, &dummy, &D1i, &I1i);
   VG_(cpuid)(0x80000006, 0, &dummy, &dummy, &L2i, &L3i);

   VG_(cpuid)(0x1, 0, &model, &dummy, &dummy, &dummy);

   /* Check for Duron bug */
   if (model == 0x630) {
      VG_(debugLog)(1, "cache", "warning: Buggy Duron stepping A0. "
                    "Assuming L2 size=65536 bytes\n");
      L2i = (64 << 16) | (L2i & 0xffff);
   }

   ci->num_levels = 2;
   ci->num_caches = 3;
   ci->icaches_maintain_coherence = True;

   /* Check for L3 cache */
   if (((L3i >> 18) & 0x3fff) > 0) {
      ci->num_levels = 3;
      ci->num_caches = 4;
   }

   ci->caches = VG_(malloc)("m_cache", ci->num_caches * sizeof *ci->caches);

   // D1
   size      = (D1i >> 24) & 0xff;
   assoc     = (D1i >> 16) & 0xff;
   line_size = (D1i >>  0) & 0xff;
   ci->caches[0] = VEX_CACHE_INIT(DATA_CACHE, 1, size, line_size, assoc);

   // I1
   size      = (I1i >> 24) & 0xff;
   assoc     = (I1i >> 16) & 0xff;
   line_size = (I1i >>  0) & 0xff;
   ci->caches[1] = VEX_CACHE_INIT(INSN_CACHE, 1, size, line_size, assoc);

   // L2    Nb: different bits used for L2
   size      = (L2i >> 16) & 0xffff;
   assoc     = decode_AMD_cache_L2_L3_assoc((L2i >> 12) & 0xf);
   line_size = (L2i >>  0) & 0xff;
   ci->caches[2] = VEX_CACHE_INIT(UNIFIED_CACHE, 2, size, line_size, assoc);

   // L3, if any
   if (((L3i >> 18) & 0x3fff) > 0) {
      /* There's an L3 cache. */
      /* NB: the test in the if is "if L3 size > 0 ".  I don't know if
         this is the right way to test presence-vs-absence of L3.  I
         can't see any guidance on this in the AMD documentation. */
      size      = ((L3i >> 18) & 0x3fff) * 512;
      assoc     = decode_AMD_cache_L2_L3_assoc((L3i >> 12) & 0xf);
      line_size = (L3i >>  0) & 0xff;
      ci->caches[3] = VEX_CACHE_INIT(UNIFIED_CACHE, 3, size, line_size, assoc);
   }

   return 0;
}

static Int
get_caches_from_CPUID(VexCacheInfo *ci)
{
   Int  ret, i;
   UInt level;
   HChar vendor_id[13];

   vg_assert(VG_(has_cpuid)());

   VG_(cpuid)(0, 0, &level, (UInt*)&vendor_id[0],
	      (UInt*)&vendor_id[8], (UInt*)&vendor_id[4]);
   vendor_id[12] = '\0';

   if (0 == level) {    // CPUID level is 0, early Pentium?
      return -1;
   }

   /* Only handling Intel and AMD chips... no Cyrix, Transmeta, etc */
   if (0 == VG_(strcmp)(vendor_id, "GenuineIntel")) {
      ret = Intel_cache_info(level, ci);

   } else if (0 == VG_(strcmp)(vendor_id, "AuthenticAMD")) {
      ret = AMD_cache_info(ci);

   } else if (0 == VG_(strcmp)(vendor_id, "CentaurHauls")) {
      /* Total kludge.  Pretend to be a VIA Nehemiah. */
      ci->num_levels = 2;
      ci->num_caches = 3;
      ci->icaches_maintain_coherence = True;
      ci->caches = VG_(malloc)("m_cache", ci->num_caches * sizeof *ci->caches);
      ci->caches[0] = VEX_CACHE_INIT(DATA_CACHE,    1, 64, 16, 16);
      ci->caches[1] = VEX_CACHE_INIT(INSN_CACHE,    1, 64, 16,  4);
      ci->caches[2] = VEX_CACHE_INIT(UNIFIED_CACHE, 2, 64, 16, 16);

      ret = 0;

   } else {
      VG_(debugLog)(1, "cache", "CPU vendor ID not recognised (%s)\n",
                    vendor_id);
      return -1;
   }

   /* Successful!  Convert sizes from KB to bytes */
   for (i = 0; i < ci->num_caches; ++i) {
      ci->caches[i].sizeB *= 1024;
   }

   return ret;
}

static Bool
get_cache_info(VexArchInfo *vai)
{
   Int ret = get_caches_from_CPUID(&vai->hwcache_info); 

   return ret == 0 ? True : False;
}

#elif defined(VGA_arm) || defined(VGA_ppc32)    || \
   defined(VGA_ppc64be) || defined(VGA_ppc64le) || \
   defined(VGA_mips32) || defined(VGA_mips64) || \
   defined(VGA_arm64) || defined(VGA_nanomips)
static Bool
get_cache_info(VexArchInfo *vai)
{
   vai->hwcache_info.icaches_maintain_coherence = False;

   return False;   // not yet
}

#elif defined(VGA_s390x)

static ULong
ecag(UInt ai, UInt li, UInt ti)
{
   register ULong result asm("2") = 0;
   register ULong input  asm("3") = (ai << 4) | (li << 1) | ti;

   asm volatile(".short 0xeb20\n\t"
                ".long  0x3000004c\n\t"
                 : "=d" (result) : "d" (input));

   return result;
}

static UInt
get_cache_info_for_level(ULong topology, UInt level)
{
   return (topology >> (56 - level * 8)) & 0xff;
}

static ULong
get_line_size(UInt level, Bool is_insn_cache)
{
   return ecag(1, level, is_insn_cache);
}

static ULong
get_total_size(UInt level, Bool is_insn_cache)
{
   return ecag(2, level, is_insn_cache);
}

static ULong
get_associativity(UInt level, Bool is_insn_cache)
{
   return ecag(3, level, is_insn_cache);
}

static VexCache
get_cache(UInt level, VexCacheKind kind)
{
   Bool is_insn_cache = kind == INSN_CACHE;
   UInt size = get_total_size(level, is_insn_cache);
   UInt line_size = get_line_size(level, is_insn_cache);
   UInt assoc = get_associativity(level, is_insn_cache);

   return VEX_CACHE_INIT(kind, level + 1, size, line_size, assoc);
}

static Bool
get_cache_info(VexArchInfo *vai)
{
   VexCacheInfo *ci = &vai->hwcache_info;

   ci->icaches_maintain_coherence = True;

   if (! (vai->hwcaps & VEX_HWCAPS_S390X_GIE)) {
      // ECAG is not available
      return False;
   }

   UInt level, cache_kind, info, i;
   ULong topology = ecag(0, 0, 0);   // get summary

   /* ECAG supports at most 8 levels of cache. Find out how many levels
      of cache and how many caches there are. */
   ci->num_levels = 0;
   ci->num_caches = 0;
   for (level = 0; level < 8; level++) {
      info = get_cache_info_for_level(topology, level);

      if ((info & 0xc) == 0) break;  // cache does not exist at this level
      ++ci->num_levels;

      cache_kind = info & 0x3;
      switch (cache_kind) {
      case 0:  ci->num_caches += 2; break; /* separate data and insn cache */
      case 1:  ci->num_caches += 1; break; /* only insn cache */
      case 2:  ci->num_caches += 1; break; /* only data cache */
      case 3:  ci->num_caches += 1; break; /* unified data and insn cache */
      }
   }

   ci->caches = VG_(malloc)("m_cache", ci->num_caches * sizeof *ci->caches);

   i = 0;
   for (level = 0; level < ci->num_levels; level++) {
      info = get_cache_info_for_level(topology, level);
      cache_kind = info & 0x3;
      switch (cache_kind) {
      case 0:   /* separate data and insn cache */
         ci->caches[i++] = get_cache(level, INSN_CACHE);
         ci->caches[i++] = get_cache(level, DATA_CACHE);
         break;

      case 1:   /* only insn cache */
         ci->caches[i++] = get_cache(level, INSN_CACHE);
         break;

      case 2:   /* only data cache */
         ci->caches[i++] = get_cache(level, DATA_CACHE);
         break;

      case 3:   /* unified data and insn cache */
         ci->caches[i++] = get_cache(level, UNIFIED_CACHE);
         break;
      }
   }
   return True;
}

#else

#error "Unknown arch"

#endif

/* Debug information */
static void
write_cache_info(const VexCacheInfo *ci)
{
   UInt i;

   VG_(debugLog)(1, "cache", "Cache info:\n");
   VG_(debugLog)(1, "cache", "  #levels = %u\n", ci->num_levels);
   VG_(debugLog)(1, "cache", "  #caches = %u\n", ci->num_caches);
   for (i = 0; i < ci->num_caches; ++i) {
      VexCache *c = ci->caches + i;
      const HChar *kind;
      VG_(debugLog)(1, "cache", "     cache #%u:\n", i);
      switch (c->kind) {
      case INSN_CACHE:    kind = "insn";    break;
      case DATA_CACHE:    kind = "data";    break;
      case UNIFIED_CACHE: kind = "unified"; break;
      default: kind = "unknown"; break;
      }
      VG_(debugLog)(1, "cache", "        kind = %s\n", kind);
      VG_(debugLog)(1, "cache", "        level = %u\n", c->level);
      VG_(debugLog)(1, "cache", "        size = %u bytes\n", c->sizeB);
      VG_(debugLog)(1, "cache", "        linesize = %u bytes\n", c->line_sizeB);
      VG_(debugLog)(1, "cache", "        assoc = %u\n", c->assoc);
   }
}

static Bool
cache_info_is_sensible(const VexCacheInfo *ci)
{
   UInt level, i;
   Bool sensible = True;

   /* There must be at most one cache of a given kind at the same level.
      If there is a unified cache at a given level, no other cache may
      exist at that level. */
   for (level = 1; level <= ci->num_levels; ++level) {
      UInt num_icache, num_dcache, num_ucache;

      num_icache = num_dcache = num_ucache = 0;
      for (i = 0; i < ci->num_caches; ++i) {
         if (ci->caches[i].level == level) {
            switch (ci->caches[i].kind) {
            case INSN_CACHE:    ++num_icache; break;
            case DATA_CACHE:    ++num_dcache; break;
            case UNIFIED_CACHE: ++num_ucache; break;
            }
         }
      }
      if (num_icache == 0 && num_dcache == 0 && num_ucache == 0) {
         VG_(debugLog)(1, "cache", "warning: No caches at level %u\n", level);
         sensible = False;
      }
      if (num_icache > 1 || num_dcache > 1 || num_ucache > 1) {
         VG_(debugLog)(1, "cache", "warning: More than one cache of a given "
                       "kind at level %u\n", level);
         sensible = False;
      }
      if (num_ucache != 0 && (num_icache > 0 || num_dcache > 0)) {
         VG_(debugLog)(1, "cache", "warning: Unified cache and I/D cache "
                       "at level %u\n", level);
         sensible = False;
      }
   }

   /* If there is a cache at level N > 1 there must be a cache at level N-1 */
   for (level = 2; level <= ci->num_levels; ++level) {
      Bool found = False;
      for (i = 0; i < ci->num_caches; ++i) {
         if (ci->caches[i].level == level - 1) {
            found = True;
            break;
         }
      }
      if (! found) {
         VG_(debugLog)(1, "cache", "warning: Cache at level %u but no cache "
                       "at level %u\n", level, level - 1);
         sensible = False;
      }
   }

   return sensible;
}


/* Autodetect the cache information for this host and stuff it into
   VexArchInfo::hwcache_info. Return True if successful. */
Bool
VG_(machine_get_cache_info)(VexArchInfo *vai)
{
   Bool ok = get_cache_info(vai);

   VexCacheInfo *ci = &vai->hwcache_info;

   if (! ok) {
      VG_(debugLog)(1, "cache", "Could not autodetect cache info\n");
   } else {
      ok = cache_info_is_sensible(ci);

      if (! ok) {
         VG_(debugLog)(1, "cache",
                       "Autodetected cache info is not sensible\n");
      } else {
         VG_(debugLog)(1, "cache",
                       "Autodetected cache info is sensible\n");
      }
      write_cache_info(ci);  /* write out for debugging */
   }

   if (! ok ) {
      /* Reset cache info */
      ci->num_levels = 0;
      ci->num_caches = 0;
      VG_(free)(ci->caches);
      ci->caches = NULL;
   }

   return ok;
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
