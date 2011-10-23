
/*--------------------------------------------------------------------*/
/*--- x86- and AMD64-specific definitions.          cg-x86-amd64.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Cachegrind, a Valgrind tool for cache
   profiling programs.

   Copyright (C) 2002-2011 Nicholas Nethercote
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

#if defined(VGA_x86) || defined(VGA_amd64)

#include "pub_tool_basics.h"
#include "pub_tool_cpuid.h"
#include "pub_tool_libcbase.h"
#include "pub_tool_libcassert.h"
#include "pub_tool_libcprint.h"

#include "cg_arch.h"

// All CPUID info taken from sandpile.org/ia32/cpuid.htm */
// Probably only works for Intel and AMD chips, and probably only for some of
// them. 

static void micro_ops_warn(Int actual_size, Int used_size, Int line_size)
{
   VG_(dmsg)("warning: Pentium 4 with %d KB micro-op instruction trace cache\n", 
             actual_size);
   VG_(dmsg)("         Simulating a %d KB I-cache with %d B lines\n", 
             used_size, line_size);
}

/* Intel method is truly wretched.  We have to do an insane indexing into an
 * array of pre-defined configurations for various parts of the memory
 * hierarchy.
 * According to Intel Processor Identification, App Note 485.
 * 
 * If a L3 cache is found, then data for it rather than the L2
 * is returned via *LLc.
 */
static
Int Intel_cache_info(Int level, cache_t* I1c, cache_t* D1c, cache_t* LLc)
{
   Int cpuid1_eax;
   Int cpuid1_ignore;
   Int family;
   Int model;
   UChar info[16];
   Int   i, j, trials;
   Bool  L2_found = False;
   /* If we see L3 cache info, copy it into L3c.  Then, at the end,
      copy it into *LLc.  Hence if a L3 cache is specified, *LLc will
      eventually contain a description of it rather than the L2 cache.
      The use of the L3c intermediary makes this process independent
      of the order in which the cache specifications appear in
      info[]. */
   Bool  L3_found = False;
   cache_t L3c = { 0, 0, 0 };

   if (level < 2) {
      VG_(dmsg)("warning: CPUID level < 2 for Intel processor (%d)\n", level);
      return -1;
   }

   /* family/model needed to distinguish code reuse (currently 0x49) */
   VG_(cpuid)(1, 0, &cpuid1_eax, &cpuid1_ignore,
	      &cpuid1_ignore, &cpuid1_ignore);
   family = (((cpuid1_eax >> 20) & 0xff) << 4) + ((cpuid1_eax >> 8) & 0xf);
   model =  (((cpuid1_eax >> 16) & 0xf) << 4) + ((cpuid1_eax >> 4) & 0xf);

   VG_(cpuid)(2, 0, (Int*)&info[0], (Int*)&info[4], 
                    (Int*)&info[8], (Int*)&info[12]);
   trials  = info[0] - 1;   /* AL register - bits 0..7 of %eax */
   info[0] = 0x0;           /* reset AL */

   if (0 != trials) {
      VG_(dmsg)("warning: non-zero CPUID trials for Intel processor (%d)\n",
                trials);
      return -1;
   }

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

      case 0x06: *I1c = (cache_t) {  8, 4, 32 }; break;
      case 0x08: *I1c = (cache_t) { 16, 4, 32 }; break;
      case 0x09: *I1c = (cache_t) { 32, 4, 64 }; break;
      case 0x30: *I1c = (cache_t) { 32, 8, 64 }; break;

      case 0x0a: *D1c = (cache_t) {  8, 2, 32 }; break;
      case 0x0c: *D1c = (cache_t) { 16, 4, 32 }; break;
      case 0x0d: *D1c = (cache_t) { 16, 4, 64 }; break;
      case 0x0e: *D1c = (cache_t) { 24, 6, 64 }; break;
      case 0x2c: *D1c = (cache_t) { 32, 8, 64 }; break;

      /* IA-64 info -- panic! */
      case 0x10: case 0x15: case 0x1a: 
      case 0x88: case 0x89: case 0x8a: case 0x8d:
      case 0x90: case 0x96: case 0x9b:
         VG_(tool_panic)("IA-64 cache detected?!");

      /* L3 cache info. */
      case 0x22: L3c = (cache_t) { 512,    4, 64 }; L3_found = True; break;
      case 0x23: L3c = (cache_t) { 1024,   8, 64 }; L3_found = True; break;
      case 0x25: L3c = (cache_t) { 2048,   8, 64 }; L3_found = True; break;
      case 0x29: L3c = (cache_t) { 4096,   8, 64 }; L3_found = True; break;
      case 0x46: L3c = (cache_t) { 4096,   4, 64 }; L3_found = True; break;
      case 0x47: L3c = (cache_t) { 8192,   8, 64 }; L3_found = True; break;
      case 0x4a: L3c = (cache_t) { 6144,  12, 64 }; L3_found = True; break;
      case 0x4b: L3c = (cache_t) { 8192,  16, 64 }; L3_found = True; break;
      case 0x4c: L3c = (cache_t) { 12288, 12, 64 }; L3_found = True; break;
      case 0x4d: L3c = (cache_t) { 16384, 16, 64 }; L3_found = True; break;
      case 0xd0: L3c = (cache_t) { 512,    4, 64 }; L3_found = True; break;
      case 0xd1: L3c = (cache_t) { 1024,   4, 64 }; L3_found = True; break;
      case 0xd2: L3c = (cache_t) { 2048,   4, 64 }; L3_found = True; break;
      case 0xd6: L3c = (cache_t) { 1024,   8, 64 }; L3_found = True; break;
      case 0xd7: L3c = (cache_t) { 2048,   8, 64 }; L3_found = True; break;
      case 0xd8: L3c = (cache_t) { 4096,   8, 64 }; L3_found = True; break;
      case 0xdc: L3c = (cache_t) { 1536,  12, 64 }; L3_found = True; break;
      case 0xdd: L3c = (cache_t) { 3072,  12, 64 }; L3_found = True; break;
      case 0xde: L3c = (cache_t) { 6144,  12, 64 }; L3_found = True; break;
      case 0xe2: L3c = (cache_t) { 2048,  16, 64 }; L3_found = True; break;
      case 0xe3: L3c = (cache_t) { 4096,  16, 64 }; L3_found = True; break;
      case 0xe4: L3c = (cache_t) { 8192,  16, 64 }; L3_found = True; break;
      case 0xea: L3c = (cache_t) { 12288, 24, 64 }; L3_found = True; break;
      case 0xeb: L3c = (cache_t) { 18432, 24, 64 }; L3_found = True; break;
      case 0xec: L3c = (cache_t) { 24576, 24, 64 }; L3_found = True; break;

      /* Described as "MLC" in Intel documentation */
      case 0x21: *LLc = (cache_t) {  256, 8, 64 }; L2_found = True; break;

      /* These are sectored, whatever that means */
      case 0x39: *LLc = (cache_t) {  128, 4, 64 }; L2_found = True; break;
      case 0x3c: *LLc = (cache_t) {  256, 4, 64 }; L2_found = True; break;

      /* If a P6 core, this means "no L2 cache".  
         If a P4 core, this means "no L3 cache".
         We don't know what core it is, so don't issue a warning.  To detect
         a missing L2 cache, we use 'L2_found'. */
      case 0x40:
          break;

      case 0x41: *LLc = (cache_t) {  128,  4, 32 }; L2_found = True; break;
      case 0x42: *LLc = (cache_t) {  256,  4, 32 }; L2_found = True; break;
      case 0x43: *LLc = (cache_t) {  512,  4, 32 }; L2_found = True; break;
      case 0x44: *LLc = (cache_t) { 1024,  4, 32 }; L2_found = True; break;
      case 0x45: *LLc = (cache_t) { 2048,  4, 32 }; L2_found = True; break;
      case 0x48: *LLc = (cache_t) { 3072, 12, 64 }; L2_found = True; break;
      case 0x4e: *LLc = (cache_t) { 6144, 24, 64 }; L2_found = True; break;
      case 0x49:
         if (family == 15 && model == 6) {
            /* On Xeon MP (family F, model 6), this is for L3 */
            L3c = (cache_t) { 4096, 16, 64 }; L3_found = True;
         } else {
	    *LLc = (cache_t) { 4096, 16, 64 }; L2_found = True;
         }
         break;

      /* These are sectored, whatever that means */
      case 0x60: *D1c = (cache_t) { 16, 8, 64 };  break;      /* sectored */
      case 0x66: *D1c = (cache_t) {  8, 4, 64 };  break;      /* sectored */
      case 0x67: *D1c = (cache_t) { 16, 4, 64 };  break;      /* sectored */
      case 0x68: *D1c = (cache_t) { 32, 4, 64 };  break;      /* sectored */

      /* HACK ALERT: Instruction trace cache -- capacity is micro-ops based.
       * conversion to byte size is a total guess;  treat the 12K and 16K
       * cases the same since the cache byte size must be a power of two for
       * everything to work!.  Also guessing 32 bytes for the line size... 
       */
      case 0x70:    /* 12K micro-ops, 8-way */
         *I1c = (cache_t) { 16, 8, 32 };  
         micro_ops_warn(12, 16, 32);
         break;  
      case 0x71:    /* 16K micro-ops, 8-way */
         *I1c = (cache_t) { 16, 8, 32 };  
         micro_ops_warn(16, 16, 32); 
         break;  
      case 0x72:    /* 32K micro-ops, 8-way */
         *I1c = (cache_t) { 32, 8, 32 };  
         micro_ops_warn(32, 32, 32); 
         break;  

      /* not sectored, whatever that might mean */
      case 0x78: *LLc = (cache_t) { 1024, 4,  64 }; L2_found = True;  break;

      /* These are sectored, whatever that means */
      case 0x79: *LLc = (cache_t) {  128, 8,  64 }; L2_found = True;  break;
      case 0x7a: *LLc = (cache_t) {  256, 8,  64 }; L2_found = True;  break;
      case 0x7b: *LLc = (cache_t) {  512, 8,  64 }; L2_found = True;  break;
      case 0x7c: *LLc = (cache_t) { 1024, 8,  64 }; L2_found = True;  break;
      case 0x7d: *LLc = (cache_t) { 2048, 8,  64 }; L2_found = True;  break;
      case 0x7e: *LLc = (cache_t) {  256, 8, 128 }; L2_found = True;  break;
      case 0x7f: *LLc = (cache_t) {  512, 2,  64 }; L2_found = True;  break;
      case 0x80: *LLc = (cache_t) {  512, 8,  64 }; L2_found = True;  break;
      case 0x81: *LLc = (cache_t) {  128, 8,  32 }; L2_found = True;  break;
      case 0x82: *LLc = (cache_t) {  256, 8,  32 }; L2_found = True;  break;
      case 0x83: *LLc = (cache_t) {  512, 8,  32 }; L2_found = True;  break;
      case 0x84: *LLc = (cache_t) { 1024, 8,  32 }; L2_found = True;  break;
      case 0x85: *LLc = (cache_t) { 2048, 8,  32 }; L2_found = True;  break;
      case 0x86: *LLc = (cache_t) {  512, 4,  64 }; L2_found = True;  break;
      case 0x87: *LLc = (cache_t) { 1024, 8,  64 }; L2_found = True;  break;

      /* Ignore prefetch information */
      case 0xf0: case 0xf1:
         break;

      case 0xff:
         j = 0;
         VG_(cpuid)(4, j++, (Int*)&info[0], (Int*)&info[4], 
                            (Int*)&info[8], (Int*)&info[12]);

         while ((info[0] & 0x1f) != 0) {
            UInt assoc = ((*(UInt *)&info[4] >> 22) & 0x3ff) + 1;
            UInt parts = ((*(UInt *)&info[4] >> 12) & 0x3ff) + 1;
            UInt line_size = (*(UInt *)&info[4] & 0x7ff) + 1;
            UInt sets = *(UInt *)&info[8] + 1;
            cache_t c;

            c.size = assoc * parts * line_size * sets / 1024;
            c.assoc = assoc;
            c.line_size = line_size;

            switch ((info[0] & 0xe0) >> 5)
            {
            case 1:
               switch (info[0] & 0x1f)
               {
               case 1: *D1c = c; break;
               case 2: *I1c = c; break;
               case 3: VG_(dmsg)("warning: L1 unified cache ignored\n"); break;
               default: VG_(dmsg)("warning: L1 cache of unknown type ignored\n"); break;
               }
               break;
            case 2:
               switch (info[0] & 0x1f)
               {
               case 1: VG_(dmsg)("warning: L2 data cache ignored\n"); break;
               case 2: VG_(dmsg)("warning: L2 instruction cache ignored\n"); break;
               case 3: *LLc = c; L2_found = True; break;
               default: VG_(dmsg)("warning: L2 cache of unknown type ignored\n"); break;
               }
               break;
            case 3:
               switch (info[0] & 0x1f)
               {
               case 1: VG_(dmsg)("warning: L3 data cache ignored\n"); break;
               case 2: VG_(dmsg)("warning: L3 instruction cache ignored\n"); break;
               case 3: L3c = c; L3_found = True; break;
               default: VG_(dmsg)("warning: L3 cache of unknown type ignored\n"); break;
               }
               break;
            default:
               VG_(dmsg)("warning: L%u cache ignored\n", (info[0] & 0xe0) >> 5);
               break;
            }

            VG_(cpuid)(4, j++, (Int*)&info[0], (Int*)&info[4], 
                               (Int*)&info[8], (Int*)&info[12]);
         }
         break;

      default:
         VG_(dmsg)("warning: Unknown Intel cache config value (0x%x), ignoring\n",
                   info[i]);
         break;
      }
   }

   /* If we found a L3 cache, throw away the L2 data and use the L3's instead. */
   if (L3_found) {
      VG_(dmsg)("warning: L3 cache found, using its data for the LL simulation.\n");
      *LLc = L3c;
      L2_found = True;
   }

   if (!L2_found)
      VG_(dmsg)("warning: L2 cache not installed, ignore LL results.\n");

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
static Int decode_AMD_cache_L2_L3_assoc ( Int bits_15_12 )
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

static
Int AMD_cache_info(cache_t* I1c, cache_t* D1c, cache_t* LLc)
{
   UInt ext_level;
   UInt dummy, model;
   UInt I1i, D1i, L2i, L3i;
   
   VG_(cpuid)(0x80000000, 0, &ext_level, &dummy, &dummy, &dummy);

   if (0 == (ext_level & 0x80000000) || ext_level < 0x80000006) {
      VG_(dmsg)("warning: ext_level < 0x80000006 for AMD processor (0x%x)\n", 
                ext_level);
      return -1;
   }

   VG_(cpuid)(0x80000005, 0, &dummy, &dummy, &D1i, &I1i);
   VG_(cpuid)(0x80000006, 0, &dummy, &dummy, &L2i, &L3i);

   VG_(cpuid)(0x1, 0, &model, &dummy, &dummy, &dummy);

   /* Check for Duron bug */
   if (model == 0x630) {
      VG_(dmsg)("warning: Buggy Duron stepping A0. Assuming L2 size=65536 bytes\n");
      L2i = (64 << 16) | (L2i & 0xffff);
   }

   D1c->size      = (D1i >> 24) & 0xff;
   D1c->assoc     = (D1i >> 16) & 0xff;
   D1c->line_size = (D1i >>  0) & 0xff;

   I1c->size      = (I1i >> 24) & 0xff;
   I1c->assoc     = (I1i >> 16) & 0xff;
   I1c->line_size = (I1i >>  0) & 0xff;

   LLc->size      = (L2i >> 16) & 0xffff; /* Nb: different bits used for L2 */
   LLc->assoc     = decode_AMD_cache_L2_L3_assoc((L2i >> 12) & 0xf);
   LLc->line_size = (L2i >>  0) & 0xff;

   if (((L3i >> 18) & 0x3fff) > 0) {
      /* There's an L3 cache.  Replace *LLc contents with this info. */
      /* NB: the test in the if is "if L3 size > 0 ".  I don't know if
         this is the right way to test presence-vs-absence of L3.  I
         can't see any guidance on this in the AMD documentation. */
      LLc->size      = ((L3i >> 18) & 0x3fff) * 512;
      LLc->assoc     = decode_AMD_cache_L2_L3_assoc((L3i >> 12) & 0xf);
      LLc->line_size = (L3i >>  0) & 0xff;
      VG_(dmsg)("warning: L3 cache found, using its data for the L2 simulation.\n");
   }

   return 0;
}

static 
Int get_caches_from_CPUID(cache_t* I1c, cache_t* D1c, cache_t* LLc)
{
   Int  level, ret;
   Char vendor_id[13];

   if (!VG_(has_cpuid)()) {
      VG_(dmsg)("CPUID instruction not supported\n");
      return -1;
   }

   VG_(cpuid)(0, 0, &level, (int*)&vendor_id[0], 
	      (int*)&vendor_id[8], (int*)&vendor_id[4]);    
   vendor_id[12] = '\0';

   if (0 == level) {
      VG_(dmsg)("CPUID level is 0, early Pentium?\n");
      return -1;
   }

   /* Only handling Intel and AMD chips... no Cyrix, Transmeta, etc */
   if (0 == VG_(strcmp)(vendor_id, "GenuineIntel")) {
      ret = Intel_cache_info(level, I1c, D1c, LLc);

   } else if (0 == VG_(strcmp)(vendor_id, "AuthenticAMD")) {
      ret = AMD_cache_info(I1c, D1c, LLc);

   } else if (0 == VG_(strcmp)(vendor_id, "CentaurHauls")) {
      /* Total kludge.  Pretend to be a VIA Nehemiah. */
      D1c->size      = 64;
      D1c->assoc     = 16;
      D1c->line_size = 16;
      I1c->size      = 64;
      I1c->assoc     = 4;
      I1c->line_size = 16;
      LLc->size      = 64;
      LLc->assoc     = 16;
      LLc->line_size = 16;
      ret = 0;

   } else {
      VG_(dmsg)("CPU vendor ID not recognised (%s)\n", vendor_id);
      return -1;
   }

   /* Successful!  Convert sizes from KB to bytes */
   I1c->size *= 1024;
   D1c->size *= 1024;
   LLc->size *= 1024;

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
      sets.  The "fix" in this case is to increase the associativity
      by 50% to 24, which reduces the number of sets to 8192, making
      it a power of 2.  That's what the following code does (handing
      the "3/2 rescaling case".)  We might need to deal with other
      ratios later (5/4 ?).

      The "fix" is "justified" (cough, cough) by alleging that
      increases of associativity above about 4 have very little effect
      on the actual miss rate.  It would be far more inaccurate to
      fudge this by changing the size of the simulated cache --
      changing the associativity is a much better option.
   */
   if (LLc->size > 0 && LLc->assoc > 0 && LLc->line_size > 0) {
      Long nSets = (Long)LLc->size / (Long)(LLc->line_size * LLc->assoc);
      if (/* stay sane */
          nSets >= 4
          /* nSets is not a power of 2 */
          && VG_(log2_64)( (ULong)nSets ) == -1
          /* nSets is 50% above a power of 2 */
          && VG_(log2_64)( (ULong)((2 * nSets) / (Long)3) ) != -1
          /* associativity can be increased by exactly 50% */
          && (LLc->assoc % 2) == 0
         ) {
         /* # sets is 1.5 * a power of two, but the associativity is
            even, so we can increase that up by 50% and implicitly
            scale the # sets down accordingly. */
         Int new_assoc = LLc->assoc + (LLc->assoc / 2);
         VG_(dmsg)("warning: pretending that LL cache has associativity"
                   " %d instead of actual %d\n", new_assoc, LLc->assoc);
         LLc->assoc = new_assoc;
      }
   }

   return ret;
}


void VG_(configure_caches)(cache_t* I1c, cache_t* D1c, cache_t* LLc,
                           Bool all_caches_clo_defined)
{
   Int res;
   
   // Set caches to default.
   *I1c = (cache_t) {  65536, 2, 64 };
   *D1c = (cache_t) {  65536, 2, 64 };
   *LLc = (cache_t) { 262144, 8, 64 };

   // Then replace with any info we can get from CPUID.
   res = get_caches_from_CPUID(I1c, D1c, LLc);

   // Warn if CPUID failed and config not completely specified from cmd line.
   if (res != 0 && !all_caches_clo_defined) {
      VG_(dmsg)("Warning: Couldn't auto-detect cache config, using one "
                "or more defaults \n");
   }
}

#endif // defined(VGA_x86) || defined(VGA_amd64)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
