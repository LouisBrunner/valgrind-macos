
/*--------------------------------------------------------------------*/
/*--- x86-specific definitions.                      x86/cg_arch.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Cachegrind, a Valgrind tool for cache
   profiling programs.

   Copyright (C) 2002-2005 Nicholas Nethercote
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

#include "tool.h"
#include "cg_arch.h"

// All CPUID info taken from sandpile.org/a32/cpuid.htm */
// Probably only works for Intel and AMD chips, and probably only for some of
// them. 

#if 0
static void micro_ops_warn(Int actual_size, Int used_size, Int line_size)
{
    VG_(message)(Vg_DebugMsg, 
       "warning: Pentium with %d K micro-op instruction trace cache", 
       actual_size);
    VG_(message)(Vg_DebugMsg, 
       "         Simulating a %d KB cache with %d B lines", 
       used_size, line_size);
}

/* Intel method is truly wretched.  We have to do an insane indexing into an
 * array of pre-defined configurations for various parts of the memory
 * hierarchy. 
 */
static
Int Intel_cache_info(Int level, cache_t* I1c, cache_t* D1c, cache_t* L2c)
{
   UChar info[16];
   Int   i, trials;
   Bool  L2_found = False;

   if (level < 2) {
      VG_(message)(Vg_DebugMsg, 
         "warning: CPUID level < 2 for Intel processor (%d)", 
         level);
      return -1;
   }

   VG_(cpuid)(2, (Int*)&info[0], (Int*)&info[4], 
                 (Int*)&info[8], (Int*)&info[12]);
   trials  = info[0] - 1;   /* AL register - bits 0..7 of %eax */
   info[0] = 0x0;           /* reset AL */

   if (0 != trials) {
      VG_(message)(Vg_DebugMsg, 
         "warning: non-zero CPUID trials for Intel processor (%d)",
         trials);
      return -1;
   }

   for (i = 0; i < 16; i++) {

      switch (info[i]) {

      case 0x0:       /* ignore zeros */
          break;
          
      /* TLB info, ignore */
      case 0x01: case 0x02: case 0x03: case 0x04:
      case 0x50: case 0x51: case 0x52: case 0x5b: case 0x5c: case 0x5d:
      case 0xb0: case 0xb3:
          break;      

      case 0x06: *I1c = (cache_t) {  8, 4, 32 }; break;
      case 0x08: *I1c = (cache_t) { 16, 4, 32 }; break;
      case 0x30: *I1c = (cache_t) { 32, 8, 64 }; break;

      case 0x0a: *D1c = (cache_t) {  8, 2, 32 }; break;
      case 0x0c: *D1c = (cache_t) { 16, 4, 32 }; break;
      case 0x2c: *D1c = (cache_t) { 32, 8, 64 }; break;

      /* IA-64 info -- panic! */
      case 0x10: case 0x15: case 0x1a: 
      case 0x88: case 0x89: case 0x8a: case 0x8d:
      case 0x90: case 0x96: case 0x9b:
         VG_(tool_panic)("IA-64 cache detected?!");

      case 0x22: case 0x23: case 0x25: case 0x29: 
          VG_(message)(Vg_DebugMsg, 
             "warning: L3 cache detected but ignored\n");
          break;

      /* These are sectored, whatever that means */
      case 0x39: *L2c = (cache_t) {  128, 4, 64 }; L2_found = True; break;
      case 0x3c: *L2c = (cache_t) {  256, 4, 64 }; L2_found = True; break;

      /* If a P6 core, this means "no L2 cache".  
         If a P4 core, this means "no L3 cache".
         We don't know what core it is, so don't issue a warning.  To detect
         a missing L2 cache, we use 'L2_found'. */
      case 0x40:
          break;

      case 0x41: *L2c = (cache_t) {  128, 4, 32 }; L2_found = True; break;
      case 0x42: *L2c = (cache_t) {  256, 4, 32 }; L2_found = True; break;
      case 0x43: *L2c = (cache_t) {  512, 4, 32 }; L2_found = True; break;
      case 0x44: *L2c = (cache_t) { 1024, 4, 32 }; L2_found = True; break;
      case 0x45: *L2c = (cache_t) { 2048, 4, 32 }; L2_found = True; break;

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

      /* These are sectored, whatever that means */
      case 0x79: *L2c = (cache_t) {  128, 8,  64 }; L2_found = True;  break;
      case 0x7a: *L2c = (cache_t) {  256, 8,  64 }; L2_found = True;  break;
      case 0x7b: *L2c = (cache_t) {  512, 8,  64 }; L2_found = True;  break;
      case 0x7c: *L2c = (cache_t) { 1024, 8,  64 }; L2_found = True;  break;
      case 0x7e: *L2c = (cache_t) {  256, 8, 128 }; L2_found = True;  break;

      case 0x81: *L2c = (cache_t) {  128, 8, 32 };  L2_found = True;  break;
      case 0x82: *L2c = (cache_t) {  256, 8, 32 };  L2_found = True;  break;
      case 0x83: *L2c = (cache_t) {  512, 8, 32 };  L2_found = True;  break;
      case 0x84: *L2c = (cache_t) { 1024, 8, 32 };  L2_found = True;  break;
      case 0x85: *L2c = (cache_t) { 2048, 8, 32 };  L2_found = True;  break;
      case 0x86: *L2c = (cache_t) {  512, 4, 64 };  L2_found = True;  break;
      case 0x87: *L2c = (cache_t) { 1024, 8, 64 };  L2_found = True;  break;

      default:
          VG_(message)(Vg_DebugMsg, 
             "warning: Unknown Intel cache config value "
             "(0x%x), ignoring", info[i]);
          break;
      }
   }

   if (!L2_found)
      VG_(message)(Vg_DebugMsg, 
         "warning: L2 cache not installed, ignore L2 results.");

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
 * Returns 0 on success, non-zero on failure.
 */
static
Int AMD_cache_info(cache_t* I1c, cache_t* D1c, cache_t* L2c)
{
   UInt ext_level;
   UInt dummy, model;
   UInt I1i, D1i, L2i;
   
   VG_(cpuid)(0x80000000, &ext_level, &dummy, &dummy, &dummy);

   if (0 == (ext_level & 0x80000000) || ext_level < 0x80000006) {
      VG_(message)(Vg_UserMsg, 
         "warning: ext_level < 0x80000006 for AMD processor (0x%x)", 
         ext_level);
      return -1;
   }

   VG_(cpuid)(0x80000005, &dummy, &dummy, &D1i, &I1i);
   VG_(cpuid)(0x80000006, &dummy, &dummy, &L2i, &dummy);

   VG_(cpuid)(0x1, &model, &dummy, &dummy, &dummy);

   /* Check for Duron bug */
   if (model == 0x630) {
      VG_(message)(Vg_UserMsg,
         "Buggy Duron stepping A0. Assuming L2 size=65536 bytes");
      L2i = (64 << 16) | (L2i & 0xffff);
   }

   D1c->size      = (D1i >> 24) & 0xff;
   D1c->assoc     = (D1i >> 16) & 0xff;
   D1c->line_size = (D1i >>  0) & 0xff;

   I1c->size      = (I1i >> 24) & 0xff;
   I1c->assoc     = (I1i >> 16) & 0xff;
   I1c->line_size = (I1i >>  0) & 0xff;

   L2c->size      = (L2i >> 16) & 0xffff; /* Nb: different bits used for L2 */
   L2c->assoc     = (L2i >> 12) & 0xf;
   L2c->line_size = (L2i >>  0) & 0xff;

   return 0;
}
#endif

static 
Int get_caches_from_CPUID(cache_t* I1c, cache_t* D1c, cache_t* L2c)
{
   VG_(message)(Vg_DebugMsg, "Can't auto-detect CPUID yet, giving up");
   return -1;
#if 0
   Int  level, ret;
   Char vendor_id[13];
//   tl_assert(res == 0);
//
//   res = VG_(sigaction)( VKI_SIGILL, &sigill_new, &sigill_saved );
//   tl_assert(res == 0);

   if (!VG_(has_cpuid)()) {
      VG_(message)(Vg_DebugMsg, "CPUID instruction not supported");
      return -1;
   }
   VG_(cpuid)(0, &level, (int*)&vendor_id[0], 
	      (int*)&vendor_id[8], (int*)&vendor_id[4]);    
   vendor_id[12] = '\0';

   if (0 == level) {
      VG_(message)(Vg_DebugMsg, "CPUID level is 0, early Pentium?\n");
      return -1;
   }

   /* Only handling Intel and AMD chips... no Cyrix, Transmeta, etc */
   if (0 == VG_(strcmp)(vendor_id, "GenuineIntel")) {
      ret = Intel_cache_info(level, I1c, D1c, L2c);

   } else if (0 == VG_(strcmp)(vendor_id, "AuthenticAMD")) {
      ret = AMD_cache_info(I1c, D1c, L2c);

   } else if (0 == VG_(strcmp)(vendor_id, "CentaurHauls")) {
      /* Total kludge.  Pretend to be a VIA Nehemiah. */
      D1c->size      = 64;
      D1c->assoc     = 16;
      D1c->line_size = 16;
      I1c->size      = 64;
      I1c->assoc     = 4;
      I1c->line_size = 16;
      L2c->size      = 64;
      L2c->assoc     = 16;
      L2c->line_size = 16;
      ret = 0;

   } else {
      VG_(message)(Vg_DebugMsg, "CPU vendor ID not recognised (%s)",
                   vendor_id);
      return -1;
   }

   /* Successful!  Convert sizes from KB to bytes */
   I1c->size *= 1024;
   D1c->size *= 1024;
   L2c->size *= 1024;
      
   return ret;
#endif
}


void VGA_(configure_caches)(cache_t* I1c, cache_t* D1c, cache_t* L2c,
                            Bool all_caches_clo_defined)
{
   Int res;
   
   // Set caches to default.
   *I1c = (cache_t) {  65536, 2, 64 };
   *D1c = (cache_t) {  65536, 2, 64 };
   *L2c = (cache_t) { 262144, 8, 64 };

   // Then replace with any info we can get from CPUID.
   res = get_caches_from_CPUID(I1c, D1c, L2c);

   // Warn if CPUID failed and config not completely specified from cmd line.
   if (res != 0 && !all_caches_clo_defined) {
      VG_(message)(Vg_DebugMsg, 
                   "Warning: Couldn't auto-detect cache config, using one "
                   "or more defaults ");
   }
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
