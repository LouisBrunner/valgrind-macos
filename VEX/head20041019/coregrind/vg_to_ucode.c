
/*--------------------------------------------------------------------*/
/*--- The JITter: translate x86 code to ucode.                     ---*/
/*---                                                vg_to_ucode.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2004 Julian Seward
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
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

#include "core.h"

/*------------------------------------------------------------*/
/*--- CPU feature set stuff                                ---*/
/*--- This is a little out of place here, but it will do   ---*/
/*--- for now.                                             ---*/
/*------------------------------------------------------------*/

#define VG_CPU_VENDOR_GENERIC   0
#define VG_CPU_VENDOR_INTEL     1
#define VG_CPU_VENDOR_AMD       2

/* Standard macro to see if a specific flag is changeable */
static inline Bool flag_is_changeable(UInt flag)
{
   UInt f1, f2;

   asm("pushfl\n\t"
       "pushfl\n\t"
       "popl %0\n\t"
       "movl %0,%1\n\t"
       "xorl %2,%0\n\t"
       "pushl %0\n\t"
       "popfl\n\t"
       "pushfl\n\t"
       "popl %0\n\t"
       "popfl\n\t"
       : "=&r" (f1), "=&r" (f2)
       : "ir" (flag));

   return ((f1^f2) & flag) != 0;
}


/* Probe for the CPUID instruction */
static Bool has_cpuid(void)
{
   return flag_is_changeable(EFlagID);
}


// Returns the CPU features, and also the vendorid.  Only ever calls CPUID
// once, and caches the necessary info in static variables for later reuse.
static UInt* get_cpu_features(Int* cpu_vendorid_ptr)
{
   Char vendorstr[13];
   Int  i, cpuid_level;
   static Bool done_before = False;
   static Int  cpu_vendorid = VG_CPU_VENDOR_GENERIC;
   static UInt cpu_features[VG_N_FEATURE_WORDS];
   static const struct {
      const Char *vendorstr;
      Int   vendorid;
   } cpu_vendors[] = {
      { "GenuineIntel", VG_CPU_VENDOR_INTEL },
      { "AuthenticAMD", VG_CPU_VENDOR_AMD },
   };

   // If we haven't already worked this stuff out...
   if (!done_before && has_cpuid()) {

      cpu_features[VG_INT_FEAT] |= (1 << (VG_X86_FEAT_CPUID%32));

      // Get vendor string, eg. "GenuineIntel".  Note characteristically
      // stupid word order chosen by Intel.
      VG_(cpuid)(0, &cpuid_level, (UInt *)&vendorstr[0],
                                  (UInt *)&vendorstr[8],
                                  (UInt *)&vendorstr[4]);
      vendorstr[12] = '\0';

      // Determine vendor ID
      for (i = 0; i < sizeof(cpu_vendors)/sizeof(*cpu_vendors); i++)
         if (VG_(memcmp)(vendorstr, cpu_vendors[i].vendorstr, 12) == 0) {
            cpu_vendorid = cpu_vendors[i].vendorid;
            break;
         }

      // Determine CPU features
      if (cpuid_level >= 1)
         VG_(cpuid)(1, NULL, NULL, &cpu_features[VG_EXT_FEAT],
                                   &cpu_features[VG_X86_FEAT]);

      if (VG_CPU_VENDOR_AMD == cpu_vendorid) {
         /* get AMD-specific flags */
         VG_(cpuid)(0x80000001, NULL, NULL, NULL, &cpu_features[VG_AMD_FEAT]);
      }
   }
   if (NULL != cpu_vendorid_ptr) *cpu_vendorid_ptr = cpu_vendorid;
   return cpu_features;
}

Bool VG_(cpu_has_feature)(UInt feature)
{
   UInt  word = feature / 32;
   UInt  bit  = feature % 32;
   UInt* cpu_features;

   vg_assert(word >= 0 && word < VG_N_FEATURE_WORDS);

   cpu_features = get_cpu_features(NULL);

   return !!( cpu_features[word] & (1 << bit) );
}


/* The set of features we're willing to support for the client

   This includes supported instruction set extensions, plus any
   extensions which don't have any user-mode visible effect (but the
   client may find interesting).
 */
#define VG_X86_SUPPORTED_FEATURES               \
         ((1 << VG_X86_FEAT_FPU)        |       \
          (1 << VG_X86_FEAT_VME)        |       \
          (1 << VG_X86_FEAT_DE)         |       \
          (1 << VG_X86_FEAT_PSE)        |       \
          (1 << VG_X86_FEAT_TSC)        |       \
          (0 << VG_X86_FEAT_MSR)        |       \
          (1 << VG_X86_FEAT_PAE)        |       \
          (1 << VG_X86_FEAT_MCE)        |       \
          (1 << VG_X86_FEAT_CX8)        |       \
          (1 << VG_X86_FEAT_APIC)       |       \
          (0 << VG_X86_FEAT_SEP)        |       \
          (1 << VG_X86_FEAT_MTRR)       |       \
          (1 << VG_X86_FEAT_PGE)        |       \
          (1 << VG_X86_FEAT_MCA)        |       \
          (1 << VG_X86_FEAT_CMOV)       |       \
          (1 << VG_X86_FEAT_PAT)        |       \
          (1 << VG_X86_FEAT_PSE36)      |       \
          (0 << VG_X86_FEAT_CLFSH)      |       \
          (1 << VG_X86_FEAT_DS)         |       \
          (1 << VG_X86_FEAT_ACPI)       |       \
          (1 << VG_X86_FEAT_MMX)        |       \
          (1 << VG_X86_FEAT_FXSR)       |       \
          (1 << VG_X86_FEAT_SSE)        |       \
          (1 << VG_X86_FEAT_SSE2)       |       \
          (1 << VG_X86_FEAT_SS)         |       \
          (1 << VG_X86_FEAT_HT)         |       \
          (1 << VG_X86_FEAT_TM)         |       \
          (0 << VG_X86_FEAT_IA64)       |       \
          (1 << VG_X86_FEAT_PBE))

#define VG_AMD_SUPPORTED_FEATURES                                       \
        ((0 << (VG_AMD_FEAT_SYSCALL % 32))      |                       \
         (0 << (VG_AMD_FEAT_NXP % 32))          |                       \
         (1 << (VG_AMD_FEAT_MMXEXT % 32))       |                       \
         (0 << (VG_AMD_FEAT_FFXSR % 32))        |                       \
         (0 << (VG_AMD_FEAT_LONGMODE % 32))     |                       \
         (0 << (VG_AMD_FEAT_3DNOWEXT % 32))     |                       \
         (0 << (VG_AMD_FEAT_3DNOW % 32))        |                       \
         /* Common bits between standard features and AMD features */   \
         (1 << VG_X86_FEAT_FPU)         |                               \
         (1 << VG_X86_FEAT_VME)         |                               \
         (1 << VG_X86_FEAT_DE)          |                               \
         (1 << VG_X86_FEAT_PSE)         |                               \
         (1 << VG_X86_FEAT_TSC)         |                               \
         (0 << VG_X86_FEAT_MSR)         |                               \
         (1 << VG_X86_FEAT_PAE)         |                               \
         (1 << VG_X86_FEAT_MCE)         |                               \
         (1 << VG_X86_FEAT_CX8)         |                               \
         (1 << VG_X86_FEAT_APIC)        |                               \
         (1 << VG_X86_FEAT_MTRR)        |                               \
         (1 << VG_X86_FEAT_PGE)         |                               \
         (1 << VG_X86_FEAT_MCA)         |                               \
         (1 << VG_X86_FEAT_CMOV)        |                               \
         (1 << VG_X86_FEAT_PAT)         |                               \
         (1 << VG_X86_FEAT_PSE36)       |                               \
         (1 << VG_X86_FEAT_MMX)         |                               \
         (1 << VG_X86_FEAT_FXSR))


/*
   For simulating the cpuid instruction, we will
   issue a "real" cpuid instruction and then mask out
   the bits of the features we do not support currently (3dnow mostly).
   We also claim to not support most CPUID operations.

   Dirk Mueller <mueller@kde.org>

   http://www.sandpile.org/ia32/cpuid.htm

   references:

   pre-MMX pentium:

   <werner> cpuid words (0): 0x1 0x756e6547 0x6c65746e 0x49656e69
   <werner> cpuid words (1): 0x52b 0x0 0x0 0x1bf

   Updated to be more extensible about future vendor extensions and
   vendor-specific parts of CPUID.
*/
void VG_(helperc_CPUID)(UInt op, UInt *eax_ret, UInt *ebx_ret,
                                 UInt *ecx_ret, UInt *edx_ret)
{
   UInt eax, ebx, ecx, edx;
   Int  cpu_vendorid;

   // This function should not be called unless the CPU has CPUID.
   vg_assert( VG_(cpu_has_feature)(VG_X86_FEAT_CPUID) );

   // Get vendor ID.
   get_cpu_features( &cpu_vendorid );

   VG_(cpuid)(op, &eax, &ebx, &ecx, &edx);

   /* Common mangling */
   switch(op) {
   case 1:
      edx &= VG_X86_SUPPORTED_FEATURES;
      break;


   case 0xd8000000: {
      /* Implement some private information at 0xd8000000 */
      static const Char valgrind_vendor[] = "ValgrindVCPU";

      eax = 0xd8000000;         /* max request */
      ebx = *(UInt *)&valgrind_vendor[0];
      ecx = *(UInt *)&valgrind_vendor[8];
      edx = *(UInt *)&valgrind_vendor[4];
   }
      break;
   }

   /* Vendor-specific mangling of the results */
   switch (cpu_vendorid) {
   case VG_CPU_VENDOR_INTEL:
      switch(op) {
      case 1:
         ecx = 0;               /* mask out all extended features for now */
         break;

      case 0x80000001:
         ebx = ecx = edx = 0;
         break;
      }
      break;

   case VG_CPU_VENDOR_AMD:
      switch(op) {
      case 0x80000001:
         edx &= VG_AMD_SUPPORTED_FEATURES;
         break;
      }
      break;
   }

   *eax_ret = eax;
   *ebx_ret = ebx;
   *ecx_ret = ecx;
   *edx_ret = edx;
}




/*--------------------------------------------------------------------*/
/*--- end                                            vg_to_ucode.c ---*/
/*--------------------------------------------------------------------*/
