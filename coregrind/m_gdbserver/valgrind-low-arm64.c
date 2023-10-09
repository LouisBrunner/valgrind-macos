/* Low level interface to valgrind, for the remote server for GDB integrated
   in valgrind.
   Copyright (C) 2014
   Free Software Foundation, Inc.

   This file is part of VALGRIND.
   It has been inspired from a file from gdbserver in gdb 6.6.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

#include "server.h"
#include "target.h"
#include "regdef.h"
#include "regcache.h"

#include "pub_core_machine.h"
#include "pub_core_threadstate.h"
#include "pub_core_transtab.h"
#include "pub_core_gdbserver.h" 
#include "pub_core_debuginfo.h"

#include "valgrind_low.h"

#include "libvex_guest_arm64.h"

static struct reg regs[] = {
  { "x0", 0, 64 },
  { "x1", 64, 64 },
  { "x2", 128, 64 },
  { "x3", 192, 64 },
  { "x4", 256, 64 },
  { "x5", 320, 64 },
  { "x6", 384, 64 },
  { "x7", 448, 64 },
  { "x8", 512, 64 },
  { "x9", 576, 64 },
  { "x10", 640, 64 },
  { "x11", 704, 64 },
  { "x12", 768, 64 },
  { "x13", 832, 64 },
  { "x14", 896, 64 },
  { "x15", 960, 64 },
  { "x16", 1024, 64 },
  { "x17", 1088, 64 },
  { "x18", 1152, 64 },
  { "x19", 1216, 64 },
  { "x20", 1280, 64 },
  { "x21", 1344, 64 },
  { "x22", 1408, 64 },
  { "x23", 1472, 64 },
  { "x24", 1536, 64 },
  { "x25", 1600, 64 },
  { "x26", 1664, 64 },
  { "x27", 1728, 64 },
  { "x28", 1792, 64 },
  { "x29", 1856, 64 },
  { "x30", 1920, 64 },
  { "sp", 1984, 64 },
  { "pc", 2048, 64 },
  { "cpsr", 2112, 32 },
  { "v0", 2144, 128 },
  { "v1", 2272, 128 },
  { "v2", 2400, 128 },
  { "v3", 2528, 128 },
  { "v4", 2656, 128 },
  { "v5", 2784, 128 },
  { "v6", 2912, 128 },
  { "v7", 3040, 128 },
  { "v8", 3168, 128 },
  { "v9", 3296, 128 },
  { "v10", 3424, 128 },
  { "v11", 3552, 128 },
  { "v12", 3680, 128 },
  { "v13", 3808, 128 },
  { "v14", 3936, 128 },
  { "v15", 4064, 128 },
  { "v16", 4192, 128 },
  { "v17", 4320, 128 },
  { "v18", 4448, 128 },
  { "v19", 4576, 128 },
  { "v20", 4704, 128 },
  { "v21", 4832, 128 },
  { "v22", 4960, 128 },
  { "v23", 5088, 128 },
  { "v24", 5216, 128 },
  { "v25", 5344, 128 },
  { "v26", 5472, 128 },
  { "v27", 5600, 128 },
  { "v28", 5728, 128 },
  { "v29", 5856, 128 },
  { "v30", 5984, 128 },
  { "v31", 6112, 128 },
  { "fpsr", 6240, 32 },
  { "fpcr", 6272, 32 },
};

static const char *expedite_regs[] = { "x29", "sp", "pc", 0 };

#define num_regs (sizeof (regs) / sizeof (regs[0]))

static
CORE_ADDR get_pc (void)
{
   unsigned long pc;

   collect_register_by_name ("pc", &pc);
   
   dlog(1, "stop pc is %p\n", (void *) pc);
   return pc;
}

static
void set_pc (CORE_ADDR newpc)
{
   supply_register_by_name ("pc", &newpc);
}

/* store registers in the guest state (gdbserver_to_valgrind)
   or fetch register from the guest state (valgrind_to_gdbserver). */
static
void transfer_register (ThreadId tid, int abs_regno, void * buf,
                        transfer_direction dir, int size, Bool *mod)
{
   ThreadState* tst = VG_(get_ThreadState)(tid);
   int set = abs_regno / num_regs;
   int regno = abs_regno % num_regs;
   *mod = False;

   VexGuestARM64State* arm = (VexGuestARM64State*) get_arch (set, tst);

   switch (regno) { 
   // numbers here have to match the order of regs above
   // Attention: gdb order does not match valgrind order.
   case 0:  VG_(transfer) (&arm->guest_X0,   buf, dir, size, mod); break;
   case 1:  VG_(transfer) (&arm->guest_X1,   buf, dir, size, mod); break;
   case 2:  VG_(transfer) (&arm->guest_X2,   buf, dir, size, mod); break;
   case 3:  VG_(transfer) (&arm->guest_X3,   buf, dir, size, mod); break;
   case 4:  VG_(transfer) (&arm->guest_X4,   buf, dir, size, mod); break;
   case 5:  VG_(transfer) (&arm->guest_X5,   buf, dir, size, mod); break;
   case 6:  VG_(transfer) (&arm->guest_X6,   buf, dir, size, mod); break;
   case 7:  VG_(transfer) (&arm->guest_X7,   buf, dir, size, mod); break;
   case 8:  VG_(transfer) (&arm->guest_X8,   buf, dir, size, mod); break;
   case 9:  VG_(transfer) (&arm->guest_X9,   buf, dir, size, mod); break;
   case 10: VG_(transfer) (&arm->guest_X10,  buf, dir, size, mod); break;
   case 11: VG_(transfer) (&arm->guest_X11,  buf, dir, size, mod); break;
   case 12: VG_(transfer) (&arm->guest_X12,  buf, dir, size, mod); break;
   case 13: VG_(transfer) (&arm->guest_X13,  buf, dir, size, mod); break;
   case 14: VG_(transfer) (&arm->guest_X14,  buf, dir, size, mod); break;
   case 15: VG_(transfer) (&arm->guest_X15,  buf, dir, size, mod); break;
   case 16: VG_(transfer) (&arm->guest_X16,  buf, dir, size, mod); break;
   case 17: VG_(transfer) (&arm->guest_X17,  buf, dir, size, mod); break;
   case 18: VG_(transfer) (&arm->guest_X18,  buf, dir, size, mod); break;
   case 19: VG_(transfer) (&arm->guest_X19,  buf, dir, size, mod); break;
   case 20: VG_(transfer) (&arm->guest_X20,  buf, dir, size, mod); break;
   case 21: VG_(transfer) (&arm->guest_X21,  buf, dir, size, mod); break;
   case 22: VG_(transfer) (&arm->guest_X22,  buf, dir, size, mod); break;
   case 23: VG_(transfer) (&arm->guest_X23,  buf, dir, size, mod); break;
   case 24: VG_(transfer) (&arm->guest_X24,  buf, dir, size, mod); break;
   case 25: VG_(transfer) (&arm->guest_X25,  buf, dir, size, mod); break;
   case 26: VG_(transfer) (&arm->guest_X26,  buf, dir, size, mod); break;
   case 27: VG_(transfer) (&arm->guest_X27,  buf, dir, size, mod); break;
   case 28: VG_(transfer) (&arm->guest_X28,  buf, dir, size, mod); break;
   case 29: VG_(transfer) (&arm->guest_X29,  buf, dir, size, mod); break;
   case 30: VG_(transfer) (&arm->guest_X30,  buf, dir, size, mod); break;
   case 31: VG_(transfer) (&arm->guest_XSP,  buf, dir, size, mod); break;
   case 32: VG_(transfer) (&arm->guest_PC,   buf, dir, size, mod); break;
   case 33: *mod = False; break; // GDBTD cpsr what to do for arm64 ???

   case 34: VG_(transfer) (&arm->guest_Q0,  buf, dir, size, mod); break;
   case 35: VG_(transfer) (&arm->guest_Q1,  buf, dir, size, mod); break;
   case 36: VG_(transfer) (&arm->guest_Q2,  buf, dir, size, mod); break;
   case 37: VG_(transfer) (&arm->guest_Q3,  buf, dir, size, mod); break;
   case 38: VG_(transfer) (&arm->guest_Q4,  buf, dir, size, mod); break;
   case 39: VG_(transfer) (&arm->guest_Q5,  buf, dir, size, mod); break;
   case 40: VG_(transfer) (&arm->guest_Q6,  buf, dir, size, mod); break;
   case 41: VG_(transfer) (&arm->guest_Q7,  buf, dir, size, mod); break;
   case 42: VG_(transfer) (&arm->guest_Q8,  buf, dir, size, mod); break;
   case 43: VG_(transfer) (&arm->guest_Q9,  buf, dir, size, mod); break;
   case 44: VG_(transfer) (&arm->guest_Q10, buf, dir, size, mod); break;
   case 45: VG_(transfer) (&arm->guest_Q11, buf, dir, size, mod); break;
   case 46: VG_(transfer) (&arm->guest_Q12, buf, dir, size, mod); break;
   case 47: VG_(transfer) (&arm->guest_Q13, buf, dir, size, mod); break;
   case 48: VG_(transfer) (&arm->guest_Q14, buf, dir, size, mod); break;
   case 49: VG_(transfer) (&arm->guest_Q15, buf, dir, size, mod); break;
   case 50: VG_(transfer) (&arm->guest_Q16, buf, dir, size, mod); break;
   case 51: VG_(transfer) (&arm->guest_Q17, buf, dir, size, mod); break;
   case 52: VG_(transfer) (&arm->guest_Q18, buf, dir, size, mod); break;
   case 53: VG_(transfer) (&arm->guest_Q19, buf, dir, size, mod); break;
   case 54: VG_(transfer) (&arm->guest_Q20, buf, dir, size, mod); break;
   case 55: VG_(transfer) (&arm->guest_Q21, buf, dir, size, mod); break;
   case 56: VG_(transfer) (&arm->guest_Q22, buf, dir, size, mod); break;
   case 57: VG_(transfer) (&arm->guest_Q23, buf, dir, size, mod); break;
   case 58: VG_(transfer) (&arm->guest_Q24, buf, dir, size, mod); break;
   case 59: VG_(transfer) (&arm->guest_Q25, buf, dir, size, mod); break;
   case 60: VG_(transfer) (&arm->guest_Q26, buf, dir, size, mod); break;
   case 61: VG_(transfer) (&arm->guest_Q27, buf, dir, size, mod); break;
   case 62: VG_(transfer) (&arm->guest_Q28, buf, dir, size, mod); break;
   case 63: VG_(transfer) (&arm->guest_Q29, buf, dir, size, mod); break;
   case 64: VG_(transfer) (&arm->guest_Q30, buf, dir, size, mod); break;
   case 65: VG_(transfer) (&arm->guest_Q31, buf, dir, size, mod); break;
   case 66: {
      /* The VEX ARM64 FPSR representation is not the same as the
          architecturally defined representation.  Hence use conversion
          functions to convert to/from it.
          VEX FPSR only models QC (bit 27), and uses a 64 bits to store
          this FPSR QC bit. So, we need to transfer from/to the lowest
          significant part of the ULong that VEX provides/needs,
          as GDB expects or gives only 4 bytes. */
      if (dir == valgrind_to_gdbserver) {
         ULong fpsr64 = LibVEX_GuestARM64_get_fpsr(arm);
         UInt fpsr = (UInt)fpsr64;
         VG_(transfer) (&fpsr, buf, dir, size, mod);
      } else {
         UInt fpsr;
         ULong fpsr64;
         VG_(transfer) ((UInt*)&fpsr, buf, dir, size, mod);
         fpsr64 = fpsr;
         LibVEX_GuestARM64_set_fpsr(arm, fpsr64);
         /* resync the cache with the part of fpsr that VEX represents. */
         fpsr64 = LibVEX_GuestARM64_get_fpsr(arm);
         fpsr = (UInt)fpsr64;
         VG_(transfer) (&fpsr, buf, valgrind_to_gdbserver, size, mod);
      }
      break;
   }
   case 67: VG_(transfer) (&arm->guest_FPCR, buf, dir, size, mod); break;
   default: vg_assert(0);
   }
}

static
const char* target_xml (Bool shadow_mode)
{
   return NULL;
#if 0
   GDBTD
   if (shadow_mode) {
      return "arm-with-vfpv3-valgrind.xml";
   } else {
      return "arm-with-vfpv3.xml";
   }
#endif 
}

static CORE_ADDR** target_get_dtv (ThreadState *tst)
{
   VexGuestARM64State* arm64 = (VexGuestARM64State*)&tst->arch.vex;
   // arm64 dtv is pointed to by TPIDR_EL0.
   return (CORE_ADDR**)((CORE_ADDR)arm64->guest_TPIDR_EL0);
}

static struct valgrind_target_ops low_target = {
   num_regs,
   31, //SP
   regs,
   transfer_register,
   get_pc,
   set_pc,
   "arm64",
   target_xml,
   target_get_dtv
};

void arm64_init_architecture (struct valgrind_target_ops *target)
{
   *target = low_target;
   set_register_cache (regs, num_regs);
   gdbserver_expedite_regs = expedite_regs;
}
