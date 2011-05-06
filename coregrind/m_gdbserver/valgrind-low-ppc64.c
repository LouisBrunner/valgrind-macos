/* Low level interface to valgrind, for the remote server for GDB integrated
   in valgrind.
   Copyright (C) 2011
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

#include "pub_core_aspacemgr.h"
#include "pub_tool_machine.h"
#include "pub_core_threadstate.h"
#include "pub_core_transtab.h"
#include "pub_core_gdbserver.h" 

#include "valgrind_low.h"

#include "libvex_guest_ppc64.h"

struct reg regs[] = {
  { "r0", 0, 64 },
  { "r1", 64, 64 },
  { "r2", 128, 64 },
  { "r3", 192, 64 },
  { "r4", 256, 64 },
  { "r5", 320, 64 },
  { "r6", 384, 64 },
  { "r7", 448, 64 },
  { "r8", 512, 64 },
  { "r9", 576, 64 },
  { "r10", 640, 64 },
  { "r11", 704, 64 },
  { "r12", 768, 64 },
  { "r13", 832, 64 },
  { "r14", 896, 64 },
  { "r15", 960, 64 },
  { "r16", 1024, 64 },
  { "r17", 1088, 64 },
  { "r18", 1152, 64 },
  { "r19", 1216, 64 },
  { "r20", 1280, 64 },
  { "r21", 1344, 64 },
  { "r22", 1408, 64 },
  { "r23", 1472, 64 },
  { "r24", 1536, 64 },
  { "r25", 1600, 64 },
  { "r26", 1664, 64 },
  { "r27", 1728, 64 },
  { "r28", 1792, 64 },
  { "r29", 1856, 64 },
  { "r30", 1920, 64 },
  { "r31", 1984, 64 },
  { "f0", 2048, 64 },
  { "f1", 2112, 64 },
  { "f2", 2176, 64 },
  { "f3", 2240, 64 },
  { "f4", 2304, 64 },
  { "f5", 2368, 64 },
  { "f6", 2432, 64 },
  { "f7", 2496, 64 },
  { "f8", 2560, 64 },
  { "f9", 2624, 64 },
  { "f10", 2688, 64 },
  { "f11", 2752, 64 },
  { "f12", 2816, 64 },
  { "f13", 2880, 64 },
  { "f14", 2944, 64 },
  { "f15", 3008, 64 },
  { "f16", 3072, 64 },
  { "f17", 3136, 64 },
  { "f18", 3200, 64 },
  { "f19", 3264, 64 },
  { "f20", 3328, 64 },
  { "f21", 3392, 64 },
  { "f22", 3456, 64 },
  { "f23", 3520, 64 },
  { "f24", 3584, 64 },
  { "f25", 3648, 64 },
  { "f26", 3712, 64 },
  { "f27", 3776, 64 },
  { "f28", 3840, 64 },
  { "f29", 3904, 64 },
  { "f30", 3968, 64 },
  { "f31", 4032, 64 },
  { "pc", 4096, 64 },
  { "msr", 4160, 64 },
  { "cr", 4224, 32 },
  { "lr", 4256, 64 },
  { "ctr", 4320, 64 },
  { "xer", 4384, 32 },
  { "fpscr", 4416, 32 },
  { "orig_r3", 4448, 64 },
  { "trap", 4512, 64 },
  { "vr0", 4576, 128 },
  { "vr1", 4704, 128 },
  { "vr2", 4832, 128 },
  { "vr3", 4960, 128 },
  { "vr4", 5088, 128 },
  { "vr5", 5216, 128 },
  { "vr6", 5344, 128 },
  { "vr7", 5472, 128 },
  { "vr8", 5600, 128 },
  { "vr9", 5728, 128 },
  { "vr10", 5856, 128 },
  { "vr11", 5984, 128 },
  { "vr12", 6112, 128 },
  { "vr13", 6240, 128 },
  { "vr14", 6368, 128 },
  { "vr15", 6496, 128 },
  { "vr16", 6624, 128 },
  { "vr17", 6752, 128 },
  { "vr18", 6880, 128 },
  { "vr19", 7008, 128 },
  { "vr20", 7136, 128 },
  { "vr21", 7264, 128 },
  { "vr22", 7392, 128 },
  { "vr23", 7520, 128 },
  { "vr24", 7648, 128 },
  { "vr25", 7776, 128 },
  { "vr26", 7904, 128 },
  { "vr27", 8032, 128 },
  { "vr28", 8160, 128 },
  { "vr29", 8288, 128 },
  { "vr30", 8416, 128 },
  { "vr31", 8544, 128 },
  { "vscr", 8672, 32 },
  { "vrsave", 8704, 32 },
};
static const char *expedite_regs[] = { "r1", "pc", 0 };
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
   Bool mod;
   supply_register_by_name ("pc", &newpc, &mod);
   if (mod)
      dlog(1, "set pc to %p\n", C2v (newpc));
   else
      dlog(1, "set pc not changed %p\n", C2v (newpc));
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

   VexGuestPPC64State* ppc64 = (VexGuestPPC64State*) get_arch (set, tst);

   switch (regno) { 
   // numbers here have to match the order of regs above
   // Attention: gdb order does not match valgrind order.
   case 0:  VG_(transfer) (&ppc64->guest_GPR0,  buf, dir, size, mod); break;
   case 1:  VG_(transfer) (&ppc64->guest_GPR1,  buf, dir, size, mod); break;
   case 2:  VG_(transfer) (&ppc64->guest_GPR2,  buf, dir, size, mod); break;
   case 3:  VG_(transfer) (&ppc64->guest_GPR3,  buf, dir, size, mod); break;
   case 4:  VG_(transfer) (&ppc64->guest_GPR4,  buf, dir, size, mod); break;
   case 5:  VG_(transfer) (&ppc64->guest_GPR5,  buf, dir, size, mod); break;
   case 6:  VG_(transfer) (&ppc64->guest_GPR6,  buf, dir, size, mod); break;
   case 7:  VG_(transfer) (&ppc64->guest_GPR7,  buf, dir, size, mod); break;
   case 8:  VG_(transfer) (&ppc64->guest_GPR8,  buf, dir, size, mod); break;
   case 9:  VG_(transfer) (&ppc64->guest_GPR9,  buf, dir, size, mod); break;
   case 10: VG_(transfer) (&ppc64->guest_GPR10, buf, dir, size, mod); break;
   case 11: VG_(transfer) (&ppc64->guest_GPR11, buf, dir, size, mod); break;
   case 12: VG_(transfer) (&ppc64->guest_GPR12, buf, dir, size, mod); break;
   case 13: VG_(transfer) (&ppc64->guest_GPR13, buf, dir, size, mod); break;
   case 14: VG_(transfer) (&ppc64->guest_GPR14, buf, dir, size, mod); break;
   case 15: VG_(transfer) (&ppc64->guest_GPR15, buf, dir, size, mod); break;
   case 16: VG_(transfer) (&ppc64->guest_GPR16, buf, dir, size, mod); break;
   case 17: VG_(transfer) (&ppc64->guest_GPR17, buf, dir, size, mod); break;
   case 18: VG_(transfer) (&ppc64->guest_GPR18, buf, dir, size, mod); break;
   case 19: VG_(transfer) (&ppc64->guest_GPR19, buf, dir, size, mod); break;
   case 20: VG_(transfer) (&ppc64->guest_GPR20, buf, dir, size, mod); break;
   case 21: VG_(transfer) (&ppc64->guest_GPR21, buf, dir, size, mod); break;
   case 22: VG_(transfer) (&ppc64->guest_GPR22, buf, dir, size, mod); break;
   case 23: VG_(transfer) (&ppc64->guest_GPR23, buf, dir, size, mod); break;
   case 24: VG_(transfer) (&ppc64->guest_GPR24, buf, dir, size, mod); break;
   case 25: VG_(transfer) (&ppc64->guest_GPR25, buf, dir, size, mod); break;
   case 26: VG_(transfer) (&ppc64->guest_GPR26, buf, dir, size, mod); break;
   case 27: VG_(transfer) (&ppc64->guest_GPR27, buf, dir, size, mod); break;
   case 28: VG_(transfer) (&ppc64->guest_GPR28, buf, dir, size, mod); break;
   case 29: VG_(transfer) (&ppc64->guest_GPR29, buf, dir, size, mod); break;
   case 30: VG_(transfer) (&ppc64->guest_GPR30, buf, dir, size, mod); break;
   case 31: VG_(transfer) (&ppc64->guest_GPR31, buf, dir, size, mod); break;
   case 32: VG_(transfer) (&ppc64->guest_VSR0,  buf, dir, size, mod); break;
   case 33: VG_(transfer) (&ppc64->guest_VSR1,  buf, dir, size, mod); break;
   case 34: VG_(transfer) (&ppc64->guest_VSR2,  buf, dir, size, mod); break;
   case 35: VG_(transfer) (&ppc64->guest_VSR3,  buf, dir, size, mod); break;
   case 36: VG_(transfer) (&ppc64->guest_VSR4,  buf, dir, size, mod); break;
   case 37: VG_(transfer) (&ppc64->guest_VSR5,  buf, dir, size, mod); break;
   case 38: VG_(transfer) (&ppc64->guest_VSR6,  buf, dir, size, mod); break;
   case 39: VG_(transfer) (&ppc64->guest_VSR7,  buf, dir, size, mod); break;
   case 40: VG_(transfer) (&ppc64->guest_VSR8,  buf, dir, size, mod); break;
   case 41: VG_(transfer) (&ppc64->guest_VSR9,  buf, dir, size, mod); break;
   case 42: VG_(transfer) (&ppc64->guest_VSR10, buf, dir, size, mod); break;
   case 43: VG_(transfer) (&ppc64->guest_VSR11, buf, dir, size, mod); break;
   case 44: VG_(transfer) (&ppc64->guest_VSR12, buf, dir, size, mod); break;
   case 45: VG_(transfer) (&ppc64->guest_VSR13, buf, dir, size, mod); break;
   case 46: VG_(transfer) (&ppc64->guest_VSR14, buf, dir, size, mod); break;
   case 47: VG_(transfer) (&ppc64->guest_VSR15, buf, dir, size, mod); break;
   case 48: VG_(transfer) (&ppc64->guest_VSR16, buf, dir, size, mod); break;
   case 49: VG_(transfer) (&ppc64->guest_VSR17, buf, dir, size, mod); break;
   case 50: VG_(transfer) (&ppc64->guest_VSR18, buf, dir, size, mod); break;
   case 51: VG_(transfer) (&ppc64->guest_VSR19, buf, dir, size, mod); break;
   case 52: VG_(transfer) (&ppc64->guest_VSR20, buf, dir, size, mod); break;
   case 53: VG_(transfer) (&ppc64->guest_VSR21, buf, dir, size, mod); break;
   case 54: VG_(transfer) (&ppc64->guest_VSR22, buf, dir, size, mod); break;
   case 55: VG_(transfer) (&ppc64->guest_VSR23, buf, dir, size, mod); break;
   case 56: VG_(transfer) (&ppc64->guest_VSR24, buf, dir, size, mod); break;
   case 57: VG_(transfer) (&ppc64->guest_VSR25, buf, dir, size, mod); break;
   case 58: VG_(transfer) (&ppc64->guest_VSR26, buf, dir, size, mod); break;
   case 59: VG_(transfer) (&ppc64->guest_VSR27, buf, dir, size, mod); break;
   case 60: VG_(transfer) (&ppc64->guest_VSR28, buf, dir, size, mod); break;
   case 61: VG_(transfer) (&ppc64->guest_VSR29, buf, dir, size, mod); break;
   case 62: VG_(transfer) (&ppc64->guest_VSR30, buf, dir, size, mod); break;
   case 63: VG_(transfer) (&ppc64->guest_VSR31, buf, dir, size, mod); break;
   case 64: VG_(transfer) (&ppc64->guest_CIA,   buf, dir, size, mod); break;
   case 65: *mod = False; break; // VEX does not model Machine State Register
   case 66: {
      UInt cr = LibVEX_GuestPPC64_get_CR (ppc64);
      if (dir == valgrind_to_gdbserver) {
         VG_(transfer) (&cr, buf, dir, size, mod); 
      } else {
         UInt newcr;
         VG_(transfer) (&newcr, buf, dir, size, mod);
         *mod = newcr != cr;
         LibVEX_GuestPPC64_put_CR (newcr, ppc64);
      }
      break;
   }
   case 67: VG_(transfer) (&ppc64->guest_LR,    buf, dir, size, mod); break;
   case 68: VG_(transfer) (&ppc64->guest_CTR,   buf, dir, size, mod); break;
   case 69: {
      UInt xer = LibVEX_GuestPPC64_get_XER (ppc64);
      if (dir == valgrind_to_gdbserver) {
         VG_(transfer) (&xer, buf, dir, size, mod); 
      } else {
         UInt newxer;
         VG_(transfer) (&newxer, buf, dir, size, mod);
         *mod = newxer != xer;
         LibVEX_GuestPPC64_put_XER (newxer, ppc64);
      }
      break;
   }
   case 70:  VG_(transfer) (&ppc64->guest_FPROUND, buf, dir, size, mod); break;
   case 71:  *mod = False; break; // GDBTD???? VEX { "orig_r3", 4448, 64 },
   case 72:  *mod = False; break; // GDBTD???? VEX { "trap", 4512, 64 },
   case 73:  VG_(transfer) (&ppc64->guest_VSR32, buf, dir, size, mod); break;
   case 74:  VG_(transfer) (&ppc64->guest_VSR33, buf, dir, size, mod); break;
   case 75:  VG_(transfer) (&ppc64->guest_VSR34, buf, dir, size, mod); break;
   case 76:  VG_(transfer) (&ppc64->guest_VSR35, buf, dir, size, mod); break;
   case 77:  VG_(transfer) (&ppc64->guest_VSR36, buf, dir, size, mod); break;
   case 78:  VG_(transfer) (&ppc64->guest_VSR37, buf, dir, size, mod); break;
   case 79:  VG_(transfer) (&ppc64->guest_VSR38, buf, dir, size, mod); break;
   case 80:  VG_(transfer) (&ppc64->guest_VSR39, buf, dir, size, mod); break;
   case 81:  VG_(transfer) (&ppc64->guest_VSR40, buf, dir, size, mod); break;
   case 82:  VG_(transfer) (&ppc64->guest_VSR41, buf, dir, size, mod); break;
   case 83:  VG_(transfer) (&ppc64->guest_VSR42, buf, dir, size, mod); break;
   case 84:  VG_(transfer) (&ppc64->guest_VSR43, buf, dir, size, mod); break;
   case 85:  VG_(transfer) (&ppc64->guest_VSR44, buf, dir, size, mod); break;
   case 86:  VG_(transfer) (&ppc64->guest_VSR45, buf, dir, size, mod); break;
   case 87:  VG_(transfer) (&ppc64->guest_VSR46, buf, dir, size, mod); break;
   case 88:  VG_(transfer) (&ppc64->guest_VSR47, buf, dir, size, mod); break;
   case 89:  VG_(transfer) (&ppc64->guest_VSR48, buf, dir, size, mod); break;
   case 90:  VG_(transfer) (&ppc64->guest_VSR49, buf, dir, size, mod); break;
   case 91:  VG_(transfer) (&ppc64->guest_VSR50, buf, dir, size, mod); break;
   case 92:  VG_(transfer) (&ppc64->guest_VSR51, buf, dir, size, mod); break;
   case 93:  VG_(transfer) (&ppc64->guest_VSR52, buf, dir, size, mod); break;
   case 94:  VG_(transfer) (&ppc64->guest_VSR53, buf, dir, size, mod); break;
   case 95:  VG_(transfer) (&ppc64->guest_VSR54, buf, dir, size, mod); break;
   case 96:  VG_(transfer) (&ppc64->guest_VSR55, buf, dir, size, mod); break;
   case 97:  VG_(transfer) (&ppc64->guest_VSR56, buf, dir, size, mod); break;
   case 98:  VG_(transfer) (&ppc64->guest_VSR57, buf, dir, size, mod); break;
   case 99:  VG_(transfer) (&ppc64->guest_VSR58, buf, dir, size, mod); break;
   case 100: VG_(transfer) (&ppc64->guest_VSR59, buf, dir, size, mod); break;
   case 101: VG_(transfer) (&ppc64->guest_VSR60, buf, dir, size, mod); break;
   case 102: VG_(transfer) (&ppc64->guest_VSR61, buf, dir, size, mod); break;
   case 103: VG_(transfer) (&ppc64->guest_VSR62, buf, dir, size, mod); break;
   case 104: VG_(transfer) (&ppc64->guest_VSR63, buf, dir, size, mod); break;
   case 105: VG_(transfer) (&ppc64->guest_VSCR, buf, dir, size, mod); break;
   case 106: VG_(transfer) (&ppc64->guest_VRSAVE, buf, dir, size, mod); break;
   default: vg_assert(0);
   }
}

static struct valgrind_target_ops low_target = {
   num_regs,
   regs,
   1, //r1
   transfer_register,
   get_pc,
   set_pc,
   "ppc64",
   "powerpc-altivec64l.xml",
   "powerpc-altivec64l-valgrind.xml"
};

void ppc64_init_architecture (struct valgrind_target_ops *target)
{
   *target = low_target;
   set_register_cache (regs, num_regs);
   gdbserver_expedite_regs = expedite_regs;
}
