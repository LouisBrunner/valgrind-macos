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

#include "libvex_guest_mips64.h"

static struct reg regs[] = {
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
   { "status", 2048, 64 },
   { "lo", 2112, 64 },
   { "hi", 2176, 64 },
   { "badvaddr", 2240, 64 },
   { "cause", 2304, 64 },
   { "pc", 2368, 64 },
   { "f0", 2432, 64 },
   { "f1", 2496, 64 },
   { "f2", 2560, 64 },
   { "f3", 2624, 64 },
   { "f4", 2688, 64 },
   { "f5", 2752, 64 },
   { "f6", 2816, 64 },
   { "f7", 2880, 64 },
   { "f8", 2944, 64 },
   { "f9", 3008, 64 },
   { "f10", 3072, 64 },
   { "f11", 3136, 64 },
   { "f12", 3200, 64 },
   { "f13", 3264, 64 },
   { "f14", 3328, 64 },
   { "f15", 3392, 64 },
   { "f16", 3456, 64 },
   { "f17", 3520, 64 },
   { "f18", 3584, 64 },
   { "f19", 3648, 64 },
   { "f20", 3712, 64 },
   { "f21", 3776, 64 },
   { "f22", 3840, 64 },
   { "f23", 3904, 64 },
   { "f24", 3968, 64 },
   { "f25", 4032, 64 },
   { "f26", 4096, 64 },
   { "f27", 4160, 64 },
   { "f28", 4224, 64 },
   { "f29", 4288, 64 },
   { "f30", 4352, 64 },
   { "f31", 4416, 64 },
   { "fcsr", 4480, 64 },
   { "fir", 4544, 64 },
   { "restart", 4608, 64 }
};


#define num_regs (sizeof (regs) / sizeof (regs[0]))

static const char *expedite_regs[] = { "r29", "pc", 0 };

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

   VexGuestMIPS64State* mips1 = (VexGuestMIPS64State*) get_arch (set, tst);

   switch (regno) { 
   case 0:  VG_(transfer) (&mips1->guest_r0,  buf, dir, size, mod); break;
   case 1:  VG_(transfer) (&mips1->guest_r1,  buf, dir, size, mod); break;
   case 2:  VG_(transfer) (&mips1->guest_r2,  buf, dir, size, mod); break;
   case 3:  VG_(transfer) (&mips1->guest_r3,  buf, dir, size, mod); break;
   case 4:  VG_(transfer) (&mips1->guest_r4,  buf, dir, size, mod); break;
   case 5:  VG_(transfer) (&mips1->guest_r5,  buf, dir, size, mod); break;
   case 6:  VG_(transfer) (&mips1->guest_r6,  buf, dir, size, mod); break;
   case 7:  VG_(transfer) (&mips1->guest_r7,  buf, dir, size, mod); break;
   case 8:  VG_(transfer) (&mips1->guest_r8,  buf, dir, size, mod); break;
   case 9:  VG_(transfer) (&mips1->guest_r9,  buf, dir, size, mod); break;
   case 10: VG_(transfer) (&mips1->guest_r10,  buf, dir, size, mod); break;
   case 11: VG_(transfer) (&mips1->guest_r11,  buf, dir, size, mod); break;
   case 12: VG_(transfer) (&mips1->guest_r12, buf, dir, size, mod); break;
   case 13: VG_(transfer) (&mips1->guest_r13, buf, dir, size, mod); break;
   case 14: VG_(transfer) (&mips1->guest_r14, buf, dir, size, mod); break;
   case 15: VG_(transfer) (&mips1->guest_r15, buf, dir, size, mod); break;
   case 16: VG_(transfer) (&mips1->guest_r16, buf, dir, size, mod); break;
   case 17: VG_(transfer) (&mips1->guest_r17, buf, dir, size, mod); break;
   case 18: VG_(transfer) (&mips1->guest_r18,  buf, dir, size, mod); break;
   case 19: VG_(transfer) (&mips1->guest_r19,  buf, dir, size, mod); break;
   case 20: VG_(transfer) (&mips1->guest_r20,  buf, dir, size, mod); break;
   case 21: VG_(transfer) (&mips1->guest_r21,  buf, dir, size, mod); break;
   case 22: VG_(transfer) (&mips1->guest_r22,  buf, dir, size, mod); break;
   case 23: VG_(transfer) (&mips1->guest_r23,  buf, dir, size, mod); break;
   case 24: VG_(transfer) (&mips1->guest_r24,  buf, dir, size, mod); break;
   case 25: VG_(transfer) (&mips1->guest_r25,  buf, dir, size, mod); break;
   case 26: VG_(transfer) (&mips1->guest_r26,  buf, dir, size, mod); break;
   case 27: VG_(transfer) (&mips1->guest_r27,  buf, dir, size, mod); break;
   case 28: VG_(transfer) (&mips1->guest_r28, buf, dir, size, mod); break;
   case 29: VG_(transfer) (&mips1->guest_r29, buf, dir, size, mod); break;
   case 30: VG_(transfer) (&mips1->guest_r30, buf, dir, size, mod); break;
   case 31: VG_(transfer) (&mips1->guest_r31, buf, dir, size, mod); break;
   case 32: *mod = False; break; // GDBTD???? VEX { "status", 1024, 64 }
   case 33: VG_(transfer) (&mips1->guest_LO, buf, dir, size, mod); break;
   case 34: VG_(transfer) (&mips1->guest_HI, buf, dir, size, mod); break;
   case 35: *mod = False; break; // GDBTD???? VEX { "badvaddr", 1120, 64 },
   case 36: *mod = False; break; // GDBTD???? VEX { "cause", 1152, 64 },
   case 37: VG_(transfer) (&mips1->guest_PC,  buf, dir, size, mod); break;
   case 38: VG_(transfer) (&mips1->guest_f0,  buf, dir, size, mod); break;
   case 39: VG_(transfer) (&mips1->guest_f1,  buf, dir, size, mod); break;
   case 40: VG_(transfer) (&mips1->guest_f2,  buf, dir, size, mod); break;
   case 41: VG_(transfer) (&mips1->guest_f3,  buf, dir, size, mod); break;
   case 42: VG_(transfer) (&mips1->guest_f4,  buf, dir, size, mod); break;
   case 43: VG_(transfer) (&mips1->guest_f5,  buf, dir, size, mod); break;
   case 44: VG_(transfer) (&mips1->guest_f6,  buf, dir, size, mod); break;
   case 45: VG_(transfer) (&mips1->guest_f7, buf, dir, size, mod); break;
   case 46: VG_(transfer) (&mips1->guest_f8, buf, dir, size, mod); break;
   case 47: VG_(transfer) (&mips1->guest_f9, buf, dir, size, mod); break;
   case 48: VG_(transfer) (&mips1->guest_f10, buf, dir, size, mod); break;
   case 49: VG_(transfer) (&mips1->guest_f11, buf, dir, size, mod); break;
   case 50: VG_(transfer) (&mips1->guest_f12, buf, dir, size, mod); break;
   case 51: VG_(transfer) (&mips1->guest_f13,  buf, dir, size, mod); break;
   case 52: VG_(transfer) (&mips1->guest_f14,  buf, dir, size, mod); break;
   case 53: VG_(transfer) (&mips1->guest_f15,  buf, dir, size, mod); break;
   case 54: VG_(transfer) (&mips1->guest_f16,  buf, dir, size, mod); break;
   case 55: VG_(transfer) (&mips1->guest_f17,  buf, dir, size, mod); break;
   case 56: VG_(transfer) (&mips1->guest_f18,  buf, dir, size, mod); break;
   case 57: VG_(transfer) (&mips1->guest_f19, buf, dir, size, mod); break;
   case 58: VG_(transfer) (&mips1->guest_f20, buf, dir, size, mod); break;
   case 59: VG_(transfer) (&mips1->guest_f21, buf, dir, size, mod); break;
   case 60: VG_(transfer) (&mips1->guest_f22, buf, dir, size, mod); break;
   case 61: VG_(transfer) (&mips1->guest_f23, buf, dir, size, mod); break;
   case 62: VG_(transfer) (&mips1->guest_f24,  buf, dir, size, mod); break;
   case 63: VG_(transfer) (&mips1->guest_f25,  buf, dir, size, mod); break;
   case 64: VG_(transfer) (&mips1->guest_f26,  buf, dir, size, mod); break;
   case 65: VG_(transfer) (&mips1->guest_f27,  buf, dir, size, mod); break;
   case 66: VG_(transfer) (&mips1->guest_f28,  buf, dir, size, mod); break;
   case 67: VG_(transfer) (&mips1->guest_f29,  buf, dir, size, mod); break;
   case 68: VG_(transfer) (&mips1->guest_f30, buf, dir, size, mod); break;
   case 69: VG_(transfer) (&mips1->guest_f31, buf, dir, size, mod); break;
   case 70: VG_(transfer) (&mips1->guest_FCSR, buf, dir, size, mod); break;
   case 71: VG_(transfer) (&mips1->guest_FIR, buf, dir, size, mod); break;
   case 72: *mod = False; break; // GDBTD???? VEX{ "restart", 2304, 64 },
   default: VG_(printf)("regno: %d\n", regno); vg_assert(0);
   }
}

static
const char* target_xml (Bool shadow_mode)
{
   if (shadow_mode) {
      return "mips64-linux-valgrind.xml";
   } else {
      return "mips64-linux.xml";
   }  
}

static struct valgrind_target_ops low_target = {
   num_regs,
   regs,
   29, //sp = r29, which is register offset 29 in regs
   transfer_register,
   get_pc,
   set_pc,
   "mips64",
   target_xml
};

void mips64_init_architecture (struct valgrind_target_ops *target)
{
   *target = low_target;
   set_register_cache (regs, num_regs);
   gdbserver_expedite_regs = expedite_regs;
}
