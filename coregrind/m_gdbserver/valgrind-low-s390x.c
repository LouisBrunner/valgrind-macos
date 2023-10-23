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

#include "pub_core_machine.h"
#include "pub_core_threadstate.h"
#include "pub_core_transtab.h"
#include "pub_core_gdbserver.h" 

#include "valgrind_low.h"

#include "libvex_guest_s390x.h"

static struct reg regs[] = {
  { "pswm", 0, 64 },
  { "pswa", 64, 64 },
  { "r0", 128, 64 },
  { "r1", 192, 64 },
  { "r2", 256, 64 },
  { "r3", 320, 64 },
  { "r4", 384, 64 },
  { "r5", 448, 64 },
  { "r6", 512, 64 },
  { "r7", 576, 64 },
  { "r8", 640, 64 },
  { "r9", 704, 64 },
  { "r10", 768, 64 },
  { "r11", 832, 64 },
  { "r12", 896, 64 },
  { "r13", 960, 64 },
  { "r14", 1024, 64 },
  { "r15", 1088, 64 },
  { "acr0", 1152, 32 },
  { "acr1", 1184, 32 },
  { "acr2", 1216, 32 },
  { "acr3", 1248, 32 },
  { "acr4", 1280, 32 },
  { "acr5", 1312, 32 },
  { "acr6", 1344, 32 },
  { "acr7", 1376, 32 },
  { "acr8", 1408, 32 },
  { "acr9", 1440, 32 },
  { "acr10", 1472, 32 },
  { "acr11", 1504, 32 },
  { "acr12", 1536, 32 },
  { "acr13", 1568, 32 },
  { "acr14", 1600, 32 },
  { "acr15", 1632, 32 },
  { "fpc", 1664, 32 },
  { "f0", 1696, 64 },
  { "f1", 1760, 64 },
  { "f2", 1824, 64 },
  { "f3", 1888, 64 },
  { "f4", 1952, 64 },
  { "f5", 2016, 64 },
  { "f6", 2080, 64 },
  { "f7", 2144, 64 },
  { "f8", 2208, 64 },
  { "f9", 2272, 64 },
  { "f10", 2336, 64 },
  { "f11", 2400, 64 },
  { "f12", 2464, 64 },
  { "f13", 2528, 64 },
  { "f14", 2592, 64 },
  { "f15", 2656, 64 },
  { "orig_r2", 2720, 64 },
  { "v0l", 2784, 64 },
  { "v1l", 2848, 64 },
  { "v2l", 2912, 64 },
  { "v3l", 2976, 64 },
  { "v4l", 3040, 64 },
  { "v5l", 3104, 64 },
  { "v6l", 3168, 64 },
  { "v7l", 3232, 64 },
  { "v8l", 3296, 64 },
  { "v9l", 3360, 64 },
  { "v10l", 3424, 64 },
  { "v11l", 3488, 64 },
  { "v12l", 3552, 64 },
  { "v13l", 3616, 64 },
  { "v14l", 3680, 64 },
  { "v15l", 3744, 64 },
  { "v16", 3808, 128 },
  { "v17", 3936, 128 },
  { "v18", 4064, 128 },
  { "v19", 4192, 128 },
  { "v20", 4320, 128 },
  { "v21", 4448, 128 },
  { "v22", 4576, 128 },
  { "v23", 4704, 128 },
  { "v24", 4832, 128 },
  { "v25", 4960, 128 },
  { "v26", 5088, 128 },
  { "v27", 5216, 128 },
  { "v28", 5344, 128 },
  { "v29", 5472, 128 },
  { "v30", 5600, 128 },
  { "v31", 5728, 128 },
};
static const char *expedite_regs[] = { "r14", "r15", "pswa", 0 };
#define num_regs_all (sizeof (regs) / sizeof (regs[0]))
static int num_regs;

static
CORE_ADDR get_pc (void)
{
   unsigned long pc;

   collect_register_by_name ("pswa", &pc);
   
   dlog(1, "stop pc is %p\n", (void *) pc);
   return pc;
}

static
void set_pc (CORE_ADDR newpc)
{
   supply_register_by_name ("pswa", &newpc);
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

   VexGuestS390XState* s390x = (VexGuestS390XState*) get_arch (set, tst);

   switch (regno) { 
   // numbers here have to match the order of regs above
   // Attention: gdb order does not match valgrind order.
   case 0:  *mod = False; break; //GDBTD??? { "pswm", 0, 64 },  
   case 1:  VG_(transfer) (&s390x->guest_IA,  buf, dir, size, mod); break;
   case 2:  VG_(transfer) (&s390x->guest_r0,  buf, dir, size, mod); break;
   case 3:  VG_(transfer) (&s390x->guest_r1,  buf, dir, size, mod); break;
   case 4:  VG_(transfer) (&s390x->guest_r2,  buf, dir, size, mod); break;
   case 5:  VG_(transfer) (&s390x->guest_r3,  buf, dir, size, mod); break;
   case 6:  VG_(transfer) (&s390x->guest_r4,  buf, dir, size, mod); break;
   case 7:  VG_(transfer) (&s390x->guest_r5,  buf, dir, size, mod); break;
   case 8:  VG_(transfer) (&s390x->guest_r6,  buf, dir, size, mod); break;
   case 9:  VG_(transfer) (&s390x->guest_r7,  buf, dir, size, mod); break;
   case 10: VG_(transfer) (&s390x->guest_r8,  buf, dir, size, mod); break;
   case 11: VG_(transfer) (&s390x->guest_r9,  buf, dir, size, mod); break;
   case 12: VG_(transfer) (&s390x->guest_r10, buf, dir, size, mod); break;
   case 13: VG_(transfer) (&s390x->guest_r11, buf, dir, size, mod); break;
   case 14: VG_(transfer) (&s390x->guest_r12, buf, dir, size, mod); break;
   case 15: VG_(transfer) (&s390x->guest_r13, buf, dir, size, mod); break;
   case 16: VG_(transfer) (&s390x->guest_r14, buf, dir, size, mod); break;
   case 17: VG_(transfer) (&s390x->guest_r15, buf, dir, size, mod); break;
   case 18: VG_(transfer) (&s390x->guest_a0,  buf, dir, size, mod); break;
   case 19: VG_(transfer) (&s390x->guest_a1,  buf, dir, size, mod); break;
   case 20: VG_(transfer) (&s390x->guest_a2,  buf, dir, size, mod); break;
   case 21: VG_(transfer) (&s390x->guest_a3,  buf, dir, size, mod); break;
   case 22: VG_(transfer) (&s390x->guest_a4,  buf, dir, size, mod); break;
   case 23: VG_(transfer) (&s390x->guest_a5,  buf, dir, size, mod); break;
   case 24: VG_(transfer) (&s390x->guest_a6,  buf, dir, size, mod); break;
   case 25: VG_(transfer) (&s390x->guest_a7,  buf, dir, size, mod); break;
   case 26: VG_(transfer) (&s390x->guest_a8,  buf, dir, size, mod); break;
   case 27: VG_(transfer) (&s390x->guest_a9,  buf, dir, size, mod); break;
   case 28: VG_(transfer) (&s390x->guest_a10, buf, dir, size, mod); break;
   case 29: VG_(transfer) (&s390x->guest_a11, buf, dir, size, mod); break;
   case 30: VG_(transfer) (&s390x->guest_a12, buf, dir, size, mod); break;
   case 31: VG_(transfer) (&s390x->guest_a13, buf, dir, size, mod); break;
   case 32: VG_(transfer) (&s390x->guest_a14, buf, dir, size, mod); break;
   case 33: VG_(transfer) (&s390x->guest_a15, buf, dir, size, mod); break;
   case 34: VG_(transfer) (&s390x->guest_fpc, buf, dir, size, mod); break;
   case 35: VG_(transfer) (&s390x->guest_v0.w64[0],  buf, dir, size, mod); break;
   case 36: VG_(transfer) (&s390x->guest_v1.w64[0],  buf, dir, size, mod); break;
   case 37: VG_(transfer) (&s390x->guest_v2.w64[0],  buf, dir, size, mod); break;
   case 38: VG_(transfer) (&s390x->guest_v3.w64[0],  buf, dir, size, mod); break;
   case 39: VG_(transfer) (&s390x->guest_v4.w64[0],  buf, dir, size, mod); break;
   case 40: VG_(transfer) (&s390x->guest_v5.w64[0],  buf, dir, size, mod); break;
   case 41: VG_(transfer) (&s390x->guest_v6.w64[0],  buf, dir, size, mod); break;
   case 42: VG_(transfer) (&s390x->guest_v7.w64[0],  buf, dir, size, mod); break;
   case 43: VG_(transfer) (&s390x->guest_v8.w64[0],  buf, dir, size, mod); break;
   case 44: VG_(transfer) (&s390x->guest_v9.w64[0],  buf, dir, size, mod); break;
   case 45: VG_(transfer) (&s390x->guest_v10.w64[0], buf, dir, size, mod); break;
   case 46: VG_(transfer) (&s390x->guest_v11.w64[0], buf, dir, size, mod); break;
   case 47: VG_(transfer) (&s390x->guest_v12.w64[0], buf, dir, size, mod); break;
   case 48: VG_(transfer) (&s390x->guest_v13.w64[0], buf, dir, size, mod); break;
   case 49: VG_(transfer) (&s390x->guest_v14.w64[0], buf, dir, size, mod); break;
   case 50: VG_(transfer) (&s390x->guest_v15.w64[0], buf, dir, size, mod); break;
   case 51:  *mod = False; break; //GDBTD??? { "orig_r2", 0, 64 },
   case 52: VG_(transfer) (&s390x->guest_v0.w64[1],  buf, dir, size, mod); break;
   case 53: VG_(transfer) (&s390x->guest_v1.w64[1],  buf, dir, size, mod); break;
   case 54: VG_(transfer) (&s390x->guest_v2.w64[1],  buf, dir, size, mod); break;
   case 55: VG_(transfer) (&s390x->guest_v3.w64[1],  buf, dir, size, mod); break;
   case 56: VG_(transfer) (&s390x->guest_v4.w64[1],  buf, dir, size, mod); break;
   case 57: VG_(transfer) (&s390x->guest_v5.w64[1],  buf, dir, size, mod); break;
   case 58: VG_(transfer) (&s390x->guest_v6.w64[1],  buf, dir, size, mod); break;
   case 59: VG_(transfer) (&s390x->guest_v7.w64[1],  buf, dir, size, mod); break;
   case 60: VG_(transfer) (&s390x->guest_v8.w64[1],  buf, dir, size, mod); break;
   case 61: VG_(transfer) (&s390x->guest_v9.w64[1],  buf, dir, size, mod); break;
   case 62: VG_(transfer) (&s390x->guest_v10.w64[1], buf, dir, size, mod); break;
   case 63: VG_(transfer) (&s390x->guest_v11.w64[1], buf, dir, size, mod); break;
   case 64: VG_(transfer) (&s390x->guest_v12.w64[1], buf, dir, size, mod); break;
   case 65: VG_(transfer) (&s390x->guest_v13.w64[1], buf, dir, size, mod); break;
   case 66: VG_(transfer) (&s390x->guest_v14.w64[1], buf, dir, size, mod); break;
   case 67: VG_(transfer) (&s390x->guest_v15.w64[1], buf, dir, size, mod); break;
   case 68: VG_(transfer) (&s390x->guest_v16, buf, dir, size, mod); break;
   case 69: VG_(transfer) (&s390x->guest_v17, buf, dir, size, mod); break;
   case 70: VG_(transfer) (&s390x->guest_v18, buf, dir, size, mod); break;
   case 71: VG_(transfer) (&s390x->guest_v19, buf, dir, size, mod); break;
   case 72: VG_(transfer) (&s390x->guest_v20, buf, dir, size, mod); break;
   case 73: VG_(transfer) (&s390x->guest_v21, buf, dir, size, mod); break;
   case 74: VG_(transfer) (&s390x->guest_v22, buf, dir, size, mod); break;
   case 75: VG_(transfer) (&s390x->guest_v23, buf, dir, size, mod); break;
   case 76: VG_(transfer) (&s390x->guest_v24, buf, dir, size, mod); break;
   case 77: VG_(transfer) (&s390x->guest_v25, buf, dir, size, mod); break;
   case 78: VG_(transfer) (&s390x->guest_v26, buf, dir, size, mod); break;
   case 79: VG_(transfer) (&s390x->guest_v27, buf, dir, size, mod); break;
   case 80: VG_(transfer) (&s390x->guest_v28, buf, dir, size, mod); break;
   case 81: VG_(transfer) (&s390x->guest_v29, buf, dir, size, mod); break;
   case 82: VG_(transfer) (&s390x->guest_v30, buf, dir, size, mod); break;
   case 83: VG_(transfer) (&s390x->guest_v31, buf, dir, size, mod); break;
   default: vg_assert(0);
   }
}

static
Bool have_vx (void)
{
   VexArch va;
   VexArchInfo vai;
   VG_(machine_get_VexArchInfo) (&va, &vai);
   return (vai.hwcaps & VEX_HWCAPS_S390X_VX) != 0;
}

static
const char* target_xml (Bool shadow_mode)
{
   if (shadow_mode) {
      if (have_vx())
         return "s390x-vx-linux-valgrind.xml";
      else
         return "s390x-generic-valgrind.xml";
   } else {
      if (have_vx())
         return "s390x-vx-linux.xml";
      else
         return "s390x-generic.xml";
   }
}

static CORE_ADDR** target_get_dtv (ThreadState *tst)
{
   VexGuestS390XState* s390x = (VexGuestS390XState*)&tst->arch.vex;
   // Thread pointer is in a0 (high 32 bits) and a1. Dtv is the second word.
   return (CORE_ADDR**)((Addr)((Addr64)s390x->guest_a0 << 32
                              | (Addr64)s390x->guest_a1)
                        + sizeof(CORE_ADDR));
}

static struct valgrind_target_ops low_target = {
   -1, // Override at init time.
   17, //sp = r15, which is register offset 17 in regs
   regs,
   transfer_register,
   get_pc,
   set_pc,
   "s390x",
   target_xml,
   target_get_dtv
};

void s390x_init_architecture (struct valgrind_target_ops *target)
{
   *target = low_target;
   if (have_vx())
      num_regs = num_regs_all;
   else
      num_regs = num_regs_all - 32; // Remove all VX registers.
   target->num_regs = num_regs;
   set_register_cache (regs, num_regs);
   gdbserver_expedite_regs = expedite_regs;
}
