/* Low level interface to valgrind, for the remote server for GDB integrated
   in valgrind.
   Copyright (C) 2022
   Free Software Foundation, Inc.

   This file is part of VALGRIND.
   It has been inspired from a file from gdbserver in gdb 6.6.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
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

#include "libvex_guest_riscv64.h"

/* from GDB gdb/features/riscv/64bit-{cpu,fpu}.c */
static struct reg regs[] = {
  { "zero", 0, 64 },
  { "ra", 64, 64 },
  { "sp", 128, 64 },
  { "gp", 192, 64 },
  { "tp", 256, 64 },
  { "t0", 320, 64 },
  { "t1", 384, 64 },
  { "t2", 448, 64 },
  { "fp", 512, 64 },
  { "s1", 576, 64 },
  { "a0", 640, 64 },
  { "a1", 704, 64 },
  { "a2", 768, 64 },
  { "a3", 832, 64 },
  { "a4", 896, 64 },
  { "a5", 960, 64 },
  { "a6", 1024, 64 },
  { "a7", 1088, 64 },
  { "s2", 1152, 64 },
  { "s3", 1216, 64 },
  { "s4", 1280, 64 },
  { "s5", 1344, 64 },
  { "s6", 1408, 64 },
  { "s7", 1472, 64 },
  { "s8", 1536, 64 },
  { "s9", 1600, 64 },
  { "s10", 1664, 64 },
  { "s11", 1728, 64 },
  { "t3", 1792, 64 },
  { "t4", 1856, 64 },
  { "t5", 1920, 64 },
  { "t6", 1984, 64 },
  { "pc", 2048, 64 },

  { "ft0", 2112, 64 },
  { "ft1", 2176, 64 },
  { "ft2", 2240, 64 },
  { "ft3", 2304, 64 },
  { "ft4", 2368, 64 },
  { "ft5", 2432, 64 },
  { "ft6", 2496, 64 },
  { "ft7", 2560, 64 },
  { "fs0", 2624, 64 },
  { "fs1", 2688, 64 },
  { "fa0", 2752, 64 },
  { "fa1", 2816, 64 },
  { "fa2", 2880, 64 },
  { "fa3", 2944, 64 },
  { "fa4", 3008, 64 },
  { "fa5", 3072, 64 },
  { "fa6", 3136, 64 },
  { "fa7", 3200, 64 },
  { "fs2", 3264, 64 },
  { "fs3", 3328, 64 },
  { "fs4", 3392, 64 },
  { "fs5", 3456, 64 },
  { "fs6", 3520, 64 },
  { "fs7", 3584, 64 },
  { "fs8", 3648, 64 },
  { "fs9", 3712, 64 },
  { "fs10", 3776, 64 },
  { "fs11", 3840, 64 },
  { "ft8", 3904, 64 },
  { "ft9", 3968, 64 },
  { "ft10", 4032, 64 },
  { "ft11", 4096, 64 },
  { "", 4160, 0 },  /* regnums have a hole here */
  { "fflags", 4160, 32 },
  { "frm", 4192, 32 },
  { "fcsr", 4224, 32 },
};

/* from GDB gdbserver/linux-riscv-low.cc */
static const char *expedite_regs[] = { "sp", "pc", 0 };

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
   UInt v, *p;

   VexGuestRISCV64State* riscv = (VexGuestRISCV64State*) get_arch (set, tst);

   switch (regno) {
   // numbers here have to match the order of regs above
   // Attention: gdb order does not match valgrind order.
   case 0:  VG_(transfer) (&riscv->guest_x0,   buf, dir, size, mod); break;
   case 1:  VG_(transfer) (&riscv->guest_x1,   buf, dir, size, mod); break;
   case 2:  VG_(transfer) (&riscv->guest_x2,   buf, dir, size, mod); break;
   case 3:  VG_(transfer) (&riscv->guest_x3,   buf, dir, size, mod); break;
   case 4:  VG_(transfer) (&riscv->guest_x4,   buf, dir, size, mod); break;
   case 5:  VG_(transfer) (&riscv->guest_x5,   buf, dir, size, mod); break;
   case 6:  VG_(transfer) (&riscv->guest_x6,   buf, dir, size, mod); break;
   case 7:  VG_(transfer) (&riscv->guest_x7,   buf, dir, size, mod); break;
   case 8:  VG_(transfer) (&riscv->guest_x8,   buf, dir, size, mod); break;
   case 9:  VG_(transfer) (&riscv->guest_x9,   buf, dir, size, mod); break;
   case 10: VG_(transfer) (&riscv->guest_x10,  buf, dir, size, mod); break;
   case 11: VG_(transfer) (&riscv->guest_x11,  buf, dir, size, mod); break;
   case 12: VG_(transfer) (&riscv->guest_x12,  buf, dir, size, mod); break;
   case 13: VG_(transfer) (&riscv->guest_x13,  buf, dir, size, mod); break;
   case 14: VG_(transfer) (&riscv->guest_x14,  buf, dir, size, mod); break;
   case 15: VG_(transfer) (&riscv->guest_x15,  buf, dir, size, mod); break;
   case 16: VG_(transfer) (&riscv->guest_x16,  buf, dir, size, mod); break;
   case 17: VG_(transfer) (&riscv->guest_x17,  buf, dir, size, mod); break;
   case 18: VG_(transfer) (&riscv->guest_x18,  buf, dir, size, mod); break;
   case 19: VG_(transfer) (&riscv->guest_x19,  buf, dir, size, mod); break;
   case 20: VG_(transfer) (&riscv->guest_x20,  buf, dir, size, mod); break;
   case 21: VG_(transfer) (&riscv->guest_x21,  buf, dir, size, mod); break;
   case 22: VG_(transfer) (&riscv->guest_x22,  buf, dir, size, mod); break;
   case 23: VG_(transfer) (&riscv->guest_x23,  buf, dir, size, mod); break;
   case 24: VG_(transfer) (&riscv->guest_x24,  buf, dir, size, mod); break;
   case 25: VG_(transfer) (&riscv->guest_x25,  buf, dir, size, mod); break;
   case 26: VG_(transfer) (&riscv->guest_x26,  buf, dir, size, mod); break;
   case 27: VG_(transfer) (&riscv->guest_x27,  buf, dir, size, mod); break;
   case 28: VG_(transfer) (&riscv->guest_x28,  buf, dir, size, mod); break;
   case 29: VG_(transfer) (&riscv->guest_x29,  buf, dir, size, mod); break;
   case 30: VG_(transfer) (&riscv->guest_x30,  buf, dir, size, mod); break;
   case 31: VG_(transfer) (&riscv->guest_x31,  buf, dir, size, mod); break;
   case 32: VG_(transfer) (&riscv->guest_pc,   buf, dir, size, mod); break;

   case 33: VG_(transfer) (&riscv->guest_f0,   buf, dir, size, mod); break;
   case 34: VG_(transfer) (&riscv->guest_f1,   buf, dir, size, mod); break;
   case 35: VG_(transfer) (&riscv->guest_f2,   buf, dir, size, mod); break;
   case 36: VG_(transfer) (&riscv->guest_f3,   buf, dir, size, mod); break;
   case 37: VG_(transfer) (&riscv->guest_f4,   buf, dir, size, mod); break;
   case 38: VG_(transfer) (&riscv->guest_f5,   buf, dir, size, mod); break;
   case 39: VG_(transfer) (&riscv->guest_f6,   buf, dir, size, mod); break;
   case 40: VG_(transfer) (&riscv->guest_f7,   buf, dir, size, mod); break;
   case 41: VG_(transfer) (&riscv->guest_f8,   buf, dir, size, mod); break;
   case 42: VG_(transfer) (&riscv->guest_f9,   buf, dir, size, mod); break;
   case 43: VG_(transfer) (&riscv->guest_f10,  buf, dir, size, mod); break;
   case 44: VG_(transfer) (&riscv->guest_f11,  buf, dir, size, mod); break;
   case 45: VG_(transfer) (&riscv->guest_f12,  buf, dir, size, mod); break;
   case 46: VG_(transfer) (&riscv->guest_f13,  buf, dir, size, mod); break;
   case 47: VG_(transfer) (&riscv->guest_f14,  buf, dir, size, mod); break;
   case 48: VG_(transfer) (&riscv->guest_f15,  buf, dir, size, mod); break;
   case 49: VG_(transfer) (&riscv->guest_f16,  buf, dir, size, mod); break;
   case 50: VG_(transfer) (&riscv->guest_f17,  buf, dir, size, mod); break;
   case 51: VG_(transfer) (&riscv->guest_f18,  buf, dir, size, mod); break;
   case 52: VG_(transfer) (&riscv->guest_f19,  buf, dir, size, mod); break;
   case 53: VG_(transfer) (&riscv->guest_f20,  buf, dir, size, mod); break;
   case 54: VG_(transfer) (&riscv->guest_f21,  buf, dir, size, mod); break;
   case 55: VG_(transfer) (&riscv->guest_f22,  buf, dir, size, mod); break;
   case 56: VG_(transfer) (&riscv->guest_f23,  buf, dir, size, mod); break;
   case 57: VG_(transfer) (&riscv->guest_f24,  buf, dir, size, mod); break;
   case 58: VG_(transfer) (&riscv->guest_f25,  buf, dir, size, mod); break;
   case 59: VG_(transfer) (&riscv->guest_f26,  buf, dir, size, mod); break;
   case 60: VG_(transfer) (&riscv->guest_f27,  buf, dir, size, mod); break;
   case 61: VG_(transfer) (&riscv->guest_f28,  buf, dir, size, mod); break;
   case 62: VG_(transfer) (&riscv->guest_f29,  buf, dir, size, mod); break;
   case 63: VG_(transfer) (&riscv->guest_f30,  buf, dir, size, mod); break;
   case 64: VG_(transfer) (&riscv->guest_f31,  buf, dir, size, mod); break;

   case 65: break;

   case 66: /* fflags = fcsr & 0x1F */
        p = &riscv->guest_fcsr;
        if (dir == valgrind_to_gdbserver)
            v = *p & 0x1F;
        VG_(transfer) (&v, buf, dir, size, mod);
        if (dir == gdbserver_to_valgrind)
            *p = (*p & ~0x1F) | v;
        break;

   case 67: /* frm = (fcsr & 0xE0) >> 5 */
        p = &riscv->guest_fcsr;
        if (dir == valgrind_to_gdbserver)
            v = (*p & 0xE0) >> 5;
        VG_(transfer) (&v, buf, dir, size, mod);
        if (dir == gdbserver_to_valgrind)
            *p = (*p & ~0xE0) | (v << 5);
        break;

   case 68: VG_(transfer) (&riscv->guest_fcsr, buf, dir, size, mod); break;
   default: vg_assert(0);
   }
}

static
const char* target_xml (Bool shadow_mode)
{
   if (shadow_mode) {
      return "riscv64-linux-valgrind.xml";
   } else {
      return "riscv64-linux.xml";
   }
}

static CORE_ADDR** target_get_dtv (ThreadState *tst)
{
   VexGuestRISCV64State* riscv = (VexGuestRISCV64State*)&tst->arch.vex;

   /* RISC-V uses Variant I as described by the ELF TLS specification,
      with tp containing the address one past the end of the TCB.

      from GLIBC sysdeps/riscv/nptl/tls.h, tp is just after tcbhead_t
        typedef struct {
            dtv_t *dtv;
            void *private;
        } tcbhead_t;
   */
   return (CORE_ADDR**)(void *)((HWord)riscv->guest_x4 - 2 * sizeof(void *));
}

static struct valgrind_target_ops low_target = {
   num_regs,
   2, //SP
   regs,
   transfer_register,
   get_pc,
   set_pc,
   "riscv64",
   target_xml,
   target_get_dtv
};

void riscv64_init_architecture (struct valgrind_target_ops *target)
{
   *target = low_target;
   set_register_cache (regs, num_regs);
   gdbserver_expedite_regs = expedite_regs;
}
