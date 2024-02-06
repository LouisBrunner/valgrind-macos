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

#include "libvex_guest_ppc32.h"

/* this is only the basic set of registers.
   Need to look at what is the exact ppc32 model to support.
*/
static struct reg regs[] = {
  { "r0", 0, 32 },
  { "r1", 32, 32 },
  { "r2", 64, 32 },
  { "r3", 96, 32 },
  { "r4", 128, 32 },
  { "r5", 160, 32 },
  { "r6", 192, 32 },
  { "r7", 224, 32 },
  { "r8", 256, 32 },
  { "r9", 288, 32 },
  { "r10", 320, 32 },
  { "r11", 352, 32 },
  { "r12", 384, 32 },
  { "r13", 416, 32 },
  { "r14", 448, 32 },
  { "r15", 480, 32 },
  { "r16", 512, 32 },
  { "r17", 544, 32 },
  { "r18", 576, 32 },
  { "r19", 608, 32 },
  { "r20", 640, 32 },
  { "r21", 672, 32 },
  { "r22", 704, 32 },
  { "r23", 736, 32 },
  { "r24", 768, 32 },
  { "r25", 800, 32 },
  { "r26", 832, 32 },
  { "r27", 864, 32 },
  { "r28", 896, 32 },
  { "r29", 928, 32 },
  { "r30", 960, 32 },
  { "r31", 992, 32 },
  { "f0", 1024, 64 },
  { "f1", 1088, 64 },
  { "f2", 1152, 64 },
  { "f3", 1216, 64 },
  { "f4", 1280, 64 },
  { "f5", 1344, 64 },
  { "f6", 1408, 64 },
  { "f7", 1472, 64 },
  { "f8", 1536, 64 },
  { "f9", 1600, 64 },
  { "f10", 1664, 64 },
  { "f11", 1728, 64 },
  { "f12", 1792, 64 },
  { "f13", 1856, 64 },
  { "f14", 1920, 64 },
  { "f15", 1984, 64 },
  { "f16", 2048, 64 },
  { "f17", 2112, 64 },
  { "f18", 2176, 64 },
  { "f19", 2240, 64 },
  { "f20", 2304, 64 },
  { "f21", 2368, 64 },
  { "f22", 2432, 64 },
  { "f23", 2496, 64 },
  { "f24", 2560, 64 },
  { "f25", 2624, 64 },
  { "f26", 2688, 64 },
  { "f27", 2752, 64 },
  { "f28", 2816, 64 },
  { "f29", 2880, 64 },
  { "f30", 2944, 64 },
  { "f31", 3008, 64 },
  { "pc", 3072, 32 },
  { "msr", 3104, 32 },
  { "cr", 3136, 32 },
  { "lr", 3168, 32 },
  { "ctr", 3200, 32 },
  { "xer", 3232, 32 },
  { "fpscr", 3264, 32 },
  { "orig_r3", 3296, 32 },
  { "trap", 3328, 32 },
  { "vr0", 3360, 128 },
  { "vr1", 3488, 128 },
  { "vr2", 3616, 128 },
  { "vr3", 3744, 128 },
  { "vr4", 3872, 128 },
  { "vr5", 4000, 128 },
  { "vr6", 4128, 128 },
  { "vr7", 4256, 128 },
  { "vr8", 4384, 128 },
  { "vr9", 4512, 128 },
  { "vr10", 4640, 128 },
  { "vr11", 4768, 128 },
  { "vr12", 4896, 128 },
  { "vr13", 5024, 128 },
  { "vr14", 5152, 128 },
  { "vr15", 5280, 128 },
  { "vr16", 5408, 128 },
  { "vr17", 5536, 128 },
  { "vr18", 5664, 128 },
  { "vr19", 5792, 128 },
  { "vr20", 5920, 128 },
  { "vr21", 6048, 128 },
  { "vr22", 6176, 128 },
  { "vr23", 6304, 128 },
  { "vr24", 6432, 128 },
  { "vr25", 6560, 128 },
  { "vr26", 6688, 128 },
  { "vr27", 6816, 128 },
  { "vr28", 6944, 128 },
  { "vr29", 7072, 128 },
  { "vr30", 7200, 128 },
  { "vr31", 7328, 128 },
  { "vscr", 7456, 32 },
  { "vrsave", 7488, 32 }
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

   VexGuestPPC32State* ppc32 = (VexGuestPPC32State*) get_arch (set, tst);

   switch (regno) { 
   // numbers here have to match the order of regs above
   // Attention: gdb order does not match valgrind order.
   case 0:  VG_(transfer) (&ppc32->guest_GPR0,  buf, dir, size, mod); break;
   case 1:  VG_(transfer) (&ppc32->guest_GPR1,  buf, dir, size, mod); break;
   case 2:  VG_(transfer) (&ppc32->guest_GPR2,  buf, dir, size, mod); break;
   case 3:  VG_(transfer) (&ppc32->guest_GPR3,  buf, dir, size, mod); break;
   case 4:  VG_(transfer) (&ppc32->guest_GPR4,  buf, dir, size, mod); break;
   case 5:  VG_(transfer) (&ppc32->guest_GPR5,  buf, dir, size, mod); break;
   case 6:  VG_(transfer) (&ppc32->guest_GPR6,  buf, dir, size, mod); break;
   case 7:  VG_(transfer) (&ppc32->guest_GPR7,  buf, dir, size, mod); break;
   case 8:  VG_(transfer) (&ppc32->guest_GPR8,  buf, dir, size, mod); break;
   case 9:  VG_(transfer) (&ppc32->guest_GPR9,  buf, dir, size, mod); break;
   case 10: VG_(transfer) (&ppc32->guest_GPR10, buf, dir, size, mod); break;
   case 11: VG_(transfer) (&ppc32->guest_GPR11, buf, dir, size, mod); break;
   case 12: VG_(transfer) (&ppc32->guest_GPR12, buf, dir, size, mod); break;
   case 13: VG_(transfer) (&ppc32->guest_GPR13, buf, dir, size, mod); break;
   case 14: VG_(transfer) (&ppc32->guest_GPR14, buf, dir, size, mod); break;
   case 15: VG_(transfer) (&ppc32->guest_GPR15, buf, dir, size, mod); break;
   case 16: VG_(transfer) (&ppc32->guest_GPR16, buf, dir, size, mod); break;
   case 17: VG_(transfer) (&ppc32->guest_GPR17, buf, dir, size, mod); break;
   case 18: VG_(transfer) (&ppc32->guest_GPR18, buf, dir, size, mod); break;
   case 19: VG_(transfer) (&ppc32->guest_GPR19, buf, dir, size, mod); break;
   case 20: VG_(transfer) (&ppc32->guest_GPR20, buf, dir, size, mod); break;
   case 21: VG_(transfer) (&ppc32->guest_GPR21, buf, dir, size, mod); break;
   case 22: VG_(transfer) (&ppc32->guest_GPR22, buf, dir, size, mod); break;
   case 23: VG_(transfer) (&ppc32->guest_GPR23, buf, dir, size, mod); break;
   case 24: VG_(transfer) (&ppc32->guest_GPR24, buf, dir, size, mod); break;
   case 25: VG_(transfer) (&ppc32->guest_GPR25, buf, dir, size, mod); break;
   case 26: VG_(transfer) (&ppc32->guest_GPR26, buf, dir, size, mod); break;
   case 27: VG_(transfer) (&ppc32->guest_GPR27, buf, dir, size, mod); break;
   case 28: VG_(transfer) (&ppc32->guest_GPR28, buf, dir, size, mod); break;
   case 29: VG_(transfer) (&ppc32->guest_GPR29, buf, dir, size, mod); break;
   case 30: VG_(transfer) (&ppc32->guest_GPR30, buf, dir, size, mod); break;
   case 31: VG_(transfer) (&ppc32->guest_GPR31, buf, dir, size, mod); break;
   case 32: VG_(transfer) (&ppc32->guest_VSR0,  buf, dir, size, mod); break;
   case 33: VG_(transfer) (&ppc32->guest_VSR1,  buf, dir, size, mod); break;
   case 34: VG_(transfer) (&ppc32->guest_VSR2,  buf, dir, size, mod); break;
   case 35: VG_(transfer) (&ppc32->guest_VSR3,  buf, dir, size, mod); break;
   case 36: VG_(transfer) (&ppc32->guest_VSR4,  buf, dir, size, mod); break;
   case 37: VG_(transfer) (&ppc32->guest_VSR5,  buf, dir, size, mod); break;
   case 38: VG_(transfer) (&ppc32->guest_VSR6,  buf, dir, size, mod); break;
   case 39: VG_(transfer) (&ppc32->guest_VSR7,  buf, dir, size, mod); break;
   case 40: VG_(transfer) (&ppc32->guest_VSR8,  buf, dir, size, mod); break;
   case 41: VG_(transfer) (&ppc32->guest_VSR9,  buf, dir, size, mod); break;
   case 42: VG_(transfer) (&ppc32->guest_VSR10, buf, dir, size, mod); break;
   case 43: VG_(transfer) (&ppc32->guest_VSR11, buf, dir, size, mod); break;
   case 44: VG_(transfer) (&ppc32->guest_VSR12, buf, dir, size, mod); break;
   case 45: VG_(transfer) (&ppc32->guest_VSR13, buf, dir, size, mod); break;
   case 46: VG_(transfer) (&ppc32->guest_VSR14, buf, dir, size, mod); break;
   case 47: VG_(transfer) (&ppc32->guest_VSR15, buf, dir, size, mod); break;
   case 48: VG_(transfer) (&ppc32->guest_VSR16, buf, dir, size, mod); break;
   case 49: VG_(transfer) (&ppc32->guest_VSR17, buf, dir, size, mod); break;
   case 50: VG_(transfer) (&ppc32->guest_VSR18, buf, dir, size, mod); break;
   case 51: VG_(transfer) (&ppc32->guest_VSR19, buf, dir, size, mod); break;
   case 52: VG_(transfer) (&ppc32->guest_VSR20, buf, dir, size, mod); break;
   case 53: VG_(transfer) (&ppc32->guest_VSR21, buf, dir, size, mod); break;
   case 54: VG_(transfer) (&ppc32->guest_VSR22, buf, dir, size, mod); break;
   case 55: VG_(transfer) (&ppc32->guest_VSR23, buf, dir, size, mod); break;
   case 56: VG_(transfer) (&ppc32->guest_VSR24, buf, dir, size, mod); break;
   case 57: VG_(transfer) (&ppc32->guest_VSR25, buf, dir, size, mod); break;
   case 58: VG_(transfer) (&ppc32->guest_VSR26, buf, dir, size, mod); break;
   case 59: VG_(transfer) (&ppc32->guest_VSR27, buf, dir, size, mod); break;
   case 60: VG_(transfer) (&ppc32->guest_VSR28, buf, dir, size, mod); break;
   case 61: VG_(transfer) (&ppc32->guest_VSR29, buf, dir, size, mod); break;
   case 62: VG_(transfer) (&ppc32->guest_VSR30, buf, dir, size, mod); break;
   case 63: VG_(transfer) (&ppc32->guest_VSR31, buf, dir, size, mod); break;
   case 64: VG_(transfer) (&ppc32->guest_CIA,   buf, dir, size, mod); break;
   case 65: *mod = False; break; // VEX does not model Machine State Register
   case 66: {
      UInt cr = LibVEX_GuestPPC32_get_CR (ppc32);
      if (dir == valgrind_to_gdbserver) {
         VG_(transfer) (&cr, buf, dir, size, mod); 
      } else {
         UInt newcr;
         VG_(transfer) (&newcr, buf, dir, size, mod);
         *mod = newcr != cr;
         LibVEX_GuestPPC32_put_CR (newcr, ppc32);
      }
      break;
   }
   case 67: VG_(transfer) (&ppc32->guest_LR,    buf, dir, size, mod); break;
   case 68: VG_(transfer) (&ppc32->guest_CTR,   buf, dir, size, mod); break;
   case 69: {
      UInt xer = LibVEX_GuestPPC32_get_XER (ppc32);
      if (dir == valgrind_to_gdbserver) {
         VG_(transfer) (&xer, buf, dir, size, mod); 
      } else {
         UInt newxer;
         VG_(transfer) (&newxer, buf, dir, size, mod);
         *mod = newxer != xer;
         LibVEX_GuestPPC32_put_XER (newxer, ppc32);
      }
      break;
   }
   case 70:  VG_(transfer) (&ppc32->guest_FPROUND, buf, dir, size, mod); break;
   case 71:  *mod = False; break; // GDBTD???? VEX { "orig_r3", 3296, 32 },
   case 72:  *mod = False; break; // GDBTD???? VEX { "trap", 3328, 32 },
   case 73:  VG_(transfer) (&ppc32->guest_VSR32, buf, dir, size, mod); break;
   case 74:  VG_(transfer) (&ppc32->guest_VSR33, buf, dir, size, mod); break;
   case 75:  VG_(transfer) (&ppc32->guest_VSR34, buf, dir, size, mod); break;
   case 76:  VG_(transfer) (&ppc32->guest_VSR35, buf, dir, size, mod); break;
   case 77:  VG_(transfer) (&ppc32->guest_VSR36, buf, dir, size, mod); break;
   case 78:  VG_(transfer) (&ppc32->guest_VSR37, buf, dir, size, mod); break;
   case 79:  VG_(transfer) (&ppc32->guest_VSR38, buf, dir, size, mod); break;
   case 80:  VG_(transfer) (&ppc32->guest_VSR39, buf, dir, size, mod); break;
   case 81:  VG_(transfer) (&ppc32->guest_VSR40, buf, dir, size, mod); break;
   case 82:  VG_(transfer) (&ppc32->guest_VSR41, buf, dir, size, mod); break;
   case 83:  VG_(transfer) (&ppc32->guest_VSR42, buf, dir, size, mod); break;
   case 84:  VG_(transfer) (&ppc32->guest_VSR43, buf, dir, size, mod); break;
   case 85:  VG_(transfer) (&ppc32->guest_VSR44, buf, dir, size, mod); break;
   case 86:  VG_(transfer) (&ppc32->guest_VSR45, buf, dir, size, mod); break;
   case 87:  VG_(transfer) (&ppc32->guest_VSR46, buf, dir, size, mod); break;
   case 88:  VG_(transfer) (&ppc32->guest_VSR47, buf, dir, size, mod); break;
   case 89:  VG_(transfer) (&ppc32->guest_VSR48, buf, dir, size, mod); break;
   case 90:  VG_(transfer) (&ppc32->guest_VSR49, buf, dir, size, mod); break;
   case 91:  VG_(transfer) (&ppc32->guest_VSR50, buf, dir, size, mod); break;
   case 92:  VG_(transfer) (&ppc32->guest_VSR51, buf, dir, size, mod); break;
   case 93:  VG_(transfer) (&ppc32->guest_VSR52, buf, dir, size, mod); break;
   case 94:  VG_(transfer) (&ppc32->guest_VSR53, buf, dir, size, mod); break;
   case 95:  VG_(transfer) (&ppc32->guest_VSR54, buf, dir, size, mod); break;
   case 96:  VG_(transfer) (&ppc32->guest_VSR55, buf, dir, size, mod); break;
   case 97:  VG_(transfer) (&ppc32->guest_VSR56, buf, dir, size, mod); break;
   case 98:  VG_(transfer) (&ppc32->guest_VSR57, buf, dir, size, mod); break;
   case 99:  VG_(transfer) (&ppc32->guest_VSR58, buf, dir, size, mod); break;
   case 100: VG_(transfer) (&ppc32->guest_VSR59, buf, dir, size, mod); break;
   case 101: VG_(transfer) (&ppc32->guest_VSR60, buf, dir, size, mod); break;
   case 102: VG_(transfer) (&ppc32->guest_VSR61, buf, dir, size, mod); break;
   case 103: VG_(transfer) (&ppc32->guest_VSR62, buf, dir, size, mod); break;
   case 104: VG_(transfer) (&ppc32->guest_VSR63, buf, dir, size, mod); break;
   case 105: VG_(transfer) (&ppc32->guest_VSCR,  buf, dir, size, mod); break;
   case 106: VG_(transfer) (&ppc32->guest_VRSAVE, buf, dir, size, mod); break;
   default: vg_assert(0);
   }
}

static
const char* target_xml (Bool shadow_mode)
{
   if (shadow_mode) {
      return "powerpc-altivec32l-valgrind.xml";
   } else {
      return "powerpc-altivec32l.xml";
   }  
}

static CORE_ADDR** target_get_dtv (ThreadState *tst)
{
   VexGuestPPC32State* ppc32 = (VexGuestPPC32State*)&tst->arch.vex;
   // ppc32 dtv is located just before the tcb, which is 0x7000 before
   // the thread id (r2)
   return (CORE_ADDR**)((CORE_ADDR)ppc32->guest_GPR2
                        - 0x7000 - sizeof(CORE_ADDR));
}

static struct valgrind_target_ops low_target = {
   num_regs,
   1, //r1
   regs,
   transfer_register,
   get_pc,
   set_pc,
   "ppc32",
   target_xml,
   target_get_dtv
};

void ppc32_init_architecture (struct valgrind_target_ops *target)
{
   *target = low_target;
   set_register_cache (regs, num_regs);
   gdbserver_expedite_regs = expedite_regs;
}

