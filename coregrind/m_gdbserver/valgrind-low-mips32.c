/* Low level interface to valgrind, for the remote server for GDB integrated
   in valgrind.
   Copyright (C) 2012
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
#include "pub_core_debuginfo.h"
#include "pub_core_threadstate.h"
#include "pub_core_transtab.h"
#include "pub_core_gdbserver.h" 

#include "valgrind_low.h"

#include "libvex_guest_mips32.h"

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
  { "status", 1024, 32 },
  { "lo", 1056, 32 },
  { "hi", 1088, 32 },
  { "badvaddr", 1120, 32 },
  { "cause", 1152, 32 },
  { "pc", 1184, 32 },
  { "f0", 1216, 32 },
  { "f1", 1248, 32 },
  { "f2", 1280, 32 },
  { "f3", 1312, 32 },
  { "f4", 1344, 32 },
  { "f5", 1376, 32 },
  { "f6", 1408, 32 },
  { "f7", 1440, 32 },
  { "f8", 1472, 32 },
  { "f9", 1504, 32 },
  { "f10", 1536, 32 },
  { "f11", 1568, 32 },
  { "f12", 1600, 32 },
  { "f13", 1632, 32 },
  { "f14", 1664, 32 },
  { "f15", 1696, 32 },
  { "f16", 1728, 32 },
  { "f17", 1760, 32 },
  { "f18", 1792, 32 },
  { "f19", 1824, 32 },
  { "f20", 1856, 32 },
  { "f21", 1888, 32 },
  { "f22", 1920, 32 },
  { "f23", 1952, 32 },
  { "f24", 1984, 32 },
  { "f25", 2016, 32 },
  { "f26", 2048, 32 },
  { "f27", 2080, 32 },
  { "f28", 2112, 32 },
  { "f29", 2144, 32 },
  { "f30", 2176, 32 },
  { "f31", 2208, 32 },
  { "fcsr", 2240, 32 },
  { "fir", 2272, 32 },
  { "restart", 2304, 32 },
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
   supply_register_by_name ("pc", &newpc);
}

/* These are the fields of 32 bit mips instructions. */
#define itype_op(x) (x >> 26)
#define itype_rs(x) ((x >> 21) & 0x1f)
#define itype_rt(x) ((x >> 16) & 0x1f)
#define rtype_funct(x) (x & 0x3f)

/* Do a endian load of a 32-bit word, regardless of the
   endianness of the underlying host. */
static inline UInt getUInt(UChar * p)
{
   UInt w = 0;
#if defined (_MIPSEL)
   w = (w << 8) | p[3];
   w = (w << 8) | p[2];
   w = (w << 8) | p[1];
   w = (w << 8) | p[0];
#elif defined (_MIPSEB)
   w = (w << 8) | p[0];
   w = (w << 8) | p[1];
   w = (w << 8) | p[2];
   w = (w << 8) | p[3];
#endif
   return w;
}

/* Return non-zero if the ADDR instruction has a branch delay slot
   (i.e. it is a jump or branch instruction). */
static UInt
mips_instruction_has_delay_slot (Addr addr)
{
   UInt op, rs, rt;
   UInt inst = getUInt((UChar *)addr);

   op = itype_op (inst);
   if ((inst & 0xe0000000) != 0) {
      rs = itype_rs (inst);
      rt = itype_rt (inst);
      return (op >> 2 == 5        /* BEQL, BNEL, BLEZL, BGTZL: bits 0101xx  */
              || op == 29         /* JALX: bits 011101  */
              || (op == 17
                  && (rs == 8     /* BC1F, BC1FL, BC1T, BC1TL: 010001 01000  */
                      || (rs == 9 && (rt & 0x2) == 0)
                                  /* BC1ANY2F, BC1ANY2T: bits 010001 01001  */
                      || (rs == 10 && (rt & 0x2) == 0))));
                                  /* BC1ANY4F, BC1ANY4T: bits 010001 01010  */
   } else
      switch (op & 0x07) {        /* extract bits 28,27,26  */
         case 0:                  /* SPECIAL  */
            op = rtype_funct (inst);
         return (op == 8          /* JR  */
                 || op == 9);     /* JALR  */
         break;                   /* end SPECIAL  */
         case 1:                  /* REGIMM  */
            rs = itype_rs (inst);
            rt = itype_rt (inst); /* branch condition  */
            return ((rt & 0xc) == 0
                                  /* BLTZ, BLTZL, BGEZ, BGEZL: bits 000xx  */
                                  /* BLTZAL, BLTZALL, BGEZAL, BGEZALL: 100xx  */
                    || ((rt & 0x1e) == 0x1c && rs == 0));
                                  /* BPOSGE32, BPOSGE64: bits 1110x  */
            break;                /* end REGIMM  */
         default:                 /* J, JAL, BEQ, BNE, BLEZ, BGTZ  */
            return 1;
            break;
   }
}

/* Move the breakpoint at BPADDR out of any branch delay slot by shifting
   it backwards if necessary.  Return the address of the new location.  */
static Addr mips_adjust_breakpoint_address (Addr pc)
{
   Addr prev_addr;
   Addr boundary;
   Addr func_addr;
   Addr bpaddr = pc;
   Addr mask = 0xffffffff;
   int segsize;
   PtrdiffT offset;

   /* Calculate the starting address of the MIPS memory segment pc is in. */
   if (bpaddr & 0x80000000)  /* kernel segment */
      segsize = 29;
   else
      segsize = 31;          /* user segment */
   mask <<= segsize;
   boundary = pc & mask;

   /* Make sure we don't scan back before the beginning of the current
      function, since we may fetch constant data or insns that look like
      a jump. */

   // Placing a breakpoint, so pc should be in di of current epoch.
   const DiEpoch cur_ep = VG_(current_DiEpoch)();

   if (VG_(get_inst_offset_in_function) (cur_ep, bpaddr, &offset)) {
      func_addr = bpaddr - offset;
      if (func_addr > boundary && func_addr <= bpaddr)
         boundary = func_addr;
   }

   if (bpaddr == boundary)
      return bpaddr;
   /* If the previous instruction has a branch delay slot, we have
      to move the breakpoint to the branch instruction. */
   prev_addr = bpaddr - 4;
   if (mips_instruction_has_delay_slot (prev_addr))
      bpaddr = prev_addr;

   return bpaddr;
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

   VexGuestMIPS32State* mips1 = (VexGuestMIPS32State*) get_arch (set, tst);

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
   case 32: *mod = False; break; // GDBTD???? VEX { "status", 1024, 32 },
   case 33: VG_(transfer) (&mips1->guest_LO, buf, dir, size, mod); break;
   case 34: VG_(transfer) (&mips1->guest_HI, buf, dir, size, mod); break;
   case 35: *mod = False; break; // GDBTD???? VEX { "badvaddr", 1120, 32 },
   case 36: *mod = False; break; // GDBTD???? VEX { "cause", 1152, 32 },
   case 37:
      /* If a breakpoint is set on the instruction in a branch delay slot,
         GDB gets confused.  When the breakpoint is hit, the PC isn't on
         the instruction in the branch delay slot, the PC will point to
         the branch instruction. */
      mips1->guest_PC = mips_adjust_breakpoint_address(mips1->guest_PC);
      VG_(transfer) (&mips1->guest_PC,  buf, dir, size, mod);
      break;
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
   case 72: *mod = False; break; // GDBTD???? VEX{ "restart", 2304, 32 },
   default: VG_(printf)("regno: %d\n", regno); vg_assert(0);
   }
}

static
const char* target_xml (Bool shadow_mode)
{
   if (shadow_mode) {
      return "mips-linux-valgrind.xml";
   } else {
      return "mips-linux.xml";
   }  
}

static CORE_ADDR** target_get_dtv (ThreadState *tst)
{
   VexGuestMIPS32State* mips32 = (VexGuestMIPS32State*)&tst->arch.vex;
   // Top of MIPS tcbhead structure is located 0x7000 bytes before the value
   // of ULR. Dtv is the first of two pointers in tcbhead structure.
   // More details can be found in GLIBC/sysdeps/nptl/tls.h.
   return (CORE_ADDR**)((CORE_ADDR)mips32->guest_ULR
                        - 0x7000 - 2 * sizeof(CORE_ADDR));
}

static struct valgrind_target_ops low_target = {
   num_regs,
   29, //sp = r29, which is register offset 29 in regs
   regs,
   transfer_register,
   get_pc,
   set_pc,
   "mips",
   target_xml,
   target_get_dtv
};

void mips32_init_architecture (struct valgrind_target_ops *target)
{
   *target = low_target;
   set_register_cache (regs, num_regs);
   gdbserver_expedite_regs = expedite_regs;
}
