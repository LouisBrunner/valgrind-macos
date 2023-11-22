/* -*- mode: C; c-basic-offset: 3; -*- */

/*---------------------------------------------------------------*/
/*--- begin                              guest_s390_helpers.c ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright IBM Corp. 2010-2020

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

/* Contributed by Florian Krohm */

#include "libvex_basictypes.h"
#include "libvex_emnote.h"
#include "libvex_guest_s390x.h"
#include "libvex_ir.h"
#include "libvex.h"
#include "libvex_s390x_common.h"

#include "main_util.h"
#include "main_globals.h"
#include "guest_generic_bb_to_IR.h"
#include "guest_s390_defs.h"
#include "s390_defs.h"               /* S390_BFP_ROUND_xyzzy */

void
LibVEX_GuestS390X_initialise(VexGuestS390XState *state)
{
/*------------------------------------------------------------*/
/*--- Initialise ar registers                              ---*/
/*------------------------------------------------------------*/

   state->guest_a0 = 0;
   state->guest_a1 = 0;
   state->guest_a2 = 0;
   state->guest_a3 = 0;
   state->guest_a4 = 0;
   state->guest_a5 = 0;
   state->guest_a6 = 0;
   state->guest_a7 = 0;
   state->guest_a8 = 0;
   state->guest_a9 = 0;
   state->guest_a10 = 0;
   state->guest_a11 = 0;
   state->guest_a12 = 0;
   state->guest_a13 = 0;
   state->guest_a14 = 0;
   state->guest_a15 = 0;

/*------------------------------------------------------------*/
/*--- Initialise vr registers                             ---*/
/*------------------------------------------------------------*/

#define VRZERO(vr) \
   do { \
      vr.w64[0] = vr.w64[1] = 0ULL; \
   } while(0);

   VRZERO(state->guest_v0)
   VRZERO(state->guest_v1)
   VRZERO(state->guest_v2)
   VRZERO(state->guest_v3)
   VRZERO(state->guest_v4)
   VRZERO(state->guest_v5)
   VRZERO(state->guest_v6)
   VRZERO(state->guest_v7)
   VRZERO(state->guest_v8)
   VRZERO(state->guest_v9)
   VRZERO(state->guest_v10)
   VRZERO(state->guest_v11)
   VRZERO(state->guest_v12)
   VRZERO(state->guest_v13)
   VRZERO(state->guest_v14)
   VRZERO(state->guest_v15)
   VRZERO(state->guest_v16)
   VRZERO(state->guest_v17)
   VRZERO(state->guest_v18)
   VRZERO(state->guest_v19)
   VRZERO(state->guest_v20)
   VRZERO(state->guest_v21)
   VRZERO(state->guest_v22)
   VRZERO(state->guest_v23)
   VRZERO(state->guest_v24)
   VRZERO(state->guest_v25)
   VRZERO(state->guest_v26)
   VRZERO(state->guest_v27)
   VRZERO(state->guest_v28)
   VRZERO(state->guest_v29)
   VRZERO(state->guest_v30)
   VRZERO(state->guest_v31)

#undef VRZERO
/*------------------------------------------------------------*/
/*--- Initialise gpr registers                             ---*/
/*------------------------------------------------------------*/

   state->guest_r0 = 0;
   state->guest_r1 = 0;
   state->guest_r2 = 0;
   state->guest_r3 = 0;
   state->guest_r4 = 0;
   state->guest_r5 = 0;
   state->guest_r6 = 0;
   state->guest_r7 = 0;
   state->guest_r8 = 0;
   state->guest_r9 = 0;
   state->guest_r10 = 0;
   state->guest_r11 = 0;
   state->guest_r12 = 0;
   state->guest_r13 = 0;
   state->guest_r14 = 0;
   state->guest_r15 = 0;

/*------------------------------------------------------------*/
/*--- Initialise S390 miscellaneous registers              ---*/
/*------------------------------------------------------------*/

   state->guest_counter = 0;
   state->guest_fpc = 0;
   state->guest_IA = 0;

/*------------------------------------------------------------*/
/*--- Initialise S390 pseudo registers                     ---*/
/*------------------------------------------------------------*/

   state->guest_SYSNO = 0;

/*------------------------------------------------------------*/
/*--- Initialise generic pseudo registers                  ---*/
/*------------------------------------------------------------*/

   state->guest_NRADDR = 0;
   state->guest_CMSTART = 0;
   state->guest_CMLEN = 0;
   state->guest_IP_AT_SYSCALL = 0;
   state->guest_EMNOTE = EmNote_NONE;
   state->host_EvC_COUNTER = 0;
   state->host_EvC_FAILADDR = 0;

/*------------------------------------------------------------*/
/*--- Initialise thunk                                     ---*/
/*------------------------------------------------------------*/

   state->guest_CC_OP = 0;
   state->guest_CC_DEP1 = 0;
   state->guest_CC_DEP2 = 0;
   state->guest_CC_NDEP = 0;

   __builtin_memset(state->padding, 0x0, sizeof(state->padding));
}


/* Figure out if any part of the guest state contained in minoff
   .. maxoff requires precise memory exceptions.  If in doubt return
   True (but this generates significantly slower code).  */
Bool
guest_s390x_state_requires_precise_mem_exns (
   Int minoff, Int maxoff, VexRegisterUpdates pxControl
)
{
   Int lr_min = S390X_GUEST_OFFSET(guest_LR);
   Int lr_max = lr_min + 8 - 1;
   Int sp_min = S390X_GUEST_OFFSET(guest_SP);
   Int sp_max = sp_min + 8 - 1;
   Int fp_min = S390X_GUEST_OFFSET(guest_FP);
   Int fp_max = fp_min + 8 - 1;
   Int ia_min = S390X_GUEST_OFFSET(guest_IA);
   Int ia_max = ia_min + 8 - 1;

   if (maxoff < sp_min || minoff > sp_max) {
      /* No overlap with SP */
      if (pxControl == VexRegUpdSpAtMemAccess)
         return False; // We only need to check stack pointer.
   } else {
      return True;
   }

   if (maxoff < lr_min || minoff > lr_max) {
      /* No overlap with LR */
   } else {
      return True;
   }

   if (maxoff < fp_min || minoff > fp_max) {
      /* No overlap with FP */
   } else {
      return True;
   }

   if (maxoff < ia_min || minoff > ia_max) {
      /* No overlap with IA */
   } else {
      return True;
   }

   return False;
}


#define ALWAYSDEFD(field)                             \
    { S390X_GUEST_OFFSET(field),            \
      (sizeof ((VexGuestS390XState*)0)->field) }

VexGuestLayout s390xGuest_layout = {

   /* Total size of the guest state, in bytes. */
   .total_sizeB = sizeof(VexGuestS390XState),

   /* Describe the stack pointer. */
   .offset_SP = S390X_GUEST_OFFSET(guest_SP),
   .sizeof_SP = 8,

   /* Describe the frame pointer. */
   .offset_FP = S390X_GUEST_OFFSET(guest_FP),
   .sizeof_FP = 8,

   /* Describe the instruction pointer. */
   .offset_IP = S390X_GUEST_OFFSET(guest_IA),
   .sizeof_IP = 8,

   /* Describe any sections to be regarded by Memcheck as
      'always-defined'. */
   .n_alwaysDefd = 9,

   /* Flags thunk: OP and NDEP are always defined, whereas DEP1
      and DEP2 have to be tracked.  See detailed comment in
      gdefs.h on meaning of thunk fields. */
   .alwaysDefd = {
      /*  0 */ ALWAYSDEFD(guest_CC_OP),     /* generic */
      /*  1 */ ALWAYSDEFD(guest_CC_NDEP),   /* generic */
      /*  2 */ ALWAYSDEFD(guest_EMNOTE),    /* generic */
      /*  3 */ ALWAYSDEFD(guest_CMSTART),   /* generic */
      /*  4 */ ALWAYSDEFD(guest_CMLEN),     /* generic */
      /*  5 */ ALWAYSDEFD(guest_IP_AT_SYSCALL), /* generic */
      /*  6 */ ALWAYSDEFD(guest_IA),        /* control reg */
      /*  7 */ ALWAYSDEFD(guest_fpc),       /* control reg */
      /*  8 */ ALWAYSDEFD(guest_counter),   /* internal usage register */
   }
};

/*------------------------------------------------------------*/
/*--- Dirty helper for EXecute                             ---*/
/*------------------------------------------------------------*/
void
s390x_dirtyhelper_EX(ULong torun)
{
   last_execute_target = torun;
}


/*------------------------------------------------------------*/
/*--- Dirty helper for Clock instructions                  ---*/
/*------------------------------------------------------------*/
#if defined(VGA_s390x)
ULong
s390x_dirtyhelper_STCK(ULong *addr)
{
   UInt cc;

   asm volatile("stck %0\n"
                "ipm %1\n"
                "srl %1,28\n"
                : "+Q" (*addr), "=d" (cc) : : "cc");
   return cc;
}

ULong
s390x_dirtyhelper_STCKE(ULong *addr)
{
   UInt cc;

   asm volatile("stcke %0\n"
                "ipm %1\n"
                "srl %1,28\n"
                : "+Q" (*addr), "=d" (cc) : : "cc");
   return cc;
}

ULong s390x_dirtyhelper_STCKF(ULong *addr)
{
   UInt cc;

   asm volatile(".insn s,0xb27c0000,%0\n"
                "ipm %1\n"
                "srl %1,28\n"
                : "+Q" (*addr), "=d" (cc) : : "cc");
   return cc;
}
#else
ULong s390x_dirtyhelper_STCK(ULong *addr)  {return 3;}
ULong s390x_dirtyhelper_STCKF(ULong *addr) {return 3;}
ULong s390x_dirtyhelper_STCKE(ULong *addr) {return 3;}
#endif /* VGA_s390x */

/*------------------------------------------------------------*/
/*--- Dirty helper for Store Facility instruction          ---*/
/*------------------------------------------------------------*/
#if defined(VGA_s390x)

static ULong
s390_stfle_range(UInt lo, UInt hi)
{
   return ((1UL << (hi + 1 - lo)) - 1) << (63 - (hi % 64));
}

ULong
s390x_dirtyhelper_STFLE(VexGuestS390XState *guest_state, ULong *addr)
{
   ULong hoststfle[S390_NUM_FACILITY_DW], cc, last_dw, i;
   register ULong reg0 asm("0") = guest_state->guest_r0;
   last_dw = reg0 & 0xf;

   /* Restrict to facilities that we know about and that we assume to be
      compatible with Valgrind.  Of course, in this way we may reject features
      that Valgrind is not really involved in (and thus would be compatible
      with), but quering for such features doesn't seem like a typical use
      case. */
   ULong accepted_facility[S390_NUM_FACILITY_DW] = {
      /* ===  0 .. 63  === */
      (s390_stfle_range(0, 16)
       /* 17: message-security-assist, not supported */
       | s390_stfle_range(18, 19)
       /* 20: HFP-multiply-and-add/subtract, not supported */
       | s390_stfle_range(21, 22)
       /* 23: HFP-unnormalized-extension, not supported */
       | s390_stfle_range(24, 25)
       /* 26: parsing-enhancement, not supported */
       | s390_stfle_range(27, 28)
       /* 29: unassigned */
       | s390_stfle_range(30, 30)
       /* 31: extract-CPU-time, not supported */
       | s390_stfle_range(32, 41)
       /* 42-43: DFP, not fully supported */
       /* 44: PFPO, not fully supported */
       | s390_stfle_range(45, 47)
       /* 48: DFP zoned-conversion, not supported */
       /* 49: includes PPA, not supported */
       /* 50: constrained transactional-execution, not supported */
       | s390_stfle_range(51, 55)
       /* 56: unassigned */
       /* 57: MSA5, not supported */
       | s390_stfle_range(58, 63)),

      /* ===  64 .. 127  === */
      (s390_stfle_range(64, 72)
       /* 73: transactional-execution, not supported */
       | s390_stfle_range(74, 75)
       /* 76: MSA3, not supported */
       /* 77: MSA4, not supported */
       | s390_stfle_range(78, 78)
       /* 80: DFP packed-conversion, not supported */
       /* 81: PPA-in-order, not supported */
       | s390_stfle_range(82, 82)
       /* 83-127: unassigned */ ),

      /* ===  128 .. 191  === */
      (s390_stfle_range(128, 131)
       /* 132: unassigned */
       /* 133: guarded-storage, not supported */
       /* 134: vector packed decimal, not supported */
       | s390_stfle_range(135, 135)
       /* 136: unassigned */
       /* 137: unassigned */
       | s390_stfle_range(138, 142)
       /* 143: unassigned */
       | s390_stfle_range(144, 145)
       /* 146: MSA8, not supported */
       | s390_stfle_range(147, 149)
       /* 150: unassigned */
       /* 151: DEFLATE-conversion, not supported */
       /* 152: vector packed decimal enhancement, not supported */
       /* 153: unassigned */
       /* 154: unassigned */
       /* 155: MSA9, not supported */
       | s390_stfle_range(156, 156)
       /* 157-167: unassigned */
       | s390_stfle_range(168, 168)
       /* 168-191: unassigned */ ),

      /* ===  192 .. 255  === */
      /* 192: vector-packed-decimal, not supported */
      (s390_stfle_range(193, 194)
       /* 195: unassigned */
       | s390_stfle_range(196, 197)),
   };

   asm(".insn s,0xb2b00000,%0\n" /* stfle */
       "ipm   %2\n"
       "srl   %2,28\n"
       : "=Q"(hoststfle), "+d"(reg0), "=d"(cc)
       :
       : "cc");

   /* Update guest register 0  with what STFLE set r0 to */
   guest_state->guest_r0 = reg0;

   /* VM facilities = host facilities, filtered by acceptance */
   for (i = 0; i <= last_dw; ++i) {
      if (i < S390_NUM_FACILITY_DW)
         addr[i] = hoststfle[i] & accepted_facility[i];
      else
         addr[i] = 0; /* mask out any excess doublewords */
   }

   return cc;
}

#else

ULong
s390x_dirtyhelper_STFLE(VexGuestS390XState *guest_state, ULong *addr)
{
   return 3;
}
#endif /* VGA_s390x */

/*------------------------------------------------------------*/
/*--- Dirty helper for the "convert unicode" insn family.  ---*/
/*------------------------------------------------------------*/
void
s390x_dirtyhelper_CUxy(UChar *address, ULong data, ULong num_bytes)
{
   UInt i;

   vassert(num_bytes >= 1 && num_bytes <= 4);

   /* Store the least significant NUM_BYTES bytes in DATA left to right
      at ADDRESS. */
   for (i = 1; i <= num_bytes; ++i) {
      address[num_bytes - i] = data & 0xff;
      data >>= 8;
   }
}


/*------------------------------------------------------------*/
/*--- Clean helper for CU21.                               ---*/
/*------------------------------------------------------------*/

/* The function performs a CU21 operation. It returns three things
   encoded in an ULong value:
   - the converted bytes (at most 4)
   - the number of converted bytes
   - an indication whether LOW_SURROGATE, if any, is invalid

   64      48                16           8                       0
    +-------+-----------------+-----------+-----------------------+
    |  0x0  | converted bytes | num_bytes | invalid_low_surrogate |
    +-------+-----------------+-----------+-----------------------+
*/
ULong
s390_do_cu21(UInt srcval, UInt low_surrogate)
{
   ULong retval = 0;   // shut up gcc
   UInt b1, b2, b3, b4, num_bytes, invalid_low_surrogate = 0;

   srcval &= 0xffff;

   /* Determine the number of bytes in the converted value */
   if (srcval <= 0x007f)
      num_bytes = 1;
   else if (srcval >= 0x0080 && srcval <= 0x07ff)
      num_bytes = 2;
   else if ((srcval >= 0x0800 && srcval <= 0xd7ff) ||
            (srcval >= 0xdc00 && srcval <= 0xffff))
      num_bytes = 3;
   else
      num_bytes = 4;

   /* Determine UTF-8 bytes according to calculated num_bytes */
   switch (num_bytes){
   case 1:
      retval = srcval;
      break;

   case 2:
      /* order of bytes left to right: b1, b2 */
      b1  = 0xc0;
      b1 |= srcval >> 6;

      b2  = 0x80;
      b2 |= srcval & 0x3f;

      retval = (b1 << 8) | b2;
      break;

   case 3:
      /* order of bytes left to right: b1, b2, b3 */
      b1  = 0xe0;
      b1 |= srcval >> 12;

      b2  = 0x80;
      b2 |= (srcval >> 6) & 0x3f;

      b3  = 0x80;
      b3 |= srcval & 0x3f;

      retval = (b1 << 16) | (b2 << 8) | b3;
      break;

   case 4: {
      /* order of bytes left to right: b1, b2, b3, b4 */
      UInt high_surrogate = srcval;
      UInt uvwxy = ((high_surrogate >> 6) & 0xf) + 1;   // abcd + 1

      b1  = 0xf0;
      b1 |= uvwxy >> 2;     // uvw

      b2  = 0x80;
      b2 |= (uvwxy & 0x3) << 4;           // xy
      b2 |= (high_surrogate >> 2) & 0xf;  // efgh

      b3  = 0x80;
      b3 |= (high_surrogate & 0x3) << 4;   // ij
      b3 |= (low_surrogate >> 6) & 0xf;    // klmn

      b4  = 0x80;
      b4 |= low_surrogate & 0x3f;

      retval = (b1 << 24) | (b2 << 16) | (b3 << 8) | b4;

      invalid_low_surrogate = (low_surrogate & 0xfc00) != 0xdc00;
      break;
   }
   }

   /* At this point RETVAL contains the converted bytes.
      Build up the final return value. */
   return (retval << 16) | (num_bytes << 8) | invalid_low_surrogate;
}


/*------------------------------------------------------------*/
/*--- Clean helper for CU24.                               ---*/
/*------------------------------------------------------------*/

/* The function performs a CU24 operation. It returns two things
   encoded in an ULong value:
   - the 4 converted bytes
   - an indication whether LOW_SURROGATE, if any, is invalid

   64     40                 8                       0
    +------------------------+-----------------------+
    |  0x0 | converted bytes | invalid_low_surrogate |
    +------------------------+-----------------------+
*/
ULong
s390_do_cu24(UInt srcval, UInt low_surrogate)
{
   ULong retval;
   UInt invalid_low_surrogate = 0;

   srcval &= 0xffff;

   if ((srcval <= 0xd7ff) ||
       (srcval >= 0xdc00 && srcval <= 0xffff)) {
      retval = srcval;
   } else {
      /* D800 - DBFF */
      UInt high_surrogate = srcval;
      UInt uvwxy  = ((high_surrogate >> 6) & 0xf) + 1;   // abcd + 1
      UInt efghij = high_surrogate & 0x3f;
      UInt klmnoprst = low_surrogate & 0x3ff;

      retval = (uvwxy << 16) | (efghij << 10) | klmnoprst;

      invalid_low_surrogate = (low_surrogate & 0xfc00) != 0xdc00;
   }

   /* At this point RETVAL contains the converted bytes.
      Build up the final return value. */
   return (retval << 8) | invalid_low_surrogate;
}


/*------------------------------------------------------------*/
/*--- Clean helper for CU42.                               ---*/
/*------------------------------------------------------------*/

/* The function performs a CU42 operation. It returns three things
   encoded in an ULong value:
   - the converted bytes (at most 4)
   - the number of converted bytes (2 or 4; 0 if invalid character)
   - an indication whether the UTF-32 character is invalid

   64      48                16           8                   0
    +-------+-----------------+-----------+-------------------+
    |  0x0  | converted bytes | num_bytes | invalid_character |
    +-------+-----------------+-----------+-------------------+
*/
ULong
s390_do_cu42(UInt srcval)
{
   ULong retval;
   UInt num_bytes, invalid_character = 0;

   if ((srcval >= 0x0000 && srcval <= 0xd7ff) ||
       (srcval >= 0xdc00 && srcval <= 0xffff)) {
      retval = srcval;
      num_bytes = 2;
   } else if (srcval >= 0x00010000 && srcval <= 0x0010FFFF) {
      UInt uvwxy  = srcval >> 16;
      UInt abcd   = (uvwxy - 1) & 0xf;
      UInt efghij = (srcval >> 10) & 0x3f;

      UInt high_surrogate = (0xd8 << 8) | (abcd << 6) | efghij;
      UInt low_surrogate  = (0xdc << 8) | (srcval & 0x3ff);

      retval = (high_surrogate << 16) | low_surrogate;
      num_bytes = 4;
   } else {
      /* D800 - DBFF or 00110000 - FFFFFFFF */
      invalid_character = 1;
      retval = num_bytes = 0;   /* does not matter; not used */
   }

   /* At this point RETVAL contains the converted bytes.
      Build up the final return value. */
   return (retval << 16) | (num_bytes << 8) | invalid_character;
}


/*------------------------------------------------------------*/
/*--- Clean helper for CU41.                               ---*/
/*------------------------------------------------------------*/

/* The function performs a CU41 operation. It returns three things
   encoded in an ULong value:
   - the converted bytes (at most 4)
   - the number of converted bytes (1, 2, 3, or 4; 0 if invalid character)
   - an indication whether the UTF-32 character is invalid

   64      48                16           8                   0
    +-------+-----------------+-----------+-------------------+
    |  0x0  | converted bytes | num_bytes | invalid_character |
    +-------+-----------------+-----------+-------------------+
*/
ULong
s390_do_cu41(UInt srcval)
{
   ULong retval;
   UInt num_bytes, invalid_character = 0;

   if (srcval <= 0x7f) {
      retval = srcval;
      num_bytes = 1;
   } else if (srcval >= 0x80 && srcval <= 0x7ff) {
      UInt fghij  = srcval >> 6;
      UInt klmnop = srcval & 0x3f;
      UInt byte1  = (0xc0 | fghij);
      UInt byte2  = (0x80 | klmnop);

      retval = (byte1 << 8) | byte2;
      num_bytes = 2;
   } else if ((srcval >= 0x800  && srcval <= 0xd7ff) ||
              (srcval >= 0xdc00 && srcval <= 0xffff)) {
      UInt abcd   = srcval >> 12;
      UInt efghij = (srcval >> 6) & 0x3f;
      UInt klmnop = srcval & 0x3f;
      UInt byte1  = 0xe0 | abcd;
      UInt byte2  = 0x80 | efghij;
      UInt byte3  = 0x80 | klmnop;

      retval = (byte1 << 16) | (byte2 << 8) | byte3;
      num_bytes = 3;
   } else if (srcval >= 0x10000 && srcval <= 0x10ffff) {
      UInt uvw    = (srcval >> 18) & 0x7;
      UInt xy     = (srcval >> 16) & 0x3;
      UInt efgh   = (srcval >> 12) & 0xf;
      UInt ijklmn = (srcval >>  6) & 0x3f;
      UInt opqrst = srcval & 0x3f;
      UInt byte1  = 0xf0 | uvw;
      UInt byte2  = 0x80 | (xy << 4) | efgh;
      UInt byte3  = 0x80 | ijklmn;
      UInt byte4  = 0x80 | opqrst;

      retval = (byte1 << 24) | (byte2 << 16) | (byte3 << 8) | byte4;
      num_bytes = 4;
   } else {
      /* d800 ... dbff or 00110000 ... ffffffff */
      invalid_character = 1;

      retval = 0;
      num_bytes = 0;
   }

   /* At this point RETVAL contains the converted bytes.
      Build up the final return value. */
   return (retval << 16) | (num_bytes << 8) | invalid_character;
}


/*------------------------------------------------------------*/
/*--- Clean helpers for CU12.                              ---*/
/*------------------------------------------------------------*/

/* The function looks at the first byte of an UTF-8 character and returns
   two things encoded in an ULong value:

   - the number of bytes that need to be read
   - an indication whether the UTF-8 character is invalid

   64      16           8                   0
    +-------------------+-------------------+
    |  0x0  | num_bytes | invalid_character |
    +-------+-----------+-------------------+
*/
ULong
s390_do_cu12_cu14_helper1(UInt byte, UInt etf3_and_m3_is_1)
{
   vassert(byte <= 0xff);

   /* Check whether the character is invalid */
   if (byte >= 0x80 && byte <= 0xbf) return 1;
   if (byte >= 0xf8) return 1;

   if (etf3_and_m3_is_1) {
      if (byte == 0xc0 || byte == 0xc1) return 1;
      if (byte >= 0xf5 && byte <= 0xf7) return 1;
   }

   /* Character is valid */
   if (byte <= 0x7f) return 1 << 8;   // 1 byte
   if (byte <= 0xdf) return 2 << 8;   // 2 bytes
   if (byte <= 0xef) return 3 << 8;   // 3 bytes

   return 4 << 8;  // 4 bytes
}

/* The function performs a CU12 or CU14 operation. BYTE1, BYTE2, etc are the
   bytes as read from the input stream, left to right. BYTE1 is a valid
   byte. The function returns three things encoded in an ULong value:

   - the converted bytes
   - the number of converted bytes (2 or 4; 0 if invalid character)
   - an indication whether the UTF-16 character is invalid

   64      48                16           8                   0
    +-------+-----------------+-----------+-------------------+
    |  0x0  | converted bytes | num_bytes | invalid_character |
    +-------+-----------------+-----------+-------------------+
*/
static ULong
s390_do_cu12_cu14_helper2(UInt byte1, UInt byte2, UInt byte3, UInt byte4,
                          ULong stuff, Bool is_cu12)
{
   UInt num_src_bytes = stuff >> 1, etf3_and_m3_is_1 = stuff & 0x1;
   UInt num_bytes = 0, invalid_character = 0;
   ULong retval = 0;

   vassert(num_src_bytes <= 4);

   switch (num_src_bytes) {
   case 1:
      num_bytes = 2;
      retval = byte1;
      break;

   case 2: {
      /* Test validity */
      if (etf3_and_m3_is_1) {
         if (byte2 < 0x80 || byte2 > 0xbf) {
            invalid_character = 1;
            break;
         }
      }

      /* OK */
      UInt fghij  = byte1 & 0x1f;
      UInt klmnop = byte2 & 0x3f;

      num_bytes = 2;
      retval = (fghij << 6) | klmnop;
      break;
   }

   case 3: {
      /* Test validity */
      if (etf3_and_m3_is_1) {
         if (byte1 == 0xe0) {
            if ((byte2 < 0xa0 || byte2 > 0xbf) ||
                (byte3 < 0x80 || byte3 > 0xbf)) {
               invalid_character = 1;
               break;
            }
         }
         if ((byte1 >= 0xe1 && byte1 <= 0xec) ||
             byte1 == 0xee || byte1 == 0xef) {
            if ((byte2 < 0x80 || byte2 > 0xbf) ||
                (byte3 < 0x80 || byte3 > 0xbf)) {
               invalid_character = 1;
               break;
            }
         }
         if (byte1 == 0xed) {
            if ((byte2 < 0x80 || byte2 > 0x9f) ||
                (byte3 < 0x80 || byte3 > 0xbf)) {
               invalid_character = 1;
               break;
            }
         }
      }

      /* OK */
      UInt abcd   = byte1 & 0xf;
      UInt efghij = byte2 & 0x3f;
      UInt klmnop = byte3 & 0x3f;

      num_bytes = 2;
      retval = (abcd << 12) | (efghij << 6) | klmnop;
      break;
   }

   case 4: {
      /* Test validity */
      if (etf3_and_m3_is_1) {
         if (byte1 == 0xf0) {
            if ((byte2 < 0x90 || byte2 > 0xbf) ||
                (byte3 < 0x80 || byte3 > 0xbf) ||
                (byte4 < 0x80 || byte4 > 0xbf)) {
               invalid_character = 1;
               break;
            }
         }
         if (byte1 == 0xf1 || byte1 == 0xf2 || byte1 == 0xf3) {
            if ((byte2 < 0x80 || byte2 > 0xbf) ||
                (byte3 < 0x80 || byte3 > 0xbf) ||
                (byte4 < 0x80 || byte4 > 0xbf)) {
               invalid_character = 1;
               break;
            }
         }
         if (byte1 == 0xf4) {
            if ((byte2 < 0x80 || byte2 > 0x8f) ||
                (byte3 < 0x80 || byte3 > 0xbf) ||
                (byte4 < 0x80 || byte4 > 0xbf)) {
               invalid_character = 1;
               break;
            }
         }
      }

      /* OK */
      UInt uvw    = byte1 & 0x7;
      UInt xy     = (byte2 >> 4) & 0x3;
      UInt uvwxy  = (uvw << 2) | xy;
      UInt efgh   = byte2 & 0xf;
      UInt ij     = (byte3 >> 4) & 0x3;
      UInt klmn   = byte3 & 0xf;
      UInt opqrst = byte4 & 0x3f;
      
      if (is_cu12) {
         UInt abcd = (uvwxy - 1) & 0xf;
         UInt high_surrogate = (0xd8 << 8) | (abcd << 6) | (efgh << 2) | ij;
         UInt low_surrogate  = (0xdc << 8) | (klmn << 6) | opqrst;

         num_bytes = 4;
         retval = (high_surrogate << 16) | low_surrogate;
      } else {
         num_bytes = 4;
         retval =
            (uvwxy << 16) | (efgh << 12) | (ij << 10) | (klmn << 6) | opqrst;
      }
      break;
   }
   }

   if (! is_cu12) num_bytes = 4;   // for CU14, by definition

   /* At this point RETVAL contains the converted bytes.
      Build up the final return value. */
   return (retval << 16) | (num_bytes << 8) | invalid_character;
}

ULong
s390_do_cu12_helper2(UInt byte1, UInt byte2, UInt byte3, UInt byte4,
                     ULong stuff)
{
   return s390_do_cu12_cu14_helper2(byte1, byte2, byte3, byte4, stuff,
                                    /* is_cu12 = */ 1);
}

ULong
s390_do_cu14_helper2(UInt byte1, UInt byte2, UInt byte3, UInt byte4,
                     ULong stuff)
{
   return s390_do_cu12_cu14_helper2(byte1, byte2, byte3, byte4, stuff,
                                    /* is_cu12 = */ 0);
}


/*------------------------------------------------------------*/
/*--- Clean helper for "convert to binary".                ---*/
/*------------------------------------------------------------*/
#if defined(VGA_s390x)
UInt
s390_do_cvb(ULong decimal)
{
   UInt binary;

   __asm__ volatile (
        "cvb %[result],%[input]\n\t"
          : [result] "=d"(binary)
          : [input] "R"(decimal)
   );

   return binary;
}

#else
UInt s390_do_cvb(ULong decimal) { return 0; }
#endif


/*------------------------------------------------------------*/
/*--- Clean helper for "convert to decimal".                ---*/
/*------------------------------------------------------------*/
#if defined(VGA_s390x)
ULong
s390_do_cvd(ULong binary_in)
{
   UInt binary = binary_in & 0xffffffffULL;
   ULong decimal;

   __asm__ volatile (
        "cvd %[input],%[result]\n\t"
          : [result] "=m"(decimal)
          : [input] "d"(binary)
   );

   return decimal;
}

#else
ULong s390_do_cvd(ULong binary) { return 0; }
#endif

/*------------------------------------------------------------*/
/*--- Clean helper for "Extract cache attribute".          ---*/
/*------------------------------------------------------------*/
#if defined(VGA_s390x)
ULong
s390_do_ecag(ULong op2addr)
{
   ULong result;

   __asm__ volatile(".insn rsy,0xEB000000004C,%[out],0,0(%[in])\n\t"
                    : [out] "=d"(result)
                    : [in] "d"(op2addr));
   return result;
}

#else
ULong s390_do_ecag(ULong op2addr) { return 0; }
#endif

/*------------------------------------------------------------*/
/*--- Clean helper for "Perform Floating Point Operation". ---*/
/*------------------------------------------------------------*/
#if defined(VGA_s390x)
UInt
s390_do_pfpo(UInt gpr0)
{
   UChar rm;
   UChar op1_ty, op2_ty;

   rm  = gpr0 & 0xf;
   if (rm > 1 && rm < 8)
      return EmFail_S390X_invalid_PFPO_rounding_mode;

   op1_ty = (gpr0 >> 16) & 0xff; // gpr0[40:47]
   op2_ty = (gpr0 >> 8)  & 0xff; // gpr0[48:55]
   /* Operand type must be BFP 32, 64, 128 or DFP 32, 64, 128
      which correspond to 0x5, 0x6, 0x7, 0x8, 0x9, 0xa respectively.
      Any other operand type value is unsupported */
   if ((op1_ty == op2_ty) ||
       (op1_ty < 0x5 || op1_ty > 0xa) ||
       (op2_ty < 0x5 || op2_ty > 0xa))
      return EmFail_S390X_invalid_PFPO_function;

   return EmNote_NONE;
}
#else
UInt s390_do_pfpo(UInt gpr0) { return 0; }
#endif

/*------------------------------------------------------------*/
/*--- Helper for condition code.                           ---*/
/*------------------------------------------------------------*/

#if defined(VGA_s390x)
typedef long double Float128;
union s390x_F64 { ULong i; Double f; };
union s390x_F128 { struct { ULong hi, lo; } i; Float128 f; };

/* Convert an IRRoundingMode value to s390_bfp_round_t */
static s390_bfp_round_t
decode_bfp_rounding_mode(UInt irrm)
{
   switch (irrm) {
   case Irrm_NEAREST: return S390_BFP_ROUND_NEAREST_EVEN;
   case Irrm_NegINF:  return S390_BFP_ROUND_NEGINF;
   case Irrm_PosINF:  return S390_BFP_ROUND_POSINF;
   case Irrm_ZERO:    return S390_BFP_ROUND_ZERO;
   }
   vpanic("decode_bfp_rounding_mode");
}

#define S390_CC_FOR_BINARY(opcode,cc_dep1,cc_dep2) \
({ \
   __asm__ volatile ( \
        opcode " %[op1],%[op2]\n\t" \
        "ipm %[psw]\n\t"           : [psw] "=d"(psw), [op1] "+d"(cc_dep1) \
                                   : [op2] "d"(cc_dep2) \
                                   : "cc");\
   psw >> 28;   /* cc */ \
})

#define S390_CC_FOR_TERNARY(opcode,cc_dep1,cc_dep2) \
({ \
   __asm__ volatile ( \
        opcode ",%[op1],%[op1],%[op2],0\n\t" \
        "ipm %[psw]\n\t"           : [psw] "=d"(psw), [op1] "+d"(cc_dep1) \
                                   : [op2] "d"(cc_dep2) \
                                   : "cc");\
   psw >> 28;   /* cc */ \
})

#define S390_CC_FOR_TERNARY_SUBB(opcode,cc_dep1,cc_dep2,cc_ndep) \
({ \
   /* Recover the original DEP2 value. See comment near s390_cc_thunk_put3 \
      for rationale. */ \
   cc_dep2 = cc_dep2 ^ cc_ndep; \
   ULong tmp = 1; \
   __asm__ volatile ( \
        "sr %[tmp],%[op3]\n\t" /* borrow to cc */ \
        opcode " %[op1],%[op2]\n\t" /* then redo the op */\
        "ipm %[psw]\n\t"           : [psw] "=d"(psw), [op1] "+&d"(cc_dep1), \
                                     [tmp] "+&d"(tmp) \
                                   : [op2] "d"(cc_dep2), [op3] "d"(cc_ndep) \
                                   : "cc");\
   psw >> 28;   /* cc */ \
})

#define S390_CC_FOR_TERNARY_ADDC(opcode,cc_dep1,cc_dep2,cc_ndep) \
({ \
   /* Recover the original DEP2 value. See comment near s390_cc_thunk_put3 \
      for rationale. */ \
   cc_dep2 = cc_dep2 ^ cc_ndep; \
   ULong tmp; \
   __asm__ volatile ( \
        "lgfr %[tmp],%[op3]\n\t" /* first load cc_ndep */ \
        "aghi %[tmp],0\n\t" /* and convert it into a cc */ \
        opcode " %[op1],%[op2]\n\t" /* then redo the op */\
        "ipm %[psw]\n\t"           : [psw] "=d"(psw), [op1] "+&d"(cc_dep1), \
                                     [tmp] "=&d"(tmp) \
                                   : [op2] "d"(cc_dep2), [op3] "d"(cc_ndep) \
                                   : "cc");\
   psw >> 28;   /* cc */ \
})


#define S390_CC_FOR_BFP_RESULT(opcode,cc_dep1) \
({ \
   union s390x_F64 op = { .i = cc_dep1 }; \
   Double tmp; \
   __asm__ volatile ( \
        opcode " %[tmp],%[op]\n\t" \
        "ipm %[psw]\n\t"           : [psw] "=d"(psw), [tmp] "=f"(tmp) \
                                   : [op]  "f"(op.f) \
                                   : "cc");\
   psw >> 28;   /* cc */ \
})

#define S390_CC_FOR_BFP128_RESULT(hi,lo) \
({ \
   union s390x_F128 op = { .i = { hi, lo } }; \
   Float128 tmp; \
   __asm__ volatile ( \
        "ltxbr %[tmp],%[op]\n\t" \
        "ipm %[psw]\n\t"           : [psw] "=d"(psw), [tmp] "=f"(tmp) \
                                   : [op]  "f"(op.f) \
                                   : "cc");\
   psw >> 28;   /* cc */ \
})

#define S390_CC_FOR_BFP_CONVERT_AUX(opcode,cc_dep1,rounding_mode) \
({ \
   union s390x_F64 op = { .i = cc_dep1 }; \
   ULong tmp; \
   __asm__ volatile ( \
        opcode " %[tmp]," #rounding_mode ",%[op]\n\t" \
        "ipm %[psw]\n\t"           : [psw] "=d"(psw), [tmp] "=d"(tmp) \
                                   : [op]  "f"(op.f) \
                                   : "cc");\
   psw >> 28;   /* cc */ \
})

#define S390_CC_FOR_BFP_CONVERT(opcode,cc_dep1,cc_dep2)   \
({                                                        \
   UInt cc;                                               \
   switch (decode_bfp_rounding_mode(cc_dep2)) {           \
   case S390_BFP_ROUND_NEAREST_EVEN:                      \
      cc = S390_CC_FOR_BFP_CONVERT_AUX(opcode,cc_dep1,4); \
      break;                                              \
   case S390_BFP_ROUND_ZERO:                              \
      cc = S390_CC_FOR_BFP_CONVERT_AUX(opcode,cc_dep1,5); \
      break;                                              \
   case S390_BFP_ROUND_POSINF:                            \
      cc = S390_CC_FOR_BFP_CONVERT_AUX(opcode,cc_dep1,6); \
      break;                                              \
   case S390_BFP_ROUND_NEGINF:                            \
      cc = S390_CC_FOR_BFP_CONVERT_AUX(opcode,cc_dep1,7); \
      break;                                              \
   default:                                               \
      vpanic("unexpected bfp rounding mode");             \
   }                                                      \
   cc;                                                    \
})

#define S390_CC_FOR_BFP_UCONVERT_AUX(opcode,cc_dep1,rounding_mode) \
({ \
   union s390x_F64 op = { .i = cc_dep1 }; \
   ULong tmp; \
   __asm__ volatile ( \
        opcode ",%[tmp],%[op]," #rounding_mode ",0\n\t" \
        "ipm %[psw]\n\t"           : [psw] "=d"(psw), [tmp] "=d"(tmp) \
                                   : [op]  "f"(op.f) \
                                   : "cc");\
   psw >> 28;   /* cc */ \
})

#define S390_CC_FOR_BFP_UCONVERT(opcode,cc_dep1,cc_dep2)   \
({                                                         \
   UInt cc;                                                \
   switch (decode_bfp_rounding_mode(cc_dep2)) {            \
   case S390_BFP_ROUND_NEAREST_EVEN:                       \
      cc = S390_CC_FOR_BFP_UCONVERT_AUX(opcode,cc_dep1,4); \
      break;                                               \
   case S390_BFP_ROUND_ZERO:                               \
      cc = S390_CC_FOR_BFP_UCONVERT_AUX(opcode,cc_dep1,5); \
      break;                                               \
   case S390_BFP_ROUND_POSINF:                             \
      cc = S390_CC_FOR_BFP_UCONVERT_AUX(opcode,cc_dep1,6); \
      break;                                               \
   case S390_BFP_ROUND_NEGINF:                             \
      cc = S390_CC_FOR_BFP_UCONVERT_AUX(opcode,cc_dep1,7); \
      break;                                               \
   default:                                                \
      vpanic("unexpected bfp rounding mode");              \
   }                                                       \
   cc;                                                     \
})

#define S390_CC_FOR_BFP128_CONVERT_AUX(opcode,hi,lo,rounding_mode) \
({ \
   union s390x_F128 op = { .i = { hi, lo } }; \
   ULong tmp; \
   __asm__ volatile ( \
        opcode " %[tmp]," #rounding_mode ",%[op]\n\t" \
        "ipm %[psw]\n\t"           : [psw] "=d"(psw), [tmp] "=d"(tmp) \
                                   : [op] "f"(op.f) \
                                   : "cc");\
   psw >> 28;   /* cc */ \
})

#define S390_CC_FOR_BFP128_CONVERT(opcode,cc_dep1,cc_dep2,cc_ndep)   \
({                                                                   \
   UInt cc;                                                          \
   /* Recover the original DEP2 value. See comment near              \
      s390_cc_thunk_put3 for rationale. */                           \
   cc_dep2 = cc_dep2 ^ cc_ndep;                                      \
   switch (decode_bfp_rounding_mode(cc_ndep)) {                      \
   case S390_BFP_ROUND_NEAREST_EVEN:                                 \
      cc = S390_CC_FOR_BFP128_CONVERT_AUX(opcode,cc_dep1,cc_dep2,4); \
      break;                                                         \
   case S390_BFP_ROUND_ZERO:                                         \
      cc = S390_CC_FOR_BFP128_CONVERT_AUX(opcode,cc_dep1,cc_dep2,5); \
      break;                                                         \
   case S390_BFP_ROUND_POSINF:                                       \
      cc = S390_CC_FOR_BFP128_CONVERT_AUX(opcode,cc_dep1,cc_dep2,6); \
      break;                                                         \
   case S390_BFP_ROUND_NEGINF:                                       \
      cc = S390_CC_FOR_BFP128_CONVERT_AUX(opcode,cc_dep1,cc_dep2,7); \
      break;                                                         \
   default:                                                          \
      vpanic("unexpected bfp rounding mode");                        \
   }                                                                 \
   cc;                                                               \
})

#define S390_CC_FOR_BFP128_UCONVERT_AUX(opcode,hi,lo,rounding_mode) \
({ \
   union s390x_F128 op = { .i = { hi, lo } }; \
   ULong tmp; \
   __asm__ volatile ( \
        opcode ",%[tmp],%[op]," #rounding_mode ",0\n\t" \
        "ipm %[psw]\n\t"           : [psw] "=d"(psw), [tmp] "=d"(tmp) \
                                   : [op] "f"(op.f) \
                                   : "cc");\
   psw >> 28;   /* cc */ \
})

#define S390_CC_FOR_BFP128_UCONVERT(opcode,cc_dep1,cc_dep2,cc_ndep)   \
({                                                                    \
   UInt cc;                                                           \
   /* Recover the original DEP2 value. See comment near               \
      s390_cc_thunk_put3 for rationale. */                            \
   cc_dep2 = cc_dep2 ^ cc_ndep;                                       \
   switch (decode_bfp_rounding_mode(cc_ndep)) {                       \
   case S390_BFP_ROUND_NEAREST_EVEN:                                  \
      cc = S390_CC_FOR_BFP128_UCONVERT_AUX(opcode,cc_dep1,cc_dep2,4); \
      break;                                                          \
   case S390_BFP_ROUND_ZERO:                                          \
      cc = S390_CC_FOR_BFP128_UCONVERT_AUX(opcode,cc_dep1,cc_dep2,5); \
      break;                                                          \
   case S390_BFP_ROUND_POSINF:                                        \
      cc = S390_CC_FOR_BFP128_UCONVERT_AUX(opcode,cc_dep1,cc_dep2,6); \
      break;                                                          \
   case S390_BFP_ROUND_NEGINF:                                        \
      cc = S390_CC_FOR_BFP128_UCONVERT_AUX(opcode,cc_dep1,cc_dep2,7); \
      break;                                                          \
   default:                                                           \
      vpanic("unexpected bfp rounding mode");                         \
   }                                                                  \
   cc;                                                                \
})

#define S390_CC_FOR_BFP_TDC(opcode,cc_dep1,cc_dep2) \
({ \
   union s390x_F64 val = { .i = cc_dep1 }; \
   __asm__ volatile ( \
        opcode " %[value],0(%[class])\n\t" \
        "ipm %[psw]\n\t"           : [psw] "=d"(psw) \
                                   : [value] "f"(val.f), \
                                     [class] "a"(cc_dep2)  \
                                   : "cc");\
   psw >> 28;   /* cc */ \
})

#define S390_CC_FOR_BFP128_TDC(cc_dep1,cc_dep2,cc_ndep) \
({ \
   /* Recover the original DEP2 value. See comment near \
      s390_cc_thunk_put1f128Z for rationale. */ \
   cc_dep2 = cc_dep2 ^ cc_ndep; \
   union s390x_F128 val = { .i = { cc_dep1, cc_dep2 } }; \
   __asm__ volatile ( \
        "tcxb %[value],0(%[class])\n\t" \
        "ipm  %[psw]\n\t"          : [psw] "=d"(psw) \
                                   : [value] "f"(val.f), [class] "a"(cc_ndep) \
                                   : "cc");\
   psw >> 28;   /* cc */ \
})

/* Convert an IRRoundingMode value to s390_dfp_round_t */
static s390_dfp_round_t
decode_dfp_rounding_mode(UInt irrm)
{
   switch (irrm) {
   case Irrm_NEAREST:
      return S390_DFP_ROUND_NEAREST_EVEN_4;
   case Irrm_NegINF:
      return S390_DFP_ROUND_NEGINF_7;
   case Irrm_PosINF:
      return S390_DFP_ROUND_POSINF_6;
   case Irrm_ZERO:
      return S390_DFP_ROUND_ZERO_5;
   case Irrm_NEAREST_TIE_AWAY_0:
      return S390_DFP_ROUND_NEAREST_TIE_AWAY_0_1;
   case Irrm_PREPARE_SHORTER:
      return S390_DFP_ROUND_PREPARE_SHORT_3;
   case Irrm_AWAY_FROM_ZERO:
      return S390_DFP_ROUND_AWAY_0;
   case Irrm_NEAREST_TIE_TOWARD_0:
      return S390_DFP_ROUND_NEAREST_TIE_TOWARD_0;
   }
   vpanic("decode_dfp_rounding_mode");
}

#define S390_CC_FOR_DFP_RESULT(cc_dep1) \
({ \
   union s390x_F64 op = { .i = cc_dep1 }; \
   Double tmp; \
   __asm__ volatile ( \
        ".insn rre, 0xb3d60000,%[tmp],%[op]\n\t"            /* LTDTR */ \
        "ipm %[psw]\n\t"           : [psw] "=d"(psw), [tmp] "=f"(tmp) \
                                   : [op]  "f"(op.f) \
                                   : "cc");\
   psw >> 28;   /* cc */ \
})

#define S390_CC_FOR_DFP128_RESULT(hi,lo) \
({ \
   union s390x_F128 op = { .i = { hi, lo } }; \
   Float128 tmp; \
   __asm__ volatile ( \
        ".insn rre, 0xb3de0000,%[tmp],%[op]\n\t"           /* LTXTR */  \
        "ipm %[psw]\n\t"           : [psw] "=d"(psw), [tmp] "=f"(tmp)   \
                                   : [op] "f"(op.f)                     \
                                   : "cc");                             \
   psw >> 28;   /* cc */                                                \
})

#define S390_CC_FOR_DFP_TD(opcode,cc_dep1,cc_dep2)                      \
({                                                                      \
   union s390x_F64 val = { .i = cc_dep1 };                              \
   __asm__ volatile (                                                   \
        opcode ",%[value],0(%[class])\n\t"                              \
        "ipm %[psw]\n\t"           : [psw] "=d"(psw)                    \
                                   : [value] "f"(val.f),                \
                                     [class] "a"(cc_dep2)               \
                                   : "cc");                             \
   psw >> 28;   /* cc */                                                \
})

#define S390_CC_FOR_DFP128_TD(opcode,cc_dep1,cc_dep2,cc_ndep)           \
({                                                                      \
   /* Recover the original DEP2 value. See comment near                 \
      s390_cc_thunk_put1d128Z for rationale. */                         \
   cc_dep2 = cc_dep2 ^ cc_ndep;                                         \
   union s390x_F128 val = { .i = { cc_dep1, cc_dep2 } };                \
   __asm__ volatile (                                                   \
        opcode ",%[value],0(%[class])\n\t"                              \
        "ipm  %[psw]\n\t"          : [psw] "=d"(psw)                    \
                                   : [value] "f"(val.f),                \
                                     [class] "a"(cc_ndep)               \
                                   : "cc");                             \
   psw >> 28;   /* cc */                                                \
})

#define S390_CC_FOR_DFP_CONVERT_AUX(opcode,cc_dep1,rounding_mode)       \
   ({                                                                   \
      union s390x_F64 op = { .i = cc_dep1 };                            \
      Double tmp;                                                       \
      __asm__ volatile (                                                \
         opcode ",%[tmp],%[op]," #rounding_mode ",0\n\t"                \
         "ipm %[psw]\n\t"          : [psw] "=d"(psw), [tmp] "=f"(tmp)   \
                                   : [op] "f"(op.f)                     \
         : "cc");                                                       \
      psw >> 28;   /* cc */                                             \
   })

#define S390_CC_FOR_DFP_CONVERT(opcode,cc_dep1,cc_dep2)                 \
   ({                                                                   \
      UInt cc;                                                          \
      switch (decode_dfp_rounding_mode(cc_dep2)) {                      \
      case S390_DFP_ROUND_NEAREST_TIE_AWAY_0_1:                         \
      case S390_DFP_ROUND_NEAREST_TIE_AWAY_0_12:                        \
         cc = S390_CC_FOR_DFP_CONVERT_AUX(opcode,cc_dep1,1);            \
         break;                                                         \
      case S390_DFP_ROUND_PREPARE_SHORT_3:                              \
      case S390_DFP_ROUND_PREPARE_SHORT_15:                             \
         cc = S390_CC_FOR_DFP_CONVERT_AUX(opcode,cc_dep1,3);            \
         break;                                                         \
      case S390_DFP_ROUND_NEAREST_EVEN_4:                               \
      case S390_DFP_ROUND_NEAREST_EVEN_8:                               \
         cc = S390_CC_FOR_DFP_CONVERT_AUX(opcode,cc_dep1,4);            \
         break;                                                         \
      case S390_DFP_ROUND_ZERO_5:                                       \
      case S390_DFP_ROUND_ZERO_9:                                       \
         cc = S390_CC_FOR_DFP_CONVERT_AUX(opcode,cc_dep1,5);            \
         break;                                                         \
      case S390_DFP_ROUND_POSINF_6:                                     \
      case S390_DFP_ROUND_POSINF_10:                                    \
         cc = S390_CC_FOR_DFP_CONVERT_AUX(opcode,cc_dep1,6);            \
         break;                                                         \
      case S390_DFP_ROUND_NEGINF_7:                                     \
      case S390_DFP_ROUND_NEGINF_11:                                    \
         cc = S390_CC_FOR_DFP_CONVERT_AUX(opcode,cc_dep1,7);            \
         break;                                                         \
      case S390_DFP_ROUND_NEAREST_TIE_TOWARD_0:                         \
         cc = S390_CC_FOR_DFP_CONVERT_AUX(opcode,cc_dep1,13);           \
         break;                                                         \
      case S390_DFP_ROUND_AWAY_0:                                       \
         cc = S390_CC_FOR_DFP_CONVERT_AUX(opcode,cc_dep1,14);           \
         break;                                                         \
      default:                                                          \
         vpanic("unexpected dfp rounding mode");                        \
      }                                                                 \
      cc;                                                               \
   })

#define S390_CC_FOR_DFP_UCONVERT_AUX(opcode,cc_dep1,rounding_mode)      \
   ({                                                                   \
      union s390x_F64 op = { .i = cc_dep1 };                            \
      Double tmp;                                                       \
      __asm__ volatile (                                                \
         opcode ",%[tmp],%[op]," #rounding_mode ",0\n\t"                \
         "ipm %[psw]\n\t"          : [psw] "=d"(psw), [tmp] "=f"(tmp)   \
                                   : [op] "f"(op.f)                     \
                                   : "cc");                             \
      psw >> 28;   /* cc */                                             \
   })

#define S390_CC_FOR_DFP_UCONVERT(opcode,cc_dep1,cc_dep2)                \
   ({                                                                   \
      UInt cc;                                                          \
      switch (decode_dfp_rounding_mode(cc_dep2)) {                      \
      case S390_DFP_ROUND_NEAREST_TIE_AWAY_0_1:                         \
      case S390_DFP_ROUND_NEAREST_TIE_AWAY_0_12:                        \
         cc = S390_CC_FOR_DFP_UCONVERT_AUX(opcode,cc_dep1,1);           \
         break;                                                         \
      case S390_DFP_ROUND_PREPARE_SHORT_3:                              \
      case S390_DFP_ROUND_PREPARE_SHORT_15:                             \
         cc = S390_CC_FOR_DFP_UCONVERT_AUX(opcode,cc_dep1,3);           \
         break;                                                         \
      case S390_DFP_ROUND_NEAREST_EVEN_4:                               \
      case S390_DFP_ROUND_NEAREST_EVEN_8:                               \
         cc = S390_CC_FOR_DFP_UCONVERT_AUX(opcode,cc_dep1,4);           \
         break;                                                         \
      case S390_DFP_ROUND_ZERO_5:                                       \
      case S390_DFP_ROUND_ZERO_9:                                       \
         cc = S390_CC_FOR_DFP_UCONVERT_AUX(opcode,cc_dep1,5);           \
         break;                                                         \
      case S390_DFP_ROUND_POSINF_6:                                     \
      case S390_DFP_ROUND_POSINF_10:                                    \
         cc = S390_CC_FOR_DFP_UCONVERT_AUX(opcode,cc_dep1,6);           \
         break;                                                         \
      case S390_DFP_ROUND_NEGINF_7:                                     \
      case S390_DFP_ROUND_NEGINF_11:                                    \
         cc = S390_CC_FOR_DFP_UCONVERT_AUX(opcode,cc_dep1,7);           \
         break;                                                         \
      case S390_DFP_ROUND_NEAREST_TIE_TOWARD_0:                         \
         cc = S390_CC_FOR_DFP_UCONVERT_AUX(opcode,cc_dep1,13);          \
         break;                                                         \
      case S390_DFP_ROUND_AWAY_0:                                       \
         cc = S390_CC_FOR_DFP_UCONVERT_AUX(opcode,cc_dep1,14);          \
         break;                                                         \
      default:                                                          \
         vpanic("unexpected dfp rounding mode");                        \
      }                                                                 \
      cc;                                                               \
   })

#define S390_CC_FOR_DFP128_CONVERT_AUX(opcode,hi,lo,rounding_mode)      \
   ({                                                                   \
      union s390x_F128 op = { .i = { hi, lo } };                        \
      Double tmp;                                                       \
      __asm__ volatile (                                                \
         opcode ",%[tmp],%[op]," #rounding_mode ",0\n\t"                \
         "ipm %[psw]\n\t"          : [psw] "=d"(psw), [tmp] "=f"(tmp)   \
                                   : [op] "f"(op.f)                     \
                                   : "cc");                             \
      psw >> 28;   /* cc */                                             \
   })

#define S390_CC_FOR_DFP128_CONVERT(opcode,cc_dep1,cc_dep2,cc_ndep)       \
   ({                                                                    \
      UInt cc;                                                           \
      /* Recover the original DEP2 value. See comment near               \
         s390_cc_thunk_put3 for rationale. */                            \
      cc_dep2 = cc_dep2 ^ cc_ndep;                                       \
      switch (decode_dfp_rounding_mode(cc_ndep)) {                       \
      case S390_DFP_ROUND_NEAREST_TIE_AWAY_0_1:                          \
      case S390_DFP_ROUND_NEAREST_TIE_AWAY_0_12:                         \
         cc = S390_CC_FOR_DFP128_CONVERT_AUX(opcode,cc_dep1,cc_dep2,1);  \
         break;                                                          \
      case S390_DFP_ROUND_PREPARE_SHORT_3:                               \
      case S390_DFP_ROUND_PREPARE_SHORT_15:                              \
         cc = S390_CC_FOR_DFP128_CONVERT_AUX(opcode,cc_dep1,cc_dep2,3);  \
         break;                                                          \
      case S390_DFP_ROUND_NEAREST_EVEN_4:                                \
      case S390_DFP_ROUND_NEAREST_EVEN_8:                                \
         cc = S390_CC_FOR_DFP128_CONVERT_AUX(opcode,cc_dep1,cc_dep2,4);  \
         break;                                                          \
      case S390_DFP_ROUND_ZERO_5:                                        \
      case S390_DFP_ROUND_ZERO_9:                                        \
         cc = S390_CC_FOR_DFP128_CONVERT_AUX(opcode,cc_dep1,cc_dep2,5);  \
         break;                                                          \
      case S390_DFP_ROUND_POSINF_6:                                      \
      case S390_DFP_ROUND_POSINF_10:                                     \
         cc = S390_CC_FOR_DFP128_CONVERT_AUX(opcode,cc_dep1,cc_dep2,6);  \
         break;                                                          \
      case S390_DFP_ROUND_NEGINF_7:                                      \
      case S390_DFP_ROUND_NEGINF_11:                                     \
         cc = S390_CC_FOR_DFP128_CONVERT_AUX(opcode,cc_dep1,cc_dep2,7);  \
         break;                                                          \
      case S390_DFP_ROUND_NEAREST_TIE_TOWARD_0:                          \
         cc = S390_CC_FOR_DFP128_CONVERT_AUX(opcode,cc_dep1,cc_dep2,13); \
         break;                                                          \
      case S390_DFP_ROUND_AWAY_0:                                        \
         cc = S390_CC_FOR_DFP128_CONVERT_AUX(opcode,cc_dep1,cc_dep2,14); \
         break;                                                          \
      default:                                                           \
         vpanic("unexpected dfp rounding mode");                         \
      }                                                                  \
      cc;                                                                \
   })

#define S390_CC_FOR_DFP128_UCONVERT_AUX(opcode,hi,lo,rounding_mode)     \
   ({                                                                   \
      union s390x_F128 op = { .i = { hi, lo } };                        \
      Double tmp;                                                       \
      __asm__ volatile (                                                \
         opcode ",%[tmp],%[op]," #rounding_mode ",0\n\t"                \
         "ipm %[psw]\n\t"          : [psw] "=d"(psw), [tmp] "=f"(tmp)   \
                                   : [op] "f"(op.f)                     \
                                   : "cc");                             \
      psw >> 28;   /* cc */                                             \
   })

#define S390_CC_FOR_DFP128_UCONVERT(opcode,cc_dep1,cc_dep2,cc_ndep)       \
   ({                                                                     \
      UInt cc;                                                            \
      /* Recover the original DEP2 value. See comment near                \
         s390_cc_thunk_put3 for rationale. */                             \
      cc_dep2 = cc_dep2 ^ cc_ndep;                                        \
      switch (decode_dfp_rounding_mode(cc_ndep)) {                        \
      case S390_DFP_ROUND_NEAREST_TIE_AWAY_0_1:                           \
      case S390_DFP_ROUND_NEAREST_TIE_AWAY_0_12:                          \
         cc = S390_CC_FOR_DFP128_UCONVERT_AUX(opcode,cc_dep1,cc_dep2,1);  \
         break;                                                           \
      case S390_DFP_ROUND_PREPARE_SHORT_3:                                \
      case S390_DFP_ROUND_PREPARE_SHORT_15:                               \
         cc = S390_CC_FOR_DFP128_UCONVERT_AUX(opcode,cc_dep1,cc_dep2,3);  \
         break;                                                           \
      case S390_DFP_ROUND_NEAREST_EVEN_4:                                 \
      case S390_DFP_ROUND_NEAREST_EVEN_8:                                 \
         cc = S390_CC_FOR_DFP128_UCONVERT_AUX(opcode,cc_dep1,cc_dep2,4);  \
         break;                                                           \
      case S390_DFP_ROUND_ZERO_5:                                         \
      case S390_DFP_ROUND_ZERO_9:                                         \
         cc = S390_CC_FOR_DFP128_UCONVERT_AUX(opcode,cc_dep1,cc_dep2,5);  \
         break;                                                           \
      case S390_DFP_ROUND_POSINF_6:                                       \
      case S390_DFP_ROUND_POSINF_10:                                      \
         cc = S390_CC_FOR_DFP128_UCONVERT_AUX(opcode,cc_dep1,cc_dep2,6);  \
         break;                                                           \
      case S390_DFP_ROUND_NEGINF_7:                                       \
      case S390_DFP_ROUND_NEGINF_11:                                      \
         cc = S390_CC_FOR_DFP128_UCONVERT_AUX(opcode,cc_dep1,cc_dep2,7);  \
         break;                                                           \
      case S390_DFP_ROUND_NEAREST_TIE_TOWARD_0:                           \
         cc = S390_CC_FOR_DFP128_UCONVERT_AUX(opcode,cc_dep1,cc_dep2,13); \
         break;                                                           \
      case S390_DFP_ROUND_AWAY_0:                                         \
         cc = S390_CC_FOR_DFP128_UCONVERT_AUX(opcode,cc_dep1,cc_dep2,14); \
         break;                                                           \
      default:                                                            \
         vpanic("unexpected dfp rounding mode");                          \
      }                                                                   \
      cc;                                                                 \
   })
#endif /* VGA_s390x */


/* Return the value of the condition code from the supplied thunk parameters.
   This is not the value of the PSW. It is the value of the 2 CC bits within
   the PSW. The returned value is thusly in the interval [0:3]. */
UInt
s390_calculate_cc(ULong cc_op, ULong cc_dep1, ULong cc_dep2, ULong cc_ndep)
{
#if defined(VGA_s390x)
   UInt psw;

   switch (cc_op) {

   case S390_CC_OP_BITWISE:
      return S390_CC_FOR_BINARY("ogr", cc_dep1, (ULong)0);

   case S390_CC_OP_SIGNED_COMPARE:
      return S390_CC_FOR_BINARY("cgr", cc_dep1, cc_dep2);

   case S390_CC_OP_UNSIGNED_COMPARE:
      return S390_CC_FOR_BINARY("clgr", cc_dep1, cc_dep2);

   case S390_CC_OP_SIGNED_ADD_64:
      return S390_CC_FOR_BINARY("agr", cc_dep1, cc_dep2);

   case S390_CC_OP_SIGNED_ADD_32:
      return S390_CC_FOR_BINARY("ar", cc_dep1, cc_dep2);

   case S390_CC_OP_SIGNED_SUB_64:
      return S390_CC_FOR_BINARY("sgr", cc_dep1, cc_dep2);

   case S390_CC_OP_SIGNED_SUB_32:
      return S390_CC_FOR_BINARY("sr", cc_dep1, cc_dep2);

   case S390_CC_OP_UNSIGNED_ADD_64:
      return S390_CC_FOR_BINARY("algr", cc_dep1, cc_dep2);

   case S390_CC_OP_UNSIGNED_ADD_32:
      return S390_CC_FOR_BINARY("alr", cc_dep1, cc_dep2);

   case S390_CC_OP_UNSIGNED_ADDC_64:
      return S390_CC_FOR_TERNARY_ADDC("alcgr", cc_dep1, cc_dep2, cc_ndep);

   case S390_CC_OP_UNSIGNED_ADDC_32:
      return S390_CC_FOR_TERNARY_ADDC("alcr", cc_dep1, cc_dep2, cc_ndep);

   case S390_CC_OP_UNSIGNED_SUB_64:
      return S390_CC_FOR_BINARY("slgr", cc_dep1, cc_dep2);

   case S390_CC_OP_UNSIGNED_SUB_32:
      return S390_CC_FOR_BINARY("slr", cc_dep1, cc_dep2);

   case S390_CC_OP_UNSIGNED_SUBB_64:
      return S390_CC_FOR_TERNARY_SUBB("slbgr", cc_dep1, cc_dep2, cc_ndep);

   case S390_CC_OP_UNSIGNED_SUBB_32:
      return S390_CC_FOR_TERNARY_SUBB("slbr", cc_dep1, cc_dep2, cc_ndep);

   case S390_CC_OP_LOAD_AND_TEST:
      /* Like signed comparison with 0 */
      return S390_CC_FOR_BINARY("cgr", cc_dep1, (Long)0);

   case S390_CC_OP_LOAD_POSITIVE_32:
      __asm__ volatile (
           "lpr  %[result],%[op]\n\t"
           "ipm  %[psw]\n\t"         : [psw] "=d"(psw), [result] "=d"(cc_dep1)
                                     : [op] "d"(cc_dep1)
                                     : "cc");
      return psw >> 28;   /* cc */

   case S390_CC_OP_LOAD_POSITIVE_64:
      __asm__ volatile (
           "lpgr %[result],%[op]\n\t"
           "ipm  %[psw]\n\t"         : [psw] "=d"(psw), [result] "=d"(cc_dep1)
                                     : [op] "d"(cc_dep1)
                                     : "cc");
      return psw >> 28;   /* cc */

   case S390_CC_OP_TEST_UNDER_MASK_8: {
      UChar value  = cc_dep1;
      UChar mask   = cc_dep2;
      ULong pc;

      __asm__ volatile (
           "bras %[pc],1f\n\t"            /* pc = address of next insn */
           "tm %[value],0\n\t"            /* this is skipped, then EXecuted */
           "1: ex %[mask],0(%[pc])\n\t"   /* EXecute TM after modifying mask */
           "ipm %[psw]\n\t"             : [psw] "=d"(psw), [pc] "=&a"(pc)
                                        : [value] "Q"(value), [mask] "a"(mask)
                                        : "cc");
      return psw >> 28;   /* cc */
   }

   case S390_CC_OP_TEST_UNDER_MASK_16: {
      /* Create a TMLL insn with the mask as given by cc_dep2 */
      UInt insn  = (0xA701u << 16) | cc_dep2;
      UInt value = cc_dep1;

      __asm__ volatile (
           "lr   1,%[value]\n\t"
           "lhi  2,0x10\n\t"
           "ex   2,%[insn]\n\t"
           "ipm  %[psw]\n\t"       : [psw] "=d"(psw)
                                   : [value] "d"(value), [insn] "R"(insn)
                                   : "r1", "r2", "cc");
      return psw >> 28;   /* cc */
   }

   case S390_CC_OP_SHIFT_LEFT_32:
      __asm__ volatile (
           "sla  %[op],0(%[amount])\n\t"
           "ipm  %[psw]\n\t"            : [psw] "=d"(psw), [op] "+d"(cc_dep1)
                                        : [amount] "a"(cc_dep2)
                                        : "cc");
      return psw >> 28;   /* cc */

   case S390_CC_OP_SHIFT_LEFT_64: {
      Int high = (Int)(cc_dep1 >> 32);
      Int low  = (Int)(cc_dep1 & 0xFFFFFFFF);

      __asm__ volatile (
           "lr   2,%[high]\n\t"
           "lr   3,%[low]\n\t"
           "slda 2,0(%[amount])\n\t"
           "ipm %[psw]\n\t"             : [psw] "=d"(psw), [high] "+d"(high),
                                          [low] "+d"(low)
                                        : [amount] "a"(cc_dep2)
                                        : "cc", "r2", "r3");
      return psw >> 28;   /* cc */
   }

   case S390_CC_OP_INSERT_CHAR_MASK_32: {
      Int inserted = 0;
      Int msb = 0;

      if (cc_dep2 & 1) {
         inserted |= cc_dep1 & 0xff;
         msb = 0x80;
      }
      if (cc_dep2 & 2) {
         inserted |= cc_dep1 & 0xff00;
         msb = 0x8000;
      }
      if (cc_dep2 & 4) {
         inserted |= cc_dep1 & 0xff0000;
         msb = 0x800000;
      }
      if (cc_dep2 & 8) {
         inserted |= cc_dep1 & 0xff000000;
         msb = 0x80000000;
      }

      if (inserted & msb)  // MSB is 1
         return 1;
      if (inserted > 0)
         return 2;
      return 0;
   }

   case S390_CC_OP_BFP_RESULT_32:
      return S390_CC_FOR_BFP_RESULT("ltebr", cc_dep1);

   case S390_CC_OP_BFP_RESULT_64:
      return S390_CC_FOR_BFP_RESULT("ltdbr", cc_dep1);

   case S390_CC_OP_BFP_RESULT_128:
      return S390_CC_FOR_BFP128_RESULT(cc_dep1, cc_dep2);

   case S390_CC_OP_BFP_32_TO_INT_32:
      return S390_CC_FOR_BFP_CONVERT("cfebr", cc_dep1, cc_dep2);

   case S390_CC_OP_BFP_64_TO_INT_32:
      return S390_CC_FOR_BFP_CONVERT("cfdbr", cc_dep1, cc_dep2);

   case S390_CC_OP_BFP_128_TO_INT_32:
      return S390_CC_FOR_BFP128_CONVERT("cfxbr", cc_dep1, cc_dep2, cc_ndep);

   case S390_CC_OP_BFP_32_TO_INT_64:
      return S390_CC_FOR_BFP_CONVERT("cgebr", cc_dep1, cc_dep2);

   case S390_CC_OP_BFP_64_TO_INT_64:
      return S390_CC_FOR_BFP_CONVERT("cgdbr", cc_dep1, cc_dep2);

   case S390_CC_OP_BFP_128_TO_INT_64:
      return S390_CC_FOR_BFP128_CONVERT("cgxbr", cc_dep1, cc_dep2, cc_ndep);

   case S390_CC_OP_BFP_TDC_32:
      return S390_CC_FOR_BFP_TDC("tceb", cc_dep1, cc_dep2);

   case S390_CC_OP_BFP_TDC_64:
      return S390_CC_FOR_BFP_TDC("tcdb", cc_dep1, cc_dep2);

   case S390_CC_OP_BFP_TDC_128:
      return S390_CC_FOR_BFP128_TDC(cc_dep1, cc_dep2, cc_ndep);

   case S390_CC_OP_SET:
      return cc_dep1;

   case S390_CC_OP_BFP_32_TO_UINT_32:
      return S390_CC_FOR_BFP_UCONVERT(".insn rrf,0xb39c0000", cc_dep1, cc_dep2);

   case S390_CC_OP_BFP_64_TO_UINT_32:
      return S390_CC_FOR_BFP_UCONVERT(".insn rrf,0xb39d0000", cc_dep1, cc_dep2);

   case S390_CC_OP_BFP_128_TO_UINT_32:
      return S390_CC_FOR_BFP128_UCONVERT(".insn rrf,0xb39e0000", cc_dep1,
                                         cc_dep2, cc_ndep);

   case S390_CC_OP_BFP_32_TO_UINT_64:
      return S390_CC_FOR_BFP_UCONVERT(".insn rrf,0xb3ac0000", cc_dep1, cc_dep2);

   case S390_CC_OP_BFP_64_TO_UINT_64:
      return S390_CC_FOR_BFP_UCONVERT(".insn rrf,0xb3ad0000", cc_dep1, cc_dep2);

   case S390_CC_OP_BFP_128_TO_UINT_64:
      return S390_CC_FOR_BFP128_UCONVERT(".insn rrf,0xb3ae0000", cc_dep1,
                                         cc_dep2, cc_ndep);

   case S390_CC_OP_DFP_RESULT_64:
      return S390_CC_FOR_DFP_RESULT(cc_dep1);

   case S390_CC_OP_DFP_RESULT_128:
      return S390_CC_FOR_DFP128_RESULT(cc_dep1, cc_dep2);

   case S390_CC_OP_DFP_TDC_32:  /* TDCET */
      return S390_CC_FOR_DFP_TD(".insn rxe, 0xed0000000050", cc_dep1, cc_dep2);

   case S390_CC_OP_DFP_TDC_64:  /* TDCDT */
      return S390_CC_FOR_DFP_TD(".insn rxe, 0xed0000000054", cc_dep1, cc_dep2);

   case S390_CC_OP_DFP_TDC_128: /* TDCXT */
      return S390_CC_FOR_DFP128_TD(".insn rxe, 0xed0000000058", cc_dep1,
                                   cc_dep2, cc_ndep);

   case S390_CC_OP_DFP_TDG_32:  /* TDGET */
      return S390_CC_FOR_DFP_TD(".insn rxe, 0xed0000000051", cc_dep1, cc_dep2);

   case S390_CC_OP_DFP_TDG_64:  /* TDGDT */
      return S390_CC_FOR_DFP_TD(".insn rxe, 0xed0000000055", cc_dep1, cc_dep2);

   case S390_CC_OP_DFP_TDG_128: /* TDGXT */
      return S390_CC_FOR_DFP128_TD(".insn rxe, 0xed0000000059", cc_dep1,
                                   cc_dep2, cc_ndep);

   case S390_CC_OP_DFP_64_TO_INT_32: /* CFDTR */
      return S390_CC_FOR_DFP_CONVERT(".insn rrf,0xb9410000", cc_dep1, cc_dep2);

   case S390_CC_OP_DFP_128_TO_INT_32: /* CFXTR */
      return S390_CC_FOR_DFP128_CONVERT(".insn rrf,0xb9490000", cc_dep1,
                                        cc_dep2, cc_ndep);

   case S390_CC_OP_DFP_64_TO_INT_64: /* CGDTR */
      return S390_CC_FOR_DFP_CONVERT(".insn rrf,0xb3e10000", cc_dep1, cc_dep2);

   case S390_CC_OP_DFP_128_TO_INT_64: /* CGXTR */
      return S390_CC_FOR_DFP128_CONVERT(".insn rrf,0xb3e90000", cc_dep1,
                                        cc_dep2, cc_ndep);

   case S390_CC_OP_DFP_64_TO_UINT_32: /* CLFDTR */
      return S390_CC_FOR_DFP_UCONVERT(".insn rrf,0xb9430000", cc_dep1, cc_dep2);

   case S390_CC_OP_DFP_128_TO_UINT_32: /* CLFXTR */
      return S390_CC_FOR_DFP128_UCONVERT(".insn rrf,0xb94b0000", cc_dep1,
                                         cc_dep2, cc_ndep);

   case S390_CC_OP_DFP_64_TO_UINT_64: /* CLGDTR */
      return S390_CC_FOR_DFP_UCONVERT(".insn rrf,0xb9420000", cc_dep1, cc_dep2);

   case S390_CC_OP_DFP_128_TO_UINT_64: /* CLGXTR */
      return S390_CC_FOR_DFP128_UCONVERT(".insn rrf,0xb94a0000", cc_dep1,
                                         cc_dep2, cc_ndep);

   case S390_CC_OP_PFPO_32: {
      __asm__ volatile(
           "ldgr 4, %[cc_dep1]\n\t"     /* Load FR from GR */
           "lr   0, %[cc_dep2]\n\t"     /* 32 bit GR move */
           ".insn e,0x010a\n\t"         /* PFPO */
           "ipm  %[psw]\n\t"            : [psw] "=d"(psw)
                                        : [cc_dep1] "d"(cc_dep1),
                                          [cc_dep2] "d"(cc_dep2)
                                        : "r0", "r1", "f4");
      return psw >> 28;  /* cc */
   }

   case S390_CC_OP_PFPO_64: {
      __asm__ volatile(
           "ldgr 4, %[cc_dep1]\n\t"
           "lr   0, %[cc_dep2]\n\t"     /* 32 bit register move */
           ".insn e,0x010a\n\t"         /* PFPO */
           "ipm  %[psw]\n\t"            : [psw] "=d"(psw)
                                        : [cc_dep1] "d"(cc_dep1),
                                          [cc_dep2] "d"(cc_dep2)
                                        : "r0", "r1", "f4");
      return psw >> 28;  /* cc */
   }

   case S390_CC_OP_PFPO_128: {
      __asm__ volatile(
           "ldgr 4,%[cc_dep1]\n\t"
           "ldgr 6,%[cc_dep2]\n\t"
           "lr   0,%[cc_ndep]\n\t"      /* 32 bit register move */
           ".insn e,0x010a\n\t"         /* PFPO */
           "ipm  %[psw]\n\t"             : [psw] "=d"(psw)
                                        : [cc_dep1] "d"(cc_dep1),
                                          [cc_dep2] "d"(cc_dep2),
                                          [cc_ndep] "d"(cc_ndep)
                                        : "r0", "r1", "f0", "f2", "f4", "f6");
      return psw >> 28;  /* cc */
   }

   case S390_CC_OP_MUL_32:
      return S390_CC_FOR_TERNARY(".insn rrf,0xb9fd0000", cc_dep1, cc_dep2);

   case S390_CC_OP_MUL_64:
      return S390_CC_FOR_TERNARY(".insn rrf,0xb9ed0000", cc_dep1, cc_dep2);

   default:
      break;
   }
#endif
   vpanic("s390_calculate_cc");
}


/* Note that this does *not* return a Boolean value. The result needs to be
   explicitly tested against zero. */
UInt
s390_calculate_cond(ULong mask, ULong op, ULong dep1, ULong dep2, ULong ndep)
{
   UInt cc = s390_calculate_cc(op, dep1, dep2, ndep);

   return ((mask << cc) & 0x8);
}

/*------------------------------------------------------------*/
/*--- spechelper for performance                           ---*/
/*------------------------------------------------------------*/


/* Convenience macros */
#define unop(op,a1) IRExpr_Unop((op),(a1))
#define binop(op,a1,a2) IRExpr_Binop((op),(a1),(a2))
#define mkU64(v) IRExpr_Const(IRConst_U64(v))
#define mkU32(v) IRExpr_Const(IRConst_U32(v))
#define mkU8(v)  IRExpr_Const(IRConst_U8(v))


static inline Bool
isC64(const IRExpr *expr)
{
   return expr->tag == Iex_Const && expr->Iex.Const.con->tag == Ico_U64;
}

static inline Bool
isC64_exactly(const IRExpr *expr, ULong n)
{
   return expr->tag == Iex_Const && expr->Iex.Const.con->tag == Ico_U64
          && expr->Iex.Const.con->Ico.U64 == n;
}


/* The returned expression is NULL if no specialization was found. In that
   case the helper function will be called. Otherwise, the expression has
   type Ity_I32 and a Boolean value. */
IRExpr *
guest_s390x_spechelper(const HChar *function_name, IRExpr **args,
                       IRStmt **precedingStmts, Int n_precedingStmts)
{
   UInt i, arity = 0;

   for (i = 0; args[i]; i++)
      arity++;

#  if 0
   vex_printf("spec request:\n");
   vex_printf("   %s  ", function_name);
   for (i = 0; i < arity; i++) {
      vex_printf("  ");
      ppIRExpr(args[i]);
   }
   vex_printf("\n");
#  endif

   /* --------- Specialising "s390_calculate_cond" --------- */

   if (vex_streq(function_name, "s390_calculate_cond")) {
      IRExpr *cond_expr, *cc_op_expr, *cc_dep1, *cc_dep2;
      ULong cond, cc_op;

      vassert(arity == 5);

      cond_expr  = args[0];
      cc_op_expr = args[1];

      /* The necessary requirement for all optimizations here is that the
         condition and the cc_op are constant. So check that upfront. */
      if (! isC64(cond_expr))  return NULL;
      if (! isC64(cc_op_expr)) return NULL;

      cond    = cond_expr->Iex.Const.con->Ico.U64;
      cc_op   = cc_op_expr->Iex.Const.con->Ico.U64;

      vassert(cond <= 15);

      /*
        +------+---+---+---+---+
        | cc   | 0 | 1 | 2 | 3 |
        | cond | 8 | 4 | 2 | 1 |
        +------+---+---+---+---+
      */
      cc_dep1 = args[2];
      cc_dep2 = args[3];

      /* S390_CC_OP_SIGNED_COMPARE */
      if (cc_op == S390_CC_OP_SIGNED_COMPARE) {
         /*
            cc == 0  --> cc_dep1 == cc_dep2   (cond == 8)
            cc == 1  --> cc_dep1 <  cc_dep2   (cond == 4)
            cc == 2  --> cc_dep1 >  cc_dep2   (cond == 2)

            Because cc == 3 cannot occur the rightmost bit of cond is
            a don't care.
         */
         if (cond == 8 || cond == 8 + 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpEQ64, cc_dep1, cc_dep2));
         }
         if (cond == 4 + 2 || cond == 4 + 2 + 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpNE64, cc_dep1, cc_dep2));
         }
         if (cond == 4 || cond == 4 + 1) {
            if (isC64_exactly(cc_dep2, 0)) {
               /*     dep1 <signed 0
                  --> m.s.bit of dep1 == 1 */
               return unop(Iop_64to32,
                           binop(Iop_And64,
                                 binop(Iop_Shr64, cc_dep1, mkU8(63)),
                                 mkU64(1)));
            }
            return unop(Iop_1Uto32, binop(Iop_CmpLT64S, cc_dep1, cc_dep2));
         }
         if (cond == 8 + 4 || cond == 8 + 4 + 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpLE64S, cc_dep1, cc_dep2));
         }
         /* cc_dep1 > cc_dep2  ---->  cc_dep2 < cc_dep1 */
         if (cond == 2 || cond == 2 + 1) {
            /* If we ever need the counterpart of the bug387712 fix just
               below, then here is the place.  We'll need to give an
               alternative expression for the case "cc_dep2 <s 0".  From a
               bit of simple testing, I've yet to see any such cases,
               however. */
            return unop(Iop_1Uto32, binop(Iop_CmpLT64S, cc_dep2, cc_dep1));
         }
         if (cond == 8 + 2 || cond == 8 + 2 + 1) {
            if (isC64_exactly(cc_dep2, 0)) {
               /*     0    <=signed dep1
                  --> dep1 >=signed 0
                  --> m.s.bit of dep1 == 0 */
               /* See bug 387712.  This is an old trick from gcc to extract
                  the most significant bit of a word. */
               return unop(Iop_64to32,
                           binop(Iop_Xor64,
                                 binop(Iop_Shr64, cc_dep1, mkU8(63)),
                                 mkU64(1)));
            }
            return unop(Iop_1Uto32, binop(Iop_CmpLE64S, cc_dep2, cc_dep1));
         }
         if (cond == 8 + 4 + 2 || cond == 8 + 4 + 2 + 1) {
            return mkU32(1);
         }
         /* Remaining case */
         return mkU32(0);
      }

      /* S390_CC_OP_UNSIGNED_COMPARE */
      if (cc_op == S390_CC_OP_UNSIGNED_COMPARE) {
         /*
            cc == 0  --> cc_dep1 == cc_dep2   (cond == 8)
            cc == 1  --> cc_dep1 <  cc_dep2   (cond == 4)
            cc == 2  --> cc_dep1 >  cc_dep2   (cond == 2)

            Because cc == 3 cannot occur the rightmost bit of cond is
            a don't care.
         */
         if (isC64(cc_dep2)) {
            /* Avoid memcheck false positives for comparisons like `<= 0x1f',
               `> 0x1f', `< 0x20', or `>= 0x20', where the lower bits don't
               matter.  Some compiler optimizations yield such comparisons when
               testing if any (or none) of the upper bits are set. */

            ULong mask = cc_dep2->Iex.Const.con->Ico.U64;
            ULong c    = cond & (8 + 4 + 2);

            if ((mask & (mask - 1)) == 0) {
               /* Transform `<  0x20' to `<= 0x1f' and
                            `>= 0x20' to `>  0x1f' */
               mask -= 1;
               c ^= 8;
            }
            if (mask != 0 && (mask + 1) != 0 && (mask & (mask + 1)) == 0 &&
                (c == 8 + 4 || c == 2)) {
               IROp cmp = c == 8 + 4 ? Iop_CmpEQ64 : Iop_CmpNE64;
               return unop(Iop_1Uto32,
                           binop(cmp, binop(Iop_And64, cc_dep1, mkU64(~mask)),
                                 mkU64(0)));
            }
         }
         if (cond == 8 || cond == 8 + 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpEQ64, cc_dep1, cc_dep2));
         }
         if (cond == 4 + 2 || cond == 4 + 2 + 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpNE64, cc_dep1, cc_dep2));
         }
         if (cond == 4 || cond == 4 + 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpLT64U, cc_dep1, cc_dep2));
         }
         if (cond == 8 + 4 || cond == 8 + 4 + 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpLE64U, cc_dep1, cc_dep2));
         }
         /* cc_dep1 > cc_dep2  ---->  cc_dep2 < cc_dep1 */
         if (cond == 2 || cond == 2 + 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpLT64U, cc_dep2, cc_dep1));
         }
         if (cond == 8 + 2 || cond == 8 + 2 + 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpLE64U, cc_dep2, cc_dep1));
         }
         if (cond == 8 + 4 + 2 || cond == 8 + 4 + 2 + 1) {
            return mkU32(1);
         }
         /* Remaining case */
         return mkU32(0);
      }

      /* S390_CC_OP_LOAD_AND_TEST */
      if (cc_op == S390_CC_OP_LOAD_AND_TEST) {
         /*
            cc == 0  --> cc_dep1 == 0   (cond == 8)
            cc == 1  --> cc_dep1 <  0   (cond == 4)
            cc == 2  --> cc_dep1 >  0   (cond == 2)

            Because cc == 3 cannot occur the rightmost bit of cond is
            a don't care.
         */
         if (cond == 8 || cond == 8 + 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpEQ64, cc_dep1, mkU64(0)));
         }
         if (cond == 4 + 2 || cond == 4 + 2 + 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpNE64, cc_dep1, mkU64(0)));
         }
         if (cond == 4 || cond == 4 + 1) {
             /* Special case cc_dep < 0. Only check the MSB to avoid bogus
               memcheck complaints due to gcc magic. Fixes 343802
             */
            return unop(Iop_64to32, binop(Iop_Shr64, cc_dep1, mkU8(63)));
         }
         if (cond == 8 + 4 || cond == 8 + 4 + 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpLE64S, cc_dep1, mkU64(0)));
         }
         /* cc_dep1 > 0  ---->  0 < cc_dep1 */
         if (cond == 2 || cond == 2 + 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpLT64S, mkU64(0), cc_dep1));
         }
         if (cond == 8 + 2 || cond == 8 + 2 + 1) {
            /* Special case cc_dep >= 0. Only check the MSB to avoid bogus
               memcheck complaints due to gcc magic. Fixes 308427
             */
            return unop(Iop_64to32, binop(Iop_Xor64,
                                          binop(Iop_Shr64, cc_dep1, mkU8(63)),
                                          mkU64(1)));
         }
         if (cond == 8 + 4 + 2 || cond == 8 + 4 + 2 + 1) {
            return mkU32(1);
         }
         /* Remaining case */
         return mkU32(0);
      }

      /* S390_CC_OP_BITWISE */
      if (cc_op == S390_CC_OP_BITWISE) {
         /*
            cc_dep1 is the result of the boolean operation.

            cc == 0  --> cc_dep1 == 0   (cond == 8)
            cc == 1  --> cc_dep1 != 0   (cond == 4)

            Because cc == 2 and cc == 3 cannot occur the two rightmost bits of
            cond are don't cares. Therefore:

            cond == 00xx  -> always false
            cond == 01xx  -> not equal
            cond == 10xx  -> equal
            cond == 11xx  -> always true
         */
         if ((cond & (8 + 4)) == 8 + 4) {
            return mkU32(1);
         }
         if (cond & 8) {
            return unop(Iop_1Uto32, binop(Iop_CmpEQ64, cc_dep1, mkU64(0)));
         }
         if (cond & 4) {
            return unop(Iop_1Uto32, binop(Iop_CmpNE64, cc_dep1, mkU64(0)));
         }
         /* Remaining case */
         return mkU32(0);
      }

      /* S390_CC_OP_INSERT_CHAR_MASK_32
         Since the mask comes from an immediate field in the opcode, we
         expect the mask to be a constant here. That simplifies matters. */
      if (cc_op == S390_CC_OP_INSERT_CHAR_MASK_32) {
         ULong mask;
         UInt imask = 0, shift = 0;
         IRExpr *word;

         if (! isC64(cc_dep2)) goto missed;

         mask = cc_dep2->Iex.Const.con->Ico.U64;

         /* Extract the 32-bit value from the thunk */

         word = unop(Iop_64to32, cc_dep1);

         switch (mask) {
         case 0:  shift =  0; imask = 0x00000000; break;
         case 1:  shift = 24; imask = 0x000000FF; break;
         case 2:  shift = 16; imask = 0x0000FF00; break;
         case 3:  shift = 16; imask = 0x0000FFFF; break;
         case 4:  shift =  8; imask = 0x00FF0000; break;
         case 5:  shift =  8; imask = 0x00FF00FF; break;
         case 6:  shift =  8; imask = 0x00FFFF00; break;
         case 7:  shift =  8; imask = 0x00FFFFFF; break;
         case 8:  shift =  0; imask = 0xFF000000; break;
         case 9:  shift =  0; imask = 0xFF0000FF; break;
         case 10: shift =  0; imask = 0xFF00FF00; break;
         case 11: shift =  0; imask = 0xFF00FFFF; break;
         case 12: shift =  0; imask = 0xFFFF0000; break;
         case 13: shift =  0; imask = 0xFFFF00FF; break;
         case 14: shift =  0; imask = 0xFFFFFF00; break;
         case 15: shift =  0; imask = 0xFFFFFFFF; break;
         }

         /* Select the bits that were inserted */
         word = binop(Iop_And32, word, mkU32(imask));

         /* cc == 0  --> all inserted bits zero or mask == 0   (cond == 8)
            cc == 1  --> leftmost inserted bit is one          (cond == 4)
            cc == 2  --> leftmost inserted bit is zero and not (cond == 2)
                         all inserted bits are zero

            Because cc == 0,1,2 the rightmost bit of the mask is a don't care */
         if (cond == 8 || cond == 8 + 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpEQ32, word, mkU32(0)));
         }
         if (cond == 4 + 2 || cond == 4 + 2 + 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpNE32, word, mkU32(0)));
         }

         /* Sign extend */
         if (shift != 0) {
            word = binop(Iop_Sar32, binop(Iop_Shl32, word, mkU8(shift)),
                         mkU8(shift));
         }

         if (cond == 4 || cond == 4 + 1) {  /* word < 0 */
            return unop(Iop_1Uto32, binop(Iop_CmpLT32S, word, mkU32(0)));
         }
         if (cond == 2 || cond == 2 + 1) {  /* word > 0 */
            return unop(Iop_1Uto32, binop(Iop_CmpLT32S, mkU32(0), word));
         }
         if (cond == 8 + 4 || cond == 8 + 4 + 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpLE32S, word, mkU32(0)));
         }
         if (cond == 8 + 2 || cond == 8 + 2 + 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpLE32S, mkU32(0), word));
         }
         if (cond == 8 + 4 + 2 || cond == 8 + 4 + 2 + 1) {
            return mkU32(1);
         }
         /* Remaining case */
         return mkU32(0);
      }

      /* S390_CC_OP_TEST_UNDER_MASK_8
         Since the mask comes from an immediate field in the opcode, we
         expect the mask to be a constant here. That simplifies matters. */
      if (cc_op == S390_CC_OP_TEST_UNDER_MASK_8) {
         ULong mask16;

         if (! isC64(cc_dep2)) goto missed;

         mask16 = cc_dep2->Iex.Const.con->Ico.U64;

         /* Get rid of the mask16 == 0 case first. Some of the simplifications
            below (e.g. for OVFL) only hold if mask16 == 0.  */
         if (mask16 == 0) {   /* cc == 0 */
            if (cond & 0x8) return mkU32(1);
            return mkU32(0);
         }

         /* cc == 2 is a don't care */
         if (cond == 8 || cond == 8 + 2) {
            return unop(Iop_1Uto32, binop(Iop_CmpEQ64,
                                          binop(Iop_And64, cc_dep1, cc_dep2),
                                          mkU64(0)));
         }
         if (cond == 7 || cond == 7 - 2) {
            return unop(Iop_1Uto32, binop(Iop_CmpNE64,
                                          binop(Iop_And64, cc_dep1, cc_dep2),
                                          mkU64(0)));
         }
         if (cond == 1 || cond == 1 + 2) {
            return unop(Iop_1Uto32, binop(Iop_CmpEQ64,
                                          binop(Iop_And64, cc_dep1, cc_dep2),
                                          cc_dep2));
         }
         if (cond == 14 || cond == 14 - 2) {  /* ! OVFL */
            return unop(Iop_1Uto32, binop(Iop_CmpNE64,
                                          binop(Iop_And64, cc_dep1, cc_dep2),
                                          cc_dep2));
         }
         goto missed;
      }

      /* S390_CC_OP_TEST_UNDER_MASK_16
         Since the mask comes from an immediate field in the opcode, we
         expect the mask to be a constant here. That simplifies matters. */
      if (cc_op == S390_CC_OP_TEST_UNDER_MASK_16) {
         ULong mask16;
         UInt msb;

         if (! isC64(cc_dep2)) goto missed;

         mask16 = cc_dep2->Iex.Const.con->Ico.U64;

         /* Get rid of the mask16 == 0 case first. Some of the simplifications
            below (e.g. for OVFL) only hold if mask16 == 0.  */
         if (mask16 == 0) {   /* cc == 0 */
            if (cond & 0x8) return mkU32(1);
            return mkU32(0);
         }

         if (cond == 15) return mkU32(1);

         if (cond == 8) {
            return unop(Iop_1Uto32, binop(Iop_CmpEQ64,
                                          binop(Iop_And64, cc_dep1, cc_dep2),
                                          mkU64(0)));
         }
         if (cond == 7) {
            return unop(Iop_1Uto32, binop(Iop_CmpNE64,
                                          binop(Iop_And64, cc_dep1, cc_dep2),
                                          mkU64(0)));
         }
         if (cond == 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpEQ64,
                                          binop(Iop_And64, cc_dep1, cc_dep2),
                                          mkU64(mask16)));
         }
         if (cond == 14) {  /* ! OVFL */
            return unop(Iop_1Uto32, binop(Iop_CmpNE64,
                                          binop(Iop_And64, cc_dep1, cc_dep2),
                                          mkU64(mask16)));
         }

         /* Find MSB in mask */
         msb = 0x8000;
         while (msb > mask16)
            msb >>= 1;

         if (cond == 2) {  /* cc == 2 */
            IRExpr *c1, *c2;

            /* (cc_dep & msb) != 0 && (cc_dep & mask16) != mask16 */
            c1 = binop(Iop_CmpNE64,
                       binop(Iop_And64, cc_dep1, mkU64(msb)), mkU64(0));
            c2 = binop(Iop_CmpNE64,
                       binop(Iop_And64, cc_dep1, cc_dep2),
                       mkU64(mask16));
            return binop(Iop_And32, unop(Iop_1Uto32, c1),
                         unop(Iop_1Uto32, c2));
         }

         if (cond == 4) {  /* cc == 1 */
            IRExpr *c1, *c2;

            /* (cc_dep & msb) == 0 && (cc_dep & mask16) != 0 */
            c1 = binop(Iop_CmpEQ64,
                       binop(Iop_And64, cc_dep1, mkU64(msb)), mkU64(0));
            c2 = binop(Iop_CmpNE64,
                       binop(Iop_And64, cc_dep1, cc_dep2),
                       mkU64(0));
            return binop(Iop_And32, unop(Iop_1Uto32, c1),
                         unop(Iop_1Uto32, c2));
         }

         if (cond == 11) {  /* cc == 0,2,3 */
            IRExpr *c1, *c2;

            c1 = binop(Iop_CmpNE64,
                       binop(Iop_And64, cc_dep1, mkU64(msb)), mkU64(0));
            c2 = binop(Iop_CmpEQ64,
                       binop(Iop_And64, cc_dep1, cc_dep2),
                       mkU64(0));
            return binop(Iop_Or32, unop(Iop_1Uto32, c1),
                         unop(Iop_1Uto32, c2));
         }

         if (cond == 3) {  /* cc == 2 || cc == 3 */
            return unop(Iop_1Uto32,
                        binop(Iop_CmpNE64,
                              binop(Iop_And64, cc_dep1, mkU64(msb)),
                              mkU64(0)));
         }
         if (cond == 12) { /* cc == 0 || cc == 1 */
            return unop(Iop_1Uto32,
                        binop(Iop_CmpEQ64,
                              binop(Iop_And64, cc_dep1, mkU64(msb)),
                              mkU64(0)));
         }
         if (cond == 13) { /* cc == 0 || cc == 1 || cc == 3 */
            IRExpr *c01, *c3;

            c01 = binop(Iop_CmpEQ64, binop(Iop_And64, cc_dep1, mkU64(msb)),
                        mkU64(0));
            c3 = binop(Iop_CmpEQ64, binop(Iop_And64, cc_dep1, cc_dep2),
                       mkU64(mask16));
            return binop(Iop_Or32, unop(Iop_1Uto32, c01),
                         unop(Iop_1Uto32, c3));
         }
         // fixs390: handle cond = 5,6,9,10 (the missing cases)
         // vex_printf("TUM mask = 0x%llx\n", mask16);
         goto missed;
      }

      /* S390_CC_OP_UNSIGNED_SUB_64/32 */
      if (cc_op == S390_CC_OP_UNSIGNED_SUB_64 ||
          cc_op == S390_CC_OP_UNSIGNED_SUB_32) {
         /*
            cc_dep1, cc_dep2 are the zero extended left and right operands

            cc == 1  --> result != 0, borrow    (cond == 4)
            cc == 2  --> result == 0, no borrow (cond == 2)
            cc == 3  --> result != 0, no borrow (cond == 1)

            cc = (cc_dep1 == cc_dep2) ? 2
                                      : (cc_dep1 > cc_dep2) ? 3 : 1;

            Because cc == 0 cannot occur the leftmost bit of cond is
            a don't care.
         */
         if (cond == 1 || cond == 1 + 8) {  /* cc == 3   op2 < op1 */
            return unop(Iop_1Uto32, binop(Iop_CmpLT64U, cc_dep2, cc_dep1));
         }
         if (cond == 2 || cond == 2 + 8) {  /* cc == 2 */
            return unop(Iop_1Uto32, binop(Iop_CmpEQ64, cc_dep1, cc_dep2));
         }
         if (cond == 4 || cond == 4 + 8) {  /* cc == 1 */
            return unop(Iop_1Uto32, binop(Iop_CmpLT64U, cc_dep1, cc_dep2));
         }
         if (cond == 3 || cond == 3 + 8) {  /* cc == 2 || cc == 3 */
            return unop(Iop_1Uto32, binop(Iop_CmpLE64U, cc_dep2, cc_dep1));
         }
         if (cond == 6 || cond == 6 + 8) {  /* cc == 2 || cc == 1 */
            return unop(Iop_1Uto32, binop(Iop_CmpLE64U, cc_dep1, cc_dep2));
         }

         if (cond == 5 || cond == 5 + 8) {  /* cc == 3 || cc == 1 */
            return unop(Iop_1Uto32, binop(Iop_CmpNE64, cc_dep1, cc_dep2));
         }
         if (cond == 7 || cond == 7 + 8) {
            return mkU32(1);
         }
         /* Remaining case */
         return mkU32(0);
      }

      /* S390_CC_OP_UNSIGNED_ADD_64 */
      if (cc_op == S390_CC_OP_UNSIGNED_ADD_64) {
         /*
            cc_dep1, cc_dep2 are the zero extended left and right operands

            cc == 0  --> result == 0, no carry  (cond == 8)
            cc == 1  --> result != 0, no carry  (cond == 4)
            cc == 2  --> result == 0, carry     (cond == 2)
            cc == 3  --> result != 0, carry     (cond == 1)
         */
         if (cond == 8) { /* cc == 0 */
            /* Both inputs are 0 */
            return unop(Iop_1Uto32, binop(Iop_CmpEQ64,
                                          binop(Iop_Or64, cc_dep1, cc_dep2),
                                          mkU64(0)));
         }
         if (cond == 7) { /* cc == 1,2,3 */
            /* Not both inputs are 0 */
            return unop(Iop_1Uto32, binop(Iop_CmpNE64,
                                          binop(Iop_Or64, cc_dep1, cc_dep2),
                                          mkU64(0)));
         }
         if (cond == 8 + 2) {  /* cc == 0,2  -> result is zero */
            return unop(Iop_1Uto32, binop(Iop_CmpEQ64,
                                          binop(Iop_Add64, cc_dep1, cc_dep2),
                                          mkU64(0)));
         }
         if (cond == 4 + 1) {  /* cc == 1,3  -> result is not zero */
            return unop(Iop_1Uto32, binop(Iop_CmpNE64,
                                          binop(Iop_Add64, cc_dep1, cc_dep2),
                                          mkU64(0)));
         }
         goto missed;
      }

      /* S390_CC_OP_UNSIGNED_ADD_32 */
      if (cc_op == S390_CC_OP_UNSIGNED_ADD_32) {
         /*
            cc_dep1, cc_dep2 are the zero extended left and right operands

            cc == 0  --> result == 0, no carry  (cond == 8)
            cc == 1  --> result != 0, no carry  (cond == 4)
            cc == 2  --> result == 0, carry     (cond == 2)
            cc == 3  --> result != 0, carry     (cond == 1)
         */
         if (cond == 8) { /* cc == 0 */
            /* Both inputs are 0 */
            return unop(Iop_1Uto32, binop(Iop_CmpEQ64,
                                          binop(Iop_Or64, cc_dep1, cc_dep2),
                                          mkU64(0)));
         }
         if (cond == 7) { /* cc == 1,2,3 */
            /* Not both inputs are 0 */
            return unop(Iop_1Uto32, binop(Iop_CmpNE64,
                                          binop(Iop_Or64, cc_dep1, cc_dep2),
                                          mkU64(0)));
         }
         if (cond == 8 + 2) {  /* cc == 0,2  -> result is zero */
            return unop(Iop_1Uto32, binop(Iop_CmpEQ32,
                                          binop(Iop_Add32,
                                                unop(Iop_64to32, cc_dep1),
                                                unop(Iop_64to32, cc_dep2)),
                                          mkU32(0)));
         }
         if (cond == 4 + 1) {  /* cc == 1,3  -> result is not zero */
            return unop(Iop_1Uto32, binop(Iop_CmpNE32,
                                          binop(Iop_Add32,
                                                unop(Iop_64to32, cc_dep1),
                                                unop(Iop_64to32, cc_dep2)),
                                          mkU32(0)));
         }
         goto missed;
      }

      /* S390_CC_OP_SET */
      if (cc_op == S390_CC_OP_SET) {
         /* cc_dep1 is the condition code

            Return 1, if ((cond << cc_dep1) & 0x8) != 0 */

        return unop(Iop_1Uto32,
                    binop(Iop_CmpNE64,
                          binop(Iop_And64,
                                binop(Iop_Shl64, cond_expr,
                                      unop(Iop_64to8, cc_dep1)),
                                mkU64(8)),
                          mkU64(0)));
      }

      goto missed;
   }

   /* --------- Specialising "s390_calculate_cc" --------- */

   if (vex_streq(function_name, "s390_calculate_cc")) {
      IRExpr *cc_op_expr, *cc_dep1;
      ULong cc_op;

      vassert(arity == 4);

      cc_op_expr = args[0];

      /* The necessary requirement for all optimizations here is that
         cc_op is constant. So check that upfront. */
      if (! isC64(cc_op_expr)) return NULL;

      cc_op   = cc_op_expr->Iex.Const.con->Ico.U64;
      cc_dep1 = args[1];

      if (cc_op == S390_CC_OP_BITWISE) {
         return unop(Iop_1Uto32,
                     binop(Iop_CmpNE64, cc_dep1, mkU64(0)));
      }

      if (cc_op == S390_CC_OP_SET) {
         return unop(Iop_64to32, cc_dep1);
      }

      goto missed;
   }

missed:
   return NULL;
}

/*------------------------------------------------------------*/
/*--- Dirty helper for vector instructions                 ---*/
/*------------------------------------------------------------*/

#if defined(VGA_s390x)
ULong
s390x_dirtyhelper_vec_op(VexGuestS390XState *guest_state,
                         const ULong serialized)
{
   UInt psw;
   s390x_vec_op_details_t details;
   const s390x_vec_op_details_t* d = (const s390x_vec_op_details_t*) &details;

   details.serialized = serialized;

   vassert(d->op > S390_VEC_OP_INVALID && d->op < S390_VEC_OP_LAST);
   static const UChar opcodes[][2] = {
      {0x00, 0x00}, /* invalid */
      [S390_VEC_OP_VPKS]  = {0xe7, 0x97},
      [S390_VEC_OP_VPKLS] = {0xe7, 0x95},
      [S390_VEC_OP_VCEQ]  = {0xe7, 0xf8},
      [S390_VEC_OP_VTM]   = {0xe7, 0xd8},
      [S390_VEC_OP_VGFM]  = {0xe7, 0xb4},
      [S390_VEC_OP_VGFMA] = {0xe7, 0xbc},
      [S390_VEC_OP_VMAH]  = {0xe7, 0xab},
      [S390_VEC_OP_VMALH] = {0xe7, 0xa9},
      [S390_VEC_OP_VCH]   = {0xe7, 0xfb},
      [S390_VEC_OP_VCHL]  = {0xe7, 0xf9},
      [S390_VEC_OP_VFTCI] = {0xe7, 0x4a},
      [S390_VEC_OP_VFMIN] = {0xe7, 0xee},
      [S390_VEC_OP_VFMAX] = {0xe7, 0xef},
      [S390_VEC_OP_VBPERM]= {0xe7, 0x85},
      [S390_VEC_OP_VMSL]  = {0xe7, 0xb8},
   };

   union {
      struct {
        unsigned int op1 : 8;
        unsigned int v1  : 4;
        unsigned int v2  : 4;
        unsigned int v3  : 4;
        unsigned int     : 4;
        unsigned int m5  : 4;
        unsigned int     : 4;
        unsigned int m4  : 4;
        unsigned int rxb : 4;
        unsigned int op2 : 8;
      } VRR;
      struct {
        unsigned int op1 : 8;
        unsigned int v1  : 4;
        unsigned int v2  : 4;
        unsigned int v3  : 4;
        unsigned int m5  : 4;
        unsigned int m6  : 4;
        unsigned int     : 4;
        unsigned int v4  : 4;
        unsigned int rxb : 4;
        unsigned int op2 : 8;
      } VRRd;
      struct {
         UInt op1 : 8;
         UInt v1  : 4;
         UInt v2  : 4;
         UInt v3  : 4;
         UInt     : 4;
         UInt m6  : 4;
         UInt m5  : 4;
         UInt m4  : 4;
         UInt rxb : 4;
         UInt op2 : 8;
      } VRRc;
      struct {
         UInt op1 : 8;
         UInt v1  : 4;
         UInt v2  : 4;
         UInt i3  : 12;
         UInt m5  : 4;
         UInt m4  : 4;
         UInt rxb : 4;
         UInt op2 : 8;
      } VRIe;
      UChar bytes[6];
   } the_insn;

   the_insn.VRR.op1 = opcodes[d->op][0];
   the_insn.bytes[1] = the_insn.bytes[2]
      = the_insn.bytes[3] = the_insn.bytes[4] = 0;
   the_insn.VRR.op2 = opcodes[d->op][1];

   switch(d->op) {
   case S390_VEC_OP_VTM:
      the_insn.VRR.v1 = 2;
      the_insn.VRR.v2 = 3;
      the_insn.VRR.rxb = 0b1100;
      break;

   case S390_VEC_OP_VPKS:
   case S390_VEC_OP_VPKLS:
   case S390_VEC_OP_VCEQ:
   case S390_VEC_OP_VGFM:
   case S390_VEC_OP_VCH:
   case S390_VEC_OP_VCHL:
      the_insn.VRR.v1 = 1;
      the_insn.VRR.v2 = 2;
      the_insn.VRR.v3 = 3;
      the_insn.VRR.rxb = 0b1110;
      the_insn.VRR.m4 = d->m4;
      the_insn.VRR.m5 = d->m5;
      break;

   case S390_VEC_OP_VGFMA:
   case S390_VEC_OP_VMAH:
   case S390_VEC_OP_VMALH:
   case S390_VEC_OP_VMSL:
      the_insn.VRRd.v1 = 1;
      the_insn.VRRd.v2 = 2;
      the_insn.VRRd.v3 = 3;
      the_insn.VRRd.v4 = 4;
      the_insn.VRRd.rxb = 0b1111;
      the_insn.VRRd.m5 = d->m4;
      the_insn.VRRd.m6 = d->m5;
      break;

   case S390_VEC_OP_VFMIN:
   case S390_VEC_OP_VFMAX:
   case S390_VEC_OP_VBPERM:
      the_insn.VRRc.v1 = 1;
      the_insn.VRRc.v2 = 2;
      the_insn.VRRc.v3 = 3;
      the_insn.VRRc.rxb = 0b1110;
      the_insn.VRRc.m4 = d->m4;
      the_insn.VRRc.m5 = d->m5;
      the_insn.VRRc.m6 = d->m6;
      break;

   case S390_VEC_OP_VFTCI:
      the_insn.VRIe.v1 = 1;
      the_insn.VRIe.v2 = 2;
      the_insn.VRIe.rxb = 0b1100;
      the_insn.VRIe.i3 = d->i3;
      the_insn.VRIe.m4 = d->m4;
      the_insn.VRIe.m5 = d->m5;
      break;

   default:
      vex_printf("operation = %d\n", d->op);
      vpanic("s390x_dirtyhelper_vec_op: unknown operation");
   }

   const V128* guest_v = &(guest_state->guest_v0);
   __asm__ volatile (
      "lgr %%r10, %[arg1]\n"
      VL(2, 0, a, 000, 8)
      "lgr %%r10, %[arg2]\n"
      VL(3, 0, a, 000, 8)
      "lgr %%r10, %[arg3]\n"
      VL(4, 0, a, 000, 8)
      "ex %[zero], %[insn]\n"

      "cijne %[read_only], 0, return_cc\n"
      "lgr %%r10, %[res]\n"
      VST(1, 0, a, 000, 8)

      "return_cc: "
      "ipm %[psw]\n\t"
         : [psw] "=d" (psw)

         : [res]  "r" (&guest_v[d->v1]),
           [arg1] "r" (&guest_v[d->v2]),
           [arg2] "r" (&guest_v[d->v3]),
           [arg3] "r" (&guest_v[d->v4]),

           [zero] "r" (0ULL),
           [insn] "R" (the_insn),
           [read_only] "r" (d->read_only)

         : "cc", "r10", "v16", "v17", "v18", "v19"
      );

   return psw >> 28;   /* cc */
}

#else

ULong
s390x_dirtyhelper_vec_op(VexGuestS390XState *guest_state,
                         const ULong serialized)
{ return 0; }

#endif

/*-----------------------------------------------------------------*/
/*--- Dirty helper for Perform Pseudorandom number instruction  ---*/
/*-----------------------------------------------------------------*/

/* Dummy helper that is needed to indicate load of parameter block.
   We have to use it because dirty helper cannot have two memory side
   effects.
 */
void s390x_dirtyhelper_PPNO_sha512_load_param_block( void )
{
}

#if defined(VGA_s390x)

/* IMPORTANT!
   We return here bit mask where only supported functions are set to one.
   If you implement new functions don't forget the supported array.
 */
void
s390x_dirtyhelper_PPNO_query(VexGuestS390XState *guest_state, ULong r1, ULong r2)
{
   ULong supported[2] = {0x9000000000000000ULL, 0x0000000000000000ULL};
   ULong *result = (ULong*) guest_state->guest_r1;

   result[0] = supported[0];
   result[1] = supported[1];
}

ULong
s390x_dirtyhelper_PPNO_sha512(VexGuestS390XState *guest_state, ULong r1, ULong r2)
{
   ULong* op1 = (ULong*) (((ULong)(&guest_state->guest_r0)) + r1 * sizeof(ULong));
   ULong* op2 = (ULong*) (((ULong)(&guest_state->guest_r0)) + r2 * sizeof(ULong));

   register ULong reg0 asm("0") = guest_state->guest_r0;
   register ULong reg1 asm("1") = guest_state->guest_r1;
   register ULong reg2 asm("2") = op1[0];
   register ULong reg3 asm("3") = op1[1];
   register ULong reg4 asm("4") = op2[0];
   register ULong reg5 asm("5") = op2[1];

   ULong cc = 0;
   asm volatile(".insn rre, 0xb93c0000, %%r2, %%r4\n"
                "ipm %[cc]\n"
                "srl %[cc], 28\n"
                : "+d"(reg0), "+d"(reg1),
                  "+d"(reg2), "+d"(reg3),
                  "+d"(reg4), "+d"(reg5),
                  [cc] "=d"(cc)
                :
                : "cc", "memory");

   return cc;
}

#else

void
s390x_dirtyhelper_PPNO_query(VexGuestS390XState *guest_state, ULong r1, ULong r2)
{
}

ULong
s390x_dirtyhelper_PPNO_sha512(VexGuestS390XState *guest_state, ULong r1, ULong r2)
{
   return 0;
}

#endif /* VGA_s390x */
/*---------------------------------------------------------------*/
/*--- end                                guest_s390_helpers.c ---*/
/*---------------------------------------------------------------*/
