/* -*- mode: C; c-basic-offset: 3; -*- */

/*---------------------------------------------------------------*/
/*--- begin                                     s390_disasm.h ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright IBM Corp. 2010-2017

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#ifndef __VEX_S390_DISASM_H
#define __VEX_S390_DISASM_H

#include "libvex_basictypes.h"

/* The different kinds of operands in an asm insn */
typedef enum {
   S390_OPND_DONE,
   S390_OPND_GPR,
   S390_OPND_FPR,
   S390_OPND_AR,
   S390_OPND_VR,
   S390_OPND_INT,
   S390_OPND_UINT,
   S390_OPND_PCREL,
   S390_OPND_SDXB,
   S390_OPND_UDXB,
   S390_OPND_UDLB,
   S390_OPND_UDVB,
   S390_OPND_MNM,
   S390_OPND_XMNM,
   S390_OPND_MASK,      // used for operands that modify the mnemonic
} opnd_t;

typedef struct s390_opnd s390_opnd;

struct s390_opnd {
   opnd_t kind;         // S390_OPND_....
   union {
      const HChar *mnm; // MNM
      UInt regno;       // GPR, AR, FPR, VR
      UInt mask;        // MASK
      Int  pcrel;       // PCREL
      UInt u;           // UINT
      Int  i;           // INT
      struct {          // UDXB, SDXB, UDLB, UDVB
         UInt d : 20;
         union {
            UInt x : 4;
            UInt l : 8;
            UInt v : 5;
         };
         UInt b : 4;
      };
      struct {
         const HChar *base;
         HChar *(*handler)(const s390_opnd *, HChar *);
      } xmnm;
   };
};

/* Convenience macro to piece together a 20-bit displacement value. */
#define D20(dh,dl) (((dh) << 12) | (dl))

/* Macros for operand construction */
#define MNM(x)         { S390_OPND_MNM,   .mnm   = (x) }
#define GPR(x)         { S390_OPND_GPR,   .regno = (x) }
#define FPR(x)         { S390_OPND_FPR,   .regno = (x) }
#define AR(x)          { S390_OPND_AR,    .regno = (x) }
#define VR(x)          { S390_OPND_VR,    .regno = (x) }
#define MASK(x)        { S390_OPND_MASK,  .mask  = (x) }
#define UINT(x)        { S390_OPND_UINT,  .u     = (x) }
#define INT(x)         { S390_OPND_INT,   .i     = (x) }
#define PCREL(x)       { S390_OPND_PCREL, .pcrel = (x) }
#define UDXB(_d,_x,_b) { S390_OPND_UDXB,  .d = (_d), .x = (_x), .b = (_b) }
#define UDVB(_d,_v,_b) { S390_OPND_UDVB,  .d = (_d), .v = (_v), .b = (_b) }
#define UDLB(_d,_l,_b) { S390_OPND_UDLB,  .d = (_d), .l = (_l), .b = (_b) }
#define SDXB(dh,dl,_x,_b) \
            { S390_OPND_SDXB, .d = D20((dh), (dl)), .x = (_x), .b = (_b) }
#define XMNM(mnm,h) \
            { S390_OPND_XMNM, .xmnm.base = (mnm), .xmnm.handler = (h) }

#define S390_DISASM(...) \
        s390_disasm((s390_opnd []){ __VA_ARGS__, { S390_OPND_DONE } })

void s390_disasm(const s390_opnd *);

/* Handlers for extended mnemonics */
HChar *bc_disasm(const s390_opnd *, HChar *);
HChar *bcr_disasm(const s390_opnd *, HChar *);
HChar *brc_disasm(const s390_opnd *, HChar *);
HChar *bic_disasm(const s390_opnd *, HChar *);
HChar *cls_disasm(const s390_opnd *, HChar *);
HChar *brcl_disasm(const s390_opnd *, HChar *);
HChar *cabt_disasm(const s390_opnd *, HChar *);
HChar *mask0_disasm(const s390_opnd *, HChar *);
HChar *vcdg_disasm(const s390_opnd *, HChar *);
HChar *vcgd_disasm(const s390_opnd *, HChar *);
HChar *vcgld_disasm(const s390_opnd *, HChar *);
HChar *vclgd_disasm(const s390_opnd *, HChar *);
HChar *vfi_disasm(const s390_opnd *, HChar *);
HChar *vfll_disasm(const s390_opnd *, HChar *);
HChar *vflr_disasm(const s390_opnd *, HChar *);
HChar *vfms_like_disasm(const s390_opnd *, HChar *);
HChar *vfpso_disasm(const s390_opnd *, HChar *);
HChar *vgbm_disasm(const s390_opnd *, HChar *);
HChar *vllez_disasm(const s390_opnd *, HChar *);
HChar *vllebrz_disasm(const s390_opnd *, HChar *);
HChar *vmsl_disasm(const s390_opnd *, HChar *);
HChar *vstebrf_disasm(const s390_opnd *, HChar *);
HChar *vstebrg_disasm(const s390_opnd *, HChar *);
HChar *vstrc_disasm(const s390_opnd *, HChar *);
HChar *va_like_disasm(const s390_opnd *, HChar *);
HChar *vch_like_disasm(const s390_opnd *, HChar *);
HChar *vfa_like_disasm(const s390_opnd *, HChar *);
HChar *wfc_like_disasm(const s390_opnd *, HChar *);
HChar *vfae_like_disasm(const s390_opnd *, HChar *);
HChar *vfce_like_disasm(const s390_opnd *, HChar *);
HChar *vfmix_like_disasm(const s390_opnd *, HChar *);
HChar *fp_convf_disasm(const s390_opnd *, HChar *);
HChar *fp_convt_disasm(const s390_opnd *, HChar *);
HChar *adtra_like_disasm(const s390_opnd *, HChar *);
HChar *rotate_disasm(const s390_opnd *, HChar *);

/*---------------------------------------------------------------*/
/*--- end                                       s390_disasm.h ---*/
/*---------------------------------------------------------------*/

#endif /* __VEX_S390_DISASM_H */
