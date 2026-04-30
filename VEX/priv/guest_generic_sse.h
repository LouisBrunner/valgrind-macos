/*---------------------------------------------------------------*/
/*--- begin                               guest_generic_sse.h ---*/
/*---------------------------------------------------------------*/

/*
     This file is part of Valgrind, a dynamic binary instrumentation
     framework.

     Copyright (C) 2025-2026 Alexandra Hájková <ahajkova@redhat.com>

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

/*   This file contains functions for SSE/SSE2/SSE3/SSE4-specific
     operations.  Both the amd64 and x86 front ends (guests) call
     these functions.  By putting them here, code duplication is
     avoided.  Some of these functions are tricky and hard to verify,
     so there is much to be said for only having one copy thereof.

     IMPORTANT: This header must be included AFTER the following
     helper functions/macros are defined in the including file:
       - newTemp(IRType)          - allocate a new IRTemp
       - assign(IRTemp, IRExpr*)  - create assignment statement
       - mkV128(UShort)           - create V128 constant expression
       - binop(IROp, IRExpr*, IRExpr*)  - create binary operation
       - unop(IROp, IRExpr*)      - create unary operation
       - mkexpr(IRTemp)           - create IRTemp expression
       - vassert(Bool)            - assertion macro

     These helper functions are defined locally in each guest_*_toIR.c
     file because they rely on file-local global state, particularly
     the 'irsb' variable (the IR super block being built). Moving them
     to a shared header would require either passing 'irsb' as a
     parameter to each call (breaking thousands of call sites) or
     making 'irsb' a shared extern (architecturally problematic).

     Therefore, this header is included after those helpers are defined,
     typically around line 300 in guest_*_toIR.c files.
*/

#ifndef __VEX_GUEST_GENERIC_SSE_H
#define __VEX_GUEST_GENERIC_SSE_H

#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex.h"
#include "guest_generic_helpers.h"


/* BLENDPD 128-bit */
static inline IRTemp math_BLENDPD_128 ( IRTemp sV, IRTemp dV, UInt imm8 )
{
   UShort imm8_mask_16;
   IRTemp imm8_mask = newTemp(Ity_V128);

   switch( imm8 & 3 ) {
      case 0:  imm8_mask_16 = 0x0000; break;
      case 1:  imm8_mask_16 = 0x00FF; break;
      case 2:  imm8_mask_16 = 0xFF00; break;
      case 3:  imm8_mask_16 = 0xFFFF; break;
      default: vassert(0);            break;
   }
   assign( imm8_mask, mkV128( imm8_mask_16 ) );

   IRTemp res = newTemp(Ity_V128);
   assign ( res, binop( Iop_OrV128,
                        binop( Iop_AndV128, mkexpr(sV),
                                            mkexpr(imm8_mask) ),
                        binop( Iop_AndV128, mkexpr(dV),
                               unop( Iop_NotV128, mkexpr(imm8_mask) ) ) ) );
   return res;
}

static inline IRTemp math_BLENDPS_128 ( IRTemp sV, IRTemp dV, UInt imm8 )
{
   UShort imm8_perms[16] = { 0x0000, 0x000F, 0x00F0, 0x00FF, 0x0F00,
                             0x0F0F, 0x0FF0, 0x0FFF, 0xF000, 0xF00F,
                             0xF0F0, 0xF0FF, 0xFF00, 0xFF0F, 0xFFF0,
                             0xFFFF };
   IRTemp imm8_mask = newTemp(Ity_V128);
   assign( imm8_mask, mkV128( imm8_perms[ (imm8 & 15) ] ) );

   IRTemp res = newTemp(Ity_V128);
   assign ( res, binop( Iop_OrV128,
                        binop( Iop_AndV128, mkexpr(sV),
                                            mkexpr(imm8_mask) ),
                        binop( Iop_AndV128, mkexpr(dV),
                               unop( Iop_NotV128, mkexpr(imm8_mask) ) ) ) );
   return res;
}

/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- Top-level SSE4: dis_ESC_0F38__SSE4                   ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

static inline IRTemp math_PBLENDVB_128 ( IRTemp vecE, IRTemp vecG,
                                  IRTemp vec0/*controlling mask*/,
                                  UInt gran, IROp opSAR )
{
   /* The tricky bit is to convert vec0 into a suitable mask, by
      copying the most significant bit of each lane into all positions
      in the lane. */
   IRTemp sh = newTemp(Ity_I8);
   assign(sh, mkU8(8 * gran - 1));

   IRTemp mask = newTemp(Ity_V128);
   assign(mask, binop(opSAR, mkexpr(vec0), mkexpr(sh)));

   IRTemp notmask = newTemp(Ity_V128);
   assign(notmask, unop(Iop_NotV128, mkexpr(mask)));

   IRTemp res = newTemp(Ity_V128);
   assign(res,  binop(Iop_OrV128,
                      binop(Iop_AndV128, mkexpr(vecE), mkexpr(mask)),
                      binop(Iop_AndV128, mkexpr(vecG), mkexpr(notmask))));
   return res;
}

static inline IRTemp math_PBLENDW_128 ( IRTemp sV, IRTemp dV, UInt imm8 )
{
   /* Make w be a 16-bit version of imm8, formed by duplicating each
      bit in imm8. */
   Int i;
   UShort imm16 = 0;
   for (i = 0; i < 8; i++) {
      if (imm8 & (1 << i))
         imm16 |= (3 << (2*i));
   }
   IRTemp imm16_mask = newTemp(Ity_V128);
   assign( imm16_mask, mkV128( imm16 ));

   IRTemp res = newTemp(Ity_V128);
   assign ( res, binop( Iop_OrV128,
                        binop( Iop_AndV128, mkexpr(sV),
                                            mkexpr(imm16_mask) ),
                        binop( Iop_AndV128, mkexpr(dV),
                               unop( Iop_NotV128, mkexpr(imm16_mask) ) ) ) );
   return res;
}

static IRTemp math_MPSADBW_128 ( IRTemp dst_vec, IRTemp src_vec, UInt imm8 )
{
   /* Mask out bits of the operands we don't need.  This isn't
      strictly necessary, but it does ensure Memcheck doesn't
      give us any false uninitialised value errors as a
      result. */
   UShort src_mask[4] = { 0x000F, 0x00F0, 0x0F00, 0xF000 };
   UShort dst_mask[2] = { 0x07FF, 0x7FF0 };

   IRTemp src_maskV = newTemp(Ity_V128);
   IRTemp dst_maskV = newTemp(Ity_V128);
   assign(src_maskV, mkV128( src_mask[ imm8 & 3 ] ));
   assign(dst_maskV, mkV128( dst_mask[ (imm8 >> 2) & 1 ] ));

   IRTemp src_masked = newTemp(Ity_V128);
   IRTemp dst_masked = newTemp(Ity_V128);
   assign(src_masked, binop(Iop_AndV128, mkexpr(src_vec), mkexpr(src_maskV)));
   assign(dst_masked, binop(Iop_AndV128, mkexpr(dst_vec), mkexpr(dst_maskV)));

   /* Generate 4 64 bit values that we can hand to a clean helper */
   IRTemp sHi = newTemp(Ity_I64);
   IRTemp sLo = newTemp(Ity_I64);
   assign( sHi, unop(Iop_V128HIto64, mkexpr(src_masked)) );
   assign( sLo, unop(Iop_V128to64,   mkexpr(src_masked)) );

   IRTemp dHi = newTemp(Ity_I64);
   IRTemp dLo = newTemp(Ity_I64);
   assign( dHi, unop(Iop_V128HIto64, mkexpr(dst_masked)) );
   assign( dLo, unop(Iop_V128to64,   mkexpr(dst_masked)) );

   /* Compute halves of the result separately */
   IRTemp resHi = newTemp(Ity_I64);
   IRTemp resLo = newTemp(Ity_I64);

   IRExpr** argsHi
      = mkIRExprVec_5( mkexpr(sHi), mkexpr(sLo), mkexpr(dHi), mkexpr(dLo),
                       mkU64( 0x80 | (imm8 & 7) ));
   IRExpr** argsLo
      = mkIRExprVec_5( mkexpr(sHi), mkexpr(sLo), mkexpr(dHi), mkexpr(dLo),
                       mkU64( 0x00 | (imm8 & 7) ));

   assign(resHi, mkIRExprCCall( Ity_I64, 0/*regparm*/,
         "g_calc_mpsadbw",
         &g_calc_mpsadbw, argsHi ));
   assign(resLo, mkIRExprCCall( Ity_I64, 0/*regparm*/,
         "g_calc_mpsadbw",
         &g_calc_mpsadbw, argsLo ));

   IRTemp res = newTemp(Ity_V128);
   assign(res, binop(Iop_64HLtoV128, mkexpr(resHi), mkexpr(resLo)));
   return res;
}

#endif /* ndef __VEX_GUEST_GENERIC_SSE_H */

/*---------------------------------------------------------------*/
/*--- end                                 guest_generic_sse.h ---*/
/*---------------------------------------------------------------*/
