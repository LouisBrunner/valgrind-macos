/* -*- mode: C; c-basic-offset: 3; -*- */

/*---------------------------------------------------------------*/
/*--- begin                                       ir_inject.c ---*/
/*---------------------------------------------------------------*/


/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2012-2015  Florian Krohm   (britzel@acm.org)

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.

   The GNU General Public License is contained in the file COPYING.
*/

#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex.h"
#include "main_util.h"

/* Convenience macros for readibility */
#define mkU8(v)   IRExpr_Const(IRConst_U8(v))
#define mkU16(v)  IRExpr_Const(IRConst_U16(v))
#define mkU32(v)  IRExpr_Const(IRConst_U32(v))
#define mkU64(v)  IRExpr_Const(IRConst_U64(v))
#define unop(kind, a)  IRExpr_Unop(kind, a)
#define binop(kind, a1, a2)  IRExpr_Binop(kind, a1, a2)
#define triop(kind, a1, a2, a3)  IRExpr_Triop(kind, a1, a2, a3)
#define qop(kind, a1, a2, a3, a4)  IRExpr_Qop(kind, a1, a2, a3, a4)
#define stmt(irsb, st)  addStmtToIRSB(irsb, st)


/* The IR Injection Control Block. vex_inject_ir will query its contents
   to construct IR statements for testing purposes. */
static IRICB iricb;


void
LibVEX_InitIRI(const IRICB *iricb_in)
{
   iricb = *iricb_in;  // copy in
}


static IRExpr *
load_aux(IREndness endian, IRType type, IRExpr *addr)
{
   if (type == Ity_D64) {
      /* The insn selectors do not support loading a DFP value from memory.
         So we need to fix it here by loading an integer value and
         reinterpreting it as DFP. */
      return unop(Iop_ReinterpI64asD64,
                  IRExpr_Load(endian, Ity_I64, addr));
   }
   if (type == Ity_I1) {
      /* A Boolean value is stored as a 32-bit entity (see store_aux). */
      return unop(Iop_32to1, IRExpr_Load(endian, Ity_I32, addr));
   }

   return IRExpr_Load(endian, type, addr);
}


/* Load a value from memory. Loads of more than 8 byte are split into
   a series of 8-byte loads and combined using appropriate IROps. */
static IRExpr *
load(IREndness endian, IRType type, HWord haddr)
{
   IROp concat;
   IRExpr *addr, *next_addr;

   vassert(type == Ity_I1 || sizeofIRType(type) <= 16);

   if (VEX_HOST_WORDSIZE == 8) {
      addr = mkU64(haddr);
      next_addr = binop(Iop_Add64, addr, mkU64(8));
   } else if (VEX_HOST_WORDSIZE == 4) {
      addr = mkU32(haddr);
      next_addr = binop(Iop_Add32, addr, mkU32(8));
   } else {
      vpanic("invalid #bytes for address");
   }

   switch (type) {
   case Ity_I128: concat = Iop_64HLto128;   type = Ity_I64; goto load128;
   case Ity_F128: concat = Iop_F64HLtoF128; type = Ity_F64; goto load128;
   case Ity_D128: concat = Iop_D64HLtoD128; type = Ity_D64; goto load128;

   load128:
     /* Two loads of 64 bit each. */
      if (endian == Iend_BE) {
         /* The more significant bits are at the lower address. */
         return binop(concat,
                      load_aux(endian, type, addr),
                      load_aux(endian, type, next_addr));
      } else {
         /* The more significant bits are at the higher address. */
         return binop(concat,
                      load_aux(endian, type, next_addr),
                      load_aux(endian, type, addr));
      }

   default:
      return load_aux(endian, type, addr);
   }
}


static void
store_aux(IRSB *irsb, IREndness endian, IRExpr *addr, IRExpr *data)
{
   if (typeOfIRExpr(irsb->tyenv, data) == Ity_D64) {
      /* The insn selectors do not support writing a DFP value to memory.
         So we need to fix it here by reinterpreting the DFP value as an
         integer and storing that. */
      data = unop(Iop_ReinterpD64asI64, data);
   }
   if (typeOfIRExpr(irsb->tyenv, data) == Ity_I1) {
      /* We cannot store a single bit. So we store it in a 32-bit container.
         See also load_aux. */
      data = unop(Iop_1Uto32, data);
   }
   stmt(irsb, IRStmt_Store(endian, addr, data));
}


/* Store a value to memory. If a value requires more than 8 bytes a series
   of 8-byte stores will be generated. */
static __inline__ void
store(IRSB *irsb, IREndness endian, HWord haddr, IRExpr *data)
{
   IROp high, low;
   IRExpr *addr, *next_addr;

   if (VEX_HOST_WORDSIZE == 8) {
      addr = mkU64(haddr);
      next_addr = binop(Iop_Add64, addr, mkU64(8));
   } else if (VEX_HOST_WORDSIZE == 4) {
      addr = mkU32(haddr);
      next_addr = binop(Iop_Add32, addr, mkU32(8));
   } else {
      vpanic("invalid #bytes for address");
   }

   IRType type = typeOfIRExpr(irsb->tyenv, data);

   vassert(type == Ity_I1 || sizeofIRType(type) <= 16);

   switch (type) {
   case Ity_I128: high = Iop_128HIto64;   low = Iop_128to64;     goto store128;
   case Ity_F128: high = Iop_F128HItoF64; low = Iop_F128LOtoF64; goto store128;
   case Ity_D128: high = Iop_D128HItoD64; low = Iop_D128LOtoD64; goto store128;

   store128:
     /* Two stores of 64 bit each. */
      if (endian == Iend_BE) {
         /* The more significant bits are at the lower address. */
         store_aux(irsb, endian, addr, unop(high, data));
         store_aux(irsb, endian, next_addr, unop(low, data));
      } else {
         /* The more significant bits are at the higher address. */
         store_aux(irsb, endian, addr, unop(low, data));
         store_aux(irsb, endian, next_addr, unop(high, data));
      }
      return;

   default:
      store_aux(irsb, endian, addr, data);
      return;
   }
}


/* Inject IR stmts depending on the data provided in the control
   block iricb. */
void
vex_inject_ir(IRSB *irsb, IREndness endian)
{
   IRExpr *data, *rounding_mode, *opnd1, *opnd2, *opnd3, *opnd4;

   rounding_mode = NULL;
   if (iricb.rounding_mode != NO_ROUNDING_MODE) {
      rounding_mode = mkU32(iricb.rounding_mode);
   }

   switch (iricb.num_operands) {
   case 1:
      opnd1 = load(endian, iricb.t_opnd1, iricb.opnd1);
      if (rounding_mode)
         data = binop(iricb.op, rounding_mode, opnd1);
      else
         data = unop(iricb.op, opnd1);
      break;

   case 2:
      opnd1 = load(endian, iricb.t_opnd1, iricb.opnd1);
      /* HACK, compiler warning ‘opnd2’ may be used uninitialized */
      opnd2 = opnd1;

      /* immediate_index = 0  immediate value is not used.
       * immediate_index = 2  opnd2 is an immediate value.
       */
      vassert(iricb.immediate_index == 0 || iricb.immediate_index == 2);

      if (iricb.immediate_index == 2) {
         vassert((iricb.t_opnd2 == Ity_I8) || (iricb.t_opnd2 == Ity_I16)
                 || (iricb.t_opnd2 == Ity_I32));

         /* Interpret the memory as an ULong. */
         if (iricb.immediate_type == Ity_I8) {
            opnd2 = mkU8(*((ULong *)iricb.opnd2));
         } else if (iricb.immediate_type == Ity_I16) {
            opnd2 = mkU16(*((ULong *)iricb.opnd2));
         } else if (iricb.immediate_type == Ity_I32) {
            opnd2 = mkU32(*((ULong *)iricb.opnd2));
         }
      } else {
         opnd2 = load(endian, iricb.t_opnd2, iricb.opnd2);
      }

      if (rounding_mode)
         data = triop(iricb.op, rounding_mode, opnd1, opnd2);
      else
         data = binop(iricb.op, opnd1, opnd2);
      break;

   case 3:
      opnd1 = load(endian, iricb.t_opnd1, iricb.opnd1);
      opnd2 = load(endian, iricb.t_opnd2, iricb.opnd2);
      /* HACK, compiler warning ‘opnd3’ may be used uninitialized */
      opnd3 = opnd2;

      /* immediate_index = 0  immediate value is not used.
       * immediate_index = 3  opnd3 is an immediate value.
       */
      vassert(iricb.immediate_index == 0 || iricb.immediate_index == 3);

      if (iricb.immediate_index == 3) {
         vassert((iricb.t_opnd3 == Ity_I8) || (iricb.t_opnd3 == Ity_I16)
                 || (iricb.t_opnd2 == Ity_I32));

         if (iricb.immediate_type == Ity_I8) {
            opnd3 = mkU8(*((ULong *)iricb.opnd3));
         } else if (iricb.immediate_type == Ity_I16) {
            opnd3 = mkU16(*((ULong *)iricb.opnd3));
         } else if (iricb.immediate_type == Ity_I32) {
            opnd3 = mkU32(*((ULong *)iricb.opnd3));
         }
      } else {
         opnd3 = load(endian, iricb.t_opnd3, iricb.opnd3);
      }
      if (rounding_mode)
         data = qop(iricb.op, rounding_mode, opnd1, opnd2, opnd3);
      else
         data = triop(iricb.op, opnd1, opnd2, opnd3);
      break;

   case 4:
      vassert(rounding_mode == NULL);
      opnd1 = load(endian, iricb.t_opnd1, iricb.opnd1);
      opnd2 = load(endian, iricb.t_opnd2, iricb.opnd2);
      opnd3 = load(endian, iricb.t_opnd3, iricb.opnd3);
      /* HACK, compiler warning ‘opnd4’ may be used uninitialized */
      opnd4 = opnd3;

      /* immediate_index = 0  immediate value is not used.
       * immediate_index = 4  opnd4 is an immediate value.
       */
      vassert(iricb.immediate_index == 0 || iricb.immediate_index == 4);

      if (iricb.immediate_index == 4) {
         vassert((iricb.t_opnd3 == Ity_I8) || (iricb.t_opnd3 == Ity_I16)
                 || (iricb.t_opnd2 == Ity_I32));

         if (iricb.immediate_type == Ity_I8) {
            opnd4 = mkU8(*((ULong *)iricb.opnd4));
         } else if (iricb.immediate_type == Ity_I16) {
            opnd4 = mkU16(*((ULong *)iricb.opnd4));
         } else if (iricb.immediate_type == Ity_I32) {
            opnd4 = mkU32(*((ULong *)iricb.opnd4));
         }
      } else {
         opnd4 = load(endian, iricb.t_opnd4, iricb.opnd4);
      }
      data = qop(iricb.op, opnd1, opnd2, opnd3, opnd4);
      break;

   default:
      vpanic("unsupported operator");
   }

   store(irsb, endian, iricb.result, data);

   if (0) {
      vex_printf("BEGIN inject\n");
      if (iricb.t_result == Ity_I1 || sizeofIRType(iricb.t_result) <= 8) {
         ppIRStmt(irsb->stmts[irsb->stmts_used - 1]);
      } else if (sizeofIRType(iricb.t_result) == 16) {
         ppIRStmt(irsb->stmts[irsb->stmts_used - 2]);
         vex_printf("\n");
         ppIRStmt(irsb->stmts[irsb->stmts_used - 1]);
      }
      vex_printf("\nEND inject\n");
   }
}

/*---------------------------------------------------------------*/
/*--- end                                         ir_inject.c ---*/
/*---------------------------------------------------------------*/
