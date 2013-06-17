/* -*- mode: C; c-basic-offset: 3; -*- */

#include <stdio.h>     // fprintf
#include <stdlib.h>    // exit
#include <assert.h>    // assert
#if defined(__APPLE__)
#include <machine/endian.h>
#define __BYTE_ORDER    BYTE_ORDER
#define __LITTLE_ENDIAN LITTLE_ENDIAN
#else
#include <endian.h>
#endif
#include <inttypes.h>
#include "vtest.h"


/* Something bad happened. Cannot continue. */
void __attribute__((noreturn))
panic(const char *string)
{
   fprintf(stderr, "*** OOPS: %s\n", string);
   exit(1);
}


/* Issue a complaint because the V-bits of the result of an operation
   differ from what was expected. */
void
complain(const irop_t *op, const test_data_t *data, vbits_t expected)
{
   fprintf(stderr, "*** Incorrect result for operator %s\n", op->name);
   
   int num_operands = get_num_operands(op->op);

   for (unsigned i = 0; i < num_operands; ++i) {
      fprintf(stderr, "    opnd %u:  ", i);
      print_opnd(stderr, &data->opnds[i]);
      fprintf(stderr, "\n");
   }
   fprintf(stderr, "    result:  ");
   print_opnd(stderr, &data->result);
   fprintf(stderr, "\n");
   fprintf(stderr, "    expect:  vbits = ");
   print_vbits(stderr, expected);
   fprintf(stderr, "\n");
}


static void
print_value(FILE *fp, value_t val, unsigned num_bits)
{
   switch (num_bits) {
   case 1:  fprintf(fp, "%02x",   val.u8);  break;
   case 8:  fprintf(fp, "%02x",   val.u8);  break;
   case 16: fprintf(fp, "%04x",   val.u16); break;
   case 32: fprintf(fp, "%08x",   val.u32); break;
   case 64: fprintf(fp, "%016"PRIx64, val.u64); break;
   case 128:
      if (__BYTE_ORDER == __LITTLE_ENDIAN) {
         fprintf(fp, "%016"PRIx64, val.u128[1]);
         fprintf(fp, "%016"PRIx64, val.u128[0]);
      } else {
         fprintf(fp, "%016"PRIx64, val.u128[0]);
         fprintf(fp, "%016"PRIx64, val.u128[1]);
      }
      break;
   case 256:
      if (__BYTE_ORDER == __LITTLE_ENDIAN) {
         fprintf(fp, "%016"PRIx64, val.u256[3]);
         fprintf(fp, "%016"PRIx64, val.u256[2]);
         fprintf(fp, "%016"PRIx64, val.u256[1]);
         fprintf(fp, "%016"PRIx64, val.u256[0]);
      } else {
         fprintf(fp, "%016"PRIx64, val.u256[0]);
         fprintf(fp, "%016"PRIx64, val.u256[1]);
         fprintf(fp, "%016"PRIx64, val.u256[2]);
         fprintf(fp, "%016"PRIx64, val.u256[3]);
      }
      break;
  default:
      panic(__func__);
   }
}


void
print_opnd(FILE *fp, const opnd_t *opnd)
{
   fprintf(fp, "vbits = ");
   print_vbits(fp, opnd->vbits);
   /* Write the value only if it is defined. Otherwise, there will be error
      messages about it being undefined */
   if (equal_vbits(opnd->vbits, defined_vbits(opnd->vbits.num_bits))) {
      fprintf(fp, "   value = ");
      print_value(fp, opnd->value, opnd->vbits.num_bits);
   }
}


static int
is_floating_point_type(IRType type)
{
   switch (type) {
   case Ity_F32:
   case Ity_F64:
   case Ity_F128:
   case Ity_D32:
   case Ity_D64:
   case Ity_D128:
      return 1;

   default:
      return 0;
   }
}


int
is_floating_point_op_with_rounding_mode(IROp op)
{
   IRType t_dst, t_arg1, t_arg2, t_arg3, t_arg4;

   typeof_primop(op, &t_dst, &t_arg1, &t_arg2, &t_arg3, &t_arg4);

   // A unary operator cannot have a rounding mode
   if (t_arg2 == Ity_INVALID) return 0;

   if (is_floating_point_type(t_dst)  ||
       is_floating_point_type(t_arg1) || 
       is_floating_point_type(t_arg2) || 
       is_floating_point_type(t_arg3) || 
       is_floating_point_type(t_arg4)) {
      // Rounding mode, if present, is the 1st operand
      return t_arg1 == Ity_I32;
   }
   return 0;
}


/* Return the number of operands for which input values can
   be freely chosen. For floating point ops, the rounding mode
   is not counted here, as it is restricted. */
int
get_num_operands(IROp op)
{
   IRType unused, t1, t2, t3, t4;

   typeof_primop(op, &unused, &t1, &t2, &t3, &t4);

   int num_operands = 4;
   if (t4 == Ity_INVALID) num_operands = 3;
   if (t3 == Ity_INVALID) num_operands = 2;
   if (t2 == Ity_INVALID) num_operands = 1;

   if (is_floating_point_op_with_rounding_mode(op))
      -- num_operands;

   return num_operands;
}


/* ---------------------------------------------------------------- */

/* The functions below have been imported from VEX/pric/ir_defs.c.
   This is more convenient because
   (1) Don't have to figure out the Makefile machinery to pick up the
       correct VEX library (platform specific)
   (2) Would have to export typeofIRType in VEX
   (3) There is no worry that these functions get out of synch because
       the test harness will iterate over all IROps in libvex_ir.h.
       So if a new one was added there, we would assert here and elsewhere.
*/

// Taken from VEX/priv/ir_defs.c: function sizeofIRType
unsigned
sizeof_irtype(IRType ty)
{
   switch (ty) {
      case Ity_I8:   return 1;
      case Ity_I16:  return 2;
      case Ity_I32:  return 4;
      case Ity_I64:  return 8;
      case Ity_I128: return 16;
      case Ity_F32:  return 4;
      case Ity_F64:  return 8;
      case Ity_F128: return 16;
      case Ity_D32:  return 4;
      case Ity_D64:  return 8;
      case Ity_D128: return 16;
      case Ity_V128: return 16;
      case Ity_V256: return 32;
      default:
         panic(__func__);
   }
}


// Taken from VEX/priv/ir_defs.c: function typeOfPrimop
// Modified minimally to break dependencies on VEX infrastructure.
void
typeof_primop(IROp op, IRType *t_dst, IRType *t_arg1, IRType *t_arg2, 
              IRType *t_arg3, IRType *t_arg4)
{
#  define UNARY(_ta1,_td)                                      \
      *t_dst = (_td); *t_arg1 = (_ta1); break
#  define BINARY(_ta1,_ta2,_td)                                \
     *t_dst = (_td); *t_arg1 = (_ta1); *t_arg2 = (_ta2); break
#  define TERNARY(_ta1,_ta2,_ta3,_td)                          \
     *t_dst = (_td); *t_arg1 = (_ta1);                         \
     *t_arg2 = (_ta2); *t_arg3 = (_ta3); break
#  define QUATERNARY(_ta1,_ta2,_ta3,_ta4,_td)                  \
     *t_dst = (_td); *t_arg1 = (_ta1);                         \
     *t_arg2 = (_ta2); *t_arg3 = (_ta3);                       \
     *t_arg4 = (_ta4); break
#  define COMPARISON(_ta)                                      \
     *t_dst = Ity_I1; *t_arg1 = *t_arg2 = (_ta); break;
#  define UNARY_COMPARISON(_ta)                                \
     *t_dst = Ity_I1; *t_arg1 = (_ta); break;

   /* Rounding mode values are always Ity_I32, encoded as per
      IRRoundingMode */
   const IRType ity_RMode = Ity_I32;

   *t_dst  = Ity_INVALID;
   *t_arg1 = Ity_INVALID;
   *t_arg2 = Ity_INVALID;
   *t_arg3 = Ity_INVALID;
   *t_arg4 = Ity_INVALID;
   switch (op) {
      case Iop_Add8: case Iop_Sub8: case Iop_Mul8: 
      case Iop_Or8:  case Iop_And8: case Iop_Xor8:
         BINARY(Ity_I8,Ity_I8, Ity_I8);

      case Iop_Add16: case Iop_Sub16: case Iop_Mul16:
      case Iop_Or16:  case Iop_And16: case Iop_Xor16:
         BINARY(Ity_I16,Ity_I16, Ity_I16);

      case Iop_CmpORD32U:
      case Iop_CmpORD32S:
      case Iop_Add32: case Iop_Sub32: case Iop_Mul32:
      case Iop_Or32:  case Iop_And32: case Iop_Xor32:
      case Iop_Max32U:
      case Iop_QAdd32S: case Iop_QSub32S:
      case Iop_Add16x2: case Iop_Sub16x2:
      case Iop_QAdd16Sx2: case Iop_QAdd16Ux2:
      case Iop_QSub16Sx2: case Iop_QSub16Ux2:
      case Iop_HAdd16Ux2: case Iop_HAdd16Sx2:
      case Iop_HSub16Ux2: case Iop_HSub16Sx2:
      case Iop_Add8x4: case Iop_Sub8x4:
      case Iop_QAdd8Sx4: case Iop_QAdd8Ux4:
      case Iop_QSub8Sx4: case Iop_QSub8Ux4:
      case Iop_HAdd8Ux4: case Iop_HAdd8Sx4:
      case Iop_HSub8Ux4: case Iop_HSub8Sx4:
      case Iop_Sad8Ux4:
         BINARY(Ity_I32,Ity_I32, Ity_I32);

      case Iop_Add64: case Iop_Sub64: case Iop_Mul64:
      case Iop_Or64:  case Iop_And64: case Iop_Xor64:
      case Iop_CmpORD64U:
      case Iop_CmpORD64S:
      case Iop_Avg8Ux8: case Iop_Avg16Ux4:
      case Iop_Add8x8: case Iop_Add16x4: case Iop_Add32x2:
      case Iop_Add32Fx2: case Iop_Sub32Fx2:
      case Iop_CmpEQ8x8: case Iop_CmpEQ16x4: case Iop_CmpEQ32x2:
      case Iop_CmpGT8Sx8: case Iop_CmpGT16Sx4: case Iop_CmpGT32Sx2:
      case Iop_CmpGT8Ux8: case Iop_CmpGT16Ux4: case Iop_CmpGT32Ux2:
      case Iop_CmpGT32Fx2: case Iop_CmpEQ32Fx2: case Iop_CmpGE32Fx2:
      case Iop_InterleaveHI8x8: case Iop_InterleaveLO8x8:
      case Iop_InterleaveHI16x4: case Iop_InterleaveLO16x4:
      case Iop_InterleaveHI32x2: case Iop_InterleaveLO32x2:
      case Iop_CatOddLanes8x8: case Iop_CatEvenLanes8x8:
      case Iop_CatOddLanes16x4: case Iop_CatEvenLanes16x4:
      case Iop_InterleaveOddLanes8x8: case Iop_InterleaveEvenLanes8x8:
      case Iop_InterleaveOddLanes16x4: case Iop_InterleaveEvenLanes16x4:
      case Iop_Perm8x8:
      case Iop_Max8Ux8: case Iop_Max16Ux4: case Iop_Max32Ux2:
      case Iop_Max8Sx8: case Iop_Max16Sx4: case Iop_Max32Sx2:
      case Iop_Max32Fx2: case Iop_Min32Fx2:
      case Iop_PwMax32Fx2: case Iop_PwMin32Fx2:
      case Iop_Min8Ux8: case Iop_Min16Ux4: case Iop_Min32Ux2:
      case Iop_Min8Sx8: case Iop_Min16Sx4: case Iop_Min32Sx2:
      case Iop_PwMax8Ux8: case Iop_PwMax16Ux4: case Iop_PwMax32Ux2:
      case Iop_PwMax8Sx8: case Iop_PwMax16Sx4: case Iop_PwMax32Sx2:
      case Iop_PwMin8Ux8: case Iop_PwMin16Ux4: case Iop_PwMin32Ux2:
      case Iop_PwMin8Sx8: case Iop_PwMin16Sx4: case Iop_PwMin32Sx2:
      case Iop_Mul8x8: case Iop_Mul16x4: case Iop_Mul32x2:
      case Iop_Mul32Fx2:
      case Iop_PolynomialMul8x8:
      case Iop_MulHi16Sx4: case Iop_MulHi16Ux4:
      case Iop_QDMulHi16Sx4: case Iop_QDMulHi32Sx2:
      case Iop_QRDMulHi16Sx4: case Iop_QRDMulHi32Sx2:
      case Iop_QAdd8Sx8: case Iop_QAdd16Sx4:
      case Iop_QAdd32Sx2: case Iop_QAdd64Sx1:
      case Iop_QAdd8Ux8: case Iop_QAdd16Ux4:
      case Iop_QAdd32Ux2: case Iop_QAdd64Ux1:
      case Iop_PwAdd8x8: case Iop_PwAdd16x4: case Iop_PwAdd32x2:
      case Iop_PwAdd32Fx2:
      case Iop_QNarrowBin32Sto16Sx4:
      case Iop_QNarrowBin16Sto8Sx8: case Iop_QNarrowBin16Sto8Ux8:
      case Iop_NarrowBin16to8x8: case Iop_NarrowBin32to16x4:
      case Iop_Sub8x8: case Iop_Sub16x4: case Iop_Sub32x2:
      case Iop_QSub8Sx8: case Iop_QSub16Sx4:
      case Iop_QSub32Sx2: case Iop_QSub64Sx1:
      case Iop_QSub8Ux8: case Iop_QSub16Ux4:
      case Iop_QSub32Ux2: case Iop_QSub64Ux1:
      case Iop_Shl8x8: case Iop_Shl16x4: case Iop_Shl32x2:
      case Iop_Shr8x8: case Iop_Shr16x4: case Iop_Shr32x2:
      case Iop_Sar8x8: case Iop_Sar16x4: case Iop_Sar32x2:
      case Iop_Sal8x8: case Iop_Sal16x4: case Iop_Sal32x2: case Iop_Sal64x1:
      case Iop_QShl8x8: case Iop_QShl16x4: case Iop_QShl32x2: case Iop_QShl64x1:
      case Iop_QSal8x8: case Iop_QSal16x4: case Iop_QSal32x2: case Iop_QSal64x1:
      case Iop_Recps32Fx2:
      case Iop_Rsqrts32Fx2:
         BINARY(Ity_I64,Ity_I64, Ity_I64);

      case Iop_ShlN32x2: case Iop_ShlN16x4: case Iop_ShlN8x8:
      case Iop_ShrN32x2: case Iop_ShrN16x4: case Iop_ShrN8x8:
      case Iop_SarN32x2: case Iop_SarN16x4: case Iop_SarN8x8:
      case Iop_QShlN8x8: case Iop_QShlN16x4:
      case Iop_QShlN32x2: case Iop_QShlN64x1:
      case Iop_QShlN8Sx8: case Iop_QShlN16Sx4:
      case Iop_QShlN32Sx2: case Iop_QShlN64Sx1:
      case Iop_QSalN8x8: case Iop_QSalN16x4:
      case Iop_QSalN32x2: case Iop_QSalN64x1:
         BINARY(Ity_I64,Ity_I8, Ity_I64);

      case Iop_Shl8: case Iop_Shr8: case Iop_Sar8:
         BINARY(Ity_I8,Ity_I8, Ity_I8);
      case Iop_Shl16: case Iop_Shr16: case Iop_Sar16:
         BINARY(Ity_I16,Ity_I8, Ity_I16);
      case Iop_Shl32: case Iop_Shr32: case Iop_Sar32:
         BINARY(Ity_I32,Ity_I8, Ity_I32);
      case Iop_Shl64: case Iop_Shr64: case Iop_Sar64:
         BINARY(Ity_I64,Ity_I8, Ity_I64);

      case Iop_Not8:
         UNARY(Ity_I8, Ity_I8);
      case Iop_Not16:
         UNARY(Ity_I16, Ity_I16);
      case Iop_Not32:
      case Iop_CmpNEZ16x2: case Iop_CmpNEZ8x4:
         UNARY(Ity_I32, Ity_I32);

      case Iop_Not64:
      case Iop_CmpNEZ32x2: case Iop_CmpNEZ16x4: case Iop_CmpNEZ8x8:
      case Iop_Cnt8x8:
      case Iop_Clz8Sx8: case Iop_Clz16Sx4: case Iop_Clz32Sx2:
      case Iop_Cls8Sx8: case Iop_Cls16Sx4: case Iop_Cls32Sx2:
      case Iop_PwAddL8Ux8: case Iop_PwAddL16Ux4: case Iop_PwAddL32Ux2:
      case Iop_PwAddL8Sx8: case Iop_PwAddL16Sx4: case Iop_PwAddL32Sx2:
      case Iop_Reverse64_8x8: case Iop_Reverse64_16x4: case Iop_Reverse64_32x2:
      case Iop_Reverse32_8x8: case Iop_Reverse32_16x4:
      case Iop_Reverse16_8x8:
      case Iop_FtoI32Sx2_RZ: case Iop_FtoI32Ux2_RZ:
      case Iop_I32StoFx2: case Iop_I32UtoFx2:
      case Iop_Recip32x2: case Iop_Recip32Fx2:
      case Iop_Abs32Fx2:
      case Iop_Rsqrte32Fx2:
      case Iop_Rsqrte32x2:
      case Iop_Neg32Fx2:
      case Iop_Abs8x8: case Iop_Abs16x4: case Iop_Abs32x2:
         UNARY(Ity_I64, Ity_I64);

      case Iop_CmpEQ8: case Iop_CmpNE8:
      case Iop_CasCmpEQ8: case Iop_CasCmpNE8:
         COMPARISON(Ity_I8);
      case Iop_CmpEQ16: case Iop_CmpNE16:
      case Iop_CasCmpEQ16: case Iop_CasCmpNE16:
         COMPARISON(Ity_I16);
      case Iop_CmpEQ32: case Iop_CmpNE32:
      case Iop_CasCmpEQ32: case Iop_CasCmpNE32:
      case Iop_CmpLT32S: case Iop_CmpLE32S:
      case Iop_CmpLT32U: case Iop_CmpLE32U:
         COMPARISON(Ity_I32);
      case Iop_CmpEQ64: case Iop_CmpNE64:
      case Iop_CasCmpEQ64: case Iop_CasCmpNE64:
      case Iop_CmpLT64S: case Iop_CmpLE64S:
      case Iop_CmpLT64U: case Iop_CmpLE64U:
         COMPARISON(Ity_I64);

      case Iop_CmpNEZ8:  UNARY_COMPARISON(Ity_I8);
      case Iop_CmpNEZ16: UNARY_COMPARISON(Ity_I16);
      case Iop_CmpNEZ32: UNARY_COMPARISON(Ity_I32);
      case Iop_CmpNEZ64: UNARY_COMPARISON(Ity_I64);

      case Iop_Left8:  UNARY(Ity_I8, Ity_I8);
      case Iop_Left16: UNARY(Ity_I16,Ity_I16);
      case Iop_CmpwNEZ32: case Iop_Left32: UNARY(Ity_I32,Ity_I32);
      case Iop_CmpwNEZ64: case Iop_Left64: UNARY(Ity_I64,Ity_I64);

      case Iop_MullU8: case Iop_MullS8:
         BINARY(Ity_I8,Ity_I8, Ity_I16);
      case Iop_MullU16: case Iop_MullS16:
         BINARY(Ity_I16,Ity_I16, Ity_I32);
      case Iop_MullU32: case Iop_MullS32:
         BINARY(Ity_I32,Ity_I32, Ity_I64);
      case Iop_MullU64: case Iop_MullS64:
         BINARY(Ity_I64,Ity_I64, Ity_I128);

      case Iop_Clz32: case Iop_Ctz32:
         UNARY(Ity_I32, Ity_I32);

      case Iop_Clz64: case Iop_Ctz64:
         UNARY(Ity_I64, Ity_I64);

      case Iop_DivU32: case Iop_DivS32: case Iop_DivU32E: case Iop_DivS32E:
         BINARY(Ity_I32,Ity_I32, Ity_I32);

      case Iop_DivU64: case Iop_DivS64: case Iop_DivS64E: case Iop_DivU64E:
         BINARY(Ity_I64,Ity_I64, Ity_I64);

      case Iop_DivModU64to32: case Iop_DivModS64to32:
         BINARY(Ity_I64,Ity_I32, Ity_I64);

      case Iop_DivModU128to64: case Iop_DivModS128to64:
         BINARY(Ity_I128,Ity_I64, Ity_I128);

      case Iop_DivModS64to64:
         BINARY(Ity_I64,Ity_I64, Ity_I128);

      case Iop_16HIto8: case Iop_16to8:
         UNARY(Ity_I16, Ity_I8);
      case Iop_8HLto16:
         BINARY(Ity_I8,Ity_I8, Ity_I16);

      case Iop_32HIto16: case Iop_32to16:
         UNARY(Ity_I32, Ity_I16);
      case Iop_16HLto32:
         BINARY(Ity_I16,Ity_I16, Ity_I32);

      case Iop_64HIto32: case Iop_64to32:
         UNARY(Ity_I64, Ity_I32);
      case Iop_32HLto64:
         BINARY(Ity_I32,Ity_I32, Ity_I64);

      case Iop_128HIto64: case Iop_128to64:
         UNARY(Ity_I128, Ity_I64);
      case Iop_64HLto128:
         BINARY(Ity_I64,Ity_I64, Ity_I128);

      case Iop_Not1:   UNARY(Ity_I1, Ity_I1);
      case Iop_1Uto8:  UNARY(Ity_I1, Ity_I8);
      case Iop_1Sto8:  UNARY(Ity_I1, Ity_I8);
      case Iop_1Sto16: UNARY(Ity_I1, Ity_I16);
      case Iop_1Uto32: case Iop_1Sto32: UNARY(Ity_I1, Ity_I32);
      case Iop_1Sto64: case Iop_1Uto64: UNARY(Ity_I1, Ity_I64);
      case Iop_32to1:  UNARY(Ity_I32, Ity_I1);
      case Iop_64to1:  UNARY(Ity_I64, Ity_I1);

      case Iop_8Uto32: case Iop_8Sto32:
         UNARY(Ity_I8, Ity_I32);

      case Iop_8Uto16: case Iop_8Sto16:
         UNARY(Ity_I8, Ity_I16);

      case Iop_16Uto32: case Iop_16Sto32: 
         UNARY(Ity_I16, Ity_I32);

      case Iop_32Sto64: case Iop_32Uto64:
         UNARY(Ity_I32, Ity_I64);

      case Iop_8Uto64: case Iop_8Sto64:
         UNARY(Ity_I8, Ity_I64);

      case Iop_16Uto64: case Iop_16Sto64:
         UNARY(Ity_I16, Ity_I64);
      case Iop_64to16:
         UNARY(Ity_I64, Ity_I16);

      case Iop_32to8: UNARY(Ity_I32, Ity_I8);
      case Iop_64to8: UNARY(Ity_I64, Ity_I8);

      case Iop_AddF64:    case Iop_SubF64: 
      case Iop_MulF64:    case Iop_DivF64:
      case Iop_AddF64r32: case Iop_SubF64r32: 
      case Iop_MulF64r32: case Iop_DivF64r32:
         TERNARY(ity_RMode,Ity_F64,Ity_F64, Ity_F64);

      case Iop_AddF32: case Iop_SubF32:
      case Iop_MulF32: case Iop_DivF32:
         TERNARY(ity_RMode,Ity_F32,Ity_F32, Ity_F32);

      case Iop_NegF64: case Iop_AbsF64: 
         UNARY(Ity_F64, Ity_F64);

      case Iop_NegF32: case Iop_AbsF32:
         UNARY(Ity_F32, Ity_F32);

      case Iop_SqrtF64:
         BINARY(ity_RMode,Ity_F64, Ity_F64);

      case Iop_SqrtF32:
      case Iop_RoundF32toInt:
         BINARY(ity_RMode,Ity_F32, Ity_F32);

      case Iop_CmpF32:
         BINARY(Ity_F32,Ity_F32, Ity_I32);

      case Iop_CmpF64:
         BINARY(Ity_F64,Ity_F64, Ity_I32);

      case Iop_CmpF128:
         BINARY(Ity_F128,Ity_F128, Ity_I32);

      case Iop_F64toI16S: BINARY(ity_RMode,Ity_F64, Ity_I16);
      case Iop_F64toI32S: BINARY(ity_RMode,Ity_F64, Ity_I32);
      case Iop_F64toI64S: case Iop_F64toI64U:
         BINARY(ity_RMode,Ity_F64, Ity_I64);

      case Iop_F64toI32U: BINARY(ity_RMode,Ity_F64, Ity_I32);

      case Iop_I32StoF64: UNARY(Ity_I32, Ity_F64);
      case Iop_I64StoF64: BINARY(ity_RMode,Ity_I64, Ity_F64);
      case Iop_I64UtoF64: BINARY(ity_RMode,Ity_I64, Ity_F64);
      case Iop_I64UtoF32: BINARY(ity_RMode,Ity_I64, Ity_F32);

      case Iop_I32UtoF64: UNARY(Ity_I32, Ity_F64);

      case Iop_F32toI32S: BINARY(ity_RMode,Ity_F32, Ity_I32);
      case Iop_F32toI64S: BINARY(ity_RMode,Ity_F32, Ity_I64);
      case Iop_F32toI32U: BINARY(ity_RMode,Ity_F32, Ity_I32);
      case Iop_F32toI64U: BINARY(ity_RMode,Ity_F32, Ity_I64);
 
      case Iop_I32UtoF32: BINARY(ity_RMode,Ity_I32, Ity_F32);
      case Iop_I32StoF32: BINARY(ity_RMode,Ity_I32, Ity_F32);
      case Iop_I64StoF32: BINARY(ity_RMode,Ity_I64, Ity_F32);

      case Iop_F32toF64: UNARY(Ity_F32, Ity_F64);
      case Iop_F64toF32: BINARY(ity_RMode,Ity_F64, Ity_F32);

      case Iop_ReinterpI64asF64: UNARY(Ity_I64, Ity_F64);
      case Iop_ReinterpF64asI64: UNARY(Ity_F64, Ity_I64);
      case Iop_ReinterpI32asF32: UNARY(Ity_I32, Ity_F32);
      case Iop_ReinterpF32asI32: UNARY(Ity_F32, Ity_I32);

      case Iop_AtanF64: case Iop_Yl2xF64:  case Iop_Yl2xp1F64: 
      case Iop_ScaleF64: case Iop_PRemF64: case Iop_PRem1F64:
         TERNARY(ity_RMode,Ity_F64,Ity_F64, Ity_F64);

      case Iop_PRemC3210F64: case Iop_PRem1C3210F64:
         TERNARY(ity_RMode,Ity_F64,Ity_F64, Ity_I32);

      case Iop_SinF64: case Iop_CosF64: case Iop_TanF64: 
      case Iop_2xm1F64:
      case Iop_RoundF64toInt: BINARY(ity_RMode,Ity_F64, Ity_F64);

      case Iop_MAddF64: case Iop_MSubF64:
      case Iop_MAddF64r32: case Iop_MSubF64r32:
         QUATERNARY(ity_RMode,Ity_F64,Ity_F64,Ity_F64, Ity_F64);

      case Iop_Est5FRSqrt:
      case Iop_RoundF64toF64_NEAREST: case Iop_RoundF64toF64_NegINF:
      case Iop_RoundF64toF64_PosINF: case Iop_RoundF64toF64_ZERO:
         UNARY(Ity_F64, Ity_F64);
      case Iop_RoundF64toF32:
         BINARY(ity_RMode,Ity_F64, Ity_F64);
      case Iop_TruncF64asF32:
         UNARY(Ity_F64, Ity_F32);

      case Iop_I32UtoFx4:
      case Iop_I32StoFx4:
      case Iop_QFtoI32Ux4_RZ:
      case Iop_QFtoI32Sx4_RZ:
      case Iop_FtoI32Ux4_RZ:
      case Iop_FtoI32Sx4_RZ:
      case Iop_RoundF32x4_RM:
      case Iop_RoundF32x4_RP:
      case Iop_RoundF32x4_RN:
      case Iop_RoundF32x4_RZ:
      case Iop_Abs32Fx4:
      case Iop_Rsqrte32Fx4:
      case Iop_Rsqrte32x4:
         UNARY(Ity_V128, Ity_V128);

      case Iop_64HLtoV128:
         BINARY(Ity_I64,Ity_I64, Ity_V128);

      case Iop_V128to64: case Iop_V128HIto64:
      case Iop_NarrowUn16to8x8:
      case Iop_NarrowUn32to16x4:
      case Iop_NarrowUn64to32x2:
      case Iop_QNarrowUn16Uto8Ux8:
      case Iop_QNarrowUn32Uto16Ux4:
      case Iop_QNarrowUn64Uto32Ux2:
      case Iop_QNarrowUn16Sto8Sx8:
      case Iop_QNarrowUn32Sto16Sx4:
      case Iop_QNarrowUn64Sto32Sx2:
      case Iop_QNarrowUn16Sto8Ux8:
      case Iop_QNarrowUn32Sto16Ux4:
      case Iop_QNarrowUn64Sto32Ux2:
      case Iop_F32toF16x4:
         UNARY(Ity_V128, Ity_I64);

      case Iop_Widen8Uto16x8:
      case Iop_Widen16Uto32x4:
      case Iop_Widen32Uto64x2:
      case Iop_Widen8Sto16x8:
      case Iop_Widen16Sto32x4:
      case Iop_Widen32Sto64x2:
      case Iop_F16toF32x4:
         UNARY(Ity_I64, Ity_V128);

      case Iop_V128to32:    UNARY(Ity_V128, Ity_I32);
      case Iop_32UtoV128:   UNARY(Ity_I32, Ity_V128);
      case Iop_64UtoV128:   UNARY(Ity_I64, Ity_V128);
      case Iop_SetV128lo32: BINARY(Ity_V128,Ity_I32, Ity_V128);
      case Iop_SetV128lo64: BINARY(Ity_V128,Ity_I64, Ity_V128);

      case Iop_Dup8x16: UNARY(Ity_I8, Ity_V128);
      case Iop_Dup16x8: UNARY(Ity_I16, Ity_V128);
      case Iop_Dup32x4: UNARY(Ity_I32, Ity_V128);
      case Iop_Dup8x8:  UNARY(Ity_I8, Ity_I64);
      case Iop_Dup16x4: UNARY(Ity_I16, Ity_I64);
      case Iop_Dup32x2: UNARY(Ity_I32, Ity_I64);

      case Iop_CmpEQ32Fx4: case Iop_CmpLT32Fx4:
      case Iop_CmpEQ64Fx2: case Iop_CmpLT64Fx2:
      case Iop_CmpLE32Fx4: case Iop_CmpUN32Fx4:
      case Iop_CmpLE64Fx2: case Iop_CmpUN64Fx2:
      case Iop_CmpGT32Fx4: case Iop_CmpGE32Fx4:
      case Iop_CmpEQ32F0x4: case Iop_CmpLT32F0x4:
      case Iop_CmpEQ64F0x2: case Iop_CmpLT64F0x2:
      case Iop_CmpLE32F0x4: case Iop_CmpUN32F0x4:
      case Iop_CmpLE64F0x2: case Iop_CmpUN64F0x2:
      case Iop_Add32Fx4: case Iop_Add32F0x4:
      case Iop_Add64Fx2: case Iop_Add64F0x2:
      case Iop_Div32Fx4: case Iop_Div32F0x4:
      case Iop_Div64Fx2: case Iop_Div64F0x2:
      case Iop_Max32Fx4: case Iop_Max32F0x4:
      case Iop_PwMax32Fx4: case Iop_PwMin32Fx4:
      case Iop_Max64Fx2: case Iop_Max64F0x2:
      case Iop_Min32Fx4: case Iop_Min32F0x4:
      case Iop_Min64Fx2: case Iop_Min64F0x2:
      case Iop_Mul32Fx4: case Iop_Mul32F0x4:
      case Iop_Mul64Fx2: case Iop_Mul64F0x2:
      case Iop_Sub32Fx4: case Iop_Sub32F0x4:
      case Iop_Sub64Fx2: case Iop_Sub64F0x2:
      case Iop_AndV128: case Iop_OrV128: case Iop_XorV128:
      case Iop_Add8x16:   case Iop_Add16x8:   
      case Iop_Add32x4:   case Iop_Add64x2:
      case Iop_QAdd8Ux16: case Iop_QAdd16Ux8:
      case Iop_QAdd32Ux4: //case Iop_QAdd64Ux2:
      case Iop_QAdd8Sx16: case Iop_QAdd16Sx8:
      case Iop_QAdd32Sx4: case Iop_QAdd64Sx2:
      case Iop_PwAdd8x16: case Iop_PwAdd16x8: case Iop_PwAdd32x4:
      case Iop_Sub8x16:   case Iop_Sub16x8:
      case Iop_Sub32x4:   case Iop_Sub64x2:
      case Iop_QSub8Ux16: case Iop_QSub16Ux8:
      case Iop_QSub32Ux4: //case Iop_QSub64Ux2:
      case Iop_QSub8Sx16: case Iop_QSub16Sx8:
      case Iop_QSub32Sx4: case Iop_QSub64Sx2:
      case Iop_Mul8x16: case Iop_Mul16x8: case Iop_Mul32x4:
      case Iop_PolynomialMul8x16:
      case Iop_MulHi16Ux8: case Iop_MulHi32Ux4: 
      case Iop_MulHi16Sx8: case Iop_MulHi32Sx4: 
      case Iop_QDMulHi16Sx8: case Iop_QDMulHi32Sx4:
      case Iop_QRDMulHi16Sx8: case Iop_QRDMulHi32Sx4:
      case Iop_MullEven8Ux16: case Iop_MullEven16Ux8:
      case Iop_MullEven8Sx16: case Iop_MullEven16Sx8:
      case Iop_Avg8Ux16: case Iop_Avg16Ux8: case Iop_Avg32Ux4:
      case Iop_Avg8Sx16: case Iop_Avg16Sx8: case Iop_Avg32Sx4:
      case Iop_Max8Sx16: case Iop_Max16Sx8: case Iop_Max32Sx4:
      case Iop_Max8Ux16: case Iop_Max16Ux8: case Iop_Max32Ux4:
      case Iop_Min8Sx16: case Iop_Min16Sx8: case Iop_Min32Sx4:
      case Iop_Min8Ux16: case Iop_Min16Ux8: case Iop_Min32Ux4:
      case Iop_CmpEQ8x16:  case Iop_CmpEQ16x8:  case Iop_CmpEQ32x4:
      case Iop_CmpEQ64x2:
      case Iop_CmpGT8Sx16: case Iop_CmpGT16Sx8: case Iop_CmpGT32Sx4:
      case Iop_CmpGT64Sx2:
      case Iop_CmpGT8Ux16: case Iop_CmpGT16Ux8: case Iop_CmpGT32Ux4:
      case Iop_Shl8x16: case Iop_Shl16x8: case Iop_Shl32x4: case Iop_Shl64x2:
      case Iop_QShl8x16: case Iop_QShl16x8:
      case Iop_QShl32x4: case Iop_QShl64x2:
      case Iop_QSal8x16: case Iop_QSal16x8:
      case Iop_QSal32x4: case Iop_QSal64x2:
      case Iop_Shr8x16: case Iop_Shr16x8: case Iop_Shr32x4: case Iop_Shr64x2:
      case Iop_Sar8x16: case Iop_Sar16x8: case Iop_Sar32x4: case Iop_Sar64x2:
      case Iop_Sal8x16: case Iop_Sal16x8: case Iop_Sal32x4: case Iop_Sal64x2:
      case Iop_Rol8x16: case Iop_Rol16x8: case Iop_Rol32x4:
      case Iop_QNarrowBin16Sto8Ux16: case Iop_QNarrowBin32Sto16Ux8:
      case Iop_QNarrowBin16Sto8Sx16: case Iop_QNarrowBin32Sto16Sx8:
      case Iop_QNarrowBin16Uto8Ux16: case Iop_QNarrowBin32Uto16Ux8:
      case Iop_NarrowBin16to8x16:   case Iop_NarrowBin32to16x8:
      case Iop_InterleaveHI8x16: case Iop_InterleaveHI16x8:
      case Iop_InterleaveHI32x4: case Iop_InterleaveHI64x2:
      case Iop_InterleaveLO8x16: case Iop_InterleaveLO16x8:
      case Iop_InterleaveLO32x4: case Iop_InterleaveLO64x2:
      case Iop_CatOddLanes8x16: case Iop_CatEvenLanes8x16:
      case Iop_CatOddLanes16x8: case Iop_CatEvenLanes16x8:
      case Iop_CatOddLanes32x4: case Iop_CatEvenLanes32x4:
      case Iop_InterleaveOddLanes8x16: case Iop_InterleaveEvenLanes8x16:
      case Iop_InterleaveOddLanes16x8: case Iop_InterleaveEvenLanes16x8:
      case Iop_InterleaveOddLanes32x4: case Iop_InterleaveEvenLanes32x4:
      case Iop_Perm8x16: case Iop_Perm32x4:
      case Iop_Recps32Fx4:
      case Iop_Rsqrts32Fx4:
         BINARY(Ity_V128,Ity_V128, Ity_V128);

      case Iop_PolynomialMull8x8:
      case Iop_Mull8Ux8: case Iop_Mull8Sx8:
      case Iop_Mull16Ux4: case Iop_Mull16Sx4:
      case Iop_Mull32Ux2: case Iop_Mull32Sx2:
         BINARY(Ity_I64, Ity_I64, Ity_V128);

      case Iop_NotV128:
      case Iop_Recip32Fx4: case Iop_Recip32F0x4:
      case Iop_Recip32x4:
      case Iop_Recip64Fx2: case Iop_Recip64F0x2:
      case Iop_RSqrt32Fx4: case Iop_RSqrt32F0x4:
      case Iop_RSqrt64Fx2: case Iop_RSqrt64F0x2:
      case Iop_Sqrt32Fx4:  case Iop_Sqrt32F0x4:
      case Iop_Sqrt64Fx2:  case Iop_Sqrt64F0x2:
      case Iop_CmpNEZ8x16: case Iop_CmpNEZ16x8:
      case Iop_CmpNEZ32x4: case Iop_CmpNEZ64x2:
      case Iop_Cnt8x16:
      case Iop_Clz8Sx16: case Iop_Clz16Sx8: case Iop_Clz32Sx4:
      case Iop_Cls8Sx16: case Iop_Cls16Sx8: case Iop_Cls32Sx4:
      case Iop_PwAddL8Ux16: case Iop_PwAddL16Ux8: case Iop_PwAddL32Ux4:
      case Iop_PwAddL8Sx16: case Iop_PwAddL16Sx8: case Iop_PwAddL32Sx4:
      case Iop_Reverse64_8x16: case Iop_Reverse64_16x8: case Iop_Reverse64_32x4:
      case Iop_Reverse32_8x16: case Iop_Reverse32_16x8:
      case Iop_Reverse16_8x16:
      case Iop_Neg32Fx4:
      case Iop_Abs8x16: case Iop_Abs16x8: case Iop_Abs32x4:
         UNARY(Ity_V128, Ity_V128);

      case Iop_ShlV128: case Iop_ShrV128:
      case Iop_ShlN8x16: case Iop_ShlN16x8: 
      case Iop_ShlN32x4: case Iop_ShlN64x2:
      case Iop_ShrN8x16: case Iop_ShrN16x8: 
      case Iop_ShrN32x4: case Iop_ShrN64x2:
      case Iop_SarN8x16: case Iop_SarN16x8:
      case Iop_SarN32x4: case Iop_SarN64x2:
      case Iop_QShlN8x16: case Iop_QShlN16x8:
      case Iop_QShlN32x4: case Iop_QShlN64x2:
      case Iop_QShlN8Sx16: case Iop_QShlN16Sx8:
      case Iop_QShlN32Sx4: case Iop_QShlN64Sx2:
      case Iop_QSalN8x16: case Iop_QSalN16x8:
      case Iop_QSalN32x4: case Iop_QSalN64x2:
         BINARY(Ity_V128,Ity_I8, Ity_V128);

      case Iop_F32ToFixed32Ux4_RZ:
      case Iop_F32ToFixed32Sx4_RZ:
      case Iop_Fixed32UToF32x4_RN:
      case Iop_Fixed32SToF32x4_RN:
         BINARY(Ity_V128, Ity_I8, Ity_V128);

      case Iop_F32ToFixed32Ux2_RZ:
      case Iop_F32ToFixed32Sx2_RZ:
      case Iop_Fixed32UToF32x2_RN:
      case Iop_Fixed32SToF32x2_RN:
         BINARY(Ity_I64, Ity_I8, Ity_I64);

      case Iop_GetElem8x16:
         BINARY(Ity_V128, Ity_I8, Ity_I8);
      case Iop_GetElem16x8:
         BINARY(Ity_V128, Ity_I8, Ity_I16);
      case Iop_GetElem32x4:
         BINARY(Ity_V128, Ity_I8, Ity_I32);
      case Iop_GetElem64x2:
         BINARY(Ity_V128, Ity_I8, Ity_I64);
      case Iop_GetElem8x8:
         BINARY(Ity_I64, Ity_I8, Ity_I8);
      case Iop_GetElem16x4:
         BINARY(Ity_I64, Ity_I8, Ity_I16);
      case Iop_GetElem32x2:
         BINARY(Ity_I64, Ity_I8, Ity_I32);
      case Iop_SetElem8x8:
         TERNARY(Ity_I64, Ity_I8, Ity_I8, Ity_I64);
      case Iop_SetElem16x4:
         TERNARY(Ity_I64, Ity_I8, Ity_I16, Ity_I64);
      case Iop_SetElem32x2:
         TERNARY(Ity_I64, Ity_I8, Ity_I32, Ity_I64);

      case Iop_Extract64:
         TERNARY(Ity_I64, Ity_I64, Ity_I8, Ity_I64);
      case Iop_ExtractV128:
         TERNARY(Ity_V128, Ity_V128, Ity_I8, Ity_V128);

      case Iop_QDMulLong16Sx4: case Iop_QDMulLong32Sx2:
         BINARY(Ity_I64, Ity_I64, Ity_V128);

         /* s390 specific */
      case Iop_MAddF32:
      case Iop_MSubF32:
         QUATERNARY(ity_RMode,Ity_F32,Ity_F32,Ity_F32, Ity_F32);

      case Iop_F64HLtoF128:
        BINARY(Ity_F64,Ity_F64, Ity_F128);

      case Iop_F128HItoF64:
      case Iop_F128LOtoF64:
        UNARY(Ity_F128, Ity_F64);

      case Iop_AddF128:
      case Iop_SubF128:
      case Iop_MulF128:
      case Iop_DivF128:
         TERNARY(ity_RMode,Ity_F128,Ity_F128, Ity_F128);

      case Iop_NegF128:
      case Iop_AbsF128:
         UNARY(Ity_F128, Ity_F128);

      case Iop_SqrtF128:
         BINARY(ity_RMode,Ity_F128, Ity_F128);

      case Iop_I32StoF128: UNARY(Ity_I32, Ity_F128);
      case Iop_I64StoF128: UNARY(Ity_I64, Ity_F128);

      case Iop_I32UtoF128: UNARY(Ity_I32, Ity_F128);
      case Iop_I64UtoF128: UNARY(Ity_I64, Ity_F128);

      case Iop_F128toI32S: BINARY(ity_RMode,Ity_F128, Ity_I32);
      case Iop_F128toI64S: BINARY(ity_RMode,Ity_F128, Ity_I64);

      case Iop_F128toI32U: BINARY(ity_RMode,Ity_F128, Ity_I32);
      case Iop_F128toI64U: BINARY(ity_RMode,Ity_F128, Ity_I64);

      case Iop_F32toF128: UNARY(Ity_F32, Ity_F128);
      case Iop_F64toF128: UNARY(Ity_F64, Ity_F128);

      case Iop_F128toF32: BINARY(ity_RMode,Ity_F128, Ity_F32);
      case Iop_F128toF64: BINARY(ity_RMode,Ity_F128, Ity_F64);

      case Iop_D32toD64:
         UNARY(Ity_D32, Ity_D64);

      case Iop_ExtractExpD64:
         UNARY(Ity_D64, Ity_I64);

      case Iop_ExtractSigD64:
         UNARY(Ity_D64, Ity_I64);

      case Iop_InsertExpD64:
         BINARY(Ity_I64,Ity_D64, Ity_D64);

      case Iop_ExtractExpD128:
         UNARY(Ity_D128, Ity_I64);

      case Iop_ExtractSigD128:
         UNARY(Ity_D128, Ity_I64);

      case Iop_InsertExpD128:
         BINARY(Ity_I64,Ity_D128, Ity_D128);

      case Iop_D64toD128:
         UNARY(Ity_D64, Ity_D128);

      case Iop_ReinterpD64asI64:
	UNARY(Ity_D64, Ity_I64);

      case Iop_ReinterpI64asD64:
         UNARY(Ity_I64, Ity_D64);

      case Iop_RoundD64toInt:
         BINARY(ity_RMode,Ity_D64, Ity_D64);

      case Iop_RoundD128toInt:
         BINARY(ity_RMode,Ity_D128, Ity_D128);

      case Iop_I32StoD128:
      case Iop_I32UtoD128:
         UNARY(Ity_I32, Ity_D128);

      case Iop_I64StoD128:
         UNARY(Ity_I64, Ity_D128);

      case Iop_I64UtoD128:
         UNARY(Ity_I64, Ity_D128);

      case Iop_F32toD32:
         BINARY(ity_RMode, Ity_F32, Ity_D32);

      case Iop_F32toD64:
         BINARY(ity_RMode, Ity_F32, Ity_D64);

      case Iop_F32toD128:
         BINARY(ity_RMode, Ity_F32, Ity_D128);

      case Iop_F64toD32:
         BINARY(ity_RMode, Ity_F64, Ity_D32);

      case Iop_F64toD64:
         BINARY(ity_RMode, Ity_F64, Ity_D64);

      case Iop_F64toD128:
         BINARY(ity_RMode, Ity_F64, Ity_D128);

      case Iop_F128toD32:
         BINARY(ity_RMode, Ity_F128, Ity_D32);

      case Iop_F128toD64:
         BINARY(ity_RMode, Ity_F128, Ity_D64);

      case Iop_F128toD128:
         BINARY(ity_RMode, Ity_F128, Ity_D128);

      case Iop_D32toF32:
         BINARY(ity_RMode, Ity_D32, Ity_F32);

      case Iop_D32toF64:
         BINARY(ity_RMode, Ity_D32, Ity_F64);

      case Iop_D32toF128:
         BINARY(ity_RMode, Ity_D32, Ity_F128);

      case Iop_D64toF32:
         BINARY(ity_RMode, Ity_D64, Ity_F32);

      case Iop_D64toF64:
         BINARY(ity_RMode, Ity_D64, Ity_F64);

      case Iop_D64toF128:
         BINARY(ity_RMode, Ity_D64, Ity_F128);

      case Iop_D128toF32:
         BINARY(ity_RMode, Ity_D128, Ity_F32);

      case Iop_D128toF64:
         BINARY(ity_RMode, Ity_D128, Ity_F64);

      case Iop_D128toF128:
         BINARY(ity_RMode, Ity_D128, Ity_F128);

      case Iop_DPBtoBCD:
      case Iop_BCDtoDPB:
         UNARY(Ity_I64, Ity_I64);

      case Iop_D128HItoD64:
      case Iop_D128LOtoD64:
         UNARY(Ity_D128, Ity_D64);

      case Iop_D128toI32S:
      case Iop_D128toI32U:
         BINARY(ity_RMode, Ity_D128, Ity_I32);

      case Iop_D128toI64S:
         BINARY(ity_RMode, Ity_D128, Ity_I64);

      case Iop_D128toI64U:
         BINARY(ity_RMode, Ity_D128, Ity_I64);

      case Iop_D64HLtoD128:
         BINARY(Ity_D64, Ity_D64, Ity_D128);

      case Iop_ShlD64:
      case Iop_ShrD64:
         BINARY(Ity_D64, Ity_I8, Ity_D64 );

      case Iop_D64toD32:
         BINARY(ity_RMode, Ity_D64, Ity_D32);

      case Iop_D64toI32S:
      case Iop_D64toI32U:
         BINARY(ity_RMode, Ity_D64, Ity_I32);

      case Iop_D64toI64S:
      case Iop_D64toI64U:
         BINARY(ity_RMode, Ity_D64, Ity_I64);

      case Iop_I32StoD64:
      case Iop_I32UtoD64:
         UNARY(Ity_I32, Ity_D64);

      case Iop_I64StoD64:
      case Iop_I64UtoD64:
         BINARY(ity_RMode, Ity_I64, Ity_D64);

      case Iop_CmpD64:
      case Iop_CmpExpD64:
         BINARY(Ity_D64,Ity_D64, Ity_I32);

      case Iop_CmpD128:
      case Iop_CmpExpD128:
         BINARY(Ity_D128,Ity_D128, Ity_I32);

      case Iop_QuantizeD64:
         TERNARY(ity_RMode,Ity_D64,Ity_D64, Ity_D64);

      case Iop_SignificanceRoundD64:
         TERNARY(ity_RMode,Ity_I8,Ity_D64, Ity_D64);

      case Iop_QuantizeD128:
         TERNARY(ity_RMode,Ity_D128,Ity_D128, Ity_D128);

      case Iop_SignificanceRoundD128:
         TERNARY(ity_RMode,Ity_I8,Ity_D128, Ity_D128);

      case Iop_ShlD128:
      case Iop_ShrD128:
         BINARY(Ity_D128, Ity_I8, Ity_D128 );

      case Iop_AddD64:
      case Iop_SubD64:
      case Iop_MulD64:
      case Iop_DivD64:
         TERNARY( ity_RMode, Ity_D64, Ity_D64, Ity_D64 );

      case Iop_D128toD64:
         BINARY( ity_RMode, Ity_D128, Ity_D64 );

      case Iop_AddD128:
      case Iop_SubD128:
      case Iop_MulD128:
      case Iop_DivD128:
         TERNARY(ity_RMode,Ity_D128,Ity_D128, Ity_D128);

      case Iop_V256to64_0: case Iop_V256to64_1:
      case Iop_V256to64_2: case Iop_V256to64_3:
         UNARY(Ity_V256, Ity_I64);

      case Iop_64x4toV256:
         QUATERNARY(Ity_I64, Ity_I64, Ity_I64, Ity_I64, Ity_V256);

      case Iop_Add64Fx4: case Iop_Sub64Fx4:
      case Iop_Mul64Fx4: case Iop_Div64Fx4:
      case Iop_Add32Fx8: case Iop_Sub32Fx8:
      case Iop_Mul32Fx8: case Iop_Div32Fx8:
      case Iop_AndV256:  case Iop_OrV256:
      case Iop_XorV256:
      case Iop_Max32Fx8: case Iop_Min32Fx8:
      case Iop_Max64Fx4: case Iop_Min64Fx4:
         BINARY(Ity_V256,Ity_V256, Ity_V256);

      case Iop_V256toV128_1: case Iop_V256toV128_0:
         UNARY(Ity_V256, Ity_V128);

      case Iop_V128HLtoV256:
         BINARY(Ity_V128,Ity_V128, Ity_V256);

      case Iop_NotV256:
      case Iop_RSqrt32Fx8:
      case Iop_Sqrt32Fx8:
      case Iop_Sqrt64Fx4:
      case Iop_Recip32Fx8:
      case Iop_CmpNEZ64x4: case Iop_CmpNEZ32x8:
         UNARY(Ity_V256, Ity_V256);

      default:
         panic(__func__);
   }
#  undef UNARY
#  undef BINARY
#  undef TERNARY
#  undef COMPARISON
#  undef UNARY_COMPARISON
}
