
/*--------------------------------------------------------------------*/
/*--- Handle s390x-specific extensions.          extension-s390x.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) IBM Corp. 2024

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

/* Contributed by Andreas Arnez */

#if defined(VGP_s390x_linux)

#include "libvex_s390x_common.h"
#include "priv_extension.h"
#include "priv_types_n_macros.h"
#include "pub_core_libcassert.h"
#include "pub_core_threadstate.h"

#undef SYSNO

#define READ_FUNCTION_CODE(tst, extname)                                       \
   ({                                                                          \
      PRE_REG_READ(tst, extname "(func_code)", r0, 7, sizeof(UChar));          \
      tst->arch.vex.guest_r0 & 0xff;                                           \
   })

#define READ_GPR(tst, name, regno)                                             \
   ({                                                                          \
      PRE_REG_READ(tst, name, r0, sizeof(ULong) * (regno), sizeof(ULong));     \
      *((&tst->arch.vex.guest_r0) + (regno));                                  \
   })

#define WRITE_GPR(tst, regno, value)                                           \
   ({                                                                          \
      *((&tst->arch.vex.guest_r0) + (regno)) = value;                          \
      POST_REG_WRITE(tst, r0, sizeof(ULong) * (regno), sizeof(ULong));         \
   })

#define S390_CC_OP_SET 35

#define WRITE_CC(tst, value)                                                   \
   ({                                                                          \
      tst->arch.vex.guest_CC_OP   = S390_CC_OP_SET;                            \
      tst->arch.vex.guest_CC_DEP1 = value;                                     \
      tst->arch.vex.guest_CC_DEP2 = 0;                                         \
      tst->arch.vex.guest_CC_NDEP = 0;                                         \
      POST_REG_WRITE(tst, CC_OP, 0, sizeof(ULong) * 4);                        \
   })

#define INSN_ERR(msg)                                                          \
   ({                                                                          \
      VG_(umsg)("Illegal operation: %s", msg);                                 \
      ExtErr_Illop;                                                            \
   })

union reg_pair {
   struct {
      ULong a, b;
   };
   unsigned __int128 pair;
};

/* To avoid code duplication, provide macros for generating inline assembly
   functions where applicable. */

#define S390_DEFINE_DO_RRR_INSN(fname, opc)                                    \
   static Int fname(ULong func, ULong parms, ULong* addr1, ULong* addr2,       \
                    ULong* len2, ULong* addr3, ULong* len3)                    \
   {                                                                           \
      union reg_pair op1           = {{*addr1, 0}};                            \
      union reg_pair op2           = {{*addr2, *len2}};                        \
      union reg_pair op3           = {{*addr3, *len3}};                        \
      register ULong reg0 asm("0") = func;                                     \
      register void* reg1 asm("1") = (void*)parms;                             \
      UInt           cc;                                                       \
                                                                               \
      asm volatile(".insn rrf, " #opc "0000, %[op1], %[op2], %[op3], 0\n"      \
                   "ipm %[cc]\n"                                               \
                   : [cc] "=d"(cc), [op1] "+a"(op1.pair),                      \
                     [op2] "+a"(op2.pair), [op3] "+a"(op3.pair)                \
                   : "d"(reg0), "d"(reg1)                                      \
                   : "cc", "memory");                                          \
      *addr1 = op1.a;                                                          \
      *addr2 = op2.a;                                                          \
      *len2  = op2.b;                                                          \
      *addr3 = op3.a;                                                          \
      *len3  = op3.b;                                                          \
      return cc >> 28;                                                         \
   }

#define S390_DEFINE_DO_RR_INSN(fname, opc)                                     \
   static Int fname(ULong func, ULong parms, ULong* addr1, ULong* len1,        \
                    ULong* addr2, ULong* len2)                                 \
   {                                                                           \
      union reg_pair op1           = {{*addr1, *len1}};                        \
      union reg_pair op2           = {{*addr2, *len2}};                        \
      register ULong reg0 asm("0") = func;                                     \
      register void* reg1 asm("1") = (void*)parms;                             \
      UInt           cc;                                                       \
                                                                               \
      asm volatile(".insn rre, " #opc "0000, %[op1], %[op2]\n"                 \
                   "ipm %[cc]\n"                                               \
                   : [cc] "=d"(cc), [op1] "+a"(op1.pair), [op2] "+a"(op2.pair) \
                   : "d"(reg0), "d"(reg1)                                      \
                   : "cc", "memory");                                          \
      *addr1 = op1.a;                                                          \
      *len1  = op1.b;                                                          \
      *addr2 = op2.a;                                                          \
      *len2  = op2.b;                                                          \
      return cc >> 28;                                                         \
   }

#define S390_DEFINE_DO_0R_INSN(fname, opc)                                     \
   static Int fname(ULong func, ULong parms, ULong* addr2, ULong* len2)        \
   {                                                                           \
      union reg_pair op2           = {{*addr2, *len2}};                        \
      register ULong reg0 asm("0") = func;                                     \
      register void* reg1 asm("1") = (void*)parms;                             \
      UInt           cc;                                                       \
                                                                               \
      asm volatile(".insn rre, " #opc "0000, 0, %[op2]\n"                      \
                   "ipm %[cc]\n"                                               \
                   : [cc] "=d"(cc), [op2] "+a"(op2.pair)                       \
                   : "d"(reg0), "d"(reg1)                                      \
                   : "cc", "memory");                                          \
      *addr2 = op2.a;                                                          \
      *len2  = op2.b;                                                          \
      return cc >> 28;                                                         \
   }

#define S390_DEFINE_DO_00_INSN(fname, opc)                                     \
   static Int fname(ULong func, ULong parms)                                   \
   {                                                                           \
      register ULong reg0 asm("0") = func;                                     \
      register void* reg1 asm("1") = (void*)parms;                             \
      UInt           cc;                                                       \
                                                                               \
      asm volatile(".insn rre, " #opc "0000, 0, 0\n"                           \
                   "ipm %[cc]\n"                                               \
                   : [cc] "=d"(cc)                                             \
                   : "d"(reg0), "d"(reg1)                                      \
                   : "cc", "memory");                                          \
      return cc >> 28;                                                         \
   }

#define S390_SETBIT(x)       (1UL << (63 - (x % 64)))
#define S390_SETBITS(lo, hi) (((1UL << (hi + 1 - lo)) - 1) << (63 - (hi % 64)))

/* Helper routine for query functions: Filter the bit vector `fc' using a given
   `filter' vector */
static void s390_filter_functions(ULong*       fc,
                                  ULong        fc_len,
                                  const ULong* filter,
                                  ULong        filter_len)
{
   ULong n_fc     = fc_len / sizeof(ULong);
   ULong n_filter = filter_len / sizeof(ULong);

   for (ULong i = 0; i < n_fc; i++) {
      if (i < n_filter)
         fc[i] &= filter[i];
      else
         fc[i] = 0;
   }
}

/*---------------------------------------------------------------*/
/*--- PRNO (perform random number operation)                  ---*/
/*---------------------------------------------------------------*/

S390_DEFINE_DO_RR_INSN(do_PRNO_insn, 0xb93c)

/* PRNO functions that we support if the hardware does. */
static const ULong PRNO_functions[] = {
   (S390_SETBIT(0)       // Query
    | S390_SETBIT(3)),   // SHA-512-DRNG
   (S390_SETBIT(112)     // TRNG-Query-Raw-to-Conditioned-Ratio
    | S390_SETBIT(114)), // TRNG
};

static enum ExtensionError do_extension_PRNO(ThreadState* tst, ULong variant)
{
   UChar r1    = variant & 0xf;
   UChar r2    = (variant >> 4) & 0xf;
   UChar func  = READ_FUNCTION_CODE(tst, "PRNO");
   UChar fc    = func & 0x7f;
   UChar mflag = func & 128;
   ULong parms = READ_GPR(tst, "PRNO(r1)", 1);
   ULong parms_len;
   Int   cc         = 0;
   ULong orig_addr1 = 0, orig_len1 = 0, orig_addr2 = 0, orig_len2 = 0;
   ULong addr1 = 0, len1 = 0, addr2 = 0, len2 = 0;

   switch (fc) {
   case 0: // Query
      parms_len = 16;
      PRE_MEM_WRITE(tst, "PRNO(parms)", parms, parms_len);
      cc = do_PRNO_insn(func, parms, &addr1, &len1, &addr2, &len2);
      s390_filter_functions((ULong*)parms, parms_len, PRNO_functions,
                            sizeof(PRNO_functions));
      POST_MEM_WRITE(tst, parms, parms_len);
      break;
   case 112: // TRNG-Query-Raw-to-Conditioned-Ratio
      parms_len = 8;
      PRE_MEM_WRITE(tst, "PRNO(parms)", parms, parms_len);
      cc = do_PRNO_insn(func, parms, &addr1, &len1, &addr2, &len2);
      POST_MEM_WRITE(tst, parms, parms_len);
      break;
   case 3: // SHA-512-DRNG
      parms_len = 240;
      PRE_MEM_READ(tst, "PRNO(parms)", parms, parms_len);
      if (mflag == 0) {
         // Generate operation
         addr1 = orig_addr1 = READ_GPR(tst, "PRNO(op1_addr)", r1);
         len1 = orig_len1 = READ_GPR(tst, "PRNO(op1_len)", r1 + 1);
         PRE_MEM_WRITE(tst, "PRNO(op1)", addr1, len1);
      } else {
         // Seed operation
         addr2 = READ_GPR(tst, "PRNO(op2_addr)", r2);
         len2  = READ_GPR(tst, "PRNO(op2_len)", r2 + 1);
         PRE_MEM_READ(tst, "PRNO(op2)", addr2, len2);
      }
      PRE_MEM_WRITE(tst, "PRNO(parms)", parms, parms_len);
      cc = do_PRNO_insn(func, parms, &addr1, &len1, &addr2, &len2);
      POST_MEM_WRITE(tst, parms, parms_len);
      if (mflag == 0) {
         WRITE_GPR(tst, r1 + 1, len1);
         // The operand is filled from right to left
         POST_MEM_WRITE(tst, orig_addr1 + len1, orig_len1 - len1);
      }
      break;
   case 114: // TRNG
      addr1 = orig_addr1 = READ_GPR(tst, "PRNO(op1_addr)", r1);
      len1 = orig_len1 = READ_GPR(tst, "PRNO(op1_len)", r1 + 1);
      PRE_MEM_WRITE(tst, "PRNO(op1)", addr1, len1);
      addr2 = orig_addr2 = READ_GPR(tst, "PRNO(op2_addr)", r2);
      len2 = orig_len2 = READ_GPR(tst, "PRNO(op2_len)", r2 + 1);
      PRE_MEM_WRITE(tst, "PRNO(op2)", addr2, len2);
      cc = do_PRNO_insn(func, parms, &addr1, &len1, &addr2, &len2);
      WRITE_GPR(tst, r1, addr1);
      WRITE_GPR(tst, r1 + 1, len1);
      WRITE_GPR(tst, r2, addr2);
      WRITE_GPR(tst, r2 + 1, len2);
      POST_MEM_WRITE(tst, orig_addr1, orig_len1 - len1);
      POST_MEM_WRITE(tst, orig_addr2, orig_len2 - len2);
      break;
   default:
      return INSN_ERR("PRNO: unknown function code\n");
   }
   WRITE_CC(tst, cc);
   return ExtErr_OK;
}

/*---------------------------------------------------------------*/
/*--- NNPA (neural network processing assist)                 ---*/
/*---------------------------------------------------------------*/

static Int do_NNPA_insn(ULong* gpr0, ULong parms)
{
   register ULong reg0 asm("0") = *gpr0;
   register void* reg1 asm("1") = (void*)parms;
   Int            cc;

   asm volatile(".insn rre, 0xb93b0000, 0, 0\n"
                "ipm %[cc]\n"
                "srl %[cc], 28\n"
                : [cc] "=d"(cc), "+d"(reg0)
                : "d"(reg1)
                : "cc", "memory");
   *gpr0 = reg0;
   return cc;
}

/* NNPA function codes */
typedef enum {
   S390_NNPA_QAF               = 0x00,
   S390_NNPA_ADD               = 0x10,
   S390_NNPA_SUB               = 0x11,
   S390_NNPA_MUL               = 0x12,
   S390_NNPA_DIV               = 0x13,
   S390_NNPA_MIN               = 0x14,
   S390_NNPA_MAX               = 0x15,
   S390_NNPA_LOG               = 0x20,
   S390_NNPA_EXP               = 0x21,
   S390_NNPA_SQRT              = 0x22,
   S390_NNPA_INVSQRT           = 0x23,
   S390_NNPA_RELU              = 0x31,
   S390_NNPA_TANH              = 0x32,
   S390_NNPA_SIGMOID           = 0x33,
   S390_NNPA_SOFTMAX           = 0x34,
   S390_NNPA_GELU              = 0x35,
   S390_NNPA_BATCHNORM         = 0x40,
   S390_NNPA_MOMENTS           = 0x41,
   S390_NNPA_LAYERNORM         = 0x42,
   S390_NNPA_NORM              = 0x43,
   S390_NNPA_MAXPOOL2D         = 0x50,
   S390_NNPA_AVGPOOL2D         = 0x51,
   S390_NNPA_LSTMACT           = 0x60,
   S390_NNPA_GRUACT            = 0x61,
   S390_NNPA_CONVOLUTION       = 0x70,
   S390_NNPA_MATMUL_OP         = 0x71,
   S390_NNPA_MATMUL_OP_BCAST23 = 0x72,
   S390_NNPA_MATMUL_OP_BCAST1  = 0x73,
   S390_NNPA_TRANSFORM         = 0xf0,
   S390_NNPA_REDUCE            = 0xf1,
} s390_nnpa_function_t;

/* Suported NNPA functions */
static const ULong NNPA_functions[] = {
   (S390_SETBIT(S390_NNPA_QAF) | S390_SETBIT(S390_NNPA_ADD) |
    S390_SETBIT(S390_NNPA_SUB) | S390_SETBIT(S390_NNPA_MUL) |
    S390_SETBIT(S390_NNPA_DIV) | S390_SETBIT(S390_NNPA_MIN) |
    S390_SETBIT(S390_NNPA_MAX) | S390_SETBIT(S390_NNPA_LOG) |
    S390_SETBIT(S390_NNPA_EXP) | S390_SETBIT(S390_NNPA_SQRT) |
    S390_SETBIT(S390_NNPA_INVSQRT) | S390_SETBIT(S390_NNPA_RELU) |
    S390_SETBIT(S390_NNPA_TANH) | S390_SETBIT(S390_NNPA_SIGMOID) |
    S390_SETBIT(S390_NNPA_SOFTMAX) | S390_SETBIT(S390_NNPA_GELU)),
   (S390_SETBIT(S390_NNPA_BATCHNORM) | S390_SETBIT(S390_NNPA_MOMENTS) |
    S390_SETBIT(S390_NNPA_LAYERNORM) | S390_SETBIT(S390_NNPA_NORM) |
    S390_SETBIT(S390_NNPA_MAXPOOL2D) | S390_SETBIT(S390_NNPA_AVGPOOL2D) |
    S390_SETBIT(S390_NNPA_LSTMACT) | S390_SETBIT(S390_NNPA_GRUACT) |
    S390_SETBIT(S390_NNPA_CONVOLUTION) | S390_SETBIT(S390_NNPA_MATMUL_OP) |
    S390_SETBIT(S390_NNPA_MATMUL_OP_BCAST23) |
    S390_SETBIT(S390_NNPA_MATMUL_OP_BCAST1)),
   0,
   (S390_SETBIT(S390_NNPA_TRANSFORM) | S390_SETBIT(S390_NNPA_REDUCE)),
};

/* Supported parameter block formats */
static const ULong NNPA_ipbf[] = {
   (S390_SETBIT(0) | S390_SETBIT(1)),
};

/* Supported data types and data layout formats */
enum {
   S390_NNPA_TYPE_1     = 0, // data type 1 (16 bit)
   S390_NNPA_TYPE_BFP32 = 6,
   S390_NNPA_TYPE_INT8  = 8,
   S390_NNPA_TYPE_INT32 = 10,
};

enum {
   S390_NNPA_4D_FEATURE_TENSOR = 0,
   S390_NNPA_4D_KERNEL_TENSOR  = 1,
   S390_NNPA_4D_WEIGHTS_TENSOR = 2,
   S390_NNPA_4D_GENERIC_TENSOR = 31,
};

static const ULong NNPA_dtypes_layouts[] = {
   /* Data types */
   (S390_SETBIT(S390_NNPA_TYPE_1) | S390_SETBIT(S390_NNPA_TYPE_BFP32) |
    S390_SETBIT(S390_NNPA_TYPE_INT8) | S390_SETBIT(S390_NNPA_TYPE_INT32) |

    /* Data layout formats */
    S390_SETBIT(32 + S390_NNPA_4D_FEATURE_TENSOR) |
    S390_SETBIT(32 + S390_NNPA_4D_KERNEL_TENSOR) |
    S390_SETBIT(32 + S390_NNPA_4D_WEIGHTS_TENSOR) |
    S390_SETBIT(32 + S390_NNPA_4D_GENERIC_TENSOR)),
};

static const ULong NNPA_conversions[] = {
   (S390_SETBIT(1) | // BFP tiny format
    S390_SETBIT(2)), // BFP short format
};

struct s390_NNPA_parms_qaf {
   ULong funcs[4];
   ULong ipbf[2];
   ULong dtypes_layouts;
   UInt  reserved1;
   UInt  mdis;
   ULong mts;
   ULong conversions;
   ULong reserved2;
   UInt  mdnis[4];
   struct {
      ULong reserved[19];
   } reserved3;
};

/* Tensor descriptor, common for all data-layout formats */
struct s390_NNPA_tensor {
   UChar  layout;
   UChar  dtype;
   UShort reserved1;
   UInt   reserved2;
   UInt   dim4, dim3, dim2, dim1;
   ULong  address;
};

/* Parameter block format 0 or 1 */
struct s390_NNPA_parms {
   ULong                   pbvn : 16;
   ULong                   mvn : 8;
   ULong                   ribm : 24;
   ULong                   reserved0 : 15;
   ULong                   cf : 1;
   ULong                   reserved1[6];
   ULong                   save_area_address;
   struct s390_NNPA_tensor out[2];
   struct s390_NNPA_tensor reserved2[2];
   struct s390_NNPA_tensor in[3];
   ULong                   reserved3[12];
   UInt                    param[16];
   ULong                   reserved4[8];
};

enum {
   s390_NNPA_message_in  = 0,
   s390_NNPA_message_out = 3,
   s390_NNPA_message_n   = 5,
};

static const char* const s390_NNPA_errmsg_dtype[s390_NNPA_message_n] = {
   "NNPA: unknown data type in input tensor 1",
   "NNPA: unknown data type in input tensor 2",
   "NNPA: unknown data type in input tensor 3",
   "NNPA: unknown data type in output tensor 1",
   "NNPA: unknown data type in output tensor 2",
};

static const char* const s390_NNPA_errmsg_layout[s390_NNPA_message_n] = {
   "NNPA: unknown layout in input tensor 1",
   "NNPA: unknown layout in input tensor 2",
   "NNPA: unknown layout in input tensor 3",
   "NNPA: unknown layout in output tensor 1",
   "NNPA: unknown layout in output tensor 2",
};

static const char* const s390_NNPA_errmsg_access[s390_NNPA_message_n] = {
   "NNPA(in_tensor_1)",  "NNPA(in_tensor_2)",  "NNPA(in_tensor_3)",
   "NNPA(out_tensor_1)", "NNPA(out_tensor_2)",
};

struct s390_NNPA_mem_dimensions {
   UChar layout;
   ULong dim[4];
   ULong total_size;
   ULong used_sticks; // occupied sticks per next-higher dimension
   ULong stick_fill;
   ULong last_stick_fill;
};

/* Determine the dimensions used to represent the tensor data in memory */
static enum ExtensionError
NNPA_tensor_size(const struct s390_NNPA_tensor*   t,
                 UInt                             msg_idx,
                 struct s390_NNPA_mem_dimensions* out_md)
{
   struct s390_NNPA_mem_dimensions md;
   ULong                           elem_size;
   ULong                           eps;

   switch (t->dtype) {
   case S390_NNPA_TYPE_INT8:
      elem_size = 1;
      break;
   case S390_NNPA_TYPE_1:
      elem_size = 2;
      break;
   case S390_NNPA_TYPE_BFP32:
   case S390_NNPA_TYPE_INT32:
      elem_size = 4;
      break;
   default:
      return INSN_ERR(s390_NNPA_errmsg_dtype[msg_idx]);
   }
   eps = 128 / elem_size;

   md.layout = t->layout;
   switch (t->layout) {
   case S390_NNPA_4D_FEATURE_TENSOR:
      md.dim[0]      = t->dim4;
      md.dim[1]      = (t->dim1 + eps - 1) / eps;
      md.used_sticks = t->dim2;
      goto common_tensor_dimensions;
   case S390_NNPA_4D_KERNEL_TENSOR:
      md.dim[0]      = (t->dim1 + eps - 1) / eps;
      md.dim[1]      = t->dim4;
      md.used_sticks = t->dim2;
      goto common_tensor_dimensions;
   case S390_NNPA_4D_WEIGHTS_TENSOR:
      elem_size *= 2;
      eps /= 2;
      md.dim[0]      = t->dim4;
      md.dim[1]      = (t->dim1 + eps - 1) / eps;
      md.used_sticks = (t->dim2 + 1) / 2;
   common_tensor_dimensions:
      md.dim[2]          = t->dim3;
      md.dim[3]          = (md.used_sticks + 31) / 32 * 32;
      md.stick_fill      = elem_size * (t->dim1 >= eps ? eps : t->dim1);
      md.last_stick_fill = elem_size * ((t->dim1 - 1) % eps + 1);
      break;
   case S390_NNPA_4D_GENERIC_TENSOR:
      md.dim[0] = t->dim4;
      md.dim[1] = t->dim3;
      md.dim[2] = t->dim2;
      md.dim[3] = t->dim1;
      eps       = 1;
      break;
   default:
      return INSN_ERR(s390_NNPA_errmsg_layout[msg_idx]);
   }
   md.total_size =
      elem_size * eps * md.dim[3] * md.dim[2] * md.dim[1] * md.dim[0];
   *out_md = md;
   return ExtErr_OK;
}

/* Track a tensor's memory regions with PRE_MEM_READ or POST_MEM_WRITE */
static enum ExtensionError NNPA_track_tensor(ThreadState* tst,
                                             UInt         msg_idx,
                                             const struct s390_NNPA_tensor* t,
                                             Bool do_write)
{
   struct s390_NNPA_mem_dimensions md;
   enum ExtensionError             ret;
   ULong                           addr = t->address;

   ret = NNPA_tensor_size(t, msg_idx, &md);
   if (ret != ExtErr_OK)
      return ret;

   switch (md.layout) {
   case S390_NNPA_4D_FEATURE_TENSOR:
   case S390_NNPA_4D_KERNEL_TENSOR:
   case S390_NNPA_4D_WEIGHTS_TENSOR:
      for (ULong d0 = 0; d0 < md.dim[0]; d0++) {
         for (ULong d1 = 0; d1 < md.dim[1]; d1++) {
            ULong len;
            switch (md.layout) {
            case S390_NNPA_4D_FEATURE_TENSOR:
            case S390_NNPA_4D_WEIGHTS_TENSOR:
               len = d1 + 1 == md.dim[1] ? md.last_stick_fill : md.stick_fill;
               break;
            case S390_NNPA_4D_KERNEL_TENSOR:
               len = d0 + 1 == md.dim[0] ? md.last_stick_fill : md.stick_fill;
               break;
            }
            for (ULong d2 = 0; d2 < md.dim[2]; d2++) {
               for (ULong d3 = 0; d3 < md.used_sticks; d3++) {
                  if (md.layout == S390_NNPA_4D_WEIGHTS_TENSOR &&
                      d3 == md.used_sticks - 1 && t->dim2 % 2 != 0) {
                     // even elements only
                     for (ULong i = 0; i < len - 1; i += 2) {
                        if (do_write) {
                           POST_MEM_WRITE(tst, addr + i, 1);
                        } else {
                           PRE_MEM_READ(tst, s390_NNPA_errmsg_access[msg_idx],
                                        addr + i, 1);
                        }
                     }
                  } else if (do_write) {
                     POST_MEM_WRITE(tst, addr, len);
                  } else {
                     PRE_MEM_READ(tst, s390_NNPA_errmsg_access[msg_idx], addr,
                                  len);
                  }
                  addr += 128;
               }
               addr += 128 * (md.dim[3] - md.used_sticks);
            }
         }
      }
      break;
   case S390_NNPA_4D_GENERIC_TENSOR:
      if (do_write) {
         POST_MEM_WRITE(tst, t->address, md.total_size);
      } else {
         PRE_MEM_READ(tst, s390_NNPA_errmsg_access[msg_idx], t->address,
                      md.total_size);
      }
      break;
   }
   return ExtErr_OK;
}

static enum ExtensionError do_extension_NNPA(ThreadState* tst, ULong variant)
{
   ULong gpr0       = READ_GPR(tst, "NNPA(r0)", 0);
   UChar fc         = gpr0 & 0x7f;
   ULong parms_addr = READ_GPR(tst, "NNPA(r1)", 1);
   Int   cc         = 0;
   ULong parms_len;

   if (fc == S390_NNPA_QAF) { // Query
      struct s390_NNPA_parms_qaf* parms = (void*)parms_addr;

      parms_len = sizeof(struct s390_NNPA_parms_qaf);
      PRE_MEM_WRITE(tst, "NNPA(parms)", parms_addr, parms_len);
      cc = do_NNPA_insn(&gpr0, parms_addr);
      s390_filter_functions(parms->funcs, sizeof(parms->funcs), NNPA_functions,
                            sizeof(NNPA_functions));
      s390_filter_functions(parms->ipbf, sizeof(parms->ipbf), NNPA_ipbf,
                            sizeof(NNPA_ipbf));
      s390_filter_functions(&parms->dtypes_layouts, sizeof(ULong),
                            NNPA_dtypes_layouts, sizeof(NNPA_dtypes_layouts));
      s390_filter_functions(&parms->conversions, sizeof(ULong),
                            NNPA_conversions, sizeof(NNPA_conversions));
      // Clear reserved fields
      parms->reserved1 = 0;
      parms->reserved2 = 0;
      parms->reserved3 = (__typeof__(parms->reserved3)){0};
   } else {
      struct s390_NNPA_parms*      parms          = (void*)parms_addr;
      const struct s390_NNPA_parms orig_parms     = *parms;
      ULong                        save_area_size = 0;
      UInt                         in_tensors;
      UInt                         out_tensors;
      enum ExtensionError          retval;

      parms_len = 4096;
      PRE_MEM_READ(tst, "NNPA(parms)", parms_addr,
                   sizeof(struct s390_NNPA_parms));
      if (parms->cf) {
         PRE_MEM_READ(tst, "NNPA(parms.csb)", parms_addr + 512,
                      parms_len - 512);
      }
      PRE_MEM_WRITE(tst, "NNPA(parms)", parms_addr, parms_len);

      switch (fc) {
      case S390_NNPA_ADD:
      case S390_NNPA_SUB:
      case S390_NNPA_MUL:
      case S390_NNPA_DIV:
      case S390_NNPA_MIN:
      case S390_NNPA_MAX:
      case S390_NNPA_NORM:
         in_tensors  = 2;
         out_tensors = 1;
         break;
      case S390_NNPA_LOG:
      case S390_NNPA_EXP:
      case S390_NNPA_SQRT:
      case S390_NNPA_INVSQRT:
      case S390_NNPA_RELU:
      case S390_NNPA_TANH:
      case S390_NNPA_SIGMOID:
      case S390_NNPA_GELU:
         in_tensors  = 1;
         out_tensors = 1;
         break;
      case S390_NNPA_SOFTMAX:
      case S390_NNPA_REDUCE:
         in_tensors     = 1;
         out_tensors    = 1;
         save_area_size = 8192;
         break;
      case S390_NNPA_BATCHNORM:
      case S390_NNPA_LAYERNORM:
         in_tensors  = 3;
         out_tensors = 1;
         break;
      case S390_NNPA_MOMENTS:
         in_tensors  = 1;
         out_tensors = 2;
         break;
      case S390_NNPA_MAXPOOL2D:
      case S390_NNPA_AVGPOOL2D:
      case S390_NNPA_TRANSFORM:
         in_tensors  = 1;
         out_tensors = 1;
         break;
      case S390_NNPA_LSTMACT:
         in_tensors  = 3;
         out_tensors = 2;
         break;
      case S390_NNPA_GRUACT:
      case S390_NNPA_CONVOLUTION:
      case S390_NNPA_MATMUL_OP:
      case S390_NNPA_MATMUL_OP_BCAST23:
      case S390_NNPA_MATMUL_OP_BCAST1:
         in_tensors  = 3;
         out_tensors = 1;
         break;
      default:
         return INSN_ERR("NNPA: unknown function code\n");
      }

      for (UInt i = 0; i < in_tensors; i++) {
         retval = NNPA_track_tensor(tst, s390_NNPA_message_in + i,
                                    &parms->in[i], False);
         if (retval != ExtErr_OK)
            return retval;
      }
      for (UInt i = 0; i < out_tensors; i++) {
         UInt                            msg_idx = s390_NNPA_message_out + i;
         struct s390_NNPA_mem_dimensions md;

         retval = NNPA_tensor_size(&parms->out[i], msg_idx, &md);
         if (retval != ExtErr_OK)
            return retval;
         PRE_MEM_WRITE(tst, s390_NNPA_errmsg_access[msg_idx],
                       parms->out[i].address, md.total_size);
      }
      if (save_area_size != 0) {
         PRE_MEM_WRITE(tst, "NNPA(save_area)", parms->save_area_address,
                       save_area_size);
      }
      cc = do_NNPA_insn(&gpr0, parms_addr);
      if (cc == 0) {
         for (UInt i = 0; i < out_tensors; i++) {
            retval = NNPA_track_tensor(tst, s390_NNPA_message_out + i,
                                       &orig_parms.out[i], True);
            if (retval != ExtErr_OK)
               return retval;
         }
      }
   }
   POST_MEM_WRITE(tst, parms_addr, parms_len);
   WRITE_GPR(tst, 0, gpr0);
   WRITE_CC(tst, cc);
   return ExtErr_OK;
}

/*---------------------------------------------------------------*/
/*--- DFLTCC (deflate conversion call)                        ---*/
/*---------------------------------------------------------------*/

static Int do_DFLTCC_insn(UChar  func,
                          ULong  parms,
                          ULong* addr1,
                          ULong* len1,
                          ULong* addr2,
                          ULong* len2,
                          ULong  addr3)
{
   register UChar reg0 asm("0") = func;
   register void* reg1 asm("1") = (void*)parms;
   union reg_pair op1           = {{*addr1, *len1}};
   union reg_pair op2           = {{*addr2, *len2}};
   Int            cc;

   asm volatile(".insn rrf, 0xb9390000, %[op1], %[op2], %[op3], 0\n"
                "ipm %[cc]\n"
                "srl %[cc], 28\n"
                : [cc] "=d"(cc), [op1] "+a"(op1.pair), [op2] "+a"(op2.pair)
                : "d"(reg0), "d"(reg1), [op3] "d"(addr3)
                : "cc", "memory");
   *addr1 = op1.a;
   *len1  = op1.b;
   *addr2 = op2.a;
   *len2  = op2.b;
   return cc;
}

struct s390_DFLTCC_parms0 {
   UShort pbvn;
   UChar  mvn;
   UChar  ribm[3];
   UShort reserved0 : 15;
   UShort cf : 1;
   ULong  reserved1;
   UShort nt : 1;
   UShort reserved2 : 1;
   UShort cvt : 1;
   UShort reserved3 : 1;
   UShort htt : 1;
   UShort bcf : 1;
   UShort bcc : 1;
   UShort bhf : 1;
   UShort reserved4 : 1;
   UShort reserved5 : 1;
   UShort dhtgc : 1;
   UShort reserved6 : 5;
   UChar  reserved7 : 5;
   UChar  sbb : 3;
   UChar  oesc : 8;
   UShort reserved8 : 12;
   UShort ifs : 4;
   UShort ifl;
   UChar  reserved9[20];
   UShort hl;
   UShort reserved10 : 1;
   UShort ho : 15;
   UChar  data[1488];
};

/* DFLTCC functions that we support if the hardware does. */
static const ULong DFLTCC_functions[] = {
   (S390_SETBIT(0)     // Query
    | S390_SETBIT(1)   // GDHT
    | S390_SETBIT(2)   // CMPR
    | S390_SETBIT(4)), // XPND
};

static UWord do_extension_DFLTCC(ThreadState* tst, ULong variant)
{
   enum { circ_hist_len = 32768 };
   UChar r1    = variant & 0xf;
   UChar r2    = (variant >> 4) & 0xf;
   UChar r3    = (variant >> 8) & 0xf;
   UChar func  = READ_FUNCTION_CODE(tst, "DFLTCC");
   UChar fc    = func & 0x7f;
   Bool  hbt   = (func & 128) != 0;
   ULong parms = READ_GPR(tst, "DFLTCC(r1)", 1);
   ULong parms_len;
   Int   cc         = 0;
   ULong orig_addr1 = 0, orig_len1 = 0;
   ULong addr1 = 0, len1 = 0, addr2 = 0, len2 = 0, addr3 = 0;
   Bool  do_compress = 0;

   switch (fc) {
   case 0: // Query
      parms_len = 32;
      PRE_MEM_WRITE(tst, "DFLTCC(parms)", parms, parms_len);
      cc = do_DFLTCC_insn(func, parms, &addr1, &len1, &addr2, &len2, addr3);
      s390_filter_functions((ULong*)parms, 16, DFLTCC_functions,
                            sizeof(DFLTCC_functions));
      POST_MEM_WRITE(tst, parms, parms_len);
      break;
   case 1: // GDHT
      parms_len = 384;
      PRE_MEM_READ(tst, "DFLTCC(parms)", parms, parms_len);
      PRE_MEM_WRITE(tst, "DFLTCC(parms)", parms, parms_len);
      addr2 = READ_GPR(tst, "DFLTCC(op2_addr)", r2);
      len2  = READ_GPR(tst, "DFLTCC(op2_len)", r2 + 1);
      if (len2 > 32768)
         len2 = 32768;
      PRE_MEM_READ(tst, "DFLTCC(parms)", addr2, len2);
      cc = do_DFLTCC_insn(func, parms, &addr1, &len1, &addr2, &len2, addr3);
      POST_MEM_WRITE(tst, parms, parms_len);
      break;
   case 2: // CMPR
      do_compress = 1;
      /* fallthrough */
   case 4: // XPND
   {
      struct s390_DFLTCC_parms0* p;
      parms_len = 1536;
      PRE_MEM_READ(tst, "DFLTCC(parms)", parms, parms_len);
      p     = (void*)parms;
      addr1 = orig_addr1 = READ_GPR(tst, "DFLTCC(op1_addr)", r1);
      len1 = orig_len1 = READ_GPR(tst, "DFLTCC(op1_len)", r1 + 1);
      PRE_MEM_WRITE(tst, "DFLTCC(op1)", addr1, len1);
      addr2 = READ_GPR(tst, "DFLTCC(op2_addr)", r2);
      len2  = READ_GPR(tst, "DFLTCC(op2_len)", r2 + 1);
      PRE_MEM_READ(tst, "DFLTCC(op2)", addr2, len2);
      addr3 = READ_GPR(tst, "DFLTCC(op3)", r3);
      if (hbt) {
         PRE_MEM_WRITE(tst, "DFLTCC(op3)", addr3, circ_hist_len);
      }
      if (!p->nt) {
         if (hbt) {
            ULong hl1 = circ_hist_len - p->ho;
            if (hl1 >= p->hl) {
               hl1 = p->hl;
            } else {
               PRE_MEM_READ(tst, "DFLTCC(op3)", addr3, p->hl - hl1);
            }
            PRE_MEM_READ(tst, "DFLTCC(op3)", addr3 + p->ho, hl1);
         } else {
            PRE_MEM_READ(tst, "DFLTCC(op2.hist)", addr2 - p->hl, p->hl);
         }
      }
      cc = do_DFLTCC_insn(func, parms, &addr1, &len1, &addr2, &len2, addr3);
      POST_MEM_WRITE(tst, parms, parms_len);
      POST_MEM_WRITE(tst, orig_addr1,
                     orig_len1 - len1 + (do_compress && p->sbb ? 1 : 0));
      if (hbt) {
         ULong hl1 = circ_hist_len - p->ho;
         if (hl1 >= p->hl) {
            hl1 = p->hl;
         } else {
            POST_MEM_WRITE(tst, addr3, p->hl - hl1);
         }
         POST_MEM_WRITE(tst, addr3 + p->ho, hl1);
      }
      WRITE_GPR(tst, r1, addr1);
      WRITE_GPR(tst, r1 + 1, len1);
      WRITE_GPR(tst, r2, addr2);
      WRITE_GPR(tst, r2 + 1, len2);
      break;
   }
   default:
      return INSN_ERR("DFLTCC: unknown function code\n");
   }
   WRITE_CC(tst, cc);
   return ExtErr_OK;
}

/*---------------------------------------------------------------*/
/*--- STFLE (store facility list extended)                    ---*/
/*---------------------------------------------------------------*/

static enum ExtensionError do_extension_STFLE(ThreadState* tst, ULong variant)
{
   Int    cc      = 0;
   UChar  b2      = variant & 0xf;
   UShort d2      = (variant >> 8) & 0xfff;
   ULong  gpr0    = READ_GPR(tst, "STFLE(r0)", 0);
   ULong  last_dw = gpr0 & 0xff;
   ULong  addr    = READ_GPR(tst, "STFLE(b2)", b2) + d2;

   PRE_MEM_WRITE(tst, "STFLE(bits)", addr, (last_dw + 1) * sizeof(ULong));
   static const ULong accepted_facility[] = {
      /* ===  0 .. 63  === */
      (S390_SETBITS(0, 19)
       /* 20: HFP-multiply-and-add/subtract, not supported */
       | S390_SETBITS(21, 22)
       /* 23: HFP-unnormalized-extension, not supported */
       | S390_SETBITS(24, 25)
       /* 26: parsing-enhancement, not supported */
       | S390_SETBITS(27, 28)
       /* 29: unassigned */
       | S390_SETBITS(30, 30)
       /* 31: extract-CPU-time, not supported */
       | S390_SETBITS(32, 41)
       /* 42-43: DFP, not fully supported */
       /* 44: PFPO, not fully supported */
       | S390_SETBITS(45, 47)
       /* 48: DFP zoned-conversion, not supported */
       | S390_SETBITS(49, 49)
       /* 50: constrained transactional-execution, not supported */
       | S390_SETBITS(51, 55)
       /* 56: unassigned */
       | S390_SETBITS(57, 63)),

      /* ===  64 .. 127  === */
      (S390_SETBITS(64, 72)
       /* 73: transactional-execution, not supported */
       | S390_SETBITS(74, 78)
       /* 80: DFP packed-conversion, not supported */
       /* 81: PPA-in-order, not supported */
       | S390_SETBITS(82, 82)
       /* 83-127: unassigned */),

      /* ===  128 .. 191  === */
      (S390_SETBITS(128, 131)
       /* 132: unassigned */
       /* 133: guarded-storage, not supported */
       | S390_SETBITS(134, 135)
       /* 136: unassigned */
       /* 137: unassigned */
       | S390_SETBITS(138, 142)
       /* 143: unassigned */
       | S390_SETBITS(144, 149)
       /* 150: unassigned */
       | S390_SETBITS(151, 151)
       /* 152: vector packed decimal enhancement, not supported */
       /* 153: unassigned */
       /* 154: unassigned */
       | S390_SETBITS(155, 156)
       /* 157-164: unassigned */
       | S390_SETBITS(165, 165)
       /* 166-167: unassigned */
       | S390_SETBITS(168, 168)
       /* 168-191: unassigned */),

      /* ===  192 .. 255  === */
      /* 192: vector-packed-decimal, not supported */
      (S390_SETBITS(193, 194)
       /* 195: unassigned */
       | S390_SETBITS(196, 197)),
   };
   asm("lgr 0,%[r0]\n"
       ".insn s,0xb2b00000,%[out]\n" /* stfle */
       "lgr %[r0],0\n"
       "ipm %[cc]\n"
       "srl %[cc],28\n"
       : [out] "=Q"(*(ULong(*)[last_dw + 1])(void*)addr), [r0] "+d"(gpr0),
         [cc] "=d"(cc)
       :
       : "cc");

   WRITE_GPR(tst, 0, gpr0);
   if (last_dw > (gpr0 & 0xff))
      last_dw = gpr0 & 0xff;
   s390_filter_functions((ULong*)addr, (last_dw + 1) * sizeof(ULong),
                         accepted_facility, sizeof(accepted_facility));
   POST_MEM_WRITE(tst, addr, (last_dw + 1) * sizeof(ULong));

   WRITE_CC(tst, cc);
   return ExtErr_OK;
}

/*---------------------------------------------------------------*/
/*--- KM (cypher message)                                     ---*/
/*---------------------------------------------------------------*/

S390_DEFINE_DO_RR_INSN(do_KM_insn, 0xb92e)

/* List all the functions supported.  This list provides the parameter block
   sizes and will also be used for filtering the supported functions.  The
   function names are included for documentation purposes only. */

#define S390_DO_FUNCTIONS                                                      \
   S390_DO_FUNC(0, S390_KM_Query, 16)                                          \
   S390_DO_FUNC(1, S390_KM_DEA, 8)                                             \
   S390_DO_FUNC(2, S390_KM_TDEA_128, 16)                                       \
   S390_DO_FUNC(3, S390_KM_TDEA_192, 24)                                       \
   S390_DO_FUNC(9, S390_KM_Encrypted_DEA, 32)                                  \
   S390_DO_FUNC(10, S390_KM_Encrypted_TDEA_128, 40)                            \
   S390_DO_FUNC(11, S390_KM_Encrypted_TDEA_192, 48)                            \
   S390_DO_FUNC(18, S390_KM_AES_128, 16)                                       \
   S390_DO_FUNC(19, S390_KM_AES_192, 24)                                       \
   S390_DO_FUNC(20, S390_KM_AES_256, 32)                                       \
   S390_DO_FUNC(26, S390_KM_Encrypted_AES_128, 48)                             \
   S390_DO_FUNC(27, S390_KM_Encrypted_AES_192, 56)                             \
   S390_DO_FUNC(28, S390_KM_Encrypted_AES_256, 64)                             \
   S390_DO_FUNC(50, S390_KM_XTS_AES_128, 32)                                   \
   S390_DO_FUNC(52, S390_KM_XTS_AES_256, 48)                                   \
   S390_DO_FUNC(58, S390_KM_XTS_Encrypted_AES_128, 64)                         \
   S390_DO_FUNC(60, S390_KM_XTS_Encrypted_AES_256, 80)

#define S390_DO_FUNC(fc, name, plen) [fc] = plen,
static const UChar S390_KM_parms_len[] = {S390_DO_FUNCTIONS};
#undef S390_DO_FUNC

#define S390_DO_FUNC(fc, name, plen) | S390_SETBIT(fc)
static const ULong S390_KM_supported_fc[] = {0 S390_DO_FUNCTIONS};
#undef S390_DO_FUNC

#undef S390_DO_FUNCTIONS

static enum ExtensionError do_extension_KM(ThreadState* tst, ULong variant)
{
   UChar r1        = variant & 0xf;
   UChar r2        = (variant >> 4) & 0xf;
   UChar func      = READ_FUNCTION_CODE(tst, "KM");
   UChar fc        = func & 0x7f;
   ULong parms     = READ_GPR(tst, "KM(r1)", 1);
   ULong parms_len = 0;
   Int   cc        = 0;
   ULong orig_addr1, orig_len2 = 0;
   ULong addr1 = 0, len1 = 0, addr2 = 0, len2 = 0;

   if (fc < sizeof(S390_KM_parms_len) / sizeof(S390_KM_parms_len[0]))
      parms_len = S390_KM_parms_len[fc];
   if (parms_len == 0)
      return INSN_ERR("KM: unknown function code\n");

   if (fc == 0) { // Query
      PRE_MEM_WRITE(tst, "KM(parms)", parms, parms_len);
      cc = do_KM_insn(func, parms, &addr1, &len1, &addr2, &len2);
      s390_filter_functions((ULong*)parms, parms_len, S390_KM_supported_fc,
                            sizeof(S390_KM_supported_fc));
      POST_MEM_WRITE(tst, parms, parms_len);
   } else {
      addr1 = orig_addr1 = READ_GPR(tst, "KM(op1_addr)", r1);
      addr2              = READ_GPR(tst, "KM(op2_addr)", r2);
      len2 = orig_len2 = READ_GPR(tst, "KM(op2_len)", r2 + 1);
      PRE_MEM_READ(tst, "KM(parms)", parms, parms_len);
      PRE_MEM_WRITE(tst, "KM(op1)", addr1, len2);
      PRE_MEM_READ(tst, "KM(op2)", addr2, len2);
      cc = do_KM_insn(func, parms, &addr1, &len1, &addr2, &len2);
      WRITE_GPR(tst, r1, addr1);
      WRITE_GPR(tst, r2, addr2);
      WRITE_GPR(tst, r2 + 1, len2);
      POST_MEM_WRITE(tst, orig_addr1, orig_len2 - len2);
   }
   WRITE_CC(tst, cc);
   return ExtErr_OK;
}

/*---------------------------------------------------------------*/
/*--- KMC (cypher message with chaining)                      ---*/
/*---------------------------------------------------------------*/

S390_DEFINE_DO_RR_INSN(do_KMC_insn, 0xb92f)

#define S390_DO_FUNCTIONS                                                      \
   S390_DO_FUNC(0, S390_KMC_Query, 16)                                         \
   S390_DO_FUNC(1, S390_KMC_DEA, 16)                                           \
   S390_DO_FUNC(2, S390_KMC_TDEA_128, 24)                                      \
   S390_DO_FUNC(3, S390_KMC_TDEA_192, 32)                                      \
   S390_DO_FUNC(9, S390_KMC_Encrypted_DEA, 40)                                 \
   S390_DO_FUNC(10, S390_KMC_Encrypted_TDEA_128, 48)                           \
   S390_DO_FUNC(11, S390_KMC_Encrypted_TDEA_192, 56)                           \
   S390_DO_FUNC(18, S390_KMC_AES_128, 32)                                      \
   S390_DO_FUNC(19, S390_KMC_AES_192, 40)                                      \
   S390_DO_FUNC(20, S390_KMC_AES_256, 48)                                      \
   S390_DO_FUNC(26, S390_KMC_Encrypted_AES_128, 64)                            \
   S390_DO_FUNC(27, S390_KMC_Encrypted_AES_192, 72)                            \
   S390_DO_FUNC(28, S390_KMC_Encrypted_AES_256, 80)

#define S390_DO_FUNCTIONS1 S390_DO_FUNC(67, S390_KMC_PRNG, 32)

#define S390_DO_FUNC(fc, name, plen) [fc] = plen,
static const UChar S390_KMC_parms_len[] = {
   S390_DO_FUNCTIONS S390_DO_FUNCTIONS1};
#undef S390_DO_FUNC

#define S390_DO_FUNC(fc, name, plen) | S390_SETBIT(fc)
static const ULong S390_KMC_supported_fc[] = {0 S390_DO_FUNCTIONS,
                                              0 S390_DO_FUNCTIONS1};
#undef S390_DO_FUNC

#undef S390_DO_FUNCTIONS
#undef S390_DO_FUNCTIONS1

static enum ExtensionError do_extension_KMC(ThreadState* tst, ULong variant)
{
   UChar r1        = variant & 0xf;
   UChar r2        = (variant >> 4) & 0xf;
   UChar func      = READ_FUNCTION_CODE(tst, "KMC");
   UChar fc        = func & 0x7f;
   ULong parms     = READ_GPR(tst, "KMC(r1)", 1);
   ULong parms_len = 0;
   Int   cc        = 0;
   ULong orig_addr1, orig_len2 = 0;
   ULong addr1 = 0, len1 = 0, addr2 = 0, len2 = 0;

   if (fc < sizeof(S390_KMC_parms_len) / sizeof(S390_KMC_parms_len[0]))
      parms_len = S390_KMC_parms_len[fc];
   if (parms_len == 0)
      return INSN_ERR("KMC: unknown function code\n");

   PRE_MEM_WRITE(tst, "KMC(parms)", parms, parms_len);
   if (fc == 0) { // Query
      cc = do_KMC_insn(func, parms, &addr1, &len1, &addr2, &len2);
      s390_filter_functions((ULong*)parms, parms_len, S390_KMC_supported_fc,
                            sizeof(S390_KMC_supported_fc));
   } else {
      addr1 = orig_addr1 = READ_GPR(tst, "KMC(op1_addr)", r1);
      addr2              = READ_GPR(tst, "KMC(op2_addr)", r2);
      len2 = orig_len2 = READ_GPR(tst, "KMC(op2_len)", r2 + 1);
      PRE_MEM_READ(tst, "KMC(parms)", parms, parms_len);
      PRE_MEM_WRITE(tst, "KMC(op1)", addr1, len2);
      PRE_MEM_READ(tst, "KMC(op2)", addr2, len2);
      cc = do_KMC_insn(func, parms, &addr1, &len1, &addr2, &len2);
      WRITE_GPR(tst, r1, addr1);
      WRITE_GPR(tst, r2, addr2);
      WRITE_GPR(tst, r2 + 1, len2);
      POST_MEM_WRITE(tst, orig_addr1, orig_len2 - len2);
   }
   POST_MEM_WRITE(tst, parms, parms_len);
   WRITE_CC(tst, cc);
   return ExtErr_OK;
}

/*---------------------------------------------------------------*/
/*--- KIMD (compute intermediate message digest)              ---*/
/*---------------------------------------------------------------*/

S390_DEFINE_DO_0R_INSN(do_KIMD_insn, 0xb93e)

#define S390_DO_FUNCTIONS                                                      \
   S390_DO_FUNC(0, S390_KIMD_Query, 16)                                        \
   S390_DO_FUNC(1, S390_KIMD_SHA_1, 20)                                        \
   S390_DO_FUNC(2, S390_KIMD_SHA_256, 32)                                      \
   S390_DO_FUNC(3, S390_KIMD_SHA_512, 64)                                      \
   S390_DO_FUNC(32, S390_KIMD_SHA3_224, 200)                                   \
   S390_DO_FUNC(33, S390_KIMD_SHA3_256, 200)                                   \
   S390_DO_FUNC(34, S390_KIMD_SHA3_384, 200)                                   \
   S390_DO_FUNC(35, S390_KIMD_SHA3_512, 200)                                   \
   S390_DO_FUNC(36, S390_KIMD_SHAKE_128, 200)                                  \
   S390_DO_FUNC(37, S390_KIMD_SHAKE_256, 200)                                  \
   S390_DO_FUNC(65, S390_KIMD_GHASH, 32)

#define S390_DO_FUNC(fc, name, plen) [fc] = plen,
static const UChar S390_KIMD_parms_len[] = {S390_DO_FUNCTIONS};
#undef S390_DO_FUNC

#define S390_DO_FUNC(fc, name, plen) | S390_SETBIT(fc)
static const ULong S390_KIMD_supported_fc[] = {0 S390_DO_FUNCTIONS};
#undef S390_DO_FUNC

#undef S390_DO_FUNCTIONS

static enum ExtensionError do_extension_KIMD(ThreadState* tst, ULong variant)
{
   UChar r2        = (variant >> 4) & 0xf;
   UChar func      = READ_FUNCTION_CODE(tst, "KIMD");
   UChar fc        = func & 0x7f;
   ULong parms     = READ_GPR(tst, "KIMD(r1)", 1);
   ULong parms_len = 0;
   Int   cc        = 0;
   ULong addr2 = 0, len2 = 0;

   if (fc < sizeof(S390_KIMD_parms_len) / sizeof(S390_KIMD_parms_len[0]))
      parms_len = S390_KIMD_parms_len[fc];
   if (parms_len == 0)
      return INSN_ERR("KIMD: unknown function code\n");

   if (fc == 0) { // Query
      PRE_MEM_WRITE(tst, "KIMD(parms)", parms, parms_len);
      cc = do_KIMD_insn(func, parms, &addr2, &len2);
      s390_filter_functions((ULong*)parms, parms_len, S390_KIMD_supported_fc,
                            sizeof(S390_KIMD_supported_fc));
      POST_MEM_WRITE(tst, parms, parms_len);
   } else {
      addr2 = READ_GPR(tst, "KIMD(op2_addr)", r2);
      len2  = READ_GPR(tst, "KIMD(op2_len)", r2 + 1);
      PRE_MEM_READ(tst, "KIMD(parms)", parms, parms_len);
      PRE_MEM_WRITE(tst, "KIMD(parms)", parms, parms_len);
      PRE_MEM_READ(tst, "KIMD(op2)", addr2, len2);
      cc = do_KIMD_insn(func, parms, &addr2, &len2);
      WRITE_GPR(tst, r2, addr2);
      WRITE_GPR(tst, r2 + 1, len2);
      POST_MEM_WRITE(tst, parms, parms_len);
   }
   WRITE_CC(tst, cc);
   return ExtErr_OK;
}

/*---------------------------------------------------------------*/
/*--- KLMD (compute last message digest)                      ---*/
/*---------------------------------------------------------------*/

S390_DEFINE_DO_RR_INSN(do_KLMD_insn, 0xb93f)

#define S390_DO_FUNCTIONS                                                      \
   S390_DO_FUNC(0, S390_KLMD_Query, 16)                                        \
   S390_DO_FUNC(1, S390_KLMD_SHA_1, 28)                                        \
   S390_DO_FUNC(2, S390_KLMD_SHA_256, 40)                                      \
   S390_DO_FUNC(3, S390_KLMD_SHA_512, 80)                                      \
   S390_DO_FUNC(32, S390_KLMD_SHA3_224, 200)                                   \
   S390_DO_FUNC(33, S390_KLMD_SHA3_256, 200)                                   \
   S390_DO_FUNC(34, S390_KLMD_SHA3_384, 200)                                   \
   S390_DO_FUNC(35, S390_KLMD_SHA3_512, 200)                                   \
   S390_DO_FUNC(36, S390_KLMD_SHAKE_128, 200)                                  \
   S390_DO_FUNC(37, S390_KLMD_SHAKE_256, 200)

#define S390_DO_FUNC(fc, name, plen) [fc] = plen,
static const UChar S390_KLMD_parms_len[] = {S390_DO_FUNCTIONS};
#undef S390_DO_FUNC

#define S390_DO_FUNC(fc, name, plen) | S390_SETBIT(fc)
static const ULong S390_KLMD_supported_fc[] = {0 S390_DO_FUNCTIONS};
#undef S390_DO_FUNC

#undef S390_DO_FUNCTIONS

static enum ExtensionError do_extension_KLMD(ThreadState* tst, ULong variant)
{
   UChar r1         = variant & 0xf;
   UChar r2         = (variant >> 4) & 0xf;
   ULong func       = READ_GPR(tst, "KLMD(r0)", 0);
   UChar fc         = func & 0x7f;
   ULong parms      = READ_GPR(tst, "KLMD(r1)", 1);
   ULong parms_len  = 0;
   Int   cc         = 0;
   ULong orig_addr1 = 0, orig_len1 = 0;
   ULong addr1 = 0, len1 = 0, addr2 = 0, len2 = 0;

   if (fc < sizeof(S390_KLMD_parms_len) / sizeof(S390_KLMD_parms_len[0]))
      parms_len = S390_KLMD_parms_len[fc];
   if (parms_len == 0)
      return INSN_ERR("KLMD: unknown function code\n");
   PRE_MEM_WRITE(tst, "KLMD(parms)", parms, parms_len);

   if (fc == 0) { // Query
      cc = do_KLMD_insn(func, parms, &addr1, &len1, &addr2, &len2);
      s390_filter_functions((ULong*)parms, parms_len, S390_KLMD_supported_fc,
                            sizeof(S390_KLMD_supported_fc));
   } else {
      /* The "shake" functions use the first operand */
      Bool have_op1 = fc >= 36 && fc <= 37;

      PRE_MEM_READ(tst, "KLMD(parms)", parms, parms_len);
      if (have_op1) {
         if (r1 == 0 || r1 % 2 != 0)
            return INSN_ERR("KLMD: bad r1 field");
         addr1 = orig_addr1 = READ_GPR(tst, "KLMD(op1_addr)", r1);
         len1 = orig_len1 = READ_GPR(tst, "KLMD(op1_len)", r1 + 1);
         PRE_MEM_WRITE(tst, "KLMD(op1)", addr1, len1);
      }
      addr2 = READ_GPR(tst, "KLMD(op2_addr)", r2);
      len2  = READ_GPR(tst, "KLMD(op2_len)", r2 + 1);
      PRE_MEM_READ(tst, "KLMD(op2)", addr2, len2);
      cc = do_KLMD_insn(func, parms, &addr1, &len1, &addr2, &len2);
      if (have_op1) {
         WRITE_GPR(tst, r1, addr1);
         WRITE_GPR(tst, r1 + 1, len1);
         POST_MEM_WRITE(tst, orig_addr1, orig_len1 - len1);
      }
      WRITE_GPR(tst, r2, addr2);
      WRITE_GPR(tst, r2 + 1, len2);
   }
   POST_MEM_WRITE(tst, parms, parms_len);
   WRITE_CC(tst, cc);
   return ExtErr_OK;
}

/*---------------------------------------------------------------*/
/*--- KMAC (compute message authentication code)              ---*/
/*---------------------------------------------------------------*/

S390_DEFINE_DO_0R_INSN(do_KMAC_insn, 0xb91e)

#define S390_DO_FUNCTIONS                                                      \
   S390_DO_FUNC(0, S390_KMAC_Query, 16)                                        \
   S390_DO_FUNC(1, S390_KMAC_DEA, 16)                                          \
   S390_DO_FUNC(2, S390_KMAC_TDEA_128, 24)                                     \
   S390_DO_FUNC(3, S390_KMAC_TDEA_192, 32)                                     \
   S390_DO_FUNC(9, S390_KMAC_Encrypted_DEA, 40)                                \
   S390_DO_FUNC(10, S390_KMAC_Encrypted_TDEA_128, 48)                          \
   S390_DO_FUNC(11, S390_KMAC_Encrypted_TDEA_192, 56)                          \
   S390_DO_FUNC(18, S390_KMAC_AES_128, 32)                                     \
   S390_DO_FUNC(19, S390_KMAC_AES_192, 40)                                     \
   S390_DO_FUNC(20, S390_KMAC_AES_256, 48)                                     \
   S390_DO_FUNC(26, S390_KMAC_Encrypted_AES_128, 64)                           \
   S390_DO_FUNC(27, S390_KMAC_Encrypted_AES_192, 72)                           \
   S390_DO_FUNC(28, S390_KMAC_Encrypted_AES_256, 80)

#define S390_DO_FUNC(fc, name, plen) [fc] = plen,
static const UChar S390_KMAC_parms_len[] = {S390_DO_FUNCTIONS};
#undef S390_DO_FUNC

#define S390_DO_FUNC(fc, name, plen) | S390_SETBIT(fc)
static const ULong S390_KMAC_supported_fc[] = {0 S390_DO_FUNCTIONS};
#undef S390_DO_FUNC

#undef S390_DO_FUNCTIONS

static enum ExtensionError do_extension_KMAC(ThreadState* tst, ULong variant)
{
   UChar r2        = (variant >> 4) & 0xf;
   UChar func      = READ_FUNCTION_CODE(tst, "KMAC");
   UChar fc        = func & 0x7f;
   ULong parms     = READ_GPR(tst, "KMAC(r1)", 1);
   ULong parms_len = 0;
   Int   cc        = 0;
   ULong addr2 = 0, len2 = 0;

   if (fc < sizeof(S390_KMAC_parms_len) / sizeof(S390_KMAC_parms_len[0]))
      parms_len = S390_KMAC_parms_len[fc];
   if (parms_len == 0)
      return INSN_ERR("KMAC: unknown function code\n");

   if (fc == 0) { // Query
      PRE_MEM_WRITE(tst, "KMAC(parms)", parms, parms_len);
      cc = do_KMAC_insn(func, parms, &addr2, &len2);
      s390_filter_functions((ULong*)parms, parms_len, S390_KMAC_supported_fc,
                            sizeof(S390_KMAC_supported_fc));
      POST_MEM_WRITE(tst, parms, parms_len);
   } else {
      addr2 = READ_GPR(tst, "KMAC(op2_addr)", r2);
      len2  = READ_GPR(tst, "KMAC(op2_len)", r2 + 1);
      PRE_MEM_READ(tst, "KMAC(parms)", parms, parms_len);
      PRE_MEM_WRITE(tst, "KMAC(parms)", parms, parms_len);
      PRE_MEM_READ(tst, "KMAC(op2)", addr2, len2);
      cc = do_KMAC_insn(func, parms, &addr2, &len2);
      WRITE_GPR(tst, r2, addr2);
      WRITE_GPR(tst, r2 + 1, len2);
      if (cc != 1)
         POST_MEM_WRITE(tst, parms, parms_len);
   }
   WRITE_CC(tst, cc);
   return ExtErr_OK;
}

/*---------------------------------------------------------------*/
/*--- PCC (perform cryptographic computation)                 ---*/
/*---------------------------------------------------------------*/

S390_DEFINE_DO_00_INSN(do_PCC_insn, 0xb92c)

enum PCC_function_class {
   PCC_Unassigned = 0,
   PCC_Query,
   PCC_Compute_Last_Block_CMAC,
   PCC_Compute_XTS_Parameter,
   PCC_Scalar_Multiply,
};

#define S390_DO_FUNCTIONS                                                      \
   S390_DO_FUNC(0, PCC_Query, Query, 16, 16)                                   \
   S390_DO_FUNC(1, PCC_Compute_Last_Block_CMAC, Using_DEA, 8, 32)              \
   S390_DO_FUNC(2, PCC_Compute_Last_Block_CMAC, Using_TDEA_128, 8, 40)         \
   S390_DO_FUNC(3, PCC_Compute_Last_Block_CMAC, Using_TDEA_192, 8, 48)         \
   S390_DO_FUNC(9, PCC_Compute_Last_Block_CMAC, Using_Encrypted_DEA, 8, 56)    \
   S390_DO_FUNC(10, PCC_Compute_Last_Block_CMAC, Using_Encrypted_TDEA_128, 8,  \
                64)                                                            \
   S390_DO_FUNC(11, PCC_Compute_Last_Block_CMAC, Using_Encrypted_TDEA_192, 8,  \
                72)                                                            \
   S390_DO_FUNC(18, PCC_Compute_Last_Block_CMAC, Using_AES_128, 16, 56)        \
   S390_DO_FUNC(19, PCC_Compute_Last_Block_CMAC, Using_AES_192, 16, 64)        \
   S390_DO_FUNC(20, PCC_Compute_Last_Block_CMAC, Using_AES_256, 16, 72)        \
   S390_DO_FUNC(26, PCC_Compute_Last_Block_CMAC, Using_Encrypted_AES_128, 16,  \
                88)                                                            \
   S390_DO_FUNC(27, PCC_Compute_Last_Block_CMAC, Using_Encrypted_AES_192, 16,  \
                96)                                                            \
   S390_DO_FUNC(28, PCC_Compute_Last_Block_CMAC, Using_Encrypted_AES_256, 16,  \
                104)                                                           \
   S390_DO_FUNC(50, PCC_Compute_XTS_Parameter, Using_AES_128, 16, 80)          \
   S390_DO_FUNC(52, PCC_Compute_XTS_Parameter, Using_AES_256, 16, 96)          \
   S390_DO_FUNC(58, PCC_Compute_XTS_Parameter, Using_Encrypted_AES_128, 16,    \
                112)                                                           \
   S390_DO_FUNC(60, PCC_Compute_XTS_Parameter, Using_Encrypted_AES_256, 16, 128)

#define S390_DO_FUNCTIONS1                                                     \
   S390_DO_FUNC(64, PCC_Scalar_Multiply, P256, 64, 168)                        \
   S390_DO_FUNC(65, PCC_Scalar_Multiply, P384, 96, 248)                        \
   S390_DO_FUNC(66, PCC_Scalar_Multiply, P521, 160, 408)                       \
   S390_DO_FUNC(72, PCC_Scalar_Multiply, Ed25519, 64, 168)                     \
   S390_DO_FUNC(73, PCC_Scalar_Multiply, Ed448, 128, 328)                      \
   S390_DO_FUNC(80, PCC_Scalar_Multiply, X25519, 32, 104)                      \
   S390_DO_FUNC(81, PCC_Scalar_Multiply, X448, 64, 200)

#define S390_DO_FUNC(fc, class, name, rlen, plen)                              \
   [fc] = {class, rlen / 8, plen / 8},
static const struct {
   UChar fc_class : 3;
   UChar result_len : 5;
   UChar parms_len;
} S390_PCC_fc_info[] = {S390_DO_FUNCTIONS S390_DO_FUNCTIONS1};
#undef S390_DO_FUNC

#define S390_DO_FUNC(fc, class, name, rlen, plen) | S390_SETBIT(fc)
static const ULong S390_PCC_supported_fc[] = {0 S390_DO_FUNCTIONS,
                                              0 S390_DO_FUNCTIONS1};
#undef S390_DO_FUNC

#undef S390_DO_FUNCTIONS
#undef S390_DO_FUNCTIONS1

static enum ExtensionError do_extension_PCC(ThreadState* tst, ULong variant)
{
   UChar func      = READ_FUNCTION_CODE(tst, "PCC");
   UChar fc        = func & 0x7f;
   ULong parms     = READ_GPR(tst, "PCC(r1)", 1);
   ULong parms_len = 0;
   Int   cc        = 0;
   UChar fc_class  = PCC_Unassigned;
   ULong result_offs, result_len, msg_len;

   if (fc < sizeof(S390_PCC_fc_info) / sizeof(S390_PCC_fc_info[0])) {
      fc_class   = S390_PCC_fc_info[fc].fc_class;
      result_len = S390_PCC_fc_info[fc].result_len * 8;
      parms_len  = S390_PCC_fc_info[fc].parms_len * 8;
   }

   switch (fc_class) {
   case PCC_Query:
      PRE_MEM_WRITE(tst, "PCC(parms)", parms, parms_len);
      cc = do_PCC_insn(func, parms);
      s390_filter_functions((ULong*)parms, parms_len, S390_PCC_supported_fc,
                            sizeof(S390_PCC_supported_fc));
      result_offs = 0;
      break;
   case PCC_Compute_Last_Block_CMAC:
      /* result_len == sizeof(ICV) == sizeof(message) */
      PRE_MEM_READ(tst, "PCC(parms)", parms, 8);
      msg_len = (*(UChar*)parms + 7) / 8;
      if (msg_len > result_len)
         msg_len = result_len;
      if (msg_len != 0) {
         PRE_MEM_READ(tst, "PCC(parms)", parms + 8, msg_len);
      }
      result_offs = 8 + result_len;
      PRE_MEM_READ(tst, "PCC(parms)", parms + result_offs,
                   parms_len - result_offs);
      PRE_MEM_WRITE(tst, "PCC(parms.CMAC)", parms + result_offs, result_len);
      cc = do_PCC_insn(func, parms);
      break;
   case PCC_Compute_XTS_Parameter:
      /* result_len == sizeof(XTS parameter)  */
      result_offs = parms_len - result_len;
      PRE_MEM_READ(tst, "PCC(parms)", parms, result_offs - 16);
      if (*(ULong*)(parms + result_offs - 32) != 0) {
         /* block sequential number non-zero -> read intermediate bit index t */
         result_offs -= 16;
         result_len += 16;
         PRE_MEM_READ(tst, "PCC(parms.t)", parms + result_offs, 16);
         if (*(ULong*)(parms + result_offs) != 0) {
            /* t != 0: read partial XTS parameter */
            PRE_MEM_READ(tst, "PCC(parms.XTS)", parms + result_offs + 16,
                         result_len);
         }
      }
      PRE_MEM_WRITE(tst, "PCC(parms)", parms + result_offs, result_len);
      cc = do_PCC_insn(func, parms);
      break;
   case PCC_Scalar_Multiply:
      /* result_len == sizeof(result) == sizeof(source) */
      result_offs = 0;
      PRE_MEM_READ(tst, "PCC(parms)", parms + result_len,
                   parms_len - result_len);
      PRE_MEM_WRITE(tst, "PCC(parms)", parms, 4096);
      if (*(ULong*)(parms + parms_len - 8) != 0) {
         /* continuation -> read the continuation state buffer as well */
         PRE_MEM_READ(tst, "PCC(parms.CSB)", parms + parms_len,
                      4096 - parms_len);
      }
      cc = do_PCC_insn(func, parms);
      break;
   default:
      return INSN_ERR("PCC: unknown function code\n");
   }

   if (cc == 0 || cc == 3) // normal or partial completion
      POST_MEM_WRITE(tst, parms + result_offs, result_len);
   WRITE_CC(tst, cc);
   return ExtErr_OK;
}

/*---------------------------------------------------------------*/
/*--- KMCTR (cypher message with counter)                     ---*/
/*---------------------------------------------------------------*/

S390_DEFINE_DO_RRR_INSN(do_KMCTR_insn, 0xb92d)

#define S390_DO_FUNCTIONS                                                      \
   S390_DO_FUNC(0, S390_KMCTR_Query, 16)                                       \
   S390_DO_FUNC(1, S390_KMCTR_DEA, 8)                                          \
   S390_DO_FUNC(2, S390_KMCTR_TDEA_128, 16)                                    \
   S390_DO_FUNC(3, S390_KMCTR_TDEA_192, 24)                                    \
   S390_DO_FUNC(9, S390_KMCTR_Encrypted_DEA, 32)                               \
   S390_DO_FUNC(10, S390_KMCTR_Encrypted_TDEA_128, 40)                         \
   S390_DO_FUNC(11, S390_KMCTR_Encrypted_TDEA_192, 48)                         \
   S390_DO_FUNC(18, S390_KMCTR_AES_128, 16)                                    \
   S390_DO_FUNC(19, S390_KMCTR_AES_192, 24)                                    \
   S390_DO_FUNC(20, S390_KMCTR_AES_256, 32)                                    \
   S390_DO_FUNC(26, S390_KMCTR_Encrypted_AES_128, 48)                          \
   S390_DO_FUNC(27, S390_KMCTR_Encrypted_AES_192, 56)                          \
   S390_DO_FUNC(28, S390_KMCTR_Encrypted_AES_256, 64)

#define S390_DO_FUNC(fc, name, plen) [fc] = plen,
static const UChar S390_KMCTR_parms_len[] = {S390_DO_FUNCTIONS};
#undef S390_DO_FUNC

#define S390_DO_FUNC(fc, name, plen) | S390_SETBIT(fc)
static const ULong S390_KMCTR_supported_fc[] = {0 S390_DO_FUNCTIONS};
#undef S390_DO_FUNC

#undef S390_DO_FUNCTIONS

enum {
   S390_KMCTR_parms_len_n =
      sizeof(S390_KMCTR_parms_len) / sizeof(S390_KMCTR_parms_len[0])
};

static enum ExtensionError do_extension_KMCTR(ThreadState* tst, ULong variant)
{
   UChar r1        = variant & 0xf;
   UChar r2        = (variant >> 4) & 0xf;
   UChar r3        = (variant >> 8) & 0xf;
   UChar func      = READ_FUNCTION_CODE(tst, "KMCTR");
   UChar fc        = func & 0x7f;
   ULong parms     = READ_GPR(tst, "KMCTR(r1)", 1);
   ULong parms_len = 0;
   Int   cc        = 0;
   ULong orig_addr1, orig_len2 = 0;
   ULong addr1 = 0, addr2 = 0, addr3 = 0, len2 = 0, len3 = 0;

   if (fc < S390_KMCTR_parms_len_n)
      parms_len = S390_KMCTR_parms_len[fc];
   if (parms_len == 0)
      return INSN_ERR("KMCTR: unknown function code\n");

   if (fc == 0) { // Query
      PRE_MEM_WRITE(tst, "KMCTR(parms)", parms, parms_len);
      cc = do_KMCTR_insn(func, parms, &addr1, &addr2, &len2, &addr3, &len3);
      s390_filter_functions((ULong*)parms, parms_len, S390_KMCTR_supported_fc,
                            sizeof(S390_KMCTR_supported_fc));
      POST_MEM_WRITE(tst, parms, parms_len);
   } else {
      addr1 = orig_addr1 = READ_GPR(tst, "KMCTR(op1_addr)", r1);
      addr2              = READ_GPR(tst, "KMCTR(op2_addr)", r2);
      addr3              = READ_GPR(tst, "KMCTR(op3_addr)", r3);
      len2 = orig_len2 = READ_GPR(tst, "KMCTR(op2_len)", r2 + 1);
      PRE_MEM_READ(tst, "KMCTR(parms)", parms, parms_len);
      PRE_MEM_WRITE(tst, "KMCTR(op1)", addr1, len2);
      PRE_MEM_READ(tst, "KMCTR(op2)", addr2, len2);
      PRE_MEM_READ(tst, "KMCTR(op3)", addr3, len2);
      cc = do_KMCTR_insn(func, parms, &addr1, &addr2, &len2, &addr3, &len3);
      WRITE_GPR(tst, r1, addr1);
      WRITE_GPR(tst, r2, addr2);
      WRITE_GPR(tst, r2 + 1, len2);
      WRITE_GPR(tst, r3, addr3);
      POST_MEM_WRITE(tst, orig_addr1, orig_len2 - len2);
   }
   WRITE_CC(tst, cc);
   return ExtErr_OK;
}

/*---------------------------------------------------------------*/
/*--- KMO (cypher message with output feedback)               ---*/
/*---------------------------------------------------------------*/

S390_DEFINE_DO_RR_INSN(do_KMO_insn, 0xb92b)

/* Same functions and parameter block sizes as for KMCTR */
static const UChar* const S390_KMO_parms_len    = S390_KMCTR_parms_len;
static const ULong* const S390_KMO_supported_fc = S390_KMCTR_supported_fc;
enum { S390_KMO_parms_len_n = S390_KMCTR_parms_len_n };

static enum ExtensionError do_extension_KMO(ThreadState* tst, ULong variant)
{
   UChar r1        = variant & 0xf;
   UChar r2        = (variant >> 4) & 0xf;
   UChar func      = READ_FUNCTION_CODE(tst, "KMO");
   UChar fc        = func & 0x7f;
   ULong parms     = READ_GPR(tst, "KMO(r1)", 1);
   ULong parms_len = 0;
   Int   cc        = 0;
   ULong orig_addr1, orig_len2 = 0;
   ULong addr1 = 0, len1 = 0, addr2 = 0, len2 = 0;

   if (fc < S390_KMO_parms_len_n)
      parms_len = S390_KMO_parms_len[fc];
   if (parms_len == 0)
      return INSN_ERR("KMO: unknown function code\n");

   PRE_MEM_WRITE(tst, "KMO(parms)", parms, parms_len);
   if (fc == 0) { // Query
      cc = do_KMO_insn(func, parms, &addr1, &len1, &addr2, &len2);
      s390_filter_functions((ULong*)parms, parms_len, S390_KMO_supported_fc,
                            sizeof(S390_KMO_supported_fc));
   } else {
      addr1 = orig_addr1 = READ_GPR(tst, "KMO(op1_addr)", r1);
      addr2              = READ_GPR(tst, "KMO(op2_addr)", r2);
      len2 = orig_len2 = READ_GPR(tst, "KMO(op2_len)", r2 + 1);
      PRE_MEM_READ(tst, "KMO(parms)", parms, parms_len);
      PRE_MEM_WRITE(tst, "KMO(op1)", addr1, len2);
      PRE_MEM_READ(tst, "KMO(op2)", addr2, len2);
      cc = do_KMO_insn(func, parms, &addr1, &len1, &addr2, &len2);
      WRITE_GPR(tst, r1, addr1);
      WRITE_GPR(tst, r2, addr2);
      WRITE_GPR(tst, r2 + 1, len2);
      POST_MEM_WRITE(tst, orig_addr1, orig_len2 - len2);
   }
   POST_MEM_WRITE(tst, parms, parms_len);
   WRITE_CC(tst, cc);
   return ExtErr_OK;
}

/*---------------------------------------------------------------*/
/*--- KMF (cypher message with output feedback)               ---*/
/*---------------------------------------------------------------*/

S390_DEFINE_DO_RR_INSN(do_KMF_insn, 0xb92a)

/* Same functions and parameter block sizes as for KMCTR */
static const UChar* const S390_KMF_parms_len    = S390_KMCTR_parms_len;
static const ULong* const S390_KMF_supported_fc = S390_KMCTR_supported_fc;
enum { S390_KMF_parms_len_n = S390_KMCTR_parms_len_n };

static enum ExtensionError do_extension_KMF(ThreadState* tst, ULong variant)
{
   UChar r1        = variant & 0xf;
   UChar r2        = (variant >> 4) & 0xf;
   ULong func      = READ_GPR(tst, "KLMD(r0)", 0);
   UChar fc        = func & 0x7f;
   ULong parms     = READ_GPR(tst, "KMF(r1)", 1);
   ULong parms_len = 0;
   Int   cc        = 0;
   ULong orig_addr1, orig_len2 = 0;
   ULong addr1 = 0, len1 = 0, addr2 = 0, len2 = 0;

   if (fc < S390_KMF_parms_len_n)
      parms_len = S390_KMF_parms_len[fc];
   if (parms_len == 0)
      return INSN_ERR("KMF: unknown function code\n");

   PRE_MEM_WRITE(tst, "KMF(parms)", parms, parms_len);
   if (fc == 0) { // Query
      cc = do_KMF_insn(func, parms, &addr1, &len1, &addr2, &len2);
      s390_filter_functions((ULong*)parms, parms_len, S390_KMF_supported_fc,
                            sizeof(S390_KMF_supported_fc));
   } else {
      addr1 = orig_addr1 = READ_GPR(tst, "KMF(op1_addr)", r1);
      addr2              = READ_GPR(tst, "KMF(op2_addr)", r2);
      len2 = orig_len2 = READ_GPR(tst, "KMF(op2_len)", r2 + 1);
      PRE_MEM_READ(tst, "KMF(parms)", parms, parms_len);
      PRE_MEM_WRITE(tst, "KMF(op1)", addr1, len2);
      PRE_MEM_READ(tst, "KMF(op2)", addr2, len2);
      cc = do_KMF_insn(func, parms, &addr1, &len1, &addr2, &len2);
      WRITE_GPR(tst, r1, addr1);
      WRITE_GPR(tst, r2, addr2);
      WRITE_GPR(tst, r2 + 1, len2);
      POST_MEM_WRITE(tst, orig_addr1, orig_len2 - len2);
   }
   POST_MEM_WRITE(tst, parms, parms_len);
   WRITE_CC(tst, cc);
   return ExtErr_OK;
}

/*---------------------------------------------------------------*/
/*--- KMA (cypher message with authentication)                ---*/
/*---------------------------------------------------------------*/

S390_DEFINE_DO_RRR_INSN(do_KMA_insn, 0xb929)

#define S390_DO_FUNCTIONS                                                      \
   S390_DO_FUNC(0, S390_KMA_Query, 16)                                         \
   S390_DO_FUNC(18, S390_KMA_GCM_AES_128, 96)                                  \
   S390_DO_FUNC(19, S390_KMA_GCM_AES_192, 104)                                 \
   S390_DO_FUNC(20, S390_KMA_GCM_AES_256, 112)                                 \
   S390_DO_FUNC(26, S390_KMA_GCM_Encrypted_AES_128, 128)                       \
   S390_DO_FUNC(27, S390_KMA_GCM_Encrypted_AES_192, 136)                       \
   S390_DO_FUNC(28, S390_KMA_GCM_Encrypted_AES_256, 144)

#define S390_DO_FUNC(fc, name, plen) [fc] = plen,
static const UChar S390_KMA_parms_len[] = {S390_DO_FUNCTIONS};
#undef S390_DO_FUNC

#define S390_DO_FUNC(fc, name, plen) | S390_SETBIT(fc)
static const ULong S390_KMA_supported_fc[] = {0 S390_DO_FUNCTIONS};
#undef S390_DO_FUNC

#undef S390_DO_FUNCTIONS

static enum ExtensionError do_extension_KMA(ThreadState* tst, ULong variant)
{
   UChar r1        = variant & 0xf;
   UChar r2        = (variant >> 4) & 0xf;
   UChar r3        = (variant >> 8) & 0xf;
   ULong func      = READ_GPR(tst, "KMA(gpr0)", 0);
   UChar fc        = func & 0x7f;
   ULong parms     = READ_GPR(tst, "KMA(r1)", 1);
   ULong parms_len = 0;
   Int   cc        = 0;
   ULong orig_addr1, orig_len2;
   ULong addr1 = 0, addr2 = 0, addr3 = 0, len2 = 0, len3 = 0;

   if (fc < sizeof(S390_KMA_parms_len) / sizeof(S390_KMA_parms_len[0]))
      parms_len = S390_KMA_parms_len[fc];
   if (parms_len == 0)
      return INSN_ERR("KMA: unknown function code\n");

   if (fc == 0) { // Query
      PRE_MEM_WRITE(tst, "KMA(parms)", parms, parms_len);
      cc = do_KMA_insn(func, parms, &addr1, &addr2, &len2, &addr3, &len3);
      s390_filter_functions((ULong*)parms, parms_len, S390_KMA_supported_fc,
                            sizeof(S390_KMA_supported_fc));
      POST_MEM_WRITE(tst, parms, parms_len);
   } else {
      addr1 = orig_addr1 = READ_GPR(tst, "KMA(op1_addr)", r1);
      addr2              = READ_GPR(tst, "KMA(op2_addr)", r2);
      addr3              = READ_GPR(tst, "KMA(op3_addr)", r3);
      len2 = orig_len2 = READ_GPR(tst, "KMA(op2_len)", r2 + 1);
      len3             = READ_GPR(tst, "KMA(op3_len)", r3 + 1);
      PRE_MEM_READ(tst, "KMA(parms)", parms, parms_len);
      PRE_MEM_WRITE(tst, "KMA(op1)", addr1, len2);
      PRE_MEM_READ(tst, "KMA(op2)", addr2, len2);
      PRE_MEM_READ(tst, "KMA(op3)", addr3, len3);
      cc = do_KMA_insn(func, parms, &addr1, &addr2, &len2, &addr3, &len3);
      WRITE_GPR(tst, r1, addr1);
      WRITE_GPR(tst, r2, addr2);
      WRITE_GPR(tst, r2 + 1, len2);
      WRITE_GPR(tst, r3, addr3);
      WRITE_GPR(tst, r3 + 1, len3);
      POST_MEM_WRITE(tst, orig_addr1, orig_len2 - len2);
   }
   WRITE_CC(tst, cc);
   return ExtErr_OK;
}

/*---------------------------------------------------------------*/
/*--- KDSA (compute intermediate message digest)              ---*/
/*---------------------------------------------------------------*/

S390_DEFINE_DO_0R_INSN(do_KDSA_insn, 0xb93a)

/* We specify the parameter block size without the CSB here.  Also note that
   this approach only supports sizes that are a multiple of 8. */
#define S390_DO_FUNCTIONS                                                      \
   S390_DO_FUNC(0, S390_KDSA_Query, 16)                                        \
   S390_DO_FUNC(1, S390_KDSA_ECDSA_Verify_P256, 168)                           \
   S390_DO_FUNC(2, S390_KDSA_ECDSA_Verify_P384, 248)                           \
   S390_DO_FUNC(3, S390_KDSA_ECDSA_Verify_P521, 408)                           \
   S390_DO_FUNC(9, S390_KDSA_ECDSA_Sign_P256, 168)                             \
   S390_DO_FUNC(10, S390_KDSA_ECDSA_Sign_P384, 248)                            \
   S390_DO_FUNC(11, S390_KDSA_ECDSA_Sign_P521, 408)                            \
   S390_DO_FUNC(17, S390_KDSA_Encrypted_ECDSA_Sign_P256, 200)                  \
   S390_DO_FUNC(18, S390_KDSA_Encrypted_ECDSA_Sign_P384, 280)                  \
   S390_DO_FUNC(19, S390_KDSA_Encrypted_ECDSA_Sign_P521, 440)                  \
   S390_DO_FUNC(32, S390_KDSA_EdDSA_Verify_Ed25519, 104)                       \
   S390_DO_FUNC(36, S390_KDSA_EdDSA_Verify_Ed448, 200)                         \
   S390_DO_FUNC(40, S390_KDSA_EdDSA_Sign_Ed25519, 120)                         \
   S390_DO_FUNC(44, S390_KDSA_EdDSA_Sign_Ed448, 216)                           \
   S390_DO_FUNC(48, S390_KDSA_Encrypted_EdDSA_Sign_Ed25519, 152)               \
   S390_DO_FUNC(52, S390_KDSA_Encrypted_EdDSA_Sign_Ed448, 248)

#define S390_DO_FUNC(fc, name, plen) [fc] = plen / 8,
static const UChar S390_KDSA_parms_len[] = {S390_DO_FUNCTIONS};
#undef S390_DO_FUNC

#define S390_DO_FUNC(fc, name, plen) | S390_SETBIT(fc)
static const ULong S390_KDSA_supported_fc[] = {0 S390_DO_FUNCTIONS};
#undef S390_DO_FUNC

#undef S390_DO_FUNCTIONS

static enum ExtensionError do_extension_KDSA(ThreadState* tst, ULong variant)
{
   UChar r2        = (variant >> 4) & 0xf;
   UChar func      = READ_FUNCTION_CODE(tst, "KDSA");
   UChar fc        = func & 0x7f;
   ULong parms     = READ_GPR(tst, "KDSA(r1)", 1);
   ULong parms_len = 0;
   Int   cc        = 0;
   ULong addr2 = 0, len2 = 0;

   if (fc < sizeof(S390_KDSA_parms_len) / sizeof(S390_KDSA_parms_len[0]))
      parms_len = S390_KDSA_parms_len[fc] * 8;
   if (parms_len == 0)
      return INSN_ERR("KDSA: unknown function code\n");

   if (fc == 0) { // Query
      PRE_MEM_WRITE(tst, "KDSA(parms)", parms, parms_len);
      cc = do_KDSA_insn(func, parms, &addr2, &len2);
      s390_filter_functions((ULong*)parms, parms_len, S390_KDSA_supported_fc,
                            sizeof(S390_KDSA_supported_fc));
      POST_MEM_WRITE(tst, parms, parms_len);
   } else {
      addr2 = READ_GPR(tst, "KDSA(op2_addr)", r2);
      len2  = READ_GPR(tst, "KDSA(op2_len)", r2 + 1);
      PRE_MEM_READ(tst, "KDSA(parms)", parms, parms_len);
      /* the CSB must also be writable */
      PRE_MEM_WRITE(tst, "KDSA(parms)", parms, 4096);
      PRE_MEM_READ(tst, "KDSA(op2)", addr2, len2);
      cc = do_KDSA_insn(func, parms, &addr2, &len2);
      WRITE_GPR(tst, r2, addr2);
      WRITE_GPR(tst, r2 + 1, len2);
      POST_MEM_WRITE(tst, parms, parms_len);
   }
   WRITE_CC(tst, cc);
   return ExtErr_OK;
}

/*---------------------------------------------------------------*/
/*--- Main function: select and call appropriate extension    ---*/
/*---------------------------------------------------------------*/

enum ExtensionError ML_(do_client_extension)(ThreadState* tst)
{
   ULong code    = REG_READ(tst, SYSNO);
   ULong id      = code & ((1ULL << S390_EXT_ID_NBITS) - 1);
   ULong variant = code >> S390_EXT_ID_NBITS;

   switch (id) {
   case S390_EXT_PRNO:
      return do_extension_PRNO(tst, variant);
   case S390_EXT_NNPA:
      return do_extension_NNPA(tst, variant);
   case S390_EXT_DFLT:
      return do_extension_DFLTCC(tst, variant);
   case S390_EXT_STFLE:
      return do_extension_STFLE(tst, variant);
   case S390_EXT_KM:
      return do_extension_KM(tst, variant);
   case S390_EXT_KMC:
      return do_extension_KMC(tst, variant);
   case S390_EXT_KIMD:
      return do_extension_KIMD(tst, variant);
   case S390_EXT_KLMD:
      return do_extension_KLMD(tst, variant);
   case S390_EXT_KMAC:
      return do_extension_KMAC(tst, variant);
   case S390_EXT_PCC:
      return do_extension_PCC(tst, variant);
   case S390_EXT_KMCTR:
      return do_extension_KMCTR(tst, variant);
   case S390_EXT_KMO:
      return do_extension_KMO(tst, variant);
   case S390_EXT_KMF:
      return do_extension_KMF(tst, variant);
   case S390_EXT_KMA:
      return do_extension_KMA(tst, variant);
   case S390_EXT_KDSA:
      return do_extension_KDSA(tst, variant);
   default:
      VG_(core_panic)("unknown extension ID");
   }
}

#endif
