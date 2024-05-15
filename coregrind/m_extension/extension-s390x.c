
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

#define S390_SETBIT(x) (1UL << (63 - (x % 64)))

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

static Int do_PRNO_insn(UChar  func,
                        ULong  parms,
                        ULong* addr1,
                        ULong* len1,
                        ULong* addr2,
                        ULong* len2)
{
   register UChar reg0 asm("0") = func;
   register void* reg1 asm("1") = (void*)parms;
   union reg_pair op1           = {{*addr1, *len1}};
   union reg_pair op2           = {{*addr2, *len2}};
   Int            cc;

   asm volatile(".insn rre, 0xb93c0000, %[op1], %[op2]\n"
                "ipm %[cc]\n"
                "srl %[cc], 28\n"
                : [cc] "=d"(cc), [op1] "+a"(op1.pair), [op2] "+a"(op2.pair)
                : "d"(reg0), "d"(reg1)
                : "cc", "memory");
   *addr1 = op1.a;
   *len1  = op1.b;
   *addr2 = op2.a;
   *len2  = op2.b;
   return cc;
}

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
         WRITE_GPR(tst, r2 + 1, len1);
         POST_MEM_WRITE(tst, orig_addr1, orig_len1 - len1);
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
   S390_NNPA_RELU              = 0x31,
   S390_NNPA_TANH              = 0x32,
   S390_NNPA_SIGMOID           = 0x33,
   S390_NNPA_SOFTMAX           = 0x34,
   S390_NNPA_BATCHNORM         = 0x40,
   S390_NNPA_MAXPOOL2D         = 0x50,
   S390_NNPA_AVGPOOL2D         = 0x51,
   S390_NNPA_LSTMACT           = 0x60,
   S390_NNPA_GRUACT            = 0x61,
   S390_NNPA_CONVOLUTION       = 0x70,
   S390_NNPA_MATMUL_OP         = 0x71,
   S390_NNPA_MATMUL_OP_BCAST23 = 0x72,
} s390_nnpa_function_t;

/* Suported NNPA functions */
static const ULong NNPA_functions[] = {
   (S390_SETBIT(S390_NNPA_QAF) | S390_SETBIT(S390_NNPA_ADD) |
    S390_SETBIT(S390_NNPA_SUB) | S390_SETBIT(S390_NNPA_MUL) |
    S390_SETBIT(S390_NNPA_DIV) | S390_SETBIT(S390_NNPA_MIN) |
    S390_SETBIT(S390_NNPA_MAX) | S390_SETBIT(S390_NNPA_LOG) |
    S390_SETBIT(S390_NNPA_EXP) | S390_SETBIT(S390_NNPA_RELU) |
    S390_SETBIT(S390_NNPA_TANH) | S390_SETBIT(S390_NNPA_SIGMOID) |
    S390_SETBIT(S390_NNPA_SOFTMAX)),
   (S390_SETBIT(S390_NNPA_BATCHNORM) | S390_SETBIT(S390_NNPA_MAXPOOL2D) |
    S390_SETBIT(S390_NNPA_AVGPOOL2D) | S390_SETBIT(S390_NNPA_LSTMACT) |
    S390_SETBIT(S390_NNPA_GRUACT) | S390_SETBIT(S390_NNPA_CONVOLUTION) |
    S390_SETBIT(S390_NNPA_MATMUL_OP) |
    S390_SETBIT(S390_NNPA_MATMUL_OP_BCAST23)),
};

/* Supported parameter block formats */
static const ULong NNPA_ipbf[] = {
   (S390_SETBIT(0)),
};

/* Supported data types and data layout formats */
static const ULong NNPA_dtypes_layouts[] = {
   /* Data types */
   (S390_SETBIT(0) | // data type 1 (16 bit)

    /* Data layout formats */
    S390_SETBIT(32 + 0) | // 4D-feature tensor
    S390_SETBIT(32 + 1)   // 4D-kernel tensor
    ),
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
   ULong reserved2[22];
};

struct s390_NNPA_tensor0 {
   UChar  layout;
   UChar  dtype;
   UShort reserved1;
   UInt   reserved2;
   UInt   dim4, dim3, dim2, dim1;
   ULong  address;
};

struct s390_NNPA_parms0 {
   ULong                    pbvn : 16;
   ULong                    mvn : 8;
   ULong                    ribm : 24;
   ULong                    reserved0 : 15;
   ULong                    cf : 1;
   ULong                    reserved1[6];
   ULong                    save_area_address;
   struct s390_NNPA_tensor0 out[2];
   struct s390_NNPA_tensor0 reserved2[2];
   struct s390_NNPA_tensor0 in[3];
   ULong                    reserved3[12];
   UInt                     param[5];
   UInt                     reserved4;
   ULong                    reserved5[13];
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
   ULong dim[5];  // total dimensions
   ULong used[5]; // used dimensions, without padding
   ULong step[5];
};

/* Determine the 5 dimensions used to represent the tensor data in memory */
static enum ExtensionError
NNPA_tensor0_size(const struct s390_NNPA_tensor0*  t,
                  UInt                             msg_idx,
                  struct s390_NNPA_mem_dimensions* out_md)
{
   struct s390_NNPA_mem_dimensions md;
   ULong                           elem_size;

   if (t->dtype == 0)
      elem_size = 2;
   else
      return INSN_ERR(s390_NNPA_errmsg_dtype[msg_idx]);

   switch (t->layout) {
   case 0: // 4D-feature tensor
      md.dim[0] = md.used[0] = t->dim4;
      md.dim[1] = md.used[1] = (t->dim1 + 63) / 64;
      md.dim[2] = md.used[2] = t->dim3;
      md.dim[3]              = (t->dim2 + 31) / 32 * 32;
      md.used[3]             = t->dim2;
      md.dim[4]              = 64;
      md.used[4]             = t->dim1;
      break;
   case 1: // 4D-kernel tensor
      md.dim[0] = md.used[0] = (t->dim1 + 63) / 64;
      md.dim[1] = md.used[1] = t->dim4;
      md.dim[2] = md.used[2] = t->dim3;
      md.dim[3]              = (t->dim2 + 31) / 32 * 32;
      md.used[3]             = t->dim2;
      md.dim[4]              = 64;
      md.used[4]             = t->dim1;
      break;
   default:
      return INSN_ERR(s390_NNPA_errmsg_layout[msg_idx]);
   }
   md.step[4] = elem_size * md.dim[4];
   md.step[3] = md.step[4] * md.dim[3];
   md.step[2] = md.step[3] * md.dim[2];
   md.step[1] = md.step[2] * md.dim[1];
   md.step[0] = md.step[1] * md.dim[0]; // total size
   *out_md    = md;
   return ExtErr_OK;
}

static enum ExtensionError NNPA_pre_read_tensor0(
   ThreadState* tst, UInt msg_idx, const struct s390_NNPA_tensor0* t)
{
   struct s390_NNPA_mem_dimensions md;
   enum ExtensionError             ret;

   ret = NNPA_tensor0_size(t, msg_idx, &md);
   if (ret != ExtErr_OK)
      return ret;

   for (ULong d0 = 0; d0 < md.used[0]; d0++) {
      for (ULong d1 = 0; d1 < md.used[1]; d1++) {
         for (ULong d2 = 0; d2 < md.used[2]; d2++) {
            for (ULong d3 = 0; d3 < md.used[3]; d3++) {
               ULong addr = t->address + d0 * md.step[1] + d1 * md.step[2] +
                            d2 * md.step[3] + d3 * md.step[4];
               PRE_MEM_READ(tst, s390_NNPA_errmsg_access[msg_idx], addr,
                            md.dim[4]);
            }
         }
      }
   }
   return ExtErr_OK;
}

static UWord NNPA_pre_write_tensor0(ThreadState*                    tst,
                                    UInt                            msg_idx,
                                    const struct s390_NNPA_tensor0* t)
{
   struct s390_NNPA_mem_dimensions md;
   enum ExtensionError             ret;

   ret = NNPA_tensor0_size(t, msg_idx, &md);
   if (ret != ExtErr_OK)
      return ret;

   PRE_MEM_WRITE(tst, "NNPA(out_tensor)", t->address, md.step[0]);
   return ExtErr_OK;
}

static void NNPA_post_write_tensor0(ThreadState*                    tst,
                                    UInt                            msg_idx,
                                    const struct s390_NNPA_tensor0* t)
{
   struct s390_NNPA_mem_dimensions md;
   enum ExtensionError             ret;

   ret = NNPA_tensor0_size(t, msg_idx, &md);
   if (ret != ExtErr_OK)
      return;

   for (ULong d0 = 0; d0 < md.used[0]; d0++) {
      for (ULong d1 = 0; d1 < md.used[1]; d1++) {
         for (ULong d2 = 0; d2 < md.used[2]; d2++) {
            for (ULong d3 = 0; d3 < md.used[3]; d3++) {
               ULong addr = t->address + d0 * md.step[1] + d1 * md.step[2] +
                            d2 * md.step[3] + d3 * md.step[4];
               POST_MEM_WRITE(tst, addr, md.dim[4]);
            }
         }
      }
   }
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
   } else {
      struct s390_NNPA_parms0*      parms          = (void*)parms_addr;
      const struct s390_NNPA_parms0 orig_parms     = *parms;
      ULong                         save_area_size = 0;
      UInt                          in_tensors;
      UInt                          out_tensors;

      parms_len = 4096;
      PRE_MEM_READ(tst, "NNPA(parms)", parms_addr,
                   sizeof(struct s390_NNPA_parms0));
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
         in_tensors  = 2;
         out_tensors = 1;
         break;
      case S390_NNPA_LOG:
      case S390_NNPA_EXP:
      case S390_NNPA_RELU:
      case S390_NNPA_TANH:
      case S390_NNPA_SIGMOID:
         in_tensors  = 1;
         out_tensors = 1;
         break;
      case S390_NNPA_SOFTMAX:
         in_tensors     = 1;
         out_tensors    = 1;
         save_area_size = 8192;
         break;
      case S390_NNPA_BATCHNORM:
         in_tensors  = 3;
         out_tensors = 1;
         break;
      case S390_NNPA_MAXPOOL2D:
      case S390_NNPA_AVGPOOL2D:
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
         in_tensors  = 3;
         out_tensors = 1;
         break;
      default:
         return INSN_ERR("NNPA: unknown function code\n");
      }

      for (UInt i = 0; i < in_tensors; i++) {
         enum ExtensionError retval =
            NNPA_pre_read_tensor0(tst, s390_NNPA_message_in + i, &parms->in[i]);
         if (retval != ExtErr_OK)
            return retval;
      }
      for (UInt i = 0; i < out_tensors; i++) {
         enum ExtensionError retval = NNPA_pre_write_tensor0(
            tst, s390_NNPA_message_out + i, &parms->out[i]);
         if (retval != ExtErr_OK)
            return retval;
      }
      if (save_area_size != 0) {
         PRE_MEM_WRITE(tst, "NNPA(save_area)", parms->save_area_address,
                       save_area_size);
      }
      cc = do_NNPA_insn(&gpr0, parms_addr);
      if (cc == 0) {
         for (UInt i = 0; i < out_tensors; i++) {
            NNPA_post_write_tensor0(tst, s390_NNPA_message_out + i,
                                    &orig_parms.out[i]);
         }
      }
   }
   POST_MEM_WRITE(tst, parms_addr, parms_len);
   WRITE_GPR(tst, 0, gpr0);
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
   default:
      VG_(core_panic)("unknown extension ID");
   }
}

#endif
