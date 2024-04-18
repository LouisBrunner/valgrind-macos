
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

#define READ_FUNCTION_CODE(tst)                                                \
   ({                                                                          \
      PRE_REG_READ(tst, "func_code", r0, 7, sizeof(UChar));                    \
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
      VG_(umsg)("Illegal operation: ");                                        \
      VG_(umsg)(msg);                                                          \
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

static UWord do_extension_PRNO(ThreadState* tst, ULong variant)
{
   UChar r1    = variant & 0xf;
   UChar r2    = (variant >> 4) & 0xf;
   UChar func  = READ_FUNCTION_CODE(tst);
   UChar fc    = func & 0x7f;
   UChar mflag = func & 128;
   ULong parms = READ_GPR(tst, "r1", 1);
   ULong parms_len;
   Int   cc         = 0;
   ULong orig_addr1 = 0, orig_len1 = 0, orig_addr2 = 0, orig_len2 = 0;
   ULong addr1 = 0, len1 = 0, addr2 = 0, len2 = 0;

   switch (fc) {
   case 0: // Query
      parms_len = 16;
      PRE_MEM_WRITE(tst, "parms", parms, parms_len);
      cc = do_PRNO_insn(func, parms, &addr1, &len1, &addr2, &len2);
      s390_filter_functions((ULong*)parms, parms_len, PRNO_functions,
                            sizeof(PRNO_functions));
      POST_MEM_WRITE(tst, parms, parms_len);
      break;
   case 112: // TRNG-Query-Raw-to-Conditioned-Ratio
      parms_len = 8;
      PRE_MEM_WRITE(tst, "parms", parms, parms_len);
      cc = do_PRNO_insn(func, parms, &addr1, &len1, &addr2, &len2);
      POST_MEM_WRITE(tst, parms, parms_len);
      break;
   case 3: // SHA-512-DRNG
      parms_len = 240;
      PRE_MEM_READ(tst, "parms", parms, parms_len);
      if (mflag == 0) {
         // Generate operation
         addr1 = orig_addr1 = READ_GPR(tst, "op1_addr", r1);
         len1 = orig_len1 = READ_GPR(tst, "op1_len", r1 + 1);
         PRE_MEM_WRITE(tst, "operand1", addr1, len1);
      } else {
         // Seed operation
         addr2 = orig_addr2 = READ_GPR(tst, "op2_addr", r2);
         len2 = orig_len2 = READ_GPR(tst, "op2_len", r2 + 1);
         PRE_MEM_READ(tst, "operand2", addr2, len2);
      }
      PRE_MEM_WRITE(tst, "parms", parms, parms_len);
      cc = do_PRNO_insn(func, parms, &addr1, &len1, &addr2, &len2);
      POST_MEM_WRITE(tst, parms, parms_len);
      if (mflag == 0) {
         WRITE_GPR(tst, r2 + 1, len1);
         POST_MEM_WRITE(tst, orig_addr1, orig_len1 - len1);
      }
      break;
   case 114: // TRNG
      addr1 = orig_addr1 = READ_GPR(tst, "op1_addr", r1);
      len1  = orig_len1 = READ_GPR(tst, "op1_len", r1 + 1);
      PRE_MEM_WRITE(tst, "operand1", addr1, len1);
      addr2 = orig_addr2 = READ_GPR(tst, "op2_addr", r2);
      len2  = orig_len2 = READ_GPR(tst, "op2_len", r2 + 1);
      PRE_MEM_WRITE(tst, "operand2", addr2, len2);
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

enum ExtensionError ML_(do_client_extension)(ThreadState* tst)
{
   ULong code    = REG_READ(tst, SYSNO);
   ULong id      = code & ((1ULL << S390_EXT_ID_NBITS) - 1);
   ULong variant = code >> S390_EXT_ID_NBITS;

   switch (id) {
   case S390_EXT_PRNO:
      return do_extension_PRNO(tst, variant);
   default:
      VG_(core_panic)("unknown extension ID");
   }
}

#endif
