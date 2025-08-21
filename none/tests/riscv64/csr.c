/* Tests for the RV64Zicsr standard control-and-status register instruction-set
   extension. */

#include "testinst.h"

static void test_csr64_shared(void)
{
   printf("RV64Zicsr control-and-status register instruction set, shared "
          "operations\n");

   /* ----------------- csrrw rd, csr, rs1 ------------------ */
   /* fflags */
   TESTINST_1_1_CSR(4, "csrrw a0, fflags, a1", 0x00, 0x01, a0, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrw a0, fflags, a1", 0x00, 0x1f, a0, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrw a0, fflags, a1", 0xff, 0x1e, a0, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrw a0, fflags, a1", 0xff, 0x00, a0, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrw a0, fflags, a1", 0x00, 0xff, a0, fcsr, a1);

   TESTINST_1_1_CSR(4, "csrrw t5, fflags, t6", 0x00, 0x01, t5, fcsr, t6);
   TESTINST_1_1_CSR(4, "csrrw zero, fflags, a1", 0xff, 0x01, zero, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrw a0, fflags, zero", 0xff, 0x00, a0, fcsr, zero);

   /* frm */
   TESTINST_1_1_CSR(4, "csrrw a0, frm, a1", 0x00, 0x1, a0, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrw a0, frm, a1", 0x00, 0x7, a0, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrw a0, frm, a1", 0xff, 0x6, a0, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrw a0, frm, a1", 0xff, 0x0, a0, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrw a0, frm, a1", 0x00, 0xff, a0, fcsr, a1);

   TESTINST_1_1_CSR(4, "csrrw t5, frm, t6", 0x00, 0x1, t5, fcsr, t6);
   TESTINST_1_1_CSR(4, "csrrw zero, frm, a1", 0xff, 0x1, zero, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrw a0, frm, zero", 0xff, 0x0, a0, fcsr, zero);

   /* fcsr */
   TESTINST_1_1_CSR(4, "csrrw a0, fcsr, a1", 0x00, 0x01, a0, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrw a0, fcsr, a1", 0x00, 0xff, a0, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrw a0, fcsr, a1", 0xff, 0xfe, a0, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrw a0, fcsr, a1", 0xff, 0x00, a0, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrw a0, fcsr, a1", 0x00, 0xff, a0, fcsr, a1);

   TESTINST_1_1_CSR(4, "csrrw t5, fcsr, t6", 0x00, 0x01, t5, fcsr, t6);
   TESTINST_1_1_CSR(4, "csrrw zero, fcsr, a1", 0xff, 0x01, zero, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrw a0, fcsr, zero", 0xff, 0x00, a0, fcsr, zero);

   /* ----------------- csrrs rd, csr, rs1 ------------------ */
   /* fflags */
   TESTINST_1_1_CSR(4, "csrrs a0, fflags, a1", 0x00, 0x01, a0, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrs a0, fflags, a1", 0x00, 0x1f, a0, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrs a0, fflags, a1", 0xff, 0x1e, a0, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrs a0, fflags, a1", 0xff, 0x00, a0, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrs a0, fflags, a1", 0x00, 0xff, a0, fcsr, a1);

   TESTINST_1_1_CSR(4, "csrrs t5, fflags, t6", 0x00, 0x01, t5, fcsr, t6);
   TESTINST_1_1_CSR(4, "csrrs zero, fflags, a1", 0xff, 0x01, zero, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrs a0, fflags, zero", 0xff, 0x00, a0, fcsr, zero);

   /* frm */
   TESTINST_1_1_CSR(4, "csrrs a0, frm, a1", 0x00, 0x1, a0, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrs a0, frm, a1", 0x00, 0x7, a0, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrs a0, frm, a1", 0xff, 0x6, a0, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrs a0, frm, a1", 0xff, 0x0, a0, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrs a0, frm, a1", 0x00, 0xff, a0, fcsr, a1);

   TESTINST_1_1_CSR(4, "csrrs t5, frm, t6", 0x00, 0x1, t5, fcsr, t6);
   TESTINST_1_1_CSR(4, "csrrs zero, frm, a1", 0xff, 0x1, zero, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrs a0, frm, zero", 0xff, 0x0, a0, fcsr, zero);

   /* fcsr */
   TESTINST_1_1_CSR(4, "csrrs a0, fcsr, a1", 0x00, 0x01, a0, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrs a0, fcsr, a1", 0x00, 0xff, a0, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrs a0, fcsr, a1", 0xff, 0xfe, a0, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrs a0, fcsr, a1", 0xff, 0x00, a0, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrs a0, fcsr, a1", 0x00, 0xff, a0, fcsr, a1);

   TESTINST_1_1_CSR(4, "csrrs t5, fcsr, t6", 0x00, 0x01, t5, fcsr, t6);
   TESTINST_1_1_CSR(4, "csrrs zero, fcsr, a1", 0xff, 0x01, zero, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrs a0, fcsr, zero", 0xff, 0x00, a0, fcsr, zero);

   /* ----------------- csrrc rd, csr, rs1 ------------------ */
   /* fflags */
   TESTINST_1_1_CSR(4, "csrrc a0, fflags, a1", 0x00, 0x01, a0, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrc a0, fflags, a1", 0x00, 0x1f, a0, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrc a0, fflags, a1", 0xff, 0x1e, a0, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrc a0, fflags, a1", 0xff, 0x00, a0, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrc a0, fflags, a1", 0x00, 0xff, a0, fcsr, a1);

   TESTINST_1_1_CSR(4, "csrrc t5, fflags, t6", 0x00, 0x01, t5, fcsr, t6);
   TESTINST_1_1_CSR(4, "csrrc zero, fflags, a1", 0xff, 0x01, zero, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrc a0, fflags, zero", 0xff, 0x00, a0, fcsr, zero);

   /* frm */
   TESTINST_1_1_CSR(4, "csrrc a0, frm, a1", 0x00, 0x1, a0, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrc a0, frm, a1", 0x00, 0x7, a0, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrc a0, frm, a1", 0xff, 0x6, a0, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrc a0, frm, a1", 0xff, 0x0, a0, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrc a0, frm, a1", 0x00, 0xff, a0, fcsr, a1);

   TESTINST_1_1_CSR(4, "csrrc t5, frm, t6", 0x00, 0x1, t5, fcsr, t6);
   TESTINST_1_1_CSR(4, "csrrc zero, frm, a1", 0xff, 0x1, zero, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrc a0, frm, zero", 0xff, 0x0, a0, fcsr, zero);

   /* fcsr */
   TESTINST_1_1_CSR(4, "csrrc a0, fcsr, a1", 0x00, 0x01, a0, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrc a0, fcsr, a1", 0x00, 0xff, a0, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrc a0, fcsr, a1", 0xff, 0xfe, a0, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrc a0, fcsr, a1", 0xff, 0x00, a0, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrc a0, fcsr, a1", 0x00, 0xff, a0, fcsr, a1);

   TESTINST_1_1_CSR(4, "csrrc t5, fcsr, t6", 0x00, 0x01, t5, fcsr, t6);
   TESTINST_1_1_CSR(4, "csrrc zero, fcsr, a1", 0xff, 0x01, zero, fcsr, a1);
   TESTINST_1_1_CSR(4, "csrrc a0, fcsr, zero", 0xff, 0x00, a0, fcsr, zero);

   /* -------------- csrrwi rd, csr, uimm[4:0] -------------- */
   /* Not currently handled. */

   /* -------------- csrrsi rd, csr, uimm[4:0] -------------- */
   /* Not currently handled. */

   /* -------------- csrrci rd, csr, uimm[4:0] -------------- */
   /* Not currently handled. */
}

int main(void)
{
   test_csr64_shared();
   return 0;
}
