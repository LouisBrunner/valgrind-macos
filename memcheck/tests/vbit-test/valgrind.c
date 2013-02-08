/* -*- mode: C; c-basic-offset: 3; -*- */

#include <assert.h>
#include "memcheck.h"  // VALGRIND_SET_VBITS
#include "vtest.h"     


/* Return a completely initialised control block */
IRICB
new_iricb(const irop_t *op, test_data_t *data)
{
   IRICB cb;

   cb.op = op->op;
   cb.result = (HWord)&data->result.value;
   cb.opnd1  = (HWord)&data->opnds[0].value;
   cb.opnd2  = (HWord)&data->opnds[1].value;
   cb.opnd3  = (HWord)&data->opnds[2].value;
   cb.opnd4  = (HWord)&data->opnds[3].value;
   cb.t_result = data->result.type;
   cb.t_opnd1  = data->opnds[0].type;
   cb.t_opnd2  = data->opnds[1].type;
   cb.t_opnd3  = data->opnds[2].type;
   cb.t_opnd4  = data->opnds[3].type;

   cb.rounding_mode = data->rounding_mode;

   cb.num_operands = get_num_operands(op->op);

   cb.shift_amount_is_immediate = op->shift_amount_is_immediate;

   return cb;
}


/* Ity_I1 values cannot be stored or loaded. So vex_inject_ir will load/store
   such a value from/to a 4-byte container. It uses 32to1 and 1Uto32,
   respectively. */
static void
valgrind_set_vbits(opnd_t *opnd)
{
   unsigned rc, num_bytes;
   
   /* 1-bit wide values cannot be read. So we read a 4 bytes here */
   num_bytes = opnd->type == Ity_I1 ? 4 : sizeof_irtype(opnd->type);
   rc = VALGRIND_SET_VBITS(&opnd->value, &opnd->vbits.bits, num_bytes);
   assert(rc == 1);

   // Make sure the v-bits were set correctly
   vbits_t actual = { .num_bits = opnd->vbits.num_bits };
   rc = VALGRIND_GET_VBITS(&opnd->value, &actual.bits, num_bytes);
   assert(rc == 1);

   assert(equal_vbits(opnd->vbits, actual));
}


static void
valgrind_get_vbits(opnd_t *opnd)
{
   unsigned rc, num_bytes;

   /* 1-bit wide values cannot be stored. So we store them by writing a
      single byte */
   num_bytes = opnd->type == Ity_I1 ? 4 : sizeof_irtype(opnd->type);
   opnd->vbits.num_bits = bitsof_irtype(opnd->type);
   rc = VALGRIND_GET_VBITS(&opnd->value, &opnd->vbits.bits, num_bytes);
   assert(rc == 1);
}


/* Insert a client request that will initialize VEX for IR injection */
void
valgrind_vex_init_for_iri(IRICB *cb)
{
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ__VEX_INIT_FOR_IRI, cb, 0,0,0,0);
}


/* Insert a special opcode that will cause VEX to inject an IR stmt based
   on the information passed in the IRICB (in valgrind_vex_init_for_iri). */
static void
valgrind_vex_inject_ir(void)
{
   VALGRIND_VEX_INJECT_IR();
}


/* Execute the test under valgrind. Well, yes, we're not really executing
   it here, just preparing for it... */
void
valgrind_execute_test(const irop_t *op, test_data_t *data)
{
   unsigned i, num_operands;

   if (verbose > 2) printf("---------- Running a test\n");
   num_operands = get_num_operands(op->op);

   for (i = 0; i < num_operands; ++i) {
      valgrind_set_vbits(&data->opnds[i]);
      if (verbose > 2) {
         printf("opnd #%u:  ", i);
         print_opnd(stdout, &data->opnds[i]);
         printf("\n");
      }
   }
   if (verbose > 2)
      if (data->rounding_mode != NO_ROUNDING_MODE)
         printf("rounding mode %u\n", data->rounding_mode);

   valgrind_vex_inject_ir();
   valgrind_get_vbits(&data->result);
   if (verbose > 2) {
      printf("result:   ");
      print_opnd(stdout, &data->result);
      printf("\n");
   }
}
