/* -*- mode: C; c-basic-offset: 3; -*- */

#ifndef VTEST_H
#define VTEST_H

/* Main header file for the V-bit tester */

#include <stdint.h>   // uint64_t
#include "libvex.h"   // IROp
#include "vbits.h"    // vbits_t


/* How undefinedness propagates from input to output */

typedef enum {
   // For any undefined input bit, all output bits are defined.
   UNDEF_NONE,

   // For any undefined input bit, all output bits are undefined.
   UNDEF_ALL,

   // For each undefined input bit, the corresponding output bit
   // in the same position is undefined. No other bit is undefined.
   UNDEF_SAME,

   // For each undefined input bit, the corresponding output bit
   // in the same position is undefined. No other bit is undefined.
   // If the corresponding output bit does not exist, the input bit
   // does not cause any output bits to be undefined.
   UNDEF_TRUNC,

   // For each undefined input bit, the corresponding output bit
   // in the same position is undefined. No other bit is undefined.
   // Output bits that do no not have a corresponding input bit are
   // defined.
   UNDEF_ZEXT,

   // For each undefined input bit, the corresponding output bit
   // in the same position is undefined. If the MSB of the input value
   // is undefined, so are all output bits with higher significance
   // than the MSB input bit.
   UNDEF_SEXT,

   // For each undefined input bit, the corresponding output bit
   // and all output bits with higher significance are undefined.
   UNDEF_LEFT,    

   UNDEF_CONCAT,  // nHLto2n ops e.g. Iop_32HLto64
   UNDEF_UPPER,   // 2nHIton ops e.g. Iop_64HIto32
   UNDEF_SHL,     // shift-left
   UNDEF_SHR,     // logical shift-right
   UNDEF_SAR,     // arithmetic shift-right
   UNDEF_OR,      // bitwise OR operation
   UNDEF_AND,     // bitwise AND operation

   UNDEF_ORD,     // Iop_CmpORD compare 

   // For IROps I don't know anything about
   UNDEF_UNKNOWN
} undef_t;


// Everything we want to know about an IROp
typedef struct {
   IROp op;
   const char *name;
   undef_t     undef_kind;
   int         shift_amount_is_immediate;
   // Indicate whether IROp can be tested on a particular architecture
   unsigned    s390x  : 1;
   unsigned    amd64  : 1;
   unsigned    ppc32  : 1;
   unsigned    ppc64  : 1;
   unsigned    arm    : 1;
   unsigned    x86    : 1;
   unsigned    mips32 : 1;
   unsigned    mips64 : 1;
} irop_t;


/* The maximum number of input operands */
#define MAX_OPERANDS 4

/* An operand of an IROp (also used for the result) */
typedef struct {
   IRType  type;
   vbits_t vbits;
   value_t value;
} opnd_t;


/* Carries the data needed to execute and evaluate a test. I.e.
   inputs and results (V-bits and actual value). */
typedef struct {
   opnd_t result;
   opnd_t opnds[MAX_OPERANDS];
   unsigned rounding_mode;
} test_data_t;


/* Function prototypes */
irop_t *get_irop(IROp);
int  is_floating_point_op_with_rounding_mode(IROp);
int  get_num_operands(IROp);

void print_opnd(FILE *, const opnd_t *);

int test_unary_op(const irop_t *, test_data_t *);
int test_binary_op(const irop_t *, test_data_t *);
int test_ternary_op(const irop_t *, test_data_t *);
int test_qernary_op(const irop_t *, test_data_t *);

void valgrind_vex_init_for_iri(IRICB *);
void valgrind_execute_test(const irop_t *, test_data_t *);

IRICB new_iricb(const irop_t *, test_data_t *);

void panic(const char *) __attribute__((noreturn));
void complain(const irop_t *, const test_data_t *, vbits_t expected);

/* Imported from VEX */
unsigned sizeof_irtype(IRType);
void typeof_primop(IROp, IRType *t_dst, IRType *t_arg1, IRType *t_arg2, 
                   IRType *t_arg3, IRType *t_arg4);

static unsigned __inline__ bitsof_irtype(IRType type)
{
   return type == Ity_I1 ? 1 : sizeof_irtype(type) * 8;
}


/* Exported variables */
extern int verbose;

#endif // VTEST_H
