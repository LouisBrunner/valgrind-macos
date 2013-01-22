#ifndef ROUNDING_H
#define ROUNDING_H

/* Macros, so the values can be used together with opcodes.h */

/* ---------------------------------------------------------------- */
/* BFP rounding mode as it is encoded in the m3 field of certain    */
/* instructions (e.g. CFEBR)                                        */
/* ---------------------------------------------------------------- */
#define M3_BFP_ROUND_PER_FPC        0

// Cannot be mapped to IRRoundingMode
#define M3_BFP_ROUND_NEAREST_AWAY   1

// 2 is not allowed

// Needs floating point extension facility
// Cannot be mapped to IRRoundingMode
#define M3_BFP_ROUND_PREPARE_SHORT  3

#define M3_BFP_ROUND_NEAREST_EVEN   4
#define M3_BFP_ROUND_ZERO           5
#define M3_BFP_ROUND_POSINF         6
#define M3_BFP_ROUND_NEGINF         7


/* ---------------------------------------------------------------- */
/* BFP rounding mode as it is encoded in bits [29:31] of the FPC    */
/* register. Only rounding modes 0..3 are universally supported.    */
/* Others require additional hardware facilities.                   */
/* ---------------------------------------------------------------- */
#define FPC_BFP_ROUND_NEAREST_EVEN  0
#define FPC_BFP_ROUND_ZERO          1
#define FPC_BFP_ROUND_POSINF        2
#define FPC_BFP_ROUND_NEGINF        3

// 4,5,6 are not allowed

// Needs floating point extension facility
// Cannot be mapped to IRRoundingMode
#define FPC_BFP_ROUND_PREPARE_SHORT 7


#endif /* ROUNDING_H */
