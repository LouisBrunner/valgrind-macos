/* register definitions used in tests for isa_3_1.  */

/* ACC / Accumulator.
   An ACC is associated with a set of four VSR registers.
   Each ACC contains four 128-bit rows.
 Each row of each ACC is aliased to a specific VSR in the following manner.
 ACC[0][0] == VSR[0]; ACC[0][1] == VSR[1]; ACC[0][2] == VSR[2]; ACC[0][3] == VSR[3]
 ...
 ACC[7][0] == VSR[28]; ACC[7][0] == VSR[28]; ACC[7][0] == VSR[28]; ACC[7][0] == VSR[28]
*/
#define ACCNUM 4
register vector long long TEST_ACC0 __asm__ ("vs16");
register vector long long TEST_ACC1 __asm__ ("vs17");
register vector long long TEST_ACC2 __asm__ ("vs18");
register vector long long TEST_ACC3 __asm__ ("vs19");

/* XSp and XTp use the same register pair, defined here as 20 and 21.
  This includes the XT and XS values too. */
register vector long long XTp0 __asm__ ("vs20"); // XTp[0];XSp[0];
register vector long long XTp1 __asm__ ("vs21"); // XTp[1];XSp[1];

// xa,xb,xc references are mapped to a specific vector register.
// out of order to allow xap mapped over vec_xa and vec_xc.
register vector long long vec_xa __asm__ ("vs22"); // also xap.
register vector long long vec_xc __asm__ ("vs23"); // also 2nd half of xap.
register vector long long vec_xb __asm__ ("vs24");

/* frs,frb (variable named frsb) both use the same register pair.
   (top half of vs26,vs27)  */
register double frsb   __asm__  ("fr26");
register double frsbp  __asm__  ("fr27");
/* frt,frtp register pair. (top half of vs28,vs29)  */
register double frt    __asm__  ("vs28");
register double frtp   __asm__  ("vs29");

register uint64_t ra   __asm__  ("r20");
register uint64_t rb   __asm__  ("r21");
register uint64_t rc   __asm__  ("r22");
register uint64_t rs   __asm__  ("r24");  /* rsp part 1 */
register uint64_t rsp  __asm__  ("r25");  /* rsp part 2 */
register uint64_t rt   __asm__  ("r26");  /* rtp part 1 */
register uint64_t rtp  __asm__  ("r27");  /* rtp part 2 */

extern unsigned long long vsrd;
extern unsigned long get_vsrhd_vs26();
extern unsigned long get_vsrhd_vs27();
extern unsigned long get_vsrhd_vs28();
extern unsigned long get_vsrhd_vs29();
