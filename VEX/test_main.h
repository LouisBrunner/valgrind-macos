
/* Copy this file (test_main.h.in) to test_main.h, and edit */

/* DEBUG RUN, ON V */
#if 1
#define TEST_VSUPPORT  True
#define TEST_N_ITERS   1
#define TEST_N_BBS     1
#define TEST_FLAGS     (1<<7)|(0<<6)|(1<<3)|(0<<2)|(0<<1)|(0<<0)
#endif

/* CHECKING RUN, ON V */
#if 0
#define TEST_VSUPPORT  True
#define TEST_N_ITERS   1
#define TEST_N_BBS     100000
#define TEST_FLAGS     0
#endif

/* PROFILING RUN, NATIVE */
#if 0
#define TEST_VSUPPORT  False
#define TEST_N_ITERS   100
#define TEST_N_BBS     1000
#define TEST_FLAGS     0
#endif

/* PROFILING RUN, REDUCED WORKLOAD */
#if 0
#define TEST_VSUPPORT  False
#define TEST_N_ITERS   3
#define TEST_N_BBS     1000
#define TEST_FLAGS     0
#endif

