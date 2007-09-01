
#include <stdio.h>
#include <signal.h>
#include <setjmp.h>
#include <string.h>
#include <assert.h>

static jmp_buf env_sigtrap;
static void handler_sigtrap ( int x ) { longjmp(env_sigtrap,1); }

void try ( char* who, void(*maybe_traps)(long,long), long arg1, long arg2 )
{
   struct sigaction tmp_act;
   int r, trapped = 0;
   memset(&tmp_act, 0, sizeof(tmp_act));
   tmp_act.sa_handler = handler_sigtrap;
   sigemptyset(&tmp_act.sa_mask);
   tmp_act.sa_flags = SA_NODEFER;
   r = sigaction(SIGTRAP, &tmp_act, NULL);
   assert(r == 0);
   if (setjmp(env_sigtrap)) {
      trapped = 1;
   } else {
      maybe_traps(arg1, arg2);
   }
   signal(SIGTRAP, SIG_DFL);

   printf("%s(%4lld,%4lld) -> %s\n", who, (long long int)arg1,
	   (long long int)arg2,
	  trapped ? "TRAP" : "no trap" );
}

static void tw_0 ( long n, long m ) {
  __asm__ __volatile__("tw 0, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void tw_1 ( long n, long m ) {
  __asm__ __volatile__("tw 1, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void tw_2 ( long n, long m ) {
  __asm__ __volatile__("tw 2, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void tw_3 ( long n, long m ) {
  __asm__ __volatile__("tw 3, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void tw_4 ( long n, long m ) {
  __asm__ __volatile__("tw 4, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void tw_5 ( long n, long m ) {
  __asm__ __volatile__("tw 5, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void tw_6 ( long n, long m ) {
  __asm__ __volatile__("tw 6, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void tw_7 ( long n, long m ) {
  __asm__ __volatile__("tw 7, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void tw_8 ( long n, long m ) {
  __asm__ __volatile__("tw 8, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void tw_9 ( long n, long m ) {
  __asm__ __volatile__("tw 9, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void tw_10 ( long n, long m ) {
  __asm__ __volatile__("tw 10, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void tw_11 ( long n, long m ) {
  __asm__ __volatile__("tw 11, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void tw_12 ( long n, long m ) {
  __asm__ __volatile__("tw 12, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void tw_13 ( long n, long m ) {
  __asm__ __volatile__("tw 13, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void tw_14 ( long n, long m ) {
  __asm__ __volatile__("tw 14, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void tw_15 ( long n, long m ) {
  __asm__ __volatile__("tw 15, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void tw_16 ( long n, long m ) {
  __asm__ __volatile__("tw 16, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void tw_17 ( long n, long m ) {
  __asm__ __volatile__("tw 17, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void tw_18 ( long n, long m ) {
  __asm__ __volatile__("tw 18, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void tw_19 ( long n, long m ) {
  __asm__ __volatile__("tw 19, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void tw_20 ( long n, long m ) {
  __asm__ __volatile__("tw 20, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void tw_21 ( long n, long m ) {
  __asm__ __volatile__("tw 21, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void tw_22 ( long n, long m ) {
  __asm__ __volatile__("tw 22, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void tw_23 ( long n, long m ) {
  __asm__ __volatile__("tw 23, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void tw_24 ( long n, long m ) {
  __asm__ __volatile__("tw 24, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void tw_25 ( long n, long m ) {
  __asm__ __volatile__("tw 25, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void tw_26 ( long n, long m ) {
  __asm__ __volatile__("tw 26, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void tw_27 ( long n, long m ) {
  __asm__ __volatile__("tw 27, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void tw_28 ( long n, long m ) {
  __asm__ __volatile__("tw 28, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void tw_29 ( long n, long m ) {
  __asm__ __volatile__("tw 29, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void tw_30 ( long n, long m ) {
  __asm__ __volatile__("tw 30, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void tw_31 ( long n, long m ) {
  __asm__ __volatile__("tw 31, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}

#if defined(__powerpc64__)

static void td_0 ( long n, long m ) {
  __asm__ __volatile__("td 0, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void td_1 ( long n, long m ) {
  __asm__ __volatile__("td 1, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void td_2 ( long n, long m ) {
  __asm__ __volatile__("td 2, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void td_3 ( long n, long m ) {
  __asm__ __volatile__("td 3, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void td_4 ( long n, long m ) {
  __asm__ __volatile__("td 4, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void td_5 ( long n, long m ) {
  __asm__ __volatile__("td 5, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void td_6 ( long n, long m ) {
  __asm__ __volatile__("td 6, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void td_7 ( long n, long m ) {
  __asm__ __volatile__("td 7, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void td_8 ( long n, long m ) {
  __asm__ __volatile__("td 8, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void td_9 ( long n, long m ) {
  __asm__ __volatile__("td 9, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void td_10 ( long n, long m ) {
  __asm__ __volatile__("td 10, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void td_11 ( long n, long m ) {
  __asm__ __volatile__("td 11, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void td_12 ( long n, long m ) {
  __asm__ __volatile__("td 12, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void td_13 ( long n, long m ) {
  __asm__ __volatile__("td 13, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void td_14 ( long n, long m ) {
  __asm__ __volatile__("td 14, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void td_15 ( long n, long m ) {
  __asm__ __volatile__("td 15, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void td_16 ( long n, long m ) {
  __asm__ __volatile__("td 16, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void td_17 ( long n, long m ) {
  __asm__ __volatile__("td 17, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void td_18 ( long n, long m ) {
  __asm__ __volatile__("td 18, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void td_19 ( long n, long m ) {
  __asm__ __volatile__("td 19, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void td_20 ( long n, long m ) {
  __asm__ __volatile__("td 20, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void td_21 ( long n, long m ) {
  __asm__ __volatile__("td 21, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void td_22 ( long n, long m ) {
  __asm__ __volatile__("td 22, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void td_23 ( long n, long m ) {
  __asm__ __volatile__("td 23, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void td_24 ( long n, long m ) {
  __asm__ __volatile__("td 24, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void td_25 ( long n, long m ) {
  __asm__ __volatile__("td 25, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void td_26 ( long n, long m ) {
  __asm__ __volatile__("td 26, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void td_27 ( long n, long m ) {
  __asm__ __volatile__("td 27, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void td_28 ( long n, long m ) {
  __asm__ __volatile__("td 28, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void td_29 ( long n, long m ) {
  __asm__ __volatile__("td 29, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void td_30 ( long n, long m ) {
  __asm__ __volatile__("td 30, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}
static void td_31 ( long n, long m ) {
  __asm__ __volatile__("td 31, %0,%1"
		       : /*out*/ : /*in*/ "r" (n), "r" (m) );
}

#endif


int main ( void )
{
#define TW_GROUP(cmp) \
   try("tw_" #cmp "", tw_##cmp, -150, -100); \
   try("tw_" #cmp "", tw_##cmp, -100, -100); \
   try("tw_" #cmp "", tw_##cmp,  -50, -100);

   TW_GROUP(0);
   TW_GROUP(1);
   TW_GROUP(2);
   TW_GROUP(3);
   TW_GROUP(4);
   TW_GROUP(5);
   TW_GROUP(6);
   TW_GROUP(7);
   TW_GROUP(8);
   TW_GROUP(9);
   TW_GROUP(10);
   TW_GROUP(11);
   TW_GROUP(12);
   TW_GROUP(13);
   TW_GROUP(14);
   TW_GROUP(15);
   TW_GROUP(16);
   TW_GROUP(17);
   TW_GROUP(18);
   TW_GROUP(19);
   TW_GROUP(20);
   TW_GROUP(21);
   TW_GROUP(22);
   TW_GROUP(23);
   TW_GROUP(24);
   TW_GROUP(25);
   TW_GROUP(26);
   TW_GROUP(27);
   TW_GROUP(28);
   TW_GROUP(29);
   TW_GROUP(30);
   TW_GROUP(31);
#if defined(__powerpc64__)
#define TD_GROUP(cmp) \
   try("td_" #cmp "", td_##cmp, -150, -100); \
   try("td_" #cmp "", td_##cmp, -100, -100); \
   try("td_" #cmp "", td_##cmp,  -50, -100);

   TD_GROUP(0);
   TD_GROUP(1);
   TD_GROUP(2);
   TD_GROUP(3);
   TD_GROUP(4);
   TD_GROUP(5);
   TD_GROUP(6);
   TD_GROUP(7);
   TD_GROUP(8);
   TD_GROUP(9);
   TD_GROUP(10);
   TD_GROUP(11);
   TD_GROUP(12);
   TD_GROUP(13);
   TD_GROUP(14);
   TD_GROUP(15);
   TD_GROUP(16);
   TD_GROUP(17);
   TD_GROUP(18);
   TD_GROUP(19);
   TD_GROUP(20);
   TD_GROUP(21);
   TD_GROUP(22);
   TD_GROUP(23);
   TD_GROUP(24);
   TD_GROUP(25);
   TD_GROUP(26);
   TD_GROUP(27);
   TD_GROUP(28);
   TD_GROUP(29);
   TD_GROUP(30);
   TD_GROUP(31);
#endif
   return 0;
}
