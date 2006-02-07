
#include <stdio.h>
#include <signal.h>
#include <setjmp.h>
#include <string.h>
#include <assert.h>

static jmp_buf env_sigtrap;
static void handler_sigtrap ( int x ) { longjmp(env_sigtrap,1); }

void try ( char* who, void(*maybe_traps)(long), long arg )
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
      maybe_traps(arg);
   }
   signal(SIGTRAP, SIG_DFL);

   printf("%s(%4lld) -> %s\n", who, (long long int)arg, 
	  trapped ? "TRAP" : "no trap" );
}

static void twi_0_neg100 ( long n ) {
  __asm__ __volatile__("twi 0, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void twi_1_neg100 ( long n ) {
  __asm__ __volatile__("twi 1, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void twi_2_neg100 ( long n ) {
  __asm__ __volatile__("twi 2, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void twi_3_neg100 ( long n ) {
  __asm__ __volatile__("twi 3, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void twi_4_neg100 ( long n ) {
  __asm__ __volatile__("twi 4, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void twi_5_neg100 ( long n ) {
  __asm__ __volatile__("twi 5, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void twi_6_neg100 ( long n ) {
  __asm__ __volatile__("twi 6, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void twi_7_neg100 ( long n ) {
  __asm__ __volatile__("twi 7, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void twi_8_neg100 ( long n ) {
  __asm__ __volatile__("twi 8, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void twi_9_neg100 ( long n ) {
  __asm__ __volatile__("twi 9, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void twi_10_neg100 ( long n ) {
  __asm__ __volatile__("twi 10, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void twi_11_neg100 ( long n ) {
  __asm__ __volatile__("twi 11, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void twi_12_neg100 ( long n ) {
  __asm__ __volatile__("twi 12, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void twi_13_neg100 ( long n ) {
  __asm__ __volatile__("twi 13, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void twi_14_neg100 ( long n ) {
  __asm__ __volatile__("twi 14, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void twi_15_neg100 ( long n ) {
  __asm__ __volatile__("twi 15, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void twi_16_neg100 ( long n ) {
  __asm__ __volatile__("twi 16, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void twi_17_neg100 ( long n ) {
  __asm__ __volatile__("twi 17, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void twi_18_neg100 ( long n ) {
  __asm__ __volatile__("twi 18, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void twi_19_neg100 ( long n ) {
  __asm__ __volatile__("twi 19, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void twi_20_neg100 ( long n ) {
  __asm__ __volatile__("twi 20, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void twi_21_neg100 ( long n ) {
  __asm__ __volatile__("twi 21, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void twi_22_neg100 ( long n ) {
  __asm__ __volatile__("twi 22, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void twi_23_neg100 ( long n ) {
  __asm__ __volatile__("twi 23, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void twi_24_neg100 ( long n ) {
  __asm__ __volatile__("twi 24, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void twi_25_neg100 ( long n ) {
  __asm__ __volatile__("twi 25, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void twi_26_neg100 ( long n ) {
  __asm__ __volatile__("twi 26, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void twi_27_neg100 ( long n ) {
  __asm__ __volatile__("twi 27, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void twi_28_neg100 ( long n ) {
  __asm__ __volatile__("twi 28, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void twi_29_neg100 ( long n ) {
  __asm__ __volatile__("twi 29, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void twi_30_neg100 ( long n ) {
  __asm__ __volatile__("twi 30, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void twi_31_neg100 ( long n ) {
  __asm__ __volatile__("twi 31, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}

#if defined(__powerpc64__)

static void tdi_0_neg100 ( long n ) {
  __asm__ __volatile__("tdi 0, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void tdi_1_neg100 ( long n ) {
  __asm__ __volatile__("tdi 1, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void tdi_2_neg100 ( long n ) {
  __asm__ __volatile__("tdi 2, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void tdi_3_neg100 ( long n ) {
  __asm__ __volatile__("tdi 3, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void tdi_4_neg100 ( long n ) {
  __asm__ __volatile__("tdi 4, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void tdi_5_neg100 ( long n ) {
  __asm__ __volatile__("tdi 5, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void tdi_6_neg100 ( long n ) {
  __asm__ __volatile__("tdi 6, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void tdi_7_neg100 ( long n ) {
  __asm__ __volatile__("tdi 7, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void tdi_8_neg100 ( long n ) {
  __asm__ __volatile__("tdi 8, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void tdi_9_neg100 ( long n ) {
  __asm__ __volatile__("tdi 9, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void tdi_10_neg100 ( long n ) {
  __asm__ __volatile__("tdi 10, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void tdi_11_neg100 ( long n ) {
  __asm__ __volatile__("tdi 11, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void tdi_12_neg100 ( long n ) {
  __asm__ __volatile__("tdi 12, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void tdi_13_neg100 ( long n ) {
  __asm__ __volatile__("tdi 13, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void tdi_14_neg100 ( long n ) {
  __asm__ __volatile__("tdi 14, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void tdi_15_neg100 ( long n ) {
  __asm__ __volatile__("tdi 15, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void tdi_16_neg100 ( long n ) {
  __asm__ __volatile__("tdi 16, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void tdi_17_neg100 ( long n ) {
  __asm__ __volatile__("tdi 17, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void tdi_18_neg100 ( long n ) {
  __asm__ __volatile__("tdi 18, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void tdi_19_neg100 ( long n ) {
  __asm__ __volatile__("tdi 19, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void tdi_20_neg100 ( long n ) {
  __asm__ __volatile__("tdi 20, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void tdi_21_neg100 ( long n ) {
  __asm__ __volatile__("tdi 21, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void tdi_22_neg100 ( long n ) {
  __asm__ __volatile__("tdi 22, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void tdi_23_neg100 ( long n ) {
  __asm__ __volatile__("tdi 23, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void tdi_24_neg100 ( long n ) {
  __asm__ __volatile__("tdi 24, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void tdi_25_neg100 ( long n ) {
  __asm__ __volatile__("tdi 25, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void tdi_26_neg100 ( long n ) {
  __asm__ __volatile__("tdi 26, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void tdi_27_neg100 ( long n ) {
  __asm__ __volatile__("tdi 27, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void tdi_28_neg100 ( long n ) {
  __asm__ __volatile__("tdi 28, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void tdi_29_neg100 ( long n ) {
  __asm__ __volatile__("tdi 29, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void tdi_30_neg100 ( long n ) {
  __asm__ __volatile__("tdi 30, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}
static void tdi_31_neg100 ( long n ) {
  __asm__ __volatile__("tdi 31, %0,-100"
		       : /*out*/ : /*in*/ "r" (n) );
}

#endif


int main ( void )
{
#define TWI_GROUP(cmp) \
   try("twi_" #cmp "_neg100", twi_##cmp##_neg100, -150); \
   try("twi_" #cmp "_neg100", twi_##cmp##_neg100, -100); \
   try("twi_" #cmp "_neg100", twi_##cmp##_neg100,  -50);

   TWI_GROUP(0);
   TWI_GROUP(1);
   TWI_GROUP(2);
   TWI_GROUP(3);
   TWI_GROUP(4);
   TWI_GROUP(5);
   TWI_GROUP(6);
   TWI_GROUP(7);
   TWI_GROUP(8);
   TWI_GROUP(9);
   TWI_GROUP(10);
   TWI_GROUP(11);
   TWI_GROUP(12);
   TWI_GROUP(13);
   TWI_GROUP(14);
   TWI_GROUP(15);
   TWI_GROUP(16);
   TWI_GROUP(17);
   TWI_GROUP(18);
   TWI_GROUP(19);
   TWI_GROUP(20);
   TWI_GROUP(21);
   TWI_GROUP(22);
   TWI_GROUP(23);
   TWI_GROUP(24);
   TWI_GROUP(25);
   TWI_GROUP(26);
   TWI_GROUP(27);
   TWI_GROUP(28);
   TWI_GROUP(29);
   TWI_GROUP(30);
   TWI_GROUP(31);
#if defined(__powerpc64__)
#define TDI_GROUP(cmp) \
   try("tdi_" #cmp "_neg100", tdi_##cmp##_neg100, -150); \
   try("tdi_" #cmp "_neg100", tdi_##cmp##_neg100, -100); \
   try("tdi_" #cmp "_neg100", tdi_##cmp##_neg100,  -50);

   TDI_GROUP(0);
   TDI_GROUP(1);
   TDI_GROUP(2);
   TDI_GROUP(3);
   TDI_GROUP(4);
   TDI_GROUP(5);
   TDI_GROUP(6);
   TDI_GROUP(7);
   TDI_GROUP(8);
   TDI_GROUP(9);
   TDI_GROUP(10);
   TDI_GROUP(11);
   TDI_GROUP(12);
   TDI_GROUP(13);
   TDI_GROUP(14);
   TDI_GROUP(15);
   TDI_GROUP(16);
   TDI_GROUP(17);
   TDI_GROUP(18);
   TDI_GROUP(19);
   TDI_GROUP(20);
   TDI_GROUP(21);
   TDI_GROUP(22);
   TDI_GROUP(23);
   TDI_GROUP(24);
   TDI_GROUP(25);
   TDI_GROUP(26);
   TDI_GROUP(27);
   TDI_GROUP(28);
   TDI_GROUP(29);
   TDI_GROUP(30);
   TDI_GROUP(31);
#endif
   return 0;
}
