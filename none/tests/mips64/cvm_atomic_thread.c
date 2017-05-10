/* This is an example of a program which does cavium atomic memory operations
   between two processes which share a page. This test is based on :
   memcheck/tests/atomic_incs.c */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <unistd.h>
#include <sys/wait.h>
#include "tests/sys_mman.h"

#define N 19
#define NNN 3456987  // Number of repetition.

/* Expected values */
int p1_expd[N] = { 2156643710, 2156643710, 3456986, 6913974,
                             4288053322, 0, 4294967295,
                             6913974, 21777111,
                             3456986, 2153186724,
                             6913974, 21777111,
                             4294967295, 4288053323,  // Test 14
                             4288053322, 4273190185,  // Test 16
                             0, 0 };                  // Test 18

long long int p2_expd[N] = { 12633614303292, 12633614303292, 3555751, 6913974,
                              -6913974, 0, -1,
                             6913974, 23901514779351,
                             3456986, 11950752204196,
                             6913974, 23901514779351,
                             -1, -6913973,               // Test 15
                             -6913974, -23901514779351,  // Test 17
                             0, 0 };                     // Test 19

#define IS_8_ALIGNED(_ptr)   (0 == (((unsigned long)(_ptr)) & 7))

__attribute__((noinline)) void atomic_saa ( int* p, int n )
{
#if (_MIPS_ARCH_OCTEON2)
   unsigned long block[2] = { (unsigned long)p, (unsigned long)n };
   __asm__ __volatile__(
      "move $t0, %0"      "\n\t"
      "ld   $t1, 0($t0)"  "\n\t"  // p
      "ld   $t2, 8($t0)"  "\n\t"  // n
      "saa  $t2, ($t1)"   "\n\t"
      : /*out*/
      : /*in*/ "r"(&block[0])
      : /*trash*/ "memory", "t0", "t1", "t2"
   );
#endif
}

__attribute__((noinline)) void atomic_saad ( long long int* p, int n )
{
#if (_MIPS_ARCH_OCTEON2)
   unsigned long block[2] = { (unsigned long)p, (unsigned long)n };
   __asm__ __volatile__(
      "move $t0, %0"      "\n\t"
      "ld   $t1, 0($t0)"  "\n\t"  // p
      "ld   $t2, 8($t0)"  "\n\t"  // n
      "saad $t2, ($t1)"   "\n\t"
      : /*out*/
      : /*in*/ "r"(&block[0])
      : /*trash*/ "memory", "t0", "t1", "t2"
   );
#endif
}

__attribute__((noinline)) void atomic_laa ( int* p, int n )
{
#if (_MIPS_ARCH_OCTEON2)
   unsigned long block[2] = { (unsigned long)p, (unsigned long)n };
   __asm__ __volatile__(
      "move $t0, %0"          "\n\t"
      "ld   $t1, 0($t0)"      "\n\t"  // p
      "ld   $t2, 8($t0)"      "\n\t"  // n
      "laa  $t3, ($t1), $t2"  "\n\t"
      : /*out*/
      : /*in*/ "r"(&block[0])
      : /*trash*/ "memory", "t0", "t1", "t2"
   );
#endif
}

__attribute__((noinline)) void atomic_laad ( long long int* p, int n )
{
#if (_MIPS_ARCH_OCTEON2)
   unsigned long block[2] = { (unsigned long)p, (unsigned long)n };
   __asm__ __volatile__(
      "move $t0, %0"           "\n\t"
      "ld   $t1, 0($t0)"       "\n\t"  // p
      "ld   $t2, 8($t0)"       "\n\t"  // n
      "laad $t3, ($t1), $t2"   "\n\t"
      : /*out*/
      : /*in*/ "r"(&block[0])
      : /*trash*/ "memory", "t0", "t1", "t2", "t3"
   );
#endif
}

__attribute__((noinline)) void atomic_law ( int* p, int n )
{
#if (_MIPS_ARCH_OCTEON2)
   unsigned long block[2] = { (unsigned long)p, (unsigned long)n };
   __asm__ __volatile__(
      "move $t0, %0"           "\n\t"
      "ld   $t1, 0($t0)"       "\n\t"  // p
      "ld   $t2, 8($t0)"       "\n\t"  // n
      "law  $t3, ($t1), $t2"  "\n\t"
      : /*out*/
      : /*in*/ "r"(&block[0])
      : /*trash*/ "memory", "t0", "t1", "t2"
   );
#endif
}

__attribute__((noinline)) void atomic_lawd ( long long int* p, int n )
{
#if (_MIPS_ARCH_OCTEON2)
   unsigned long block[2] = { (unsigned long)p, (unsigned long)n };
   __asm__ __volatile__(
      "move $t0, %0"          "\n\t"
      "ld   $t1, 0($t0)"      "\n\t"  // p
      "ld   $t2, 8($t0)"      "\n\t"  // n
      "lawd $t3, ($t1), $t2"  "\n\t"
      : /*out*/
      : /*in*/ "r"(&block[0])
      : /*trash*/ "memory", "t0", "t1", "t2", "t3"
   );
#endif
}

__attribute__((noinline)) void atomic_lai ( int* p )
{
#if (_MIPS_ARCH_OCTEON2)
   unsigned long block[2] = { (unsigned long)p };
   __asm__ __volatile__(
      "move $t0, %0"      "\n\t"
      "ld   $t1, 0($t0)"  "\n\t"  // p
      "ld   $t2, 8($t0)"  "\n\t"  // n
      "lai  $t2, ($t1)"   "\n\t"
      : /*out*/
      : /*in*/ "r"(&block[0])
      : /*trash*/ "memory", "t0", "t1", "t2"
   );
#endif
}

__attribute__((noinline)) void atomic_laid ( long long int* p )
{
#if (_MIPS_ARCH_OCTEON2)
   unsigned long block[2] = { (unsigned long)p };
   __asm__ __volatile__(
      "move $t0, %0"      "\n\t"
      "ld   $t1, 0($t0)"  "\n\t"  // p
      "ld   $t2, 8($t0)"  "\n\t"  // n
      "laid $t2, ($t1)"   "\n\t"
      : /*out*/
      : /*in*/ "r"(&block[0])
      : /*trash*/ "memory", "t0", "t1", "t2"
   );
#endif
}

__attribute__((noinline)) void atomic_lad ( int* p )
{
#if (_MIPS_ARCH_OCTEON2)
   unsigned long block[2] = { (unsigned long)p };
   __asm__ __volatile__(
      "move $t0, %0"      "\n\t"
      "ld   $t1, 0($t0)"  "\n\t"  // p
      "ld   $t2, 8($t0)"  "\n\t"  // n
      "lad  $t2, ($t1)"   "\n\t"
      : /*out*/
      : /*in*/ "r"(&block[0])
      : /*trash*/ "memory", "t0", "t1", "t2"
   );
#endif
}

__attribute__((noinline)) void atomic_ladd ( long long int* p )
{
#if (_MIPS_ARCH_OCTEON2)
   unsigned long block[2] = { (unsigned long)p };
   __asm__ __volatile__(
      "move $t0, %0"      "\n\t"
      "ld   $t1, 0($t0)"  "\n\t"  // p
      "ld   $t2, 8($t0)"  "\n\t"  // n
      "ladd $t2, ($t1)"   "\n\t"
      : /*out*/
      : /*in*/ "r"(&block[0])
      : /*trash*/ "memory", "t0", "t1", "t2"
   );
#endif
}

__attribute__((noinline)) void atomic_lac ( int* p )
{
#if (_MIPS_ARCH_OCTEON2)
   unsigned long block[2] = { (unsigned long)p };
   __asm__ __volatile__(
      "move $t0, %0"      "\n\t"
      "ld   $t1, 0($t0)"  "\n\t"  // p
      "ld   $t2, 8($t0)"  "\n\t"  // n
      "lac  $t2, ($t1)"   "\n\t"
      : /*out*/
      : /*in*/ "r"(&block[0])
      : /*trash*/ "memory", "t0", "t1", "t2"
   );
#endif
}

__attribute__((noinline)) void atomic_lacd ( long long int* p )
{
#if (_MIPS_ARCH_OCTEON2)
   unsigned long block[2] = { (unsigned long)p };
   __asm__ __volatile__(
      "move $t0, %0"      "\n\t"
      "ld   $t1, 0($t0)"  "\n\t"  // p
      "ld   $t2, 8($t0)"  "\n\t"  // n
      "lacd $t2, ($t1)"   "\n\t"
      : /*out*/
      : /*in*/ "r"(&block[0])
      : /*trash*/ "memory", "t0", "t1", "t2"
   );
#endif
}

__attribute__((noinline)) void atomic_las ( int* p )
{
#if (_MIPS_ARCH_OCTEON2)
   unsigned long block[2] = { (unsigned long)p };
   __asm__ __volatile__(
      "move $t0, %0"      "\n\t"
      "ld   $t1, 0($t0)"  "\n\t"  // p
      "ld   $t2, 8($t0)"  "\n\t"  // n
      "las  $t2, ($t1)"   "\n\t"
      : /*out*/
      : /*in*/ "r"(&block[0])
      : /*trash*/ "memory", "t0", "t1", "t2"
   );
#endif
}

__attribute__((noinline)) void atomic_lasd ( long long int* p )
{
#if (_MIPS_ARCH_OCTEON2)
   unsigned long block[2] = { (unsigned long)p };
   __asm__ __volatile__(
      "move $t0, %0"      "\n\t"
      "ld   $t1, 0($t0)"  "\n\t"  // p
      "ld   $t2, 8($t0)"  "\n\t"  // n
      "lasd $t2, ($t1)"   "\n\t"
      : /*out*/
      : /*in*/ "r"(&block[0])
      : /*trash*/ "memory", "t0", "t1", "t2"
   );
#endif
}

#define TRIOP_AND_SAA(instruction, base1, base2, n)  \
{                                                    \
   __asm__ __volatile__(                             \
      instruction"  $t0, (%0), %2"  "\n\t"           \
      "saa          $t0, (%1)"       "\n\t"          \
      : /*out*/                                      \
      : /*in*/ "r"(base1), "r"(base2), "r"(n)        \
      : /*trash*/ "memory", "t0"                     \
   );                                                \
}

#define TRIOP_AND_SAAD(instruction, base1, base2, n)  \
{                                                     \
   __asm__ __volatile__(                              \
      instruction"  $t0, (%0), %2"  "\n\t"            \
      "saad         $t0, (%1)"       "\n\t"           \
      : /*out*/                                       \
      : /*in*/ "r"(base1), "r"(base2), "r"(n)         \
      : /*trash*/ "memory", "t0"                      \
   );                                                 \
}

#define BINOP_AND_SAA(instruction, base1, base2)  \
{                                                 \
   __asm__ __volatile__(                          \
      instruction"  $t0, (%0)"  "\n\t"            \
      "saa          $t0, (%1)"  "\n\t"            \
      : /*out*/                                   \
      : /*in*/ "r"(base1), "r"(base2)             \
      : /*trash*/ "memory", "t0"                  \
   );                                             \
}

#define BINOP_AND_SAAD(instruction, base1, base2)  \
{                                                  \
   __asm__ __volatile__(                           \
      instruction"  $t0, (%0)"  "\n\t"             \
      "saad         $t0, (%1)"  "\n\t"             \
      : /*out*/                                    \
      : /*in*/ "r"(base1), "r"(base2)              \
      : /*trash*/ "memory", "t0"                   \
   );                                              \
}

int main ( int argc, char** argv )
{
#if (_MIPS_ARCH_OCTEON2)
   int    i, status;
   char*  page[N];
   int* p1[N];
   long long int* p2[N];
   pid_t  child, pc2;

   for (i = 0; i < N; i++) {
      page[i] = mmap( 0, sysconf(_SC_PAGESIZE),
                      PROT_READ|PROT_WRITE,
                      MAP_ANONYMOUS|MAP_SHARED, -1, 0 );
      if (page[i] == MAP_FAILED) {
         perror("mmap failed");
         exit(1);
      }
      p1[i] = (int*)(page[i] + 0);
      p2[i] = (long long int*)(page[i] + 256);

      assert( IS_8_ALIGNED(p1[i]) );
      assert( IS_8_ALIGNED(p2[i]) );

      memset(page[i], 0, 1024);
      memset(page[i], 0, 1024);

      *p1[i] = 0;
      *p2[i] = 0;
   }

   child = fork();
   if (child == -1) {
      perror("fork() failed\n");
      return 1;
   }

   if (child == 0) {
      /* --- CHILD --- */
      for (i = 0; i < NNN; i++) {
         atomic_saa(p1[0], i);
         atomic_saad(p2[0], i + 98765 ); /* ensure we hit the upper 32 bits */
         atomic_laa(p1[1], i);
         atomic_laad(p2[1], i + 98765 ); /* ensure we hit the upper 32 bits */
         atomic_law(p1[2], i);
         atomic_lawd(p2[2], i + 98765 ); /* ensure we hit the upper 32 bits */
         atomic_lai(p1[3]);
         atomic_laid(p2[3]);
         atomic_lad(p1[4]);
         atomic_ladd(p2[4]);
         atomic_lac(p1[5]);
         atomic_lacd(p2[5]);
         atomic_las(p1[6]);
         atomic_lasd(p2[6]);
         TRIOP_AND_SAA("laa ", p1[7], p1[8], 1)
         TRIOP_AND_SAAD("laad ", p2[7], p2[8], 1)
         TRIOP_AND_SAA("law ", p1[9], p1[10], i)
         TRIOP_AND_SAAD("lawd ", p2[9], p2[10], i)
         BINOP_AND_SAA("lai ", p1[11], p1[12])
         BINOP_AND_SAAD("laid ", p2[11], p2[12])
         BINOP_AND_SAA("las ", p1[13], p1[14])
         BINOP_AND_SAAD("lasd ", p2[13], p2[14])
         BINOP_AND_SAA("lad ", p1[15], p1[16])
         BINOP_AND_SAAD("ladd ", p2[15], p2[16])
         BINOP_AND_SAA("lac ", p1[17], p1[18])
         BINOP_AND_SAAD("lacd ", p2[17], p2[18])
      }
      return 1;
      /* NOTREACHED */

   }

   /* --- PARENT --- */
   for (i = 0; i < NNN; i++) {
      atomic_saa(p1[0], i);
      atomic_saad(p2[0], i + 98765); /* ensure we hit the upper 32 bits */
      atomic_laa(p1[1], i);
      atomic_laad(p2[1], i + 98765); /* ensure we hit the upper 32 bits */
      atomic_law(p1[2], i);
      atomic_lawd(p2[2], i + 98765 ); /* ensure we hit the upper 32 bits */
      atomic_lai(p1[3]);
      atomic_laid(p2[3]);
      atomic_lad(p1[4]);
      atomic_ladd(p2[4]);
      atomic_lac(p1[5]);
      atomic_lacd(p2[5]);
      atomic_las(p1[6]);
      atomic_lasd(p2[6]);
      TRIOP_AND_SAA("laa ", p1[7], p1[8], 1)
      TRIOP_AND_SAAD("laad ", p2[7], p2[8], 1)
      TRIOP_AND_SAA("law ", p1[9], p1[10], i)
      TRIOP_AND_SAAD("lawd ", p2[9], p2[10], i)
      BINOP_AND_SAA("lai ", p1[11], p1[12])
      BINOP_AND_SAAD("laid ", p2[11], p2[12])
      BINOP_AND_SAA("las ", p1[13], p1[14])
      BINOP_AND_SAAD("lasd ", p2[13], p2[14])
      BINOP_AND_SAA("lad ", p1[15], p1[16])
      BINOP_AND_SAAD("ladd ", p2[15], p2[16])
      BINOP_AND_SAA("lac ", p1[17], p1[18])
      BINOP_AND_SAAD("lacd ", p2[17], p2[18])
   }

   pc2 = waitpid(child, &status, 0);
   assert(pc2 == child);

   /* assert that child finished normally */
   assert(WIFEXITED(status));

   printf("Store Atomic Add: 32 bit %u, 64 bit %lld\n",      *p1[0], *p2[0]);
   printf("Load Atomic Add: 32 bit %u, 64 bit %lld\n",       *p1[1], *p2[1]);
   printf("Load Atomic Swap: 32 bit %u, 64 bit %lld\n",      *p1[2], *p2[2]);
   printf("Load Atomic Increment: 32 bit %u, 64 bit %lld\n", *p1[3], *p2[3]);
   printf("Load Atomic Decrement: 32 bit %u, 64 bit %lld\n", *p1[4], *p2[4]);
   printf("Load Atomic Clear: 32 bit %u, 64 bit %lld\n",     *p1[5], *p2[5]);
   printf("Load Atomic Set: 32 bit %u, 64 bit %lld\n",       *p1[6], *p2[6]);
   printf("laa and saa: base1: %u, base2: %u\n",             *p1[7], *p1[8]);
   printf("laad and saad: base1: %lld, base2: %lld\n",       *p2[7], *p2[8]);
   printf("law and saa: base1: %u, base2: %u\n",             *p1[9], *p1[10]);
   printf("lawd and saad: base1: %lld, base2: %lld\n",       *p2[9], *p2[10]);
   printf("lai and saa: base1: %u, base2: %u\n",             *p1[11], *p1[12]);
   printf("laid and saad: base1: %lld, base2: %lld\n",       *p2[11], *p2[12]);
   printf("las and saa: base1: %u, base2: %u\n",             *p1[13], *p1[14]);
   printf("lasd and saad: base1: %lld, base2: %lld\n",       *p2[13], *p2[14]);
   printf("lad and saa: base1: %u, base2: %u\n",             *p1[15], *p1[16]);
   printf("ladd and saad: base1: %lld, base2: %lld\n",       *p2[15], *p2[16]);
   printf("lac and saa: base1: %u, base2: %u\n",             *p1[17], *p1[18]);
   printf("lacd and saad: base1: %lld, base2: %lld\n",       *p2[17], *p2[18]);

   for (i = 0; i < N; i++) {
      if (p1_expd[i] == *p1[i] && p2_expd[i] == *p2[i]) {
         printf("PASS %d\n", i+1);
      } else {
         printf("FAIL %d -- see source code for expected values\n", i+1);
      }
   }

   printf("parent exits\n");
#endif
   return 0;
}
