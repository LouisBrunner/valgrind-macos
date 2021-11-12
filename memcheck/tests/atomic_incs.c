
/* This is an example of a program which does atomic memory operations
   between two processes which share a page.  Valgrind 3.4.1 and
   earlier produce incorrect answers because it does not preserve
   atomicity of the relevant instructions in the generated code; but
   the post-DCAS-merge versions of Valgrind do behave correctly. */

/* On ARM, this can be compiled into either ARM or Thumb code, so as
   to test both A and T encodings of LDREX/STREX et al.  Also on ARM,
   it tests doubleword atomics (LDREXD, STREXD) which I don't think it
   does on any other platform. */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <unistd.h>
#include <sys/wait.h>
#include "tests/sys_mman.h"
#include "pub_core_basics.h"

#define NNN 3456987

#define IS_8_ALIGNED(_ptr)   (0 == (((unsigned long)(_ptr)) & 7))
#define IS_16_ALIGNED(_ptr)  (0 == (((unsigned long)(_ptr)) & 15))

// U128 from libvex_basictypes.h is a 4-x-UInt array, which is a bit
// inconvenient, hence:
typedef
   struct {
      // assuming little-endianness
      unsigned long long int lo64;
      unsigned long long int hi64;
   }
   MyU128;


__attribute__((noinline)) void atomic_add_8bit ( char* p, int n ) 
{
#if defined(VGA_x86)
   unsigned long block[2];
   block[0] = (unsigned long)p;
   block[1] = n;
   __asm__ __volatile__(
      "movl 0(%%esi),%%eax"      "\n\t"
      "movl 4(%%esi),%%ebx"      "\n\t"
      "lock; addb %%bl,(%%eax)"  "\n"
      : : "S"(&block[0])/* S means "esi only" */ : "memory","cc","eax","ebx"
   );
#elif defined(VGA_amd64)
   unsigned long block[2];
   block[0] = (unsigned long)p;
   block[1] = n;
   __asm__ __volatile__(
      "movq 0(%%rsi),%%rax"      "\n\t"
      "movq 8(%%rsi),%%rbx"      "\n\t"
      "lock; addb %%bl,(%%rax)"  "\n"
      : : "S"(&block[0])/* S means "rsi only" */ : "memory","cc","rax","rbx"
   );
#elif defined(VGA_ppc32)
   /* Nasty hack.  Does correctly atomically do *p += n, but only if p
      is 4-aligned -- guaranteed by caller. */
   unsigned long success;
   do {
      __asm__ __volatile__(
         "lwarx  15,0,%1"    "\n\t"
         "add    15,15,%2"   "\n\t"
         "stwcx. 15,0,%1"    "\n\t"
         "mfcr   %0"         "\n\t"
         "srwi   %0,%0,29"   "\n\t"
         "andi.  %0,%0,1"    "\n"
         : /*out*/"=b"(success)
         : /*in*/ "b"(p), "b"(((unsigned long)n) << 24)
         : /*trash*/ "memory", "cc", "r15"
      );
   } while (success != 1);
#elif defined(VGA_ppc64be)
   /* Nasty hack.  Does correctly atomically do *p += n, but only if p
      is 8-aligned -- guaranteed by caller. */
   unsigned long success;
   do {
      __asm__ __volatile__(
         "ldarx  15,0,%1"    "\n\t"
         "add    15,15,%2"   "\n\t"
         "stdcx. 15,0,%1"    "\n\t"
         "mfcr   %0"         "\n\t"
         "srwi   %0,%0,29"   "\n\t"
         "andi.  %0,%0,1"    "\n"
         : /*out*/"=b"(success)
         : /*in*/ "b"(p), "b"(((unsigned long)n) << 56)
         : /*trash*/ "memory", "cc", "r15"
      );
   } while (success != 1);
#elif defined(VGA_ppc64le)
   /* Nasty hack.  Does correctly atomically do *p += n, but only if p
      is 8-aligned -- guaranteed by caller. */
   unsigned long success;
   do {
      __asm__ __volatile__(
         "ldarx  15,0,%1"    "\n\t"
         "add    15,15,%2"   "\n\t"
         "stdcx. 15,0,%1"    "\n\t"
         "mfcr   %0"         "\n\t"
         "srwi   %0,%0,29"   "\n\t"
         "andi.  %0,%0,1"    "\n"
         : /*out*/"=b"(success)
         : /*in*/ "b"(p), "b"(((unsigned long)n))
         : /*trash*/ "memory", "cc", "r15"
      );
   } while (success != 1);
#elif defined(VGA_arm)
   unsigned int block[3]
      = { (unsigned int)p, (unsigned int)n, 0xFFFFFFFF };
   do {
      __asm__ __volatile__(
         "mov    r5, %0"         "\n\t"
         "ldr    r9, [r5, #0]"   "\n\t" // p
         "ldr    r10, [r5, #4]"  "\n\t" // n
         "ldrexb r8, [r9]"       "\n\t"
         "add    r8, r8, r10"    "\n\t"
         "strexb r4, r8, [r9]"   "\n\t"
         "str    r4, [r5, #8]"   "\n\t"
         : /*out*/
         : /*in*/ "r"(&block[0])
         : /*trash*/ "memory", "cc", "r5", "r8", "r9", "r10", "r4"
      );
   } while (block[2] != 0);
#elif defined(VGA_arm64)
   unsigned long long int block[3]
      = { (unsigned long long int)p, (unsigned long long int)n,
          0xFFFFFFFFFFFFFFFFULL};
   do {
      __asm__ __volatile__(
         "mov   x5, %0"         "\n\t"
         "ldr   x9, [x5, #0]"   "\n\t" // p
         "ldr   x10, [x5, #8]"  "\n\t" // n
         "ldxrb w8, [x9]"       "\n\t"
         "add   x8, x8, x10"    "\n\t"
         "stxrb w4, w8, [x9]"    "\n\t"
         "str   x4, [x5, #16]"   "\n\t"
         : /*out*/
         : /*in*/ "r"(&block[0])
         : /*trash*/ "memory", "cc", "x5", "x8", "x9", "x10", "x4"
      );
   } while (block[2] != 0);
#elif defined(VGA_s390x)
   int dummy;
   __asm__ __volatile__(
      "   l	0,%0\n\t"
      "0: st	0,%1\n\t"
      "   icm	1,1,%1\n\t"
      "   ar	1,%2\n\t"
      "   stcm  1,1,%1\n\t"
      "   l     1,%1\n\t"
      "   cs	0,1,%0\n\t"
      "   jl    0b\n\t"
      : "+m" (*p), "+m" (dummy)
      : "d" (n)
      : "cc", "memory", "0", "1");
#elif defined(VGA_mips32) || defined (VGA_nanomips)
   /* We rely on the fact that p is 4-aligned. Otherwise 'll' may throw an
      exception that can cause this function to fail. */
#if defined (_MIPSEL)
   unsigned int block[3]
      = { (unsigned int)p, (unsigned int)n, 0x0 };
   do {
      __asm__ __volatile__(
         "move $t0, %0"           "\n\t"
         "lw   $t1, 0($t0)"       "\n\t"  // p
         "lw   $t2, 4($t0)"       "\n\t"  // n
         "andi $t2, $t2, 0xFF"    "\n\t"  // n = n and 0xFF
         "li   $t4, 0xFF"         "\n\t"
         "nor  $t4, $t4, $zero"   "\n\t"  // $t4 = 0xFFFFFF00
         "ll   $t3, 0($t1)"       "\n\t"  // $t3 = old value
         "and  $t4, $t4, $t3"     "\n\t"  // $t4 = $t3 and 0xFFFFFF00
         "addu $t3, $t3, $t2"     "\n\t"  // $t3 = $t3 + n
         "andi $t3, $t3, 0xFF"    "\n\t"  // $t3 = $t3 and 0xFF
         "or   $t3, $t3, $t4"     "\n\t"  // $t3 = $t3 or $t4
         "sc   $t3, 0($t1)"       "\n\t"
         "sw   $t3, 8($t0)"       "\n\t"  // save result
         : /*out*/
         : /*in*/ "r"(&block[0])
         : /*trash*/ "memory", "t0", "t1", "t2", "t3", "t4"
      );
   } while (block[2] != 1);
#elif defined (_MIPSEB)
   unsigned int block[3]
      = { (unsigned int)p, (unsigned int)n << 24, 0x0 };
   do {
      __asm__ __volatile__(
         "move $t0, %0"          "\n\t"
         "lw   $t1, 0($t0)"      "\n\t"  // p
         "lw   $t2, 4($t0)"      "\n\t"  // n
         "ll   $t3, 0($t1)"      "\n\t"
         "addu $t3, $t3, $t2"    "\n\t"
         "sc   $t3, 0($t1)"      "\n\t"
         "sw   $t3, 8($t0)"      "\n\t"
         : /*out*/
         : /*in*/ "r"(&block[0])
         : /*trash*/ "memory", "t0", "t1", "t2", "t3"
      );
   } while (block[2] != 1);
#endif
#elif defined(VGA_mips64)
   /* We rely on the fact that p is 4-aligned. Otherwise 'll' may throw an
      exception that can cause this function to fail. */
#if defined (_MIPSEL)
   RegWord block[3]
      = { (RegWord)(Addr)p, (RegWord)n, 0x0ULL };
   do {
      __asm__ __volatile__(
         "move $t0, %0"           "\n\t"
         "ld   $t1, 0($t0)"       "\n\t"  // p
         "ld   $t2, 8($t0)"       "\n\t"  // n
         "andi $t2, $t2, 0xFF"    "\n\t"  // n = n and 0xFF
         "li   $s0, 0xFF"         "\n\t"
         "nor  $s0, $s0, $zero"   "\n\t"  // $s0 = 0xFFFFFF00
         "ll   $t3, 0($t1)"       "\n\t"  // $t3 = old value
         "and  $s0, $s0, $t3"     "\n\t"  // $s0 = $t3 and 0xFFFFFF00
         "addu $t3, $t3, $t2"     "\n\t"  // $t3 = $t3 + n
         "andi $t3, $t3, 0xFF"    "\n\t"  // $t3 = $t3 and 0xFF
         "or   $t3, $t3, $s0"     "\n\t"  // $t3 = $t3 or $s0
         "sc   $t3, 0($t1)"       "\n\t"
         "sw   $t3, 16($t0)"      "\n\t"  // save result
         : /*out*/
         : /*in*/ "r"(&block[0])
         : /*trash*/ "memory", "t0", "t1", "t2", "t3", "s0"
      );
   } while (block[2] != 1);
#elif defined (_MIPSEB)
   RegWord block[3]
      = { (RegWord)(Addr)p, (RegWord)n << 56, 0x0 };
   do {
      __asm__ __volatile__(
         "move  $t0, %0"          "\n\t"
         "ld    $t1, 0($t0)"      "\n\t"  // p
         "ld    $t2, 8($t0)"      "\n\t"  // n
         "lld   $t3, 0($t1)"      "\n\t"
         "daddu $t3, $t3, $t2"    "\n\t"
         "scd   $t3, 0($t1)"      "\n\t"
         "sd    $t3, 16($t0)"     "\n\t"
         : /*out*/
         : /*in*/ "r"(&block[0])
         : /*trash*/ "memory", "t0", "t1", "t2", "t3"
      );
   } while (block[2] != 1);
#endif
#else
# error "Unsupported arch"
#endif
}


__attribute__((noinline)) void atomic_add_16bit ( short* p, int n ) 
{
#if defined(VGA_x86)
   unsigned long block[2];
   block[0] = (unsigned long)p;
   block[1] = n;
   __asm__ __volatile__(
      "movl 0(%%esi),%%eax"      "\n\t"
      "movl 4(%%esi),%%ebx"      "\n\t"
      "lock; addw %%bx,(%%eax)"  "\n"
      : : "S"(&block[0])/* S means "esi only" */ : "memory","cc","eax","ebx"
   );
#elif defined(VGA_amd64)
   unsigned long block[2];
   block[0] = (unsigned long)p;
   block[1] = n;
   __asm__ __volatile__(
      "movq 0(%%rsi),%%rax"      "\n\t"
      "movq 8(%%rsi),%%rbx"      "\n\t"
      "lock; addw %%bx,(%%rax)"  "\n"
      : : "S"(&block[0])/* S means "rsi only" */ : "memory","cc","rax","rbx"
   );
#elif defined(VGA_ppc32)
   /* Nasty hack.  Does correctly atomically do *p += n, but only if p
      is 8-aligned -- guaranteed by caller. */
   unsigned long success;
   do {
      __asm__ __volatile__(
         "lwarx  15,0,%1"    "\n\t"
         "add    15,15,%2"   "\n\t"
         "stwcx. 15,0,%1"    "\n\t"
         "mfcr   %0"         "\n\t"
         "srwi   %0,%0,29"   "\n\t"
         "andi.  %0,%0,1"    "\n"
         : /*out*/"=b"(success)
         : /*in*/ "b"(p), "b"(((unsigned long)n) << 16)
         : /*trash*/ "memory", "cc", "r15"
      );
   } while (success != 1);
#elif defined(VGA_ppc64be)
   /* Nasty hack.  Does correctly atomically do *p += n, but only if p
      is 8-aligned -- guaranteed by caller. */
   unsigned long success;
   do {
      __asm__ __volatile__(
         "ldarx  15,0,%1"    "\n\t"
         "add    15,15,%2"   "\n\t"
         "stdcx. 15,0,%1"    "\n\t"
         "mfcr   %0"         "\n\t"
         "srwi   %0,%0,29"   "\n\t"
         "andi.  %0,%0,1"    "\n"
         : /*out*/"=b"(success)
         : /*in*/ "b"(p), "b"(((unsigned long)n) << 48)
         : /*trash*/ "memory", "cc", "r15"
      );
   } while (success != 1);
#elif defined(VGA_ppc64le)
   /* Nasty hack.  Does correctly atomically do *p += n, but only if p
      is 8-aligned -- guaranteed by caller. */
   unsigned long success;
   do {
      __asm__ __volatile__(
         "ldarx  15,0,%1"    "\n\t"
         "add    15,15,%2"   "\n\t"
         "stdcx. 15,0,%1"    "\n\t"
         "mfcr   %0"         "\n\t"
         "srwi   %0,%0,29"   "\n\t"
         "andi.  %0,%0,1"    "\n"
         : /*out*/"=b"(success)
         : /*in*/ "b"(p), "b"(((unsigned long)n))
         : /*trash*/ "memory", "cc", "r15"
      );
   } while (success != 1);
#elif defined(VGA_arm)
   unsigned int block[3]
      = { (unsigned int)p, (unsigned int)n, 0xFFFFFFFF };
   do {
      __asm__ __volatile__(
         "mov    r5, %0"         "\n\t"
         "ldr    r9, [r5, #0]"   "\n\t" // p
         "ldr    r10, [r5, #4]"  "\n\t" // n
         "ldrexh r8, [r9]"       "\n\t"
         "add    r8, r8, r10"    "\n\t"
         "strexh r4, r8, [r9]"   "\n\t"
         "str    r4, [r5, #8]"   "\n\t"
         : /*out*/
         : /*in*/ "r"(&block[0])
         : /*trash*/ "memory", "cc", "r5", "r8", "r9", "r10", "r4"
      );
   } while (block[2] != 0);
#elif defined(VGA_arm64)
   unsigned long long int block[3]
      = { (unsigned long long int)p, (unsigned long long int)n,
          0xFFFFFFFFFFFFFFFFULL};
   do {
      __asm__ __volatile__(
         "mov   x5, %0"         "\n\t"
         "ldr   x9, [x5, #0]"   "\n\t" // p
         "ldr   x10, [x5, #8]"  "\n\t" // n
         "ldxrh w8, [x9]"       "\n\t"
         "add   x8, x8, x10"    "\n\t"
         "stxrh w4, w8, [x9]"    "\n\t"
         "str   x4, [x5, #16]"   "\n\t"
         : /*out*/
         : /*in*/ "r"(&block[0])
         : /*trash*/ "memory", "cc", "x5", "x8", "x9", "x10", "x4"
      );
   } while (block[2] != 0);
#elif defined(VGA_s390x)
   int dummy;
   __asm__ __volatile__(
      "   l	0,%0\n\t"
      "0: st	0,%1\n\t"
      "   icm	1,3,%1\n\t"
      "   ar	1,%2\n\t"
      "   stcm  1,3,%1\n\t"
      "   l     1,%1\n\t"
      "   cs	0,1,%0\n\t"
      "   jl    0b\n\t"
      : "+m" (*p), "+m" (dummy)
      : "d" (n)
      : "cc", "memory", "0", "1");
#elif defined(VGA_mips32) || defined (VGA_nanomips)
   /* We rely on the fact that p is 4-aligned. Otherwise 'll' may throw an
      exception that can cause this function to fail. */
#if defined (_MIPSEL)
   unsigned int block[3]
      = { (unsigned int)p, (unsigned int)n, 0x0 };
   do {
      __asm__ __volatile__(
         "move $t0, %0"           "\n\t"
         "lw   $t1, 0($t0)"       "\n\t"  // p
         "lw   $t2, 4($t0)"       "\n\t"  // n
         "andi $t2, $t2, 0xFFFF"  "\n\t"  // n = n and 0xFFFF
         "li   $t4, 0xFFFF"       "\n\t"
         "nor  $t4, $t4, $zero"   "\n\t"  // $t4 = 0xFFFF0000
         "ll   $t3, 0($t1)"       "\n\t"  // $t3 = old value
         "and  $t4, $t4, $t3"     "\n\t"  // $t4 = $t3 and 0xFFFF0000
         "addu $t3, $t3, $t2"     "\n\t"  // $t3 = $t3 + n
         "andi $t3, $t3, 0xFFFF"  "\n\t"  // $t3 = $t3 and 0xFFFF
         "or   $t3, $t3, $t4"     "\n\t"  // $t3 = $t3 or $t4
         "sc   $t3, 0($t1)"       "\n\t"
         "sw   $t3, 8($t0)"       "\n\t"  // save result
         : /*out*/
         : /*in*/ "r"(&block[0])
         : /*trash*/ "memory", "t0", "t1", "t2", "t3", "t4"
      );
   } while (block[2] != 1);
#elif defined (_MIPSEB)
   unsigned int block[3]
      = { (unsigned int)p, (unsigned int)n << 16, 0x0 };
   do {
      __asm__ __volatile__(
         "move $t0, %0"          "\n\t"
         "lw   $t1, 0($t0)"      "\n\t"  // p
         "lw   $t2, 4($t0)"      "\n\t"  // n
         "ll   $t3, 0($t1)"      "\n\t"
         "addu $t3, $t3, $t2"    "\n\t"
         "sc   $t3, 0($t1)"      "\n\t"
         "sw   $t3, 8($t0)"      "\n\t"
         : /*out*/
         : /*in*/ "r"(&block[0])
         : /*trash*/ "memory", "t0", "t1", "t2", "t3"
      );
   } while (block[2] != 1);
#endif
#elif defined(VGA_mips64)
   /* We rely on the fact that p is 4-aligned. Otherwise 'll' may throw an
      exception that can cause this function to fail. */
#if defined (_MIPSEL)
   RegWord block[3]
      = { (RegWord)(Addr)p, (RegWord)n, 0x0ULL };
   do {
      __asm__ __volatile__(
         "move $t0, %0"           "\n\t"
         "ld   $t1, 0($t0)"       "\n\t"  // p
         "ld   $t2, 8($t0)"       "\n\t"  // n
         "andi $t2, $t2, 0xFFFF"  "\n\t"  // n = n and 0xFFFF
         "li   $s0, 0xFFFF"       "\n\t"
         "nor  $s0, $s0, $zero"   "\n\t"  // $s0= 0xFFFF0000
         "ll   $t3, 0($t1)"       "\n\t"  // $t3 = old value
         "and  $s0, $s0, $t3"     "\n\t"  // $s0 = $t3 and 0xFFFF0000
         "addu $t3, $t3, $t2"     "\n\t"  // $t3 = $t3 + n
         "andi $t3, $t3, 0xFFFF"  "\n\t"  // $t3 = $t3 and 0xFFFF
         "or   $t3, $t3, $s0"     "\n\t"  // $t3 = $t3 or $s0
         "sc   $t3, 0($t1)"       "\n\t"
         "sw   $t3, 16($t0)"      "\n\t"  // save result
         : /*out*/
         : /*in*/ "r"(&block[0])
         : /*trash*/ "memory", "t0", "t1", "t2", "t3", "s0"
      );
   } while (block[2] != 1);
#elif defined (_MIPSEB)
   RegWord block[3]
      = { (RegWord)(Addr)p, (RegWord)n << 48, 0x0 };
   do {
      __asm__ __volatile__(
         "move  $t0, %0"          "\n\t"
         "ld    $t1, 0($t0)"      "\n\t"  // p
         "ld    $t2, 8($t0)"      "\n\t"  // n
         "lld   $t3, 0($t1)"      "\n\t"
         "daddu $t3, $t3, $t2"    "\n\t"
         "scd   $t3, 0($t1)"      "\n\t"
         "sd    $t3, 16($t0)"     "\n\t"
         : /*out*/
         : /*in*/ "r"(&block[0])
         : /*trash*/ "memory", "t0", "t1", "t2", "t3"
      );
   } while (block[2] != 1);
#endif
#else
# error "Unsupported arch"
#endif
}

__attribute__((noinline)) void atomic_add_32bit ( int* p, int n ) 
{
#if defined(VGA_x86)
   unsigned long block[2];
   block[0] = (unsigned long)p;
   block[1] = n;
   __asm__ __volatile__(
      "movl 0(%%esi),%%eax"       "\n\t"
      "movl 4(%%esi),%%ebx"       "\n\t"
      "lock; addl %%ebx,(%%eax)"  "\n"
      : : "S"(&block[0])/* S means "esi only" */ : "memory","cc","eax","ebx"
   );
#elif defined(VGA_amd64)
   unsigned long block[2];
   block[0] = (unsigned long)p;
   block[1] = n;
   __asm__ __volatile__(
      "movq 0(%%rsi),%%rax"       "\n\t"
      "movq 8(%%rsi),%%rbx"       "\n\t"
      "lock; addl %%ebx,(%%rax)"  "\n"
      : : "S"(&block[0])/* S means "rsi only" */ : "memory","cc","rax","rbx"
   );
#elif defined(VGA_ppc32)
   unsigned long success;
   do {
      __asm__ __volatile__(
         "lwarx  15,0,%1"    "\n\t"
         "add    15,15,%2"   "\n\t"
         "stwcx. 15,0,%1"    "\n\t"
         "mfcr   %0"         "\n\t"
         "srwi   %0,%0,29"   "\n\t"
         "andi.  %0,%0,1"    "\n"
         : /*out*/"=b"(success)
         : /*in*/ "b"(p), "b"(n)
         : /*trash*/ "memory", "cc", "r15"
      );
   } while (success != 1);
#elif defined(VGA_ppc64be)
   /* Nasty hack.  Does correctly atomically do *p += n, but only if p
      is 8-aligned -- guaranteed by caller. */
   unsigned long success;
   do {
      __asm__ __volatile__(
         "ldarx  15,0,%1"    "\n\t"
         "add    15,15,%2"   "\n\t"
         "stdcx. 15,0,%1"    "\n\t"
         "mfcr   %0"         "\n\t"
         "srwi   %0,%0,29"   "\n\t"
         "andi.  %0,%0,1"    "\n"
         : /*out*/"=b"(success)
         : /*in*/ "b"(p), "b"(((unsigned long)n) << 32)
         : /*trash*/ "memory", "cc", "r15"
      );
   } while (success != 1);
#elif defined(VGA_ppc64le)
   /* Nasty hack.  Does correctly atomically do *p += n, but only if p
      is 8-aligned -- guaranteed by caller. */
   unsigned long success;
   do {
      __asm__ __volatile__(
         "ldarx  15,0,%1"    "\n\t"
         "add    15,15,%2"   "\n\t"
         "stdcx. 15,0,%1"    "\n\t"
         "mfcr   %0"         "\n\t"
         "srwi   %0,%0,29"   "\n\t"
         "andi.  %0,%0,1"    "\n"
         : /*out*/"=b"(success)
         : /*in*/ "b"(p), "b"(((unsigned long)n))
         : /*trash*/ "memory", "cc", "r15"
      );
   } while (success != 1);
#elif defined(VGA_arm)
   unsigned int block[3]
      = { (unsigned int)p, (unsigned int)n, 0xFFFFFFFF };
   do {
      __asm__ __volatile__(
         "mov   r5, %0"         "\n\t"
         "ldr   r9, [r5, #0]"   "\n\t" // p
         "ldr   r10, [r5, #4]"  "\n\t" // n
         "ldrex r8, [r9]"       "\n\t"
         "add   r8, r8, r10"    "\n\t"
         "strex r4, r8, [r9]"   "\n\t"
         "str   r4, [r5, #8]"   "\n\t"
         : /*out*/
         : /*in*/ "r"(&block[0])
         : /*trash*/ "memory", "cc", "r5", "r8", "r9", "r10", "r4"
      );
   } while (block[2] != 0);
#elif defined(VGA_arm64)
   unsigned long long int block[3]
      = { (unsigned long long int)p, (unsigned long long int)n,
          0xFFFFFFFFFFFFFFFFULL};
   do {
      __asm__ __volatile__(
         "mov   x5, %0"         "\n\t"
         "ldr   x9, [x5, #0]"   "\n\t" // p
         "ldr   x10, [x5, #8]"  "\n\t" // n
         "ldxr  w8, [x9]"       "\n\t"
         "add   x8, x8, x10"    "\n\t"
         "stxr  w4, w8, [x9]"    "\n\t"
         "str   x4, [x5, #16]"   "\n\t"
         : /*out*/
         : /*in*/ "r"(&block[0])
         : /*trash*/ "memory", "cc", "x5", "x8", "x9", "x10", "x4"
      );
   } while (block[2] != 0);
#elif defined(VGA_s390x)
   __asm__ __volatile__(
      "   l	0,%0\n\t"
      "0: lr	1,0\n\t"
      "   ar	1,%1\n\t"
      "   cs	0,1,%0\n\t"
      "   jl    0b\n\t"
      : "+m" (*p)
      : "d" (n)
      : "cc", "memory", "0", "1");
#elif defined(VGA_mips32) || defined (VGA_nanomips)
   unsigned int block[3]
      = { (unsigned int)p, (unsigned int)n, 0x0 };
   do {
      __asm__ __volatile__(
         "move $t0, %0"        "\n\t"
         "lw   $t1, 0($t0)"    "\n\t"  // p
         "lw   $t2, 4($t0)"    "\n\t"  // n
         "ll   $t3, 0($t1)"    "\n\t"
         "addu $t3, $t3, $t2"  "\n\t"
         "sc   $t3, 0($t1)"    "\n\t"
         "sw   $t3, 8($t0)"    "\n\t"
         : /*out*/
         : /*in*/ "r"(&block[0])
         : /*trash*/ "memory", "t0", "t1", "t2", "t3"
      );
   } while (block[2] != 1);
#elif defined(VGA_mips64)
   RegWord block[3]
      = { (RegWord)(Addr)p, (RegWord)n, 0x0ULL };
   do {
      __asm__ __volatile__(
         "move  $t0, %0"        "\n\t"
         "ld    $t1, 0($t0)"    "\n\t"  // p
         "ld    $t2, 8($t0)"    "\n\t"  // n
         "ll    $t3, 0($t1)"    "\n\t"
         "addu  $t3, $t3, $t2"  "\n\t"
         "sc    $t3, 0($t1)"    "\n\t"
         "sd    $t3, 16($t0)"   "\n\t"
         : /*out*/
         : /*in*/ "r"(&block[0])
         : /*trash*/ "memory", "t0", "t1", "t2", "t3"
      );
   } while (block[2] != 1);
#else
# error "Unsupported arch"
#endif
}

__attribute__((noinline)) void atomic_add_64bit ( long long int* p, int n ) 
{
#if defined(VGA_x86) || defined(VGA_ppc32) || defined(VGA_mips32) \
 || defined (VGA_nanomips)
   /* do nothing; is not supported */
#elif defined(VGA_amd64)
   // this is a bit subtle.  It relies on the fact that, on a 64-bit platform,
   // sizeof(unsigned long long int) == sizeof(unsigned long) == sizeof(void*) 
   unsigned long long int block[2];
   block[0] = (unsigned long long int)(unsigned long)p;
   block[1] = n;
   __asm__ __volatile__(
      "movq 0(%%rsi),%%rax"      "\n\t"
      "movq 8(%%rsi),%%rbx"      "\n\t"
      "lock; addq %%rbx,(%%rax)" "\n"
      : : "S"(&block[0])/* S means "rsi only" */ : "memory","cc","rax","rbx"
   );
#elif defined(VGA_ppc64be) || defined(VGA_ppc64le)
   unsigned long success;
   do {
      __asm__ __volatile__(
         "ldarx  15,0,%1"    "\n\t"
         "add    15,15,%2"   "\n\t"
         "stdcx. 15,0,%1"    "\n\t"
         "mfcr   %0"         "\n\t"
         "srwi   %0,%0,29"   "\n\t"
         "andi.  %0,%0,1"    "\n"
         : /*out*/"=b"(success)
         : /*in*/ "b"(p), "b"(n)
         : /*trash*/ "memory", "cc", "r15"
      );
   } while (success != 1);
#elif defined(VGA_arm)
   unsigned long long int block[3]
     = { (unsigned long long int)(unsigned long)p,
         (unsigned long long int)n, 
         0xFFFFFFFFFFFFFFFFULL };
   do {
      __asm__ __volatile__(
         "mov    r5, %0"             "\n\t"
         "ldr    r8,     [r5, #0]"   "\n\t" // p
         "ldrd   r2, r3, [r5, #8]"   "\n\t" // n
         "ldrexd r0, r1, [r8]"       "\n\t"
         "adds   r2, r2, r0"         "\n\t"
         "adc    r3, r3, r1"         "\n\t"
         "strexd r1, r2, r3, [r8]"   "\n\t"
         "str    r1, [r5, #16]"      "\n\t"
         : /*out*/
         : /*in*/ "r"(&block[0])
         : /*trash*/ "memory", "cc", "r5", "r0", "r1", "r8", "r2", "r3"
      );
   } while (block[2] != 0xFFFFFFFF00000000ULL);
#elif defined(VGA_arm64)
   unsigned long long int block[3]
      = { (unsigned long long int)p, (unsigned long long int)n,
          0xFFFFFFFFFFFFFFFFULL};
   do {
      __asm__ __volatile__(
         "mov   x5, %0"         "\n\t"
         "ldr   x9, [x5, #0]"   "\n\t" // p
         "ldr   x10, [x5, #8]"  "\n\t" // n
         "ldxr  x8, [x9]"       "\n\t"
         "add   x8, x8, x10"    "\n\t"
         "stxr  w4, x8, [x9]"   "\n\t"
         "str   x4, [x5, #16]"   "\n\t"
         : /*out*/
         : /*in*/ "r"(&block[0])
         : /*trash*/ "memory", "cc", "x5", "x8", "x9", "x10", "x4"
      );
   } while (block[2] != 0);
#elif defined(VGA_s390x)
   __asm__ __volatile__(
      "   lg	0,%0\n\t"
      "0: lgr	1,0\n\t"
      "   agr	1,%1\n\t"
      "   csg	0,1,%0\n\t"
      "   jl    0b\n\t"
      : "+m" (*p)
      : "d" (n)
      : "cc", "memory", "0", "1");
#elif defined(VGA_mips64)
   RegWord block[3]
      = { (RegWord)(Addr)p, (RegWord)n, 0x0ULL };
   do {
      __asm__ __volatile__(
         "move  $t0, %0"        "\n\t"
         "ld    $t1, 0($t0)"    "\n\t" // p
         "ld    $t2, 8($t0)"    "\n\t" // n
         "lld   $t3, 0($t1)"    "\n\t"
         "daddu $t3, $t3, $t2"  "\n\t"
         "scd   $t3, 0($t1)"    "\n\t"
         "sd    $t3, 16($t0)"   "\n\t"
         : /*out*/
         : /*in*/ "r"(&block[0])
         : /*trash*/ "memory", "t0", "t1", "t2", "t3"
      );
   } while (block[2] != 1);
#else
# error "Unsupported arch"
#endif
}

__attribute__((noinline)) void atomic_add_128bit ( MyU128* p,
                                                   unsigned long long int n )
{
#if defined(VGA_x86) || defined(VGA_ppc32) || defined(VGA_mips32) \
    || defined (VGA_nanomips) || defined(VGA_mips64) \
    || defined(VGA_amd64) \
    || defined(VGA_ppc64be) || defined(VGA_ppc64le) \
    || defined(VGA_arm) \
    || defined(VGA_s390x)
   /* do nothing; is not supported */
#elif defined(VGA_arm64)
   unsigned long long int block[3]
      = { (unsigned long long int)p, (unsigned long long int)n,
          0xFFFFFFFFFFFFFFFFULL};
   do {
      __asm__ __volatile__(
         "mov   x5, %0"             "\n\t" // &block[0]
         "ldr   x9, [x5, #0]"       "\n\t" // p
         "ldr   x10, [x5, #8]"      "\n\t" // n
         "ldxp  x7, x8, [x9]"       "\n\t"
         "adds  x7, x7, x10"        "\n\t"
         "adc   x8, x8, xzr"        "\n\t"
         "stxp  w4, x7, x8, [x9]"   "\n\t"
         "str   x4, [x5, #16]"      "\n\t"
         : /*out*/
         : /*in*/ "r"(&block[0])
         : /*trash*/ "memory", "cc", "x5", "x7", "x8", "x9", "x10", "x4"
      );
   } while (block[2] != 0);
#else
# error "Unsupported arch"
#endif
}

int main ( int argc, char** argv )
{
   int    i, status;
   char*  page;
   char*  p8;
   short* p16;
   int*   p32;
   long long int* p64;
   MyU128*  p128;
   pid_t  child, p2;

   assert(sizeof(MyU128) == 16);
   assert(sysconf(_SC_PAGESIZE) >= 4096);

   printf("parent, pre-fork\n");

   page = mmap( 0, sysconf(_SC_PAGESIZE),
                   PROT_READ|PROT_WRITE,
                   MAP_ANONYMOUS|MAP_SHARED, -1, 0 );
   if (page == MAP_FAILED) {
      perror("mmap failed");
      exit(1);
   }

   p8  = (char*)(page+0);
   p16 = (short*)(page+256);
   p32 = (int*)(page+512);
   p64 = (long long int*)(page+768);
   p128 = (MyU128*)(page+1024);

   assert( IS_8_ALIGNED(p8) );
   assert( IS_8_ALIGNED(p16) );
   assert( IS_8_ALIGNED(p32) );
   assert( IS_8_ALIGNED(p64) );
   assert( IS_16_ALIGNED(p128) );

   memset(page, 0, 1024);

   *p8  = 0;
   *p16 = 0;
   *p32 = 0;
   *p64 = 0;
   p128->lo64 = p128->hi64 = 0;

   child = fork();
   if (child == -1) {
      perror("fork() failed\n");
      return 1;
   }

   if (child == 0) {
      /* --- CHILD --- */
      printf("child\n");
      for (i = 0; i < NNN; i++) {
         atomic_add_8bit(p8, 1);
         atomic_add_16bit(p16, 1);
         atomic_add_32bit(p32, 1);
         atomic_add_64bit(p64, 98765 ); /* ensure we hit the upper 32 bits */
         atomic_add_128bit(p128, 0x1000000013374771ULL); // ditto re upper 64
      }
      return 1;
      /* NOTREACHED */

   }

   /* --- PARENT --- */

   printf("parent\n");

   for (i = 0; i < NNN; i++) {
      atomic_add_8bit(p8, 1);
      atomic_add_16bit(p16, 1);
      atomic_add_32bit(p32, 1);
      atomic_add_64bit(p64, 98765 ); /* ensure we hit the upper 32 bits */
      atomic_add_128bit(p128, 0x1000000013374771ULL); // ditto re upper 64
   }

   p2 = waitpid(child, &status, 0);
   assert(p2 == child);

   /* assert that child finished normally */
   assert(WIFEXITED(status));

   printf("FINAL VALUES:  8 bit %d,  16 bit %d,  32 bit %d,  64 bit %lld\n",
          (int)(*(signed char*)p8), (int)(*p16), *p32, *p64 );
   printf("               128 bit 0x%016llx:0x%016llx\n",
          p128->hi64, p128->lo64);

   if (-74 == (int)(*(signed char*)p8) 
       && 32694 == (int)(*p16) 
       && 6913974 == *p32
       && (0LL == *p64 || 682858642110LL == *p64)
       && ((0 == p128->hi64 && 0 == p128->lo64)
           || (0x00000000000697fb == p128->hi64
               && 0x6007eb426316d956ULL == p128->lo64))
      ) {
      printf("PASS\n");
   } else {
      printf("FAIL -- see source code for expected values\n");
   }

   printf("parent exits\n");

   return 0;
}
