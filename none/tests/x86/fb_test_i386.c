/*
 *  x86 CPU test
 *
 *  Copyright (c) 2003 Fabrice Bellard
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, see <http://www.gnu.org/licenses/>.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <math.h>
#include <stdarg.h>
#include <assert.h>


//////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////

/*
 * This is an OpenSSL-compatible implementation of the RSA Data Security, Inc.
 * MD5 Message-Digest Algorithm (RFC 1321).
 *
 * Homepage:
 * http://openwall.info/wiki/people/solar/software/public-domain-source-code/md5
 *
 * Author:
 * Alexander Peslyak, better known as Solar Designer <solar at openwall.com>
 *
 * This software was written by Alexander Peslyak in 2001.  No copyright is
 * claimed, and the software is hereby placed in the public domain.
 * In case this attempt to disclaim copyright and place the software in the
 * public domain is deemed null and void, then the software is
 * Copyright (c) 2001 Alexander Peslyak and it is hereby released to the
 * general public under the following terms:
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted.
 *
 * There's ABSOLUTELY NO WARRANTY, express or implied.
 *
 * (This is a heavily cut-down "BSD license".)
 *
 * This differs from Colin Plumb's older public domain implementation in that
 * no exactly 32-bit integer data type is required (any 32-bit or wider
 * unsigned integer data type will do), there's no compile-time endianness
 * configuration, and the function prototypes match OpenSSL's.  No code from
 * Colin Plumb's implementation has been reused; this comment merely compares
 * the properties of the two independent implementations.
 *
 * The primary goals of this implementation are portability and ease of use.
 * It is meant to be fast, but not as fast as possible.  Some known
 * optimizations are not included to reduce source code size and avoid
 * compile-time configuration.
 */
 
#include <string.h>
 
// BEGIN #include "md5.h"
/* Any 32-bit or wider unsigned integer data type will do */
typedef unsigned int MD5_u32plus;
 
typedef struct {
	MD5_u32plus lo, hi;
	MD5_u32plus a, b, c, d;
	unsigned char buffer[64];
	MD5_u32plus block[16];
} MD5_CTX;
 
void MD5_Init(MD5_CTX *ctx);
void MD5_Update(MD5_CTX *ctx, const void *data, unsigned long size);
void MD5_Final(unsigned char *result, MD5_CTX *ctx);
// END  #include "md5.h"
 
/*
 * The basic MD5 functions.
 *
 * F and G are optimized compared to their RFC 1321 definitions for
 * architectures that lack an AND-NOT instruction, just like in Colin Plumb's
 * implementation.
 */
#define F(x, y, z)			((z) ^ ((x) & ((y) ^ (z))))
#define G(x, y, z)			((y) ^ ((z) & ((x) ^ (y))))
#define H(x, y, z)			(((x) ^ (y)) ^ (z))
#define H2(x, y, z)			((x) ^ ((y) ^ (z)))
#define I(x, y, z)			((y) ^ ((x) | ~(z)))
 
/*
 * The MD5 transformation for all four rounds.
 */
#define STEP(f, a, b, c, d, x, t, s) \
	(a) += f((b), (c), (d)) + (x) + (t); \
	(a) = (((a) << (s)) | (((a) & 0xffffffff) >> (32 - (s)))); \
	(a) += (b);
 
/*
 * SET reads 4 input bytes in little-endian byte order and stores them in a
 * properly aligned word in host byte order.
 *
 * The check for little-endian architectures that tolerate unaligned memory
 * accesses is just an optimization.  Nothing will break if it fails to detect
 * a suitable architecture.
 *
 * Unfortunately, this optimization may be a C strict aliasing rules violation
 * if the caller's data buffer has effective type that cannot be aliased by
 * MD5_u32plus.  In practice, this problem may occur if these MD5 routines are
 * inlined into a calling function, or with future and dangerously advanced
 * link-time optimizations.  For the time being, keeping these MD5 routines in
 * their own translation unit avoids the problem.
 */
#if defined(__i386__) || defined(__x86_64__) || defined(__vax__)
#define SET(n) \
	(*(MD5_u32plus *)&ptr[(n) * 4])
#define GET(n) \
	SET(n)
#else
#define SET(n) \
	(ctx->block[(n)] = \
	(MD5_u32plus)ptr[(n) * 4] | \
	((MD5_u32plus)ptr[(n) * 4 + 1] << 8) | \
	((MD5_u32plus)ptr[(n) * 4 + 2] << 16) | \
	((MD5_u32plus)ptr[(n) * 4 + 3] << 24))
#define GET(n) \
	(ctx->block[(n)])
#endif
 
/*
 * This processes one or more 64-byte data blocks, but does NOT update the bit
 * counters.  There are no alignment requirements.
 */
static const void *body(MD5_CTX *ctx, const void *data, unsigned long size)
{
	const unsigned char *ptr;
	MD5_u32plus a, b, c, d;
	MD5_u32plus saved_a, saved_b, saved_c, saved_d;
 
	ptr = (const unsigned char *)data;
 
	a = ctx->a;
	b = ctx->b;
	c = ctx->c;
	d = ctx->d;
 
	do {
		saved_a = a;
		saved_b = b;
		saved_c = c;
		saved_d = d;
 
/* Round 1 */
		STEP(F, a, b, c, d, SET(0), 0xd76aa478, 7)
		STEP(F, d, a, b, c, SET(1), 0xe8c7b756, 12)
		STEP(F, c, d, a, b, SET(2), 0x242070db, 17)
		STEP(F, b, c, d, a, SET(3), 0xc1bdceee, 22)
		STEP(F, a, b, c, d, SET(4), 0xf57c0faf, 7)
		STEP(F, d, a, b, c, SET(5), 0x4787c62a, 12)
		STEP(F, c, d, a, b, SET(6), 0xa8304613, 17)
		STEP(F, b, c, d, a, SET(7), 0xfd469501, 22)
		STEP(F, a, b, c, d, SET(8), 0x698098d8, 7)
		STEP(F, d, a, b, c, SET(9), 0x8b44f7af, 12)
		STEP(F, c, d, a, b, SET(10), 0xffff5bb1, 17)
		STEP(F, b, c, d, a, SET(11), 0x895cd7be, 22)
		STEP(F, a, b, c, d, SET(12), 0x6b901122, 7)
		STEP(F, d, a, b, c, SET(13), 0xfd987193, 12)
		STEP(F, c, d, a, b, SET(14), 0xa679438e, 17)
		STEP(F, b, c, d, a, SET(15), 0x49b40821, 22)
 
/* Round 2 */
		STEP(G, a, b, c, d, GET(1), 0xf61e2562, 5)
		STEP(G, d, a, b, c, GET(6), 0xc040b340, 9)
		STEP(G, c, d, a, b, GET(11), 0x265e5a51, 14)
		STEP(G, b, c, d, a, GET(0), 0xe9b6c7aa, 20)
		STEP(G, a, b, c, d, GET(5), 0xd62f105d, 5)
		STEP(G, d, a, b, c, GET(10), 0x02441453, 9)
		STEP(G, c, d, a, b, GET(15), 0xd8a1e681, 14)
		STEP(G, b, c, d, a, GET(4), 0xe7d3fbc8, 20)
		STEP(G, a, b, c, d, GET(9), 0x21e1cde6, 5)
		STEP(G, d, a, b, c, GET(14), 0xc33707d6, 9)
		STEP(G, c, d, a, b, GET(3), 0xf4d50d87, 14)
		STEP(G, b, c, d, a, GET(8), 0x455a14ed, 20)
		STEP(G, a, b, c, d, GET(13), 0xa9e3e905, 5)
		STEP(G, d, a, b, c, GET(2), 0xfcefa3f8, 9)
		STEP(G, c, d, a, b, GET(7), 0x676f02d9, 14)
		STEP(G, b, c, d, a, GET(12), 0x8d2a4c8a, 20)
 
/* Round 3 */
		STEP(H, a, b, c, d, GET(5), 0xfffa3942, 4)
		STEP(H2, d, a, b, c, GET(8), 0x8771f681, 11)
		STEP(H, c, d, a, b, GET(11), 0x6d9d6122, 16)
		STEP(H2, b, c, d, a, GET(14), 0xfde5380c, 23)
		STEP(H, a, b, c, d, GET(1), 0xa4beea44, 4)
		STEP(H2, d, a, b, c, GET(4), 0x4bdecfa9, 11)
		STEP(H, c, d, a, b, GET(7), 0xf6bb4b60, 16)
		STEP(H2, b, c, d, a, GET(10), 0xbebfbc70, 23)
		STEP(H, a, b, c, d, GET(13), 0x289b7ec6, 4)
		STEP(H2, d, a, b, c, GET(0), 0xeaa127fa, 11)
		STEP(H, c, d, a, b, GET(3), 0xd4ef3085, 16)
		STEP(H2, b, c, d, a, GET(6), 0x04881d05, 23)
		STEP(H, a, b, c, d, GET(9), 0xd9d4d039, 4)
		STEP(H2, d, a, b, c, GET(12), 0xe6db99e5, 11)
		STEP(H, c, d, a, b, GET(15), 0x1fa27cf8, 16)
		STEP(H2, b, c, d, a, GET(2), 0xc4ac5665, 23)
 
/* Round 4 */
		STEP(I, a, b, c, d, GET(0), 0xf4292244, 6)
		STEP(I, d, a, b, c, GET(7), 0x432aff97, 10)
		STEP(I, c, d, a, b, GET(14), 0xab9423a7, 15)
		STEP(I, b, c, d, a, GET(5), 0xfc93a039, 21)
		STEP(I, a, b, c, d, GET(12), 0x655b59c3, 6)
		STEP(I, d, a, b, c, GET(3), 0x8f0ccc92, 10)
		STEP(I, c, d, a, b, GET(10), 0xffeff47d, 15)
		STEP(I, b, c, d, a, GET(1), 0x85845dd1, 21)
		STEP(I, a, b, c, d, GET(8), 0x6fa87e4f, 6)
		STEP(I, d, a, b, c, GET(15), 0xfe2ce6e0, 10)
		STEP(I, c, d, a, b, GET(6), 0xa3014314, 15)
		STEP(I, b, c, d, a, GET(13), 0x4e0811a1, 21)
		STEP(I, a, b, c, d, GET(4), 0xf7537e82, 6)
		STEP(I, d, a, b, c, GET(11), 0xbd3af235, 10)
		STEP(I, c, d, a, b, GET(2), 0x2ad7d2bb, 15)
		STEP(I, b, c, d, a, GET(9), 0xeb86d391, 21)
 
		a += saved_a;
		b += saved_b;
		c += saved_c;
		d += saved_d;
 
		ptr += 64;
	} while (size -= 64);
 
	ctx->a = a;
	ctx->b = b;
	ctx->c = c;
	ctx->d = d;
 
	return ptr;
}
 
void MD5_Init(MD5_CTX *ctx)
{
	ctx->a = 0x67452301;
	ctx->b = 0xefcdab89;
	ctx->c = 0x98badcfe;
	ctx->d = 0x10325476;
 
	ctx->lo = 0;
	ctx->hi = 0;
}
 
void MD5_Update(MD5_CTX *ctx, const void *data, unsigned long size)
{
	MD5_u32plus saved_lo;
	unsigned long used, available;
 
	saved_lo = ctx->lo;
	if ((ctx->lo = (saved_lo + size) & 0x1fffffff) < saved_lo)
		ctx->hi++;
	ctx->hi += size >> 29;
 
	used = saved_lo & 0x3f;
 
	if (used) {
		available = 64 - used;
 
		if (size < available) {
			memcpy(&ctx->buffer[used], data, size);
			return;
		}
 
		memcpy(&ctx->buffer[used], data, available);
		data = (const unsigned char *)data + available;
		size -= available;
		body(ctx, ctx->buffer, 64);
	}
 
	if (size >= 64) {
		data = body(ctx, data, size & ~(unsigned long)0x3f);
		size &= 0x3f;
	}
 
	memcpy(ctx->buffer, data, size);
}
 
#define OUT(dst, src) \
	(dst)[0] = (unsigned char)(src); \
	(dst)[1] = (unsigned char)((src) >> 8); \
	(dst)[2] = (unsigned char)((src) >> 16); \
	(dst)[3] = (unsigned char)((src) >> 24);
 
void MD5_Final(unsigned char *result, MD5_CTX *ctx)
{
	unsigned long used, available;
 
	used = ctx->lo & 0x3f;
 
	ctx->buffer[used++] = 0x80;
 
	available = 64 - used;
 
	if (available < 8) {
		memset(&ctx->buffer[used], 0, available);
		body(ctx, ctx->buffer, 64);
		used = 0;
		available = 64;
	}
 
	memset(&ctx->buffer[used], 0, available - 8);
 
	ctx->lo <<= 3;
	OUT(&ctx->buffer[56], ctx->lo)
	OUT(&ctx->buffer[60], ctx->hi)
 
	body(ctx, ctx->buffer, 64);
 
	OUT(&result[0], ctx->a)
	OUT(&result[4], ctx->b)
	OUT(&result[8], ctx->c)
	OUT(&result[12], ctx->d)
 
	memset(ctx, 0, sizeof(*ctx));
}
 

//////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////

static MD5_CTX md5ctx;

void xxprintf_start(void)
{
   MD5_Init(&md5ctx);
}

void xxprintf_done(void)
{
   const char hexchar[16] = "0123456789abcdef";
   unsigned char result[100];
   memset(result, 0, sizeof(result));
   MD5_Final(&result[0], &md5ctx);
   printf("final MD5 = ");
   int i;
   for (i = 0; i < 16; i++) {
      printf("%c%c", hexchar[0xF & (result[i] >> 4)],
                     hexchar[0xF & (result[i] >> 0)]);
   }
   printf("\n");
}

__attribute__((format(__printf__, 1, 2)))
void xxprintf (const char *format, ...)
{
   char buf[128];
   memset(buf, 0, sizeof(buf));

   va_list vargs;
   va_start(vargs, format);
   int n = vsnprintf(buf, sizeof(buf)-1, format, vargs);
   va_end(vargs);

   assert(n < sizeof(buf)-1);
   assert(buf[sizeof(buf)-1] == 0);
   assert(buf[sizeof(buf)-2] == 0);

   MD5_Update(&md5ctx, buf, strlen(buf));
   if (0) printf("QQQ %s", buf);
}

//////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////


/* Setting this to 1 creates a very comprehensive test of
   integer condition codes. */
#define TEST_INTEGER_VERBOSE 1


//#define LINUX_VM86_IOPL_FIX
//#define TEST_P4_FLAGS

#define xglue(x, y) x ## y
#define glue(x, y) xglue(x, y)
#define stringify(s)	tostring(s)
#define tostring(s)	#s

#define CC_C   	0x0001
#define CC_P 	0x0004
#define CC_A	0x0010
#define CC_Z	0x0040
#define CC_S    0x0080
#define CC_O    0x0800

#define CC_MASK (CC_C | CC_P | CC_Z | CC_S | CC_O | CC_A)

#define OP add
#include "fb_test_i386.h"

#define OP sub
#include "fb_test_i386.h"

#define OP xor
#include "fb_test_i386.h"

#define OP and
#include "fb_test_i386.h"

#define OP or
#include "fb_test_i386.h"

#define OP cmp
#include "fb_test_i386.h"

#define OP adc
#define OP_CC
#include "fb_test_i386.h"

#define OP sbb
#define OP_CC
#include "fb_test_i386.h"

#define OP inc
#define OP_CC
#define OP1
#include "fb_test_i386.h"

#define OP dec
#define OP_CC
#define OP1
#include "fb_test_i386.h"

#define OP neg
#define OP_CC
#define OP1
#include "fb_test_i386.h"

#define OP not
#define OP_CC
#define OP1
#include "fb_test_i386.h"

#undef CC_MASK
#define CC_MASK (CC_C | CC_P | CC_Z | CC_S | CC_O)

#define OP shl
#include "fb_test_i386_shift.h"

#define OP shr
#include "fb_test_i386_shift.h"

#define OP sar
#include "fb_test_i386_shift.h"

#define OP rol
#include "fb_test_i386_shift.h"

#define OP ror
#include "fb_test_i386_shift.h"

#define OP rcr
#define OP_CC
#include "fb_test_i386_shift.h"

#define OP rcl
#define OP_CC
#include "fb_test_i386_shift.h"

#define OP shld
#define OP_SHIFTD
#define OP_NOBYTE
#include "fb_test_i386_shift.h"

#define OP shrd
#define OP_SHIFTD
#define OP_NOBYTE
#include "fb_test_i386_shift.h"


/* XXX: should be more precise ? */
#undef CC_MASK
#define CC_MASK (CC_C)

#define OP bt
#define OP_NOBYTE
#include "fb_test_i386_shift.h"

#define OP bts
#define OP_NOBYTE
#include "fb_test_i386_shift.h"

#define OP btr
#define OP_NOBYTE
#include "fb_test_i386_shift.h"

#define OP btc
#define OP_NOBYTE
#include "fb_test_i386_shift.h"


/* lea test (modrm support) */
#define TEST_LEA(STR)\
{\
    asm("leal " STR ", %0"\
        : "=r" (res)\
        : "a" (eax), "b" (ebx), "c" (ecx), "d" (edx), "S" (esi), "D" (edi));\
    xxprintf("lea %s = %08x\n", STR, res);\
}

void test_lea(void)
{
    int eax, ebx, ecx, edx, esi, edi, res;
    eax = 0x0001;
    ebx = 0x0002;
    ecx = 0x0004;
    edx = 0x0008;
    esi = 0x0010;
    edi = 0x0020;

    TEST_LEA("0x4000");

    TEST_LEA("(%%eax)");
    TEST_LEA("(%%ebx)");
    TEST_LEA("(%%ecx)");
    TEST_LEA("(%%edx)");
    TEST_LEA("(%%esi)");
    TEST_LEA("(%%edi)");

    TEST_LEA("0x40(%%eax)");
    TEST_LEA("0x40(%%ebx)");
    TEST_LEA("0x40(%%ecx)");
    TEST_LEA("0x40(%%edx)");
    TEST_LEA("0x40(%%esi)");
    TEST_LEA("0x40(%%edi)");

    TEST_LEA("0x4000(%%eax)");
    TEST_LEA("0x4000(%%ebx)");
    TEST_LEA("0x4000(%%ecx)");
    TEST_LEA("0x4000(%%edx)");
    TEST_LEA("0x4000(%%esi)");
    TEST_LEA("0x4000(%%edi)");

    TEST_LEA("(%%eax, %%ecx)");
    TEST_LEA("(%%ebx, %%edx)");
    TEST_LEA("(%%ecx, %%ecx)");
    TEST_LEA("(%%edx, %%ecx)");
    TEST_LEA("(%%esi, %%ecx)");
    TEST_LEA("(%%edi, %%ecx)");

    TEST_LEA("0x40(%%eax, %%ecx)");
    TEST_LEA("0x4000(%%ebx, %%edx)");

    TEST_LEA("(%%ecx, %%ecx, 2)");
    TEST_LEA("(%%edx, %%ecx, 4)");
    TEST_LEA("(%%esi, %%ecx, 8)");

    TEST_LEA("(,%%eax, 2)");
    TEST_LEA("(,%%ebx, 4)");
    TEST_LEA("(,%%ecx, 8)");

    TEST_LEA("0x40(,%%eax, 2)");
    TEST_LEA("0x40(,%%ebx, 4)");
    TEST_LEA("0x40(,%%ecx, 8)");


    TEST_LEA("-10(%%ecx, %%ecx, 2)");
    TEST_LEA("-10(%%edx, %%ecx, 4)");
    TEST_LEA("-10(%%esi, %%ecx, 8)");

    TEST_LEA("0x4000(%%ecx, %%ecx, 2)");
    TEST_LEA("0x4000(%%edx, %%ecx, 4)");
    TEST_LEA("0x4000(%%esi, %%ecx, 8)");
}

#define TEST_JCC(JCC, v1, v2)\
{   int one = 1; \
    int res;\
    asm("movl $1, %0\n\t"\
        "cmpl %2, %1\n\t"\
        "j" JCC " 1f\n\t"\
        "movl $0, %0\n\t"\
        "1:\n\t"\
        : "=r" (res)\
        : "r" (v1), "r" (v2));\
    xxprintf("%-10s %d\n", "j" JCC, res);\
\
    asm("movl $0, %0\n\t"\
        "cmpl %2, %1\n\t"\
        "set" JCC " %b0\n\t"\
        : "=r" (res)\
        : "r" (v1), "r" (v2));\
    xxprintf("%-10s %d\n", "set" JCC, res);\
 {\
    asm("movl $0x12345678, %0\n\t"\
        "cmpl %2, %1\n\t"\
        "cmov" JCC "l %3, %0\n\t"\
        : "=r" (res)\
        : "r" (v1), "r" (v2), "m" (one));\
        xxprintf("%-10s R=0x%08x\n", "cmov" JCC "l", res);\
    asm("movl $0x12345678, %0\n\t"\
        "cmpl %2, %1\n\t"\
        "cmov" JCC "w %w3, %w0\n\t"\
        : "=r" (res)\
        : "r" (v1), "r" (v2), "r" (one));\
        xxprintf("%-10s R=0x%08x\n", "cmov" JCC "w", res);\
 } \
}

/* various jump tests */
void test_jcc(void)
{
    TEST_JCC("ne", 1, 1);
    TEST_JCC("ne", 1, 0);

    TEST_JCC("e", 1, 1);
    TEST_JCC("e", 1, 0);

    TEST_JCC("l", 1, 1);
    TEST_JCC("l", 1, 0);
    TEST_JCC("l", 1, -1);

    TEST_JCC("le", 1, 1);
    TEST_JCC("le", 1, 0);
    TEST_JCC("le", 1, -1);

    TEST_JCC("ge", 1, 1);
    TEST_JCC("ge", 1, 0);
    TEST_JCC("ge", -1, 1);

    TEST_JCC("g", 1, 1);
    TEST_JCC("g", 1, 0);
    TEST_JCC("g", 1, -1);

    TEST_JCC("b", 1, 1);
    TEST_JCC("b", 1, 0);
    TEST_JCC("b", 1, -1);

    TEST_JCC("be", 1, 1);
    TEST_JCC("be", 1, 0);
    TEST_JCC("be", 1, -1);

    TEST_JCC("ae", 1, 1);
    TEST_JCC("ae", 1, 0);
    TEST_JCC("ae", 1, -1);

    TEST_JCC("a", 1, 1);
    TEST_JCC("a", 1, 0);
    TEST_JCC("a", 1, -1);


    TEST_JCC("p", 1, 1);
    TEST_JCC("p", 1, 0);

    TEST_JCC("np", 1, 1);
    TEST_JCC("np", 1, 0);

    TEST_JCC("o", 0x7fffffff, 0);
    TEST_JCC("o", 0x7fffffff, -1);

    TEST_JCC("no", 0x7fffffff, 0);
    TEST_JCC("no", 0x7fffffff, -1);

    TEST_JCC("s", 0, 1);
    TEST_JCC("s", 0, -1);
    TEST_JCC("s", 0, 0);

    TEST_JCC("ns", 0, 1);
    TEST_JCC("ns", 0, -1);
    TEST_JCC("ns", 0, 0);
}

#undef CC_MASK
#ifdef TEST_P4_FLAGS
#define CC_MASK (CC_C | CC_P | CC_Z | CC_S | CC_O | CC_A)
#else
#define CC_MASK (CC_O | CC_C)
#endif

 #define OP mul
#include "fb_test_i386_muldiv.h"

 #define OP imul
#include "fb_test_i386_muldiv.h"

void test_imulw2(int op0, int op1) 
{
    int res, s1, s0, flags;
    s0 = op0;
    s1 = op1;
    res = s0;
    flags = 0;
    asm ("push %4\n\t"
         "popf\n\t"
         "imulw %w2, %w0\n\t" 
         "pushf\n\t"
         "popl %1\n\t"
         : "=q" (res), "=g" (flags)
         : "q" (s1), "0" (res), "1" (flags));
    xxprintf("%-10s A=%08x B=%08x R=%08x CC=%04x\n",
           "imulw", s0, s1, res, flags & CC_MASK);
}

void test_imull2(int op0, int op1) 
{
    int res, s1, s0, flags;
    s0 = op0;
    s1 = op1;
    res = s0;
    flags = 0;
    asm ("push %4\n\t"
         "popf\n\t"
         "imull %2, %0\n\t" 
         "pushf\n\t"
         "popl %1\n\t"
         : "=q" (res), "=g" (flags)
         : "q" (s1), "0" (res), "1" (flags));
    xxprintf("%-10s A=%08x B=%08x R=%08x CC=%04x\n",
           "imull", s0, s1, res, flags & CC_MASK);
}

#define TEST_IMUL_IM(size, size1, op0, op1)\
{\
    int res, flags;\
    flags = 0;\
    res = 0;\
    asm ("push %3\n\t"\
         "popf\n\t"\
         "imul" size " $" #op0 ", %" size1 "2, %" size1 "0\n\t" \
         "pushf\n\t"\
         "popl %1\n\t"\
         : "=r" (res), "=g" (flags)\
         : "r" (op1), "1" (flags), "0" (res));\
    xxprintf("%-10s A=%08x B=%08x R=%08x CC=%04x\n",\
           "imul" size, op0, op1, res, flags & CC_MASK);\
}


#undef CC_MASK
#define CC_MASK (0)

#define OP div
#include "fb_test_i386_muldiv.h"

#define OP idiv
#include "fb_test_i386_muldiv.h"

void test_mul(void)
{
    test_imulb(0x1234561d, 4);
    test_imulb(3, -4);
    test_imulb(0x80, 0x80);
    test_imulb(0x10, 0x10);

    test_imulw(0, 0, 0);
    test_imulw(0, 0xFF, 0xFF);
    test_imulw(0, 0xFF, 0x100);
    test_imulw(0, 0x1234001d, 45);
    test_imulw(0, 23, -45);
    test_imulw(0, 0x8000, 0x8000);
    test_imulw(0, 0x100, 0x100);

    test_imull(0, 0, 0);
    test_imull(0, 0xFFFF, 0xFFFF);
    test_imull(0, 0xFFFF, 0x10000);
    test_imull(0, 0x1234001d, 45);
    test_imull(0, 23, -45);
    test_imull(0, 0x80000000, 0x80000000);
    test_imull(0, 0x10000, 0x10000);

    test_mulb(0x1234561d, 4);
    test_mulb(3, -4);
    test_mulb(0x80, 0x80);
    test_mulb(0x10, 0x10);

    test_mulw(0, 0x1234001d, 45);
    test_mulw(0, 23, -45);
    test_mulw(0, 0x8000, 0x8000);
    test_mulw(0, 0x100, 0x100);

    test_mull(0, 0x1234001d, 45);
    test_mull(0, 23, -45);
    test_mull(0, 0x80000000, 0x80000000);
    test_mull(0, 0x10000, 0x10000);

    test_imulw2(0x1234001d, 45);
    test_imulw2(23, -45);
    test_imulw2(0x8000, 0x8000);
    test_imulw2(0x100, 0x100);

    test_imull2(0x1234001d, 45);
    test_imull2(23, -45);
    test_imull2(0x80000000, 0x80000000);
    test_imull2(0x10000, 0x10000);

    TEST_IMUL_IM("w", "w", 45, 0x1234);
    TEST_IMUL_IM("w", "w", -45, 23);
    TEST_IMUL_IM("w", "w", 0x8000, 0x80000000);
    TEST_IMUL_IM("w", "w", 0x7fff, 0x1000);

    TEST_IMUL_IM("l", "", 45, 0x1234);
    TEST_IMUL_IM("l", "", -45, 23);
    TEST_IMUL_IM("l", "", 0x8000, 0x80000000);
    TEST_IMUL_IM("l", "", 0x7fff, 0x1000);

    test_idivb(0x12341678, 0x127e);
    test_idivb(0x43210123, -5);
    test_idivb(0x12340004, -1);

    test_idivw(0, 0x12345678, 12347);
    test_idivw(0, -23223, -45);
    test_idivw(0, 0x12348000, -1);
    test_idivw(0x12343, 0x12345678, 0x81238567);

    test_idivl(0, 0x12345678, 12347);
    test_idivl(0, -233223, -45);
    test_idivl(0, 0x80000000, -1);
    test_idivl(0x12343, 0x12345678, 0x81234567);

    test_divb(0x12341678, 0x127e);
    test_divb(0x43210123, -5);
    test_divb(0x12340004, -1);

    test_divw(0, 0x12345678, 12347);
    test_divw(0, -23223, -45);
    test_divw(0, 0x12348000, -1);
    test_divw(0x12343, 0x12345678, 0x81238567);

    test_divl(0, 0x12345678, 12347);
    test_divl(0, -233223, -45);
    test_divl(0, 0x80000000, -1);
    test_divl(0x12343, 0x12345678, 0x81234567);
}

#define TEST_BSX(op, size, op0)\
{\
    int res, val, resz;\
    val = op0;\
    asm("xorl %1, %1\n\t"\
        "movl $0x12345678, %0\n\t"\
        #op " %" size "2, %" size "0\n\t" \
        "setz %b1" \
        : "=r" (res), "=q" (resz)\
        : "r" (val));\
    xxprintf("%-10s A=%08x R=%08x %d\n", #op, val, res, resz);\
}

void test_bsx(void)
{
    TEST_BSX(bsrw, "w", 0);
    TEST_BSX(bsrw, "w", 0x12340128);
    TEST_BSX(bsrl, "", 0);
    TEST_BSX(bsrl, "", 0x00340128);
    TEST_BSX(bsfw, "w", 0);
    TEST_BSX(bsfw, "w", 0x12340128);
    TEST_BSX(bsfl, "", 0);
    TEST_BSX(bsfl, "", 0x00340128);
}

/**********************************************/

void test_fops(double a, double b)
{
    xxprintf("a=%f b=%f a+b=%f\n", a, b, a + b);
    xxprintf("a=%f b=%f a-b=%f\n", a, b, a - b);
    xxprintf("a=%f b=%f a*b=%f\n", a, b, a * b);
    xxprintf("a=%f b=%f a/b=%f\n", a, b, a / b);
    xxprintf("a=%f b=%f fmod(a, b)=%f\n", a, b, fmod(a, b));
    xxprintf("a=%f sqrt(a)=%f\n", a, sqrt(a));
    xxprintf("a=%f sin(a)=%f\n", a, sin(a));
    xxprintf("a=%f cos(a)=%f\n", a, cos(a));
    xxprintf("a=%f tan(a)=%f\n", a, tan(a));
    xxprintf("a=%f log(a)=%f\n", a, log(a));
    xxprintf("a=%f exp(a)=%f\n", a, exp(a));
    xxprintf("a=%f b=%f atan2(a, b)=%f\n", a, b, atan2(a, b));
    /* just to test some op combining */
    xxprintf("a=%f asin(sin(a))=%f\n", a, asin(sin(a)));
    xxprintf("a=%f acos(cos(a))=%f\n", a, acos(cos(a)));
    xxprintf("a=%f atan(tan(a))=%f\n", a, atan(tan(a)));
}

void test_fcmp(double a, double b)
{
    xxprintf("(%f<%f)=%d\n",
           a, b, a < b);
    xxprintf("(%f<=%f)=%d\n",
           a, b, a <= b);
    xxprintf("(%f==%f)=%d\n",
           a, b, a == b);
    xxprintf("(%f>%f)=%d\n",
           a, b, a > b);
    xxprintf("(%f<=%f)=%d\n",
           a, b, a >= b);
    {
        unsigned int eflags;
        /* test f(u)comi instruction */
        asm("fcomi %2, %1\n"
            "pushf\n"
            "popl %0\n"                  // FLORI was pop
            : "=r" (eflags)
            : "t" (a), "u" (b));
        xxprintf("fcomi(%f %f)=%08x\n", a, b, eflags & (CC_Z | CC_P | CC_C));
    }
}

void test_fcvt(double a)
{
    float fa;
    long double la;
    int16_t fpuc;
    int i;
    int64_t lla;
    int ia;
    int16_t wa;
    double ra;

    fa = a;
    la = a;
    xxprintf("(float)%f = %f\n", a, fa);
    xxprintf("(long double)%f = %Lf\n", a, la);
    xxprintf("a=%016llx\n", *(unsigned long long int *) &a);
    xxprintf("la=%016llx %04x\n", *(unsigned long long int *) &la,
             *(unsigned short *) ((char *)(&la) + 8));

    /* test all roundings */
    asm volatile ("fstcw %0" : "=m" (fpuc));
    for(i=0;i<4;i++) {
        int16_t tmp = (fpuc & ~0x0c00) | (i << 10);
        asm volatile ("fldcw %0" : : "m" (tmp));
        asm volatile ("fists %0" : "=m" (wa) : "t" (a));
        asm volatile ("fistl %0" : "=m" (ia) : "t" (a));
        asm volatile ("fistpll %0" : "=m" (lla) : "t" (a) : "st");
        asm volatile ("frndint ; fstl %0" : "=m" (ra) : "t" (a));
        asm volatile ("fldcw %0" : : "m" (fpuc));
        xxprintf("(short)a = %d\n", wa);
        xxprintf("(int)a = %d\n", ia);
        xxprintf("(int64_t)a = %lld\n", lla);
        xxprintf("rint(a) = %f\n", ra);
    }
}

#define TEST(N) \
    asm("fld" #N : "=t" (a)); \
    xxprintf("fld" #N "= %f\n", a);

void test_fconst(void)
{
    double a;
    TEST(1);
    TEST(l2t);
    TEST(l2e);
    TEST(pi);
    TEST(lg2);
    TEST(ln2);
    TEST(z);
}

void test_fbcd(double a)
{
    unsigned short bcd[5];
    double b;

    asm("fbstp %0" : "=m" (bcd[0]) : "t" (a) : "st");
    asm("fbld %1" : "=t" (b) : "m" (bcd[0]));
    xxprintf("a=%f bcd=%04x%04x%04x%04x%04x b=%f\n", 
           a, bcd[4], bcd[3], bcd[2], bcd[1], bcd[0], b);
}

#define TEST_ENV(env, save, restore)\
{\
    memset((env), 0xaa, sizeof(*(env)));\
    for(i=0;i<5;i++)\
        asm volatile ("fldl %0" : : "m" (dtab[i]));\
    asm(save " %0\n" : : "m" (*(env)));\
    asm(restore " %0\n": : "m" (*(env)));\
    for(i=0;i<5;i++)\
        asm volatile ("fstpl %0" : "=m" (rtab[i]));\
    for(i=0;i<5;i++)\
        xxprintf("res[%d]=%f\n", i, rtab[i]);\
    xxprintf("fpuc=%04x fpus=%04x fptag=%04x\n",\
           (env)->fpuc,\
           (env)->fpus & 0xff00,\
           (env)->fptag);\
}

/* Commented out because results differ depending on gcc/clang version
   when run under valgrind --tool=none */
#if 0
void test_fenv(void)
{
    struct __attribute__((packed)) {
        uint16_t fpuc;
        uint16_t dummy1;
        uint16_t fpus;
        uint16_t dummy2;
        uint16_t fptag;
        uint16_t dummy3;
        uint32_t ignored[4];
        long double fpregs[8];
    } float_env32;
    struct __attribute__((packed)) {
        uint16_t fpuc;
        uint16_t fpus;
        uint16_t fptag;
        uint16_t ignored[4];
        long double fpregs[8];
    } float_env16;
    double dtab[8];
    double rtab[8];
    int i;

    for(i=0;i<8;i++)
        dtab[i] = i + 1;

    TEST_ENV(&float_env16, "data16 fnstenv", "data16 fldenv");
    TEST_ENV(&float_env16, "data16 fnsave", "data16 frstor");
    TEST_ENV(&float_env32, "fnstenv", "fldenv");
    TEST_ENV(&float_env32, "fnsave", "frstor");

    /* test for ffree */
    for(i=0;i<5;i++)
        asm volatile ("fldl %0" : : "m" (dtab[i]));
    asm volatile("ffree %st(2)");
    asm volatile ("fnstenv %0\n" : : "m" (float_env32));
    asm volatile ("fninit");
    xxprintf("fptag=%04x\n", float_env32.fptag);
}
#endif


#define TEST_FCMOV(a, b, eflags, CC)\
{\
    double res;\
    asm("push %3\n"\
        "popf\n"\
        "fcmov" CC " %2, %0\n"\
        : "=t" (res)\
        : "0" (a), "u" (b), "g" (eflags));\
    xxprintf("fcmov%s eflags=0x%04x-> %f\n", \
           CC, eflags, res);\
}

void test_fcmov(void)
{
    double a, b;
    int eflags, i;

    a = 1.0;
    b = 2.0;
    for(i = 0; i < 4; i++) {
        eflags = 0;
        if (i & 1)
            eflags |= CC_C;
        if (i & 2)
            eflags |= CC_Z;
        TEST_FCMOV(a, b, eflags, "b");
        TEST_FCMOV(a, b, eflags, "e");
        TEST_FCMOV(a, b, eflags, "be");
        TEST_FCMOV(a, b, eflags, "nb");
        TEST_FCMOV(a, b, eflags, "ne");
        TEST_FCMOV(a, b, eflags, "nbe");
    }
    TEST_FCMOV(a, b, 0, "u");
    TEST_FCMOV(a, b, CC_P, "u");
    TEST_FCMOV(a, b, 0, "nu");
    TEST_FCMOV(a, b, CC_P, "nu");
}

void test_floats(void)
{
    test_fops(2, 3);
    test_fops(1.4, -5);
    test_fcmp(2, -1);
    test_fcmp(2, 2);
    test_fcmp(2, 3);
    test_fcvt(0.5);
    test_fcvt(-0.5);
    test_fcvt(1.0/7.0);
    test_fcvt(-1.0/9.0);
    test_fcvt(32768);
    test_fcvt(-1e20);
    test_fconst();
    // REINSTATE (maybe): test_fbcd(1234567890123456);
    // REINSTATE (maybe): test_fbcd(-123451234567890);
    // REINSTATE: test_fenv();
    // REINSTATE: test_fcmov();
}

/**********************************************/

#define TEST_XCHG(op, size, opconst)\
{\
    int op0, op1;\
    op0 = 0x12345678;\
    op1 = 0xfbca7654;\
    asm(#op " %" size "0, %" size "1" \
        : "=q" (op0), opconst (op1) \
        : "0" (op0), "1" (op1));\
    xxprintf("%-10s A=%08x B=%08x\n",\
           #op, op0, op1);\
}

#define TEST_CMPXCHG(op, size, opconst, eax)\
{\
    int op0, op1;\
    op0 = 0x12345678;\
    op1 = 0xfbca7654;\
    asm(#op " %" size "0, %" size "1" \
        : "=q" (op0), opconst (op1) \
        : "0" (op0), "1" (op1), "a" (eax));\
    xxprintf("%-10s EAX=%08x A=%08x C=%08x\n",\
           #op, eax, op0, op1);\
}
#if 0
// commented out due to gcc warning: matching constraint does not allow a register
void test_xchg(void)
{
    TEST_XCHG(xchgl, "", "=q");
    TEST_XCHG(xchgw, "w", "=q");
    TEST_XCHG(xchgb, "b", "=q");

    TEST_XCHG(xchgl, "", "=m");
    TEST_XCHG(xchgw, "w", "=m");
    TEST_XCHG(xchgb, "b", "=m");

    TEST_CMPXCHG(cmpxchgl, "", "=q", 0xfbca7654);
    TEST_CMPXCHG(cmpxchgw, "w", "=q", 0xfbca7654);
    TEST_CMPXCHG(cmpxchgb, "b", "=q", 0xfbca7654);

    TEST_CMPXCHG(cmpxchgl, "", "=q", 0xfffefdfc);
    TEST_CMPXCHG(cmpxchgw, "w", "=q", 0xfffefdfc);
    TEST_CMPXCHG(cmpxchgb, "b", "=q", 0xfffefdfc);

    TEST_CMPXCHG(cmpxchgl, "", "=m", 0xfbca7654);
    TEST_CMPXCHG(cmpxchgw, "w", "=m", 0xfbca7654);
    TEST_CMPXCHG(cmpxchgb, "b", "=m", 0xfbca7654);

    TEST_CMPXCHG(cmpxchgl, "", "=m", 0xfffefdfc);
    TEST_CMPXCHG(cmpxchgw, "w", "=m", 0xfffefdfc);
    TEST_CMPXCHG(cmpxchgb, "b", "=m", 0xfffefdfc);
}
#endif
/**********************************************/

uint8_t str_buffer[4096];

#define TEST_STRING1(OP, size, DF, REP)\
{\
    int esi, edi, eax, ecx, eflags;\
\
    esi = (long)(str_buffer + sizeof(str_buffer) / 2);\
    edi = (long)(str_buffer + sizeof(str_buffer) / 2) + 16;\
    eax = 0x12345678;\
    ecx = 17;\
\
    asm volatile ("pushl $0\n\t"\
                  "popf\n\t"\
                  DF "\n\t"\
                  REP #OP size "\n\t"\
                  "cld\n\t"\
                  "pushf\n\t"\
                  "popl %4\n\t"\
                  : "=S" (esi), "=D" (edi), "=a" (eax), "=c" (ecx), "=g" (eflags)\
                  : "0" (esi), "1" (edi), "2" (eax), "3" (ecx));\
    xxprintf("%-10s ESI=%08x EDI=%08x EAX=%08x ECX=%08x EFL=%04x\n",\
           REP #OP size, esi, edi, eax, ecx,\
           eflags & (CC_C | CC_P | CC_Z | CC_S | CC_O | CC_A));\
}

#define TEST_STRING(OP, REP)\
    TEST_STRING1(OP, "b", "", REP);\
    TEST_STRING1(OP, "w", "", REP);\
    TEST_STRING1(OP, "l", "", REP);\
    TEST_STRING1(OP, "b", "std", REP);\
    TEST_STRING1(OP, "w", "std", REP);\
    TEST_STRING1(OP, "l", "std", REP)

void test_string(void)
{
    int i;
    for(i = 0;i < sizeof(str_buffer); i++)
        str_buffer[i] = i + 0x56;
   TEST_STRING(stos, "");
   TEST_STRING(stos, "rep ");
   // REINSTATE: TEST_STRING(lods, ""); /* to verify stos */
   // REINSTATE: TEST_STRING(lods, "rep "); 
   TEST_STRING(movs, "");
   TEST_STRING(movs, "rep ");
   // REINSTATE: TEST_STRING(lods, ""); /* to verify stos */

   /* XXX: better tests */
   TEST_STRING(scas, "");
   // REINSTATE: TEST_STRING(scas, "repz ");
   TEST_STRING(scas, "repnz ");
   // REINSTATE: TEST_STRING(cmps, "");
   TEST_STRING(cmps, "repz ");
   // REINSTATE: TEST_STRING(cmps, "repnz ");
}


int main(int argc, char **argv)
{
    // The commented out test cases produce different results depending on
    // gcc / clang version and optimisation levels. Therefore disabled.
    xxprintf_start();

    test_adc();
    test_add();
    test_and();
//    test_bsx();
    test_cmp();
    test_dec();
    test_fcmov();
    test_fconst();
//    test_fenv();
    test_floats();
    test_inc();
//    test_jcc();
    test_lea();
    test_mul();
    test_neg();
    test_not();
    test_or();
    test_rcl();
    test_rcr();
    test_rol();
    test_ror();
    test_sar();
    test_sbb();
    test_shl();
    test_shld();
    test_shr();
    test_shrd();
//    test_string();
    test_sub();
    test_xor();
    test_bt();
    test_btc();
    test_btr();
    test_bts();

    xxprintf_done();
    return 0;
}
