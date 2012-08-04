
/* Derived from: */

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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */


#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <math.h>

/**********************************************/

void test_fops(double a, double b)
{
    printf("a=%f b=%f a+b=%f\n", a, b, a + b);
    printf("a=%f b=%f a-b=%f\n", a, b, a - b);
    printf("a=%f b=%f a*b=%f\n", a, b, a * b);
    printf("a=%f b=%f a/b=%f\n", a, b, a / b);
    printf("a=%f b=%f fmod(a, b)=%f\n", a, b, fmod(a, b));
    printf("a=%f sqrt(a)=%f\n", a, sqrt(a));
    printf("a=%f sin(a)=%f\n", a, sin(a));
    printf("a=%f cos(a)=%f\n", a, cos(a));
    printf("a=%f tan(a)=%f\n", a, tan(a));
    printf("a=%f log(a)=%f\n", a, log(a));
    printf("a=%f exp(a)=%f\n", a, exp(a));
    printf("a=%f b=%f atan2(a, b)=%f\n", a, b, atan2(a, b));
    /* just to test some op combining */
    printf("a=%f asin(sin(a))=%f\n", a, asin(sin(a)));
    printf("a=%f acos(cos(a))=%f\n", a, acos(cos(a)));
    printf("a=%f atan(tan(a))=%f\n", a, atan(tan(a)));
}
#define CC_C    0x0001
#define CC_P    0x0004
#define CC_A    0x0010
#define CC_Z    0x0040
#define CC_S    0x0080
#define CC_O    0x0800


void test_fcmp(double a, double b)
{
    printf("(%f<%f)=%d\n",
           a, b, a < b);
    printf("(%f<=%f)=%d\n",
           a, b, a <= b);
    printf("(%f==%f)=%d\n",
           a, b, a == b);
    printf("(%f>%f)=%d\n",
           a, b, a > b);
    printf("(%f<=%f)=%d\n",
           a, b, a >= b);
    {
        unsigned int eflags;
        /* test f(u)comi instruction */
        asm("fcomi %2, %1\n"
            "pushf\n"
            "pop %0\n"
            : "=r" (eflags)
            : "t" (a), "u" (b));
        printf("fcomi(%f %f)=%08x\n", a, b, eflags & (CC_Z | CC_P | CC_C));
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
    printf("(float)%f = %f\n", a, fa);
    printf("(long double)%f = %Lf\n", a, la);
    printf("a=%016llx\n", *(long long *)&a);
    printf("la=%016llx %04x\n", *(long long *)&la, 
           *(unsigned short *)((char *)(&la) + 8));

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
        printf("(short)a = %d\n", wa);
        printf("(int)a = %d\n", ia);
        printf("(int64_t)a = %lld\n", lla);
        printf("rint(a) = %f\n", ra);
    }
}

#define TEST(N) \
    asm("fld" #N : "=t" (a)); \
    printf("fld" #N "= %f\n", a);

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
    printf("a=%f bcd=%04x%04x%04x%04x%04x b=%f\n", 
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
        printf("res[%d]=%f\n", i, rtab[i]);\
    printf("fpuc=%04x fpus=%04x fptag=%04x\n",\
           (env)->fpuc,\
           (env)->fpus & 0xff00,\
           (env)->fptag);\
}

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

    TEST_ENV(&float_env16, "data16/fnstenv", "data16/fldenv");
    TEST_ENV(&float_env16, "data16/fnsave", "data16/frstor");
    TEST_ENV(&float_env32, "fnstenv", "fldenv");
    TEST_ENV(&float_env32, "fnsave", "frstor");

    /* test for ffree */
    for(i=0;i<5;i++)
        asm volatile ("fldl %0" : : "m" (dtab[i]));
    asm volatile("ffree %st(2)");
    asm volatile ("fnstenv %0\n" : : "m" (float_env32));
    asm volatile ("fninit");
    printf("fptag=%04x\n", float_env32.fptag);
}


#define TEST_FCMOV(a, b, eflags, CC)\
{\
    double res;\
    asm("push %3\n"\
        "popf\n"\
        "fcmov" CC " %2, %0\n"\
        : "=t" (res)\
        : "0" (a), "u" (b), "g" (eflags));\
    printf("fcmov%s eflags=0x%04x-> %f\n", \
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
}

int main ( void )
{
  test_floats();
  return 0;
}
