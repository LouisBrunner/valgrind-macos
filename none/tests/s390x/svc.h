#ifndef SVC_H
#define SVC_H
#include <asm/unistd.h>
#include <sys/syscall.h>

static inline long
svc0(int num)
{
  register int _num asm("1") = num;
  register long ret asm("2");

  asm volatile(	"svc 0\n"
		:"=d"(ret)
		: "d" (_num)
		: "cc", "memory");
  return ret;
}

static inline long
svc1(int num, unsigned long arg1)
{
  register int _num asm("1") = num;
  register long ret asm("2");
  register unsigned long _arg1 asm("2") = arg1;

  asm volatile(	"svc 0\n"
		:"=d"(ret)
		: "d" (_num), "d" (_arg1)
		: "cc", "memory");
  return ret;
}

static inline long
svc2(int num, unsigned long arg1, unsigned long arg2)
{
  register int _num asm("1") = num;
  register long ret asm("2");
  register unsigned long _arg1 asm("2") = arg1;
  register unsigned long _arg2 asm("3") = arg2;

  asm volatile(	"svc 0\n"
		:"=d"(ret)
		: "d" (_num), "d" (_arg1), "d" (_arg2)
		: "cc", "memory");
  return ret;
}


static inline long
svc3(int num, unsigned long arg1, unsigned long arg2, unsigned long arg3)
{
  register int _num asm("1") = num;
  register long ret asm("2");
  register unsigned long _arg1 asm("2") = arg1;
  register unsigned long _arg2 asm("3") = arg2;
  register unsigned long _arg3 asm("4") = arg3;

  asm volatile(	"svc 0\n"
		:"=d"(ret)
		: "d" (_num), "d" (_arg1), "d" (_arg2), "d" (_arg3)
		: "cc", "memory");
  return ret;
}



static inline long
svc4(int num, unsigned long arg1, unsigned long arg2, unsigned long arg3, unsigned long arg4)
{
  register int _num asm("1") = num;
  register long ret asm("2");
  register unsigned long _arg1 asm("2") = arg1;
  register unsigned long _arg2 asm("3") = arg2;
  register unsigned long _arg3 asm("4") = arg3;
  register unsigned long _arg4 asm("5") = arg4;

  asm volatile(	"svc 0\n"
		:"=d"(ret)
		: "d" (_num), "d" (_arg1), "d" (_arg2), "d" (_arg3), "d" (_arg4)
		: "cc", "memory");
  return ret;
}



static inline long
svc5(int num, unsigned long arg1, unsigned long arg2, unsigned long arg3, unsigned long arg4,
	unsigned long arg5)
{
  register int _num asm("1") = num;
  register long ret asm("2");
  register unsigned long _arg1 asm("2") = arg1;
  register unsigned long _arg2 asm("3") = arg2;
  register unsigned long _arg3 asm("4") = arg3;
  register unsigned long _arg4 asm("5") = arg4;
  register unsigned long _arg5 asm("6") = arg5;

  asm volatile(	"svc 0\n"
		:"=d"(ret)
		: "d" (_num), "d" (_arg1), "d" (_arg2), "d" (_arg3), "d" (_arg4), "d" (_arg5)
		: "cc", "memory");
  return ret;
}

#endif /* SVC_H */
