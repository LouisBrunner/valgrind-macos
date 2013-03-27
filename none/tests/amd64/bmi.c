
#include <stdio.h>

typedef  unsigned long long int  ULong;
typedef  unsigned int            UInt;

__attribute__((noinline))
void do_andn64 ( /*OUT*/UInt* flags, /*OUT*/ULong* res, ULong arg1, ULong arg2 )
{
  ULong tem, flag;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "andn %2, %3, %0"         "\n\t"
    "pushfq"		      "\n\t"
    "popq %1"                 "\n"
    : "=&r" (tem), "=r" (flag) : "r" (arg1), "r" (arg2) : "cc"
  );
  *res = tem;
  *flags = flag & 0x8d5;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "andn %2, %3, %0"         "\n\t"
    "pushfq"		      "\n\t"
    "popq %1"                 "\n"
    : "=&r" (tem), "=r" (flag) : "m" (arg1), "r" (arg2) : "cc"
  );
  if (*res != tem || *flags != (flag & 0x8d5))
     printf ("Difference between r and m variants\n");
}

__attribute__((noinline))
void do_andn32 ( /*OUT*/UInt* flags, /*OUT*/ULong* res, UInt arg1, UInt arg2 )
{
  ULong tem, flag;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "andn %2, %3, %k0"        "\n\t"
    "pushfq"		      "\n\t"
    "popq %1"                 "\n"
    : "=&r" (tem), "=r" (flag) : "r" (arg1), "r" (arg2) : "cc"
  );
  *res = tem;
  *flags = flag & 0x8d5;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "andn %2, %3, %k0"        "\n\t"
    "pushfq"		      "\n\t"
    "popq %1"                 "\n"
    : "=&r" (tem), "=r" (flag) : "m" (arg1), "r" (arg2) : "cc"
  );
  if (*res != tem || *flags != (flag & 0x8d5))
     printf ("Difference between r and m variants\n");
}


__attribute__((noinline))
void do_mulx64 ( /*OUT*/ULong* res1, /*OUT*/ULong* res2,
                 ULong arg1, ULong arg2 )
{
  ULong tem1, tem2, flag1, flag2, flag3, flag4;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "movabsq $0x5555555555555555, %1" "\n\t"
    "movq %4, %%rdx"          "\n\t"
    "pushfq"                  "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "movq (%%rsp), %2"        "\n\t"
    "popfq"                   "\n\t"
    "mulx %5, %1, %0"         "\n\t"
    "pushfq"                  "\n\t"
    "movq (%%rsp), %3"        "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "popfq"                   "\n"
    : "=&r" (tem1), "=&r" (tem2), "=&r" (flag1), "=r" (flag2)
    : "g" (arg1), "r" (arg2) : "cc", "rdx"
  );
  *res1 = tem1;
  *res2 = tem2;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "movabsq $0x5555555555555555, %1" "\n\t"
    "movq %4, %%rdx"          "\n\t"
    "pushfq"                  "\n\t"
    "popq %2"                 "\n\t"
    "mulx %5, %1, %0"         "\n\t"
    "pushfq"                  "\n\t"
    "popq %3"                 "\n"
    : "=&r" (tem1), "=&r" (tem2), "=&r" (flag3), "=r" (flag4)
    : "g" (arg1), "m" (arg2) : "cc", "rdx"
  );
  if (*res1 != tem1 || *res2 != tem2)
     printf ("Difference between r and m variants\n");
  if (((flag1 ^ flag2) | (flag3 ^ flag4)) & 0x8d5)
     printf ("Flags changed\n");
}

__attribute__((noinline))
void do_mulx32 ( /*OUT*/ULong* res1, /*OUT*/ULong* res2,
                 UInt arg1, UInt arg2 )
{
  ULong tem1, tem2, flag1, flag2, flag3, flag4;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "movabsq $0x5555555555555555, %1" "\n\t"
    "movl %4, %%edx"          "\n\t"
    "pushfq"                  "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "movq (%%rsp), %2"        "\n\t"
    "popfq"                    "\n\t"
    "mulx %5, %k1, %k0"       "\n\t"
    "pushfq"                  "\n\t"
    "movq (%%rsp), %3"        "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "popfq"                   "\n"
    : "=&r" (tem1), "=&r" (tem2), "=&r" (flag1), "=r" (flag2)
    : "g" (arg1), "r" (arg2) : "cc", "rdx"
  );
  *res1 = tem1;
  *res2 = tem2;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "movabsq $0x5555555555555555, %1" "\n\t"
    "movl %4, %%edx"          "\n\t"
    "pushfq"                  "\n\t"
    "popq %2"                 "\n\t"
    "mulx %5, %k1, %k0"       "\n\t"
    "pushfq"                  "\n\t"
    "popq %3"                 "\n"
    : "=&r" (tem1), "=&r" (tem2), "=&r" (flag3), "=r" (flag4)
    : "g" (arg1), "m" (arg2) : "cc", "rdx"
  );
  if (*res1 != tem1 || *res2 != tem2)
     printf ("Difference between r and m variants\n");
  if (((flag1 ^ flag2) | (flag3 ^ flag4)) & 0x8d5)
     printf ("Flags changed\n");
}


__attribute__((noinline))
void do_sarx64 ( /*OUT*/ULong* res, ULong arg1, ULong arg2 )
{
  ULong tem, flag1, flag2, flag3, flag4;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "pushfq"                  "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "movq (%%rsp), %1"        "\n\t"
    "popfq"                   "\n\t"
    "sarx %3, %4, %0"         "\n\t"
    "pushfq"                  "\n\t"
    "movq (%%rsp), %2"        "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "popfq"                   "\n"
    : "=&r" (tem), "=&r" (flag1), "=r" (flag2)
    : "r" (arg1), "r" (arg2) : "cc"
  );
  *res = tem;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "pushfq"                  "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "movq (%%rsp), %1"        "\n\t"
    "popfq"                   "\n\t"
    "sarx %3, %4, %0"         "\n\t"
    "pushfq"                  "\n\t"
    "movq (%%rsp), %2"        "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "popfq"                   "\n"
    : "=&r" (tem), "=&r" (flag3), "=r" (flag4)
    : "r" (arg1), "m" (arg2) : "cc"
  );
  if (*res != tem)
     printf ("Difference between r and m variants\n");
  if (((flag1 ^ flag2) | (flag3 ^ flag4)) & 0x8d5)
     printf ("Flags changed\n");
}

__attribute__((noinline))
void do_sarx32 ( /*OUT*/ULong* res, UInt arg1, UInt arg2 )
{
  ULong tem, flag1, flag2, flag3, flag4;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "pushfq"                  "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "movq (%%rsp), %1"        "\n\t"
    "popfq"                   "\n\t"
    "sarx %3, %4, %k0"        "\n\t"
    "pushfq"                  "\n\t"
    "movq (%%rsp), %2"        "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "popfq"                   "\n"
    : "=&r" (tem), "=&r" (flag1), "=r" (flag2)
    : "r" (arg1), "r" (arg2) : "cc"
  );
  *res = tem;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "pushfq"                  "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "movq (%%rsp), %1"        "\n\t"
    "popfq"                   "\n\t"
    "sarx %3, %4, %k0"        "\n\t"
    "pushfq"                  "\n\t"
    "movq (%%rsp), %2"        "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "popfq"                   "\n"
    : "=&r" (tem), "=&r" (flag3), "=r" (flag4)
    : "r" (arg1), "m" (arg2) : "cc"
  );
  if (*res != tem)
     printf ("Difference between r and m variants\n");
  if (((flag1 ^ flag2) | (flag3 ^ flag4)) & 0x8d5)
     printf ("Flags changed\n");
}


__attribute__((noinline))
void do_shlx64 ( /*OUT*/ULong* res, ULong arg1, ULong arg2 )
{
  ULong tem, flag1, flag2, flag3, flag4;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "pushfq"                  "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "movq (%%rsp), %1"        "\n\t"
    "popfq"                   "\n\t"
    "shlx %3, %4, %0"         "\n\t"
    "pushfq"                  "\n\t"
    "movq (%%rsp), %2"        "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "popfq"                   "\n"
    : "=&r" (tem), "=&r" (flag1), "=r" (flag2)
    : "r" (arg1), "r" (arg2) : "cc"
  );
  *res = tem;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "pushfq"                  "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "movq (%%rsp), %1"        "\n\t"
    "popfq"                   "\n\t"
    "shlx %3, %4, %0"         "\n\t"
    "pushfq"                  "\n\t"
    "movq (%%rsp), %2"        "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "popfq"                   "\n"
    : "=&r" (tem), "=&r" (flag3), "=r" (flag4)
    : "r" (arg1), "m" (arg2) : "cc"
  );
  if (*res != tem)
     printf ("Difference between r and m variants\n");
  if (((flag1 ^ flag2) | (flag3 ^ flag4)) & 0x8d5)
     printf ("Flags changed\n");
}

__attribute__((noinline))
void do_shlx32 ( /*OUT*/ULong* res, UInt arg1, UInt arg2 )
{
  ULong tem, flag1, flag2, flag3, flag4;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "pushfq"                  "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "movq (%%rsp), %1"        "\n\t"
    "popfq"                   "\n\t"
    "shlx %3, %4, %k0"        "\n\t"
    "pushfq"                  "\n\t"
    "movq (%%rsp), %2"        "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "popfq"                   "\n"
    : "=&r" (tem), "=&r" (flag1), "=r" (flag2)
    : "r" (arg1), "r" (arg2) : "cc"
  );
  *res = tem;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "pushfq"                  "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "movq (%%rsp), %1"        "\n\t"
    "popfq"                   "\n\t"
    "shlx %3, %4, %k0"        "\n\t"
    "pushfq"                  "\n\t"
    "movq (%%rsp), %2"        "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "popfq"                   "\n"
    : "=&r" (tem), "=&r" (flag3), "=r" (flag4)
    : "r" (arg1), "m" (arg2) : "cc"
  );
  if (*res != tem)
     printf ("Difference between r and m variants\n");
  if (((flag1 ^ flag2) | (flag3 ^ flag4)) & 0x8d5)
     printf ("Flags changed\n");
}


__attribute__((noinline))
void do_shrx64 ( /*OUT*/ULong* res, ULong arg1, ULong arg2 )
{
  ULong tem, flag1, flag2, flag3, flag4;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "pushfq"                  "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "movq (%%rsp), %1"        "\n\t"
    "popfq"                   "\n\t"
    "shrx %3, %4, %0"         "\n\t"
    "pushfq"                  "\n\t"
    "movq (%%rsp), %2"        "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "popfq"                   "\n"
    : "=&r" (tem), "=&r" (flag1), "=r" (flag2)
    : "r" (arg1), "r" (arg2) : "cc"
  );
  *res = tem;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "pushfq"                  "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "movq (%%rsp), %1"        "\n\t"
    "popfq"                   "\n\t"
    "shrx %3, %4, %0"         "\n\t"
    "pushfq"                  "\n\t"
    "movq (%%rsp), %2"        "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "popfq"                   "\n"
    : "=&r" (tem), "=&r" (flag3), "=r" (flag4)
    : "r" (arg1), "m" (arg2) : "cc"
  );
  if (*res != tem)
     printf ("Difference between r and m variants\n");
  if (((flag1 ^ flag2) | (flag3 ^ flag4)) & 0x8d5)
     printf ("Flags changed\n");
}

__attribute__((noinline))
void do_shrx32 ( /*OUT*/ULong* res, UInt arg1, UInt arg2 )
{
  ULong tem, flag1, flag2, flag3, flag4;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "pushfq"                  "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "movq (%%rsp), %1"        "\n\t"
    "popfq"                   "\n\t"
    "shrx %3, %4, %k0"        "\n\t"
    "pushfq"                  "\n\t"
    "movq (%%rsp), %2"        "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "popfq"                   "\n"
    : "=&r" (tem), "=&r" (flag1), "=r" (flag2)
    : "r" (arg1), "r" (arg2) : "cc"
  );
  *res = tem;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "pushfq"                  "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "movq (%%rsp), %1"        "\n\t"
    "popfq"                   "\n\t"
    "shrx %3, %4, %k0"        "\n\t"
    "pushfq"                  "\n\t"
    "movq (%%rsp), %2"        "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "popfq"                   "\n"
    : "=&r" (tem), "=&r" (flag3), "=r" (flag4)
    : "r" (arg1), "m" (arg2) : "cc"
  );
  if (*res != tem)
     printf ("Difference between r and m variants\n");
  if (((flag1 ^ flag2) | (flag3 ^ flag4)) & 0x8d5)
     printf ("Flags changed\n");
}


__attribute__((noinline))
void do_rorx64 ( /*OUT*/ULong* res1, /*OUT*/ULong* res2, ULong arg )
{
  ULong tem, flag1, flag2, flag3, flag4;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "pushfq"                  "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "movq (%%rsp), %1"        "\n\t"
    "popfq"                   "\n\t"
    "rorx $12, %3, %0"        "\n\t"
    "pushfq"                  "\n\t"
    "movq (%%rsp), %2"        "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "popfq"                   "\n"
    : "=&r" (tem), "=&r" (flag1), "=r" (flag2) : "r" (arg) : "cc"
  );
  *res1 = tem;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "pushfq"                  "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "movq (%%rsp), %1"        "\n\t"
    "popfq"                   "\n\t"
    "rorx $67, %3, %0"        "\n\t"
    "pushfq"                  "\n\t"
    "movq (%%rsp), %2"        "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "popfq"                   "\n"
    : "=&r" (tem), "=&r" (flag3), "=r" (flag4) : "m" (arg) : "cc"
  );
  *res2 = tem;
  if (((flag1 ^ flag2) | (flag3 ^ flag4)) & 0x8d5)
     printf ("Flags changed\n");
}

__attribute__((noinline))
void do_rorx32 ( /*OUT*/ULong* res1, /*OUT*/ULong* res2, UInt arg )
{
  ULong tem, flag1, flag2, flag3, flag4;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "pushfq"                  "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "movq (%%rsp), %1"        "\n\t"
    "popfq"                   "\n\t"
    "rorx $12, %3, %k0"       "\n\t"
    "pushfq"                  "\n\t"
    "movq (%%rsp), %2"        "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "popfq"                   "\n"
    : "=&r" (tem), "=&r" (flag1), "=r" (flag2) : "r" (arg) : "cc"
  );
  *res1 = tem;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "pushfq"                  "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "movq (%%rsp), %1"        "\n\t"
    "popfq"                   "\n\t"
    "rorx $67, %3, %k0"       "\n\t"
    "pushfq"                  "\n\t"
    "movq (%%rsp), %2"        "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "popfq"                   "\n"
    : "=&r" (tem), "=&r" (flag3), "=r" (flag4) : "m" (arg) : "cc"
  );
  *res2 = tem;
  if (((flag1 ^ flag2) | (flag3 ^ flag4)) & 0x8d5)
     printf ("Flags changed\n");
}


__attribute__((noinline))
void do_blsi64 ( /*OUT*/UInt* flags, /*OUT*/ULong* res, ULong arg )
{
  ULong tem, flag;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "blsi %2, %0"             "\n\t"
    "pushfq"		      "\n\t"
    "popq %1"                 "\n"
    : "=&r" (tem), "=&r" (flag) : "r" (arg) : "cc"
  );
  *res = tem;
  *flags = flag & 0x8d5;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "blsi %2, %0"             "\n\t"
    "pushfq"		      "\n\t"
    "popq %1"                 "\n"
    : "=&r" (tem), "=&r" (flag) : "m" (arg) : "cc"
  );
  if (*res != tem || *flags != (flag & 0x8d5))
     printf ("Difference between r and m variants\n");
}

__attribute__((noinline))
void do_blsi32 ( /*OUT*/UInt* flags, /*OUT*/ULong* res, UInt arg )
{
  ULong tem, flag;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "blsi %2, %k0"            "\n\t"
    "pushfq"		      "\n\t"
    "popq %1"                 "\n"
    : "=&r" (tem), "=&r" (flag) : "r" (arg) : "cc"
  );
  *res = tem;
  *flags = flag & 0x8d5;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "blsi %2, %k0"            "\n\t"
    "pushfq"		      "\n\t"
    "popq %1"                 "\n"
    : "=&r" (tem), "=&r" (flag) : "m" (arg) : "cc"
  );
  if (*res != tem || *flags != (flag & 0x8d5))
     printf ("Difference between r and m variants\n");
}


__attribute__((noinline))
void do_blsmsk64 ( /*OUT*/UInt* flags, /*OUT*/ULong* res, ULong arg )
{
  ULong tem, flag;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "blsmsk %2, %0"           "\n\t"
    "pushfq"		      "\n\t"
    "popq %1"                 "\n"
    : "=&r" (tem), "=&r" (flag) : "r" (arg) : "cc"
  );
  *res = tem;
  *flags = flag & 0x8d5;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "blsmsk %2, %0"           "\n\t"
    "pushfq"		      "\n\t"
    "popq %1"                 "\n"
    : "=&r" (tem), "=&r" (flag) : "m" (arg) : "cc"
  );
  if (*res != tem || *flags != (flag & 0x8d5))
     printf ("Difference between r and m variants\n");
}

__attribute__((noinline))
void do_blsmsk32 ( /*OUT*/UInt* flags, /*OUT*/ULong* res, UInt arg )
{
  ULong tem, flag;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "blsmsk %2, %k0"          "\n\t"
    "pushfq"		      "\n\t"
    "popq %1"                 "\n"
    : "=&r" (tem), "=&r" (flag) : "r" (arg) : "cc"
  );
  *res = tem;
  *flags = flag & 0x8d5;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "blsmsk %2, %k0"          "\n\t"
    "pushfq"		      "\n\t"
    "popq %1"                 "\n"
    : "=&r" (tem), "=&r" (flag) : "m" (arg) : "cc"
  );
  if (*res != tem || *flags != (flag & 0x8d5))
     printf ("Difference between r and m variants\n");
}


__attribute__((noinline))
void do_blsr64 ( /*OUT*/UInt* flags, /*OUT*/ULong* res, ULong arg )
{
  ULong tem, flag;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "blsr %2, %0"             "\n\t"
    "pushfq"		      "\n\t"
    "popq %1"                 "\n"
    : "=&r" (tem), "=&r" (flag) : "r" (arg) : "cc"
  );
  *res = tem;
  *flags = flag & 0x8d5;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "blsr %2, %0"             "\n\t"
    "pushfq"		      "\n\t"
    "popq %1"                 "\n"
    : "=&r" (tem), "=&r" (flag) : "m" (arg) : "cc"
  );
  if (*res != tem || *flags != (flag & 0x8d5))
     printf ("Difference between r and m variants\n");
}

__attribute__((noinline))
void do_blsr32 ( /*OUT*/UInt* flags, /*OUT*/ULong* res, UInt arg )
{
  ULong tem, flag;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "blsr %2, %k0"            "\n\t"
    "pushfq"		      "\n\t"
    "popq %1"                 "\n"
    : "=&r" (tem), "=&r" (flag) : "r" (arg) : "cc"
  );
  *res = tem;
  *flags = flag & 0x8d5;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "blsr %2, %k0"            "\n\t"
    "pushfq"		      "\n\t"
    "popq %1"                 "\n"
    : "=&r" (tem), "=&r" (flag) : "m" (arg) : "cc"
  );
  if (*res != tem || *flags != (flag & 0x8d5))
     printf ("Difference between r and m variants\n");
}


__attribute__((noinline))
void do_bextr64 ( /*OUT*/UInt* flags, /*OUT*/ULong* res,
                  ULong arg1, ULong arg2 )
{
  ULong tem, flag;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "bextr %2, %3, %0"        "\n\t"
    "pushfq"		      "\n\t"
    "popq %1"                 "\n"
    : "=&r" (tem), "=&r" (flag) : "r" (arg1), "r" (arg2) : "cc"
  );
  *res = tem;
  *flags = flag & 0x8d5;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "bextr %2, %3, %0"        "\n\t"
    "pushfq"		      "\n\t"
    "popq %1"                 "\n"
    : "=&r" (tem), "=&r" (flag) : "r" (arg1), "m" (arg2) : "cc"
  );
  if (*res != tem || *flags != (flag & 0x8d5))
     printf ("Difference between r and m variants\n");
}

__attribute__((noinline))
void do_bextr32 ( /*OUT*/UInt* flags, /*OUT*/ULong* res,
                  UInt arg1, UInt arg2 )
{
  ULong tem, flag;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "bextr %2, %3, %k0"       "\n\t"
    "pushfq"		      "\n\t"
    "popq %1"                 "\n"
    : "=&r" (tem), "=&r" (flag) : "r" (arg1), "r" (arg2) : "cc"
  );
  *res = tem;
  *flags = flag & 0x8d5;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "bextr %2, %3, %k0"       "\n\t"
    "pushfq"		      "\n\t"
    "popq %1"                 "\n"
    : "=&r" (tem), "=&r" (flag) : "r" (arg1), "m" (arg2) : "cc"
  );
  if (*res != tem || *flags != (flag & 0x8d5))
     printf ("Difference between r and m variants\n");
}


__attribute__((noinline))
void do_bzhi64 ( /*OUT*/UInt* flags, /*OUT*/ULong* res,
                 ULong arg1, ULong arg2 )
{
  ULong tem, flag;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "bzhi %2, %3, %0"         "\n\t"
    "pushfq"		      "\n\t"
    "popq %1"                 "\n"
    : "=&r" (tem), "=&r" (flag) : "r" (arg1), "r" (arg2) : "cc"
  );
  *res = tem;
  *flags = flag & 0x8d5;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "bzhi %2, %3, %0"         "\n\t"
    "pushfq"		      "\n\t"
    "popq %1"                 "\n"
    : "=&r" (tem), "=&r" (flag) : "r" (arg1), "m" (arg2) : "cc"
  );
  if (*res != tem || *flags != (flag & 0x8d5))
     printf ("Difference between r and m variants\n");
}

__attribute__((noinline))
void do_bzhi32 ( /*OUT*/UInt* flags, /*OUT*/ULong* res,
                 UInt arg1, UInt arg2 )
{
  ULong tem, flag;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "bzhi %2, %3, %k0"        "\n\t"
    "pushfq"		      "\n\t"
    "popq %1"                 "\n"
    : "=&r" (tem), "=&r" (flag) : "r" (arg1), "r" (arg2) : "cc"
  );
  *res = tem;
  *flags = flag & 0x8d5;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "bzhi %2, %3, %k0"        "\n\t"
    "pushfq"		      "\n\t"
    "popq %1"                 "\n"
    : "=&r" (tem), "=&r" (flag) : "r" (arg1), "m" (arg2) : "cc"
  );
  if (*res != tem || *flags != (flag & 0x8d5))
     printf ("Difference between r and m variants\n");
}


__attribute__((noinline))
void do_pdep64 ( /*OUT*/ULong* res, ULong arg1, ULong arg2 )
{
  ULong tem, flag1, flag2, flag3, flag4;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "pushfq"                  "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "movq (%%rsp), %1"        "\n\t"
    "popfq"                   "\n\t"
    "pdep %3, %4, %0"         "\n\t"
    "pushfq"                  "\n\t"
    "movq (%%rsp), %2"        "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "popfq"                   "\n"
    : "=&r" (tem), "=&r" (flag1), "=r" (flag2)
    : "r" (arg1), "r" (arg2) : "cc"
  );
  *res = tem;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "pushfq"                  "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "movq (%%rsp), %1"        "\n\t"
    "popfq"                   "\n\t"
    "pdep %3, %4, %0"         "\n\t"
    "pushfq"                  "\n\t"
    "movq (%%rsp), %2"        "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "popfq"                   "\n"
    : "=&r" (tem), "=&r" (flag3), "=r" (flag4)
    : "m" (arg1), "r" (arg2) : "cc"
  );
  if (*res != tem)
     printf ("Difference between r and m variants\n");
  if (((flag1 ^ flag2) | (flag3 ^ flag4)) & 0x8d5)
     printf ("Flags changed\n");
}

__attribute__((noinline))
void do_pdep32 ( /*OUT*/ULong* res, UInt arg1, UInt arg2 )
{
  ULong tem, flag1, flag2, flag3, flag4;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "pushfq"                  "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "movq (%%rsp), %1"        "\n\t"
    "popfq"                   "\n\t"
    "pdep %3, %4, %k0"        "\n\t"
    "pushfq"                  "\n\t"
    "movq (%%rsp), %2"        "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "popfq"                   "\n"
    : "=&r" (tem), "=&r" (flag1), "=r" (flag2)
    : "r" (arg1), "r" (arg2) : "cc"
  );
  *res = tem;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "pushfq"                  "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "movq (%%rsp), %1"        "\n\t"
    "popfq"                   "\n\t"
    "pdep %3, %4, %k0"        "\n\t"
    "pushfq"                  "\n\t"
    "movq (%%rsp), %2"        "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "popfq"                   "\n"
    : "=&r" (tem), "=&r" (flag3), "=r" (flag4)
    : "m" (arg1), "r" (arg2) : "cc"
  );
  if (*res != tem)
     printf ("Difference between r and m variants\n");
  if (((flag1 ^ flag2) | (flag3 ^ flag4)) & 0x8d5)
     printf ("Flags changed\n");
}


__attribute__((noinline))
void do_pext64 ( /*OUT*/ULong* res, ULong arg1, ULong arg2 )
{
  ULong tem, flag1, flag2, flag3, flag4;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "pushfq"                  "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "movq (%%rsp), %1"        "\n\t"
    "popfq"                   "\n\t"
    "pext %3, %4, %0"         "\n\t"
    "pushfq"                  "\n\t"
    "movq (%%rsp), %2"        "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "popfq"                   "\n"
    : "=&r" (tem), "=&r" (flag1), "=r" (flag2)
    : "r" (arg1), "r" (arg2) : "cc"
  );
  *res = tem;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "pushfq"                  "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "movq (%%rsp), %1"        "\n\t"
    "popfq"                   "\n\t"
    "pext %3, %4, %0"         "\n\t"
    "pushfq"                  "\n\t"
    "movq (%%rsp), %2"        "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "popfq"                   "\n"
    : "=&r" (tem), "=&r" (flag3), "=r" (flag4)
    : "m" (arg1), "r" (arg2) : "cc"
  );
  if (*res != tem)
     printf ("Difference between r and m variants\n");
  if (((flag1 ^ flag2) | (flag3 ^ flag4)) & 0x8d5)
     printf ("Flags changed\n");
}

__attribute__((noinline))
void do_pext32 ( /*OUT*/ULong* res, UInt arg1, UInt arg2 )
{
  ULong tem, flag1, flag2, flag3, flag4;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "pushfq"                  "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "movq (%%rsp), %1"        "\n\t"
    "popfq"                   "\n\t"
    "pext %3, %4, %k0"        "\n\t"
    "pushfq"                  "\n\t"
    "movq (%%rsp), %2"        "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "popfq"                   "\n"
    : "=&r" (tem), "=&r" (flag1), "=r" (flag2)
    : "r" (arg1), "r" (arg2) : "cc"
  );
  *res = tem;
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %0" "\n\t"
    "pushfq"                  "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "movq (%%rsp), %1"        "\n\t"
    "popfq"                   "\n\t"
    "pext %3, %4, %k0"        "\n\t"
    "pushfq"                  "\n\t"
    "movq (%%rsp), %2"        "\n\t"
    "xorq $0x8d5, (%%rsp)"    "\n\t"
    "popfq"                   "\n"
    : "=&r" (tem), "=&r" (flag3), "=r" (flag4)
    : "m" (arg1), "r" (arg2) : "cc"
  );
  if (*res != tem)
     printf ("Difference between r and m variants\n");
  if (((flag1 ^ flag2) | (flag3 ^ flag4)) & 0x8d5)
     printf ("Flags changed\n");
}


int main ( void )
{
   ULong w1, w2;

   w1 = 0xFEDC192837475675ULL;
   w2 = 0x57657438291CDEF0ULL;
   while (1) {
      ULong res;
      UInt  flags;
      do_andn64(&flags, &res, w1, w2);
      printf("andn64 %016llx %016llx -> %016llx %04x\n", w1, w2, res, flags);
      if (w1 == 0) break;
      w1 = ((w1 >> 2) | (w1 >> 1)) + (w1 / 17ULL);
      w2 = ((w2 >> 2) | (w2 >> 1)) + (w2 / 17ULL);
   }

   w1 = 0xFEDC192837475675ULL;
   w2 = 0x57657438291CDEF0ULL;
   while (1) {
      ULong res;
      UInt  flags;
      do_andn32(&flags, &res, w1, w2);
      printf("andn32 %016llx %016llx -> %016llx %04x\n", w1, w2, res, flags);
      if (w1 == 0) break;
      w1 = ((w1 >> 2) | (w1 >> 1)) + (w1 / 17ULL);
      w2 = ((w2 >> 2) | (w2 >> 1)) + (w2 / 17ULL);
   }

   w1 = 0xFEDC192837475675ULL;
   w2 = 0x57657438291CDEF0ULL;
   while (1) {
      ULong res1, res2;
      do_mulx64(&res1, &res2, w1, w2);
      printf("mulx64 %016llx %016llx -> %016llx %016llx\n", w1, w2, res1, res2);
      if (w1 == 0) break;
      w1 = ((w1 >> 2) | (w1 >> 1)) + (w1 / 17ULL);
      w2 = ((w2 >> 2) | (w2 >> 1)) + (w2 / 17ULL);
   }

   w1 = 0xFEDC192837475675ULL;
   w2 = 0x57657438291CDEF0ULL;
   while (1) {
      ULong res1, res2;
      do_mulx32(&res1, &res2, w1, w2);
      printf("mulx32 %016llx %016llx -> %016llx %016llx\n", w1, w2, res1, res2);
      if (w1 == 0) break;
      w1 = ((w1 >> 2) | (w1 >> 1)) + (w1 / 17ULL);
      w2 = ((w2 >> 2) | (w2 >> 1)) + (w2 / 17ULL);
   }

   w1 = 0xFEDC192837475675ULL;
   w2 = 0x57657438291CDEF0ULL;
   while (1) {
      ULong res;
      do_sarx64(&res, w1, w2);
      printf("sarx64 %016llx %016llx -> %016llx\n", w1, w2, res);
      if (w1 == 0) break;
      w1 = ((w1 >> 2) | (w1 >> 1)) + (w1 / 17ULL);
      w2 = ((w2 >> 2) | (w2 >> 1)) + (w2 / 17ULL);
   }

   w1 = 0xFEDC192837475675ULL;
   w2 = 0x57657438291CDEF0ULL;
   while (1) {
      ULong res;
      do_sarx32(&res, w1, w2);
      printf("sarx32 %016llx %016llx -> %016llx\n", w1, w2, res);
      if (w1 == 0) break;
      w1 = ((w1 >> 2) | (w1 >> 1)) + (w1 / 17ULL);
      w2 = ((w2 >> 2) | (w2 >> 1)) + (w2 / 17ULL);
   }

   w1 = 0xFEDC192837475675ULL;
   w2 = 0x57657438291CDEF0ULL;
   while (1) {
      ULong res;
      do_shlx64(&res, w1, w2);
      printf("shlx64 %016llx %016llx -> %016llx\n", w1, w2, res);
      if (w1 == 0) break;
      w1 = ((w1 >> 2) | (w1 >> 1)) + (w1 / 17ULL);
      w2 = ((w2 >> 2) | (w2 >> 1)) + (w2 / 17ULL);
   }

   w1 = 0xFEDC192837475675ULL;
   w2 = 0x57657438291CDEF0ULL;
   while (1) {
      ULong res;
      do_shlx32(&res, w1, w2);
      printf("shlx32 %016llx %016llx -> %016llx\n", w1, w2, res);
      if (w1 == 0) break;
      w1 = ((w1 >> 2) | (w1 >> 1)) + (w1 / 17ULL);
      w2 = ((w2 >> 2) | (w2 >> 1)) + (w2 / 17ULL);
   }

   w1 = 0xFEDC192837475675ULL;
   w2 = 0x57657438291CDEF0ULL;
   while (1) {
      ULong res;
      do_shrx64(&res, w1, w2);
      printf("shrx64 %016llx %016llx -> %016llx\n", w1, w2, res);
      if (w1 == 0) break;
      w1 = ((w1 >> 2) | (w1 >> 1)) + (w1 / 17ULL);
      w2 = ((w2 >> 2) | (w2 >> 1)) + (w2 / 17ULL);
   }

   w1 = 0xFEDC192837475675ULL;
   w2 = 0x57657438291CDEF0ULL;
   while (1) {
      ULong res;
      do_shrx32(&res, w1, w2);
      printf("shrx32 %016llx %016llx -> %016llx\n", w1, w2, res);
      if (w1 == 0) break;
      w1 = ((w1 >> 2) | (w1 >> 1)) + (w1 / 17ULL);
      w2 = ((w2 >> 2) | (w2 >> 1)) + (w2 / 17ULL);
   }

   w1 = 0xFEDC192837475675ULL;
   while (1) {
      ULong res1, res2;
      do_rorx64(&res1, &res2, w1);
      printf("rorx64 %016llx -> %016llx %016llx\n", w1, res1, res2);
      if (w1 == 0) break;
      w1 = ((w1 >> 2) | (w1 >> 1)) + (w1 / 17ULL);
   }

   w1 = 0xFEDC192837475675ULL;
   while (1) {
      ULong res1, res2;
      do_rorx32(&res1, &res2, w1);
      printf("rorx32 %016llx -> %016llx %016llx\n", w1, res1, res2);
      if (w1 == 0) break;
      w1 = ((w1 >> 2) | (w1 >> 1)) + (w1 / 17ULL);
   }

   w1 = 0xFEDC192837475675ULL;
   while (1) {
      ULong res;
      UInt  flags;
      do_blsi64(&flags, &res, w1);
      printf("blsi64 %016llx -> %016llx %04x\n", w1, res, flags);
      if (w1 == 0) break;
      w1 = ((w1 >> 2) | (w1 >> 1)) + (w1 / 17ULL);
   }

   w1 = 0xFEDC192837475675ULL;
   while (1) {
      ULong res;
      UInt  flags;
      do_blsi32(&flags, &res, w1);
      printf("blsi32 %016llx -> %016llx %04x\n", w1, res, flags);
      if (w1 == 0) break;
      w1 = ((w1 >> 2) | (w1 >> 1)) + (w1 / 17ULL);
   }

   w1 = 0xFEDC192837475675ULL;
   while (1) {
      ULong res;
      UInt  flags;
      do_blsmsk64(&flags, &res, w1);
      printf("blsmsk64 %016llx -> %016llx %04x\n", w1, res, flags);
      if (w1 == 0) break;
      w1 = ((w1 >> 2) | (w1 >> 1)) + (w1 / 17ULL);
   }

   w1 = 0xFEDC192837475675ULL;
   while (1) {
      ULong res;
      UInt  flags;
      do_blsmsk32(&flags, &res, w1);
      printf("blsmsk32 %016llx -> %016llx %04x\n", w1, res, flags);
      if (w1 == 0) break;
      w1 = ((w1 >> 2) | (w1 >> 1)) + (w1 / 17ULL);
   }

   w1 = 0xFEDC192837475675ULL;
   while (1) {
      ULong res;
      UInt  flags;
      do_blsr64(&flags, &res, w1);
      printf("blsr64 %016llx -> %016llx %04x\n", w1, res, flags);
      if (w1 == 0) break;
      w1 = ((w1 >> 2) | (w1 >> 1)) + (w1 / 17ULL);
   }

   w1 = 0xFEDC192837475675ULL;
   while (1) {
      ULong res;
      UInt  flags;
      do_blsr32(&flags, &res, w1);
      printf("blsr32 %016llx -> %016llx %04x\n", w1, res, flags);
      if (w1 == 0) break;
      w1 = ((w1 >> 2) | (w1 >> 1)) + (w1 / 17ULL);
   }

   w1 = 0xFEDC192837475675ULL;
   w2 = 0x57657438291CDEF0ULL;
   while (1) {
      ULong res;
      UInt  flags;
      do_bextr64(&flags, &res, w1, w2);
      printf("bextr64 %016llx %016llx -> %016llx %04x\n", w1, w2, res, flags);
      if (w1 == 0) break;
      w1 = ((w1 >> 2) | (w1 >> 1)) + (w1 / 17ULL);
      w2 = ((w2 >> 2) | (w2 >> 1)) + (w2 / 17ULL);
   }

   w1 = 0xFEDC192837475675ULL;
   w2 = 0x57657438291CDEF0ULL;
   while (1) {
      ULong res;
      UInt  flags;
      do_bextr32(&flags, &res, w1, w2);
      printf("bextr32 %016llx %016llx -> %016llx %04x\n", w1, w2, res, flags);
      if (w1 == 0) break;
      w1 = ((w1 >> 2) | (w1 >> 1)) + (w1 / 17ULL);
      w2 = ((w2 >> 2) | (w2 >> 1)) + (w2 / 17ULL);
   }

   w1 = 0xFEDC192837475675ULL;
   w2 = 0x57657438291CDEF0ULL;
   while (1) {
      ULong res;
      UInt  flags;
      do_bzhi64(&flags, &res, w1, w2);
      printf("bzhi64 %016llx %016llx -> %016llx %04x\n", w1, w2, res, flags);
      if (w1 == 0) break;
      w1 = ((w1 >> 2) | (w1 >> 1)) + (w1 / 17ULL);
      w2 = ((w2 >> 2) | (w2 >> 1)) + (w2 / 17ULL);
   }

   w1 = 0xFEDC192837475675ULL;
   w2 = 0x57657438291CDEF0ULL;
   while (1) {
      ULong res;
      UInt  flags;
      do_bzhi32(&flags, &res, w1, w2);
      printf("bzhi32 %016llx %016llx -> %016llx %04x\n", w1, w2, res, flags);
      if (w1 == 0) break;
      w1 = ((w1 >> 2) | (w1 >> 1)) + (w1 / 17ULL);
      w2 = ((w2 >> 2) | (w2 >> 1)) + (w2 / 17ULL);
   }

   w1 = 0xFEDC192837475675ULL;
   w2 = 0x57657438291CDEF0ULL;
   while (1) {
      ULong res;
      do_pdep64(&res, w1, w2);
      printf("pdep64 %016llx %016llx -> %016llx\n", w1, w2, res);
      if (w1 == 0) break;
      w1 = ((w1 >> 2) | (w1 >> 1)) + (w1 / 17ULL);
      w2 = ((w2 >> 2) | (w2 >> 1)) + (w2 / 17ULL);
   }

   w1 = 0xFEDC192837475675ULL;
   w2 = 0x57657438291CDEF0ULL;
   while (1) {
      ULong res;
      do_pdep32(&res, w1, w2);
      printf("pdep32 %016llx %016llx -> %016llx\n", w1, w2, res);
      if (w1 == 0) break;
      w1 = ((w1 >> 2) | (w1 >> 1)) + (w1 / 17ULL);
      w2 = ((w2 >> 2) | (w2 >> 1)) + (w2 / 17ULL);
   }

   w1 = 0xFEDC192837475675ULL;
   w2 = 0x57657438291CDEF0ULL;
   while (1) {
      ULong res;
      do_pext64(&res, w1, w2);
      printf("pext64 %016llx %016llx -> %016llx\n", w1, w2, res);
      if (w1 == 0) break;
      w1 = ((w1 >> 2) | (w1 >> 1)) + (w1 / 17ULL);
      w2 = ((w2 >> 2) | (w2 >> 1)) + (w2 / 17ULL);
   }

   w1 = 0xFEDC192837475675ULL;
   w2 = 0x57657438291CDEF0ULL;
   while (1) {
      ULong res;
      do_pext32(&res, w1, w2);
      printf("pext32 %016llx %016llx -> %016llx\n", w1, w2, res);
      if (w1 == 0) break;
      w1 = ((w1 >> 2) | (w1 >> 1)) + (w1 / 17ULL);
      w2 = ((w2 >> 2) | (w2 >> 1)) + (w2 / 17ULL);
   }

   return 0;
}
