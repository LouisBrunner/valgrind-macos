/* -*- mode: C; c-basic-offset: 3; -*- */

/*--------------------------------------------------------------------*/
/*--- Debug (not-for-user) logging; also vprintf.     m_debuglog.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2017 Julian Seward 
      jseward@acm.org

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/


/* Performs low-level debug logging that can safely run immediately
   after startup.  To minimise the dependencies on any other parts of
   the system, the only place the debug output may go is file
   descriptor 2 (stderr).
*/
/* This is the first-initialised module in the entire system!
   Therefore it is CRITICAL that it does not depend on any other code
   running first.  Hence only the following very limited includes.  We
   cannot depend (directly or indirectly) on any dynamic memory
   allocation facilities, nor on the m_libc facilities, since the
   latter depend on this module.  DO NOT MESS WITH THESE INCLUDES
   UNLESS YOU ARE 100% CERTAIN YOU UNDERSTAND THE CONSEQUENCES.
*/

/* This module is also notable because it is linked into both 
   stage1 and stage2. */

/* IMPORTANT: on Darwin it is essential to use the _nocancel versions
   of syscalls rather than the vanilla version, if a _nocancel version
   is available.  See docs/internals/Darwin-notes.txt for the reason
   why. */

#include "pub_core_basics.h"     /* basic types */
#include "pub_core_vkiscnums.h"  /* for syscall numbers */
#include "pub_core_debuglog.h"   /* our own iface */
#include "pub_core_clreq.h"      /* for RUNNING_ON_VALGRIND */
#if defined(VGO_solaris)
#include "pub_core_vki.h"        /* for EINTR and ERESTART */
#endif

static Bool clo_xml;

void VG_(debugLog_setXml)(Bool xml)
{
   clo_xml = xml;
}

/*------------------------------------------------------------*/
/*--- Stuff to make us completely independent.             ---*/
/*------------------------------------------------------------*/

/* ----- Platform-specifics ----- */

#if defined(VGP_x86_linux)

static UInt local_sys_write_stderr ( const HChar* buf, Int n )
{
   Int result;

   __asm__ volatile (
      "pushl %%ebx\n"
      "movl  $"VG_STRINGIFY(__NR_write)", %%eax\n" /* %eax = __NR_write */
      "movl  $2, %%ebx\n"       /* %ebx = stderr */
      "int   $0x80\n"           /* write(stderr, buf, n) */
      "popl %%ebx\n"
      : /*wr*/    "=a" (result)
      : /*rd*/    "c" (buf), "d" (n)
      : /*trash*/ "edi", "memory", "cc"
   );

   return result >= 0 ? result : -1;
}

static UInt local_sys_getpid ( void )
{
   UInt __res;
   __asm__ volatile (
      "movl $"VG_STRINGIFY(__NR_getpid)", %%eax\n" /* %eax = __NR_getpid */
      "int  $0x80\n"       /* getpid() */
      "movl %%eax, %0\n"   /* set __res = eax */
      : "=mr" (__res)
      :
      : "eax" );
   return __res;
}

#elif defined(VGP_amd64_linux)

__attribute__((noinline))
static UInt local_sys_write_stderr ( const HChar* buf, Int n )
{
   volatile Long block[2];
   block[0] = (Long)buf;
   block[1] = n;
   __asm__ volatile (
      "subq  $256, %%rsp\n"     /* don't trash the stack redzone */
      "pushq %%r15\n"           /* r15 is callee-save */
      "movq  %0, %%r15\n"       /* r15 = &block */
      "pushq %%r15\n"           /* save &block */
      "movq  $"VG_STRINGIFY(__NR_write)", %%rax\n" /* rax = __NR_write */
      "movq  $2, %%rdi\n"       /* rdi = stderr */
      "movq  0(%%r15), %%rsi\n" /* rsi = buf */
      "movq  8(%%r15), %%rdx\n" /* rdx = n */
      "syscall\n"               /* write(stderr, buf, n) */
      "popq  %%r15\n"           /* reestablish &block */
      "movq  %%rax, 0(%%r15)\n" /* block[0] = result */
      "popq  %%r15\n"           /* restore r15 */
      "addq  $256, %%rsp\n"     /* restore stack ptr */
      : /*wr*/
      : /*rd*/    "r" (block)
      : /*trash*/ "rax", "rdi", "rsi", "rdx", "memory", "cc", "rcx", "r11"
   );
   if (block[0] < 0) 
      block[0] = -1;
   return (UInt)block[0];
}

static UInt local_sys_getpid ( void )
{
   UInt __res;
   __asm__ volatile (
      "movq $"VG_STRINGIFY(__NR_getpid)", %%rax\n" /* %rax = __NR_getpid */
      "syscall\n"          /* getpid() */
      "movl %%eax, %0\n"   /* set __res = %eax */
      : "=mr" (__res)
      :
      : "rax", "rcx", "r11"
   );
   return __res;
}

#elif defined(VGP_ppc32_linux)

static UInt local_sys_write_stderr ( const HChar* buf, Int n )
{
   volatile Int block[2];
   block[0] = (Int)buf;
   block[1] = n;
   __asm__ volatile (
      "addi 1,1,-256\n\t"
      "mr   5,%0\n\t"     /* r5 = &block[0] */
      "stw  5,0(1)\n\t"   /* stash on stack */
      "li   0,"VG_STRINGIFY(__NR_write)"\n\t" /* set %r0 = __NR_write */
      "li   3,2\n\t"      /* set %r3 = stderr */
      "lwz  4,0(5)\n\t"   /* set %r4 = buf */
      "lwz  5,4(5)\n\t"   /* set %r5 = n */
      "sc\n\t"            /* write(stderr, buf, n) */
      "lwz  5,0(1)\n\t"
      "addi 1,1,256\n\t"
      "stw  3,0(5)\n"     /* block[0] = result */
      :
      : "b" (block)
      : "cc","memory","cr0","ctr",
        "r0","r2","r3","r4","r5","r6","r7","r8","r9","r10","r11","r12"
   );
   if (block[0] < 0)
      block[0] = -1;
   return (UInt)block[0];
}

static UInt local_sys_getpid ( void )
{
   register UInt __res __asm__ ("r3");
   __asm__ volatile ( 
      "li 0, %1\n\t"
      "sc"
      : "=&r" (__res)
      : "i" (__NR_getpid)
      : "cc","memory","cr0","ctr",
        "r0","r2","r4","r5","r6","r7","r8","r9","r10","r11","r12"
   );
   return __res;
}

#elif defined(VGP_ppc64be_linux) || defined(VGP_ppc64le_linux)

static UInt local_sys_write_stderr ( const HChar* buf, Int n )
{
   volatile Long block[2];
   block[0] = (Long)buf;
   block[1] = (Long)n;
   __asm__ volatile (
      "addi 1,1,-256\n\t"
      "mr   5,%0\n\t"     /* r5 = &block[0] */
      "std  5,0(1)\n\t"   /* stash on stack */
      "li   0,"VG_STRINGIFY(__NR_write)"\n\t" /* %r0 = __NR_write */
      "li   3,2\n\t"      /* set %r3 = stderr */
      "ld   4,0(5)\n\t"   /* set %r4 = buf */
      "ld   5,8(5)\n\t"   /* set %r5 = n */
      "sc\n\t"            /* write(stderr, buf, n) */
      "ld   5,0(1)\n\t"
      "addi 1,1,256\n\t"
      "std  3,0(5)\n"     /* block[0] = result */
      :
      : "b" (block)
      : "cc","memory","cr0","ctr",
        "r0","r3","r4","r5","r6","r7","r8","r9","r10","r11","r12"
   );
   if (block[0] < 0)
      block[0] = -1;
   return (UInt)(Int)block[0];
}

static UInt local_sys_getpid ( void )
{
   register ULong __res __asm__ ("r3");
   __asm__ volatile ( 
      "li 0, %1\n\t"
      "sc"
      : "=&r" (__res)
      : "i" (__NR_getpid)
      : "cc","memory","cr0","ctr",
        "r0","r4","r5","r6","r7","r8","r9","r10","r11","r12"
   );
   return (UInt)__res;
}

#elif defined(VGP_arm_linux)

static UInt local_sys_write_stderr ( const HChar* buf, Int n )
{
   volatile Int block[2];
   block[0] = (Int)buf;
   block[1] = n;
   __asm__ volatile (
      "mov  r0, #2\n\t"        /* stderr */
      "ldr  r1, [%0]\n\t"      /* buf */
      "ldr  r2, [%0, #4]\n\t"  /* n */
      "push {r6,r7}\n\t"
      "mov  r7, #"VG_STRINGIFY(__NR_write)"\n\t"
      "svc  0x0\n"          /* write() */
      "pop  {r6,r7}\n\t"
      "str  r0, [%0]\n\t"
      :
      : "r" (block)
      : "r0","r1","r2"
   );
   if (block[0] < 0)
      block[0] = -1;
   return (UInt)block[0];
}

static UInt local_sys_getpid ( void )
{
   UInt __res;
   __asm__ volatile (
      "push {r6,r7}\n\t"
      "mov  r7, #"VG_STRINGIFY(__NR_getpid)"\n\t"
      "svc  0x0\n\t"      /* getpid() */
      "pop  {r6,r7}\n\t"
      "mov  %0, r0\n\t"
      : "=r" (__res)
      :
      : "r0" );
   return __res;
}

#elif defined(VGP_arm64_linux)

static UInt local_sys_write_stderr ( const HChar* buf, Int n )
{
   volatile ULong block[2];
   block[0] = (ULong)buf;
   block[1] = (ULong)n;
   __asm__ volatile (
      "mov  x0, #2\n\t"        /* stderr */
      "ldr  x1, [%0]\n\t"      /* buf */
      "ldr  x2, [%0, #8]\n\t"  /* n */
      "mov  x8, #"VG_STRINGIFY(__NR_write)"\n\t"
      "svc  0x0\n"          /* write() */
      "str  x0, [%0]\n\t"
      :
      : "r" (block)
      : "x0","x1","x2","x7"
   );
   if (block[0] < 0)
      block[0] = -1;
   return (UInt)block[0];
}

static UInt local_sys_getpid ( void )
{
   UInt __res;
   __asm__ volatile (
      "mov  x8, #"VG_STRINGIFY(__NR_getpid)"\n"
      "svc  0x0\n"      /* getpid() */
      "mov  %0, x0\n"
      : "=r" (__res)
      :
      : "x0", "x8" );
   return (UInt)__res;
}

#elif defined(VGP_x86_darwin)

/* We would use VG_DARWIN_SYSNO_TO_KERNEL instead of VG_DARWIN_SYSNO_INDEX
   except that the former has a C ternary ?: operator which isn't valid in
   asm code.  Both macros give the same results for Unix-class syscalls (which
   these all are, as identified by the use of 'int 0x80'). */
__attribute__((noinline))
static UInt local_sys_write_stderr ( const HChar* buf, Int n )
{
   UInt __res;
   __asm__ volatile (
      "movl  %2, %%eax\n"    /* push n */
      "pushl %%eax\n"
      "movl  %1, %%eax\n"    /* push buf */
      "pushl %%eax\n"
      "movl  $2, %%eax\n"    /* push stderr */
      "pushl %%eax\n"
      "movl  $"VG_STRINGIFY(VG_DARWIN_SYSNO_INDEX(__NR_write_nocancel))
             ", %%eax\n"
      "pushl %%eax\n"        /* push fake return address */
      "int   $0x80\n"        /* write(stderr, buf, n) */
      "jnc   1f\n"           /* jump if no error */
      "movl  $-1, %%eax\n"   /* return -1 if error */
      "1: "
      "movl  %%eax, %0\n"    /* __res = eax */
      "addl  $16, %%esp\n"   /* pop x4 */
      : "=mr" (__res)
      : "g" (buf), "g" (n)
      : "eax", "edx", "cc"
   );
   return __res;
}

static UInt local_sys_getpid ( void )
{
   UInt __res;
   __asm__ volatile (
      "movl $"VG_STRINGIFY(VG_DARWIN_SYSNO_INDEX(__NR_getpid))", %%eax\n"
      "int  $0x80\n"       /* getpid() */
      "movl %%eax, %0\n"   /* set __res = eax */
      : "=mr" (__res)
      :
      : "eax", "cc" );
   return __res;
}

#elif defined(VGP_amd64_darwin)

__attribute__((noinline))
static UInt local_sys_write_stderr ( const HChar* buf, Int n )
{
   UInt __res;
   __asm__ volatile (
      "movq  $2, %%rdi\n"    /* push stderr */
      "movq  %1, %%rsi\n"    /* push buf */
      "movl  %2, %%edx\n"    /* push n */
      "movl  $"VG_STRINGIFY(VG_DARWIN_SYSNO_FOR_KERNEL(__NR_write_nocancel))
             ", %%eax\n"
      "syscall\n"            /* write(stderr, buf, n) */
      "jnc   1f\n"           /* jump if no error */
      "movq  $-1, %%rax\n"   /* return -1 if error */
      "1: "
      "movl  %%eax, %0\n"    /* __res = eax */
      : "=mr" (__res)
      : "g" (buf), "g" (n)
      : "rdi", "rsi", "rdx", "rcx", "rax", "cc" );
   return __res;
}

static UInt local_sys_getpid ( void )
{
   UInt __res;
   __asm__ volatile (
      "movl $"VG_STRINGIFY(VG_DARWIN_SYSNO_FOR_KERNEL(__NR_getpid))", %%eax\n"
      "syscall\n"          /* getpid() */
      "movl %%eax, %0\n"   /* set __res = eax */
      : "=mr" (__res)
      :
      : "rax", "rcx", "cc" );
   return __res;
}

#elif defined(VGP_s390x_linux)

static UInt local_sys_write_stderr ( const HChar* buf, Int n )
{
   register Int          r2     asm("2") = 2;      /* file descriptor STDERR */
   register const HChar* r3     asm("3") = buf;
   register ULong        r4     asm("4") = n;
   register ULong        r2_res asm("2");
   ULong __res;

   __asm__ __volatile__ (
      "svc %c1\n"
      : "=d" (r2_res)
      : "i" (__NR_write),
        "0" (r2),
        "d" (r3),
        "d" (r4)
      : "cc", "memory");
   __res = r2_res;

   if (__res >= (ULong)(-125))
      __res = -1;
   return (UInt)(__res);
}

static UInt local_sys_getpid ( void )
{
   register ULong r2 asm("2");
   ULong __res;

   __asm__ __volatile__ (
      "svc %c1\n"
      : "=d" (r2)
      : "i" (__NR_getpid)
      : "cc", "memory");
   __res = r2;

   if (__res >= (ULong)(-125))
      __res = -1;
   return (UInt)(__res);
}

#elif defined(VGP_x86_freebsd)
static UInt local_sys_write_stderr (const HChar* buf, Int n )
{
   Int result;

   __asm__ volatile (
      "movl  %2, %%eax\n"    /* push n */
      "movl  %1, %%edx\n"    /* push buf */
      "pushl %%eax\n"
      "pushl %%edx\n"
      "movl  $2, %%eax\n"    /* push stderr */
      "pushl %%eax\n"
      "movl  $"VG_STRINGIFY(__NR_write)", %%eax\n"
      "pushl %%eax\n"        /* push write syscall id */
      "int   $0x80\n"        /* write(stderr, buf, n) */
      "jnc   1f\n"           /* jump if no error */
      "movl  $-1, %%eax\n"   /* return -1 if error */
      "1: "
      "movl  %%eax, %0\n"    /* __res = eax */
      "addl  $16, %%esp\n"   /* pop x4 */
      : /*wr*/    "=mr" (result)
      : /*rd*/    "g" (buf), "g" (n)
      : /*trash*/ "eax", "edx", "cc"
   );
   return result >= 0 ? result : -1;
}

static UInt local_sys_getpid ( void )
{
   UInt __res;
   __asm__ volatile (
      "movl $20, %%eax\n"  /* set %eax = __NR_getpid */
      "int  $0x80\n"       /* getpid() */
      "movl %%eax, %0\n"   /* set __res = eax */
      : "=mr" (__res)
      :
      : "eax" );
   return __res;
}

#elif defined(VGP_amd64_freebsd)
__attribute__((noinline))
static UInt local_sys_write_stderr (const HChar* buf, Int n )
{
   volatile Long block[2];
   block[0] = (Long)buf;
   block[1] = n;
   __asm__ volatile (
      "subq  $256, %%rsp\n"     /* don't trash the stack redzone */
      "pushq %%r15\n"           /* r15 is callee-save */
      "movq  %0, %%r15\n"       /* r15 = &block */
      "pushq %%r15\n"           /* save &block */
      "movq  $"VG_STRINGIFY(__NR_write)", %%rax\n" /* rax = __NR_write */
      "movq  $2, %%rdi\n"       /* rdi = stderr */
      "movq  0(%%r15), %%rsi\n" /* rsi = buf */
      "movq  8(%%r15), %%rdx\n" /* rdx = n */
      "syscall\n"               /* write(stderr, buf, n) */
      "popq  %%r15\n"           /* reestablish &block */
      "movq  %%rax, 0(%%r15)\n" /* block[0] = result */
      "popq  %%r15\n"           /* restore r15 */
      "addq  $256, %%rsp\n"     /* restore stack ptr */
      : /*wr*/
      : /*rd*/    "r" (block)
      : /*trash*/ "rax", "rdi", "rsi", "rdx", "memory", "cc", "rcx", "r8", "r9", "r11"
   );
   if (block[0] < 0)
      block[0] = -1;
   return (UInt)block[0];
}

static UInt local_sys_getpid ( void )
{
   UInt __res;
   __asm__ volatile (
      "movq $20, %%rax\n"  /* set %rax = __NR_getpid */
      "syscall\n"          /* getpid() */
      "movl %%eax, %0\n"   /* set __res = %eax */
      : "=mr" (__res)
      :
      : "rax", "rcx");//, "r11" );
   return __res;
}

#elif defined(VGP_mips32_linux) || defined(VGP_mips64_linux)

static UInt local_sys_write_stderr ( const HChar* buf, Int n )
{
   register RegWord v0 asm("2");
   register RegWord a0 asm("4");
   register RegWord a1 asm("5");
   register RegWord a2 asm("6");
   v0 = __NR_write;
   a2 = n;
   a1 = (RegWord)(Addr)buf;
   a0 = 2; // stderr
   __asm__ volatile (
      "syscall            \n\t"
      "addiu   $4, $0, -1 \n\t"
      #if ((defined(__mips_isa_rev) && __mips_isa_rev >= 6))
      "selnez  $4, $4, $7 \n\t"
      "seleqz  $2, $2, $7 \n\t"
      "or      $2, $2, $4 \n\t"
      #else
      "movn    $2, $4, $7 \n\t"
      #endif
     : "+d" (v0), "+d" (a0), "+d" (a1), "+d" (a2)
     :
     : "$1", "$3", "$7", "$8", "$9", "$10", "$11", "$12", "$13", "$14", "$15",
       "$24", "$25", "$31"
   );
   return v0;
}

static UInt local_sys_getpid ( void )
{
   register RegWord v0 asm("2");
   v0 = __NR_getpid;
   __asm__ volatile (
      "syscall \n\t"
     : "+d" (v0)
     :
     : "$1", "$3", "$4", "$5", "$6", "$7", "$8", "$9", "$10", "$11", "$12",
       "$13", "$14", "$15", "$24", "$25", "$31"
   );
   return v0;
}

#elif defined(VGP_nanomips_linux)

__attribute__((noinline))
static UInt local_sys_write_stderr ( const HChar* buf, Int n )
{
   register RegWord t4 asm("2");
   register RegWord a0 asm("4");
   register RegWord a1 asm("5");
   register RegWord a2 asm("6");
   t4 = __NR_write;
   a2 = n;
   a1 = (RegWord)(Addr)buf;
   a0 = 2; // stderr
   __asm__ volatile (
      "syscall[32] \n\t"
     : "+d" (t4), "+d" (a0), "+d" (a1), "+d" (a2)
     :
     : "$at", "$t5", "$a3", "$a4", "$a5", "$a6", "$a7", "$t0", "$t1", "$t2",
       "$t3", "$t8", "$t9"
   );
   return a0;
}

__attribute__((noinline))
static UInt local_sys_getpid ( void )
{
   register RegWord t4 asm("2");
   register RegWord a0 asm("4");
   t4 = __NR_getpid;
   __asm__ volatile (
      "syscall[32] \n\t"
     : "+d" (t4), "=d" (a0)
     :
     : "$at", "$t5", "$a1", "$a2", "$a3", "$a4", "$a5", "$a6", "$a7", "$t0",
       "$t1", "$t2", "$t3", "$t8", "$t9"
   );
   return a0;
}

#elif defined(VGP_x86_solaris)
static UInt local_sys_write_stderr ( const HChar* buf, Int n )
{
   UInt res, err;
   Bool restart;

   do {
      /* The Solaris kernel does not restart syscalls automatically so it is
         done here. */
      __asm__ __volatile__ (
         "movl  %[n], %%eax\n"          /* push n */
         "pushl %%eax\n"
         "movl  %[buf], %%eax\n"        /* push buf */
         "pushl %%eax\n"
         "movl  $2, %%eax\n"            /* push stderr */
         "pushl %%eax\n"
         "movl  $"VG_STRINGIFY(__NR_write)", %%eax\n"
         "pushl %%eax\n"                /* push fake return address */
         "int   $0x91\n"                /* write(stderr, buf, n) */
         "movl  $0, %%edx\n"            /* assume no error */
         "jnc   1f\n"                   /* jump if no error */
         "movl  $1, %%edx\n"            /* set error flag */
         "1: "
         "addl  $16, %%esp\n"           /* pop x4 */
         : "=&a" (res), "=d" (err)
         : [buf] "g" (buf), [n] "g" (n)
         : "cc");
      restart = err && (res == VKI_EINTR || res == VKI_ERESTART);
   } while (restart);

   return res;
}

static UInt local_sys_getpid ( void )
{
   UInt res;

   /* The getpid() syscall never returns EINTR or ERESTART so there is no need
      for restarting it. */
   __asm__ __volatile__ (
      "movl $"VG_STRINGIFY(__NR_getpid)", %%eax\n"
      "int  $0x91\n"                    /* getpid() */
      : "=a" (res)
      :
      : "edx", "cc");

   return res;
}

#elif defined(VGP_amd64_solaris)
static UInt local_sys_write_stderr ( const HChar* buf, Int n )
{
   ULong res, err;
   Bool restart;

   do {
      /* The Solaris kernel does not restart syscalls automatically so it is
         done here. */
      __asm__ __volatile__ (
         "movq  $2, %%rdi\n"            /* push stderr */
         "movq  $"VG_STRINGIFY(__NR_write)", %%rax\n"
         "syscall\n"                    /* write(stderr, buf, n) */
         "movq  $0, %%rdx\n"            /* assume no error */
         "jnc   1f\n"                   /* jump if no error */
         "movq  $1, %%rdx\n"            /* set error flag */
         "1: "
         : "=a" (res), "=d" (err)
         : "S" (buf), "d" (n)
         : "cc");
      restart = err && (res == VKI_EINTR || res == VKI_ERESTART);
   } while (restart);

   return res;
}

static UInt local_sys_getpid ( void )
{
   UInt res;

   /* The getpid() syscall never returns EINTR or ERESTART so there is no need
      for restarting it. */
   __asm__ __volatile__ (
      "movq $"VG_STRINGIFY(__NR_getpid)", %%rax\n"
      "syscall\n"                       /* getpid() */
      : "=a" (res)
      :
      : "edx", "cc");

   return res;
}

#else
# error Unknown platform
#endif


/* ----- generic ----- */

/* strlen, so we don't need m_libc */
static Int local_strlen ( const HChar* str )
{
   Int i = 0;
   while (str[i] != 0) i++;
   return i;
}

static HChar local_toupper ( HChar c )
{
   if (c >= 'a' && c <= 'z')
      return c + ('A' - 'a'); 
   else
      return c;
}

/* Emit buf[0 .. n-1] to stderr.  Unfortunately platform-specific. 
*/
static void emit ( const HChar* buf, Int n )
{
   if (n >= 1)
      (void)local_sys_write_stderr(buf, n);
}


/*------------------------------------------------------------*/
/*--- A simple, generic, vprintf implementation.           ---*/
/*------------------------------------------------------------*/

/* -----------------------------------------------
   Distantly derived from:

      vprintf replacement for Checker.
      Copyright 1993, 1994, 1995 Tristan Gingold
      Written September 1993 Tristan Gingold
      Tristan Gingold, 8 rue Parmentier, F-91120 PALAISEAU, FRANCE

   (Checker itself was GPL'd.)
   ----------------------------------------------- */

/* Some flags.  */
#define VG_MSG_SIGNED    1 /* The value is signed. */
#define VG_MSG_ZJUSTIFY  2 /* Must justify with '0'. */
#define VG_MSG_LJUSTIFY  4 /* Must justify on the left. */
#define VG_MSG_PAREN     8 /* Parenthesize if present (for %y) */
#define VG_MSG_COMMA    16 /* Add commas to numbers (for %d, %u) */
#define VG_MSG_ALTFORMAT 32 /* Convert the value to alternate format */

/* Copy a string into the buffer. */
static 
UInt myvprintf_str ( void(*send)(HChar,void*),
                     void* send_arg2,
                     Int flags, 
                     Int width, 
                     const HChar* str, 
                     Bool capitalise )
{
#  define MAYBE_TOUPPER(ch) (capitalise ? local_toupper(ch) : (ch))
   UInt ret = 0;
   Int i, extra;
   Int len = local_strlen(str);

   if (width == 0) {
      ret += len;
      for (i = 0; i < len; i++)
          send(MAYBE_TOUPPER(str[i]), send_arg2);
      return ret;
   }

   if (len > width) {
      ret += width;
      for (i = 0; i < width; i++)
         send(MAYBE_TOUPPER(str[i]), send_arg2);
      return ret;
   }

   extra = width - len;
   if (! (flags & VG_MSG_LJUSTIFY)) {
      ret += extra;
      for (i = 0; i < extra; i++)
         send(' ', send_arg2);
   }
   ret += len;
   for (i = 0; i < len; i++)
      send(MAYBE_TOUPPER(str[i]), send_arg2);
   if (flags & VG_MSG_LJUSTIFY) {
      ret += extra;
      for (i = 0; i < extra; i++)
         send(' ', send_arg2);
   }

#  undef MAYBE_TOUPPER
   return ret;
}


/* Copy a string into the buffer, escaping bad XML chars. */
static 
UInt myvprintf_str_XML_simplistic ( void(*send)(HChar,void*),
                                    void* send_arg2,
                                    const HChar* str )
{
   UInt   ret = 0;
   Int    i;
   Int    len = local_strlen(str);
   const HChar* alt;

   for (i = 0; i < len; i++) {
      switch (str[i]) {
         case '&': alt = "&amp;"; break;
         case '<': alt = "&lt;"; break;
         case '>': alt = "&gt;"; break;
         default:  alt = NULL;
      }

      if (alt) {
         while (*alt) {
            send(*alt, send_arg2);
            ret++;
            alt++;
         }
      } else {
         send(str[i], send_arg2);
         ret++;
      }
   }

   return ret;
}


/* Write P into the buffer according to these args:
 *  If SIGN is true, p is a signed.
 *  BASE is the base.
 *  If WITH_ZERO is true, '0' must be added.
 *  WIDTH is the width of the field.
 */
static 
UInt myvprintf_int64 ( void(*send)(HChar,void*), 
                       void* send_arg2,
                       Int flags, 
                       Int base, 
                       Int width, 
                       Bool capitalised,
                       ULong p )
{
   /* To print an ULong base 2 needs 64 characters. If commas are requested,
      add 21. Plus 1 for a possible sign plus 1 for \0. Makes 87 -- so let's
      say 90. The size of BUF needs to be max(90, WIDTH + 1) */
   HChar  buf[width + 1 > 90 ? width + 1 : 90];
   Int    ind = 0;
   Int    i, nc = 0;
   Bool   neg = False;
   const HChar* digits = capitalised ? "0123456789ABCDEF" : "0123456789abcdef";
   UInt   ret = 0;

   if (base < 2 || base > 16)
      return ret;
 
   if ((flags & VG_MSG_SIGNED) && (Long)p < 0) {
      p   = - (Long)p;
      neg = True;
   }

   if (p == 0)
      buf[ind++] = '0';
   else {
      while (p > 0) {
         if (flags & VG_MSG_COMMA && 10 == base &&
             0 == (ind-nc) % 3 && 0 != ind) 
         {
            buf[ind++] = ',';
            nc++;
         }
         buf[ind++] = digits[p % base];
         p /= base;
      }
   }

   if (neg)
      buf[ind++] = '-';

   if (width > 0 && !(flags & VG_MSG_LJUSTIFY)) {
      for(; ind < width; ind++) {
         buf[ind] = (flags & VG_MSG_ZJUSTIFY) ? '0': ' ';
      }
   }

   /* Reverse copy to buffer.  */
   ret += ind;
   for (i = ind -1; i >= 0; i--) {
      send(buf[i], send_arg2);
   }
   if (width > 0 && (flags & VG_MSG_LJUSTIFY)) {
      for(; ind < width; ind++) {
         ret++;
         /* Never pad with zeroes on RHS -- changes the value! */
         send(' ', send_arg2);
      }
   }
   return ret;
}


/* A simple vprintf().  */
/* EXPORTED */
UInt
VG_(debugLog_vprintf) ( 
   void(*send)(HChar,void*), 
   void* send_arg2,
   const HChar* format, 
   va_list vargs
)
{
   UInt ret = 0;
   Int  i;
   Int  flags;
   Int  width, precision;
   Int  n_ls = 0;
   Bool is_long, is_sizet, caps;

   /* We assume that vargs has already been initialised by the 
      caller, using va_start, and that the caller will similarly
      clean up with va_end.
   */

   for (i = 0; format[i] != 0; i++) {
      if (format[i] != '%') {
         send(format[i], send_arg2);
         ret++;
         continue;
      }
      i++;
      /* A '%' has been found.  Ignore a trailing %. */
      if (format[i] == 0)
         break;
      if (format[i] == '%') {
         /* '%%' is replaced by '%'. */
         send('%', send_arg2);
         ret++;
         continue;
      }
      flags = 0;
      n_ls  = 0;
      width = 0; /* length of the field. */
      precision = -1;  /* unspecified precision */
      while (1) {
         switch (format[i]) {
         case '(':
            flags |= VG_MSG_PAREN;
            break;
         case ',':
         case '\'':
            /* If ',' or '\'' follows '%', commas will be inserted. */
            flags |= VG_MSG_COMMA;
            break;
         case '-':
            /* If '-' follows '%', justify on the left. */
            flags |= VG_MSG_LJUSTIFY;
            break;
         case '0':
            /* If '0' follows '%', pads will be inserted. */
            flags |= VG_MSG_ZJUSTIFY;
            break;
         case '#':
            /* If '#' follows '%', alternative format will be used. */
            flags |= VG_MSG_ALTFORMAT;
            break;
         default:
            goto parse_fieldwidth;
         }
         i++;
      }
     parse_fieldwidth:
      /* Compute the field length. */
      if (format[i] == '*') {
         width = va_arg(vargs, Int);
         ++i;
      } else {
         while (format[i] >= '0' && format[i] <= '9') {
            width *= 10;
            width += format[i++] - '0';
         }
      }
      /* Parse precision, if any. Only meaningful for %f. For all other
         format specifiers the precision will be silently ignored. */
      if (format[i] == '.') {
         ++i;
         if (format[i] == '*') {
            precision = va_arg(vargs, Int);
            ++i;
         } else {
            precision = 0;
            while (format[i] >= '0' && format[i] <= '9') {
               precision *= 10;
               precision += format[i++] - '0';
            }
         }
      }

      is_sizet = False;
      if (format[i] == 'z') {
         is_sizet = True;
         ++i;
      } else {
         while (format[i] == 'l') {
            i++;
            n_ls++;
         }
      }

      //   %d means print a 32-bit integer.
      //  %ld means print a word-size integer.
      // %lld means print a 64-bit integer.
      if      (0 == n_ls) { is_long = False; }
      else if (1 == n_ls) { is_long = ( sizeof(void*) == sizeof(Long) ); }
      else                { is_long = True; }

      switch (format[i]) {
         case 'o': /* %o */
            if (flags & VG_MSG_ALTFORMAT) {
               ret += 2;
               send('0',send_arg2);
            }
            if (is_sizet)
               ret += myvprintf_int64(send, send_arg2, flags, 8, width, False,
                                      (ULong)(va_arg (vargs, SizeT)));
            else if (is_long)
               ret += myvprintf_int64(send, send_arg2, flags, 8, width, False,
                                      (ULong)(va_arg (vargs, ULong)));
            else
               ret += myvprintf_int64(send, send_arg2, flags, 8, width, False,
                                      (ULong)(va_arg (vargs, UInt)));
            break;
         case 'd': /* %d */
            flags |= VG_MSG_SIGNED;
            if (is_long)
               ret += myvprintf_int64(send, send_arg2, flags, 10, width, False,
                                      (ULong)(va_arg (vargs, Long)));
            else
               ret += myvprintf_int64(send, send_arg2, flags, 10, width, False,
                                      (ULong)(va_arg (vargs, Int)));
            break;
         case 'u': /* %u */
            if (is_sizet)
               ret += myvprintf_int64(send, send_arg2, flags, 10, width, False,
                                      (ULong)(va_arg (vargs, SizeT)));
            else if (is_long)
               ret += myvprintf_int64(send, send_arg2, flags, 10, width, False,
                                      (ULong)(va_arg (vargs, ULong)));
            else
               ret += myvprintf_int64(send, send_arg2, flags, 10, width, False,
                                      (ULong)(va_arg (vargs, UInt)));
            break;
         case 'p':
            if (format[i+1] == 'S') {
               i++;
               /* %pS, like %s but escaping chars for XML safety */
               /* Note: simplistic; ignores field width and flags */
               const HChar *str = va_arg (vargs, HChar *);
               if (str == NULL)
                  str = "(null)";
               ret += myvprintf_str_XML_simplistic(send, send_arg2, str);
            } else if (format[i+1] == 's') {
               i++;
               /* %ps, synonym for %s with --xml=no / %pS with --xml=yes */
               const HChar *str = va_arg (vargs, HChar *);
               if (str == NULL)
                  str = "(null)";
               if (clo_xml)
                  ret += myvprintf_str_XML_simplistic(send, send_arg2, str);
               else
                  ret += myvprintf_str(send, send_arg2, flags, width, str,
                                       False);
            } else {
               /* %p */
               ret += 2;
               send('0',send_arg2);
               send('x',send_arg2);
               ret += myvprintf_int64(send, send_arg2, flags, 16, width, True,
                                      (ULong)((UWord)va_arg (vargs, void *)));
            }
            break;
         case 'x': /* %x */
         case 'X': /* %X */
            caps = toBool(format[i] == 'X');
            if (flags & VG_MSG_ALTFORMAT) {
               ret += 2;
               send('0',send_arg2);
               send('x',send_arg2);
            }
            if (is_sizet)
               ret += myvprintf_int64(send, send_arg2, flags, 16, width, False,
                                      (ULong)(va_arg (vargs, SizeT)));
            else if (is_long)
               ret += myvprintf_int64(send, send_arg2, flags, 16, width, caps,
                                      (ULong)(va_arg (vargs, ULong)));
            else
               ret += myvprintf_int64(send, send_arg2, flags, 16, width, caps,
                                      (ULong)(va_arg (vargs, UInt)));
            break;
         case 'c': /* %c */
            ret++;
            send(va_arg (vargs, int), send_arg2);
            break;
         case 's': case 'S': { /* %s */
            const HChar *str = va_arg (vargs, HChar *);
            if (str == NULL) str = "(null)";
            ret += myvprintf_str(send, send_arg2, 
                                 flags, width, str, format[i]=='S');
            break;
         }
         case 'f': {
            /* Print a floating point number in the format x.y without
               any exponent. Capabilities are extremely limited, basically
               a joke, but good enough for our needs. */
            Double val = va_arg (vargs, Double);
            Bool is_negative = False;
            Int cnt;

            if (val < 0.0) {
               is_negative = True;
               val = - val;
            }
            /* If the integral part of the floating point number cannot be
               represented by an ULONG_MAX, print '*' characters */
            if (val > (Double)(~0ULL)) {
               if (width == 0) width = 6;    // say
               for (cnt = 0; cnt < width; ++cnt)
                  send('*', send_arg2);
               ret += width;
               break;
            }
            /* The integral part of the floating point number is representable
               by an ULong. */
            ULong ipval = val;
            Double frac = val - ipval;

            if (precision == -1) precision = 6;   // say

            /* Silently limit the precision to 10 digits. */
            if (precision > 10) precision = 10;

            /* Determine fractional part, possibly round up */
            ULong factor = 1;
            for (cnt = 0; cnt < precision; ++cnt)
               factor *= 10;
            ULong frval = frac * factor;
            if ((frac * factor - frval) > 0.5)    // round up
               frval += 1;
            /* Check rounding. */
            if (frval == factor)
               ipval += 1;
            frval %= factor;

            /* Find out how many characters are needed to print the number */

            /* The integral part... */
            UInt ipwidth, num_digit = 1;   // at least one digit
            ULong x, old_x = 0;
            for (x = 10; ; old_x = x, x *= 10, ++num_digit) {
               if (x <= old_x) break;    // overflow occurred
               if (ipval < x) break;
            }
            ipwidth = num_digit;   // width of integral part.
            if (is_negative) ++num_digit;
            if (precision != 0)
               num_digit += 1 + precision;

            // Print the number

            // Fill in blanks on the left
            if (num_digit < width && (flags & VG_MSG_LJUSTIFY) == 0) {
               for (cnt = 0; cnt < width - num_digit; ++cnt)
                  send(' ', send_arg2);
               ret += width - num_digit;
            }
            // Sign, maybe
            if (is_negative) {
               send('-', send_arg2);
               ret += 1;
            }
            // Integral part
            ret += myvprintf_int64(send, send_arg2, 0, 10, ipwidth, False,
                                   ipval);
            // Decimal point and fractional part
            if (precision != 0) {
               send('.', send_arg2);
               ret += 1;

               ret += myvprintf_int64(send, send_arg2, VG_MSG_ZJUSTIFY, 10,
                                      precision, False, frval);
            }
            // Fill in blanks on the right
            if (num_digit < width && (flags & VG_MSG_LJUSTIFY) != 0) {
               for (cnt = 0; cnt < width - num_digit; ++cnt)
                  send(' ', send_arg2);
               ret += width - num_digit;
            }
            break;
         }

//         case 'y': { /* %y - print symbol */
//            Addr a = va_arg(vargs, Addr);
//
//            HChar *name;
// 	      if (VG_(get_fnname_w_offset)(a, &name)) {
//               HChar buf[1 + VG_strlen(name) + 1 + 1];
// 	         if (flags & VG_MSG_PAREN) {
//                  VG_(sprintf)(str, "(%s)", name):
// 	         } else {
//                  VG_(sprintf)(str, "%s", name):
//               }
// 	         ret += myvprintf_str(send, flags, width, buf, 0);
// 	      }
//            break;
//         }
         default:
            break;
      }
   }
   return ret;
}


/*------------------------------------------------------------*/
/*--- Debuglog stuff.                                      ---*/
/*------------------------------------------------------------*/

/* Only print messages whose stated level is less than or equal to
   this.  By default, it makes this entire subsystem silent. */

static Int loglevel = 0;

/* Module startup. */
/* EXPORTED */
void VG_(debugLog_startup) ( Int level, const HChar* who )
{
   if (level < 0)  level = 0;
   if (level > 10) level = 10;
   loglevel = level;
   VG_(debugLog)(1, "debuglog", 
                 "DebugLog system started by %s, "
                 "level %d logging requested\n", 
                 who, loglevel);
}

/* Get the logging threshold level, as set by the most recent call to
   VG_(debugLog_startup), or zero if there have been no such calls so
   far. */
/* EXPORTED */
Int VG_(debugLog_getLevel) ( void )
{
   return loglevel;
}


/* ------------ */

typedef 
   struct {
      HChar buf[100];
      Int   n;
   } 
   printf_buf;

static void add_to_buf ( HChar c, void* p )
{
   printf_buf* buf = (printf_buf*)p;

   if (buf->n >= 100-10 /*paranoia*/ ) {
      emit( buf->buf, local_strlen(buf->buf) );
      buf->n = 0;
      buf->buf[buf->n] = 0;      
   }
   buf->buf[buf->n++] = c;
   buf->buf[buf->n] = 0;
}

/* Send a logging message.  Nothing is output unless 'level'
   is <= the current loglevel. */
/* EXPORTED */
void VG_(debugLog) ( Int level, const HChar* modulename,
                                const HChar* format, ... )
{
   UInt pid;
   Int indent, depth, i;
   va_list vargs;
   printf_buf buf;

   if (level > loglevel)
      return;

   indent = 2*level - 1;
   if (indent < 1) indent = 1;

   buf.n = 0;
   buf.buf[0] = 0;
   pid = local_sys_getpid();

   // Print one '>' in front of the messages for each level of self-hosting
   // being performed.
   depth = RUNNING_ON_VALGRIND;
   for (i = 0; i < depth; i++) {
      (void)myvprintf_str ( add_to_buf, &buf, 0, 1, ">", False );
   }
   
   (void)myvprintf_str ( add_to_buf, &buf, 0, 2, "--", False );
   (void)myvprintf_int64 ( add_to_buf, &buf, 0, 10, 1, False, (ULong)pid );
   (void)myvprintf_str ( add_to_buf, &buf, 0, 1, ":", False );
   (void)myvprintf_int64 ( add_to_buf, &buf, 0, 10, 1, False, (ULong)level );
   (void)myvprintf_str ( add_to_buf, &buf, 0, 1, ":", False );
   (void)myvprintf_str ( add_to_buf, &buf, 0, 8, modulename, False );
   (void)myvprintf_str ( add_to_buf, &buf, 0, indent, "", False );

   va_start(vargs,format);
   
   (void) VG_(debugLog_vprintf) ( add_to_buf, &buf, format, vargs );

   if (buf.n > 0) {
      emit( buf.buf, local_strlen(buf.buf) );
   }

   va_end(vargs);
}



/*--------------------------------------------------------------------*/
/*--- end                                           m_debuglog.c ---*/
/*--------------------------------------------------------------------*/
