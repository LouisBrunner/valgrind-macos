
/*--------------------------------------------------------------------*/
/*--- Debug (not-for-user) logging; also vprintf.     m_debuglog.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2010 Julian Seward 
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
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

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
#include "valgrind.h"            /* for RUNNING_ON_VALGRIND */

/*------------------------------------------------------------*/
/*--- Stuff to make us completely independent.             ---*/
/*------------------------------------------------------------*/

/* ----- Platform-specifics ----- */

#if defined(VGP_x86_linux)

static UInt local_sys_write_stderr ( HChar* buf, Int n )
{
   volatile Int block[2];
   block[0] = (Int)buf;
   block[1] = n;
   __asm__ volatile (
      "pushl %%ebx\n"           /* ebx is callee-save */
      "movl  %0, %%ebx\n"       /* ebx = &block */
      "pushl %%ebx\n"           /* save &block */
      "movl  0(%%ebx), %%ecx\n" /* %ecx = buf */
      "movl  4(%%ebx), %%edx\n" /* %edx = n */
      "movl  $"VG_STRINGIFY(__NR_write)", %%eax\n" /* %eax = __NR_write */
      "movl  $2, %%ebx\n"       /* %ebx = stderr */
      "int   $0x80\n"           /* write(stderr, buf, n) */
      "popl  %%ebx\n"           /* reestablish &block */
      "movl  %%eax, 0(%%ebx)\n" /* block[0] = result */
      "popl  %%ebx\n"           /* restore ebx */
      : /*wr*/
      : /*rd*/    "g" (block)
      : /*trash*/ "eax", "edi", "ecx", "edx", "memory", "cc"
   );
   if (block[0] < 0) 
      block[0] = -1;
   return block[0];
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
static UInt local_sys_write_stderr ( HChar* buf, Int n )
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
      : /*rd*/    "g" (block)
      : /*trash*/ "rax", "rdi", "rsi", "rdx", "memory", "cc"
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
      : "rax" );
   return __res;
}

#elif defined(VGP_ppc32_linux)

static UInt local_sys_write_stderr ( HChar* buf, Int n )
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

#elif defined(VGP_ppc64_linux)

static UInt local_sys_write_stderr ( HChar* buf, Int n )
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
        "r0","r2","r3","r4","r5","r6","r7","r8","r9","r10","r11","r12"
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
        "r0","r2","r4","r5","r6","r7","r8","r9","r10","r11","r12"
   );
   return (UInt)__res;
}

#elif defined(VGP_arm_linux)

static UInt local_sys_write_stderr ( HChar* buf, Int n )
{
   volatile Int block[2];
   block[0] = (Int)buf;
   block[1] = n;
   __asm__ volatile (
      "mov  r0, #2\n\t"        /* stderr */
      "ldr  r1, [%0]\n\t"      /* buf */
      "ldr  r2, [%0, #4]\n\t"  /* n */
      "mov  r7, #"VG_STRINGIFY(__NR_write)"\n\t"
      "svc  0x0\n"          /* write() */
      "str  r0, [%0]\n\t"
      :
      : "r" (block)
      : "r0","r1","r2","r7"
   );
   if (block[0] < 0)
      block[0] = -1;
   return (UInt)block[0];
}

static UInt local_sys_getpid ( void )
{
   UInt __res;
   __asm__ volatile (
      "mov  r7, #"VG_STRINGIFY(__NR_getpid)"\n"
      "svc  0x0\n"      /* getpid() */
      "mov  %0, r0\n"
      : "=r" (__res)
      :
      : "r0", "r7" );
   return __res;
}

#elif defined(VGP_ppc32_aix5)

static UInt local_sys_write_stderr ( HChar* buf, Int n )
{
   /* For some reason gcc-3.3.2 doesn't preserve r31 across the asm
      even though we state it to be trashed.  So use r27 instead. */
   volatile UInt block[3];
   block[0] = (UInt)buf;
   block[1] = n;
   block[2] = __NR_write;
   __asm__ __volatile__ (
      "mr    28,%0\n\t"      /* establish base ptr */
      "mr    27,2\n\t"       /* save r2 in r27 */
      "mflr  30\n\t"         /* save lr in r30 */

      "lwz 2,8(28)\n\t"      /* set %r2 = __NR_write */
      "li  3,2\n\t"          /* set %r3 = stderr */
      "lwz 4,0(28)\n\t"      /* set %r4 = buf */
      "lwz 5,4(28)\n\t"      /* set %r5 = n */

      "crorc 6,6,6\n\t"
      ".long 0x48000005\n\t" /* bl .+4 */
      "mflr  29\n\t"
      "addi  29,29,16\n\t"
      "mtlr  29\n\t"
      "sc\n\t"               /* write() */

      "stw 3,0(28)\n\t"      /* result */
      "stw 4,4(28)\n\t"      /* error? */

      "mr   2,27\n\t"        /* restore r2 */
      "mtlr 30"              /* restore lr */

      : /*out*/
      : /*in*/  "b" (&block[0])
      : /*trash*/
           /*temps*/    "r31","r30","r29","r28","r27",
           /*args*/     "r3","r4","r5","r6","r7","r8","r9","r10",
           /*paranoia*/ "memory","cc","r0","r1","r11","r12","r13",
                        "xer","ctr","cr0","cr1","cr2","cr3",
                        "cr4","cr5","cr6","cr7"
   );
   if (block[1] != 0)
      return -1;
   else
      return block[0];
}

static UInt local_sys_getpid ( void )
{
   /* For some reason gcc-3.3.2 doesn't preserve r31 across the asm
      even though we state it to be trashed.  So use r27 instead. */
   volatile UInt block[1];
   block[0] = __NR_getpid;
   __asm__ __volatile__ (
      "mr    28,%0\n\t"      /* establish base ptr */
      "mr    27,2\n\t"       /* save r2 in r27 */
      "mflr  30\n\t"         /* save lr in r30 */

      "lwz   2,0(28)\n\t"    /* set %r2 = __NR_getpid */

      "crorc 6,6,6\n\t"
      ".long 0x48000005\n\t" /* bl .+4 */
      "mflr  29\n\t"
      "addi  29,29,16\n\t"
      "mtlr  29\n\t"
      "sc\n\t"               /* getpid() */

      "stw   3,0(28)\n\t"    /* result -> block[0] */

      "mr   2,27\n\t"        /* restore r2 */
      "mtlr 30"              /* restore lr */

      : /*out*/
      : /*in*/  "b" (&block[0])
      : /*trash*/
           /*temps*/    "r31","r30","r29","r28","r27",
           /*args*/     "r3","r4","r5","r6","r7","r8","r9","r10",
           /*paranoia*/ "memory","cc","r0","r1","r11","r12","r13",
                        "xer","ctr","cr0","cr1","cr2","cr3",
                        "cr4","cr5","cr6","cr7"
   );
   return block[0];
}

#elif defined(VGP_ppc64_aix5)

static UInt local_sys_write_stderr ( HChar* buf, Int n )
{
   volatile ULong block[3];
   block[0] = (ULong)buf;
   block[1] = n;
   block[2] = (ULong)__NR_write;
   __asm__ __volatile__ (
      "mr    28,%0\n\t"      /* establish base ptr */
      "mr    27,2\n\t"       /* save r2 in r27 */
      "mflr  30\n\t"         /* save lr in r30 */

      "ld  2,16(28)\n\t"     /* set %r2 = __NR_write */
      "li  3,2\n\t"          /* set %r3 = stderr */
      "ld  4,0(28)\n\t"      /* set %r4 = buf */
      "ld  5,8(28)\n\t"      /* set %r5 = n */

      "crorc 6,6,6\n\t"
      ".long 0x48000005\n\t" /* bl .+4 */
      "mflr  29\n\t"
      "addi  29,29,16\n\t"
      "mtlr  29\n\t"
      "sc\n\t"               /* write() */

      "std 3,0(28)\n\t"      /* result */
      "std 4,8(28)\n\t"      /* error? */

      "mr   2,27\n\t"        /* restore r2 */
      "mtlr 30"              /* restore lr */

      : /*out*/
      : /*in*/  "b" (&block[0])
      : /*trash*/
           /*temps*/    "r31","r30","r29","r28","r27",
           /*args*/     "r3","r4","r5","r6","r7","r8","r9","r10",
           /*paranoia*/ "memory","cc","r0","r1","r11","r12","r13",
                        "xer","ctr","cr0","cr1","cr2","cr3",
                        "cr4","cr5","cr6","cr7"
   );
   if (block[1] != 0)
      return (UInt)-1;
   else
      return (UInt)block[0];
}

static UInt local_sys_getpid ( void )
{
   volatile ULong block[1];
   block[0] = __NR_getpid;
   __asm__ __volatile__ (
      "mr    28,%0\n\t"      /* establish base ptr */
      "mr    27,2\n\t"       /* save r2 in r27 */
      "mflr  30\n\t"         /* save lr in r30 */

      "ld    2,0(28)\n\t"    /* set %r2 = __NR_getpid */

      "crorc 6,6,6\n\t"
      ".long 0x48000005\n\t" /* bl .+4 */
      "mflr  29\n\t"
      "addi  29,29,16\n\t"
      "mtlr  29\n\t"
      "sc\n\t"               /* getpid() */

      "std  3,0(28)\n\t"     /* result -> block[0] */

      "mr   2,27\n\t"        /* restore r2 */
      "mtlr 30"              /* restore lr */

      : /*out*/
      : /*in*/  "b" (&block[0])
      : /*trash*/
           /*temps*/    "r31","r30","r29","r28","r27",
           /*args*/     "r3","r4","r5","r6","r7","r8","r9","r10",
           /*paranoia*/ "memory","cc","r0","r1","r11","r12","r13",
                        "xer","ctr","cr0","cr1","cr2","cr3",
                        "cr4","cr5","cr6","cr7"
   );
   return (UInt)block[0];
}

#elif defined(VGP_x86_darwin)

/* We would use VG_DARWIN_SYSNO_TO_KERNEL instead of VG_DARWIN_SYSNO_INDEX
   except that the former has a C ternary ?: operator which isn't valid in
   asm code.  Both macros give the same results for Unix-class syscalls (which
   these all are, as identified by the use of 'int 0x80'). */
__attribute__((noinline))
static UInt local_sys_write_stderr ( HChar* buf, Int n )
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
static UInt local_sys_write_stderr ( HChar* buf, Int n )
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
static void emit ( HChar* buf, Int n )
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
                     HChar* str, 
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
   if (flags & VG_MSG_LJUSTIFY) {
      ret += extra;
      for (i = 0; i < extra; i++)
         send(' ', send_arg2);
   }
   ret += len;
   for (i = 0; i < len; i++)
      send(MAYBE_TOUPPER(str[i]), send_arg2);
   if (!(flags & VG_MSG_LJUSTIFY)) {
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
                                    HChar* str )
{
   UInt   ret = 0;
   Int    i;
   Int    len = local_strlen(str);
   HChar* alt;

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
   HChar  buf[40];
   Int    ind = 0;
   Int    i, nc = 0;
   Bool   neg = False;
   HChar* digits = capitalised ? "0123456789ABCDEF" : "0123456789abcdef";
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
         /* vg_assert(ind < 39); */
         if (ind > 39) {
            buf[39] = 0;
            break;
         }
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
   Int  width;
   Int  n_ls = 0;
   Bool is_long, caps;

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
      while (format[i] >= '0' && format[i] <= '9') {
         width *= 10;
         width += format[i++] - '0';
      }
      while (format[i] == 'l') {
         i++;
         n_ls++;
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
            if (is_long)
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
            if (is_long)
               ret += myvprintf_int64(send, send_arg2, flags, 10, width, False,
                                      (ULong)(va_arg (vargs, ULong)));
            else
               ret += myvprintf_int64(send, send_arg2, flags, 10, width, False,
                                      (ULong)(va_arg (vargs, UInt)));
            break;
         case 'p': /* %p */
            ret += 2;
            send('0',send_arg2);
            send('x',send_arg2);
            ret += myvprintf_int64(send, send_arg2, flags, 16, width, True,
                                   (ULong)((UWord)va_arg (vargs, void *)));
            break;
         case 'x': /* %x */
         case 'X': /* %X */
            caps = toBool(format[i] == 'X');
            if (flags & VG_MSG_ALTFORMAT) {
               ret += 2;
               send('0',send_arg2);
               send('x',send_arg2);
            }
            if (is_long)
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
            char *str = va_arg (vargs, char *);
            if (str == (char*) 0) str = "(null)";
            ret += myvprintf_str(send, send_arg2, 
                                 flags, width, str, format[i]=='S');
            break;
         }
         case 't': { /* %t, like %s but escaping chars for XML safety */
            /* Note: simplistic; ignores field width and flags */
            char *str = va_arg (vargs, char *);
            if (str == (char*) 0) str = "(null)";
            ret += myvprintf_str_XML_simplistic(send, send_arg2, str);
            break;
         }

//         case 'y': { /* %y - print symbol */
//            Char buf[100];
//            Char *cp = buf;
//            Addr a = va_arg(vargs, Addr);
//
//            if (flags & VG_MSG_PAREN)
//               *cp++ = '(';
//            if (VG_(get_fnname_w_offset)(a, cp, sizeof(buf)-4)) {
//               if (flags & VG_MSG_PAREN) {
//                  cp += VG_(strlen)(cp);
//                  *cp++ = ')';
//                  *cp = '\0';
//               }
//               ret += myvprintf_str(send, send_arg2, flags, width, buf, 0);
//            }
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
void VG_(debugLog_startup) ( Int level, HChar* who )
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
   UInt ret, pid;
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
   (void)myvprintf_str ( add_to_buf, &buf, 0, 8, (HChar*)modulename, False );
   (void)myvprintf_str ( add_to_buf, &buf, 0, indent, "", False );

   va_start(vargs,format);
   
   ret = VG_(debugLog_vprintf) ( add_to_buf, &buf, format, vargs );

   if (buf.n > 0) {
      emit( buf.buf, local_strlen(buf.buf) );
   }

   va_end(vargs);
}



/*--------------------------------------------------------------------*/
/*--- end                                           m_debuglog.c ---*/
/*--------------------------------------------------------------------*/
